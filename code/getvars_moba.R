################################################################################
## Project: bmi-poc
## Script purpose: Prepare data for analysis    
## Date: 29th April 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(DSI)
library(DSOpal)
library(dsBaseClient)
library(purrr)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(remotes)
install_github("lifecycle-project/ds-helper")
library(dsHelper)

#datashield.workspaces(opals)
ls("package:dsBaseClient")


### Sort out combined ethnicity variable
### Environmental exposures in GEN-R and DNBC
### Everyone should have smoking in pregnancy
### Everyone except inma should have data on hypertension in pregnancy

################################################################################
# 1. Assign additional opal tables  
################################################################################

## ---- Create variable lists --------------------------------------------------

## non-repeated 
nonrep.vars <- c(
  "child_id", "sex", "coh_country", "preg_dia", "agebirth_m_y", "preg_smk", 
  "parity_m", "height_m", "prepreg_weight", "ethn3_m", "preg_ht", "ga_bj", 
  "ga_us", "cohort_id", "areases_quint_preg", "green_dist_preg", 
  "green_size_preg", "ndvi300_preg", "areases_tert_preg", "greenyn300_preg", 
  "abroad_m"
)

## monthly repeated
monthrep.vars <- c(
  "child_id", "age_months", "height_", "weight_", "height_age", "weight_age")

## yearly repeated
yearrep.vars <- c(
  "child_id", "edu_m_", "age_years", "areases_quint_", "green_dist_", 
  "green_size_", "ndvi300_", "areases_tert_", "greenyn300_")

## ---- Make tibble of details for each cohort ---------------------------------
moba_tables <- tibble(
    cohort = "moba",
    table = c(
      "lc_moba_core_2_1.2_1_core_2022_3_non_rep_early_determinants_adiposity_new",
      "lc_moba_core_2_1.2_1_core_2022_3_monthly_rep_early_determinants_adiposity_new",
      "lc_moba_core_2_1.2_1_core_2022_3_yearly_rep_early_determinants_adiposity_new"), 
    type = c("nonrep", "monthrep", "yearrep"))

## ---- Assign tables ----------------------------------------------------------
moba_tables %>%
  dplyr::filter(cohort %in% c("moba")) %>%
  pwalk(function(cohort, table, type){
    
    datashield.assign(
      conns = conns[cohort], 
      symbol = type, 
      value = table, 
      variables = eval(parse(text = paste0(type, ".vars"))))
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_1")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_1")

################################################################################
# 3. Summarise factor variables to repair  
################################################################################

# ds.dataFrameFill still won't make correct levels so need to do manually

## ---- Make reference table ---------------------------------------------------
fact_repair_moba <- tibble(
  var = c("preg_smk", "preg_ht", "greenyn300_preg", "areases_quint_", 
          "areases_tert_", "greenyn300_", "edu_m_"),
  df = c(rep("nonrep", 3), rep("yearrep", 4)))

################################################################################
# 5. Make outcome  
################################################################################
################################################################################
# 6. Calculate BMI scores from monthly repeated measures data
################################################################################

## ---- First we derive BMI scores ---------------------------------------------
ds.assign(
  toAssign='monthrep$weight_/((monthrep$height_/100)^2)', 
  newobj='bmi'
)  

## ---- Now join these back to the dataframe -----------------------------------
ds.dataFrame(
  x = c('bmi', 'monthrep'), 
  newobj = 'monthrep')

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_3")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_3") 

################################################################################
# 7. Prepare data for making z-scores
################################################################################

## ---- Merge in sex variable --------------------------------------------------
ds.merge(
  x.name = "monthrep",
  y.name = "nonrep",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  newobj = "monthrep"
)

ds.asNumeric(
  x.name = "monthrep$height_age",
  newobj = "height_age"
)

dh.dropCols(
  df = "monthrep",
  vars = c("child_id", "height_", "weight_", 
           "weight_age", "bmi", "sex"), 
  type = "keep", 
  new_obj = "monthrep"
)

ds.dataFrame(
  x = c("monthrep", "height_age"),
  newobj = "monthrep"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_4")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_4") 

################################################################################
# 8. Now we make the z scores
################################################################################
ds.getWGSR(
  sex = "monthrep$sex",
  firstPart = "monthrep$weight_",
  secondPart = "monthrep$height_",
  thirdPart = "monthrep$height_age",
  index = "bfa",
  newobj = "zscores"
)

ds.dataFrame(
  x = c("monthrep", "zscores"),
  newobj = "monthrep"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_5")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_5") 

################################################################################
# 9. Create BMI, height and weight variables corresponding to age brackets 
################################################################################
bands <- c(0, 730, 730, 1461, 1461, 2922, 2922, 5113, 5113, 6544)

## ---- Cohort set 1 -----------------------------------------------------------
dh.makeStrata(
  df = "monthrep", 
  var_to_subset = "zscores", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l", 
  keep_vars = c("bmi", "height_", "weight_"),
  new_obj = "bmi_strata")

datashield.workspace_save(conns, "bmi_poc_sec_6a")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_6_a") 

################################################################################
# 10. Fix factor variables  
################################################################################

# Now we fix factor variables in non-repeated and yearly repeated data

## ---- Make factors - this fixes levels ---------------------------------------
fact_repair_moba %>%
  pmap(function(var, df){
    
    ds.asFactor(
      input.var.name = paste0(df, "$", var),
      newobj.name = var)
  })

## ---- Remove original vars from dataframes -----------------------------------
dh.dropCols(
  df = "nonrep", 
  vars = fact_repair_moba %>% dplyr::filter(df == "nonrep") %>% pull(var), 
  new_obj = "nonrep", 
  type = "remove")

dh.dropCols(
  df = "yearrep", 
  vars = fact_repair_moba %>% dplyr::filter(df == "yearrep") %>% pull(var), 
  new_obj = "yearrep", 
  type = "remove")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_7a")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_7a")

## ---- Join correct variables back --------------------------------------------
ds.dataFrame(
  x = c("nonrep", fact_repair_moba %>% dplyr::filter(df == "nonrep") %>% pull(var)),
  newobj = "nonrep_fix")

ds.dataFrame(
  x = c("yearrep", fact_repair_moba %>% dplyr::filter(df == "yearrep") %>% pull(var)),
  newobj = "yearrep_fix")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_7")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_7")

################################################################################
# 11. Create baseline variables from non repeated tables 
################################################################################

## ---- Gestational age at birth -----------------------------------------------

# Moba has ga_us, whilst the other cohorts have ga_bj. Here we create one ga
# variable from these separate variables.
ds.assign(
  toAssign = "nonrep_fix$ga_bj", 
  newobj = "ga_all",
  datasources = conns[!conns %in% c("hgs", "moba")]
) 

ds.assign(
  toAssign = "nonrep_fix$ga_us", 
  newobj = "ga_all",
  datasources = conns["moba"]
)

## ---- Maternal pre-pregnancy BMI ---------------------------------------------
ds.assign(
  toAssign='nonrep_fix$prepreg_weight/(((nonrep_fix$height_m/100))^2)', 
  newobj='prepreg_bmi'
)  

## Create categorical version 
ds.Boole(
  V1 = "prepreg_bmi",
  V2 = 18.5, 
  Boolean.operator = "<",
  newobj = "prepreg_bmi_u"
)

ds.Boole(
  V1 = "prepreg_bmi",
  V2 = 25, 
  Boolean.operator = ">=",
  newobj = "prepreg_bmi_o"
)

## ---- Parity -----------------------------------------------------------------

# We need to recode parity as a binary variable as there are issues later with 
# disclosive information when we run the models if we leave it ordinal.
ds.recodeValues(
  var.name = "nonrep_fix$parity_m",
  values2replace.vector = c(0, 1, 2, 3, 4),
  new.values.vector = c(0, 1, 1, 1, 1),
  newobj = "parity_bin")

## ---- Combine these new variables with non-repeated dataframe ----------------
ds.dataFrame(
  x = c("nonrep_fix", "ga_all", "prepreg_bmi", "prepreg_bmi_u", 
        "prepreg_bmi_o","parity_bin"),
  newobj = "nonrep_2")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_8")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_8")

################################################################################
# 12. Create baseline variables from yearly repeated tables
################################################################################

## ---- Subset to keep observations where child's age == 0 ---------------------
ds.dataFrameSubset(
  df.name = "yearrep_fix", 
  V1.name = "yearrep_fix$age_years",
  V2.name = "0",
  Boolean.operator = "==",
  newobj = "baseline_vars")

## ---- Convert to wide format -------------------------------------------------

# For the actual analysis we will want our dataset to be in wide format 
ds.reShape(
  data.name = "baseline_vars",
  timevar.name = "age_years",
  idvar.name = "child_id",
  v.names = c("edu_m_", "greenyn300_", "green_dist_", "green_size_", 
              "ndvi300_", "areases_tert_", "areases_quint_"), 
  direction = "wide", 
  newobj = "baseline_wide")

## ---- Rename baseline_vars more sensible names -------------------------------

# Currently the baseline variables we've made don't have great names because
# they've been generated automatically by the reshape function. So we give them
# some better names using a function I wrote ("dh.renameVars"). This is a short-
# cut which creates the new variables using information provided in a table and
# joins these together in a dataframe.

## First create a dataframe with old and new variable names
old_new <- tribble(
  ~oldvar, ~newvar,
  "edu_m_.0", "edu_m",
  "greenyn300_.0", "greenyn300_1",
  "green_dist_.0", "green_dist_1",
  "green_size_.0", "green_size_1", 
  "ndvi300_.0", "ndvi300_1",
  "areases_tert_.0", "areases_tert_1", 
  "areases_quint_.0", "areases_quint_1")

## Now rename them
dh.renameVars(
  df = "baseline_wide", 
  current_names = old_new$oldvar,
  new_names = old_new$newvar)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_9")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_9")

################################################################################
# 13. Create cohort dummy  
################################################################################

## ---- Fix cohort_id ----------------------------------------------------------
dh.columnCast(
  df = "nonrep", 
  target_vars =  "cohort_id", 
  target_class = "factor")

## ---- Get cohort codes -------------------------------------------------------
coh_codes <- dh.getStats(
  df = "nonrep",
  vars = "cohort_id", 
  conns = conns)

codes.tab <- coh_codes$categorical %>% 
  dplyr::filter(value != 0 & !is.na(category) & cohort != "combined") 

## ---- Make dummy variable ----------------------------------------------------
coh_dummy <- tibble(
  cohort = paste0(codes.tab$cohort, "_dummy"), 
  value = as.character(codes.tab$category)) 

coh_dummy %>%
  pmap(function(cohort, value){
    ds.Boole(
      V1 = "nonrep$cohort_id", 
      V2 = value,
      Boolean.operator = "==",
      numeric.output = TRUE, 
      na.assign = "NA", 
      newobj = cohort)
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns["moba"], "bmi_poc_sec_10")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_10")

################################################################################
# 14. Merge various datasets  
################################################################################

## ---- First we merge the non repeated and yearly repeated --------------------
ds.merge(
  x.name = "nonrep_2",
  y.name = "baseline_wide",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  newobj = "bmi_poc")

## ---- Now merge the BMI data that we've created ------------------------------
ds.merge(
  x.name = "bmi_poc",
  y.name = "bmi_strata",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  newobj = "bmi_poc")

## First rename the age variable in days
old_new_3 <- tribble(
  ~oldvar, ~newvar,
  "height_age.0_730", "age_days.730",
  "height_age.730_1461", "age_days.1461",
  "height_age.1461_2922", "age_days.2922",
  "height_age.2922_5113", "age_days.5113")

dh.renameVars(
  df = "bmi_poc", 
  current_names = old_new_3$oldvar, 
  new_names = old_new_3$newvar)

## Now make a version of the age variables in months.
age_convert <- tibble(
  formula = c(
    "bmi_poc$age_days.730/30.4368",
    "bmi_poc$age_days.1461/30.4368",
    "bmi_poc$age_days.2922/30.4368",
    "bmi_poc$age_days.5113/30.4368"), 
  obj = c(
    "age_months.24", 
    "age_months.48", 
    "age_months.96", 
    "age_months.168")
)

age_convert %>%
  pmap(function(formula, obj){
    
    ds.assign(
      toAssign = formula, 
      newobj = obj)    
    
  })

## ---- Now join these back to the dataframe -----------------------------------
ds.dataFrame(
  x = c("bmi_poc", age_convert$obj), 
  newobj = "bmi_poc")

## ---- Now add in the cohort dummy variables ----------------------------------
ds.dataFrame(
  x = c('bmi_poc', coh_dummy$cohort), 
  newobj = 'bmi_poc')

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_11")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_11")

################################################################################
# 15. Assign additional environmental variables
################################################################################
#env.vars <- c("child_id", "ndvi100_preg", "ndvi300_preg", "ndvi500_preg")
#env_coh <- c("alspac", "inma", "moba", "rhea", 
             "ninfea")

#cohorts_tables %>%
#  dplyr::filter(cohort %in% env_coh & type == "nonrep") %>%
#  pwalk(function(cohort, table, type){
    
#    datashield.assign(
#      conns = conns[cohort], 
#      symbol = "env_vars", 
#      value = table, 
#      variables = env.vars)
#  })

## ---- Save progress ----------------------------------------------------------
#datashield.workspace_save(conns, "bmi_poc_sec_12")
#conns <- datashield.login(logindata, restore = "bmi_poc_sec_12")

################################################################################
# 16. Create IQR
################################################################################
env_coh <- c("abcd", "alspac", "bib", "dnbc", "eden", "genr", "inma", "moba", 
             "rhea", "ninfea")

## ---- Pooled --------------------------------------------------
dh.makeIQR(
  df = "bmi_poc",
  vars = "ndvi300_preg",
  type = "combine",
  conns = conns[env_coh])

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns["moba"], "bmi_poc_sec_13")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_13")

################################################################################
# 16. Create combined area deprivation variable
################################################################################

# MoBa has this at year 0-1, whilst all other cohorts have in pregnancy.

ds.assign(
  toAssign = "bmi_poc$areases_tert_preg", 
  newobj = "area_dep_tmp",
  datasources = conns[names(conns) != "moba"]) 

ds.assign(
  toAssign = "bmi_poc$areases_tert_1", 
  newobj = "area_dep_tmp",
  datasources = conns["moba"])

## ---- Join back in -----------------------------------------------------------
ds.dataFrame(
  x = c('bmi_poc', "area_dep_tmp"), 
  newobj = 'bmi_poc')

## ---- Fix factor variables ---------------------------------------------------
ds.asFactor(
  input.var.name = "bmi_poc$area_dep_tmp",
  newobj.name = "area_dep")

ds.dataFrame(
  x = c('bmi_poc', "area_dep"), 
  newobj = 'bmi_poc')

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_14")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_14")

################################################################################
# 17. Fix pregnancy diabetes variable
################################################################################
ds.asFactor(
  input.var.name = "bmi_poc$preg_dia", 
  newobj.name = "preg_dia")

dh.dropCols(
  df = "bmi_poc", 
  vars = "preg_dia", 
  type = "remove", 
  new_obj = "bmi_poc")

ds.cbind(
  x = c("bmi_poc", "preg_dia"), 
  newobj = "bmi_poc")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_15")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_15")

################################################################################
# 18. Create analysis dataset  
################################################################################

## ---- First we specify vectors of exposures and outcomes ---------------------
exp.vars <- c(
  "edu_m", "preg_dia", "ndvi300_preg", "area_dep")

out.vars <- c("zscores.0_730", "zscores.730_1461", "zscores.1461_2922", 
              "zscores.2922_5113", "zscores.5113_6544")

cov.vars <- c(
  "sex", "preg_smk", "preg_ht", "parity_bin", "ethn3_m", "height_m", 
  "prepreg_bmi", "agebirth_m_y", "prepreg_bmi_u", "prepreg_bmi_o", "ga_all")

other.vars <- c(
  "child_id", "age_days.730", "age_days.1461", "age_days.2922", "age_days.5113", 
  "age_days.6544", "age_months.24", "age_months.48", "age_months.96", 
  "age_months.168", "age_months.215", "ht.730", "ht.1461", "ht.2922", 
  "ht.5113", "ht.6544", "wt.730", "wt.1461", "wt.2922", "wt.5113", "wt.6544", 
  coh_dummy$cohort, "ndvi300_preg_iqr_s", "ndvi300_preg_iqr_c", "bmi.0_730", 
  "bmi.730_1461", "bmi.1461_2922", "bmi.2922_5113", "bmi.5113_6544")


## ---- Now we create vars indicating whether any non-missing values are present
dh.defineCases(
  df = "bmi_poc", 
  vars = exp.vars, 
  type = "any",
  new_obj = "exposure", 
  conns = conns)

dh.defineCases(
  df = "bmi_poc", 
  vars = out.vars,
  type = "any",
  new_obj = "outcome", 
  conns = conns)

## ---- Next create another variable indicating whether a valid case -----------
ds.make(
  toAssign = "exposure+outcome", 
  newobj = "n_exp_out")

ds.Boole(
  V1 = "n_exp_out", 
  V2 = "2", 
  Boolean.operator = "==", 
  na.assign = 0, 
  newobj = "valid_case")

## Check how many valid cases to make sure it's plausible
ds.table("outcome")
ds.summary("valid_case")

datashield.workspace_save(conns, "bmi_poc_sec_16a")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_16a")

## ---- Rename height and weight variables -------------------------------------
ht_wt_names <- tribble(
  ~oldvar, ~newvar,
  "height_.0_730", "ht.730", 
  "height_.730_1461", "ht.1461", 
  "height_.1461_2922", "ht.2922", 
  "height_.2922_5113", "ht.5113",
  "weight_.0_730", "wt.730",
  "weight_.730_1461", "wt.1461", 
  "weight_.1461_2922", "wt.2922", 
  "weight_.2922_5113", "wt.5113")

dh.renameVars(
  df = "bmi_poc",
  current_names = ht_wt_names$oldvar,
  new_names = ht_wt_names$newvar)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_16b")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_16b")

## ---- Fill missing variable --------------------------------------------------
ds.dataFrameFill("bmi_poc", "bmi_poc")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_16c")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_16c")

################################################################################
# 19. Fix more variables
################################################################################

## ---- Prepregnancy BMI -------------------------------------------------------
dh.dropCols(
  df = "bmi_poc", 
  vars = c("child_id", "ethn3_m", "prepreg_bmi_u", "prepreg_bmi_o"), 
  type = "keep",
  new_obj = "fix_vars")

ds.asFactor("fix_vars$prepreg_bmi_u", "prepreg_bmi_u_f")
ds.asFactor("fix_vars$prepreg_bmi_o", "prepreg_bmi_o_f")

ds.dataFrame(
  x = c("fix_vars", "prepreg_bmi_u_f", "prepreg_bmi_o_f"), 
  newobj = "fix_vars")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_17")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_17")

################################################################################
# 20. Tidy up
################################################################################
dh.dropCols(
  df = "fix_vars", 
  vars = c("prepreg_bmi_u", "prepreg_bmi_o"),
  type = "remove")

ds.merge(
  x.name = "bmi_poc", 
  y.name = "fix_vars", 
  by.x.name = "child_id", 
  by.y.name = "child_id", 
  all.x = TRUE,
  all.y = FALSE,
  newobj = "bmi_poc")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_18")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_18")

################################################################################
# 21. Fix missing cohort dummy
################################################################################
#coh_dummy <- tibble(
#  cohort = paste0(codes.tab$cohort, "_dummy"), 
#  value = as.character(codes.tab$category)) 

#coh_dummy %>%
#  pmap(function(cohort, value){
#    ds.Boole(
#      V1 = "bmi_poc$cohort_id", 
#      V2 = value,
#      Boolean.operator = "==",
#      numeric.output = TRUE, 
#      na.assign = "NA", 
#      newobj = cohort)
#  })

#ds.dataFrame(
#  x = c("bmi_poc", paste0(tmp_coh, "_dummy")),
#  newobj = "bmi_poc")

## ---- Save progress ----------------------------------------------------------
#datashield.workspace_save(conns, "bmi_poc_sec_19")
#conns <- datashield.login(logindata, restore = "bmi_poc_sec_19")
################################################################################
# 24. Create final dataset
################################################################################
keep_vars <- c(exp.vars, out.vars, cov.vars, other.vars, "prepreg_bmi_u_f", 
               "prepreg_bmi_o_f", "ethn3_m_f")

## Now finally we subset based on valid cases and required variables
    ds.dataFrameSubset(
      df.name = "bmi_poc", 
      V1.name = "valid_case", 
      V2.name = "1", 
      Boolean.operator = "==", 
      keep.NAs = FALSE, 
      newobj = "analysis_df")

datashield.workspace_save(conns, "bmi_poc_sec_24")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_24")

################################################################################
# 25. Create dataset of excluded participants
################################################################################
ds.dataFrameSubset(
  df.name = "bmi_poc", 
  V1.name = "valid_case", 
  V2.name = "0", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "excluded_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_25")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_25")

dh.columnCast(
  df = "analysis_df", 
  target_vars = c("age_days.730", "age_days.1461", "age_days.2922", 
                  "age_days.5113"),
  target_class = "numeric")

datashield.workspace_save(conns, "bmi_poc_sec_26")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_26")

dh.columnCast(
  df = "excluded_df", 
  target_vars = c("age_days.730", "age_days.1461", "age_days.2922", 
                  "age_days.5113"),
  target_class = "numeric")

datashield.workspace_save(conns, "bmi_poc_sec_27")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_27")













dh.columnCast(
  df = "analysis_df", 
  target_vars = c("ethn3_m", "age_days.730", "age_days.1461", "age_days.2922", 
  "age_days.5113", "age_days.6544"),
  target_class = "factor")




dh.dropCols(
  df = "excluded_df", 
  vars = "child_id", 
  type = "keep", 
  new_obj = "test")

ds.colnames("test")







################################################################################
# 21. Clean environment
################################################################################
dh.tidyEnv(
  obj = c("analysis_df", "excluded_df", "bmi_poc", "nonrep", "monthrep", 
          "yearrep"),
  type = "keep")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_18")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_18")



################################################################################
# 21. Get missing preg_ht variable for INMA
################################################################################

## ---- Assign variable for INMA -----------------------------------------------
datashield.assign(
  conns = conns["inma"], 
  symbol = "ht", 
  value = "lc_isglobal_core_2_1.2_1_core_1_0_non_rep_200217_1_bmi", 
  variables = c("child_id", "preg_ht")
)

ds.summary("ht$preg_ht", datasources = conns["inma"])


