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
install_github("lifecycle-project/ds-helper", ref = "new-function")
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
cohorts_tables <- bind_rows(
  tibble(
    cohort = "alspac",
    table = c(
      "alspac/2_1_core_1_3/non_rep",
      "alspac/2_1_core_1_3/monthly_rep",
      "alspac/2_1_core_1_3/yearly_rep")),
  tibble(
    cohort = "bib",
    table = c(
      "sp435/2_1_core_1_1/non_rep",
      "sp435/2_1_core_1_1/monthly_rep",
      "sp435/2_1_core_1_1/yearly_rep")),
  tibble(
    cohort = "chop",
    table = c(
      "lc_chop_core_2_1.2_1_core_non_rep_bmi_earlylife_poc",
      "lc_chop_core_2_1.2_1_core_monthly_rep_bmi_earlylife_poc",
      "lc_chop_core_2_1.2_1_core_yearly_rep_bmi_earlylife_poc")),
  tibble(
    cohort = "dnbc",
    table = c(
      "lc_dnbc_core_2_2.2_2_core_non_rep_tcadman_2020-lc19",
      "lc_dnbc_core_2_2.2_2_core_monthly_rep_tcadman_2020-lc19",
      "lc_dnbc_core_2_2.2_2_core_yearly_rep_tcadman_2020-lc19")), 
  tibble(
    cohort = "eden",
    table = c(
      "project6-eden/2_1_core_1_0/non_rep", 
      "project6-eden/2_1_core_1_0/monthly_rep", 
      "project6-eden/2_1_core_1_0/yearly_rep")),
  tibble(
    cohort = "elfe",
    table = c(
      "project6-elfe/2_1_core_1_0/non_rep", 
      "project6-elfe/2_1_core_1_1/monthly_rep", 
      "project6-elfe/2_1_core_1_0/yearly_rep")),
  tibble(
    cohort = "gecko",
    table = c(
      "gecko/2_1_core_1_1/non_rep",
      "gecko/2_1_core_1_1/monthly_rep",
      "gecko/2_1_core_1_1/yearly_rep")),
  tibble(
    cohort = "genr",
    table = c(
      "lc_genr_core_2_2.2_2_core_non_rep_TC _ECCNLC201910",
      "lc_genr_core_2_2.2_2_core_monthly_rep_TC _ECCNLC201910",
      "lc_genr_core_2_2.2_2_core_yearly_rep_TC _ECCNLC201910")),
  tibble(
    cohort = "hgs",
    table = c(
      "hgs/2_1_core_1_1/non_rep",
      "hgs/2_1_core_1_1/monthly_rep",
      "hgs/2_1_core_1_1/yearly_rep")),
  tibble(
    cohort = "inma",
    table = c(
      "lc_isglobal_core_2_1.2_1_core_1_0_non_rep_200217_1_bmi",
      "lc_isglobal_core_2_1.2_1_core_1_0_monthly_rep_200217_1_bmi",
      "lc_isglobal_core_2_1.2_1_core_1_0_yearly_rep_200217_1_bmi")),
  tibble(
    cohort = "moba",
    table = c(
      "lc_moba_core_2_1.2_1_core_2021_7_non_rep_early_determinants_adiposity",
      "lc_moba_core_2_1.2_1_core_2021_7_monthly_rep_early_determinants_adiposity",
      "lc_moba_core_2_1.2_1_core_2021_7_yearly_rep_early_determinants_adiposity")), 
  tibble(
    cohort = "nfbc66",
    table = c(
      "p0650nfbc66/2_2_core_1_0/non_rep", 
      "p0650nfbc66/2_2_core_1_0/monthly_rep", 
      "p0650nfbc66/2_2_core_1_0/yearly_rep")),
  tibble(
    cohort = "nfbc86",
    table = c(
      "p0650/2_2_core_1_0/non_rep", 
      "p0650/2_2_core_1_0/monthly_rep", 
      "p0650/2_2_core_1_0/yearly_rep")),
  tibble(
    cohort = "ninfea",
    table = c(
      "lc_ninfea_core_2_0.2_0_core_1_0_non_rep",
      "lc_ninfea_core_2_0.2_0_core_1_0_monthly_rep",
      "lc_ninfea_core_2_0.2_0_core_1_0_yearly_rep")),
  tibble(
    cohort = "raine",
    table = c(
      "lc_raine_core_2_1.2_1_core_1_0_non_rep",
      "lc_raine_core_2_1.2_1_core_1_0_monthly_rep",
      "lc_raine_core_2_1.2_1_core_1_0_yearly_rep")),
  tibble(
    cohort = "rhea",
    table = c(
      "lc_rhea_core_2_1.tcadman_nr",
      "lc_rhea_core_2_1.tcadman_m",
      "lc_rhea_core_2_1.tcadman_y")),
  tibble(
    cohort = "sws",
    table = c(
      "lc_sws_core_2_1.2_1_core_1_1_non_rep", 
      "lc_sws_core_2_1.2_1_core_1_1_monthly_rep", 
      "lc_sws_core_2_1.2_1_core_1_1_yearly_rep"))) %>%
  mutate(type = rep(c("nonrep", "monthrep", "yearrep"), 17))

## ---- Assign tables ----------------------------------------------------------
cohorts_tables %>%
  dplyr::filter(cohort %in% c("nfbc66", "nfbc86")) %>%
  pwalk(function(cohort, table, type){
    
    datashield.assign(
      conns = conns[cohort], 
      symbol = type, 
      value = table, 
      variables = eval(parse(text = paste0(type, ".vars"))))
  })

ds.colnames("nonrep", datasources = conns["nfbc86"])

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_1")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_1")

################################################################################
# 2. Check available data  
################################################################################

## ---- Non-repeated -----------------------------------------------------------
non_class <- dh.classDiscrepancy(
  df = "nonrep", 
  vars = "preg_ht")

ndvi <- dh.getStats(
  df = "nonrep",
  vars = "ndvi300_preg"
)

## ---- Monthly repeated -------------------------------------------------------
month_class <- dh.classDiscrepancy(
  df = "monthrep", 
  vars = monthrep.vars, 
  conns = conns)

month_class %>% dplyr::filter(discrepancy == "yes")

## ---- Yearly repeated --------------------------------------------------------
year_class <- dh.classDiscrepancy(
  df = "yearrep", 
  vars = yearrep.vars, 
  conns = conns)

year_class %>% dplyr::filter(discrepancy == "yes")


################################################################################
# 3. Summarise factor variables to repair  
################################################################################

# ds.dataFrameFill still won't make correct levels so need to do manually

## ---- Make reference table ---------------------------------------------------
fact_repair <- tibble(
  var = c("preg_smk", "ethn3_m", "preg_ht", "areases_quint_preg", 
          "areases_tert_preg", "greenyn300_preg", "areases_quint_", 
          "areases_tert_", "greenyn300_", "edu_m_"),
  df = c(rep("nonrep", 6), rep("yearrep", 4)))


################################################################################
# 4. Fill missing variables
################################################################################

# Later on we will need to use ds.summary to get information about variables.
# However, at present it requires that all cohorts have every variable. We make
# things simpler therefore by creating empty variables where they are missing.

ds.dataFrameFill("nonrep", "nonrep")
ds.dataFrameFill("yearrep", "yearrep")

non_class_filled <- dh.classDiscrepancy(
  df = "nonrep", 
  vars = nonrep.vars)

non_class_filled %>% dplyr::filter(discrepancy == "yes")

## ---- Yearly repeated --------------------------------------------------------
year_class_filled <- dh.classDiscrepancy(
  df = "yearrep", 
  vars = yearrep.vars, 
  conns = conns)

year_class_filled %>% filter(discrepancy == "yes")

datashield.workspace_save(conns, "bmi_poc_sec_2")
conns  <- datashield.login(logindata, restore = "bmi_poc_sec_2")

################################################################################
# 5. Make outcome  
################################################################################

# We do this here because it takes a long time and is unlikely to change.
# If we need to make further changes to covariates we can do after this point
# in script.

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

ds.colnames("monthrep")

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

cohorts_1 <- c("alspac", "bib", "chop")
cohorts_2 <- "dnbc"
cohorts_3 <- c("eden", "elfe") 
cohorts_4 <- c("gecko", "genr", "hgs", "inma")
cohorts_5 <- "moba" 
cohorts_6 <- c("nfbc66", "nfbc86")
cohorts_7 <- "ninfea"
cohorts_8 <- "raine"
cohorts_9 <- c("rhea", "sws")

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
  new_obj = "bmi_strata",
  conns = conns[cohorts_1])

datashield.workspace_save(conns[cohorts_1], "bmi_poc_sec_6_a")

## ---- Cohort set 2 -----------------------------------------------------------
dh.makeStrata(
  df = "monthrep", 
  var_to_subset = "zscores", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l", 
  keep_vars = c("bmi", "height_", "weight_"),
  new_obj = "bmi_strata",
  conns = conns[cohorts_2])

datashield.workspace_save(conns[cohorts_2], "bmi_poc_sec_6_a")

## ---- Cohort set 3 -----------------------------------------------------------
dh.makeStrata(
  df = "monthrep", 
  var_to_subset = "zscores", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l", 
  keep_vars = c("bmi", "height_", "weight_"),
  new_obj = "bmi_strata",
  conns = conns[cohorts_3])

datashield.workspace_save(conns[cohorts_3], "bmi_poc_sec_6_a")

## ---- Cohort set 4 -----------------------------------------------------------
dh.makeStrata(
  df = "monthrep", 
  var_to_subset = "zscores", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l", 
  keep_vars = c("bmi", "height_", "weight_"),
  new_obj = "bmi_strata",
  conns = conns[cohorts_4])

datashield.workspace_save(conns[cohorts_4], "bmi_poc_sec_6_a")

## ---- Cohort set 5 -----------------------------------------------------------
dh.makeStrata(
  df = "monthrep", 
  var_to_subset = "zscores", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l", 
  keep_vars = c("bmi", "height_", "weight_"),
  new_obj = "bmi_strata",
  conns = conns[cohorts_5])

datashield.workspace_save(conns[cohorts_5], "bmi_poc_sec_6_a")

## ---- Cohort set 6 -----------------------------------------------------------
dh.makeStrata(
  df = "monthrep", 
  var_to_subset = "zscores", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l", 
  keep_vars = c("bmi", "height_", "weight_"),
  new_obj = "bmi_strata",
  conns = conns[cohorts_6])

datashield.workspace_save(conns[cohorts_6], "bmi_poc_sec_6_a")

## ---- Cohort set 7 -----------------------------------------------------------
dh.makeStrata(
  df = "monthrep", 
  var_to_subset = "zscores", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l", 
  keep_vars = c("bmi", "height_", "weight_"),
  new_obj = "bmi_strata",
  conns = conns[cohorts_7])

datashield.workspace_save(conns[cohorts_7], "bmi_poc_sec_6_a")

## ---- Cohort set 8 -----------------------------------------------------------
dh.makeStrata(
  df = "monthrep", 
  var_to_subset = "zscores", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l", 
  keep_vars = c("bmi", "height_", "weight_"),
  new_obj = "bmi_strata",
  conns = conns[cohorts_8])

datashield.workspace_save(conns[cohorts_8], "bmi_poc_sec_6_a")

## ---- Cohort set 9 -----------------------------------------------------------
dh.makeStrata(
  df = "monthrep", 
  var_to_subset = "zscores", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l", 
  keep_vars = c("bmi", "height_", "weight_"),
  new_obj = "bmi_strata",
  conns = conns[cohorts_9])

datashield.workspace_save(conns[cohorts_9], "bmi_poc_sec_6_a")

## ---- Check these ------------------------------------------------------------
conns <- datashield.login(logindata, restore = "bmi_poc_sec_6_a") 
ds.colnames("bmi_strata")

################################################################################
# 10. Fix factor variables  
################################################################################

# Now we fix factor variables in non-repeated and yearly repeated data

## ---- Make factors - this fixes levels ---------------------------------------
fact_repair %>%
  pmap(function(var, df){
    
    ds.asFactor(
      input.var.name = paste0(df, "$", var),
      newobj.name = var)
  })

## ---- Remove original vars from dataframes -----------------------------------
dh.dropCols(
  df = "nonrep", 
  vars = fact_repair %>% dplyr::filter(df == "nonrep") %>% pull(var), 
  new_obj = "nonrep", 
  type = "remove"
)

dh.dropCols(
  df = "yearrep", 
  vars = fact_repair %>% dplyr::(df == "yearrep") %>% pull(var), 
  new_obj = "yearrep", 
  type = "remove"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_7a")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_7a")

## ---- Join correct variables back --------------------------------------------
ds.dataFrame(
  x = c("nonrep", fact_repair %>% dplyr::filter(df == "nonrep") %>% pull(var)),
  newobj = "nonrep_fix")

ds.dataFrame(
  x = c("yearrep", fact_repair %>% dplyr::filter(df == "yearrep") %>% pull(var)),
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

ds.summary("baseline_vars$edu_m_.1")
ds.colnames("baseline_vars")

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
datashield.workspace_save(conns, "bmi_poc_sec_10")
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
  newobj = "bmi_poc"
)

## ---- Now merge the BMI data that we've created ------------------------------
ds.merge(
  x.name = "bmi_poc",
  y.name = "bmi_strata",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  newobj = "bmi_poc"
)

## ---- Rename variables -------------------------------------------------------
ds.dataFrameFill("bmi_poc", "bmi_poc")
# Need to create sensibly names monthly and yearly age variables.

datashield.workspace_save(conns, "bmi_poc_sec_11_a")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_11_a")

## First rename the age variable in days
old_new_3 <- tribble(
  ~oldvar, ~newvar,
  "height_age.0_730", "age_days.730",
  "height_age.730_1461", "age_days.1461",
  "height_age.1461_2922", "age_days.2922",
  "height_age.2922_5113", "age_days.5113",
  "height_age.5113_6544", "age_days.6544")

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
    "bmi_poc$age_days.5113/30.4368",
    "bmi_poc$age_days.6544/30.4368"), 
  obj = c(
    "age_months.24", 
    "age_months.48", 
    "age_months.96", 
    "age_months.168", 
    "age_months.215")
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
  newobj = 'bmi_poc'
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_11")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_11")

################################################################################
# 15. Assign additional environmental variables
################################################################################
env.vars <- c("child_id", "ndvi100_preg", "ndvi300_preg", "ndvi500_preg")
env_coh <- c("alspac", "bib", "dnbc", "eden", "genr", "inma", "moba", "rhea", 
             "ninfea")

cohorts_tables %>%
  dplyr::filter(cohort %in% env_coh & type == "nonrep") %>%
  pwalk(function(cohort, table, type){
    
    datashield.assign(
      conns = conns[cohort], 
      symbol = "env_vars", 
      value = table, 
      variables = env.vars)
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_12")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_12")

################################################################################
# 16. Create IQR
################################################################################

## ---- Pooled --------------------------------------------------
dh.makeIQR(
  df = "env_vars",
  vars = "ndvi300_preg",
  type = "combine",
  conns = conns[env_coh]
)

## ---- Cohort-specific --------------------------------------------------
dh.makeIQR(
  df = "env_vars",
  vars = "ndvi300_preg",
  type = "split",
  conns = conns[env_coh]
)

iqr.vars <- c("ndvi300_preg_iqr_c", "ndvi300_preg_iqr_s")

## ---- Merge back in with main dataframe --------------------------------------
dh.dropCols(
  df = "bmi_poc",
  vars = "ndvi300_preg",
  type = "remove",
  conns = conns[env_coh]
)

ds.merge(
  x.name = "bmi_poc",
  y.name = "env_vars",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  newobj = "bmi_poc",
  datasources = conns[env_coh]
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_13")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_13")

################################################################################
# 16. Create combined area deprivation variable
################################################################################

# MoBa has this at year 0-1, whilst all other cohorts have in pregnancy.

ds.assign(
  toAssign = "bmi_poc$areases_tert_preg", 
  newobj = "area_dep_tmp",
  datasources = conns[names(conns) != "moba"]
) 

ds.assign(
  toAssign = "bmi_poc$areases_tert_1", 
  newobj = "area_dep_tmp",
  datasources = conns["moba"]
)

## ---- Join back in -----------------------------------------------------------
ds.dataFrame(
  x = c('bmi_poc', "area_dep_tmp"), 
  newobj = 'bmi_poc'
)

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
  newobj.name = "preg_dia"
)

dh.dropCols(
  df = "bmi_poc", 
  vars = "preg_dia", 
  type = "remove", 
  new_obj = "bmi_poc"
)

ds.cbind(
  x = c("bmi_poc", "preg_dia"), 
  newobj = "bmi_poc"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_15")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_15")

################################################################################
# 18. Create analysis dataset  
################################################################################

# So when it comes to write up the analysis, we need to be able to specify an
# analysis dataset as a subset of all data, e.g. "contained all participants 
# with at least one exposure and outcome"


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

ds.summary("outcome")
ds.summary("valid_case")

datashield.workspace_save(conns, "bmi_poc_sec_16a")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_16a")

ds.summary("bmi_poc$weight_.5113_6544", datasources = conns["alspac"])

## ---- Rename height and weight variables -------------------------------------
ht_wt_names <- tribble(
  ~oldvar, ~newvar,
  "height_.0_730", "ht.730", 
  "height_.730_1461", "ht.1461", 
  "height_.1461_2922", "ht.2922", 
  "height_.2922_5113", "ht.5113", 
  "height_.5113_6544", "ht.6544",
  "weight_.0_730", "wt.730",
  "weight_.730_1461", "wt.1461", 
  "weight_.1461_2922", "wt.2922", 
  "weight_.2922_5113", "wt.5113", 
  "weight_.5113_6544", "wt.6544"
)

dh.renameVars(
  df = "bmi_poc",
  current_names = ht_wt_names$oldvar,
  new_names = ht_wt_names$newvar
)

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

## ---- Ethnicity --------------------------------------------------------------
ds.asNumeric("fix_vars$ethn3_m", "ethnicity", datasources = conns[eth_coh])

eth_coh <- c("alspac", "bib", "chop", "elfe", "gecko", "genr", "inma", "raine")

ds.recodeValues(
  var.name = "ethnicity",
  values2replace.vector = c(1, 2, 3),
  new.values.vector = c(0, 1, 1),
  newobj = "ethn3_m_f", 
  datasources = conns[eth_coh])

ds.asFactor("ethn3_m_f", "ethn3_m_f", datasources = conns[eth_coh])

ds.dataFrame(
  x = c("fix_vars", "ethn3_m_f"),
  newobj = "fix_vars", 
  datasources = conns[eth_coh])

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_17")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_17")

################################################################################
# 20. Tidy up
################################################################################
dh.dropCols(
  df = "fix_vars", 
  vars = c("ethn3_m", "prepreg_bmi_u", "prepreg_bmi_o"),
  type = "remove")

ds.dataFrameFill("fix_vars", "fix_vars")

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
coh_dummy <- tibble(
  cohort = paste0(codes.tab$cohort, "_dummy"), 
  value = as.character(codes.tab$category)) 

coh_dummy %>%
  dplyr::filter(cohort %in% c("hgs_dummy", "sws_dummy")) %>%
  pmap(function(cohort, value){
    ds.Boole(
      V1 = "bmi_poc$cohort_id", 
      V2 = value,
      Boolean.operator = "==",
      numeric.output = TRUE, 
      na.assign = "NA", 
      newobj = cohort)
  })

ds.dataFrame(
  x = c("bmi_poc", "hgs_dummy", "sws_dummy"),
  newobj = "bmi_poc")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_19")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_19")

################################################################################
# 22. Fix BMI for ELFE
################################################################################

## There was a problem with incorrect outliers which has been fixed in the 
## latest version of the data

## ---- Assign data from correct table -----------------------------------------
datashield.assign(
  symbol = "monthrep", 
  value = "project6-elfe/2_1_core_1_1/monthly_rep", 
  variables = monthrep.vars, 
  conns = conns["elfe"])

## ---- Calculate BMI ----------------------------------------------------------
ds.assign(
  toAssign='monthrep$weight_/((monthrep$height_/100)^2)', 
  newobj='bmi', 
  datasources = conns["elfe"])  

ds.dataFrame(
  x = c('bmi', 'monthrep'), 
  newobj = 'monthrep', 
  datasources = conns["elfe"])

## ---- Prepare data for z-scores ----------------------------------------------
ds.merge(
  x.name = "monthrep",
  y.name = "nonrep",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  newobj = "monthrep", 
  datasources = conns["elfe"])

ds.asNumeric(
  x.name = "monthrep$height_age",
  newobj = "height_age", 
  datasources = conns["elfe"])

dh.dropCols(
  df = "monthrep",
  vars = c("child_id", "height_", "weight_", 
           "weight_age", "bmi", "sex"), 
  type = "keep", 
  new_obj = "monthrep", 
  checks = FALSE, 
  conns = conns["elfe"])

ds.dataFrame(
  x = c("monthrep", "height_age"),
  newobj = "monthrep", 
  datasources = conns["elfe"])

## ---- Make z-scores ----------------------------------------------------------
ds.getWGSR(
  sex = "monthrep$sex",
  firstPart = "monthrep$weight_",
  secondPart = "monthrep$height_",
  thirdPart = "monthrep$height_age",
  index = "bfa",
  newobj = "zscores", 
  datasources = conns["elfe"])

ds.dataFrame(
  x = c("monthrep", "zscores"),
  newobj = "monthrep", 
  datasources = conns["elfe"])

## ---- Create BMI, height and weight variables corresponding to age brackets --
bands <- c(0, 730, 730, 1461, 1461, 2922, 2922, 5113, 5113, 6544)

dh.makeStrata(
  df = "monthrep", 
  var_to_subset = "zscores", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l", 
  keep_vars = c("bmi", "height_", "weight_"),
  new_obj = "bmi_strata",
  conns = conns["elfe"])

dh.tidyEnv(
  obj = c("nonrep", "monthrep", "yearrep", "bmi_poc", "bmi_strata"), 
  type = "keep")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_20")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_20")

## ---- Rename variables -------------------------------------------------------
elfe_rename <- tribble(
  ~oldvar, ~newvar,
  "height_.0_730", "ht.730", 
  "height_.730_1461", "ht.1461", 
  "height_.1461_2922", "ht.2922", 
  "height_.2922_5113", "ht.5113", 
  "weight_.0_730", "wt.730",
  "weight_.730_1461", "wt.1461", 
  "weight_.1461_2922", "wt.2922",
  "weight_.2922_5113", "wt.5113",
  "height_age.0_730", "age_days.730",
  "height_age.730_1461", "age_days.1461",
  "height_age.1461_2922", "age_days.2922",
  "height_age.2922_5113", "age_days.5113")

dh.renameVars(
  df = "bmi_strata",
  current_names = elfe_rename$oldvar,
  new_names = elfe_rename$newvar, 
  conns = conns["elfe"], 
  checks = F)

## Now make a version of the age variables in months.
age_convert <- tibble(
  formula = c(
    "bmi_strata$age_days.730/30.4368",
    "bmi_strata$age_days.1461/30.4368",
    "bmi_strata$age_days.2922/30.4368",
    "bmi_strata$age_days.5113/30.4368"), 
  obj = c(
    "age_months.24", 
    "age_months.48", 
    "age_months.96", 
    "age_months.168"))

age_convert %>%
  pmap(function(formula, obj){
    
    ds.assign(
      toAssign = formula, 
      newobj = obj, 
      datasources = conns["elfe"])    
    
  })

## ---- Now join these back to the dataframe -----------------------------------
ds.dataFrame(
  x = c("bmi_poc", age_convert$obj), 
  newobj = "bmi_poc", 
  datasources = conns["elfe"])

## ---- Remove existing variables ----------------------------------------------
dh.dropCols(
  df = "bmi_poc", 
  vars = c(
    "zscores.0_730", "zscores.730_1461", "zscores.1461_2922", 
    "zscores.2922_5113", "bmi.0_730", "bmi.730_1461", "bmi.1461_2922", 
    "bmi.2922_5113", "ht.730", "ht.1461", "ht.2922", "ht.5113", 
    "wt.730", "wt.1461", "wt.2922", "wt.5113", "age_days.730", 
    "age_days.1461", "age_days.2922", "age_days.5113", "age_months.24", 
    "age_months.48", "age_months.96", "age_months.168"),
  type = "remove", 
  conns = conns["elfe"])

## ---- Merge corrected variables back in --------------------------------------
ds.merge(
  x.name = "bmi_poc", 
  y.name = "bmi_strata",
  by.x.names = "child_id", 
  by.y.names = "child_id",
  all.x = TRUE,
  all.y = FALSE, 
  newobj = "bmi_poc", 
  datasources = conns["elfe"])

dh.tidyEnv(
  obj = c("nonrep", "monthrep", "yearrep", "bmi_poc", "bmi_strata"), 
  type = "keep")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_21")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_21")

################################################################################
# 23. Fix maternal education for HGS
################################################################################
dh.makeStrata(
  df = "yearrep", 
  id_var = "child_id", 
  age_var = "age_years",
  var_to_subset = "edu_m_",
  bands = c(0, 14), 
  band_action = "ge_l",
  new_obj = "mat_ed",
  mult_action = "earliest", 
  conns = conns["hgs"])

## ---- Rename baseline_vars more sensible names -------------------------------
old_new <- tibble(
  oldvar = "edu_m_.0_14", 
  newvar = "edu_m")

## Now rename them
dh.renameVars(
  df = "mat_ed", 
  current_names = old_new$oldvar,
  new_names = old_new$newvar, 
  conns = conns["hgs"])

## ---- Drop column from main dataset ------------------------------------------
dh.dropCols(
  df = "bmi_poc", 
  vars = "edu_m", 
  type = "remove", 
  conns = conns["hgs"])

## ---- Remove age column from new dataset -------------------------------------
dh.dropCols(
  df = "mat_ed", 
  vars = "age_years.0_14", 
  type = "remove", 
  conns = conns["hgs"])

## ---- Merge back in ----------------------------------------------------------
ds.merge(
  x.name = "bmi_poc", 
  y.name = "mat_ed", 
  by.x.names = "child_id", 
  by.y.names = "child_id", 
  all.x = TRUE,
  all.y = FALSE,
  newobj = "bmi_poc", 
  datasources = conns["hgs"])

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_22")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_22")

################################################################################
# 24. Define cases again
################################################################################

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

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_23")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_23")

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


