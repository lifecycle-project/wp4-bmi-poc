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
#library(remotes)
#install_github("lifecycle-project/ds-helper", ref = "maintenance")
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
      "lc_eden_core_2_1.Project6_WP4_non_rep", 
      "lc_eden_core_2_1.Project6_WP4_monthly_rep", 
      "lc_eden_core_2_1.Project6_WP4_yearly_rep")),
  tibble(
    cohort = "elfe",
    table = c(
      "lc_elfe_core_2_1.Project6_WP4_non_rep", 
      "lc_elfe_core_2_1.Project6_WP4_monthly_rep", 
      "lc_elfe_core_2_1.Project6_WP4_yearly_rep")),
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
      "lc_moba_core_2_0.2_0_core_non_rep_bmi_poc_study",
      "lc_moba_core_2_0.2_0_core_monthly_rep_bmi_poc_study",
      "lc_moba_core_2_0.2_0_core_yearly_rep_bmi_poc_study")), 
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
# 2. Check available data  
################################################################################

## ---- Non-repeated -----------------------------------------------------------
non_class <- dh.classDiscrepancy(
  df = "nonrep", 
  vars = nonrep.vars)

non_class %>% filter(discrepancy == "yes")

## ---- Monthly repeated -------------------------------------------------------
month_class <- dh.classDiscrepancy(
  df = "monthrep", 
  vars = monthrep.vars, 
  conns = conns)

month_class %>% filter(discrepancy == "yes")

## ---- Yearly repeated --------------------------------------------------------
year_class <- dh.classDiscrepancy(
  df = "yearrep", 
  vars = yearrep.vars, 
  conns = conns)

year_class %>% filter(discrepancy == "yes")


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

non_class_filled %>% filter(discrepancy == "yes")

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
# 7. Create BMI, height and weight variables corresponding to age brackets 
################################################################################
cohorts_1 <- c("alspac", "bib", "chop")
cohorts_2 <- c("dnbc", "gecko", "genr")
cohorts_3 <- c("hgs", "inma", "moba")
cohorts_4 <- c("nfbc86", "ninfea", "raine", "sws")

bands <- c(0, 730, 730, 1461, 1461, 2922, 2922, 5113, 5113, 6544)

## ---- BMI --------------------------------------------------------------------
dh.makeOutcome(
  df = "monthrep", 
  outcome = "bmi", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l",
  conns = conns[cohorts_1])

datashield.workspace_save(conns[cohorts_1], "bmi_poc_sec_4_a")

dh.makeOutcome(
  df = "monthrep", 
  outcome = "bmi", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l",
  conns = conns[cohorts_2])

datashield.workspace_save(conns[cohorts_2], "bmi_poc_sec_4_a")

dh.makeOutcome(
  df = "monthrep", 
  outcome = "bmi", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l",
  conns = conns[cohorts_3])

datashield.workspace_save(conns[cohorts_3], "bmi_poc_sec_4_a")

dh.makeOutcome(
  df = "monthrep", 
  outcome = "bmi", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l",
  conns = conns[cohorts_4])

datashield.workspace_save(conns[cohorts_4], "bmi_poc_sec_4_a")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_4_a")

## ---- Rename variables to shorter length -------------------------------------
old_new_2 <- tribble(
  ~oldvar, ~newvar,
  "height_", "ht",
  "weight_", "wt")

dh.renameVars(
  df = "monthrep", 
  names = old_new_2, 
  conns = conns)

datashield.workspace_save(conns, "bmi_poc_sec_4_b")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_4_b")

## ---- Height -----------------------------------------------------------------
dh.makeOutcome(
  df = "monthrep", 
  outcome = "ht", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l",
  conns = conns[cohorts_1])

datashield.workspace_save(conns[cohorts_1], "bmi_poc_sec_4_c")

dh.makeOutcome(
  df = "monthrep", 
  outcome = "ht", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l",
  conns = conns[cohorts_2])

datashield.workspace_save(conns[cohorts_2], "bmi_poc_sec_4_c")

dh.makeOutcome(
  df = "monthrep", 
  outcome = "ht", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l",
  conns = conns[cohorts_3])

datashield.workspace_save(conns[cohorts_3], "bmi_poc_sec_4_c")

dh.makeOutcome(
  df = "monthrep", 
  outcome = "ht", 
  age_var = "height_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l",
  conns = conns[cohorts_4])

datashield.workspace_save(conns[cohorts_4], "bmi_poc_sec_4_c")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_4_c")

## ---- Weight -----------------------------------------------------------------
dh.makeOutcome(
  df = "monthrep", 
  outcome = "wt", 
  age_var = "weight_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l",
  conns = conns[cohorts_1])

datashield.workspace_save(conns[cohorts_1], "bmi_poc_sec_4_d")

dh.makeOutcome(
  df = "monthrep", 
  outcome = "wt", 
  age_var = "weight_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l",
  conns = conns[cohorts_2])

datashield.workspace_save(conns[cohorts_2], "bmi_poc_sec_4_d")

dh.makeOutcome(
  df = "monthrep", 
  outcome = "wt", 
  age_var = "weight_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l",
  conns = conns[cohorts_3])

datashield.workspace_save(conns[cohorts_3], "bmi_poc_sec_4_d")

dh.makeOutcome(
  df = "monthrep", 
  outcome = "wt", 
  age_var = "weight_age", 
  bands = bands, 
  mult_action = "earliest", 
  id_var = "child_id",
  band_action = "ge_l",
  conns = conns[cohorts_4])

datashield.workspace_save(conns[cohorts_4], "bmi_poc_sec_4_d")


## ---- Load progress ----------------------------------------------------------
conns <- datashield.login(logindata, restore = "bmi_poc_sec_4_d")

################################################################################
# 8. Fix factor variables  
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
 vars = fact_repair %>% filter(df == "nonrep") %>% pull(var), 
 new_df_name = "nonrep", 
 comp_var = "child_id", 
 type = "remove", 
 conns = conns
)

dh.dropCols(
  df = "yearrep", 
  vars = fact_repair %>% filter(df == "yearrep") %>% pull(var), 
  new_df_name = "yearrep", 
  comp_var = "child_id", 
  type = "remove", 
  conns = conns
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_5a")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_5a")

## ---- Join correct variables back --------------------------------------------
ds.dataFrame(
  x = c("nonrep", fact_repair %>% filter(df == "nonrep") %>% pull(var)),
  newobj = "nonrep_fix")

ds.dataFrame(
  x = c("yearrep", fact_repair %>% filter(df == "yearrep") %>% pull(var)),
  newobj = "yearrep_fix")


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_5")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_5")

################################################################################
# 9. Create baseline variables from non repeated tables 
################################################################################

## ---- Gestational age at birth -----------------------------------------------

## Check which cohorts have which data
ga_sum <- dh.getStats(
  df = "nonrep_fix", 
  vars = c("ga_bj", "ga_us"),
  conns = conns
)

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
  x = c("nonrep_fix", "ga_all", "prepreg_bmi", "parity_bin"),
  newobj = "nonrep_2")

dh.tidyEnv(
  obj = c("ga_all", "prepreg_bmi", "parity_bin"), 
  type = "remove", 
  conns = conns)


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_6")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_6")

################################################################################
# 10. Create baseline variables from yearly repeated tables
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
  names = old_new, 
  conns = conns)


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_7")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_7")

################################################################################
# 8. Create cohort dummy  
################################################################################

## ---- Get cohort codes -------------------------------------------------------
coh_codes <- dh.getStats(
  df = "nonrep",
  vars = "cohort_id", 
  conns = conns
)

codes.tab <- coh_codes$categorical %>% filter(value != 0 & cohort != "combined") 

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
datashield.workspace_save(conns, "bmi_poc_sec_8")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_8")

################################################################################
# 9. Merge various datasets  
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
  y.name = "bmi_derived",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  newobj = "bmi_poc"
)

## ---- Now the height and weight data -----------------------------------------
ds.merge(
  x.name = "bmi_poc",
  y.name = "ht_derived",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  newobj = "bmi_poc"
)

ds.merge(
  x.name = "bmi_poc",
  y.name = "wt_derived",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  newobj = "bmi_poc"
)

## ---- Rename variables -------------------------------------------------------
ds.dataFrameFill("bmi_poc", "bmi_poc")
# Need to create sensibly names monthly and yearly age variables.

## First rename the age variable in days
old_new_3 <- tribble(
  ~oldvar, ~newvar,
  "age.730.x", "age_days.730",
  "age.1461.x", "age_days.1461",
  "age.2922.x", "age_days.2922",
  "age.5113.x", "age_days.5113",
  "age.6544.x", "age_days.6544")

dh.renameVars(
  df = "bmi_poc", 
  names = old_new_3, 
  conns = conns)

## Now make a version of the age variables in months.
age_convert <- tibble(
  formula = c(
    "bmi_poc$age.730.x/30.4167",
    "bmi_poc$age.1461.x/30.4375",
    "bmi_poc$age.2922.x/30.4375",
    "bmi_poc$age.5113.x/30.4345",
    "bmi_poc$age.6544.x/30.4372"), 
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
datashield.workspace_save(conns, "bmi_poc_sec_9")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_9")


################################################################################
# 10. Create combined environmental exposures  
################################################################################

# MoBa has environmental exposures at year 0-1, whilst INMA and NINFEA have
# these recorded in pregnancy. Let's create combined exposures.

## ---- NDVI -------------------------------------------------------------------
ds.assign(
  toAssign = "bmi_poc$ndvi300_preg", 
  newobj = "ndvi",
  datasources = conns[c("alspac", "genr", "inma", "ninfea")]
) 

ds.assign(
  toAssign = "bmi_poc$ndvi300_1", 
  newobj = "ndvi",
  datasources = conns["moba"]
)


## ---- Area deprivation -------------------------------------------------------
ds.assign(
  toAssign = "bmi_poc$areases_tert_preg", 
  newobj = "area_dep_tmp",
  datasources = conns[c("alspac", "genr", "inma", "ninfea")]
) 

ds.assign(
  toAssign = "bmi_poc$areases_tert_1", 
  newobj = "area_dep_tmp",
  datasources = conns["moba"]
)


## ---- Join back in -----------------------------------------------------------
ds.dataFrame(
  x = c('bmi_poc', "ndvi", "area_dep_tmp"), 
  newobj = 'bmi_poc', 
  datasources = conns[c("moba", "alspac", "genr", "inma", "ninfea")]
)


## ---- Fill missing variables -------------------------------------------------
ds.dataFrameFill("bmi_poc", "bmi_poc")

## ---- Fix factor variables ---------------------------------------------------
ds.asFactor(
  input.var.name = "bmi_poc$area_dep_tmp",
  newobj.name = "area_dep")

ds.dataFrame(
  x = c('bmi_poc', "area_dep"), 
  newobj = 'bmi_poc')


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_10")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_10")

################################################################################
# 11. Fix pregnancy diabetes variable
################################################################################
ds.asFactor(
  input.var.name = "bmi_poc$preg_dia", 
  newobj.name = "preg_dia"
)

dh.dropCols(
  df = "bmi_poc", 
  vars = "preg_dia", 
  type = "remove", 
  comp_var = "child_id",
  new_df_name = "bmi_poc"
)

ds.cbind(
  x = c("bmi_poc", "preg_dia"), 
  newobj = "bmi_poc"
)

dh.getStats(
  df = "bmi_poc", 
  vars = "preg_dia"
)

## ---- Check that this has worked ok ------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_11")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_11")

################################################################################
# 10. Create analysis dataset  
################################################################################

# So when it comes to write up the analysis, we need to be able to specify an
# analysis dataset as a subset of all data, e.g. "contained all participants 
# with at least one exposure and outcome"


## ---- First we specify vectors of exposures and outcomes ---------------------
exp.vars <- c(
  "edu_m", "ga_all", "preg_dia", "ndvi", "area_dep")
           
out.vars <- c("bmi.730", "bmi.1461", "bmi.2922", "bmi.5113", "bmi.6544")
  
cov.vars <- c(
  "sex", "preg_smk", "preg_ht", "parity_bin", "ethn3_m", "height_m", 
  "prepreg_bmi", "agebirth_m_y")

other.vars <- c(
  "child_id", "age_days.730", "age_days.1461", "age_days.2922", "age_days.5113", 
  "age_days.6544", "age_months.24", "age_months.48", "age_months.96", 
  "age_months.168", "age_months.215", "ht.730", "ht.1461", "ht.2922", 
  "ht.5113", "ht.6544", "wt.730", "wt.1461", "wt.2922", "wt.5113", "wt.6544", 
  coh_dummy$cohort)


## ---- Now we create vars indicating whether any non-missing values are present
dh.subjHasData(
  df = "bmi_poc", 
  vars = exp.vars, 
  new_label = "exposure", 
  conns = conns)

dh.subjHasData(
  df = "bmi_poc", 
  vars = out.vars, 
  new_label = "outcome", 
  conns = conns)

## ---- Next create another variable indicating whether a valid case -----------
ds.make(
  toAssign = "any_exposure+any_outcome", 
  newobj = "n_exp_out")

ds.Boole(
  V1 = "n_exp_out", 
  V2 = "2", 
  Boolean.operator = "==", 
  na.assign = 0, 
  newobj = "valid_case")

datashield.workspace_save(conns, "bmi_poc_sec_11a")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_11a")

## Check how many valid cases to make sure it's plausible
ds.summary("valid_case")

## ---- Now we create a vector of all the variables we want to keep ------------
keep_vars <- c(exp.vars, out.vars, cov.vars, other.vars)

## ---- Drop variables we don't need -------------------------------------------
var_index <- dh.findVarsIndex(
      df = "bmi_poc", 
      vars = keep_vars, 
      conns = conns)

## Now finally we subset based on valid cases and required variables
var_index %>%
  imap(
    ~ds.dataFrameSubset(
      df.name = "bmi_poc", 
      V1.name = "valid_case", 
      V2.name = "1", 
      Boolean.operator = "==", 
      keep.cols = .x,
      keep.NAs = FALSE, 
      newobj = "analysis_df", 
      datasources = conns[.y]))

## ---- Check that this has worked ok ------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_12")

################################################################################
# 11. Additional fixes  
################################################################################








