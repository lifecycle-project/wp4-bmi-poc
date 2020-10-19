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
install_github("lifecycle-project/ds-helper", ref = "ds6")
library(dsHelper)

# This command is useful as it lists functionality currently available in DS.
#ls("package:dsBaseClient")
#ls("package:opal")
ls("package:dsHelper")
ls("package:dsBaseClient")

ls("package:DSI")

datashield.workspaces(opals)
################################################################################
# 1. Assign additional opal tables  
################################################################################

## ---- Create variable lists --------------------------------------------------

## non-repeated 
nonrep.vars <- c(
  "child_id", "sex", "coh_country", "preg_dia", "agebirth_m_y", "preg_smk", 
  "parity_m", "height_m", "prepreg_weight", "ethn3_m", "preg_ht", "ga_bj", 
  "ga_us", "cohort_id", "areases_quint_preg", "green_dist_preg", 
  "green_size_preg", "ndvi300_preg", "areases_tert_preg", "greenyn300_preg"
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
    opal_name = "genr",
    table = c(
      "lifecycle_1_0.1_0_genr_1_0_non_repeated",
      "lifecycle_1_0.1_0_genr_1_0_monthly_repeated",
      "lifecycle_1_0.1_0_genr_1_0_yearly_repeated")),
  tibble(
    opal_name = "ninfea",
    table = c(
      "lc_ninfea_core_2_0.2_0_core_1_0_non_rep",
      "lc_ninfea_core_2_0.2_0_core_1_0_monthly_rep",
      "lc_ninfea_core_2_0.2_0_core_1_0_yearly_rep")),
  tibble(
    opal_name = "raine",
    table = c(
      "lc_raine_core_2_1.2_1_core_1_0_non_rep",
      "lc_raine_core_2_1.2_1_core_1_0_monthly_rep",
      "lc_raine_core_2_1.2_1_core_1_0_yearly_rep")),
  tibble(
    opal_name = "gecko",
    table = c(
      "lc_gecko_core_2_1.2_1_core_1_1_non_rep",
      "lc_gecko_core_2_1.2_1_core_1_1_monthly_rep",
      "lc_gecko_core_2_1.2_1_core_1_1_yearly_rep")),
  tibble(
    opal_name = "chop",
    table = c(
      "lc_chop_core_2_1.2_1_core_non_rep_bmi_earlylife_poc",
      "lc_chop_core_2_1.2_1_core_monthly_rep_bmi_earlylife_poc",
      "lc_chop_core_2_1.2_1_core_yearly_rep_bmi_earlylife_poc")),
  tibble(
    opal_name = "moba",
    table = c(
      "lc_moba_core_2_0.2_0_core_non_rep_bmi_poc_study",
      "lc_moba_core_2_0.2_0_core_monthly_rep_bmi_poc_study",
      "lc_moba_core_2_0.2_0_core_yearly_rep_bmi_poc_study")), 
  tibble(
    opal_name = "inma",
    table = c(
      "lc_isglobal_core_2_1.2_1_core_1_0_non_rep_200217_1_bmi",
      "lc_isglobal_core_2_1.2_1_core_1_0_monthly_rep_200217_1_bmi",
      "lc_isglobal_core_2_1.2_1_core_1_0_yearly_rep_200217_1_bmi")),
  tibble(
    opal_name = "dnbc",
    table = c(
      "lc_dnbc_core_2_1.2_1_core_non_rep_tcadman_2020-lc19",
      "lc_dnbc_core_2_1.2_1_core_monthly_rep_tcadman_2020-lc19",
      "lc_dnbc_core_2_1.2_1_core_yearly_rep_tcadman_2020-lc19"))) %>%
  mutate(type = rep(c("nonrep", "monthrep", "yearrep"), 8))
         
## ---- Assign tables ----------------------------------------------------------
cohorts_tables %>%
  pwalk(function(opal_name, table, type){
  
  datashield.assign(
    conns = opals[opal_name], 
    symbol = type, 
    value = table, 
    variables = eval(parse(text = paste0(type, ".vars"))))
})

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(opals, "bmi_poc_sec_1")
opals <- datashield.login(logindata, restore = "bmi_poc_sec_1")

################################################################################
# 2. Fill missing variables
################################################################################

# Later on we will need to use ds.summary to get information about variables.
# However, at present it requires that all cohorts have every variable. We make
# things simpler therefore by creating empty variables where they are missing.

ds.dataFrameFill("nonrep", "nonrep")
ds.dataFrameFill("yearrep", "yearrep")

datashield.workspace_save(opals, "bmi_poc_sec_2")
opals <- datashield.login(logindata, restore = "bmi_poc_sec_2")

################################################################################
# 3. Fix variable classes
################################################################################

# "ds.dataFrameFill" has been updated in DS6 so that it creates the correct 
# class of variable. However there still remains an issue because it does not
# create the levels for the factor variable, so we will do that manually.

## ---- Non-repeated -----------------------------------------------------------

## Check descrepancy
dh.classDescrepancy(
  df = "nonrep", 
  vars = nonrep.vars)

## Change class
ds.asFactor("nonrep$ethn3_m", newobj.name = "ethn3_m_rev")
ds.asInteger("nonrep$ga_bj", newobj = "ga_bj_rev")
ds.asInteger("nonrep$ga_us", newobj = "ga_us_rev")
ds.asFactor("nonrep$preg_smk", newobj = "preg_smk_rev")
ds.asFactor("nonrep$preg_ht", newobj = "preg_ht_rev")
ds.asFactor("nonrep$areases_tert_preg", newobj = "areases_tert_preg_rev")
ds.asFactor("nonrep$areases_quint_preg", newobj = "areases_quint_preg_rev")
ds.asFactor("nonrep$greenyn300_preg", newobj = "greenyn300_preg_rev")

fixed_non <- c(
  "ethn3_m_rev", "ga_bj_rev", "ga_us_rev", "preg_smk_rev", 
  "preg_ht_rev", "areases_tert_preg_rev", "areases_quint_preg_rev", 
  "greenyn300_preg_rev")

## Join back in
ds.dataFrame(
  x= c("nonrep", fixed_non), 
  newobj = "nonrep"
)

## Check this has worked
dh.getStats(
  df = "nonrep", 
  vars = fixed_non
)

## ---- Yearly repeated --------------------------------------------------------

## Check descrepancy
dh.classDescrepancy(
  df = "yearrep", 
  vars = yearrep.vars)

## Change class
ds.asFactor("yearrep$areases_tert", "areases_tert_rev")
ds.asFactor("yearrep$areases_quint", "areases_quint_rev")
ds.asFactor("yearrep$greenyn300_", "greenyn300_rev")

fixed_year <- c("areases_tert_rev", "areases_quint_rev", "greenyn300_rev")

## Join back in
ds.dataFrame(
  x= c("yearrep", fixed_year), 
  newobj = "yearrep"
)

## Check this has worked
dh.getStats(
  df = "yearrep", 
  vars = fixed_year
)

## ---- Remove temporary objects -----------------------------------------------
dh.tidyEnv(
  obj = c(fixed_non, fixed_year), 
  type = "remove")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(opals, "bmi_poc_sec_3")
opals <- datashield.login(logindata, restore = "bmi_poc_sec_3")

################################################################################
# 4. Create baseline variables from non repeated tables 
################################################################################

## ---- Gestational age at birth -----------------------------------------------

# Moba has ga_us, whilst the other cohorts have ga_bj. Here we create one ga
# variable from these separate variables.

ds.assign(
  toAssign = "nonrep$ga_bj_rev", 
  newobj = "ga_all",
  datasources = opals[opals!="moba"]
) 

ds.assign(
  toAssign = "nonrep$ga_us_rev", 
  newobj = "ga_all",
  datasources = opals["moba"]
)

## ---- Maternal pre-pregnancy BMI ---------------------------------------------
ds.assign(
  toAssign='nonrep$prepreg_weight/(((nonrep$height_m/100))^2)', 
  newobj='prepreg_bmi'
)  

## ---- Parity -----------------------------------------------------------------

# We need to recode parity as a binary variable as there are issues later with 
# disclosive information when we run the models if we leave it ordinal.

ds.recodeValues(
  var.name = "nonrep$parity_m",
  values2replace.vector = c(0, 1, 2, 3, 4),
  new.values.vector = c(0, 1, 1, 1, 1),
  newobj = "parity_bin")

## ---- Combine these new variables with non-repeated dataframe ----------------
ds.dataFrame(
  x = c("nonrep", "ga_all", "prepreg_bmi", "parity_bin"),
  newobj = "nonrep_2")

dh.tidyEnv(
  obj = c("ga_all", "prepreg_bmi", "parity_bin"), 
  type = "remove")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(opals, "bmi_poc_sec_4")
opals <- datashield.login(logindata, restore = "bmi_poc_sec_4")

################################################################################
# 5. Create baseline variables from yearly repeated tables
################################################################################

## ---- Subset to keep observations where child's age == 0 ---------------------

## Make a vector containing zeros same length as age_years vector
ds.make(toAssign = "yearrep$age_years-yearrep$age_years",
        newobj = "zeros") 

## Now keep only rows which equal 0
ds.dataFrameSubset(
  df.name = "yearrep", 
  V1.name = "yearrep$age_years",
  V2.name = "zeros",
  Boolean.operator = "==",
  newobj = "baseline_vars")
  
ds.summary("baseline_vars")

## ---- Convert to wide format -------------------------------------------------

# For the actual analysis we will want our dataset to be in wide format 
ds.reShape(
  data.name = "baseline_vars",
  timevar.name = "age_years",
  idvar.name = "child_id",
  v.names = c("edu_m_", "greenyn300_rev", "green_dist_", "green_size_", 
              "ndvi300_", "areases_tert_rev", "areases_quint_rev"), 
  direction = "wide", 
  newobj = "baseline_wide")

## ---- Rename baseline_vars more sensible names -------------------------------

# Currently the baseline variables we've made don't have great names because
# they've been generated automatically by the reshape function. So we give them
# some better names using a function I wrote ("dh.renameVars"). This is a short-
# cut which creates the new variables using information provided in a table and
# joins these together in a dataframe.

ds.colnames("baseline_wide")
## First create a dataframe with old and new variable names
old_new <- tribble(
  ~oldvar, ~newvar,
  "edu_m_.0", "edu_m",
  "greenyn300_rev.0", "greenyn300_1",
  "green_dist_.0", "green_dist_1",
  "green_size_.0", "green_size_1", 
  "ndvi300_.0", "ndvi300_1",
  "areases_tert_rev.0", "areases_tert_1", 
  "areases_quint_rev.0", "areases_quint_1")
          
## Now rename them
dh.renameVars(
  df = "baseline_wide", 
  names = old_new)


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(opals, "bmi_poc_sec_5")
opals <- datashield.login(logindata, restore = "bmi_poc_sec_5")

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
datashield.workspace_save(opals, "bmi_poc_sec_6")
opals <- datashield.login(logindata, restore = "bmi_poc_sec_6")

################################################################################
# 7. Create cohort dummy  
################################################################################
coh_dummy <- tibble(
  cohort = c("moba_dummy", "raine_dummy", "ninfea_dummy", "gecko_dummy", 
             "inma_dummy", "chop_dummy", "dnbc_dummy"),
  value = c(110, 117, 103, 108, 102, 116, 106))

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
datashield.workspace_save(opals, "bmi_poc_sec_7")
opals <- datashield.login(logindata, restore = "bmi_poc_sec_7")

################################################################################
# 8. Create BMI, height and weight variables corresponding to age brackets 
################################################################################

## ---- Make BMI variables using the age in days variable ----------------------
dh.makeOutcome(
  df = "monthrep", 
  outcome = "bmi", 
  age_var = "height_age", 
  bands = c(0, 730, 730, 1461, 1461, 2922, 2922, 5113, 5113, 6544), 
  mult_action = "earliest")

datashield.workspace_save(opals, "bmi_poc_sec_8_a")
opals <- datashield.login(logindata, restore = "bmi_poc_sec_8_a")

## rename variables to make shorter length
old_new_2 <- tribble(
  ~oldvar, ~newvar,
  "height_", "ht",
  "weight_", "wt")
  
dh.renameVars(
  df = "monthrep", 
  names = old_new_2)

datashield.workspace_save(opals, "bmi_poc_sec_8_b")
opals <- datashield.login(logindata, restore = "bmi_poc_sec_8_b")

dh.makeOutcome(
  df = "monthrep", 
  outcome = "ht", 
  age_var = "height_age", 
  bands = c(0, 730, 730, 1461, 1461, 2922, 2922, 5113, 5113, 6544), 
  mult_action = "earliest", 
  remove_temp = FALSE)

datashield.workspace_save(opals, "bmi_poc_sec_8_c")
opals <- datashield.login(logindata, restore = "bmi_poc_sec_8_c")

dh.makeOutcome(
  df = "monthrep", 
  outcome = "wt", 
  age_var = "weight_age", 
  bands = c(0, 730, 730, 1461, 1461, 2922, 2922, 5113, 5113, 6544), 
  mult_action = "earliest", 
  remove_temp = FALSE)

datashield.workspace_save(opals, "bmi_poc_sec_8_d")
opals <- datashield.login(logindata, restore = "bmi_poc_sec_8_d")

ds.dataFrameFill("bmi_derived", "bmi_derived")
ds.dataFrameFill("ht_derived", "ht_derived")
ds.dataFrameFill("wt_derived", "wt_derived")


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(opals, "bmi_poc_sec_8")
opals <- datashield.login(logindata, restore = "bmi_poc_sec_8")

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
  names = old_new_3)

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

## Check all these are correct
check_age <- dh.getStats(
  df = "bmi_poc", 
  vars = c(old_new_3$newvar, age_convert$obj)
)

check_age$continuous %>%
  filter(cohort == "Combined")

# All looks reasonable


## ---- Now add in the cohort dummy variables ----------------------------------
ds.dataFrame(
  x = c('bmi_poc', coh_dummy$cohort), 
  newobj = 'bmi_poc'
  )
  
## ---- Create blank columns where data not available --------------------------
ds.dataFrameFill("bmi_poc", "bmi_poc")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(opals, "bmi_poc_sec_9")
opals <- datashield.login(logindata, restore = "bmi_poc_sec_9")


################################################################################
# 10. Create analysis dataset  
################################################################################

# So when it comes to write up the analysis, we need to be able to specify an
# analysis dataset as a subset of all data, e.g. "contained all participants 
# with at least one exposure and outcome"


## ---- First we specify vectors of exposures and outcomes ---------------------
exp.vars <- c(
  "edu_m", "ga_all", "preg_dia", "greenyn300_preg_rev", "green_dist_preg", 
  "green_size_preg", "ndvi300_preg", "greenyn300_1", "green_dist_1", 
  "green_size_1", "ndvi300_1")
           
out.vars <- c("bmi.730", "bmi.1461", "bmi.2922", "bmi.5113", "bmi.6544")
  
cov.vars <- c(
  "sex", "preg_smk_rev", "preg_ht_rev", "parity_bin", "ethn3_m_rev", "height_m", 
  "prepreg_bmi", "agebirth_m_y", "areases_tert_preg_rev", 
  "areases_quint_preg_rev", "areases_tert_1", "areases_quint_1")

other.vars <- c(
  "age_days.730", "age_days.1461", "age_days.2922", "age_days.5113", 
  "age_days.6544", "age_months.24", "age_months.48", "age_months.96", 
  "age_months.168", "age_months.215", "ht.730", "ht.1461", "ht.2922", 
  "ht.5113", "ht.6544", "wt.730", "wt.1461", "wt.2922", "wt.5113", "wt.6544", 
  coh_dummy$cohort)


## ---- Now we create vars indicating whether any non-missing values are present
dh.subjHasData(
  df = "bmi_poc", 
  vars = exp.vars, 
  new_label = "exposure")

dh.subjHasData(
  df = "bmi_poc", 
  vars = out.vars, 
  new_label = "outcome")

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

## Check how many valid cases to make sure it's plausible
ds.summary("valid_case")

## ---- Now we create a vector of all the variables we want to keep ------------
keep_vars <- c(exp.vars, out.vars, cov.vars, other.vars)

## ---- Drop variables we don't need -------------------------------------------
var_index <- dh.findVarsIndex(
      df = "bmi_poc", 
      vars = keep_vars)

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
      datasources = opals[.y]))

## ---- Check that this has worked ok ------------------------------------------
ds.summary("bmi_poc")
ds.summary("analysis_df")

datashield.workspace_save(opals, "bmi_poc_sec_10")

dh.tidyEnv(obj = c("analysis_df", "greenyn300_preg_rev_num_yn", 
                   "green_dist_preg_num_yn", "greenyn300_preg_rev_num", 
                   "areases_tert_preg_rev", "areases_quint_preg_rev"),
                   type = "keep")

datashield.workspace_save(opals, "bmi_poc_sec_10_clean")
