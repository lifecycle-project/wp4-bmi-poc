################################################################################
## Project: bmi-poc
## Script purpose: Prepare data for analysis    
## Date: 29th April 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

require(opal)
require(dsBaseClient)
library(purrr)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(remotes)
install_github("lifecycle-project/ds-helper")
library(dsHelper)

# This command is useful as it lists functionality currently available in DS.
#ls("package:dsBaseClient")
#ls("package:opal")
#ls("package:dsHelper")
#ls("package:dsBaseClient")

################################################################################
# 1. Assign additional opal tables  
################################################################################

## ---- Create variable lists --------------------------------------------------

## non-repeated 
nonrep.vars <- c(
  "child_id", "sex", "coh_country", "preg_dia", "agebirth_m_y", "preg_smk", 
  "parity_m", "height_m", "prepreg_weight", "ethn3_m", "preg_ht", "ga_bj", 
  "ga_us", "cohort_id")

## monthly repeated
monthrep.vars <- c(
  "child_id", "age_years", "age_months", "height_", "weight_", "height_age", 
  "weight_age")

## yearly repeated
yearrep.vars <- c(
  "child_id", "edu_m_", "edu_f1_", "age_years", "ndvi300_", "green_dist_", 
  "green_size_", "greenyn300_", "areases_tert_")


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
      "lc_raine_core_2_0.2_0_core_1_0_non_rep",
      "lc_raine_core_2_0.2_0_core_1_0_monthly_rep",
      "lc_raine_core_2_0.2_0_core_1_0_yearly_rep")),
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
      "lc_isglobal_core_2_1.2_1_core_1_0_yearly_rep_200217_1_bmi"))) %>%
  mutate(type = rep(c("nonrep", "monthrep", "yearrep"), 7))
         
## ---- Assign tables ----------------------------------------------------------
cohorts_tables %>%
  filter(opal_name!= "chop") %>%
  pwalk(function(opal_name, table, type){
  
  datashield.assign(
    opal = opals[opal_name], 
    symbol = type, 
    value = table, 
    variables = eval(parse(text = paste0(type, ".vars"))))
})

ds.ls()

################################################################################
# 2. Sort out cases where not all cohorts have these variables
################################################################################

# Later on we will need to use ds.summary to get information about variables.
# However, at present it requires that all cohorts have every variable. We make
# things simpler therefore by creating empty variables where they are missing.


## ---- Fill blank variables ---------------------------------------------------
ds.dataFrameFill("nonrep", "nonrep")
ds.dataFrameFill("monthrep", "monthrep")
ds.dataFrameFill("yearrep", "yearrep")


## ---- Check for class discrepancies ------------------------------------------

# At present ds.dataFrameFill doesn't make the filled variable the same class
# as the original. Again this can cause problems later, so here we correct the 
# class of the blank variable. 

## First we check for discrepancies
dh.classDescrepancy(
  df = "nonrep", 
  vars = nonrep.vars)

dh.classDescrepancy(
  df = "monthrep", 
  vars = monthrep.vars)

dh.classDescrepancy(
  df = "yearrep", 
  vars = yearrep.vars)

## ---- Fix problem variables --------------------------------------------------

# Now we fix the problem variables and combine them with their original data
# frame

## Non-repeated
ds.asFactor("nonrep$ethn3_m", newobj.name = "ethn3_m_rev")
ds.asInteger("nonrep$ga_bj", newobj = "ga_bj_rev")
ds.asInteger("nonrep$ga_us", newobj = "ga_us_rev")
ds.asInteger("nonrep$preg_smk", newobj = "preg_smk_rev")
ds.asInteger("nonrep$preg_ht", newobj = "preg_ht_rev")

names(opals) %>%
  map(
    ~ds.dataFrame(
      x= c("nonrep", "ethn3_m_rev", "ga_bj_rev", "ga_us_rev", "preg_smk_rev", 
           "preg_ht_rev"), 
      newobj = "nonrep", 
      datasources = opals[.])
  )

## Yearly repeated
ds.asFactor("yearrep$greenyn300_", newobj.name = "greenyn300_rev_")
ds.asFactor("yearrep$areases_tert_", newobj.name = "areases_tert_rev_")

names(opals) %>%
  map(
    ~ds.dataFrame(
      x = c("yearrep", "greenyn300_rev_", "areases_tert_rev_"), 
      newobj = "yearrep", datasources = opals[.])
  )
  
## Check this has worked
dh.classDescrepancy(
  df = "nonrep", 
  vars = c("nonrep", "ethn3_m_rev", "ga_bj_rev", "ga_us_rev", "preg_smk_rev", 
           "preg_ht_rev"))

dh.classDescrepancy(
  df = "yearrep", 
  vars = c("greenyn300_rev_", "areases_tert_rev_"))

dh.tidyEnv(
  obj = c("areases_tert_rev_", "ethn3_m_rev", "ga_bj_rev", "greenyn300_rev_", 
          "ga_us_rev", "preg_smk_rev", "preg_ht_rev"), 
  type = "remove")


################################################################################
# 3. Create baseline variables from non repeated tables 
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
  toAssign = "nonrep$ga_us", 
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

ds.recodeLevels(
  "nonrep$parity_m", 
  newCategories = c(0, 1, 1, 1, 1),
  newobj = "parity_bin")


## ---- Combine these new variables with non-repeated dataframe ----------------
names(opals) %>%
  map(
    ~ds.dataFrame(
    x = c("nonrep", "ga_all", "prepreg_bmi", "parity_bin"),
    newobj = "nonrep_2", 
    datasources = opals[.])
)

dh.classDescrepancy(df = "nonrep_2")
dh.tidyEnv(
  obj = c("ga_all", "prepreg_bmi", "parity_bin"), 
  type = "remove")


## ---- Age at measurement -----------------------------------------------------

# I don't currently have access to the INMA age variable from the monthrep 
# table. I will use "height_age" instead for now.

ds.assign(
  toAssign = "monthrep$age_months", 
  newobj = "age_months_all",
  datasources = opals[opals!="inma"]
)

ds.assign(
  toAssign = "monthrep$height_age/30.4368", 
  newobj = "age_months_all",
  datasources = opals[opals="inma"]
)

names(opals) %>%
  map(
    ~ds.dataFrame(
      x = c("monthrep", "age_months_all"),
      newobj = "monthrep_2", 
      datasources = opals[.])
  )

ds.summary("monthrep_2$age_months_all")

dh.tidyEnv(
  obj = "age_months_all", 
  type = "remove")

################################################################################
# 4. Create baseline variables from yearly repeated tables
################################################################################

## ---- Subset to keep observations where child's age == 0 ---------------------
ds.subset(
  x = 'yearrep', 
  subset = "baseline_vars", 
  logicalOperator = 'age_years==', 
  threshold = 0)


## ---- Convert to wide format -------------------------------------------------

# For the actual analysis we will want our dataset to be in wide format 
ds.reShape(
  data.name = "baseline_vars",
  timevar.name = "age_years",
  idvar.name = "child_id",
  v.names = c("edu_m_", "edu_f1_", "greenyn300_rev_", "green_dist_", 
              "green_size_", "ndvi300_", "areases_tert_rev_"), 
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
  "edu_f1_.0", "edu_f",
  "edu_m_.0", "edu_m",
  "greenyn300_rev_.0", "greenyn300",
  "green_dist_.0", "green_dist",
  "green_size_.0", "green_size", 
  "ndvi300_.0", "ndvi300",
  "areases_tert_rev_.0", "areases_tert")
          
## Now rename them
dh.renameVars(
  df = "baseline_wide", 
  names = old_new)

################################################################################
# 5. Calculate BMI scores from monthly repeated measures data
################################################################################

## ---- First we derive BMI scores ---------------------------------------------
ds.assign(
  toAssign='monthrep$weight_/((monthrep$height_/100)^2)', 
  newobj='bmi'
)  


## ---- Now join these back to the dataframe -----------------------------------
names(opals) %>%
  map(
    ~ds.dataFrame(
      x = c('bmi', 'monthrep_2'), 
      newobj = 'monthrep_2',
      datasources = opals[.]
      )
  )

ds.rm("bmi")

datashield.workspace_save(opals, "df_04_08_20")

opals <- datashield.login(all.logdata, restore = "df_04_08_20")

################################################################################
# 6. Create BMI variables corresponding to age brackets 
################################################################################
dh.makeOutcome(
  df = "monthrep_2", 
  outcome = "bmi", 
  age_var = "age_months_all", 
  bands = c(0, 24, 25, 48, 49, 96, 97, 168, 169, 215), 
  mult_action = "earliest")

ds.dataFrameFill("bmi_derived", "bmi_derived_2")

################################################################################
# 7. Merge various datasets  
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
  y.name = "bmi_derived_2",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  newobj = "bmi_poc"
)

## ---- Create blank columns where data not available --------------------------
ds.dataFrameFill("bmi_poc", "bmi_poc")

################################################################################
# 8. Create analysis dataset  
################################################################################

# So when it comes to write up the analysis, we need to be able to specify an
# analysis dataset as a subset of all data, e.g. "contained all participants 
# with at least one exposure and outcome"


## ---- First we specify vectors of exposures and outcomes ---------------------
exp.vars <- c("edu_m", "ga_all", "preg_dia", "greenyn300", "green_dist", 
              "green_size", "ndvi300")  

out.vars <- c("bmi.24", "bmi.48", "bmi.96", "bmi.168")

cov.vars <- c("sex", "preg_smk_rev", "preg_ht_rev", "parity_bin", "ethn3_m_rev", 
              "height_m", "prepreg_bmi", "agebirth_m_y", "areases_tert")

other.vars <- c("age.24", "age.48", "age.96", "age.168")


## ---- Now we create vars indicating whether any non-missing values are present
dh.subjHasData(
  df = "bmi_poc", 
  vars = exp.vars, 
  new_label = "exposure")

dh.subjHasData(
  df = "bmi_poc", 
  vars = out.vars, 
  new_label = "outcome")

ds.summary("n_outcome")
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
ds.summary("any_exposure")
ds.summary("any_outcome")


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

dh.tidyEnv(obj = "analysis_df", type = "keep")

datashield.workspace_save(opals, "final_df")
opals <- datashield.login(all.logdata, restore = "final_df")

ds.ls()

