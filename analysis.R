################################################################################
## Project: bmi-poc
## Script purpose: Condcut analysis
## Date: 29th April 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(DSI)
library(DSOpal)
library(dsBaseClient)
require(stringr)
require(dplyr)
require(magrittr)
require(dsHelper)
library(purrr)


opals <- datashield.login(logindata, restore = "bmi_poc_sec_10")

# Issues to report
# 
# Can create variables with length >20, but can't remove them.
# ds.dataframe fill - factor levels incorrect
# ds.glm to retain study names in output rather than "study 1" etc
# Possible to add 'leave out out' option for metafor package?

################################################################################
# 1. Descriptives  
################################################################################

## ---- Extract data -----------------------------------------------------------

# Now we extract descriptives using the function "getStats" which I wrote.
descriptives_ss <- dh.getStats(
   df = "analysis_df",
   vars = c(exp.vars, out.vars, cov.vars, other.vars), 
   conns = opals
)

save.image()


################################################################################
# 2. Create combined green exposures
################################################################################

## A small number of cohorts have this data. Moba has it in first year of life,
## NINFEA and INMA have it in pregnancy. I'll create a combined variable for 
## now.

green_preg <- c("greenyn300_preg_rev", "green_dist_preg", "green_size_preg", 
                "ndvi300_preg", "areases_tert_preg_rev", 
                "areases_quint_preg_rev")

green_1 <-c("greenyn300_1", "green_dist_1", "green_size_1", "ndvi300_1", 
            "areases_tert_1", "areases_quint_1")

green_comb <-  c("greenyn300_0_1", "green_dist_0_1", "green_size_0_1", 
                 "ndvi300_0_1", "areases_tert_0_1", "areases_quint_0_1")

green_convert <- tibble(
  formula = c(
    rep(paste0("analysis_df$", green_preg), 2), 
    paste0("analysis_df$", green_1)), 
  obj = rep(
   green_comb, 3),
  cohort = c(
    rep("ninfea", 6),
    rep("inma", 6), 
    rep("moba", 6))
  )

green_convert %>%
  pmap(function(formula, obj, cohort){
    
    ds.assign(
      toAssign = formula, 
      newobj = obj, 
      datasources = opals[cohort])    
    
  })

ds.dataFrame(
  x = c("analysis_df", green_comb), 
  newobj = "analysis_df", 
  datasources = opals[c("ninfea", "inma", "moba")])

ds.dataFrameFill("analysis_df", "analysis_df")
 
ds.asFactor("analysis_df$greenyn300_0_1", "greenyn300_0_1_f")
ds.asFactor("analysis_df$areases_tert_0_1", "areases_tert_0_1_f")
ds.asFactor("analysis_df$areases_quint_0_1", "areases_quint_0_1_f")

ds.dataFrame(
  x = c("analysis_df", "greenyn300_0_1_f", "areases_tert_0_1_f", 
        "areases_quint_0_1_f"), 
  newobj = "analysis_df")

green_comb_rev <- c("green_dist_0_1", "green_size_0_1", "ndvi300_0_1", 
                    "greenyn300_0_1_f", "areases_tert_0_1_f", 
                    "areases_quint_0_1_f")
   
green.tab <- dh.getStats(
  df = "analysis_df", 
  vars = green_comb_rev
)

datashield.workspace_save(opals, "bmi_poc_green")
opals <- datashield.login(logindata, restore = "bmi_poc_green")

################################################################################
# Model definitions  
################################################################################

## Been round in circles about this, but decided the clearest way is to make a
## series of lists holding key model information, and then write a simple 
## function to convert this in to model definitions. Has the advantage that you
## can enter information once and use the function to make the model components
## as required by the IPD vs SLMA functions.

## ---- Maternal education -----------------------------------------------------
mat_ed.mod <- list(
  bmi_24 = list(
    outcome = "bmi.730",
    exposure = "edu_m",
    covariates = c("age_days.730", "sex"),
    cohorts = cohorts), 
  bmi_48 = list(
    outcome = "bmi.1461",
    exposure = "edu_m",
    covariates = c("age_days.1461", "sex"),
    cohorts = cohorts[cohorts %in% "dnbc" == FALSE]),
  bmi_96 = list(
    outcome = "bmi.2922",
    exposure = "edu_m", 
    covariates = c("age_days.2922", "sex"),
    cohorts = cohorts),
  bmi_168 = list(
    outcome = "bmi.5113",
    exposure = "edu_m", 
    covariates = c("age_days.5113", "sex"),
    cohorts = cohorts)
)


## ---- NDVI -------------------------------------------------------------------
green_covs <- c("sex", "edu_m", "parity_bin", "areases_tert_0_1_f")

ndvi.mod <- list(
  bmi_24 = list(
    outcome = "bmi.730",
    exposure = "ndvi300_0_1",
    covariates = c("age_days.730", green_covs),
    cohorts = c("moba", "ninfea", "inma")), 
  bmi_48 = list(
    outcome = "bmi.1461",
    exposure = "ndvi300_0_1",
    covariates = c("age_days.1461", green_covs),
    cohorts = c("moba", "ninfea", "inma")),
  bmi_96 = list(
    outcome = "bmi.2922",
    exposure = "ndvi300_0_1", 
    covariates = c("age_days.2922", green_covs),
    cohorts = c("moba", "ninfea", "inma")), 
  bmi_168 = list(
    outcome = "bmi.5113",
    exposure = "ndvi300_0_1", 
    covariates = c("age_days.5113", green_covs),
    cohorts = c("moba", "ninfea", "inma")) 
)


## ---- Gestational diabetes ---------------------------------------------------
preg_dia_cov <- c("sex", "edu_m", "parity_bin", "agebirth_m_y", "prepreg_bmi")

preg_dia.mod <- list(
  bmi_24 = list(
    outcome = "bmi.730",
    exposure = "preg_dia",
    covariates = c("age_days.730", preg_dia_cov),
    cohorts = cohorts[cohorts %in% c("chop", "raine") == FALSE]), 
  bmi_48 = list(
    outcome = "bmi.1461",
    exposure = "preg_dia",
    covariates = c("age_days.1461", preg_dia_cov),
    cohorts = cohorts[cohorts %in% c("chop", "raine", "dnbc") == FALSE]),
  bmi_96 = list(
    outcome = "bmi.2922",
    exposure = "preg_dia", 
    covariates = c("age_days.2922", preg_dia_cov),
    cohorts = cohorts[cohorts %in% c("chop", "raine") == FALSE]),
  bmi_168 = list(
    outcome = "bmi.5113",
    exposure = "preg_dia", 
    covariates = c("age_days.5113", preg_dia_cov),
    cohorts = cohorts[cohorts %in% c("chop", "raine") == FALSE])
  )


################################################################################
# Function
################################################################################
dh.regWrap <- function(x, type, nodummy = "genr", dummy_suff = "_dummy", data = "analysis_df"){
  
  if(type == "ipd"){
    
    mod <- list(
      model = paste0(x$outcome, "~", x$exposure, "+", paste0(x$covariates, collapse = "+"), 
                     "+", paste0(x$cohorts[x$cohorts %in% nodummy == FALSE], "_dummy", collapse = "+")),
      cohorts = x$"cohorts"
    )
    
    out <- ds.glm(
      formula = mod$model,
      data = "analysis_df", 
      family = "gaussian", 
      datasources = opals[mod$cohorts])
    
}
  
  
  else if(type == "slma"){
    
    mod <- list(
      model = paste0(x$outcome, "~", x$exposure, "+", paste0(x$covariates, collapse = "+")), 
      cohorts = x$cohorts
    )
    
    out <- ds.glmSLMA(
      formula = mod$model,
      data = "analysis_df", 
      family = "gaussian",
      datasources = opals[mod$cohorts])
  }
  
  return(out)
}


################################################################################
# Run core models  
################################################################################

## ---- Maternal education -----------------------------------------------------
mat_ed.fit <- list(
  ipd = mat_ed.mod %>% map(dh.regWrap, type = "ipd"), 
  slma = mat_ed.mod %>% map(dh.regWrap, type = "slma")
)

## ---- NDVI -------------------------------------------------------------------
ndvi.fit <- list(
  ipd = ndvi.mod %>% map(dh.regWrap, type = "ipd", nodummy = "ninfea"),
  slma = ndvi.mod %>% map(dh.regWrap, type = "slma", nodummy = "ninfea")
)

## ---- Gestational diabetes ---------------------------------------------------
preg_dia.fit <- list(
  ipd = preg_dia.mod %>% map(dh.regWrap, type = "ipd"), 
  slma = preg_dia.mod %>% map(dh.regWrap, type = "slma")
)

save.image()


################################################################################
# Repeat analyses stratified by sex  
################################################################################

## ---- Create sex-stratified subsets ------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$sex", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "analysis_df_m")

ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$sex", 
  V2.name = "2", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "analysis_df_f")

## Hack of other function: can generalise

dh.amendSex <- function(model, var){
  model %>% map(function(x){list_modify(x, covariates = x$covariates[!x$covariates %in% var])})
}

mat_ed_sex.mod <- dh.amendSex(mat_ed.mod, "sex")

mat_ed_m.fit <- list(
  ipd = mat_ed_sex.mod %>% map(dh.regWrap, type = "ipd", data = "analysis_df_m"), 
  slma = mat_ed_sex.mod %>% map(dh.regWrap, type = "slma", data = "analysis_df_m")
)

mat_ed_f.fit <- list(
  ipd = mat_ed_sex.mod %>% map(dh.regWrap, type = "ipd", data = "analysis_df_m"), 
  slma = mat_ed_sex.mod %>% map(dh.regWrap, type = "slma", data = "analysis_df_m")
)




################################################################################
# Repeat analyses removing DNBC and MoBa  
################################################################################

## ---- Function to do one-removed ---------------------------------------------

## Function to modify the included cohorts in the model specification


dh.removeCohort <- function(model){

dh.amendModel <- function(model, cohort){
model %>% map(function(x){list_modify(x, cohorts = x$cohorts[!x$cohorts %in% cohort])})
}

remove.mod <- list(
  dnbc = dh.amendModel(model, cohort = "dnbc"), 
  moba = dh.amendModel(model, cohort = "moba"),
  both = dh.amendModel(model, cohort = c("dnbc", "moba"))
)
 
out <- list(
  dnbc = list(
    ipd = remove.mod$dnbc %>% map(dh.regWrap, type = "ipd"),
    slma = remove.mod$dnbc %>% map(dh.regWrap, type = "slma")), 
  moba = list(
    ipd = remove.mod$moba %>% map(dh.regWrap, type = "ipd"),
    slma = remove.mod$moba %>% map(dh.regWrap, type = "slma")), 
  both = list(
    ipd = remove.mod$both %>% map(dh.regWrap, type = "ipd"),
    slma = remove.mod$both %>% map(dh.regWrap, type = "slma"))
  )

return(out)
}

mat_ed_remove.fit <- dh.removeCohort(mat_ed.mod)


# Let's just show ipd one-remove sensitivity
remove_n <-  mat_ed.fit[[1]] %>% 
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("edu_m2", "edu_m3")) %>%
  mutate(removed = "none")

remove_d <- mat_ed_remove.fit$dnbc[[1]] %>% 
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("edu_m2", "edu_m3")) %>%
  mutate(removed = "dnbc")

remove_m <- mat_ed_remove.fit$moba[[1]] %>% 
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("edu_m2", "edu_m3")) %>%
  mutate(removed = "moba")

remove_b <- mat_ed_remove.fit$both[[1]] %>% 
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("edu_m2", "edu_m3")) %>%
  mutate(removed = "both")

mat_ed_removed.tab <- bind_rows(remove_n, remove_d, remove_m, remove_b)

colnames(mat_ed_removed.tab) <- c(
  "variable", "age_0_24", "age_25_48", "age_49_96", "age_96_168", "removed")
