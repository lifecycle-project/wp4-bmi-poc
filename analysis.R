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


conns <- datashield.login(logindata, restore = "bmi_poc_sec_12")

# Issues to report
# 
# Can create variables with length >20, but can't remove them.
# ds.dataframe fill - factor levels incorrect
# ds.glm to retain study names in output rather than "study 1" etc
# Possible to add 'leave out out' option for metafor package?

################################################################################
# 1. Descriptives  
################################################################################
descriptives <- dh.getStats(
   df = "analysis_df",
   vars = c(exp.vars, out.vars, cov.vars, other.vars), 
   conns = conns
)

save.image()

################################################################################
# Model definitions  
################################################################################

## Been round in circles about this, but decided the clearest way is to make a
## series of lists holding key model information, and then write a simple 
## function to convert this in to model definitions. Has the advantage that you
## can enter information once and use the function to make the model components
## as required by the IPD vs SLMA functions.

descriptives[[2]] %>% filter(str_detect(variable, "bmi.730"))
descriptives[[2]] %>% filter(str_detect(variable, "bmi.1461"))
descriptives[[2]] %>% filter(str_detect(variable, "bmi.2922"))
descriptives[[2]] %>% filter(str_detect(variable, "bmi.5113"))
descriptives[[2]] %>% filter(str_detect(variable, "bmi.6544"))

descriptives[[1]] %>% filter(str_detect(variable, "preg_dia"))

## ---- Maternal education -----------------------------------------------------
cohorts <- names(conns)

mat_ed.mod <- list(
  bmi_24 = list(
    outcome = "bmi.730",
    exposure = "edu_m",
    covariates = c("age_days.730", "sex"),
    cohorts = cohorts[cohorts %in% c("sws", "nfbc86") == FALSE]), 
  bmi_48 = list(
    outcome = "bmi.1461",
    exposure = "edu_m",
    covariates = c("age_days.1461", "sex"),
    cohorts = cohorts[cohorts %in% c("dnbc", "sws", "nfbc86") == FALSE]),
  bmi_96 = list(
    outcome = "bmi.2922",
    exposure = "edu_m", 
    covariates = c("age_days.2922", "sex"),
    cohorts = cohorts[cohorts %in% c("dnbc", "sws", "elfe", "nfbc86") == FALSE]),
  bmi_168 = list(
    outcome = "bmi.5113",
    exposure = "edu_m", 
    covariates = c("age_days.5113", "sex"),
    cohorts = cohorts[cohorts %in% c("dnbc", "sws", "elfe", "nfbc86") == FALSE])
)


## ---- Area deprivation -------------------------------------------------------
descriptives[[1]] %>% 
  filter(variable == "area_dep" & valid_n > 0) %>%
  print(n = Inf)

env_coh <- c("moba", "ninfea", "inma")

area_dep.mod <- list(
  bmi_24 = list(
    outcome = "bmi.730",
    exposure = "area_dep",
    covariates = c("age_days.730", "sex"),
    cohorts = env_coh), 
  bmi_48 = list(
    outcome = "bmi.1461",
    exposure = "area_dep",
    covariates = c("age_days.1461", "sex"),
    cohorts = env_coh),
  bmi_96 = list(
    outcome = "bmi.2922",
    exposure = "area_dep", 
    covariates = c("age_days.2922", "sex"),
    cohorts = env_coh),
  bmi_168 = list(
    outcome = "bmi.5113",
    exposure = "area_dep", 
    covariates = c("age_days.5113", "sex"),
    cohorts = env_coh)
)


## ---- NDVI -------------------------------------------------------------------
green_covs <- c("sex", "edu_m", "parity_bin", "area_dep")

ndvi.mod <- list(
  bmi_24 = list(
    outcome = "bmi.730",
    exposure = "ndvi300_0_1",
    covariates = c("age_days.730", green_covs),
    cohorts = env_coh), 
  bmi_48 = list(
    outcome = "bmi.1461",
    exposure = "ndvi300_0_1",
    covariates = c("age_days.1461", green_covs),
    cohorts = env_coh),
  bmi_96 = list(
    outcome = "bmi.2922",
    exposure = "ndvi300_0_1", 
    covariates = c("age_days.2922", green_covs),
    cohorts = env_coh), 
  bmi_168 = list(
    outcome = "bmi.5113",
    exposure = "ndvi300_0_1", 
    covariates = c("age_days.5113", green_covs),
    cohorts = env_coh) 
)


## ---- Gestational diabetes ---------------------------------------------------
descriptives[[1]] %>% filter(variable == "preg_dia" & valid_n == 0)

preg_dia_cov <- c("sex", "edu_m", "parity_bin", "agebirth_m_y", "prepreg_bmi")

preg_dia.mod <- list(
  bmi_24 = list(
    outcome = "bmi.730",
    exposure = "preg_dia",
    covariates = c("age_days.730", preg_dia_cov),
    cohorts = cohorts[cohorts %in% c("chop", "nfbc86", "sws", "nfbc86") == FALSE]), 
  bmi_48 = list(
    outcome = "bmi.1461",
    exposure = "preg_dia",
    covariates = c("age_days.1461", preg_dia_cov),
    cohorts = cohorts[cohorts %in% c("chop", "nfbc86", "dnbc", "sws", "nfbc86") == FALSE]),
  bmi_96 = list(
    outcome = "bmi.2922",
    exposure = "preg_dia", 
    covariates = c("age_days.2922", preg_dia_cov),
    cohorts = cohorts[cohorts %in% c("chop", "nfbc86", "elfe", "sws", "nfbc86") == FALSE]),
  bmi_168 = list(
    outcome = "bmi.5113",
    exposure = "preg_dia", 
    covariates = c("age_days.5113", preg_dia_cov),
    cohorts = cohorts[cohorts %in% c("chop", "nfbc86", "elfe", "sws", "nfbc86") == FALSE])
  )
  


################################################################################
# Function
################################################################################

dh.regWrap <- function(x, type, dummy_suff = "_dummy", data = "analysis_df"){
  
  if(type == "ipd"){
    
    mod <- list(
      model = paste0(x$outcome, "~", x$exposure, "+", paste0(x$covariates, collapse = "+"), 
                     "+", paste0(x$cohorts[-1], "_dummy", collapse = "+")),
      cohorts = x$"cohorts"
    )
    
    out <- ds.glm(
      formula = mod$model,
      data = "analysis_df", 
      family = "gaussian", 
      datasources = conns[mod$cohorts])
    
}
  
  
  else if(type == "slma"){
    
    mod <- list(
      model = paste0(x$outcome, "~", x$exposure, "+", paste0(x$covariates, collapse = "+")), 
      cohorts = x$cohorts
    )
    
    out <- ds.glmSLMA(
      formula = mod$model,
      dataName = "analysis_df", 
      family = "gaussian",
      datasources = conns[mod$cohorts])
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
  ipd = ndvi.mod %>% map(dh.regWrap, type = "ipd"),
  slma = ndvi.mod %>% map(dh.regWrap, type = "slma")
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
################################################################################
# Prepare data  
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


datashield.workspace_save(conns, "bmi_poc_sec_12")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_12")

## ---- Function to remove sex terms from previous model definitions -----------
dh.removeTerm <- function(model, var, category){
  model %>% map(function(x){list_modify(x, !!category := x[[category]][!x[[category]] %in% var])})
}


## ---- Create amended models --------------------------------------------------
mat_ed_sex.mod <- dh.removeTerm(
  model = mat_ed.mod, 
  var = "sex", 
  category = "covariates")

ndvi_sex.mod <- dh.removeTerm(
  model = ndvi.mod, 
  var = "sex", 
  category = "covariates")

preg_dia_sex.mod <- dh.removeTerm(
  model = preg_dia.mod, 
  var = "sex", 
  category = "covariates")


################################################################################
# Run sex-stratified models  
################################################################################

## ---- Maternal education -----------------------------------------------------
mat_ed_m.fit <- list(
  ipd = mat_ed_sex.mod %>% map(dh.regWrap, type = "ipd", data = "analysis_df_m"), 
  slma = mat_ed_sex.mod %>% map(dh.regWrap, type = "slma", data = "analysis_df_m")
)

mat_ed_f.fit <- list(
  ipd = mat_ed_sex.mod %>% map(dh.regWrap, type = "ipd", data = "analysis_df_m"), 
  slma = mat_ed_sex.mod %>% map(dh.regWrap, type = "slma", data = "analysis_df_m")
)


## ---- NDVI -------------------------------------------------------------------
ndvi_m.fit <- list(
  ipd = ndvi_sex.mod %>% map(dh.regWrap, type = "ipd", data = "analysis_df_m"), 
  slma = ndvi_sex.mod %>% map(dh.regWrap, type = "slma", data = "analysis_df_m")
)

ndvi_f.fit <- list(
  ipd = ndvi_sex.mod %>% map(dh.regWrap, type = "ipd", data = "analysis_df_m"), 
  slma = ndvi_sex.mod %>% map(dh.regWrap, type = "slma", data = "analysis_df_m")
)



## ---- Pregnancy diabetes -----------------------------------------------------
preg_dia_m.fit <- list(
  ipd = preg_dia_sex.mod %>% map(dh.regWrap, type = "ipd", data = "analysis_df_m"), 
  slma = preg_dia_sex.mod %>% map(dh.regWrap, type = "slma", data = "analysis_df_m")
)

preg_dia <- list(
  ipd = preg_dia_sex.mod %>% map(dh.regWrap, type = "ipd", data = "analysis_df_m"), 
  slma = preg_dia_sex.mod %>% map(dh.regWrap, type = "slma", data = "analysis_df_m")
)


################################################################################
# Repeat analyses removing DNBC and MoBa  
################################################################################


## ---- Amend model definitions ------------------------------------------------
mat_ed_remove.mod <- dh.removeTerm(
  model = mat_ed.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts")

ndvi_remove.mod <- dh.removeTerm(
  model = ndvi.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts")

preg_dia_remove.mod <- dh.removeTerm(
  model = preg_dia.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts")



## ---- Create amended models --------------------------------------------------

## Maternal education
mat_ed_remove.fit <- list(
  ipd = mat_ed_remove.mod %>% map(dh.regWrap, type = "ipd"), 
  slma = mat_ed_remove.mod %>% map(dh.regWrap, type = "slma")
)

## NDVI
ndvi_remove.fit <- list(
  ipd = ndvi_remove.mod %>% map(dh.regWrap, type = "ipd"), 
  slma = ndvi_remove.mod %>% map(dh.regWrap, type = "slma")
)

## Pregnancy diabetes
preg_dia_remove.fit <- list(
  ipd = preg_dia_remove.mod %>% map(dh.regWrap, type = "ipd"), 
  slma = preg_dia_remove.mod %>% map(dh.regWrap, type = "slma")
)


save.image()




















dh.removeTerm(
  model = mat_ed.mod, 
  var = "ninfea", 
  category = "cohorts")


x = model


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
