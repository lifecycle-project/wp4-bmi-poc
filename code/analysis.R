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
library(purrr)

library(remotes)
install_github("lifecycle-project/dshelper", ref = "features")
library(dsHelper)

conns <- datashield.login(logindata, restore = "bmi_poc_sec_12")

# Issues to report
# 
# Can create variables with length >20, but can't remove them.
# ds.dataframe fill - factor levels incorrect
# ds.glm to retain study names in output rather than "study 1" etc
# Possible to add 'leave out out' option for metafor package?

################################################################################
# 1. Get descriptives for tables
################################################################################

## ---- Do separately in case it breaks ----------------------------------------
descriptives_exp <- dh.getStats(
   df = "analysis_df",
   vars = exp.vars, 
   conns = conns)

descriptives_out <- dh.getStats(
  df = "analysis_df",
  vars = out.vars, 
  conns = conns)

descriptives_cov <- dh.getStats(
  df = "analysis_df",
  vars = cov.vars, 
  conns = conns)

descriptives_oth <- dh.getStats(
  df = "analysis_df",
  vars = other.vars, 
  conns = conns)

## ---- Bind together ----------------------------------------------------------
descriptives <- list(
  descriptives_exp, descriptives_out, descriptives_cov, descriptives_oth) %>%
  pmap(bind_rows)
  
save.image()

################################################################################
# 2. Model definitions  
################################################################################

## Been round in circles about this, but decided the clearest way is to make a
## series of lists holding key model information, and then write a simple 
## function to convert this in to model definitions. Has the advantage that you
## can enter information once and use the function to make the model components
## as required by the IPD vs SLMA functions.


## ---- See available outcome and exposure data --------------------------------
descriptives$continuous %>% filter(variable == "bmi.730")
descriptives$continuous %>% filter(variable == "bmi.1461")
descriptives$continuous %>% filter(variable == "bmi.2922")
descriptives$continuous %>% filter(variable == "bmi.5113")
descriptives$continuous %>% filter(variable == "bmi.6544")

descriptives$categorical %>% filter(variable == "preg_dia")
descriptives$categorical %>% filter(variable == "preg_ht") %>% print(n = Inf)

## ---- Maternal education -----------------------------------------------------
cohorts <- names(conns)

mat_ed.mod <- list(
  bmi_24 = list(
    outcome = "bmi.730",
    exposure = "edu_m",
    covariates = c("age_days.730", "sex"),
    cohorts = cohorts[!cohorts == "hgs"]), 
  bmi_48 = list(
    outcome = "bmi.1461",
    exposure = "edu_m",
    covariates = c("age_days.1461", "sex"),
    cohorts = cohorts[!cohorts %in% c("dnbc", "hgs")]),
  bmi_96 = list(
    outcome = "bmi.2922",
    exposure = "edu_m", 
    covariates = c("age_days.2922", "sex"),
    cohorts = cohorts[!cohorts == "hgs"]),
  bmi_168 = list(
    outcome = "bmi.5113",
    exposure = "edu_m", 
    covariates = c("age_days.5113", "sex"),
    cohorts = cohorts[!cohorts == "hgs"]), 
  bmi_215 = list(
    outcome = "bmi.6544",
    exposure = "edu_m", 
    covariates = c("age_days.6544", "sex"),
    cohorts = cohorts[cohorts %in% c("alspac", "raine", "nfbc86")])
)


## ---- Area deprivation -------------------------------------------------------
descriptives[[1]] %>% 
  filter(variable == "area_dep" & valid_n > 0) %>%
  print(n = Inf)

env_coh <- c("alspac", "genr", "inma", "ninfea", "moba")

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
    exposure = "ndvi",
    covariates = c("age_days.730", green_covs),
    cohorts = env_coh), 
  bmi_48 = list(
    outcome = "bmi.1461",
    exposure = "ndvi",
    covariates = c("age_days.1461", green_covs),
    cohorts = env_coh),
  bmi_96 = list(
    outcome = "bmi.2922",
    exposure = "ndvi", 
    covariates = c("age_days.2922", green_covs),
    cohorts = env_coh), 
  bmi_168 = list(
    outcome = "bmi.5113",
    exposure = "ndvi", 
    covariates = c("age_days.5113", green_covs),
    cohorts = env_coh) 
)


## ---- Gestational diabetes ---------------------------------------------------
descriptives[[1]] %>% filter(variable == "preg_dia")

preg_dia_cov <- c("sex", "edu_m", "parity_bin", "agebirth_m_y", "prepreg_bmi")

preg_dia.mod <- list(
  bmi_24 = list(
    outcome = "bmi.730",
    exposure = "preg_dia",
    covariates = c("age_days.730", preg_dia_cov),
    cohorts = cohorts[!cohorts %in% c("chop", "hgs", "nfbc86")]), 
  bmi_48 = list(
    outcome = "bmi.1461",
    exposure = "preg_dia",
    covariates = c("age_days.1461", preg_dia_cov),
    cohorts = cohorts[!cohorts %in% c("chop", "dnbc", "hgs", "nfbc86")]),
  bmi_96 = list(
    outcome = "bmi.2922",
    exposure = "preg_dia", 
    covariates = c("age_days.2922", preg_dia_cov),
    cohorts = cohorts[!cohorts %in% c("chop", "hgs", "nfbc86")]), 
  bmi_168 = list(
    outcome = "bmi.5113",
    exposure = "preg_dia", 
    covariates = c("age_days.5113", preg_dia_cov),
    cohorts = cohorts[!cohorts %in% c("chop", "nfbc86")])
  )
  


################################################################################
# 3. Functions
################################################################################

## ---- Make formulae ----------------------------------------------------------
dh.makeGlmForm <- function(x, type, dummy_suff = "_dummy", data = "analysis_df"){
  
  if(type == "ipd"){
    
    if(length(x$covariates == 0)){
      
      mod <- list(
        model = paste0(
          x$outcome, "~", x$exposure, "+", paste0(x$cohorts[-1], "_dummy", collapse = "+")),
        cohorts = x$"cohorts"
      )
    } else if(length(x$covariates >0)){
    
    mod <- list(
      model = paste0(x$outcome, "~", x$exposure, "+", paste0(x$covariates, collapse = "+"), 
                     "+", paste0(x$cohorts[-1], "_dummy", collapse = "+")),
      cohorts = x$"cohorts"
    )
    
    }
  }
  
  else if(type == "slma"){
    
    if(length(x$covariates == 0)){
      mod <- list(
        model = paste0(x$outcome, "~", x$exposure), 
        cohorts = x$cohorts
      )
      
    } else if(length(x$covariates > 0)){
    mod <- list(
      model = paste0(x$outcome, "~", x$exposure, "+", paste0(x$covariates, collapse = "+")), 
      cohorts = x$cohorts
    )
    
    }
    
  }
  
  return(mod)
}


## ---- Run models -------------------------------------------------------------
dh.glmWrap <- function(x, type, dummy_suff = "_dummy", data = "analysis_df"){
  
  if(type == "ipd"){
    
    out <- ds.glm(
      formula = x$model,
      data = "analysis_df", 
      family = "gaussian", 
      datasources = conns[x$cohorts])
    
  }
  
  
  else if(type == "slma"){
    
    out <- ds.glmSLMA(
      formula = x$model,
      dataName = "analysis_df", 
      family = "gaussian",
      datasources = conns[x$cohorts])
  }
  
  return(out)
}


## ---- Change elements from model list ----------------------------------------
dh.changeForm <- function(model, var, type = c("add", "remove"), category){
  
  if(type == "remove"){
  
  model %>% map(function(x){list_modify(x, !!category := x[[category]][!x[[category]] %in% var])})
  } else if(type == "add"){

  model %>% map(function(x){list_modify(x, !!category := c(x[[category]], var))})
  }
  
}

################################################################################
# 4. Run core models  
################################################################################

## ---- Maternal education -----------------------------------------------------
mat_ed.fit <- list(
  ipd = mat_ed.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd"), 
  slma = mat_ed.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma")
)

## ---- Area deprivation -------------------------------------------------------
area_dep.fit <- list(
  ipd = area_dep.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd"),
  slma = area_dep.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma")
)

## ---- NDVI -------------------------------------------------------------------
ndvi.fit <- list(
  ipd = ndvi.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd"),
  slma = ndvi.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma")
)

## ---- Gestational diabetes ---------------------------------------------------
preg_dia.fit <- list(
  ipd = preg_dia.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd"),
  slma = preg_dia.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma")
)

save.image()

################################################################################
# 5. Test for interactions by sex  
################################################################################

## ---- Modify model definitions -----------------------------------------------
mat_ed_int.mod <- dh.changeForm(
  model = mat_ed.mod, 
  var = "edu_m*sex", 
  category = "covariates", 
  type = "add")
  
area_dep_int.mod <- dh.changeForm(
  model = area_dep.mod, 
  var = "area_dep*sex", 
  category = "covariates", 
  type = "add")

ndvi_int.mod <- dh.changeForm(
  model = ndvi.mod, 
  var = "ndvi*sex", 
  category = "covariates", 
  type = "add")

preg_dia_int.mod <- dh.changeForm(
  model = preg_dia.mod, 
  var = "preg_dia*sex", 
  category = "covariates", 
  type = "add")
  
## ---- Run interaction models -------------------------------------------------
mat_ed_int.fit <- list(
  ipd = mat_ed_int.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd"), 
  slma = mat_ed_int.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma")
)

area_dep_int.fit <- list(
  ipd = area_dep_int.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd"), 
  slma = area_dep_int.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma")
)

ndvi_int.fit <- list(
  ipd = ndvi_int.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd"), 
  slma = ndvi_int.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma")
)

preg_dia_int.fit <- list(
  ipd = preg_dia_int.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd"), 
  slma = preg_dia_int.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma")
)


################################################################################
# 6. Repeat analyses removing DNBC and MoBa  
################################################################################

## ---- Amend model definitions ------------------------------------------------
mat_ed_remove.mod <- dh.changeForm(
  model = mat_ed.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts", 
  type = "remove")

area_dep_remove.mod <- dh.changeForm(
  model = area_dep.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts", 
  type = "remove")

ndvi_remove.mod <- dh.changeForm(
  model = ndvi.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts", 
  type = "remove")

preg_dia_remove.mod <- dh.changeForm(
  model = preg_dia.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts", 
  type = "remove")


## ---- Run models -------------------------------------------------------------
mat_ed_remove.fit <- list(
  ipd = mat_ed_remove.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd"), 
  slma = mat_ed_remove.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma")
)

## ---- Area deprivation -------------------------------------------------------
area_dep_remove.fit <- list(
  ipd = area_dep_remove.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd"),
  slma = area_dep_remove.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma")
)

## ---- NDVI -------------------------------------------------------------------
ndvi_remove.fit <- list(
  ipd = ndvi_remove.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd"),
  slma = ndvi_remove.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma")
)

## ---- Gestational diabetes ---------------------------------------------------
preg_dia_remove.fit <- list(
  ipd = preg_dia_remove.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd"),
  slma = preg_dia_remove.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma")
)

save.image()
