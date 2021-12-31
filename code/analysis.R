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
# 1. Get descriptives for analysis sample
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
# 2. Get descriptives for full sample  
################################################################################
descriptives_exp_full <- dh.getStats(
  df = "bmi_poc",
  vars = exp.vars, 
  conns = conns)

descriptives_out_full <- dh.getStats(
  df = "bmi_poc",
  vars = out.vars, 
  conns = conns)

descriptives_cov_full <- dh.getStats(
  df = "bmi_poc",
  vars = cov.vars, 
  conns = conns)

descriptives_oth_full <- dh.getStats(
  df = "bmi_poc",
  vars = other.vars, 
  conns = conns)

## ---- Bind together ----------------------------------------------------------
descriptives_full <- list(
  descriptives_exp_full, descriptives_out_full, descriptives_cov_full, 
  descriptives_oth_full) %>%
  pmap(bind_rows)

save.image()

################################################################################
# 3. Model definitions  
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
    covariates = c("age_days.730", "sex", "edu_m"),
    cohorts = env_coh), 
  bmi_48 = list(
    outcome = "bmi.1461",
    exposure = "area_dep",
    covariates = c("age_days.1461", "sex", "edu_m"),
    cohorts = env_coh),
  bmi_96 = list(
    outcome = "bmi.2922",
    exposure = "area_dep", 
    covariates = c("age_days.2922", "sex", "edu_m"),
    cohorts = env_coh),
  bmi_168 = list(
    outcome = "bmi.5113",
    exposure = "area_dep", 
    covariates = c("age_days.5113", "sex", "edu_m"),
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
    cohorts = cohorts[!cohorts %in% c("chop", "hgs", "nfbc86")])
  )
  


################################################################################
# 4. Functions
################################################################################

## ---- Make formulae ----------------------------------------------------------
dh.makeGlmForm <- function(x, type, dummy_suff = "_dummy", data = "analysis_df"){
  
  if(type == "ipd"){
    
    if(length(x$covariates) == 0){
      
      mod <- list(
        model = paste0(
          x$outcome, "~", x$exposure, "+", paste0(x$cohorts[-1], "_dummy", collapse = "+")),
        cohorts = x$"cohorts"
      )
    } else if(length(x$covariates) >0){
    
    mod <- list(
      model = paste0(x$outcome, "~", x$exposure, "+", paste0(x$covariates, collapse = "+"), 
                     "+", paste0(x$cohorts[-1], "_dummy", collapse = "+")),
      cohorts = x$"cohorts"
    )
    
    }
  }
  
  else if(type == "slma"){
    
    if(length(x$covariates) == 0){
      mod <- list(
        model = paste0(x$outcome, "~", x$exposure), 
        cohorts = x$cohorts
      )
      
    } else if(length(x$covariates) > 0){
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
      data = data, 
      family = "gaussian", 
      datasources = conns[x$cohorts])
    
  }
  
  
  else if(type == "slma"){
    
    out <- ds.glmSLMA(
      formula = x$model,
      dataName = data, 
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
# 5. Run core models  
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

ndvi.fit$ipd

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
# 7. Results stratified by sex  
################################################################################

## ---- Create subsets ---------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$sex",
  V2.name = "1", 
  Boolean.operator = "==",
  newobj = "analysis_df_m"
)

ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$sex",
  V2.name = "2", 
  Boolean.operator = "==",
  newobj = "analysis_df_f"
)


## ---- Modify formulae --------------------------------------------------------
mat_ed_sex.mod <- dh.changeForm(
  model = mat_ed.mod, 
  var = "sex",
  category = "covariates", 
  type = "remove")

area_dep_sex.mod <- dh.changeForm(
  model = area_dep.mod, 
  var = "sex",
  category = "covariates", 
  type = "remove")

ndvi_sex.mod <- dh.changeForm(
  model = ndvi.mod, 
  var = "sex",
  category = "covariates", 
  type = "remove")

preg_dia_sex.mod <- dh.changeForm(
  model = preg_dia.mod, 
  var = "sex",
  category = "covariates", 
  type = "remove")


## ---- Analysis for males -----------------------------------------------------
mat_ed_m.fit <- list(
  ipd = mat_ed_sex.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd", data = "analysis_df_m"), 
  slma = mat_ed_sex.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma", data = "analysis_df_m")
)

area_dep_sex_m.mod <- c(
  area_dep_sex.mod[1],
  area_dep_sex.mod[2] %>%
    dh.changeForm(
      var = "ninfea",
      category = "cohorts", 
      type = "remove"),
  area_dep_sex.mod[3:4])

area_dep_m.fit <- list(
  ipd = area_dep_sex_m.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd", data = "analysis_df_m"), 
  slma = area_dep_sex_m.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma", data = "analysis_df_m")
)

ndvi_sex_m.mod <- c(
  ndvi_sex.mod[1],
  ndvi_sex.mod[2] %>%
    dh.changeForm(
      var = "ninfea",
      category = "cohorts", 
      type = "remove"),
  ndvi_sex.mod[3:4])

ndvi_m.fit <- list(
  ipd = ndvi_sex_m.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd", data = "analysis_df_m"), 
  slma = ndvi_sex.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma", data = "analysis_df_m")
)

# Disclosure risk for ALPSAC and RAINE on first two models
preg_dia_sex_m.mod <- c(
  preg_dia_sex.mod[1:2] %>%
    dh.changeForm(
      var = c("alspac", "ninfea", "raine"),
      category = "cohorts", 
      type = "remove"),
  preg_dia_sex.mod[3:4])

preg_dia_m.fit <- list(
  ipd = preg_dia_sex_m.mod %>%
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd", data = "analysis_df_m"))
        

## ---- Analysis for females ---------------------------------------------------
mat_ed_f.fit <- list(
  ipd = mat_ed_sex.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd", data = "analysis_df_f"), 
  slma = mat_ed_sex.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma", data = "analysis_df_f")
)

area_dep_f.fit <- list(
  ipd = area_dep_sex_m.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd", data = "analysis_df_f"), 
  slma = area_dep_sex_m.mod %>% 
    map(dh.makeGlmForm, type = "slma") %>% 
    map(dh.glmWrap, type = "slma", data = "analysis_df_f")
)

ndvi_sex_f.mod <- c(
  ndvi_sex.mod[1],
  ndvi_sex.mod[2] %>%
    dh.changeForm(
      var = "ninfea",
      category = "cohorts", 
      type = "remove"),
  ndvi_sex.mod[3:4])

ndvi_f.fit <- list(
  ipd = ndvi_sex_f.mod %>% 
    map(dh.makeGlmForm, type = "ipd") %>% 
    map(dh.glmWrap, type = "ipd", data = "analysis_df_f"))

preg_dia_sex_f.mod <- c(
  preg_dia_sex.mod[1] %>%
    dh.changeForm(
      var = "alspac",
      category = "cohorts", 
      type = "remove"),
  preg_dia_sex.mod[2] %>%
    dh.changeForm(
      var = c("alspac", "ninfea"),
      category = "cohorts", 
      type = "remove"),
  preg_dia_sex.mod[3:4])

preg_dia_f.fit <- list(
  ipd = preg_dia_sex_f.mod %>% 
    map(dh.makeGlmForm, type = "ipd", data = "analysis_df_f") %>% 
    map(dh.glmWrap, type = "ipd", data = "analysis_df_f"))


################################################################################
# 6. Test for interactions by sex  
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

ndvi_int.mod <- c(
  ndvi_int.mod[1],
  ndvi_int.mod[2] %>%
    dh.changeForm(
      var = "ninfea",
      category = "cohorts", 
      type = "remove"),
  ndvi_int.mod[3:4])

preg_dia_int.mod <- dh.changeForm(
  model = preg_dia.mod, 
  var = "preg_dia*sex", 
  category = "covariates", 
  type = "add")

preg_dia_int.mod <- c(
  preg_dia_int.mod[1:2] %>%
    dh.changeForm(
      var = c("alspac", "ninfea", "raine"),
      category = "cohorts", 
      type = "remove"),
  preg_dia_int.mod[3:4])

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
# 8. Repeat analyses removing DNBC and MoBa  
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

################################################################################
# 7. Exploring missingness  
################################################################################
################################################################################
# Define models  
################################################################################
conns <- datashield.login(logindata, restore = "bmi_poc_sec_12")

## ---- Function to modify models ----------------------------------------------
dh.modToMiss <- function(x){
  
  out <- list(
    vars = list(c(x$exposure, x$outcome, x$covariates)), 
    cohorts = x$cohorts, 
    name = paste0(x$exposure, "_", x$outcome, "_m"))
  
}

## ---- New models for complete cases ------------------------------------------
mat_ed_miss.mod <- mat_ed.mod %>% map(dh.modToMiss)

ndvi_miss.mod <- ndvi.mod %>% map(dh.modToMiss)

area_dep_miss.mod <- area_dep.mod %>%
  map(function(x){list_modify(x, exposure = "a_d")}) %>%
  map(dh.modToMiss)

preg_dia_miss.mod <- preg_dia.mod %>% 
  map(function(x){list_modify(x, exposure = "p_d")}) %>%
  map(dh.modToMiss)
  

################################################################################
# Define complete cases  
################################################################################

## ---- Maternal education -----------------------------------------------------
mat_ed_miss.mod %>%
  map(., function(x){
  
  pmap(x, function(vars, name, cohorts){
    
    dh.defineCompleteCase(
      df = "bmi_poc", 
      vars = vars, 
      newobj = name,
      conns = conns[cohorts]
    )
  })
  })

datashield.workspace_save(conns, "missing_1a")
conns <- datashield.login(logindata, restore = "missing_1a")

## ---- NDVI -------------------------------------------------------------------
ndvi_miss.mod %>%
  map(., function(x){
    
    pmap(x, function(vars, name, cohorts){
      
      dh.defineCompleteCase(
        df = "bmi_poc", 
        vars = vars, 
        newobj = name,
        conns = conns[cohorts]
      )
    })
  })

datashield.workspace_save(conns, "missing_1b")
conns <- datashield.login(logindata, restore = "missing_1b")

## ---- Area deprivation -------------------------------------------------------
ds.make(
  toAssign = "bmi_poc$area_dep", 
  newobj = "a_d")

ds.dataFrame(
  x = c("bmi_poc", "a_d"), 
  newobj = "bmi_poc")

area_dep_miss.mod %>%
  map(., function(x){
    
    pmap(x, function(vars, name, cohorts){
      
      dh.defineCompleteCase(
        df = "bmi_poc", 
        vars = vars, 
        newobj = name,
        conns = conns[cohorts]
      )
    })
  })

datashield.workspace_save(conns, "missing_1c")
conns <- datashield.login(logindata, restore = "missing_1c")

## ---- Pregnancy diabetes -----------------------------------------------------
ds.make(
  toAssign = "bmi_poc$preg_dia", 
  newobj = "p_d")

ds.dataFrame(
  x = c("bmi_poc", "p_d"), 
  newobj = "bmi_poc")

preg_dia_miss.mod %>%
  map(., function(x){
    
    pmap(x, function(vars, name, cohorts){
      
      dh.defineCompleteCase(
        df = "bmi_poc", 
        vars = vars, 
        newobj = name,
        conns = conns[cohorts]
      )
    })
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "missing_1")
conns <- datashield.login(logindata, restore = "missing_1")


################################################################################
# Get stats of these created variables  
################################################################################
################################################################################
# Prepare data  
################################################################################

## ---- Tibble of variables to join --------------------------------------------
tojoin <- c(preg_dia_miss.mod, mat_ed_miss.mod, ndvi_miss.mod, area_dep_miss.mod) %>%
  map(function(x){
    out <- tibble(
      var = x$name, 
      cohort = x$cohort)}) %>%
  setNames(., paste0("tmp_", seq(1, length(.), 1))) %>%
  bind_rows %>%
  group_by(cohort) %>%
  group_split


## ---- Join into one dataframe ------------------------------------------------
tojoin %>%
  map(function(x){
    
    ds.dataFrame(
      x = x$var,
      newobj = "missing",
      datasources = conns[x$cohort[[1]]]
    )
  })


datashield.workspace_save(conns, "missing_2a")
conns <- datashield.login(logindata, restore = "missing_2a")


## ---- Fill missing columns ---------------------------------------------------
ds.dataFrameFill("missing", "missing")

datashield.workspace_save(conns, "missing_2b")
conns <- datashield.login(logindata, restore = "missing_2b")


## ---- Fix levels -------------------------------------------------------------
ds.colnames("missing")[[1]] %>%
  map(
    ~ds.asFactor(
      input.var.name = paste0("missing$", .), 
      newobj.name = paste0(., "_fact"))
    )

datashield.workspace_save(conns, "missing_2c")
conns <- datashield.login(logindata, restore = "missing_2c")


## ---- Join back into one dataframe -------------------------------------------
ds.dataFrame(
  x = paste0(ds.colnames("missing")[[1]], "_fact"),
  newobj = "missing")
  
ds.colnames("missing")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "missing_2")
conns <- datashield.login(logindata, restore = "missing_2")


################################################################################
# Now extract descriptives  
################################################################################
miss_vars <- ds.colnames("missing")[[1]]

miss.descriptives_1 <- dh.getStats(
  df = "missing", 
  vars = miss_vars[1:5]
)

miss.descriptives_2 <- dh.getStats(
  df = "missing", 
  vars = miss_vars[6:10]
)

miss.descriptives_3 <- dh.getStats(
  df = "missing", 
  vars = miss_vars[11:15]
)

miss.descriptives_4 <- dh.getStats(
  df = "missing", 
  vars = miss_vars[16:17]
)

miss_descriptives <- list(
  miss.descriptives_1, 
  miss.descriptives_2, 
  miss.descriptives_3, 
  miss.descriptives_4) %>%
  pmap(bind_rows)
  
save.image()
  
################################################################################
# Regression models to see whether outcome is associated with missingness  
################################################################################

## ---- Function  --------------------------------------------------------------
dh.makeMissForm <- function(x){
  
  mod <- list(
    model = paste0(
      paste0(
        x$name,
        "~",  
        "bmi_poc$", str_subset(x$vars[[1]], "bmi")),
        paste0("+bmi_poc$", str_subset(x$vars[[1]], "age_days")),
        "+bmi_poc$sex",
        "+", paste0(paste0("bmi_poc$", x$cohorts[-1]), "_dummy", collapse = "+")), 
    cohorts = x$cohorts
  )
  }
   
green_covs <- c("sex", "edu_m", "parity_bin", "area_dep")
preg_dia_cov <- c("sex", "edu_m", "parity_bin", "agebirth_m_y", "prepreg_bmi")


## ---- Maternal education ----------------------------------------------------- 
mat_ed_miss.fit <- mat_ed_miss.mod %>%
  dh.changeForm(var = c("chop", "ninfea"), type = "remove", category = "cohorts") %>%
  map(dh.makeMissForm) %>%
  map(dh.glmWrap, type = "ipd", data = NULL)

## ---- Area deprivation -------------------------------------------------------
area_dep_miss.fit <- area_dep_miss.mod %>%
  map(dh.makeMissForm) %>%
  map(dh.glmWrap, type = "ipd", data = NULL)

## ---- NDVI -------------------------------------------------------------------
ndvi_miss.fit <- ndvi_miss.mod %>%
  map(dh.makeMissForm) %>%
  map(dh.glmWrap, type = "ipd", data = NULL)

## ---- Pregnancy diabetes -----------------------------------------------------
preg_dia_miss.fit <- preg_dia_miss.mod %>%
  map(dh.makeMissForm) %>%
  map(dh.glmWrap, type = "ipd", data = NULL)

save.image()

