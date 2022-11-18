################################################################################
## Project: bmi-poc
## Script purpose: Condcut analysis
## Date: 29th April 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################
withr::with_libpaths(
  new = "~/R/userlib",
  devtools::install_github("lifecycle-project/ds-helper"))

library(DSI)
library(DSOpal)
library(dsBaseClient)
require(stringr)
require(dplyr)
require(magrittr)
library(purrr)
library(dsHelper)

conns <- datashield.login(logindata, restore = "bmi_poc_sec_27")

ds.colnames("analysis_df")
ds.ls()

library(remotes)
install_github("lifecycle-project/ds-helper")
library(dsHelper)

################################################################################
# 1. Get descriptives for analysis sample
################################################################################

## ---- Do separately in case it breaks ----------------------------------------
descriptives_exp <- dh.getStats(
   df = "analysis_df",
   vars = exp.vars, 
   digits = 1,
   conns = conns, 
   checks = F)

descriptives_out <- dh.getStats(
  df = "analysis_df",
  vars = out.vars, 
  digits = 1,
  conns = conns, 
  checks = F)

descriptives_cov <- dh.getStats(
  df = "analysis_df",
  vars = c(cov.vars, "prepreg_bmi_u_f", "prepreg_bmi_o_f"),
  digits = 1,
  conns = conns, 
  checks = F)

descriptives_eth <- dh.getStats(
  df = "analysis_df",
  vars = "ethn3_m_f",
  digits = 1,
  checks = F)

descriptives_oth <- dh.getStats(
  df = "analysis_df",
  vars = other.vars[!other.vars %in% c("child_id", coh_dummy$cohort)], 
  digits = 1,
  checks = F)

## ---- Bind together ----------------------------------------------------------
descriptives <- list(
  descriptives_exp, descriptives_out, descriptives_cov, descriptives_oth, 
  descriptives_eth) %>%
  pmap(bind_rows)
  
save.image()

################################################################################
# 2. Get descriptives for excluded sample 
################################################################################
descriptives_exp_exc <- dh.getStats(
  df = "excluded_df",
  vars = exp.vars, 
  digits = 1,
  conns = conns)

descriptives_out_exc <- dh.getStats(
  df = "excluded_df",
  vars = out.vars, 
  digits = 1,
  conns = conns)

descriptives_cov_exc <- dh.getStats(
  df = "excluded_df",
  vars = c(cov.vars, "prepreg_bmi_u_f", "prepreg_bmi_o_f"),
  digits = 1,
  conns = conns)

descriptives_eth_exc <- dh.getStats(
  df = "excluded_df",
  vars = "ethn3_m_f", 
  digits = 1,
  conns = conns[c(eth_coh, "abcd")])

descriptives_oth_exc <- dh.getStats(
  df = "excluded_df",
  vars = other.vars[!other.vars %in% c("child_id", coh_dummy$cohort)], 
  digits = 1,
  conns = conns)

## ---- Bind together ----------------------------------------------------------
descriptives_exc <- list(
  descriptives_exp_exc, descriptives_out_exc, descriptives_cov_exc, 
  descriptives_eth_exc, descriptives_oth_exc) %>%
  pmap(bind_rows)

save.image()
## ---- Round ------------------------------------------------------------------
descriptives_exc$continuous <- descriptives_exc$continuous %>%
  mutate(across(mean:perc_95, ~round(., 1)))

descriptives$continuous <- descriptives$continuous %>%
  mutate(across(mean:perc_95, ~round(., 1)))

save.image()

################################################################################
# 3. Model definitions  
################################################################################

## Been round in circles about this, but decided the clearest way is to make a
## series of lists holding key model information, and then write a simple 
## function to convert this in to model definitions. Has the advantage that you
## can enter information once and use the function to make the model components
## as required by the IPD vs SLMA functions.

## ---- Maternal education -----------------------------------------------------
cohorts <- names(conns)

mat_ed.mod <- list(
  bmi_24 = list(
    outcome = "zscores.0_730",
    exposure = "edu_m",
    covariates = c("age_days.730", "sex"),
    cohorts = cohorts[!cohorts == "hgs"]), 
  bmi_48 = list(
    outcome = "zscores.730_1461",
    exposure = "edu_m",
    covariates = c("age_days.1461", "sex"),
    cohorts = cohorts[!cohorts %in% c("dnbc", "hgs")]),
  bmi_96 = list(
    outcome = "zscores.1461_2922",
    exposure = "edu_m", 
    covariates = c("age_days.2922", "sex"),
    cohorts = cohorts[!cohorts == "hgs"]),
  bmi_168 = list(
    outcome = "zscores.2922_5113",
    exposure = "edu_m", 
    covariates = c("age_days.5113", "sex"),
    cohorts = cohorts), 
  bmi_215 = list(
    outcome = "zscores.5113_6544",
    exposure = "edu_m", 
    covariates = c("age_days.6544", "sex"),
    cohorts = cohorts[cohorts %in% c(
      "alspac", "dnbc", "nfbc66", "nfbc86", "raine")]))


## ---- Area deprivation -------------------------------------------------------
env_coh <- c("abcd", "alspac", "bib", "dnbc", "eden", "genr", "inma", "ninfea", "moba", 
             "rhea")

area_dep.mod <- list(
  bmi_24 = list(
    outcome = "zscores.0_730",
    exposure = "area_dep",
    covariates = c("age_days.730", "sex", "edu_m"),
    cohorts = env_coh), 
  bmi_48 = list(
    outcome = "zscores.730_1461",
    exposure = "area_dep",
    covariates = c("age_days.1461", "sex", "edu_m"),
    cohorts = env_coh[!env_coh %in% c("dnbc")]),
  bmi_96 = list(
    outcome = "zscores.1461_2922",
    exposure = "area_dep", 
    covariates = c("age_days.2922", "sex", "edu_m"),
    cohorts = env_coh),
  bmi_168 = list(
    outcome = "zscores.2922_5113",
    exposure = "area_dep", 
    covariates = c("age_days.5113", "sex", "edu_m"),
    cohorts = env_coh), 
  bmi_215 = list(
    outcome = "zscores.5113_6544",
    exposure = "area_dep", 
    covariates = c("age_days.6544", "sex", "edu_m"),
    cohorts = c("alspac", "dnbc")))

## ---- NDVI -------------------------------------------------------------------
green_covs <- c("sex", "edu_m", "parity_bin", "area_dep")

ndvi.mod <- list(
  bmi_24 = list(
    outcome = "zscores.0_730",
    exposure = "ndvi300_preg_iqr_c",
    covariates = c("age_days.730", green_covs),
    cohorts = env_coh), 
  bmi_48 = list(
    outcome = "zscores.730_1461",
    exposure = "ndvi300_preg_iqr_c",
    covariates = c("age_days.1461", green_covs),
    cohorts = env_coh[!env_coh %in% c("dnbc")]),
  bmi_96 = list(
    outcome = "zscores.1461_2922",
    exposure = "ndvi300_preg_iqr_c", 
    covariates = c("age_days.2922", green_covs),
    cohorts = env_coh), 
  bmi_168 = list(
    outcome = "zscores.2922_5113",
    exposure = "ndvi300_preg_iqr_c", 
    covariates = c("age_days.5113", green_covs),
    cohorts = env_coh), 
  bmi_215 = list(
    outcome = "zscores.5113_6544",
    exposure = "ndvi300_preg_iqr_c", 
    covariates = c("age_days.6544", "sex", "edu_m"),
    cohorts = c("alspac", "dnbc")))


## ---- Gestational diabetes ---------------------------------------------------
preg_dia_cov <- c(
  "sex", "edu_m", "agebirth_m_y", "prepreg_bmi_u", "prepreg_bmi_o", 
  "parity_bin", "preg_smk")

preg_dia.mod <- list(
  bmi_24 = list(
    outcome = "zscores.0_730",
    exposure = "preg_dia",
    covariates = c("age_days.730", preg_dia_cov),
    cohorts = cohorts[!cohorts %in% c("chop", "hgs", "nfbc66", "nfbc86")]), 
  bmi_48 = list(
    outcome = "zscores.730_1461",
    exposure = "preg_dia",
    covariates = c("age_days.1461", preg_dia_cov),
    cohorts = cohorts[!cohorts %in% c("chop", "dnbc", "hgs", "nfbc66", "nfbc86")]),
  bmi_96 = list(
    outcome = "zscores.1461_2922",
    exposure = "preg_dia", 
    covariates = c("age_days.2922", preg_dia_cov),
    cohorts = cohorts[!cohorts %in% c("chop", "hgs", "nfbc66", "nfbc86")]), 
  bmi_168 = list(
    outcome = "zscores.2922_5113",
    exposure = "preg_dia", 
    covariates = c("age_days.5113", preg_dia_cov),
    cohorts = cohorts[!cohorts %in% c(
      "chop", "hgs", "nfbc66", "nfbc86")]), 
  bmi_215 = list(
    outcome = "zscores.5113_6544",
    exposure = "preg_dia", 
    covariates = c("age_days.6544", preg_dia_cov),
    cohorts = cohorts[cohorts %in% c(
      "alspac", "dnbc", "raine")]))
  
################################################################################
# 4. Exploring missingness  
################################################################################
################################################################################
# Define models  
################################################################################
conns <- datashield.login(logindata, restore = "bmi_poc_sec_27")

## ---- Function to modify models ----------------------------------------------
dh.modToMiss <- function(x, suffix = "_m"){
  
  out <- list(
    vars = list(c(x$exposure, x$outcome, x$covariates)), 
    cohorts = x$cohorts, 
    name = paste0(x$exposure, "_", x$outcome, suffix))
  
}

## ---- New models for complete cases ------------------------------------------
mat_ed_miss.mod <- mat_ed.mod %>% map(dh.modToMiss)

ndvi_miss.mod <- ndvi.mod %>% 
  map(function(x){list_modify(x, exposure = "n_d")}) %>%
  map(dh.modToMiss)

area_dep_miss.mod <- area_dep.mod %>%
  map(function(x){list_modify(x, exposure = "a_d")}) %>%
  map(dh.modToMiss)

preg_dia_miss.mod <- preg_dia.mod %>% 
  map(function(x){list_modify(x, exposure = "p_d")}) %>%
  map(dh.modToMiss)

save.image("15_nov_22.RData")

################################################################################
# Define complete cases  
################################################################################

## ---- Maternal education -----------------------------------------------------
mat_ed_miss.mod %>%
  map(., function(x){
    
    pmap(x, function(vars, name, cohorts){
      
      dh.defineCases(
        df = "bmi_poc", 
        vars = vars, 
        new_obj = name, 
        type = "all", 
        conns = conns[cohorts], 
        checks = FALSE)
    })
  })

datashield.workspace_save(conns, "missing_1a")
conns <- datashield.login(logindata, restore = "missing_1a")

## ---- NDVI -------------------------------------------------------------------
ds.make(
  toAssign = "bmi_poc$ndvi300_preg_iqr_c", 
  newobj = "n_d", 
  datasources = conns[env_coh])

ds.dataFrame(
  x = c("bmi_poc", "n_d"), 
  newobj = "bmi_poc", 
  datasources = conns[env_coh])

ndvi_miss.mod %>%
  map(., function(x){
    
    pmap(x, function(vars, name, cohorts){
      
      dh.defineCases(
        df = "bmi_poc", 
        vars = vars, 
        new_obj = name, 
        type = "all", 
        conns = conns[cohorts], 
        checks = FALSE)
      
    })
  })

datashield.workspace_save(conns, "missing_1b")
conns <- datashield.login(logindata, restore = "missing_1b")

## ---- Area deprivation -------------------------------------------------------
ds.make(
  toAssign = "bmi_poc$area_dep", 
  newobj = "a_d", 
  datasources = conns[env_coh])

ds.dataFrame(
  x = c("bmi_poc", "a_d"), 
  newobj = "bmi_poc", 
  datasources = conns[env_coh])

area_dep_miss.mod %>%
  map(., function(x){
    
    pmap(x, function(vars, name, cohorts){
      
      dh.defineCases(
        df = "bmi_poc", 
        vars = vars, 
        new_obj = name, 
        type = "all", 
        conns = conns[cohorts], 
        checks = FALSE)
      
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
      
      dh.defineCases(
        df = "bmi_poc", 
        vars = vars, 
        new_obj = name, 
        type = "all", 
        conns = conns[cohorts], 
        checks = FALSE)
      
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

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "missing_2")
conns <- datashield.login(logindata, restore = "missing_2")

save.image("15_nov_22.RData")
################################################################################
# Now extract descriptives  
################################################################################
miss_vars <- ds.colnames("missing")[[1]]

miss.descriptives_1 <- dh.getStats(
  df = "missing", 
  vars = miss_vars[1:5],
  checks = F)

miss.descriptives_2 <- dh.getStats(
  df = "missing", 
  vars = miss_vars[6:10], 
  checks = F)

miss.descriptives_3 <- dh.getStats(
  df = "missing", 
  vars = miss_vars[11:15],
  checks = F)

miss.descriptives_4 <- dh.getStats(
  df = "missing", 
  vars = miss_vars[16:20],
  checks = F)

miss_descriptives <- list(
  miss.descriptives_1, 
  miss.descriptives_2, 
  miss.descriptives_3, 
  miss.descriptives_4) %>%
  pmap(bind_rows)

save.image("15_nov_22.RData")
################################################################################
# Regression models to see whether outcome is associated with missingness  
################################################################################
conns <- datashield.login(logindata, restore = "missing_2")

## ---- Function  --------------------------------------------------------------
dh.makeMissForm <- function(x){
  
  mod <- list(
    model = paste0(
      paste0(
        x$name,
        "~",  
        "bmi_poc$", str_subset(x$vars[[1]], "zscores")),
      paste0("+bmi_poc$", str_subset(x$vars[[1]], "age_days")),
      "+bmi_poc$sex",
      "+", paste0(paste0("bmi_poc$", x$cohorts[-1]), "_dummy", collapse = "+")), 
    cohorts = x$cohorts
  )
}

## ---- Maternal education ----------------------------------------------------- 
mat_ed_miss.fit <- mat_ed_miss.mod %>%
  dt.changeForm(
    vars = c("chop", "ninfea"), 
    type = "remove", 
    category = "cohorts",
    elements = "bmi_48") %>%
  dt.changeForm(
    vars = "rhea", 
    type = "remove", 
    category = "cohorts",
    elements = "bmi_168") %>%
  dt.changeForm(
    vars = c("hgs", "ninfea"), 
    type = "remove", 
    category = "cohorts",
    elements = "bmi_215") %>%
  map(dh.makeMissForm) %>%
  map(dt.glmWrap, type = "ipd")

## ---- Area deprivation -------------------------------------------------------
area_dep_miss.fit <- area_dep_miss.mod %>%
  map(dh.makeMissForm) %>%
  map(dt.glmWrap, type = "ipd")

## ---- NDVI -------------------------------------------------------------------
ndvi_miss.fit <- ndvi_miss.mod %>%
  map(dh.makeMissForm) %>%
  map(dt.glmWrap, type = "ipd")

## ---- Pregnancy diabetes -----------------------------------------------------
preg_dia_miss.fit <- preg_dia_miss.mod %>%
  dt.changeForm(
    vars = c("hgs", "inma", "rhea"), 
    type = "remove", 
    category = "cohorts",
    elements = "bmi_168") %>%
    map(dh.makeMissForm) %>%
    map(dt.glmWrap, type = "ipd")

save.image("15_nov_22.RData")

################################################################################
# 5. Run core models  
################################################################################
conns <- datashield.login(logindata, restore = "bmi_poc_sec_27")

## ---- Maternal education -----------------------------------------------------
mat_ed.fit <- list(
  ipd = mat_ed.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"), 
  slma = mat_ed.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

## ---- Area deprivation -------------------------------------------------------
area_dep.fit <- list(
  ipd = area_dep.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"),
  slma = area_dep.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

## ---- NDVI -------------------------------------------------------------------
ndvi.fit <- list(
  ipd = ndvi.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"),
  slma = ndvi.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

## ---- Gestational diabetes ---------------------------------------------------
preg_dia.fit <- list(
  ipd = preg_dia.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"),
  slma = preg_dia.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

save.image("15_nov_22.RData")


################################################################################
# 6. Now we need ns for all analyses
################################################################################
conns <- datashield.login(logindata, restore = "missing_2")

################################################################################
# Categorical variables: combined
################################################################################

## ---- Initial cleaning -------------------------------------------------------
n_ref_cat_clean <- miss_descriptives$categorical %>%
  dplyr::filter(str_detect(variable, "edu_m|a_d|p_d")) %>%
  mutate(exposure = case_when(
    str_detect(variable, "edu_m") ~ "edu_m", 
    str_detect(variable, "a_d") ~ "area_dep", 
    str_detect(variable, "p_d") ~ "preg_dia")) 

## ---- Reference table --------------------------------------------------------
n_ref_cat_comb <- n_ref_cat_clean %>%
  dplyr::filter(category == 1 & value > 0 & cohort != "combined") %>%
  group_by(variable) %>%
  dplyr::select(variable, cohort, exposure) %>% 
  group_split %>%
  map(function(x){
    
    x %>%
      mutate(cohort_str = paste0(cohort, collapse = ",")) %>%
      slice(1) %>%
      dplyr::select(-cohort)
    
  }) %>%
  bind_rows

## ---- IPD --------------------------------------------------------------------
ord_cat_ipd <- n_ref_cat_comb %>%
  pmap(function(variable, cohort_str, exposure){
    
    ds.table(
      rvar = paste0("bmi_poc$", exposure), 
      cvar = variable,
      useNA = "always",
      datasources = conns[str_split(cohort_str, ",")[[1]]])
    
  }) 

save.image("15_nov_22.RData")

################################################################################
# Categorical variables: cohort specific
################################################################################

## ---- Reference table --------------------------------------------------------
n_ref_cat_coh <- n_ref_cat_clean %>%
  dplyr::filter(category == 1 & value > 0 & cohort != "combined") %>%
  dplyr::select(variable, cohort, exposure) 

## ---- Get variables ----------------------------------------------------------
ord_cat_slma <- n_ref_cat_coh %>%
  pmap(function(variable, cohort, exposure){
    
    ds.table(
      rvar = paste0("bmi_poc$", exposure), 
      cvar = variable,
      useNA = "always",
      datasources = conns[cohort])
    
  }) 

save.image("15_nov_22.RData")

################################################################################
# 7. Results stratified by sex  
################################################################################
################################################################################
# Prepare data  
################################################################################
conns <- datashield.login(logindata, restore = "bmi_poc_sec_27")

## ---- Create subsets ---------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$sex",
  V2.name = "1", 
  Boolean.operator = "==",
  newobj = "analysis_df_m")

ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$sex",
  V2.name = "2", 
  Boolean.operator = "==",
  newobj = "analysis_df_f")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_28")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_28")

## ---- Modify formulae --------------------------------------------------------
mat_ed_sex.mod <- dt.changeForm(
  model = mat_ed.mod, 
  var = "sex",
  category = "covariates", 
  type = "remove", 
  elements = names(mat_ed.mod))

area_dep_sex.mod <- dt.changeForm(
  model = area_dep.mod, 
  var = "sex",
  category = "covariates", 
  type = "remove", 
  elements = names(area_dep.mod))

ndvi_sex.mod <- dt.changeForm(
  model = ndvi.mod, 
  var = "sex",
  category = "covariates", 
  type = "remove", 
  elements = names(ndvi.mod))

preg_dia_sex.mod <- dt.changeForm(
  model = preg_dia.mod, 
  var = "sex",
  category = "covariates", 
  type = "remove", 
  elements = names(preg_dia.mod))

save.image("15_nov_22.RData")

################################################################################
# Males  
################################################################################
mat_ed_m.fit <- list(
  ipd = mat_ed_sex.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd", df = "analysis_df_m"), 
  slma = mat_ed_sex.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma", df = "analysis_df_m"))

area_dep_sex_m.mod <- area_dep_sex.mod %>%
    dt.changeForm(
      var = "ninfea",
      category = "cohorts", 
      type = "remove", 
      elements = "bmi_48")

area_dep_m.fit <- list(
  ipd = area_dep_sex_m.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd", df = "analysis_df_m"), 
  slma = area_dep_sex_m.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma", df = "analysis_df_m"))

ndvi_sex_m.mod <- ndvi_sex.mod %>%
    dt.changeForm(
      var = "ninfea",
      category = "cohorts", 
      type = "remove", 
      elements = "bmi_48")

ndvi_m.fit <- list(
  ipd = ndvi_sex_m.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd", df = "analysis_df_m"), 
  slma = ndvi_sex_m.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma", df = "analysis_df_m"))

# Disclosure risk for ALPSAC and RAINE on first two models
preg_dia_sex_m.mod <- preg_dia_sex.mod %>%
  dt.changeForm(
    var = c("alspac", "ninfea", "raine"),
    category = "cohorts", 
    type = "remove", 
    elements = c("bmi_24", "bmi_48")) %>%
  dt.changeForm(
    var = "rhea",
    category = "cohorts", 
    type = "remove", 
    elements = "bmi_168")
  
preg_dia_m.fit <- list(
  ipd = preg_dia_sex_m.mod %>%
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd", df = "analysis_df_m"))
  
save.image("15_nov_22.RData")  

################################################################################
# Females  
################################################################################
mat_ed_f.fit <- list(
  ipd = mat_ed_sex.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd", df = "analysis_df_f"), 
  slma = mat_ed_sex.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma", df = "analysis_df_f"))

save.image("15_nov_22.RData")

area_dep_f.fit <- list(
  ipd = area_dep_sex_m.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd", df = "analysis_df_f"), 
  slma = area_dep_sex_m.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma", df = "analysis_df_f"))

save.image("15_nov_22.RData")

ndvi_sex_f.mod <- ndvi_sex.mod %>%
  dt.changeForm(
    var = "ninfea",
    category = "cohorts", 
    type = "remove", 
    elements = "bmi_48")

ndvi_f.fit <- list(
  ipd = ndvi_sex_f.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd", df = "analysis_df_f"))

save.image("15_nov_22.RData")

preg_dia_sex_f.mod <- preg_dia_sex.mod %>%
  dt.changeForm(
    var = "alspac",
    category = "cohorts", 
    type = "remove", 
    elements = c("bmi_24", "bmi_48")) %>%
  dt.changeForm(
    var = "ninfea",
    category = "cohorts", 
    type = "remove", 
    elements = "bmi_48")

preg_dia_f.fit <- list(
  ipd = preg_dia_sex_f.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd", df = "analysis_df_f"))

save.image("15_nov_22.RData")

################################################################################
# 6. Test for interactions by sex  
################################################################################

## ---- Modify model definitions -----------------------------------------------
mat_ed_int.mod <- dt.changeForm(
  model = mat_ed.mod, 
  var = "edu_m*sex", 
  category = "covariates", 
  type = "add", 
  elements = names(mat_ed.mod))

area_dep_int.mod <- dt.changeForm(
  model = area_dep.mod, 
  var = "area_dep*sex", 
  category = "covariates", 
  type = "add", 
  elements = names(area_dep.mod))

ndvi_int.mod <- dt.changeForm(
  model = ndvi.mod, 
  var = "ndvi300_preg_iqr_c*sex", 
  category = "covariates", 
  type = "add", 
  elements = names(ndvi.mod)) %>%
  dt.changeForm(
    var = "ninfea", 
    category = "cohorts", 
    type = "remove", 
    elements = "bmi_48")

preg_dia_int.mod <- dt.changeForm(
  model = preg_dia.mod, 
  var = "preg_dia*sex", 
  category = "covariates", 
  type = "add", 
  elements = names(preg_dia.mod)) %>%
  dt.changeForm(
    var = c("alspac", "ninfea", "raine"), 
    category = "cohorts", 
    type = "remove", 
    elements = c("bmi_24", "bmi_48"))


## ---- Run interaction models -------------------------------------------------
mat_ed_int.fit <- list(
  ipd = mat_ed_int.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"), 
  slma = mat_ed_int.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

save.image("15_nov_22.RData")

area_dep_int.fit <- list(
  ipd = area_dep_int.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"), 
  slma = area_dep_int.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

save.image("15_nov_22.RData")

ndvi_int.fit <- list(
  ipd = ndvi_int.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"), 
  slma = ndvi_int.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

save.image("15_nov_22.RData")

preg_dia_int.fit <- list(
  ipd = preg_dia_int.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"), 
  slma = preg_dia_int.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

save.image("15_nov_22.RData")

################################################################################
# ADDITIONALLY ADJUSTING FOR ETHNICITY
################################################################################
################################################################################
# Define models
################################################################################
eth_coh <- c("abcd", "alspac", "bib", "elfe", "gecko", "genr", "inma", "raine")

mat_ed_eth.mod <- list(
  bmi_24 = list(
    outcome = "zscores.0_730",
    exposure = "edu_m",
    covariates = c("age_days.730", "sex", "ethn3_m_f"),
    cohorts = eth_coh), 
  bmi_48 = list(
    outcome = "zscores.730_1461",
    exposure = "edu_m",
    covariates = c("age_days.1461", "sex", "ethn3_m_f"),
    cohorts = eth_coh),
  bmi_96 = list(
    outcome = "zscores.1461_2922",
    exposure = "edu_m", 
    covariates = c("age_days.2922", "sex", "ethn3_m_f"),
    cohorts = eth_coh),
  bmi_168 = list(
    outcome = "zscores.2922_5113",
    exposure = "edu_m", 
    covariates = c("age_days.5113", "sex", "ethn3_m_f"),
    cohorts = eth_coh), 
  bmi_215 = list(
    outcome = "zscores.5113_6544",
    exposure = "edu_m", 
    covariates = c("age_days.6544", "sex", "ethn3_m_f"),
    cohorts = c("alspac", "raine")))


## ---- Area deprivation -------------------------------------------------------
env_eth_coh <- c("abcd", "alspac", "bib", "genr", "inma")

area_dep_eth.mod <- list(
  bmi_24 = list(
    outcome = "zscores.0_730",
    exposure = "area_dep",
    covariates = c("age_days.730", "sex", "edu_m", "ethn3_m_f"),
    cohorts = env_eth_coh), 
  bmi_48 = list(
    outcome = "zscores.730_1461",
    exposure = "area_dep",
    covariates = c("age_days.1461", "sex", "edu_m", "ethn3_m_f"),
    cohorts = env_eth_coh),
  bmi_96 = list(
    outcome = "zscores.1461_2922",
    exposure = "area_dep", 
    covariates = c("age_days.2922", "sex", "edu_m", "ethn3_m_f"),
    cohorts = env_eth_coh),
  bmi_168 = list(
    outcome = "zscores.2922_5113",
    exposure = "area_dep", 
    covariates = c("age_days.5113", "sex", "edu_m", "ethn3_m_f"),
    cohorts = env_eth_coh), 
  bmi_215 = list(
    outcome = "zscores.5113_6544",
    exposure = "area_dep", 
    covariates = c("age_days.6544", "sex", "edu_m", "ethn3_m_f"),
    cohorts = "alspac"))

## ---- NDVI -------------------------------------------------------------------
ndvi_eth.mod <- list(
  bmi_24 = list(
    outcome = "zscores.0_730",
    exposure = "ndvi300_preg_iqr_c",
    covariates = c("age_days.730", green_covs, "ethn3_m_f"),
    cohorts = env_eth_coh), 
  bmi_48 = list(
    outcome = "zscores.730_1461",
    exposure = "ndvi300_preg_iqr_c",
    covariates = c("age_days.1461", green_covs, "ethn3_m_f"),
    cohorts = env_eth_coh),
  bmi_96 = list(
    outcome = "zscores.1461_2922",
    exposure = "ndvi300_preg_iqr_c", 
    covariates = c("age_days.2922", green_covs, "ethn3_m_f"),
    cohorts = env_eth_coh), 
  bmi_168 = list(
    outcome = "zscores.2922_5113",
    exposure = "ndvi300_preg_iqr_c", 
    covariates = c("age_days.5113", green_covs, "ethn3_m_f"),
    cohorts = env_eth_coh), 
  bmi_215 = list(
    outcome = "zscores.5113_6544",
    exposure = "ndvi300_preg_iqr_c", 
    covariates = c("age_days.6544", green_covs, "ethn3_m_f"),
    cohorts = "alspac"))


## ---- Gestational diabetes ---------------------------------------------------
preg_dia_eth.mod <- list(
  bmi_24 = list(
    outcome = "zscores.0_730",
    exposure = "preg_dia",
    covariates = c("age_days.730", preg_dia_cov, "ethn3_m_f"),
    cohorts = eth_coh), 
  bmi_48 = list(
    outcome = "zscores.730_1461",
    exposure = "preg_dia",
    covariates = c("age_days.1461", preg_dia_cov, "ethn3_m_f"),
    cohorts = eth_coh),
  bmi_96 = list(
    outcome = "zscores.1461_2922",
    exposure = "preg_dia", 
    covariates = c("age_days.2922", preg_dia_cov, "ethn3_m_f"),
    cohorts = eth_coh), 
  bmi_168 = list(
    outcome = "zscores.2922_5113",
    exposure = "preg_dia", 
    covariates = c("age_days.5113", preg_dia_cov, "ethn3_m_f"),
    cohorts = eth_coh), 
  bmi_215 = list(
    outcome = "zscores.5113_6544",
    exposure = "preg_dia", 
    covariates = c("age_days.6544", preg_dia_cov, "ethn3_m_f"),
    cohorts = c("alspac", "raine")))

################################################################################
# Define subsets
################################################################################
conns <- datashield.login(logindata, restore = "bmi_poc_sec_27")

## ---- Helper function --------------------------------------------------------
defineEthSub <- function(x){
  
  tibble(
    outcome = x$outcome,
    exposure = x$exposure,
    covariates = list(x$covariates),
    vars = list(c(outcome, exposure, unlist(covariates))), 
    coh = list(x$cohorts))}

## ---- Make reference table ---------------------------------------------------
eth_ref_mat_ed <- mat_ed_eth.mod %>% map(defineEthSub) %>%
  bind_rows(.id = "age") 

eth_ref_area_dep <- area_dep_eth.mod %>% map(defineEthSub) %>%
  bind_rows(.id = "age") 

eth_ref_ndvi <- ndvi_eth.mod %>% map(defineEthSub) %>%
  bind_rows(.id = "age") 

eth_ref_preg_dia <- preg_dia_eth.mod %>% map(defineEthSub) %>%
  bind_rows(.id = "age") 

eth_sub.mod <- bind_rows(eth_ref_mat_ed, eth_ref_area_dep, eth_ref_ndvi, 
                         eth_ref_preg_dia) %>%
  mutate(
    new_obj = paste0(exposure, ".", age, "_eth"), 
    new_sub = paste0(exposure, ".", age, "_e_sub")) 

eth_form <- eth_sub.mod %>%
  pmap(function(outcome, exposure, covariates, ...){
    
    paste0(
      paste0(outcome, "~", exposure, "+"),
      paste(unlist(covariates), collapse = "+"))
    
  }) %>% unlist

main_form <- eth_sub.mod %>%
  pmap(function(outcome, exposure, covariates, ...){
    
    covariates <-  str_subset(unlist(covariates), "ethn3_m_f", negate = T)
    
    paste0(
      paste0(outcome, "~", exposure, "+"),
      paste(covariates, collapse = "+"))
    
  }) %>% unlist

eth_sub.mod <- eth_sub.mod %>%
  mutate(
    eth_form = eth_form,
    main_form = main_form)

## ---- Define complete cases --------------------------------------------------
eth_sub.mod %>%
  pmap(function(vars, coh, new_obj, ...){
    
    dh.defineCases(
      df = "analysis_df", 
      vars = unlist(vars),
      type = "all", 
      new_obj = new_obj, 
      conns = conns[unlist(coh)]
    )
    
  })

datashield.workspace_save(conns, "bmi_poc_sec_29a")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_29")

## ---- Create subsets ---------------------------------------------------------
eth_sub.mod %>%
  pmap(function(vars, coh, new_obj, new_sub, ...){
   
   ds.dataFrameSubset(
     df.name = "analysis_df", 
     V1.name = new_obj, 
     V2.name = "1", 
     Boolean.operator = "==", 
     newobj = new_sub, 
     datasources = conns[unlist(coh)])
    
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_29")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_29")

################################################################################
# Define revised regression models
################################################################################

## ---- Main model using subset ------------------------------------------------
eth_main.fit <- eth_sub.mod %>%
  pmap(function(main_form, coh, new_sub, ...){
    
    ds.glm(
      formula = main_form,
      data = new_sub,
      family = "gaussian", 
      datasources = conns[unlist(coh)]
    )

  })

save.image("15_nov_22.RData")

## ---- Adjusting for ethnicity using subset -----------------------------------
eth_eth.fit <- eth_sub.mod %>%
  pmap(function(eth_form, coh, new_sub, ...){
    
    ds.glm(
      formula = eth_form,
      data = new_sub,
      family = "gaussian", 
      datasources = conns[unlist(coh)]
    )
    
  })

save.image("15_nov_22.RData")

################################################################################
# Repeat analyses removing DNBC and MoBa  
################################################################################
conns <- datashield.login(logindata, restore = "bmi_poc_sec_27")

## ---- Amend model definitions ------------------------------------------------
mat_ed_remove.mod <- dt.changeForm(
  model = mat_ed.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts", 
  type = "remove", 
  elements = names(mat_ed.mod))

mat_ed_remove.mod <- mat_ed_remove.mod[1:4]

area_dep_remove.mod <- dt.changeForm(
  model = area_dep.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts", 
  type = "remove", 
  elements = names(area_dep.mod))

area_dep_remove.mod <- area_dep_remove.mod[1:4]

ndvi_remove.mod <- dt.changeForm(
  model = ndvi.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts", 
  type = "remove", 
  elements = names(ndvi.mod))

ndvi_remove.mod <- ndvi_remove.mod[1:4]

preg_dia_remove.mod <- dt.changeForm(
  model = preg_dia.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts", 
  type = "remove",
  elements = names(preg_dia.mod))

preg_dia_remove.mod <- preg_dia_remove.mod[1:4]

## ---- Run models -------------------------------------------------------------
mat_ed_remove.fit <- list(
  ipd = mat_ed_remove.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"))

save.image("15_nov_22.RData")

## ---- Area deprivation -------------------------------------------------------
area_dep_remove.fit <- list(
  ipd = area_dep_remove.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"))

save.image("15_nov_22.RData")

## ---- NDVI -------------------------------------------------------------------
ndvi_remove.fit <- list(
  ipd = ndvi_remove.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"))

save.image("15_nov_22.RData")

## ---- Gestational diabetes ---------------------------------------------------
preg_dia_remove.fit <- list(
  ipd = preg_dia_remove.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"))

save.image("15_nov_22.RData")

################################################################################
# AJE REVIEWER ROUND 1  
################################################################################
################################################################################
# Identify number of observations per subject  
################################################################################
conns <- datashield.login(logindata, restore = "missing_2")

## ---- First create dataset with missing and just child_id --------------------
dh.dropCols(
  df = "bmi_poc", 
  vars = "child_id", 
  type = "keep", 
  new_obj = "baseline_min")

ds.dataFrame(
  x = c("baseline_min", "missing"),
  newobj = "missing_tmp")

## ---- Now create vectors indicating how many observations --------------------
obs_num.ref <- tribble(
  ~var, ~new_obj, ~exposure,
  "edu_m_zscores.0_730_m_fact", "edu_m_0_m", "edu_m",
  "edu_m_zscores.730_1461_m_fact", "edu_m_730_m", "edu_m",
  "edu_m_zscores.1461_2922_m_fact", "edu_m_1461_m", "edu_m",
  "edu_m_zscores.2922_5113_m_fact", "edu_m_2922_m", "edu_m",
  "edu_m_zscores.5113_6544_m_fact","edu_m_5113_m", "edu_m",
  "a_d_zscores.0_730_m_fact", "a_d_m_0", "a_d",
  "a_d_zscores.730_1461_m_fact", "a_d_m_730", "a_d",
  "a_d_zscores.1461_2922_m_fact", "a_d_m_1461", "a_d",
  "a_d_zscores.2922_5113_m_fact", "a_d_m_2922", "a_d",
  "a_d_zscores.5113_6544_m_fact", "a_d_m_5113", "a_d",
  "n_d_zscores.0_730_m_fact", "n_d_m_0", "n_d",
  "n_d_zscores.730_1461_m_fact", "n_d_m_730", "n_d",
  "n_d_zscores.1461_2922_m_fact", "n_d_m_1461", "n_d",
  "n_d_zscores.2922_5113_m_fact", "n_d_m_2922", "n_d",
  "n_d_zscores.5113_6544_m_fact", "n_d_m_5113", "n_d",
  "p_d_zscores.0_730_m_fact", "p_d_n_0", "p_d",
  "p_d_zscores.730_1461_m_fact", "p_d_n_730", "p_d",  
  "p_d_zscores.1461_2922_m_fact", "p_d_n_1461", "p_d",
  "p_d_zscores.2922_5113_m_fact", "p_d_n_2922", "p_d",
  "p_d_zscores.5113_6544_m_fact", "p_d_n_5113", "p_d") 

obs_num.ref %>%
  pmap(function(var, new_obj, ...){
    
    ds.asNumeric(var, new_obj)
    
  })

obs_num.ref %>%
  group_by(exposure) %>%
  group_split %>%
  map(function(x){
    
    ds.assign(
      toAssign = paste(x$new_obj, collapse = "+"), 
      newobj = paste0(x$exposure[1], "_n"))
    
  })

## ---- Convert back to factors ------------------------------------------------
n.vars <- c("edu_m_n", "a_d_n", "n_d_n", "p_d_n")
n.vars %>%  map(~ds.asFactor(.x, .x))

## ---- Join back into baseline_df ---------------------------------------------
ds.dataFrame(
  x = c("baseline_min", n.vars), 
  newobj = "baseline_ns")

## ---- Now left join back into final df ---------------------------------------
ds.merge(
  x.name = "analysis_df", 
  y.name = "baseline_ns", 
  by.x.names = "child_id", 
  by.y.names = "child_id", 
  all.x = TRUE,
  all.y = FALSE,
  newobj = "n_obs_df")

## ---- Drop unneeded columns --------------------------------------------------
dh.dropCols(
  df = "n_obs_df", 
  vars = c("child_id", n.vars), 
  type = "keep")

ds.dim("analysis_df")
ds.dim("n_obs_df")

## ---- Get stats --------------------------------------------------------------
n_obs.stats <- dh.getStats(
  df = "n_obs_df", 
  vars = n.vars)

datashield.workspace_save(conns, "bmi_n_obs")
conns <- datashield.login(logindata, restore = "bmi_n_obs")

################################################################################
# SELECTION BIAS  
################################################################################
################################################################################
# Define revised models  
################################################################################
sel_coh <- c("alspac", "dnbc", "nfbc66", "nfbc86", "raine")

## ---- Maternal education -----------------------------------------------------
mat_ed_sel.mod <- list(
  bmi_24 = list(
    outcome = "zscores.0_730",
    exposure = "edu_m",
    covariates = c("age_days.730", "sex"),
    cohorts = sel_coh), 
  bmi_48 = list(
    outcome = "zscores.730_1461",
    exposure = "edu_m",
    covariates = c("age_days.1461", "sex"),
    cohorts = sel_coh[sel_coh != "dnbc"]),
  bmi_96 = list(
    outcome = "zscores.1461_2922",
    exposure = "edu_m", 
    covariates = c("age_days.2922", "sex"),
    cohorts = sel_coh),
  bmi_168 = list(
    outcome = "zscores.2922_5113",
    exposure = "edu_m", 
    covariates = c("age_days.5113", "sex"),
    cohorts = sel_coh), 
  bmi_215 = list(
    outcome = "zscores.5113_6544",
    exposure = "edu_m", 
    covariates = c("age_days.6544", "sex"),
    cohorts = sel_coh))

## ---- Area deprivation -------------------------------------------------------
env_sel_coh <- c("alspac", "dnbc")

area_dep_sel.mod <- list(
  bmi_24 = list(
    outcome = "zscores.0_730",
    exposure = "area_dep",
    covariates = c("age_days.730", "sex", "edu_m"),
    cohorts = env_sel_coh), 
  bmi_48 = list(
    outcome = "zscores.730_1461",
    exposure = "area_dep",
    covariates = c("age_days.1461", "sex", "edu_m"),
    cohorts = "alspac"),
  bmi_96 = list(
    outcome = "zscores.1461_2922",
    exposure = "area_dep", 
    covariates = c("age_days.2922", "sex", "edu_m"),
    cohorts = env_sel_coh),
  bmi_168 = list(
    outcome = "zscores.2922_5113",
    exposure = "area_dep", 
    covariates = c("age_days.5113", "sex", "edu_m"),
    cohorts = env_sel_coh), 
  bmi_215 = list(
    outcome = "zscores.5113_6544",
    exposure = "area_dep", 
    covariates = c("age_days.6544", "sex", "edu_m"),
    cohorts = env_sel_coh))

## ---- NDVI -------------------------------------------------------------------
ndvi_sel.mod <- list(
  bmi_24 = list(
    outcome = "zscores.0_730",
    exposure = "ndvi300_preg_iqr_c",
    covariates = c("age_days.730", green_covs),
    cohorts = env_sel_coh), 
  bmi_48 = list(
    outcome = "zscores.730_1461",
    exposure = "ndvi300_preg_iqr_c",
    covariates = c("age_days.1461", green_covs),
    cohorts = "alspac"),
  bmi_96 = list(
    outcome = "zscores.1461_2922",
    exposure = "ndvi300_preg_iqr_c", 
    covariates = c("age_days.2922", green_covs),
    cohorts = env_sel_coh), 
  bmi_168 = list(
    outcome = "zscores.2922_5113",
    exposure = "ndvi300_preg_iqr_c", 
    covariates = c("age_days.5113", green_covs),
    cohorts = env_sel_coh), 
  bmi_215 = list(
    outcome = "zscores.5113_6544",
    exposure = "ndvi300_preg_iqr_c", 
    covariates = c("age_days.6544", "sex", "edu_m"),
    cohorts = env_sel_coh))


## ---- Gestational diabetes ---------------------------------------------------
preg_dia_sel_coh <- c("alspac", "dnbc", "raine")

preg_dia_sel.mod <- list(
  bmi_24 = list(
    outcome = "zscores.0_730",
    exposure = "preg_dia",
    covariates = c("age_days.730", preg_dia_cov),
    cohorts = preg_dia_sel_coh), 
  bmi_48 = list(
    outcome = "zscores.730_1461",
    exposure = "preg_dia",
    covariates = c("age_days.1461", preg_dia_cov),
    cohorts = c("alspac", "raine")),
  bmi_96 = list(
    outcome = "zscores.1461_2922",
    exposure = "preg_dia", 
    covariates = c("age_days.2922", preg_dia_cov),
    cohorts = preg_dia_sel_coh), 
  bmi_168 = list(
    outcome = "zscores.2922_5113",
    exposure = "preg_dia", 
    covariates = c("age_days.5113", preg_dia_cov),
    cohorts = preg_dia_sel_coh), 
  bmi_215 = list(
    outcome = "zscores.5113_6544",
    exposure = "preg_dia", 
    covariates = c("age_days.6544", preg_dia_cov),
    cohorts = preg_dia_sel_coh))

################################################################################
# Create subset containing only participants at older ages  
################################################################################
conns <- datashield.login(logindata, restore = "missing_2")

## ---- Maternal education -----------------------------------------------------
ds.dataFrameSubset(
  df.name = "bmi_poc", 
  V1.name = "edu_m_zscores.5113_6544_m_fact", 
  V2.name = "1",
  Boolean.operator = "==",
  newobj = "edu_m_sel", 
  datasources = conns[sel_coh])

## ---- Area deprivation -------------------------------------------------------
ds.dataFrameSubset(
  df.name = "bmi_poc", 
  V1.name = "a_d_zscores.5113_6544_m_fact", 
  V2.name = "1",
  Boolean.operator = "==",
  newobj = "area_dep_sel", 
  datasources = conns[env_sel_coh])

## ---- NDVI -------------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "bmi_poc", 
  V1.name = "n_d_zscores.5113_6544_m_fact", 
  V2.name = "1",
  Boolean.operator = "==",
  newobj = "ndvi_sel", 
  datasources = conns[env_sel_coh])

## ---- Pregnancy diabetes -----------------------------------------------------
ds.dataFrameSubset(
  df.name = "bmi_poc", 
  V1.name = "p_d_zscores.5113_6544_m_fact", 
  V2.name = "1",
  Boolean.operator = "==",
  newobj = "preg_dia_sel", 
  datasources = conns[preg_dia_sel_coh])

datashield.workspace_save(conns, "selection")
conns <- datashield.login(logindata, restore = "selection")
################################################################################
# Re-run models  
################################################################################

## ---- Maternal education -----------------------------------------------------
mat_ed_sel.fit <- mat_ed_sel.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd", df = "edu_m_sel")

## ---- Area deprivation -------------------------------------------------------
area_dep_sel.form <- area_dep_sel.mod %>% 
  map(dt.makeGlmForm, type = "ipd")

area_dep_sel.form$bmi_48$model <- "zscores.730_1461~area_dep+age_days.1461+sex+edu_m"

area_dep_sel.fit <- area_dep_sel.form %>%
  map(dt.glmWrap, type = "ipd", df = "area_dep_sel")

## ---- NDVI -------------------------------------------------------------------
ndvi_sel.form <- ndvi_sel.mod %>% 
  map(dt.makeGlmForm, type = "ipd")

ndvi_sel.form$bmi_48$model <- "zscores.730_1461~ndvi300_preg_iqr_c+age_days.1461+sex+edu_m+parity_bin+area_dep"

ndvi_sel.fit <- ndvi_sel.form %>%
  map(dt.glmWrap, type = "ipd", df = "ndvi_sel")

## ---- Pregnancy diabetes -----------------------------------------------------
preg_dia_sel.fit <- preg_dia_sel.mod %>% 
  map(dt.makeGlmForm, type = "ipd") %>% 
  map(dt.glmWrap, type = "ipd", df = "preg_dia_sel")

save.image("15_nov_22.RData")
################################################################################
# Get Ns for sensitivity analysis for revised figures  
################################################################################
################################################################################
# New models for complete cases   
################################################################################
mat_ed_sens_miss.mod <- mat_ed_sel.mod %>% map(dh.modToMiss, suffix = "_s")

area_dep_sens_miss.mod <- area_dep_sel.mod %>%
  map(function(x){list_modify(x, exposure = "a_d_s")}) %>%
  map(dh.modToMiss, suffix = "_s")

ndvi_sens_miss.mod <- ndvi_sel.mod %>% 
  map(function(x){list_modify(x, exposure = "n_d_s")}) %>%
  map(dh.modToMiss, suffix = "_s")

preg_dia_sens_miss.mod <- preg_dia_sel.mod %>% 
  map(function(x){list_modify(x, exposure = "p_d_s")}) %>%
  map(dh.modToMiss, suffix = "_s")

save.image("15_nov_22.RData")

################################################################################
# Define complete cases  
################################################################################

## ---- Maternal education -----------------------------------------------------
mat_ed_sens_miss.mod %>%
  map(., function(x){
    
    pmap(x, function(vars, name, cohorts){
      
      dh.defineCases(
        df = "edu_m_sel", 
        vars = vars, 
        new_obj = name, 
        type = "all", 
        conns = conns[cohorts], 
        checks = FALSE)
    })
  })

## ---- Area deprivation -------------------------------------------------------
ds.make(
  toAssign = "area_dep_sel$area_dep", 
  newobj = "a_d_s", 
  datasources = conns[env_sel_coh])

ds.dataFrame(
  x = c("area_dep_sel", "a_d_s"), 
  newobj = "area_dep_sel", 
  datasources = conns[env_sel_coh])

area_dep_sens_miss.mod %>%
  map(., function(x){
    
    pmap(x, function(vars, name, cohorts){
      
      dh.defineCases(
        df = "area_dep_sel", 
        vars = vars, 
        new_obj = name, 
        type = "all", 
        conns = conns[cohorts], 
        checks = FALSE)
      
    })
  })

## ---- NDVI -------------------------------------------------------------------
ds.make(
  toAssign = "ndvi_sel$ndvi300_preg_iqr_c", 
  newobj = "n_d_s", 
  datasources = conns[env_sel_coh])

ds.dataFrame(
  x = c("ndvi_sel", "n_d_s"), 
  newobj = "ndvi_sel", 
  datasources = conns[env_sel_coh])

ndvi_sens_miss.mod %>%
  map(., function(x){
    
    pmap(x, function(vars, name, cohorts){
      
      dh.defineCases(
        df = "ndvi_sel", 
        vars = vars, 
        new_obj = name, 
        type = "all", 
        conns = conns[cohorts], 
        checks = FALSE)
      
    })
  })

## ---- Pregnancy diabetes -----------------------------------------------------
ds.make(
  toAssign = "preg_dia_sel$preg_dia", 
  newobj = "p_d_s", 
  datasources = conns[preg_dia_sel_coh])

ds.dataFrame(
  x = c("preg_dia_sel", "p_d_s"), 
  newobj = "preg_dia_sel", 
  datasources = conns[preg_dia_sel_coh])

preg_dia_sens_miss.mod %>%
  map(., function(x){
    
    pmap(x, function(vars, name, cohorts){
      
      dh.defineCases(
        df = "preg_dia_sel", 
        vars = vars, 
        new_obj = name, 
        type = "all", 
        conns = conns[cohorts], 
        checks = FALSE)
      
    })
  })

save.image("15_nov_22.RData")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "selection_1")
conns <- datashield.login(logindata, restore = "selection_1")

################################################################################
# Categorical variables: combined
################################################################################
missRef <- function(mod){

mod %>%
  map(function(x){
    
    tibble(
      cohort_str = paste(x$cohorts, collapse = ","), 
      variable = paste(x$name, collapse = ","))
    
  }) %>%
  bind_rows
  
}

sens_n.ref <- bind_rows(
  missRef(mat_ed_sens_miss.mod) %>% mutate(exposure = "edu_m"),
  missRef(area_dep_sens_miss.mod) %>% mutate(exposure = "a_d_s"),
  missRef(preg_dia_sens_miss.mod) %>% mutate(exposure = "p_d_s")) %>%
  mutate(df = c(
    rep("edu_m_sel", 5),
    rep("area_dep_sel", 5),
    rep("preg_dia_sel", 5)))

save.image("15_nov_22.RData")

## ---- IPD --------------------------------------------------------------------
sens_cat <- sens_n.ref %>%
  pmap(function(variable, cohort_str, exposure, df){
    
    ds.table(
      rvar = paste0(df, "$", exposure), 
      cvar = variable,
      useNA = "always",
      datasources = conns[str_split(cohort_str, ",")[[1]]])
    
  }) 

save.image("15_nov_22.RData")
################################################################################
# Categorical variables: cohort specific
################################################################################
missRefCoh <- function(mod, exposure, df){
  
  mod %>%
    map(function(x){
      
      tibble(
        cohort = x$cohorts,
        variable = x$name, 
        exposure = exposure,
        df = df)
      
    }) %>%
    bind_rows
  
}

## ---- Reference table --------------------------------------------------------
sens_cat_coh.ref <- bind_rows(
  missRefCoh(mat_ed_sens_miss.mod, exposure = "edu_m", df = "edu_m_sel"),
  missRefCoh(area_dep_sens_miss.mod, exposure = "a_d_s", df = "area_dep_sel"),
  missRefCoh(ndvi_sens_miss.mod, exposure = "n_d_s", df = "ndvi_sel"),
  missRefCoh(preg_dia_sens_miss.mod, exposure = "p_d_s", df = "preg_dia_sel"))
  
## ---- Get variables ----------------------------------------------------------
sens_cat_coh <- sens_cat_coh.ref %>%
  dplyr::filter(exposure != "n_d_s") %>%
  pmap(function(variable, cohort, exposure, df){
    
    ds.table(
      rvar = paste0(df, "$", exposure), 
      cvar = variable,
      useNA = "always",
      datasources = conns[cohort])
    
  }) 

save.image("15_nov_22.RData")




