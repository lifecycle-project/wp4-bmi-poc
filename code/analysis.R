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
library(dsHelper)

conns <- datashield.login(logindata, restore = "bmi_poc_sec_25")

ds.colnames("analysis_df")

library(remotes)
install_github("lifecycle-project/ds-helper")
library(dsHelper)

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
  conns = conns[eth_coh], 
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
  conns = conns[eth_coh])

descriptives_oth_exc <- dh.getStats(
  df = "excluded_df",
  vars = other.vars[!other.vars %in% c("child_id", coh_dummy$cohort)], 
  digits = 1,
  conns = conns)

descriptives_oth_exc$continuous %>%
  dplyr::filter(cohort == "elfe") %>%
  print(n = Inf)

## ---- Bind together ----------------------------------------------------------
descriptives_exc <- list(
  descriptives_exp_exc, descriptives_out_exc, descriptives_cov_exc, 
  descriptives_eth_exc, descriptives_oth_exc) %>%
  pmap(bind_rows)

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


## ---- See available outcome and exposure data --------------------------------
descriptives$continuous %>% 
  dplyr::filter(variable == "zscores.0_730" & is.nan(mean))

descriptives$continuous %>% 
  dplyr::filter(variable == "zscores.730_1461" & is.nan(mean))

descriptives$continuous %>% 
  dplyr::filter(variable == "zscores.1461_2922" & is.nan(mean)) 

descriptives$continuous %>% 
  dplyr::filter(variable == "zscores.2922_5113" & is.nan(mean))  

descriptives$continuous %>%
dplyr::filter(variable == "zscores.5113_6544" & !is.nan(mean))  

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
      "alspac", "dnbc", "nfbc66", "nfbc86", "raine")])
)


## ---- Area deprivation -------------------------------------------------------
descriptives[[1]] %>% 
  dplyr::filter(variable == "area_dep" & valid_n > 0) %>%
  print(n = Inf)

env_coh <- c("alspac", "bib", "dnbc", "eden", "genr", "inma", "ninfea", "moba", 
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
    covariates = c("age_days.5113", "sex", "edu_m"),
    cohorts = c("alspac", "dnbc"))
)

## ---- NDVI -------------------------------------------------------------------
green_covs <- c("sex", "edu_m", "parity_bin", "area_dep")

ndvi.mod <- list(
  bmi_24 = list(
    outcome = "zscores.0_730",
    exposure = "ndvi300_preg_iqr_s",
    covariates = c("age_days.730", green_covs),
    cohorts = env_coh), 
  bmi_48 = list(
    outcome = "zscores.730_1461",
    exposure = "ndvi300_preg_iqr_s",
    covariates = c("age_days.1461", green_covs),
    cohorts = env_coh[!env_coh %in% c("dnbc")]),
  bmi_96 = list(
    outcome = "zscores.1461_2922",
    exposure = "ndvi300_preg_iqr_s", 
    covariates = c("age_days.2922", green_covs),
    cohorts = env_coh), 
  bmi_168 = list(
    outcome = "zscores.2922_5113",
    exposure = "ndvi300_preg_iqr_s", 
    covariates = c("age_days.5113", green_covs),
    cohorts = env_coh), 
  bmi_215 = list(
    outcome = "zscores.5113_6544",
    exposure = "ndvi300_preg_iqr_s", 
    covariates = c("age_days.5113", "sex", "edu_m"),
    cohorts = c("alspac", "dnbc"))
)


## ---- Gestational diabetes ---------------------------------------------------
descriptives[[1]] %>% 
  dplyr::filter(variable == "preg_dia") %>% 
  print(n = Inf)

descriptives$categorical %>% 
  dplyr::filter(variable == "preg_smk") %>% print(n = Inf)

descriptives$categorical %>% 
  dplyr::filter(variable == "parity_bin") %>% print(n = Inf)

## Note we are excluding HGS for now as they have no-one nulliparous, though
## I wonder if this is a coding issue.

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
    covariates = c("age_days.5113", preg_dia_cov),
    cohorts = cohorts[cohorts %in% c(
      "alspac", "dnbc", "raine")])
  )
  
################################################################################
# 4. Exploring missingness  
################################################################################
################################################################################
# Define models  
################################################################################
conns <- datashield.login(logindata, restore = "bmi_poc_sec_25")

## ---- Function to modify models ----------------------------------------------
dh.modToMiss <- function(x){
  
  out <- list(
    vars = list(c(x$exposure, x$outcome, x$covariates)), 
    cohorts = x$cohorts, 
    name = paste0(x$exposure, "_", x$outcome, "_m"))
  
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

save.image()

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
  toAssign = "bmi_poc$ndvi300_preg_iqr_s", 
  newobj = "n_d")

ds.dataFrame(
  x = c("bmi_poc", "n_d"), 
  newobj = "bmi_poc")

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
  newobj = "a_d")

ds.dataFrame(
  x = c("bmi_poc", "a_d"), 
  newobj = "bmi_poc")

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

save.image()


################################################################################
# 5. Run core models  
################################################################################
conns <- datashield.login(logindata, restore = "bmi_poc_sec_25")

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

save.image()

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

save.image()


## ---- SLMA -------------------------------------------------------------------


save.image()


################################################################################
# 7. Results stratified by sex  
################################################################################
conns <- datashield.login(logindata, restore = "bmi_poc_sec_25")

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
datashield.workspace_save(conns, "bmi_poc_sec_26")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_26")

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


## ---- Analysis for males -----------------------------------------------------
mat_ed_m.fit <- list(
  ipd = mat_ed_sex.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd", df = "analysis_df_m"), 
  slma = mat_ed_sex.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma", df = "analysis_df_m"))

save.image()



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

save.image()

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

save.image()

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
  
save.image()      

## ---- Analysis for females ---------------------------------------------------
mat_ed_f.fit <- list(
  ipd = mat_ed_sex.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd", df = "analysis_df_f"), 
  slma = mat_ed_sex.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma", df = "analysis_df_f"))

save.image()



area_dep_f.fit <- list(
  ipd = area_dep_sex_m.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd", df = "analysis_df_f"), 
  slma = area_dep_sex_m.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma", df = "analysis_df_f"))



save.image()

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

save.image()

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

save.image()

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
  var = "ndvi300_preg_iqr_s*sex", 
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

save.image()

area_dep_int.fit <- list(
  ipd = area_dep_int.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"), 
  slma = area_dep_int.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

save.image()

ndvi_int.fit <- list(
  ipd = ndvi_int.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"), 
  slma = ndvi_int.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

save.image()

preg_dia_int.fit <- list(
  ipd = preg_dia_int.mod %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"), 
  slma = preg_dia_int.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

save.image()

################################################################################
# ADDITIONALLY ADJUSTING FOR ETHNICITY
################################################################################
################################################################################
# Create subsets
################################################################################



################################################################################
# Define models
################################################################################
eth_coh <- c("alspac", "bib", "elfe", "gecko", "genr", "inma", "raine")

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
    cohorts = "alspac"))


## ---- Area deprivation -------------------------------------------------------
env_eth_coh <- c("alspac", "bib", "genr", "inma")

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
    covariates = c("age_days.5113", "sex", "edu_m", "ethn3_m_f"),
    cohorts = "alspac"))

## ---- NDVI -------------------------------------------------------------------
ndvi_eth.mod <- list(
  bmi_24 = list(
    outcome = "zscores.0_730",
    exposure = "ndvi300_preg_iqr_s",
    covariates = c("age_days.730", green_covs, "ethn3_m_f"),
    cohorts = env_eth_coh), 
  bmi_48 = list(
    outcome = "zscores.730_1461",
    exposure = "ndvi300_preg_iqr_s",
    covariates = c("age_days.1461", green_covs, "ethn3_m_f"),
    cohorts = env_eth_coh),
  bmi_96 = list(
    outcome = "zscores.1461_2922",
    exposure = "ndvi300_preg_iqr_s", 
    covariates = c("age_days.2922", green_covs, "ethn3_m_f"),
    cohorts = env_eth_coh), 
  bmi_168 = list(
    outcome = "zscores.2922_5113",
    exposure = "ndvi300_preg_iqr_s", 
    covariates = c("age_days.5113", green_covs, "ethn3_m_f"),
    cohorts = env_eth_coh), 
  bmi_215 = list(
    outcome = "zscores.5113_6544",
    exposure = "ndvi300_preg_iqr_s", 
    covariates = c("age_days.5113", green_covs, "ethn3_m_f"),
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
    covariates = c("age_days.5113", preg_dia_cov, "ethn3_m_f"),
    cohorts = "alspac"))

################################################################################
# Define subsets
################################################################################
conns <- datashield.login(logindata, restore = "bmi_poc_sec_25")

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

## ---- Remove some objects ----------------------------------------------------
eth_sub.mod %>%
  pmap(function(coh, new_obj, ...){

dh.tidyEnv(
  obj = new_obj,
  conns = conns[unlist(coh)], 
  type = "remove")
    
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "bmi_poc_sec_27")
conns <- datashield.login(logindata, restore = "bmi_poc_sec_27")

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

save.image()


  
  tibble(
  
)










################################################################################
# Run models
################################################################################
conns <- datashield.login(logindata, restore = "bmi_poc_sec_25")

## ---- Maternal education -----------------------------------------------------
mat_ed_eth.fit <- list(
  slma = mat_ed_eth.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

## ---- Area deprivation -------------------------------------------------------
area_dep_eth.fit <- list(
  slma = area_dep_eth.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

## ---- NDVI -------------------------------------------------------------------
ndvi_eth.fit <- list(
  slma = ndvi_eth.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

## ---- Gestational diabetes ---------------------------------------------------
preg_dia_eth.fit <- list(
  slma = preg_dia_eth.mod %>% 
    map(dt.makeGlmForm, type = "slma") %>% 
    map(dt.glmWrap, type = "slma"))

save.image()

################################################################################
# 8. Repeat analyses removing DNBC and MoBa  
################################################################################
conns <- datashield.login(logindata, restore = "bmi_poc_sec_25")

## ---- Amend model definitions ------------------------------------------------
mat_ed_remove.mod <- dt.changeForm(
  model = mat_ed.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts", 
  type = "remove", 
  elements = names(mat_ed.mod))

area_dep_remove.mod <- dt.changeForm(
  model = area_dep.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts", 
  type = "remove", 
  elements = names(area_dep.mod))

ndvi_remove.mod <- dt.changeForm(
  model = ndvi.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts", 
  type = "remove", 
  elements = names(ndvi.mod))

preg_dia_remove.mod <- dt.changeForm(
  model = preg_dia.mod, 
  var = c("dnbc", "moba"), 
  category = "cohorts", 
  type = "remove",
  elements = names(preg_dia.mod))

## ---- Run models -------------------------------------------------------------
mat_ed_remove.fit <- list(
  ipd = mat_ed_remove.mod[1:4] %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"))

save.image()

## ---- Area deprivation -------------------------------------------------------
area_dep_remove.fit <- list(
  ipd = area_dep_remove.mod[1:4] %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"))

save.image()

## ---- NDVI -------------------------------------------------------------------
ndvi_remove.fit <- list(
  ipd = ndvi_remove.mod[1:4] %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"))

save.image()

## ---- Gestational diabetes ---------------------------------------------------
preg_dia_remove.fit <- list(
  ipd = preg_dia_remove.mod[1:4] %>% 
    map(dt.makeGlmForm, type = "ipd") %>% 
    map(dt.glmWrap, type = "ipd"))

save.image()




