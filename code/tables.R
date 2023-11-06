################################################################################
## Project: bmi-poc
## Script purpose: Make tables for paper
## Date: 29th April 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

# Here we put the results from "bmi-poc-analysis.R" into some tables for the
# paper
#
# Remaining functionality required is to be able to show median - I haven't
# worked out how to do this in DS yet

library(dsHelper)

conns <- datashield.login(logindata, restore = "bmi_poc_sec_27")

################################################################################
# METHODS  
################################################################################
################################################################################
# List participating cohorts  
################################################################################
coh_inc <- names(conns) %>% sort()
coh_n <- length(coh_inc)


################################################################################
# Maximum sample size
################################################################################

## ---- Get dimensions ---------------------------------------------------------
bmi_dim <- ds.dim("bmi_poc")
analysis_dim <- ds.dim("analysis_df")

## ---- Original dataset -------------------------------------------------------
original_n <- bmi_dim[length(bmi_dim)][[1]][1]
final_n <- analysis_dim[length(analysis_dim)][[1]][1]
removed_n <- original_n - final_n

## ---- Maximum sample sizes ---------------------------------------------------
miss_descriptives$categorical %>%
  dplyr::select(variable, cohort, category, value) %>%
  dplyr::filter(
    cohort == "combined" & 
      variable %in% c(
    "edu_m_zscores.0_730_m_fact", "a_d_zscores.0_730_m_fact", 
    "n_d_zscores.0_730_m_fact", "p_d_zscores.0_730_m_fact") &
      category == 1)

## ---- Cohort numbers ---------------------------------------------------------
cohort_ns <- analysis_dim %>%
  map(~.[[1]] %>% as_tibble) %>%
  set_names(c(names(conns), "combined")) %>%
  bind_rows(.id = "cohort")


################################################################################
# Data prep  
################################################################################

## Make tibble of cohort names and sample sizes as I want to display in paper
ref_tab <- cohort_ns %>%
  left_join(., names_neat, by = "cohort") %>%
  mutate(
    cohort_neat = factor(
      cohort_neat, 
      levels = names_neat$cohort_neat, 
      ordered = TRUE)) %>%
  arrange(cohort_neat) %>%
  mutate(
    names_neat = paste0(cohort_neat, " (n=", value, ")")) %>%
  mutate(names_neat = factor(names_neat, levels = names_neat, ordered = TRUE)) %>%
  dplyr::select(-value)
  
################################################################################
# Table 1 info
################################################################################
table_1_n <- cohort_ns %>%
  dplyr::filter(cohort != "combined") 

table_1_ages <- descriptives$continuous %>%
  dplyr::filter(variable %in% c(
    "age_days.730", "age_days.1461", "age_days.2922", "age_days.5113", 
    "age_days.6544")) %>%
  dplyr::select(variable, cohort, perc_5, perc_95) %>%
  dplyr::filter(cohort != "combined") %>%
  group_by(cohort) %>%
  group_split %>%
  map(function(x){
    
    x %>%
      summarise(
        low = min(perc_5, na.rm = TRUE), 
        high = max(perc_95, na.rm = TRUE))
    }) %>%
  set_names(names(conns)) %>%
  bind_rows(.id = "cohort") %>%
  mutate(across(low:high, ~round(.x / 365.2422))) %>%
  mutate(out = paste0(low, " - ", high))

table_1_n %>% print(n = Inf)
write_csv(table_1_ages, file = here("tables", "table_1_ages.csv"))

################################################################################
# Availability of exposures within each cohort
################################################################################

## ---- Exposures --------------------------------------------------------------
avail_exp <- dh.anyData(
  df = "analysis_df", 
  vars = exp.vars)

## ---- Maternal ethnicity -----------------------------------------------------
avail_eth <- dh.anyData(
  df = "analysis_df", 
  vars = "ethn3_m")

eth.stats <- dh.getStats(
  df = "analysis_df", 
  vars = "ethn3_m")

eth.stats$categorical %>%
  print(n = Inf)

eth_coh <- c("alspac", "bib", "chop", "elfe", "gecko", "genr", "inma", "raine")

################################################################################
# Table S4: Sample characteristics analysis sample vs excluded  
################################################################################

makeSampleComp <- function(x){
  
  tmp_1 <- x$continuous %>%
    dplyr::filter(
      variable %in% c(
        "ndvi300_preg", "zscores.0_730", "zscores.730_1461", "zscores.1461_2922", 
        "zscores.2922_5113", "zscores.5113_6544", "height_m", "ga_all", 
        "agebirth_m_y") &
        cohort == "combined") %>%
    mutate(
      value = paste0(perc_50, " (", perc_25, ",", perc_75, ")"), 
      missing = paste0(missing_n, " (", missing_perc, ")")) %>%
    dplyr::select(variable, value, missing, cohort_n) 
  
  tmp_2 <- x$categorical %>%
    dplyr::filter(
      variable %in% c(
        "edu_m", "preg_dia", "area_dep", "sex", "preg_smk", "preg_ht", 
        "parity_bin", "ethn3_m_f", "prepreg_bmi_u_f", "prepreg_bmi_o_f") &
        cohort == "combined" &
        !is.na(category)) %>%
    mutate(
      value = paste0(value, " (", perc_total, ")"), 
      missing = paste0(missing_n, " (", perc_missing, ")")) %>%
    dplyr::select(variable, category, value, missing, cohort_n)
  
  bind_rows(tmp_1, tmp_2)
  
}

x <- descriptives_exc

a_sample.desc <- makeSampleComp(descriptives) %>% 
  mutate(sample = "analysis")

e_sample.desc <- makeSampleComp(descriptives_exc) %>% 
  mutate(sample = "excluded")

sample_comp <- bind_rows(a_sample.desc, e_sample.desc) %>%
  arrange(sample, variable)

var_order <- c(
  "agebirth_m_y", "area_dep", "zscores.0_730", "zscores.730_1461", "zscores.1461_2922", 
  "zscores.2922_5113", "zscores.5113_6544", "edu_m", "ethn3_m_f", "ga_all", "height_m",
  "ndvi300_preg", "parity_bin", "preg_dia", "preg_ht", "preg_smk", 
  "prepreg_bmi_u_f", "prepreg_bmi_o_f", "sex")

sample_comp <- sample_comp %>% 
  mutate(variable = factor(variable, levels = var_order, ordered = TRUE)) %>%
  arrange(sample, variable) 

write_csv(sample_comp, here("tables", "sample-comparison.csv"))

save.image()

################################################################################
# Table S5: Ns for complete cases  
################################################################################
cc.tab <- miss_descriptives$categorical %>%
  mutate(variable = str_remove(variable, "_m_fact")) %>%
  separate(variable, sep = "\\.", into = c("variable", "age")) %>%
  dplyr::filter(cohort == "combined" & category == "1") %>%
  mutate(n_perc = paste0(value, " (", perc_valid, ")")) %>%
  dplyr::select(variable, age, category, n_perc) %>%
  pivot_wider(names_from = "category", values_from = "n_perc") %>%
  mutate(age = factor(
    age, 
    levels = c("0_730", "730_1461", "1461_2922", "2922_5113", "5113_6544"),
    ordered = TRUE)) %>%
  arrange(variable, age)

write_csv(cc.tab, here("tables", "complete_cases.csv"))  

miss_descriptives$categorical %>% print(n = Inf)

################################################################################
# Table S6: Covariate descriptive statistics  
################################################################################
cov_cat.tab <- descriptives$categorical %>%
  dplyr::filter(variable %in% c("sex", "parity_bin", "preg_smk", "ethn3_m_f", 
                                "prepreg_bmi_u_f", "prepreg_bmi_o_f")) %>%
  mutate(n_perc = paste0(value, " (", perc_total, ")")) %>%
  dplyr::select(cohort, variable, category, n_perc) %>% 
  pivot_wider(
    names_from = c(variable, category),  
    values_from = n_perc)

cov_cont.tab <- descriptives$continuous %>%
  dplyr::filter(variable %in% c("agebirth_m_y")) %>%
  mutate(med_range = paste0(perc_50, " (", perc_25, ", ", perc_75, ")"),
         missing = paste0(missing_n, " (", missing_perc, ")")) %>%
  dplyr::select(cohort, variable, med_range, missing) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, missing))

cov.tab <- left_join(cov_cat.tab, cov_cont.tab, by = "cohort") %>%
  left_join(., ref_tab, by = "cohort") %>%
  arrange(names_neat) 

cov_a.tab <- cov.tab %>%
  dplyr::select(names_neat, sex_1, sex_NA, parity_bin_0, parity_bin_NA, 
                ethn3_m_f_0, ethn3_m_f_1, ethn3_m_f_NA, preg_smk_1, 
                preg_smk_NA)

cov_b.tab <- cov.tab %>%
  dplyr::select(names_neat, prepreg_bmi_u_f_1, prepreg_bmi_u_f_NA, 
                prepreg_bmi_o_f_1, prepreg_bmi_o_f_NA, med_range_agebirth_m_y, 
                missing_agebirth_m_y)

write_csv(cov_a.tab, file = here("tables", "covariates_a.csv"))
write_csv(cov_b.tab, file = here("tables", "covariates_b.csv"))

cov_b.tab %>% dplyr::select(-preg_ht_1, preg_ht_NA) %>%
  print(n = Inf)

################################################################################
# Table S6: BMI descriptives   
################################################################################
outcomes.tab <- descriptives$continuous %>%
  dplyr::filter(variable %in% c(
    "zscores.0_730", "zscores.730_1461", "zscores.1461_2922", 
    "zscores.2922_5113", "zscores.5113_6544")) %>%
  mutate(valid_n = ifelse(valid_n < 20, NA, valid_n)) %>%
  mutate(med_range = paste0(perc_50, " (", perc_25, ", ", perc_75, ")")) %>%
  dplyr::select(cohort, variable, med_range, valid_n) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, valid_n)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  dplyr::select(
    names_neat, valid_n_zscores.0_730, med_range_zscores.0_730, 
    valid_n_zscores.730_1461, med_range_zscores.730_1461, 
    valid_n_zscores.1461_2922, med_range_zscores.1461_2922, 
    valid_n_zscores.2922_5113, med_range_zscores.2922_5113, 
    valid_n_zscores.5113_6544, med_range_zscores.5113_6544) %>%
  arrange(names_neat)

write_csv(outcomes.tab, file = here("tables", "outcomes.csv"))


################################################################################
# Table S7: Height descriptives  
################################################################################
ht.tab <- descriptives$continuous %>%
  dplyr::filter(variable %in% c(
    "ht.730", "ht.1461", "ht.2922", "ht.5113", "ht.6544")) %>%
  mutate(valid_n = ifelse(valid_n < 20, NA, valid_n)) %>%
  mutate(med_range = paste0(perc_50, " (", perc_25, ", ", perc_75, ")")) %>%
  dplyr::select(cohort, variable, med_range, valid_n) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, valid_n)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  select(names_neat, valid_n_ht.730, med_range_ht.730, valid_n_ht.1461, 
         med_range_ht.1461, valid_n_ht.2922, med_range_ht.2922, valid_n_ht.5113, 
         med_range_ht.5113, valid_n_ht.6544, med_range_ht.6544) %>%
  arrange(names_neat) 

write_csv(ht.tab, file = here("tables", "height.csv"))


################################################################################
# Table S8: Weight descriptives   
################################################################################
wt.tab <- descriptives$continuous %>%
  dplyr::filter(variable %in% c(
    "wt.730", "wt.1461", "wt.2922", "wt.5113", "wt.6544")) %>%
  mutate(valid_n = ifelse(valid_n < 20, NA, valid_n)) %>%
  mutate(med_range = paste0(perc_50, " (", perc_25, ", ", perc_75, ")")) %>%
  dplyr::select(cohort, variable, med_range, valid_n) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, valid_n)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  select(names_neat, valid_n_wt.730, med_range_wt.730, valid_n_wt.1461, 
         med_range_wt.1461, valid_n_wt.2922, med_range_wt.2922, valid_n_wt.5113, 
         med_range_wt.5113, valid_n_wt.6544, med_range_wt.6544) %>%
  arrange(names_neat) 

write_csv(wt.tab, file = here("tables", "weight.csv"))


################################################################################
# Table S9: Median age at BMI measurement  
################################################################################
ages.tab <- descriptives$continuous %>%
  dplyr::filter(variable %in% c(
      "age_months.24", "age_months.48", "age_months.96", "age_months.168",
      "age_months.215")) %>%
  mutate(valid_n = ifelse(valid_n < 20, NA, valid_n)) %>%
  mutate(med_range = paste0(perc_50, " (", perc_25, ", ", perc_75, ")")) %>%
  dplyr::select(cohort, variable, med_range, valid_n) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, valid_n)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  dplyr::select(names_neat, valid_n_age_months.24,  med_range_age_months.24, 
         valid_n_age_months.48,  med_range_age_months.48, valid_n_age_months.96,  
         med_range_age_months.96, valid_n_age_months.168,  
         med_range_age_months.168, valid_n_age_months.215, 
         med_range_age_months.215) %>%
  arrange(names_neat) 

write_csv(ages.tab, file = here("tables", "measurement_ages.csv"))


################################################################################
# SENSITIVITY ANALYSES
################################################################################
################################################################################
# Table S11: GDM sensitivity 
################################################################################

## ---- Get estimates ----------------------------------------------------------
gdm_sens <- preg_dia_slma.plotdata %>%
  dplyr::select(cohort, age, est = beta, se, lowci = ci_5, uppci = ci_95)

gdm_coh <- c("bib", "eden")

gdm_quest <- gdm_sens %>% dplyr::filter(!cohort %in% gdm_coh & age != "14-17")
gdm_blood <- gdm_sens %>% dplyr::filter(cohort %in% gdm_coh & age != "14-17")

metaGroup <- function(x){
  
  x %>%
    group_by(age) %>%
    group_split %>%
    map(function(x){
      
      meta <- rma.uni(yi = x$est, sei = x$se)
      out <- tibble(
        age = x$age[1],
        est = meta$beta[1, 1],
        lowci = meta$ci.lb,
        uppci = meta$ci.ub)
      
      return(out)
      
    }) 
}

ages <- c("0-1", "2-3", "4-7", "8-13")

gdm_quest.pdata <- metaGroup(gdm_quest) %>% 
  bind_rows %>%
  mutate(type = "quest")

gdm_blood.pdata <- metaGroup(gdm_blood) %>% 
  bind_rows %>%
  mutate(type = "blood")

gdm_ns <- ns_cat_all %>%
  dplyr::filter(exposure == "p_d" & cohort != "combined" & age != "14-17") %>%
  mutate(type = ifelse(cohort %in% gdm_coh, "blood", "quest")) %>%
  group_by(type, category, age) %>%
  group_split %>%
  map(., ~mutate(., n = sum(count))) %>%
  map(., ~slice(., 1)) %>%
  map(., ~dplyr::select(., age, category, variable, n, type)) %>%
  bind_rows %>%
  dplyr::select(-variable) %>%
  pivot_wider(
    names_from = category, 
    values_from = n) %>%
  dplyr::rename(
    unexposed = '0',
    exposed = '1') %>%
  arrange(desc(type), desc(age)) %>%
  mutate(age = as.character(age))

gdm_sens.tab <- bind_rows(gdm_blood.pdata, gdm_quest.pdata) %>%
  left_join(., gdm_ns, by = c("age", "type")) %>%
  mutate(across(est:uppci, ~round(., 2))) %>%
  mutate(est = paste0(est, " (", lowci, ", ", uppci, ")")) %>%
  dplyr::select(-lowci, -uppci) %>%
  pivot_wider(
    names_from = type,
    values_from = c(exposed, unexposed, est)) %>%
  dplyr::select(age, unexposed_quest, exposed_quest, est_quest, 
                unexposed_blood, exposed_blood, est_blood) %>%
  arrange(age)

write_csv(gdm_sens.tab, file = here("tables", "gdm_sens_test.csv"))

################################################################################
# Table S10: Sex & cohort
################################################################################

## ---- Function to get info ---------------------------------------------------
sensTab <- function(fit, vars, exp_name, type_name){

  eval(as.symbol(fit))$ipd %>%
    map(dh.lmTab, 
        type = "glm_ipd", 
        direction = "wide", 
        ci_format = "paste") %>%
    map(., dplyr::filter, variable %in% vars) %>%
    bind_rows(.id = "age") %>%
    mutate(
      exposure = exp_name, 
      type = type_name)
  
  }

## ---- Extract coefficients ---------------------------------------------------
mat_ed_sex.ref <- tibble(
  vars = rep(c("edu_m2", "edu_m3"), each = 3), 
  exp_name = rep("edu_m", 6), 
  type_name = rep(c("main", "males", "females"), 2), 
  data = rep(c("mat_ed.fit", "mat_ed_m.fit", "mat_ed_f.fit"), 2))
    
area_dep_sex.ref <- tibble(
  vars = rep(c("area_dep2", "area_dep3"), each = 3), 
  exp_name = rep("area_dep", 6), 
  type_name = rep(c("main", "males", "females"), 2), 
  data = rep(c("area_dep.fit", "area_dep_m.fit", "area_dep_f.fit"), 2))

ndvi_sex.ref <- tibble(
  vars = rep("ndvi300_preg_iqr_c", 3), 
  exp_name = rep("ndvi", 3), 
  type_name = c("main", "males", "females"), 
  data = c("ndvi.fit", "ndvi_m.fit", "ndvi_f.fit"))

preg_dia_sex.ref <- tibble(
  vars = rep("preg_dia1", 3), 
  exp_name = rep("preg_dia", 3), 
  type_name = c("main", "males", "females"), 
  data = c("preg_dia.fit", "preg_dia_m.fit", "preg_dia_f.fit"))
                
sens.ref <- bind_rows(mat_ed_sex.ref, area_dep_sex.ref, ndvi_sex.ref, 
                      preg_dia_sex.ref)
  
sens_coef <- sens.ref %>%
  pmap(function(vars, exp_name, type_name, data){
    
    sensTab(
      fit = data, 
      vars = unlist(vars), 
      exp_name = exp_name,
      type_name = type_name)
    
  }) %>% bind_rows()

## ---- Get number of studies and ns -------------------------------------------
sens_k_n <- sens.ref$data %>%
      map(function(x){
        
        eval(as.symbol(x))$ipd %>%
          map(function(y){
            
            tibble(
              k = length(y$disclosure.risk), 
              n = y$Nvalid)
            
          }) 
        
      }) %>% 
  set_names(paste0(sens.ref$exp_name, ".", sens.ref$type_name)) %>%
  map(., ~bind_rows(., .id = "age")) %>%
  bind_rows(.id = "model") %>%
  separate(model, into = c("exposure", "type"), sep = "\\.") 

sens.tab %>%
  print(n = Inf)

## ---- Put together final table -----------------------------------------------
sens.tab <- left_join(sens_coef, sens_k_n, by = c("exposure", "type", "age")) %>%
  unique() %>%
  dplyr::select(exposure, age, variable, est, n, k, type) %>%
  pivot_wider(
    names_from = type, 
    values_from = c(est, k, n)) %>%
  dplyr::select(exposure, age, variable, k_main, n_main, est_main, k_males, 
                n_males, est_males, k_females, n_females, est_females) %>%
  mutate(exposure = factor(
    exposure, levels = c("edu_m", "area_dep", "ndvi", "preg_dia"), 
    ordered = T)) %>%
  arrange(exposure, variable)

write_csv(sens.tab, file = here("tables", "sensitivity.csv"))

################################################################################
# Sex interactions
################################################################################
mat_ed_int.tab <- mat_ed_int.fit$ipd %>% 
  map(dh.lmTab, type = "glm_ipd", direction = "wide", ci_format = "separate") %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable %in% c("edu_m2:sex2", "edu_m3:sex2"))

area_dep_int.tab <- area_dep_int.fit$ipd %>% 
  map(dh.lmTab, type = "glm_ipd", direction = "wide", ci_format = "separate", digits = 2) %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable %in% c("area_dep2:sex2", "area_dep3:sex2"))

ndvi_int.tab <- ndvi_int.fit$ipd %>% 
  map(dh.lmTab, type = "glm_ipd", direction = "wide", ci_format = "separate") %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "ndvi300_preg_iqr_c:sex2")

preg_dia_int.tab <- preg_dia_int.fit$ipd %>% 
  map(dh.lmTab, type = "glm_ipd", direction = "wide", ci_format = "separate") %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "preg_dia1:sex2")

interactions.tab <- list(
  mat_ed_int.tab, area_dep_int.tab, ndvi_int.tab, preg_dia_int.tab) %>%
  bind_rows %>%
  dplyr::select(-se) %>%
  arrange(variable)

write_csv(
  x = interactions.tab,
  file = here("tables", "interactions.csv"))

write_csv(
  x = preg_dia_int.tab,
  file = here("tables", "dia_interactions.csv"))
preg_dia_int.tab

################################################################################
# Ethnicity
################################################################################
eth_sub.mod %>% 
  dplyr::filter(age == "bmi_215" & exposure == "edu_m") %>%
  pull(main_fit)


## ---- Attach fit objects to reference object ---------------------------------
eth_sub.mod <- eth_sub.mod %>%
  mutate(
    main_fit = eth_main.fit,
    eth_fit = eth_eth.fit, 
    exp_var = case_when(
      exposure == "edu_m" ~ list(c("edu_m2", "edu_m3")), 
      exposure == "area_dep" ~ list(c("area_dep2", "area_dep3")), 
      exposure == "ndvi300_preg_iqr_c" ~ list("ndvi300_preg_iqr_c"), 
      exposure == "preg_dia" ~ list("preg_dia1"))
  )

eth_main.tab <- eth_sub.mod %>%
  dplyr::select(age, exposure, exp_var, fit = main_fit, coh, age) %>%
  mutate(adjust = "main")

eth_eth.tab <- eth_sub.mod %>%
  dplyr::select(age, exposure, exp_var, fit = eth_fit, coh, age) %>%
  mutate(adjust = "ethnicity")

## ---- Main model -------------------------------------------------------------
main_coefs <- eth_main.tab %>%
  pmap(function(exp_var, fit, ...){
    
    fit %>%
    dh.lmTab(
      type = "glm_ipd", 
      direction = "wide", 
      ci_format = "paste") %>%
      dplyr::filter(variable %in% unlist(exp_var))
    
  }) %>% set_names(paste0(eth_main.tab$age, ".", eth_main.tab$exposure)) %>%
  bind_rows(.id = "model") %>%
  separate(model, into = c("age", "exposure"), sep = "\\.")

n_k_main <- eth_main.tab %>%
  pmap(function(exposure, coh, fit, age, ...){
    
    tibble(
      age = age,
      exposure = exposure,
      k = length(unlist(coh)), 
      n = fit$Nvalid)
    
  }) %>% bind_rows

eth_main_out.tab <- left_join(main_coefs, n_k_main, by = c("exposure", "age")) %>%
  mutate(adjust = "main")

## ---- Ethnicity adjustment =--------------------------------------------------
eth_coefs <- eth_eth.tab %>%
  pmap(function(exp_var, fit, ...){
    
    fit %>%
      dh.lmTab(
        type = "glm_ipd", 
        direction = "wide", 
        ci_format = "paste") %>%
      dplyr::filter(variable %in% unlist(exp_var))
    
  }) %>% set_names(paste0(eth_eth.tab$age, ".", eth_eth.tab$exposure)) %>%
  bind_rows(.id = "model") %>%
  separate(model, into = c("age", "exposure"), sep = "\\.")

n_k_eth <- eth_eth.tab %>%
  pmap(function(exposure, coh, fit, age, ...){
    
    tibble(
      age = age,
      exposure = exposure,
      k = length(unlist(coh)), 
      n = fit$Nvalid)
    
  }) %>% bind_rows

eth_eth_out.tab <- left_join(eth_coefs, n_k_eth, by = c("exposure", "age")) %>%
  mutate(adjust = "ethnicity")

eth_sens.tab <- bind_rows(eth_main_out.tab, eth_eth_out.tab) %>%
  dplyr::select(exposure, age, variable, k, n, est, adjust) %>%
  pivot_wider(
    names_from = adjust, 
    values_from = c(k, n, est)) %>%
  arrange(exposure, variable) %>%
  dplyr::select(exposure, age, k = k_main, n = n_main, est_main, est_ethnicity)

write_csv(
  x = eth_sens.tab,
  file = here("tables", "ethnicity_sens.csv"))

################################################################################
# Removing DNBC and MoBa  
################################################################################
meta_sens <- function(model, vars){

dh.lmTab(
  model = model, 
  type = "glm_ipd", 
  direction = "wide", 
  ci_format = "separate") %>%
    dplyr::filter(variable %in% vars)
}

## ---- Maternal education -----------------------------------------------------
remove_1 <- mat_ed_remove.fit$ipd %>%
  map(~meta_sens(model = ., vars = c("edu_m2", "edu_m3"))) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "mat_ed", type = "excluded")

all_1 <- mat_ed.fit$ipd %>%
  map(~meta_sens(model = ., vars = c("edu_m2", "edu_m3"))) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "mat_ed", type = "all")

## ---- Area deprivation -------------------------------------------------------
remove_2 <- area_dep_remove.fit$ipd %>%
  map(~meta_sens(model = ., vars = c("area_dep2", "area_dep3"))) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "area_dep", type = "excluded")

all_2 <- area_dep.fit$ipd %>%
  map(~meta_sens(model = ., vars = c("area_dep2", "area_dep3"))) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "area_dep", type = "all")

## ---- NDVI -------------------------------------------------------------------
remove_3 <- ndvi_remove.fit$ipd %>%
  map(~meta_sens(model = ., vars = "ndvi300_preg_iqr_c")) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "ndvi", type = "excluded")

all_3 <- ndvi.fit$ipd %>%
  map(~meta_sens(model = ., vars = "ndvi300_preg_iqr_c")) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "ndvi", type = "all")

## ---- Pregnancy diabetes -----------------------------------------------------
remove_4 <- preg_dia_remove.fit$ipd %>%
  map(~meta_sens(model = ., vars = "preg_dia1")) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "preg_dia", type = "excluded")

all_4 <- preg_dia.fit$ipd %>%
  map(~meta_sens(model = ., vars = "preg_dia1")) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "preg_dia", type = "all")

## ---- Put together -----------------------------------------------------------
excluded.tab <- bind_rows(remove_1, all_1, remove_2, all_2, remove_3, all_3, 
                          remove_4, all_4) %>%
  mutate(est = paste0(est, " (", lowci, ", ", uppci, ")")) %>%
  dplyr::select(variable, age, est, type, n_obs, exposure) %>%
  pivot_wider(
    names_from = type, 
    values_from = c(n_obs, est)) %>%
  dplyr::filter(age != "bmi_215") %>%
  mutate(exposure = factor(
    exposure, 
    levels = c("mat_ed", "area_dep", "ndvi", "preg_dia"), 
    ordered = TRUE)) %>%
  mutate(age = factor(
    age, 
    levels = c("bmi_24", "bmi_48", "bmi_96", "bmi_168"), 
    ordered = TRUE)) %>%
  dplyr::select(exposure, variable, age,  n_obs_all, est_all, n_obs_excluded, 
                est_excluded) %>%
  arrange(exposure, variable, age)

write_csv(excluded.tab, here("tables", "dnbc_moba_exc.csv"))




%>%
  mutate(coef = paste0(est, " (", low_ci))




model = mat_ed_remove.fit$ipd[[1]]
vars = c("edu_m2", "edu_m3")

exc_coh <- c("dnbc", "moba")

## ---- Get estimates ----------------------------------------------------------
ndvi_exclude <- ndvi_slma.plotdata %>%
  mutate(exposure = "ndvi")

mat_ed_exclude <- mat_ed_slma.plotdata %>%
  mutate(exposure = "edu_m") %>%
  mutate(category = ifelse(variable == "Low education (ref = high)", "3", "2"))

exc_sens <- bind_rows(mat_ed_exclude, area_dep_slma.plotdata, ndvi_exclude, 
            preg_dia_slma.plotdata) %>%
  dplyr::select(cohort, age, est = beta, se, lowci = ci_5, uppci = ci_95, 
                exposure, ref, count)

inc_coh.tab <- exc_sens %>% dplyr::filter(!cohort %in% exc_coh & age != "14-17")
exc_coh.tab <- exc_sens %>% dplyr::filter(cohort %in% exc_coh & age != "14-17")

inc_keys <- inc_coh.tab %>%
  group_by(age, exposure) %>%
  group_keys

exc_keys <- exc_coh.tab %>%
  group_by(age, exposure) %>%
  group_keys

inc.tab <- inc_coh.tab %>%
  group_by(age, exposure) %>%
  group_split %>%
  set_names(paste0(inc_keys$age, "z", inc_keys$exposure)) %>%
  map(., metaGroup) %>%
  map(function(x){x[[1]]}) %>%
  bind_rows(.id = "analysis") %>%
  mutate(sens_type = "included")

exc.tab <- exc_coh.tab %>%
  group_by(age, exposure) %>%
  group_split %>%
  set_names(paste0(exc_keys$age, "z", exc_keys$exposure)) %>%
  map(., metaGroup) %>%
  map(function(x){x[[1]]}) %>%
  bind_rows(.id = "analysis") %>%
  mutate(sens_type = "excluded")

## ---- Get Ns -----------------------------------------------------------------
inc_ns <- inc_coh.tab %>%
  mutate(n = ref+count) %>%
  group_by(age, exposure) %>%
  group_split %>%
  set_names(paste0(inc_keys$age, "z", inc_keys$exposure)) %>%
  map(., ~mutate(., test = sum(n))) %>%
  bind_rows(.id = "analysis") %>%
  mutate(sens_type = "included") %>%
  dplyr::select(analysis, test, sens_type) %>%
  unique


  dplyr::select(cohort, )





inc_n <- sens.ref$data %>%
  map(function(x){
    
    eval(as.symbol(x))$ipd %>%
      map(function(y){
        
        tibble(
          k = length(y$disclosure.risk), 
          n = y$Nvalid)
        
      }) 
    
  }) %>% 
  set_names(paste0(sens.ref$exp_name, ".", sens.ref$type_name)) %>%
  map(., ~bind_rows(., .id = "age")) %>%
  bind_rows(.id = "model") %>%
  separate(model, into = c("exposure", "type"), sep = "\\.") 




cohort_sens.tab <- bind_rows(inc.tab, exc.tab) %>%
  separate(analysis, sep = "z", into = c("age", "exposure")) %>%
  mutate(exposure = factor(
    exposure, 
    levels = c("edu_m", "a_d", "ndvi", "p_d"), 
    ordered = TRUE)) %>%
  mutate(age = factor(
    age, 
    levels = c("0-1", "2-3", "4-7", "8-13"), 
    ordered = TRUE)) %>%
  arrange(exposure, age) %>%
  mutate(across(c(est, lowci, uppci), ~round(., 2))) %>%
  mutate(est = paste0(est, " (", lowci, ", ", uppci, ")")) %>%
  dplyr::select 

  
  metaGroup(gdm_quest) %>% 
  bind_rows %>%
  mutate(type = "quest")

gdm_blood.pdata <- metaGroup(gdm_blood) %>% 
  bind_rows %>%
  mutate(type = "blood")


ages <- c("0-1", "2-3", "4-7", "8-13")


gdm_ns <- ns_cat_all %>%
  dplyr::filter(exposure == "p_d" & cohort != "combined" & age != "14-17") %>%
  mutate(type = ifelse(cohort %in% gdm_coh, "blood", "quest")) %>%
  group_by(type, category, age) %>%
  group_split %>%
  map(., ~mutate(., n = sum(count))) %>%
  map(., ~slice(., 1)) %>%
  map(., ~dplyr::select(., age, category, variable, n, type)) %>%
  bind_rows %>%
  dplyr::select(-variable) %>%
  pivot_wider(
    names_from = category, 
    values_from = n) %>%
  dplyr::rename(
    unexposed = '0',
    exposed = '1') %>%
  arrange(desc(type), desc(age)) %>%
  mutate(age = as.character(age))

gdm_sens.tab <- bind_rows(gdm_blood.pdata, gdm_quest.pdata) %>%
  left_join(., gdm_ns, by = c("age", "type")) %>%
  mutate(across(est:uppci, ~round(., 2))) %>%
  mutate(est = paste0(est, " (", lowci, ", ", uppci, ")")) %>%
  dplyr::select(-lowci, -uppci) %>%
  pivot_wider(
    names_from = type,
    values_from = c(exposed, unexposed, est)) %>%
  dplyr::select(age, unexposed_quest, exposed_quest, est_quest, 
                unexposed_blood, exposed_blood, est_blood) %>%
  arrange(age)






## ---- Table of coefficients --------------------------------------------------
mat_ed_remove.tab <- mat_ed_remove.fit[[1]] %>% 
  map(
    dh.lmTab, 
      type = "glm_ipd", 
      direction = "wide", 
    ci_format = "separate") %>%
  bind_rows(.id = "outcome") %>%
  dplyr::filter(variable %in% c("edu_m2", "edu_m3")) 
  
area_dep_remove.tab <- area_dep_remove.fit[[1]] %>%
  map(
    dh.lmTab, 
    type = "glm_ipd", 
    direction = "wide", 
    ci_format = "separate") %>%
  bind_rows(.id = "outcome") %>%
  filter(variable %in% c("area_dep2", "area_dep3")) 

ndvi_remove.tab <- ndvi_remove.fit[[1]] %>%
  map(
    dh.lmTab, 
    type = "glm_ipd", 
    direction = "wide", 
    ci_format = "separate") %>%
  bind_rows(.id = "outcome") %>%
  filter(variable == "ndvi300_preg_iqr_c") 

preg_dia_remove.tab <- preg_dia_remove.fit[[1]] %>%
  map(
    dh.lmTab, 
    type = "glm_ipd", 
    direction = "wide", 
    ci_format = "separate") %>%
  bind_rows(.id = "outcome") %>%
  filter(variable == "preg_dia1") 

ipd_remove.tab <- bind_rows(mat_ed_remove.tab, area_dep_remove.tab, 
                            ndvi_remove.tab, preg_dia_remove.tab)

mat_ed_ipd.plotdata


colnames(ipd_remove.tab) <- c(
  "variable", "age_0_24", "age_25_48", "age_49_96", "age_96_168", "age_169_215")

write.csv(ipd_remove.tab)





################################################################################
# Table 3: Main analysis  
################################################################################
mat_ed.tab <- mat_ed.fit[[1]] %>% 
  map(dh.lmTab, type = "ipd", ci_format = "paste", direction = "wide") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("edu_m2", "edu_m3")) 

area_dep.tab <- area_dep.fit[[1]] %>%
  map(dh.lmTab, type = "ipd", ci_format = "paste", direction = "wide") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("area_dep2", "area_dep3")) 
  
ndvi.tab <- ndvi.fit[[1]] %>%
  map(dh.lmTab, type = "ipd", ci_format = "paste", direction = "wide") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable == "ndvi") 

preg_dia.tab <- preg_dia.fit[[1]] %>%
  map(dh.lmTab, type = "ipd", ci_format = "paste", direction = "wide") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable == "preg_dia1") 

ipd_reg.tab <- bind_rows(mat_ed.tab, area_dep.tab, ndvi.tab, preg_dia.tab)

colnames(ipd_reg.tab) <- c(
  "variable", "age_0_24", "age_25_48", "age_49_96", "age_96_168", "age_169_215")

write.csv(ipd_reg.tab)

# Need sample size for each analysis.
mat_ed_n <- mat_ed.fit[[1]] %>% map_int(function(x){x$Nvalid})
area_dep_n <- area_dep.fit[[1]] %>% map_int(function(x){x$Nvalid})
ndvi_n <- ndvi.fit[[1]] %>% map_int(function(x){x$Nvalid})
preg_dia_n <- preg_dia.fit[[1]] %>% map_int(function(x){x$Nvalid})

ipd_ns.tab <- bind_rows(mat_ed_n, area_dep_n, ndvi_n, preg_dia_n) %>%
  mutate(exposure = c("mat_ed", "area_dep", "ndvi", "preg_dia")) %>%
  select(exposure, everything())

write.csv(ipd_ns.tab)




## ---- Sample sizes -----------------------------------------------------------
mat_ed_remove_n <- mat_ed_remove.fit[[1]] %>% map_int(function(x){x$Nvalid})
area_dep_remove_n <- area_dep_remove.fit[[1]] %>% map_int(function(x){x$Nvalid})
ndvi_remove_n <- ndvi_remove.fit[[1]] %>% map_int(function(x){x$Nvalid})
preg_dia_remove_n <- preg_dia_remove.fit[[1]] %>% map_int(function(x){x$Nvalid})

ipd_remove_ns.tab <- bind_rows(mat_ed_remove_n, area_dep_remove_n, 
                               ndvi_remove_n, preg_dia_remove_n) %>%
  mutate(exposure = c("mat_ed", "area_dep", "ndvi", "preg_dia")) %>%
  select(exposure, everything())

write.csv(ipd_remove_ns.tab)


################################################################################
# PRESENTATIONS
################################################################################

################################################################################
# Prepare dataframe  
################################################################################
cohort_neat <- c("CHOP", "DNBC", "ELFE", "GECKO", "Gen-R", "INMA", "MoBa", 
                 "NFBC86", "NINFEA", "Raine", "SWS", "Combined")

plot_data_cont <- descriptives$continuous %>% 
  mutate(
    cohort = case_when(
      cohort == "chop" ~ "CHOP",
      cohort == "dnbc" ~ "DNBC",
      cohort == "elfe" ~ "ELFE",
      cohort == "gecko" ~ "GECKO",
      cohort == "genr" ~ "Gen-R", 
      cohort == "inma" ~ "INMA", 
      cohort == "moba" ~ "MoBa", 
      cohort == "nfbc86" ~ "NFBC86", 
      cohort == "ninfea" ~ "NINFEA",
      cohort == "raine" ~ "Raine",
      cohort == "sws" ~ "SWS",
      cohort == "combined" ~ "Combined"), 
    cohort = factor(cohort, levels = rev(cohort_neat), ordered = TRUE))

plot_data_cat <- descriptives$categorical %>% 
  mutate(
    cohort = case_when(
      cohort == "chop" ~ "CHOP",
      cohort == "dnbc" ~ "DNBC",
      cohort == "elfe" ~ "ELFE",
      cohort == "gecko" ~ "GECKO",
      cohort == "genr" ~ "Gen-R", 
      cohort == "inma" ~ "INMA", 
      cohort == "moba" ~ "MoBa", 
      cohort == "nfbc86" ~ "NFBC86", 
      cohort == "ninfea" ~ "NINFEA",
      cohort == "raine" ~ "Raine",
      cohort == "sws" ~ "SWS",
      cohort == "combined" ~ "Combined"), 
    cohort = factor(cohort, levels = rev(cohort_neat), ordered = TRUE))

################################################################################
# Sample size visualisation  
################################################################################
sample_data <- plot_data_cont %>%
  filter(variable %in% c("bmi.730", "bmi.1461", "bmi.2922", "bmi.5113", 
                         "bmi.6544", "age_months.24", "age_months.48", "age_months.96", 
                         "age_months.168", "age_months.215")) %>%
  separate(variable, c("var", "age"), sep = "([.])") %>% print(n = 90) %>%
  filter(var == "age_months") 
  
palette_samp <- c("#ff2600", rep("#005690", 11))

sample.plot <- ggplot(data = sample_data, aes(x = perc_50, y = cohort, size = valid_n, colour = cohort)) +
geom_point() + 
  geom_vline(xintercept = 0, linetype=2, size = 0.3) +
  geom_vline(xintercept = 24, linetype=2, size = 0.3) +
  geom_vline(xintercept = 48, linetype=2, size = 0.3) + 
  geom_vline(xintercept = 96, linetype=2, size = 0.3) + 
  geom_vline(xintercept = 168, linetype=2, size = 0.3) +
  geom_vline(xintercept = 215, linetype=2, size = 0.3) +
  xlab("Child age (months)") +
  ylab("Cohort") +
  forest_theme + 
  theme(panel.grid.major.x = element_line(colour="white"),
        panel.grid.minor.x =element_line(colour="white"),
        axis.ticks.x = element_line(colour = "grey"), 
        legend.position = "none", 
        legend.title = element_blank(), 
        legend.justification='left') +
  scale_x_continuous(
    limits = c(0, 215), 
    breaks = c(0, 24, 48, 96, 168, 215), 
    expand = c(0.01, 0)) +
  scale_colour_manual(values = palette_samp, guide = FALSE) 

ggsave(
  filename = "~/wp4-bmi-poc/figures/sample_n_mar21.png", 
  plot = sample.plot, 
  dpi = 1200, 
  h = 12,
  w = 20,
  units = "cm",
  device="png")

################################################################################
# Exposures  
################################################################################
coolors <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51", "#9e7682", 
             "#aba9c3", "#8b2635", "#ec9192", "#553739")

## ---- NDVI -------------------------------------------------------------------
ndvi_plotdata <- plot_data_cont %>%
  filter(variable == "ndvi" & !is.na(mean) & cohort != "Combined")

ndvi.plot <- ndvi_plotdata %>%
  ggplot(aes(x = variable, y = mean, fill = cohort)) +
  geom_bar(stat = "identity") +
  facet_wrap(~cohort, ncol = 3) +
  theme_traj +
  labs(x = "", y = "", title = "") +
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey"), 
        axis.ticks.length = unit(0, "cm"), 
        plot.margin=unit(c(-0.3, 0.5, 0, -0.3),"cm")) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, 0.5),
    expand = c(0, 0)) +
  scale_fill_manual(values = coolors)

ggsave(
  filename = "~/wp4-bmi-poc/figures/ndvi.png", 
  plot = ndvi.plot,
  h = 4, w = 10, units="cm", dpi=1200,
  device="png")


## ---- Area deprivation -------------------------------------------------------
area_plotdata <- plot_data_cat %>%
  filter(variable == "area_dep" & !is.na(valid_perc) & cohort != "Combined")

area.plot <- area_plotdata %>%
  ggplot(aes(x = category, y = valid_perc, fill = cohort)) +
  geom_bar(stat = "identity") +
  facet_wrap(~cohort, ncol = 5) +
  theme_traj +
  xlab("") +
  ylab("") +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(colour="grey"),        
        plot.margin=unit(c(0.3, 0.5, -0.5, -0.3),"cm")) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 20), expand = c(0, 0)) +
  scale_fill_manual(values = coolors)

ggsave(
  filename = "~/wp4-bmi-poc/figures/area.png", 
  plot = area.plot,
  h = 4, w = 10, units="cm", dpi=1200,
  device="png")

## ---- Maternal education -----------------------------------------------------
mated_plotdata <- plot_data_cat %>%
  filter(variable == "edu_m" & !is.na(mean) & cohort != "Combined" & cohort != "SWS")

mated.plot <- mated_plotdata %>%
  ggplot(aes(x = category, y = valid_perc, fill = cohort)) +
  geom_bar(stat = "identity") +
  facet_wrap(~cohort, ncol = 5) +
  theme_traj +
  xlab("") +
  ylab("") +
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(colour="grey"),
        plot.margin=unit(c(0.3, 0.3, 0.3, 0.3),"cm")) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(
    limits = c(0, 80), breaks = seq(0, 80, 20), expand = c(0, 0)) +
  scale_fill_manual(values = coolors)

ggsave(
  filename = "~/wp4-bmi-poc/figures/mated.png", 
  plot = mated.plot,
  h = 9, w = 20, units="cm", dpi=1200,
  device="png")

## ---- Gestational diabetes ---------------------------------------------------
pregdia_plotdata <- plot_data_cat %>%
  filter(variable == "preg_dia" & valid_n > 0 & cohort != "Combined" & category == 1)

pregdia.plot <- pregdia_plotdata %>%
  ggplot(aes(x = variable, y = valid_perc, fill = cohort)) +
  geom_bar(stat = "identity") +
  facet_wrap(~cohort, ncol = 9) +
  theme_traj +
  xlab("") +
  ylab("") +
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey"),
        axis.ticks.length = unit(0, "cm"), 
        plot.margin=unit(c(0.3, 0.5, 0, -0.3),"cm")) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(
    limits = c(0, 16),
    breaks = seq(0, 16, 4),
    expand = c(0, 0)) +
  scale_fill_manual(values = coolors)

ggsave(
  filename = "~/wp4-bmi-poc/figures/pregdia.png", 
  plot = pregdia.plot,
  h = 4, w = 22, units="cm", dpi=1200,
  device="png")

################################################################################
# Forest plots with SLMA and IPD combined  
################################################################################
################################################################################
# Maternal education  
################################################################################
age_orig <- c("bmi_24", "bmi_48", "bmi_96", "bmi_168", "bmi_215")
age_new <- c("0-24 months", "25-48 months", "49-96 months", "97-168 months", 
             "169-215 months")
  
## ---- Prepare data -----------------------------------------------------------
mat_ed_ipd <- mat_ed.fit[[1]] %>%
  map(dh.lmTab, type = "ipd", ci_format = "separate") %>%
  bind_rows(.id = "age") %>%
  filter(variable %in% c("edu_m2", "edu_m3")) %>%
  mutate(
    type = "IPD", 
    cohort = "combined", 
    exposure = "mat_ed") %>%
  dplyr::rename(
    beta = est,
    ci_5 = lower,
    ci_95 = upper)

mat_ed_slma <- mat_ed.pdata %>% mutate(type = "SLMA")

mat_ed_comb <- bind_rows(mat_ed_ipd, mat_ed_slma) %>% select(-se)

mat_ed_comb.pdata <- mat_ed_comb %>% 
  filter(exposure == "mat_ed" & variable %in% c("edu_m2", "edu_m3") & cohort == "combined") %>%
  mutate(  
    variable = case_when(
      variable == "edu_m2" ~ "Medium education (ref = high)", 
      variable == "edu_m3" ~ "Low education (ref = high)"),
    variable = factor(
      variable, 
      levels = c("Medium education (ref = high)", 
                 "Low education (ref = high)"),
      ordered = TRUE), 
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)
  )

beta, lower_ci, upper_ci, estimate_type

## ---- Plot -------------------------------------------------------------------
mat_ed_comb.plot <- ggplot(data = mat_ed_comb.pdata, aes(x = age, y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_point(
    aes(colour = type), size = 2.5, 
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  geom_errorbar(
    aes(colour = type),
      size = 0.5, 
      width = 0.1,
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  xlab('Child age') + 
  ylab("Difference in childhood BMI by category of maternal education") +
  facet_wrap(~variable, scales = "fixed", strip.position = "top") + 
  forest_theme +
  coord_flip() +
  theme(legend.title = element_blank(), 
        legend.position = "none") + 
  scale_y_continuous(
    limits = c(-1, 2),
    breaks = seq(-1, 2, 1),
    expand = c(0, 0)) +
  scale_colour_manual(values = coolors[c(1, 5)])


################################################################################
# NDVI  
################################################################################

## ---- Prepare data -----------------------------------------------------------
ndvi_ipd <- ndvi.fit[[1]] %>%
  map(dh.glmTab, type = "ipd", format = "separate") %>%
  bind_rows(.id = "age") %>%
  filter(variable == "ndvi") %>%
  mutate(
    type = "IPD", 
    cohort = "combined", 
    exposure = "ndvi") %>%
  dplyr::rename(
    beta = est,
    ci_5 = lower,
    ci_95 = upper)

ndvi_slma <- ndvi.pdata %>% mutate(type = "SLMA")

ndvi_comb <- bind_rows(ndvi_ipd, ndvi_slma) %>% select(-se)

ndvi_comb.pdata <- ndvi_comb %>% 
  filter(exposure == "ndvi" & variable == "ndvi" & cohort == "combined") %>%
  mutate(  
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new), 
      ordered = TRUE),
    variable = "NDVI"
  )

## ---- Plot -------------------------------------------------------------------
ndvi_comb.plot <- ggplot(data = ndvi_comb.pdata, aes(x = age, y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_point(
    aes(colour = type), size = 2.5, 
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  geom_errorbar(
    aes(colour = type),
    size = 0.5, 
    width = 0.1,
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  xlab('Child age') + 
  ylab("Difference in childhood BMI by unit change in NDVI") +
  forest_theme +
  coord_flip() +
  theme(legend.title = element_blank(), 
        legend.position = "none") + 
  scale_y_continuous(
    limits = c(-1, 2),
    breaks = seq(-1, 2, 1),
    expand = c(0, 0)) +
  scale_colour_manual(values = coolors[c(1, 5)])


################################################################################
# Area deprivation  
################################################################################

## ---- Prepare data -----------------------------------------------------------
area_ipd <- area_dep.fit[[1]] %>%
  map(dh.glmTab, type = "ipd", format = "separate") %>%
  bind_rows(.id = "age") %>%
  filter(variable %in% c("area_dep2", "area_dep3")) %>%
  mutate(
    type = "IPD", 
    cohort = "combined", 
    exposure = "area_dep") %>%
  dplyr::rename(
    beta = est,
    ci_5 = lower,
    ci_95 = upper)

area_slma <- area_dep.pdata %>% mutate(type = "SLMA")

area_comb <- bind_rows(area_ipd, area_slma) %>% select(-se)

area_comb.pdata <- area_comb %>% 
  filter(exposure == "area_dep" & variable %in% c("area_dep2", "area_dep3") & cohort == "combined") %>%
  mutate(  
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new), 
      ordered = TRUE), 
    variable = case_when(
      variable == "area_dep2" ~ "Medium area deprivation (ref = low)", 
      variable == "area_dep3" ~ "High area deprivation (ref = low)"),
    variable = factor(
      variable, 
      levels = c("Medium area deprivation (ref = low)", 
                 "High area deprivation (ref = low)"),
        ordered = TRUE))

## ---- Plot -------------------------------------------------------------------
area_comb.plot <- ggplot(data = area_comb.pdata, aes(x = age, y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_point(
    aes(colour = type), size = 2.5, 
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  geom_errorbar(
    aes(colour = type),
    size = 0.5, 
    width = 0.1,
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  xlab('Child age') + 
  ylab("Difference in childhood BMI by category of area deprivation") +
  labs(colour = "Type") +
  facet_wrap(~variable, scales = "fixed", strip.position = "top") + 
  forest_theme +
  coord_flip() +
  theme(legend.title = element_blank(),
         legend.position = "none") + 
  scale_y_continuous(
    limits = c(-1, 2),
    breaks = seq(-1, 2, 1),
    expand = c(0, 0)) +
  scale_colour_manual(values = coolors[c(1, 5)])

################################################################################
# Gestational diabetes  
################################################################################

## ---- Prepare data -----------------------------------------------------------
dia_ipd <- preg_dia.fit[[1]] %>%
  map(dh.glmTab, type = "ipd", format = "separate") %>%
  bind_rows(.id = "age") %>%
  filter(variable == "preg_dia1") %>%
  mutate(
    type = "IPD", 
    cohort = "combined", 
    exposure = "preg_dia") %>%
  dplyr::rename(
    beta = est,
    ci_5 = lower,
    ci_95 = upper)

dia_slma <- preg_dia.pdata %>% mutate(type = "SLMA")

dia_comb <- bind_rows(dia_ipd, dia_slma) %>% select(-se)

dia_comb.pdata <- dia_comb %>% 
  filter(exposure == "preg_dia" & variable == "preg_dia1" & cohort == "combined") %>%
  mutate(  
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new), 
      ordered = TRUE), 
    variable = "Presence of gestational diabetes"
  )

## ---- Plot -------------------------------------------------------------------
dia_comb.plot <- ggplot(data = dia_comb.pdata, aes(x = age, y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_point(
    aes(colour = type), size = 2.5, 
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  geom_errorbar(
    aes(colour = type),
    size = 0.5, 
    width = 0.1,
    position = position_jitterdodge(dodge.width = -0.5, jitter.width = 0)) +
  xlab('Child age') + 
  ylab("Difference in chilhood BMI where gestational diabetes present") +
  forest_theme +
  coord_flip() +
  theme(legend.title = element_blank(),
        legend.position = "none") + 
  scale_y_continuous(
    limits = c(-1, 2),
    breaks = seq(-1, 2, 1),
    expand = c(0, 0)) +
  scale_colour_manual(values = coolors[c(1, 5)])

################################################################################
# Save plots  
################################################################################
ggsave(
  filename = "~/wp4-bmi-poc/figures/mat_ed_comb.png", 
  plot = mat_ed_comb.plot,
  h = 10, w = 16, units="cm", dpi=1200,
  device="png")

ggsave(
  filename = "~/wp4-bmi-poc/figures/ndvi_comb.png", 
  plot = ndvi_comb.plot,
  h = 10, w = 16, units="cm", dpi=1200,
  device="png")

ggsave(
  filename = "~/wp4-bmi-poc/figures/area_comb.png", 
  plot = area_comb.plot,
  h = 10, w = 16, units="cm", dpi=1200,
  device="png")

ggsave(
  filename = "~/wp4-bmi-poc/figures/dia_comb.png", 
  plot = dia_comb.plot,
  h = 10, w = 16, units="cm", dpi=1200,
  device="png")

################################################################################
# AJE REVIEWER ROUND 1  
################################################################################
################################################################################
# Number of observations per subject  
################################################################################
n_obs_per_child <- n_obs.stats$categorical %>%
  dplyr::filter(cohort == "combined") %>%
  dplyr::select(variable, category, value) %>%
  dplyr::filter(!is.na(category) & category != 0) %>%
  mutate(variable = factor(
    variable,
    levels = c("edu_m_n", "a_d_n", "n_d_n", "p_d_n"), 
    ordered = TRUE)) %>%
  arrange(variable)

write_csv(n_obs_per_child, file = here("tables", "n_obs_child.csv"))

# Need to check Ns, but I think NAs are where age band didn't exist in study, 
# and 0s are where the age band existed but it wasn't a complete case

################################################################################
# Compare all available observations vs sample at oldest age  
################################################################################

## ---- Coefficients -----------------------------------------------------------
sel_1 <- mat_ed_sel.fit %>%
  map(~meta_sens(model = ., vars = c("edu_m2", "edu_m3"))) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "mat_ed", type = "oldest_only")

sel_2 <- area_dep_sel.fit %>%
  map(~meta_sens(model = ., vars = c("area_dep2", "area_dep3"))) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "area_dep", type = "oldest_only")

sel_3 <- ndvi_sel.fit %>%
  map(~meta_sens(model = ., vars = "ndvi300_preg_iqr_c")) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "ndvi", type = "oldest_only")

sel_4 <- preg_dia_sel.fit %>%
  map(~meta_sens(model = ., vars = "preg_dia1")) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "preg_dia", type = "oldest_only")


## ---- N included cohorts -----------------------------------------------------
ipdN <- function(fit, exp_name){

fit %>%
  map(function(x){
    tibble(
      cohort_n = length(dimnames(x$disclosure.risk)[[1]]))
      }) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = exp_name)
  
}
    

## Full
mat_ed_n <- mat_ed.fit$ipd %>% selN("mat_ed")
area_dep_n <- area_dep.fit$ipd %>% selN("area_dep")
ndvi_n <- ndvi.fit$ipd %>% selN("ndvi")
preg_dia_n <- preg_dia.fit$ipd %>% selN("preg_dia")

all_ns <- bind_rows(mat_ed_n, area_dep_n, ndvi_n, preg_dia_n) %>%
  mutate(type = "all")

## Selection
mat_ed_sel_n <- mat_ed_sel.fit %>% selN("mat_ed")
area_dep_sel_n <- area_dep_sel.fit %>% selN("area_dep")
ndvi_sel_n <- ndvi_sel.fit %>% selN("ndvi")
preg_dia_sel_n <- preg_dia_sel.fit %>% selN("preg_dia")

sel_ns <- bind_rows(mat_ed_sel_n, area_dep_sel_n, ndvi_sel_n, preg_dia_sel_n) %>%
  mutate(type = "oldest_only")

sel_sens_ns <- bind_rows(all_ns, sel_ns)

selection.tab <- bind_rows(sel_1, all_1, sel_2, all_2, sel_3, all_3, 
                          sel_4, all_4) %>%
  left_join(., sel_sens_ns, by = c("age", "exposure", "type")) %>%
  mutate(est = paste0(est, " (", lowci, ", ", uppci, ")")) %>%
  dplyr::select(variable, age, est, type, n_obs, cohort_n, exposure) %>%
  pivot_wider(
    names_from = type, 
    values_from = c(n_obs, est, cohort_n)) %>%
  dplyr::filter(age != "bmi_215") %>%
  mutate(exposure = factor(
    exposure, 
    levels = c("mat_ed", "area_dep", "ndvi", "preg_dia"), 
    ordered = TRUE)) %>%
  mutate(age = factor(
    age, 
    levels = c("bmi_24", "bmi_48", "bmi_96", "bmi_168"), 
    ordered = TRUE)) %>%
  dplyr::select(exposure, variable, age,  cohort_n_all, n_obs_all, est_all, 
                cohort_n_oldest_only, n_obs_oldest_only, est_oldest_only) %>%
  arrange(exposure, variable, age)

write_csv(selection.tab, file = here("tables", "selection_bias.csv"))

ds.dim()
