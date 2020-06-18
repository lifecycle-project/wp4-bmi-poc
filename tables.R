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


################################################################################
# 1. Descriptive statistics
################################################################################

## ---- Exposures --------------------------------------------------------------
exposure_cat <- descriptives_ss$categorical %>%
  filter(variable == "edu_m" | variable == "preg_dia" | 
           variable == "greenyn300") %>%
  mutate(n_perc = paste0(value, " (", valid_perc, ")")) %>%
  select(cohort, variable, category, n_perc) %>%
  pivot_wider(names_from = c(variable, category), values_from = n_perc) 

exposure_cont <- descriptives_ss$continuous %>%
  filter(variable == "ga_all" | variable == "green_dist") %>%
  mutate(mean_sd = paste0(mean, " (", std.dev, ")")) %>%
  select(cohort, variable, mean_sd) %>%
  pivot_wider(names_from = variable, values_from = mean_sd)

exposure.tab <- cbind(exposure_cat, select(exposure_cont, -cohort)) %>%
  select(cohort, greenyn300_0, greenyn300_1, edu_m_1, edu_m_2, edu_m_3, 
         ga_all, preg_dia_0, preg_dia_1)

write.csv(exposure.tab)


## ---- Outcomes ---------------------------------------------------------------
outcomes.tab <- descriptives_ss$continuous %>%
  filter(variable == "bmi.24" | variable == "bmi.48" | variable == "bmi.96" | 
           variable == "bmi.168" | variable == "age_months.24" | 
           variable == "age_months.48" | variable == "age_months.96" |
           variable == "age_months.168") %>%
  mutate(mean_sd = paste0(mean, " (", std.dev, ")")) %>%
  select(cohort, variable, mean_sd, valid_n) %>%
  pivot_wider(names_from = variable, values_from = c(mean_sd, valid_n)) %>%
  select(cohort, valid_n_bmi.24, mean_sd_age_months.24, mean_sd_bmi.24, 
         valid_n_bmi.48, mean_sd_age_months.48, mean_sd_bmi.48,
         valid_n_bmi.96, mean_sd_age_months.96, mean_sd_bmi.96,
         valid_n_bmi.168, mean_sd_age_months.168, mean_sd_bmi.168)

write.csv(outcomes.tab)


## ---- Covariates -------------------------------------------------------------
cov_cat.tab <- descriptives_ss$categorical %>%
  filter(variable == "sex" | variable == "parity_bin" | variable == "preg_smk" | 
           variable == "preg_ht" | variable == "ethn3_m_rev") %>%
  mutate(n_perc = paste0(value, " (", valid_perc, ")"), 
         missing = paste0(missing_n, " (", missing_perc, ")")) %>%
  select(cohort, variable, category, n_perc, missing) %<>% 
  pivot_wider(names_from = c(variable, category),  
              values_from = c(n_perc, missing))

cov_cont.tab <- descriptives_ss$continuous %>%
  filter(variable == "prepreg_bmi" | variable == "agebirth_m_y") %>%
  mutate(mean_sd = paste0(mean, " (", std.dev, ")"),
         missing = paste0(missing_n, " (", missing_perc, ")")) %>%
  select(cohort, variable, mean_sd, missing) %>%
  pivot_wider(names_from = variable, values_from = c(mean_sd, missing))

cov.tab <- cbind(cov_cat.tab, select(cov_cont.tab, -cohort)) %>%
  select(cohort, n_perc_sex_1, missing_sex_1, n_perc_parity_bin_0,
         missing_parity_bin_0, n_perc_ethn3_m_rev_1, n_perc_ethn3_m_rev_2,
         n_perc_ethn3_m_rev_3, missing_ethn3_m_rev_1, n_perc_preg_smk_1,
         missing_preg_smk_1, mean_sd_prepreg_bmi, missing_prepreg_bmi,
         mean_sd_agebirth_m_y, missing_agebirth_m_y, n_perc_preg_ht_1, 
         missing_preg_ht_1) %>%
  as_tibble()

write.csv(cov.tab)


## ---- Available n by cohort --------------------------------------------------
ds.summary("analysis_df", datasources = coh())


################################################################################
# 2. Analysis stats
################################################################################

## ---- Write function ---------------------------------------------------------

# This extracts the regression results for each outcome and puts them into 
# lists for each exposure

regTab <- function(x){
  
  out <- data.frame(
    var = dimnames(x$SLMA.pooled.ests.matrix)[[1]],
    est = round(x$SLMA.pooled.ests.matrix[, "pooled.ML"], 2),
    lower = round(
      x$SLMA.pooled.ests.matrix[, "pooled.ML"] - 1.96 * 
        x$SLMA.pooled.ests.matrix[, "se.ML"], 2),
    upper = round(x$SLMA.pooled.ests.matrix[, "pooled.ML"] + 1.96 * 
                    x$SLMA.pooled.ests.matrix[, "se.ML"], 2)
  )
  
  out$est <- paste0(out$est, " (", out$lower, ", ", out$upper, ")")
  out$lower <- NULL
  out$upper <- NULL
  
  rownames(out) <- NULL
  return(out)
  
}


## ---- Now apply function to model results ------------------------------------

## Maternal education
mat_ed.tab <- lapply(mat_ed.fit, regTab)
write.csv(mat_ed.tab)


## Gestational age at birth
ga.tab <- lapply(ga.fit, regTab)
write.csv(ga.tab)


## Gestational diabetes
gest_dia.tab <- lapply(gest_dia.fit, regTab)
write.csv(gest_dia.tab)
