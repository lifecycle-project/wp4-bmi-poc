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

library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

conns <- datashield.login(logindata, restore = "bmi_poc_sec_12")

################################################################################
# Data prep  
################################################################################

## Make tibble of cohort names and sample sizes as I want to display in paper
ref_tab <- cohort_ns %>%
  mutate(
    cohort_neat = case_when(
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
    names_neat = paste0(cohort_neat, " (n=", cohort_n, ")")
  )

################################################################################
# METHODS  
################################################################################
################################################################################
# List participating cohorts  
################################################################################
names(conns) %>% sort()

################################################################################
# Maximum sample size
################################################################################

## ---- Analysis dataset -------------------------------------------------------
cohort_ns <- descriptives_ss[[1]] %>%
  filter(variable == "sex" & category == 1) %>%
  select(cohort, cohort_n) %>%
  arrange(cohort) 

## ---- Original dataset -------------------------------------------------------
original_n <- dh.getStats(
  df = "nonrep", 
  vars = "sex",
  conns = conns
)

original_n[[1]] %>%
  filter(variable == "sex" & category == 1) %>%
  select(cohort, cohort_n) %>%
  arrange(cohort) 


################################################################################
# RESULTS  
################################################################################
################################################################################
# Table S4: Covariate descriptive statistics  
################################################################################
cov_cat.tab <- descriptives$categorical %>%
  filter(variable %in% c("sex", "parity_bin", "preg_smk", "preg_ht", 
                         "ethn3_m")) %>%
  mutate(n_perc = paste0(value, " (", valid_perc, ")"), 
         missing = paste0(missing_n, " (", missing_perc, ")")) %>%
  select(cohort, variable, category, n_perc, missing) %>% 
  pivot_wider(names_from = c(variable, category),  
              values_from = c(n_perc, missing))

cov_cont.tab <- descriptives$continuous %>%
  filter(variable %in% c("prepreg_bmi", "agebirth_m_y")) %>%
  mutate(med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")"),
         missing = paste0(missing_n, " (", missing_perc, ")")) %>%
  select(cohort, variable, med_range, missing) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, missing))

cov.tab <- bind_cols(cov_cat.tab, select(cov_cont.tab, -cohort)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  select(names_neat, n_perc_sex_1, missing_sex_1, n_perc_parity_bin_0,
         missing_parity_bin_0, n_perc_ethn3_m_1, n_perc_ethn3_m_2,
         n_perc_ethn3_m_3, missing_ethn3_m_1, n_perc_preg_smk_1, 
         missing_preg_smk_1, med_range_prepreg_bmi, missing_prepreg_bmi, 
         med_range_agebirth_m_y, missing_agebirth_m_y, n_perc_preg_ht_1, 
         missing_preg_ht_1) %>%
  arrange(names_neat) 

write.csv(cov.tab)

################################################################################
# Table 1: Exposures descriptive statistics  
################################################################################
exposure_cat <- descriptives$categorical %>%
  filter(variable %in% c("edu_m", "preg_dia", "area_dep")) %>%
  mutate(n_perc = paste0(value, " (", valid_perc, ")")) %>%
  select(cohort, variable, category, n_perc) %>%
  pivot_wider(names_from = c(variable, category), values_from = n_perc) 

exposure_cont <- descriptives$continuous %>%
  filter(variable == "ndvi") %>%
  mutate(med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")")) %>%
  select(cohort, variable, med_range) %>%
  pivot_wider(names_from = variable, values_from = med_range)

exposure.tab <- bind_cols(exposure_cat, select(exposure_cont, -cohort)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  select(names_neat, edu_m_1, edu_m_2, edu_m_3, area_dep_1, area_dep_2, 
         area_dep_3, ndvi, preg_dia_1) %>%
  arrange(names_neat) 

write.csv(exposure.tab)


################################################################################
# Table 2: Outcomes descriptive statistics  
################################################################################
outcomes.tab <- descriptives$continuous %>%
  filter(variable %in% c("bmi.730", "bmi.1461", "bmi.2922", "bmi.5113", 
                         "bmi.6544", "age_months.24", "age_months.48", 
                         "age_months.96", "age_months.168", "age_months.215")) %>%
  mutate(med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")")) %>%
  select(cohort, variable, med_range, valid_n) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, valid_n)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  select(names_neat, valid_n_bmi.730, med_range_age_months.24, med_range_bmi.730, 
         valid_n_bmi.1461, med_range_age_months.48, med_range_bmi.1461,
         valid_n_bmi.2922, med_range_age_months.96, med_range_bmi.2922,
         valid_n_bmi.5113, med_range_age_months.168, med_range_bmi.5113, 
         valid_n_bmi.6544, med_range_age_months.215, med_range_bmi.6544) %>%
  arrange(names_neat)

write.csv(outcomes.tab)

## ---- Available n by cohort --------------------------------------------------
ds.summary("analysis_df", datasources = coh())



################################################################################
# Table S5 height and weight by age to check BMI validity  
################################################################################
ht.tab <- descriptives$continuous %>%
  filter(variable %in% c("ht.730", "ht.1461", "ht.2922", "ht.5113", "ht.6544")) %>%
  mutate(med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")")) %>%
  select(cohort, variable, med_range, valid_n) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, valid_n)) %>%
  left_join(., ref_tab, by = "cohort") %>%
select(names_neat, valid_n_ht.730, med_range_ht.730, valid_n_ht.1461, 
       med_range_ht.1461, valid_n_ht.2922, med_range_ht.2922, valid_n_ht.5113, 
       med_range_ht.5113, valid_n_ht.6544, med_range_ht.6544) %>%
  arrange(names_neat) 

wt.tab <- descriptives$continuous %>%
  filter(variable %in% c("wt.730", "wt.1461", "wt.2922", "wt.5113", "wt.6544")) %>%
  mutate(med_range = paste0(perc_50, " (", perc_5, ", ", perc_95, ")")) %>%
  select(cohort, variable, med_range, valid_n) %>%
  pivot_wider(names_from = variable, values_from = c(med_range, valid_n)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  select(names_neat, valid_n_wt.730, med_range_wt.730, valid_n_wt.1461, 
         med_range_wt.1461, valid_n_wt.2922, med_range_wt.2922, valid_n_wt.5113, 
         med_range_wt.5113, valid_n_wt.6544, med_range_wt.6544) %>%
  arrange(names_neat) 
  
  
write.csv(ht.tab)
write.csv(wt.tab)


################################################################################
# Table 3: Main analysis  
################################################################################
mat_ed.tab <- mat_ed.fit[[1]] %>% 
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("edu_m2", "edu_m3")) 

area_dep.tab <- area_dep.fit[[1]] %>%
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("area_dep2", "area_dep3")) 
  
ndvi.tab <- ndvi.fit[[1]] %>%
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable == "ndvi") 

preg_dia.tab <- preg_dia.fit[[1]] %>%
  map(dh.glmTab, type = "ipd") %>%
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

################################################################################
# PLOTS  
################################################################################
################################################################################
# Prepare data for plotting
################################################################################

## ---- Function to create dataset from model objects --------------------------
dh.forestData <- function(obj, mod){
  
## By study
  betas <- as_tibble(t(obj$betamatrix.valid)) %>%
    mutate(cohort = mod$cohort) %>%
    pivot_longer(
      names_to = "variable", 
      values_to = "beta",
      cols = -cohort)
  
  ses <- as_tibble(t(obj$sematrix.valid)) %>%
    mutate(cohort = mod$cohort) %>%
    pivot_longer(
      names_to = "variable", 
      values_to = "se",
      cols = -cohort)
  
## Combined
  beta_comb <- as_tibble(t(obj$SLMA.pooled.ests.matrix[, "pooled.ML"])) %>%
    mutate(cohort = "combined") %>%
    pivot_longer(
      names_to = "variable", 
      values_to = "beta",
      cols = -cohort)
  
  se_comb <- as_tibble(t(obj$SLMA.pooled.ests.matrix[, "se.ML"])) %>%
    mutate(cohort = "combined") %>%
    pivot_longer(
      names_to = "variable", 
      values_to = "se",
      cols = -cohort)
  
  out_sep <- left_join(betas, ses, by = c("cohort", "variable"))
  out_comb <- left_join(beta_comb, se_comb, by = c("cohort", "variable"))
  
  out <- bind_rows(out_sep, out_comb) %>%
    mutate(
      ci_5 = beta - 1.96*se,
      ci_95 = beta + 1.96*se)
  
  return(out)
  
}

## ---- Create plot data -------------------------------------------------------
mat_ed.pdata <- list(mat_ed.fit[[2]], mat_ed_nfbc.mod) %>% 
  pmap(dh.forestData) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "mat_ed")

area_dep.pdata <- list(area_dep.fit[[2]], area_dep.mod) %>% 
  pmap(dh.forestData) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "area_dep")
  
ndvi.pdata <- list(ndvi.fit[[2]], ndvi.mod) %>% 
  pmap(dh.forestData) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "ndvi")

preg_dia.pdata <- list(preg_dia.fit[[2]], preg_dia.mod) %>% 
  pmap(dh.forestData) %>%
  bind_rows(.id = "age") %>%
  mutate(exposure = "preg_dia")
  
slma.pdata <- bind_rows(mat_ed.pdata, area_dep.pdata, ndvi.pdata, preg_dia.pdata) %>%
  left_join(., ref_tab, by = "cohort") %>%
  select(-cohort) %>%
  mutate(
    age = factor(age, levels = c("bmi_24", "bmi_48", "bmi_96", "bmi_168", "bmi_215"),
                 labels = c("Age 0-24", "Age 25-48", "Age 49-96", "Age 97-168", 
                            "Age 169-215")),
    Cohort = factor(cohort_neat, levels = rev(unique(cohort_neat)), ordered = TRUE))


################################################################################
# Figure S1: maternal education SLMA
################################################################################
source('~/useful-code-r/code/themes/forest-theme.R')
palette_n <- c("#ff2600", rep("#005690", 10))

## ---- Prepare data -----------------------------------------------------------
mat_ed.pdata <- slma.pdata %>% 
  filter(exposure == "mat_ed" & variable %in% c("edu_m2", "edu_m3")) %>%
  mutate(  
    variable = case_when(
      variable == "edu_m2" ~ "Medium education (ref = high)", 
      variable == "edu_m3" ~ "Low education (ref = high)"),
    variable = factor(
      variable, 
      levels = c("Medium education (ref = high)", 
                 "Low education (ref = high)"),
      ordered = TRUE)
  )

## ---- Plot -------------------------------------------------------------------
mat_ed.plot <- ggplot(data = mat_ed.pdata, aes(x = Cohort,y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_pointrange(aes(colour = Cohort), size = 0.15) +
  geom_hline(aes(fill=Cohort),yintercept =0, linetype=2) + 
  xlab('Cohort')+ 
  ylab("Difference in childhood BMI by category of maternal education") +
  facet_grid(age ~ variable, scales = "fixed", switch = "y") + 
  forest_theme +
  coord_flip() +
  scale_colour_manual(values = palette_n) +
  theme(
    axis.text.y = element_text(
      family="ArialMT", 
      size=6, 
      margin = margin(t = 0, r=4, b=0, l=0), 
      colour="black")
  )

## ---- Save plot --------------------------------------------------------------
ggsave(
  filename="./figures/mat_ed.png", 
  plot = mat_ed.plot,
  h = 25, w = 15.92, units="cm", dpi=1200,
  device="png")


################################################################################
# Figure S2: Area deprivation SLMA  
################################################################################

## ---- Prepare data -----------------------------------------------------------
area_dep.pdata <- slma.pdata %>% 
  filter(exposure == "area_dep" & variable %in% c("area_dep2", "area_dep3")) %>%
  mutate(
    variable = factor(variable, levels = c("area_dep2", "area_dep3"),
                      labels = c("Medium deprivation (ref = low)", 
                                 "High deprivation (ref = low)")))

## ---- Plot -------------------------------------------------------------------
area_dep.plot <- ggplot(data = area_dep.pdata,
                    aes(x = Cohort,y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_pointrange(aes(colour = Cohort), size = 0.3) +
  geom_hline(aes(fill=Cohort),yintercept =0, linetype=2) + 
  xlab('Cohort')+ 
  ylab("Difference in childhood BMI by category of area deprivation") +
  facet_grid(age ~ variable, scales = "fixed", switch = "y") + 
  forest_theme +
  coord_flip() +
  ylim(-2, 2) +
  scale_colour_manual(values = palette_n)

## ---- Save plot --------------------------------------------------------------
ggsave(
  filename="./figures/area_dep.png", 
  plot = area_dep.plot, 
  h = 12, w = 15.92, units="cm", dpi=1200,
  device="png")


################################################################################
# Figure S3: NDVI  
################################################################################

## ---- Prepare data -----------------------------------------------------------
ndvi.pdata <- slma.pdata %>% filter(exposure == "ndvi" & variable == "ndvi") 

## ---- Plot -------------------------------------------------------------------
ndvi.plot <- ggplot(data = ndvi.pdata,
       aes(x = Cohort,y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_pointrange(aes(colour = Cohort), size = 0.3) +
  geom_hline(aes(fill=Cohort),yintercept =0, linetype=2) + 
  xlab('Cohort')+ 
  ylab("Difference in childhood BMI by unit change in NDVI") +
  facet_wrap(~age, strip.position = "left", nrow = 4) + 
  forest_theme +
  coord_flip() +
  ylim(-6, 4) +
  scale_colour_manual(values = palette_n)

## ---- Save plot --------------------------------------------------------------
ggsave(
  filename="./figures/ndvi.png", 
  plot = ndvi.plot, 
  h = 12, w = 15.92, units="cm", dpi=1200,
  device="png")


################################################################################
# Figure S4: Gestational diabetes  
################################################################################

## ---- Prepare data -----------------------------------------------------------
preg_dia.pdata <- slma.pdata %>% 
  filter(variable == "preg_dia1" & exposure == "preg_dia")
  
## ---- Plot -------------------------------------------------------------------
preg_dia.plot <- ggplot(data = preg_dia.pdata,
       aes(x = Cohort,y = beta, ymin = ci_5, ymax = ci_95)) +
  geom_pointrange(aes(colour = Cohort), size = 0.2) +
  geom_hline(aes(fill=Cohort),yintercept =0, linetype=2) + 
  xlab('Cohort')+ 
  ylab("Difference in chilhood BMI where gestational diabetes present") +
  facet_wrap(~age, strip.position = "left", nrow = 4) + 
  forest_theme +
  coord_flip() +
  ylim(-1, 2) +
  scale_colour_manual(values = palette_n)

## ---- Save plot --------------------------------------------------------------
ggsave(
  filename="./figures/preg_dia.png", 
  plot = preg_dia.plot, 
  h = 20, w = 15.92, units="cm", dpi=1200,
  device="png")

################################################################################
# SENSITIVITY ANALYSES
################################################################################
################################################################################
# Sex interactions
################################################################################
mat_ed_int.tab <- mat_ed_int.fit[[1]] %>% 
  map(dh.glmTab, type = "ipd")

area_dep_int.tab <- area_dep_int.fit[[1]] %>%
  map(dh.glmTab, type = "ipd")

ndvi_int.tab <- ndvi_int.fit[[1]] %>%
  map(dh.glmTab, type = "ipd")

preg_dia_int.tab <- preg_dia_int.fit[[1]] %>%
  map(dh.glmTab, type = "ipd")

################################################################################
# Removeing DNBC and MoBa  
################################################################################

## ---- Table of coefficients --------------------------------------------------
mat_ed_remove.tab <- mat_ed_remove.fit[[1]] %>% 
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("edu_m2", "edu_m3")) 

area_dep_remove.tab <- area_dep_remove.fit[[1]] %>%
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable %in% c("area_dep2", "area_dep3")) 

ndvi_remove.tab <- ndvi_remove.fit[[1]] %>%
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable == "ndvi") 

preg_dia_remove.tab <- preg_dia_remove.fit[[1]] %>%
  map(dh.glmTab, type = "ipd") %>%
  reduce(left_join, by = "variable") %>%
  filter(variable == "preg_dia1") 

ipd_remove.tab <- bind_rows(mat_ed_remove.tab, area_dep_remove.tab, 
                            ndvi_remove.tab, preg_dia_remove.tab)

colnames(ipd_remove.tab) <- c(
  "variable", "age_0_24", "age_25_48", "age_49_96", "age_96_168", "age_169_215")

write.csv(ipd_remove.tab)


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
# OLD ANALYSES
################################################################################
################################################################################
# October 2020 GA sample size visualisation  
################################################################################
cohort_neat <- c("CHOP", "DNBC", "GECKO", "Gen-R", "INMA", "MoBa", "NINFEA", 
                 "Raine", "Combined")

test <- descriptives_ss$continuous %>%
  filter(variable %in% c("bmi.730", "bmi.1461", "bmi.2922", "bmi.5113", 
                         "bmi.6544", "age_months.24", "age_months.48", "age_months.96", 
                         "age_months.168", "age_months.215")) 
  
testy <- test %>% 
  separate(variable, c("var", "age"), sep = "([.])") %>% print(n = 90) %>%
  filter(var == "age_months") %>%
  mutate(
    cohort = case_when(
      cohort == "chop" ~ "CHOP",
      cohort == "dnbc" ~ "DNBC",
      cohort == "gecko" ~ "GECKO",
      cohort == "genr" ~ "Gen-R", 
      cohort == "inma" ~ "INMA", 
      cohort == "moba" ~ "MoBa", 
      cohort == "ninfea" ~ "NINFEA",
      cohort == "raine" ~ "Raine",
      cohort == "Combined" ~ "Combined"), 
    cohort = factor(cohort, levels = rev(cohort_neat), ordered = TRUE))

sample.plot <- ggplot(data = testy, aes(x = perc_50, y = cohort, size = valid_n, colour = cohort)) +
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
        legend.position = "top") +
  scale_x_continuous(
    limits = c(0, 215), 
    breaks = c(0, 24, 48, 96, 168, 215), 
    expand = c(0.01, 0)) +
  scale_colour_manual(values = palette_n)

ggsave(
  filename="./figures/sample_size.png", 
  plot = sample.plot, 
  dpi=300, 
  device="png")