################################################################################
## Project: bmi-poc
## Script purpose: Figures for manuscript
## Date: 1st July 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(rlang)
library(tidyr)
library(ggplot2)
library(metafor)
library(gridExtra)
library(here)

source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/lc-names-neat.R")
source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/themes.R")

################################################################################
# Figure S1: missingness models
################################################################################
################################################################################
# Helper functions 
################################################################################

## ---- Function to do prepare data --------------------------------------------
formatMissPlot <- function(x){
  
  x %>%
    map(
      dh.lmTab, 
      type = "glm_ipd", 
      ci_format = "separate", 
      direction = "wide", 
      digits = 50) %>%
    bind_rows(.id = "age") %>%
    dplyr::filter(str_detect(variable, "zscores\\.")) %>%
    mutate(across(est:uppci, ~exp(.x))) %>%
    mutate(
      age = 
        factor(
          case_when(
            age == "bmi_24" ~ "0 - 1", 
            age == "bmi_48" ~ "2 - 3", 
            age == "bmi_96" ~ "4 - 7", 
            age == "bmi_168" ~ "8 - 13", 
            age == "bmi_215" ~ "14 - 17"), 
          levels = c("0 - 1", "2 - 3", "4 - 7", "8 - 13", "14 - 17"), 
          ordered = TRUE))
}

## ---- Wrapper function for forest plot ---------------------------------------
forestWrap <- function(
  x, title, x_limits, axis_limits, type = c("logistic", "linear"), digits = 3){
 
  ref_value <- ifelse(type == "logistic", 1, 0)
  
  forest(
    x = x %>% pull(est), 
    ci.lb = x %>% pull(lowci),
    ci.ub = x %>% pull(uppci), 
    slab = x %>% pull(age), 
    xlab = title, 
    cex = 0.8, 
    cex.axis = 0.8,
    header = c("BMI Age period", "Estimate [95% CI]"), 
    refline = ref_value, 
    xlim = x_limits,
    alim = axis_limits, 
    steps = 5, 
    digits = c(digits, 2), 
    psize = 1)
}

################################################################################
# Plots  
################################################################################
word_land_half <- 13.34

## ---- Maternal education -----------------------------------------------------
png(
  file = here("figures", "mat_ed_miss_forest.png"), 
  width = word_land_half, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,4,0,2))

mat_ed_miss.fit %>% 
  formatMissPlot %>%
  forestWrap(
    title = "Odds ratio for being complete cases per unit change in BMI z-score",
    x_limits = c(0.6, 1.4), 
    axis_limits = c(0.8, 1.2), 
    type = "logistic")

dev.off()

## ---- Area deprivation -------------------------------------------------------
png(
  file = here("figures", "area_dep_miss_forest.png"), 
  width = word_land_half, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,4,0,2))

area_dep_miss.fit %>%
  formatMissPlot %>%
  forestWrap(
    title = "Odds ratio for being complete cases per unit change in BMI z-score",
    x_limits = c(0.6, 1.4), 
    axis_limits = c(0.8, 1.2), 
    type = "logistic")

dev.off()

## ---- NDVI -------------------------------------------------------------------
png(
  file = here("figures", "ndvi_miss_forest.png"), 
  width = word_land_half, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,4,0,2))

ndvi_miss.fit %>%
  formatMissPlot %>%
  forestWrap(
    title = "Odds ratio for being complete cases per unit change in BMI z-score",
    x_limits = c(0.6, 1.4), 
    axis_limits = c(0.8, 1.2), 
    type = "logistic")

dev.off()

## ---- Pregnancy Diabetes -----------------------------------------------------
png(
  file = here("figures", "preg_dia_miss_forest.png"), 
  width = word_land_half, 
  height = 8, 
  units = "cm",
  res = 300)

par(mar=c(5,4,0,2))

preg_dia_miss.fit %>%
  formatMissPlot %>%
  forestWrap(
    title = "Odds ratio for being complete cases per unit change in BMI z-score",
    x_limits = c(0.6, 1.4), 
    axis_limits = c(0.8, 1.2), 
    type = "logistic")

dev.off()

################################################################################
# Figure 1: Exposures descriptive statistics  
################################################################################

## ---- Maternal education -----------------------------------------------------
edu_desc.plot <- descriptives$categorical %>%
  dplyr::filter(variable == "edu_m" & !cohort == "combined") %>%
  mutate(
    category = 
      factor(
        case_when(
          category == 1 ~ "High", 
          category == 2 ~ "Medium", 
          category == 3 ~ "Low", 
          is.na(category) ~ "Missing"), 
        levels = c("Low", "Medium", "High", "Missing"), 
        ordered = TRUE)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  mutate(perc_total = ifelse(perc_total == 100, 0, perc_total)) %>%
  ggplot(aes(x = cohort_neat, y = perc_total, fill = category)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Percentage") +
  coord_cartesian(
    ylim = c(0, 100), 
    expand = FALSE) +
  scale_fill_manual(values = palette_std) +
  theme_std +  
  theme_word + 
  theme_bar_stack

## ---- Area deprivation -------------------------------------------------------
area_dep_desc.plot <- descriptives$categorical %>%
  dplyr::filter(
    variable == "area_dep" & !cohort == "combined") %>%
  mutate(
    category = 
      factor(
        case_when(
          category == 1 ~ "Low", 
          category == 2 ~ "Medium", 
          category == 3 ~ "High", 
          is.na(category) ~ "Missing"), 
        levels = c("Low", "Medium", "High", "Missing"), 
        ordered = TRUE)) %>%
  mutate(perc_total = ifelse(perc_total == 100, 0, perc_total)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  ggplot(aes(x = cohort_neat, y = perc_total, fill = category)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Percentage") +
  coord_cartesian(
    ylim = c(0, 100), 
    expand = FALSE) +
  scale_fill_manual(values = palette_std) +
  theme_std +  
  theme_word + 
  theme_bar_stack


## ---- NDVI -------------------------------------------------------------------
ndvi_desc.plot <- descriptives$continuous %>%
  dplyr::filter(
    variable == "ndvi300_preg" & !cohort == "combined") %>%
  left_join(., ref_tab, by = "cohort") %>%
  ggplot(aes(x = cohort_neat, y = value)) +
  geom_boxplot(
    aes(
      ymin = perc_5, 
      lower = perc_25, 
      middle = perc_50, 
      upper = perc_75, 
      ymax = perc_95), 
    stat = "identity") +
  ylab("NDVI") +
  coord_cartesian(
    ylim = c(0, 1), 
    expand = FALSE) +
  theme_std + 
  theme_word + 
  theme_bar_stack

## ---- Pregnancy diabetes -----------------------------------------------------
preg_dia_desc.plot <- descriptives$categorical %>%
  dplyr::filter(
    variable == "preg_dia" & !cohort == "combined") %>%
  mutate(
    category = 
      factor(
        case_when(
          category == 0 ~ "No", 
          category == 1 ~ "Yes", 
          is.na(category) ~ "Missing"), 
        levels = c("No", "Yes", "Missing"), 
        ordered = TRUE)) %>%
  mutate(perc_total = ifelse(perc_total == 100, 0, perc_total)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  ggplot(aes(x = cohort_neat, y = perc_total, fill = category)) +
  geom_bar(position = "stack", stat = "identity") +
  ylab("Percentage") +
  coord_cartesian(
    ylim = c(0, 100), 
    expand = FALSE) +
  scale_fill_manual(values = palette_std[c(1, 2, 4)]) +
  theme_std +  
  theme_word + 
  theme_bar_stack

## ---- Save plots -------------------------------------------------------------
ggsave(
  plot = edu_desc.plot,
  filename = here("figures", "edu_desc.png"),
  h = 6, w = word_half, 
  units="cm", 
  dpi=1200, type="cairo")

ggsave(
  plot = area_dep_desc.plot,
  filename = here("figures", "area_dep_desc.png"),
  h = 6, w = word_half, 
  units="cm", 
  dpi=1200, type="cairo")

ggsave(
  plot = preg_dia_desc.plot,
  filename = here("figures", "preg_dia_desc.png"),
  h = 6, w = word_half, 
  units="cm", 
  dpi=1200, type="cairo")

ggsave(
  plot = ndvi_desc.plot,
  filename = here("figures", "ndvi_desc.png"),
  h = 6, w = word_half,
  units="cm", 
  dpi=1200, type="cairo")


################################################################################
# Figures 2-6: main results  
################################################################################
################################################################################
# Function to prepare data
################################################################################
age_orig <- c("bmi_24", "bmi_48", "bmi_96", "bmi_168", "bmi_215")
age_new <- c("0-1", "2-3", "4-7", "8-13", "14-17")

prepCatPlotdata <- function(
  x, lev_1_name = NULL, lev_1_lab = NULL, lev_2_name = NULL, 
  lev_2_lab = NULL){
  
  x %>%
    dplyr::filter(variable %in% c(lev_1_name, lev_2_name)) %>%
    mutate(
      age = factor(
        age, 
        levels = rev(age_orig),
        labels = rev(age_new),
        ordered = TRUE)) %>%
  mutate(
    variable = 
      case_when(
        variable == lev_1_name ~ lev_1_lab, 
        variable == lev_2_name ~ lev_2_lab)) %>%
  mutate(
    variable = factor(
      variable, 
      levels = c(lev_1_lab, 
                 lev_2_lab),
      ordered = TRUE))

}

################################################################################
# Get correct Ns for ordinal variables
################################################################################
age_orig_days <- c("0_730", "730_1461", "1461_2922", "2922_5113", "5113_6544")

## ---- Combined Ns ------------------------------------------------------------
ns_comb <- ord_cat_ipd %>%
  set_names(n_ref_cat_comb$variable) %>%
  
  map(function(x){
    
    x$output.list$TABLES.COMBINED_all.sources_counts %>%
      as_tibble(rownames = "category") %>%
      dplyr::select(category, "1") %>%
      dplyr::rename(count = "1")
  }) %>%
  bind_rows(.id = "variable") %>%
  mutate(cohort = "combined") 

## ---- Cohort Ns --------------------------------------------------------------
ns_extracted <- ord_cat_slma %>%
  map(function(x){
    
    x$output.list$TABLES.COMBINED_all.sources_counts %>%
      as_tibble(rownames = "category") 
    
  })

## ---- Fix one disclosure issue -----------------------------------------------
ns_extracted[[13]] <- tibble(
  category = c("0", "1", "NA"), 
  "0" = rep(-9999, 3),
  "1" = c(
    ds.length("p_d_zscores.0_730_m_fact", datasources = conns["sws"])[[1]] -3, 
    3, -9999), 
  "NA" = rep(-9999, 3))
  
ns_coh <- ns_extracted %>%
  map(function(x){
    
    x %>%
      dplyr::select(category, count = "1")
    
  }) %>%
  set_names(paste0(n_ref_cat_coh$variable, "_", n_ref_cat_coh$cohort)) %>% 
  bind_rows(.id = "variable") %>%
  separate(
    col = variable, 
    sep = "fact_", 
    into = c("variable", "cohort")) 

ns_cat_all <- bind_rows(ns_comb, ns_coh) %>%
  separate(
    col = variable, 
    sep = "_zscores.", 
    into = c("exposure", "age")) %>%
  mutate(age = str_remove(age, pattern = "_m_fact")) %>%
  mutate(age = str_remove(age, pattern = "_m_")) %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig_days),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  dplyr::filter(category != "NA") %>%
  mutate(variable = case_when(
    exposure == "edu_m" & category == "1" ~ "High education (ref)",
    exposure == "edu_m" & category == "2" ~ "Medium education (ref = high)",
    exposure == "edu_m" & category == "3" ~ "Low education (ref = high)",
    exposure == "a_d" & category == "1" ~ "Low deprivation (ref)",
    exposure == "a_d" & category == "2" ~ "Medium deprivation (ref = low)",
    exposure == "a_d" & category == "3" ~ "High deprivation (ref = low)", 
    exposure == "p_d" & category == "0" ~ "No GDM (ref)",
    exposure == "p_d" & category == "1" ~ "preg_dia1")) 

################################################################################
# Prepare IPD plots
################################################################################
## Get reference ns
refN <- function(ref_name, ref_tab = ns_cat_all){
  
  ref_tab %>%
    dplyr::filter(variable == ref_name & cohort == "combined") %>%
    dplyr::rename(ref = count) %>%
    dplyr::select(age, ref)
  
}



################################################################################
# Maternal education  
################################################################################




## ---- Main results -----------------------------------------------------------
mat_ed_ipd_main.pdata <- mat_ed.fit$ipd %>%
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  mutate(type = "main")
  

## ---- Sensitivity ------------------------------------------------------------
mat_ed_ipd_sens.pdata <- mat_ed_sel.fit %>%
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  mutate(type = "sens")



## ---- Combined ---------------------------------------------------------------
mat_ed_ipd.pdata <- bind_rows(mat_ed_ipd_main.pdata, mat_ed_ipd_sens.pdata) %>%
  prepCatPlotdata(
    lev_1_name = "edu_m2",
    lev_1_lab = "Medium education (ref = high)",
    lev_2_name = "edu_m3",
    lev_2_lab = "Low education (ref = high)") %>%
  arrange(desc(variable), desc(age)) %>%
  left_join(., ns_cat_all %>% dplyr::filter(cohort == "combined"), 
            ord_cat_dum, by = c("age", "variable")) %>%
  left_join(., refN("High education (ref)"), by = "age")



## ---- Area deprivation -------------------------------------------------------
area_dep_ipd.plotdata <- area_dep.fit$ipd %>%
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "area_dep2",
    lev_1_lab = "Medium deprivation (ref = low)",
    lev_2_name = "area_dep3",
    lev_2_lab = "High deprivation (ref = low)") %>%
  arrange(desc(variable), desc(age))  %>%
  left_join(., ns_cat_all %>% dplyr::filter(cohort == "combined"), 
            ord_cat_dum, by = c("age", "variable")) 

## Get reference ns
area_dep_ipd.plotdata <- area_dep_ipd.plotdata %>%
  left_join(., refN("Low deprivation (ref)"), by = "age")


## ---- NDVI -------------------------------------------------------------------
ndvi_ipd.plotdata <- ndvi.fit$ipd %>% 
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "ndvi300_preg_iqr_c") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(variable = "NDVI")

## ---- Pregnancy diabetes -----------------------------------------------------
preg_dia_ipd.plotdata <- preg_dia.fit$ipd %>%
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "preg_dia1") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  left_join(., ns_cat_all %>% dplyr::filter(cohort == "combined"), 
            ord_cat_dum, by = c("age", "variable")) 

## Get reference ns
preg_dia_ipd.plotdata <- preg_dia_ipd.plotdata %>%
  left_join(., refN("No GDM (ref)"), by = "age")

mat_ed_ipd.plotdata
################################################################################
# Plots  
################################################################################

## ---- Maternal education -----------------------------------------------------
png(
  file = here("figures", "mat_ed_ipd_forest.png"), 
  width = word_full, 
  height = 12, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = mat_ed_ipd.pdata %>% pull(est), 
  ci.lb = mat_ed_ipd.pdata %>% pull(lowci),
  ci.ub = mat_ed_ipd.pdata %>% pull(uppci), 
  slab = mat_ed_ipd.pdata %>% pull(age), 
  xlab = "Difference in childhood BMI by category of maternal education", 
  ilab =  cbind(
    mat_ed_ipd.pdata %>% pull(ref),
    mat_ed_ipd.pdata %>% pull(count),
    mat_ed.fit[[1]] %>% map(function(x){length(x$disclosure.risk)})),
  ilab.xpos = c(-2.5, -1.6, -0.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 3),
  ylim = c(1, 24),
  alim = c(-0.5, 1.5), 
  steps = 6, 
  digits = c(2, 2), 
  rows = c(8, 6.5, 5, 3.5, 2, 18.5, 17, 15.5, 14, 12.5), 
  psize = 0.8)

text(-2.5, 23, "Ref (high)", cex = 0.8, font = 2)
text(-1.6, 23, "Exposed", cex = 0.8, font = 2)
text(-0.7, 23, "N studies", cex = 0.8, font = 2)
text(-2.05, 24, "N participants", cex = 0.8, font = 2)

text(-2.92, 20.3, "Medium education", cex = 0.8, font = 2)
text(-3.02, 9.8, "Low education", cex = 0.8, font = 2)

dev.off()

## Reference Ns
n_ref %>% dplyr::filter(exposure == "edu_m")
mat_ed.fit[[1]] %>% map_int(function(x){x$Nvalid})

## ---- Area deprivation -------------------------------------------------------
png(
  file = here("figures", "area_dep_ipd_forest.png"), 
  width = word_full, 
  height = 12, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = area_dep_ipd.plotdata %>% pull(est), 
  ci.lb = area_dep_ipd.plotdata %>% pull(lowci),
  ci.ub = area_dep_ipd.plotdata %>% pull(uppci), 
  slab = area_dep_ipd.plotdata %>% pull(age), 
  xlab = "Difference in childhood BMI by category of area deprivation", 
  ilab =  cbind(
    area_dep_ipd.plotdata %>% pull(ref),
    area_dep_ipd.plotdata %>% pull(count),
    area_dep.fit[[1]] %>% map(function(x){length(x$disclosure.risk)})),
  ilab.xpos = c(-2.5, -1.6, -0.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 3),
  ylim = c(2, 23),
  alim = c(-0.5, 1.5), 
  steps = 5, 
  digits = c(2, 2), 
  rows = c(8, 6.5, 5, 3.5, 2, 18.5, 17, 15.5, 14, 12.5), 
  psize = 0.8)


text(-2.5, 22, "Ref (low)", cex = 0.8, font = 2)
text(-1.6, 22, "Exposed", cex = 0.8, font = 2)
text(-0.7, 22, "N studies", cex = 0.8, font = 2)
text(-2.05, 23, "N participants", cex = 0.8, font = 2)

text(-2.88, 19.8, "Medium deprivation", cex = 0.8, font = 2)
text(-2.96, 9.5, "High deprivation", cex = 0.8, font = 2)

dev.off()

## ---- NDVI -------------------------------------------------------------------
png(
  file = here("figures", "ndvi_ipd_forest.png"), 
  width = word_full, 
  height = 7, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = ndvi_ipd.plotdata %>% pull(est), 
  ci.lb = ndvi_ipd.plotdata %>% pull(lowci),
  ci.ub = ndvi_ipd.plotdata %>% pull(uppci), 
  slab = ndvi_ipd.plotdata %>% pull(age), 
  xlab = "Difference in childhood BMI by interquartile change NDVI", 
  ilab =  cbind(
    ndvi.fit[[1]] %>% map_int(function(x){x$Nvalid}),
    ndvi.fit[[1]] %>% map(function(x){length(x$disclosure.risk)})),
  ilab.xpos = c(-2.5, -1.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-4.0, 2.5),
  ylim = c(13, 20),
  alim = c(-1.5, 1), 
  steps = 6, 
  digits = c(2, 2), 
  rows = c(17, 16, 15, 14, 13), 
  psize = 0.8)

text(-2.5, 19, "N participants", cex = 0.8, font = 2)
text(-1.7, 19, "N studies", cex = 0.8, font = 2)

dev.off()

## ---- Pregnancy diabetes -----------------------------------------------------
png(
  file = here("figures", "preg_dia_ipd_forest.png"), 
  width = word_full, 
  height = 7, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = preg_dia_ipd.plotdata %>% pull(est), 
  ci.lb = preg_dia_ipd.plotdata %>% pull(lowci),
  ci.ub = preg_dia_ipd.plotdata %>% pull(uppci), 
  slab = preg_dia_ipd.plotdata %>% pull(age), 
  xlab = "Difference in childhood BMI where pregnancy diabetes reported", 
  ilab =  cbind(
    preg_dia_ipd.plotdata %>% pull(ref),
    preg_dia_ipd.plotdata %>% pull(count),
    preg_dia.fit$ipd %>% map(function(x){length(x$disclosure.risk)})),
  ilab.xpos = c(-2.5, -1.6, -0.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-4.0, 2.5),
  ylim = c(13, 20),
  alim = c(-0.5, 1), 
  steps = 4, 
  digits = c(2, 2), 
  rows = c(17, 16, 15, 14, 13), 
  psize = 0.8)

text(-2.5, 19, "Unexposed", cex = 0.8, font = 2)
text(-1.6, 19, "Exposed", cex = 0.8, font = 2)
text(-0.7, 19, "N studies", cex = 0.8, font = 2)
text(-2.05, 20, "N participants", cex = 0.8, font = 2)

dev.off()

################################################################################
# Supplementary figures S2 - S5: SLMA forest plots
################################################################################
################################################################################
# Calculate I2
################################################################################

## ---- Maternal education -----------------------------------------------------
mat_ed.meta <- mat_ed.fit$slma %>%
  map(
    ~dh.metaManual(
      model = ., 
      method = "reml")$model[c("edu_m2", "edu_m3")])

## ---- Area deprivation -------------------------------------------------------
area_dep.meta <- area_dep.fit$slma %>%
  map(
    ~dh.metaManual(
      model = ., 
      method = "reml")$model[c("area_dep2", "area_dep3")])

## ---- NDVI -------------------------------------------------------------------
ndvi.meta <- ndvi.fit$slma %>%
  map(
    ~dh.metaManual(
      model = ., 
      method = "reml")$model["ndvi300_preg_iqr_c"])

## ---- Gestational diabetes ---------------------------------------------------
preg_dia.meta <- preg_dia.fit$slma %>%
  map(
    ~dh.metaManual(
      model = ., 
      method = "reml")$model["preg_dia1"])

################################################################################
# Wrapper function to get I2 on the plot
################################################################################
polyWrap <- function(meta, row, cex = 0.6){
  
  addpoly(
    meta, 
    row = row, 
    mlab = list(bquote(
      paste("RE Model (", I^2, " = ",
            .(round(meta$I2, 1)),
            ")"))),
    cex = cex)
  
}

################################################################################
# Prepare data  
################################################################################

## ---- Function ---------------------------------------------------------------
forestDataSLMA <- function(obj, mod){
  
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
      ci_95 = beta + 1.96*se, 
      weight = 1/(se^2)) %>%
    dplyr::filter(cohort != "combined") %>%
    group_by(variable) %>%
    group_split() %>%
  map(~mutate(., weight_scaled = (weight / sum(weight)*100))) %>%
    bind_rows()
  
  return(out)
  
}

## ---- Maternal education -----------------------------------------------------
mat_ed_slma.plotdata <- list(mat_ed.fit$slma, mat_ed.mod) %>% 
  pmap(forestDataSLMA) %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "edu_m2",
    lev_1_lab = "Medium education (ref = high)",
    lev_2_name = "edu_m3",
    lev_2_lab = "Low education (ref = high)") %>%
    arrange(desc(variable), desc(age)) %>%
    left_join(., ref_tab, by = "cohort") %>%
  left_join(., ns_cat_all)

refNCoh <- function(ref_name){
  
  ns_cat_all %>%
    dplyr::filter(variable == ref_name & cohort != "combined") %>%
    dplyr::rename(ref = count) %>%
    dplyr::select(age, ref, cohort)
  
}

## Get reference ns
mat_ed_slma.plotdata <- mat_ed_slma.plotdata %>%
  left_join(., refNCoh("High education (ref)"), by = c("age", "cohort"))


## ---- Area deprivation -------------------------------------------------------
area_dep_slma.plotdata <- list(area_dep.fit$slma, area_dep.mod) %>% 
  pmap(forestDataSLMA) %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "area_dep2",
    lev_1_lab = "Medium deprivation (ref = low)",
    lev_2_name = "area_dep3",
    lev_2_lab = "High deprivation (ref = low)") %>%
  arrange(desc(variable), desc(age)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  left_join(., ns_cat_all)

## Get reference ns
area_dep_slma.plotdata <- area_dep_slma.plotdata %>%
  left_join(., refNCoh("Low deprivation (ref)"), by = c("age", "cohort"))

## ---- NDVI -------------------------------------------------------------------
ndvi_slma.plotdata <- list(ndvi.fit$slma, ndvi.mod) %>% 
  pmap(forestDataSLMA) %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "ndvi300_preg_iqr_c") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(variable = "NDVI") %>%
  left_join(., ref_tab, by = "cohort")


## ---- Pregnancy diabetes -----------------------------------------------------
preg_dia_slma.plotdata <- list(preg_dia.fit$slma, preg_dia.mod) %>% 
  pmap(forestDataSLMA) %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "preg_dia1" & cohort != "combined") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  left_join(., ns_cat_all)

## Get reference ns
preg_dia_slma.plotdata <- preg_dia_slma.plotdata %>%
  left_join(., refNCoh("No GDM (ref)"), by = c("age", "cohort"))

save.image("15_nov_22.RData")

################################################################################
# Plots  
################################################################################
slmaN <- function(x){ 
  
  n_study <- paste0("study", 1:dim(x$betamatrix.valid)[2])
  
  out <- n_study %>% map_int(function(y){x$output.summary[[y]][["Nvalid"]]})
  
  return(out)
  }

x <- ndvi.fit$slma[[1]]
################################################################################
# Figure S3a & S3b: Maternal education SLMA
################################################################################

## ---- Medium vs high ---------------------------------------------------------
mat_ed_slma_med.plotdata <- mat_ed_slma.plotdata %>% 
  dplyr::filter(variable == "Medium education (ref = high)" & cohort != "combined")

k_age_med <- mat_ed_slma_med.plotdata %>% 
  group_by(age) %>%
  summarise(n = n()) %>%
  pull(n) 

png(
  file = here("figures", "mat_ed_slma_med_forest.png"), 
  width = word_full, 
  height = 25, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = mat_ed_slma_med.plotdata  %>% pull(beta), 
  ci.lb = mat_ed_slma_med.plotdata %>% pull(ci_5),
  ci.ub = mat_ed_slma_med.plotdata %>% pull(ci_95), 
  slab = mat_ed_slma_med.plotdata %>% pull(cohort_neat), 
  ilab = cbind(
    mat_ed_slma_med.plotdata %>% pull(ref), 
    mat_ed_slma_med.plotdata %>% pull(count), 
    mat_ed_slma_med.plotdata %>% pull(weight_scaled) %>% round(2)), 
  ilab.x = c(-2.5, -1.6, -0.7),
  xlab = "Difference in childhood BMI by category of maternal education", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 3.5),
  ylim = c(0, 89),
  alim = c(-0.5, 2.5), 
  steps = 7, 
  digits = c(2, 2), 
  rows = c(85:69, 65:50, 46:30, 26:9, 5:1))
  
text(-2.5, 88, "High education", cex = 0.6, font = 2)
text(-1.6, 88, "Med education", cex = 0.6, font = 2)
text(-0.7, 88, "Study weight", cex = 0.6, font = 2)
text(-2.05, 89, "N participants", cex = 0.6, font = 2)

text(-3.18, 86.3, "0-1 years", cex = 0.6, font = 2)
text(-3.18, 66.3, "2-3 years", cex = 0.6, font = 2)
text(-3.18, 47.3, "4-7 years", cex = 0.6, font = 2)
text(-3.15, 27.3, "8-13 years", cex = 0.6, font = 2)
text(-3.13, 6.3, "14-17 years", cex = 0.6, font = 2)

polyWrap(meta = mat_ed.meta$bmi_24$edu_m2, row = 68)
polyWrap(meta = mat_ed.meta$bmi_48$edu_m2, row = 49)
polyWrap(meta = mat_ed.meta$bmi_96$edu_m2, row = 29)
polyWrap(meta = mat_ed.meta$bmi_168$edu_m2, row = 8)
polyWrap(meta = mat_ed.meta$bmi_215$edu_m2, row = 0)

dev.off()

## ---- Low vs high ------------------------------------------------------------
mat_ed_slma_low.plotdata <- mat_ed_slma.plotdata %>% 
  dplyr::filter(variable == "Low education (ref = high)" & cohort != "combined")

png(
  file = here("figures", "mat_ed_slma_low_forest.png"), 
  width = word_full, 
  height = 25, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = mat_ed_slma_low.plotdata  %>% pull(beta), 
  ci.lb = mat_ed_slma_low.plotdata %>% pull(ci_5),
  ci.ub = mat_ed_slma_low.plotdata %>% pull(ci_95), 
  slab = mat_ed_slma_low.plotdata %>% pull(cohort_neat), 
  ilab = cbind(
    mat_ed_slma_low.plotdata %>% pull(ref), 
    mat_ed_slma_low.plotdata %>% pull(count), 
    mat_ed_slma_low.plotdata %>% pull(weight_scaled) %>% round(2)), 
  ilab.x = c(-2.5, -1.6, -0.7),
  xlab = "Difference in childhood BMI by category of maternal education", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 3.5),
  ylim = c(0, 89),
  alim = c(-0.5, 2.5), 
  steps = 7, 
  digits = c(2, 2), 
  rows = c(85:69, 65:50, 46:30, 26:9, 5:1))

text(-2.5, 88, "High education", cex = 0.6, font = 2)
text(-1.6, 88, "Low education", cex = 0.6, font = 2)
text(-0.7, 88, "Study weight", cex = 0.6, font = 2)
text(-2.05, 89, "N participants", cex = 0.6, font = 2)

text(-3.18, 86.3, "0-1 years", cex = 0.6, font = 2)
text(-3.18, 66.3, "2-3 years", cex = 0.6, font = 2)
text(-3.18, 47.3, "4-7 years", cex = 0.6, font = 2)
text(-3.15, 27.3, "8-13 years", cex = 0.6, font = 2)
text(-3.13, 6.3, "14-17 years", cex = 0.6, font = 2)

polyWrap(meta = mat_ed.meta$bmi_24$edu_m3, row = 68)
polyWrap(meta = mat_ed.meta$bmi_48$edu_m3, row = 49)
polyWrap(meta = mat_ed.meta$bmi_96$edu_m3, row = 29)
polyWrap(meta = mat_ed.meta$bmi_168$edu_m3, row = 8)
polyWrap(meta = mat_ed.meta$bmi_215$edu_m3, row = 0)

dev.off()


################################################################################
# Figures S4a & S4b: Area deprivation SLMA  
################################################################################

## ---- Medium vs high ---------------------------------------------------------
area_dep_slma_med.plotdata <- area_dep_slma.plotdata %>% 
  dplyr::filter(variable == "Medium deprivation (ref = low)" &
                  cohort != "combined")

k_age_ad <- area_dep_slma_med.plotdata %>% 
  group_by(age) %>%
  summarise(n = n()) %>%
  pull(n) 

png(
  file = here("figures", "area_dep_slma_med_forest.png"), 
  width = word_full, 
  height = 25, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = area_dep_slma_med.plotdata  %>% pull(beta), 
  ci.lb = area_dep_slma_med.plotdata %>% pull(ci_5),
  ci.ub = area_dep_slma_med.plotdata %>% pull(ci_95), 
  slab = area_dep_slma_med.plotdata %>% pull(cohort_neat), 
  ilab = cbind(
    area_dep_slma_med.plotdata %>% pull(ref), 
    area_dep_slma_med.plotdata %>% pull(count), 
    area_dep_slma_med.plotdata %>% pull(weight_scaled) %>% round(2)), 
  ilab.x = c(-2.5, -1.6, -0.7),
  xlab = "Difference in childhood BMI by category of area deprivation", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 4),
  ylim = c(0, 58),
  alim = c(-0.5, 2.5), 
  steps = 9, 
  digits = c(2, 2), 
  rows = c(53:44, 40:32, 28:19, 15:6, 2:1))

text(-2.5, 57, "Low deprivation", cex = 0.6, font = 2)
text(-1.6, 57, "Med deprivation", cex = 0.6, font = 2)
text(-0.7, 57, "Study weight", cex = 0.6, font = 2)
text(-2.05, 58, "N participants", cex = 0.6, font = 2)

text(-3.15, 54.3, "0-1 years", cex = 0.6, font = 2)
text(-3.15, 41.3, "2-3 years", cex = 0.6, font = 2)
text(-3.15, 29.3, "4-7 years", cex = 0.6, font = 2)
text(-3.12, 16.3, "8-13 years", cex = 0.6, font = 2)
text(-3.10, 3.3, "14-17 years", cex = 0.6, font = 2)

polyWrap(meta = area_dep.meta$bmi_24$area_dep2, row = 43)
polyWrap(meta = area_dep.meta$bmi_48$area_dep2, row = 31)
polyWrap(meta = area_dep.meta$bmi_96$area_dep2, row = 18)
polyWrap(meta = area_dep.meta$bmi_168$area_dep2, row = 5)
polyWrap(meta = area_dep.meta$bmi_215$area_dep2, row = 0)

dev.off()

## ---- Low vs high ------------------------------------------------------------
area_dep_slma_high.plotdata <- area_dep_slma.plotdata %>% 
  dplyr::filter(variable == "High deprivation (ref = low)" & 
                  cohort != "combined")

png(
  file = here("figures", "area_dep_slma_high_forest.png"), 
  width = word_full, 
  height = 25, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = area_dep_slma_high.plotdata  %>% pull(beta), 
  ci.lb = area_dep_slma_high.plotdata %>% pull(ci_5),
  ci.ub = area_dep_slma_high.plotdata %>% pull(ci_95), 
  slab = area_dep_slma_high.plotdata %>% pull(cohort_neat), 
  ilab = cbind(
    area_dep_slma_high.plotdata %>% pull(ref), 
    area_dep_slma_high.plotdata %>% pull(count), 
    area_dep_slma_high.plotdata %>% pull(weight_scaled) %>% round(2)), 
  ilab.x = c(-2.5, -1.6, -0.7),
  xlab = "Difference in childhood BMI by category of area deprivation", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 4),
  ylim = c(0, 58),
  alim = c(-0.5, 2.5), 
  steps = 9, 
  digits = c(2, 2), 
  rows = c(53:44, 40:32, 28:19, 15:6, 2:1))

text(-2.5, 57, "Low deprivation", cex = 0.6, font = 2)
text(-1.6, 57, "High deprivation", cex = 0.6, font = 2)
text(-0.7, 57, "Study weight", cex = 0.6, font = 2)
text(-2.05, 58, "N participants", cex = 0.6, font = 2)

text(-3.15, 54.3, "0-1 years", cex = 0.6, font = 2)
text(-3.15, 41.3, "2-3 years", cex = 0.6, font = 2)
text(-3.15, 29.3, "4-7 years", cex = 0.6, font = 2)
text(-3.12, 16.3, "8-13 years", cex = 0.6, font = 2)
text(-3.10, 3.3, "14-17 years", cex = 0.6, font = 2)

polyWrap(meta = area_dep.meta$bmi_24$area_dep3, row = 43)
polyWrap(meta = area_dep.meta$bmi_48$area_dep3, row = 31)
polyWrap(meta = area_dep.meta$bmi_96$area_dep3, row = 18)
polyWrap(meta = area_dep.meta$bmi_168$area_dep3, row = 5)
polyWrap(meta = area_dep.meta$bmi_215$area_dep3, row = 0)

dev.off()

################################################################################
# Figure S5: NDVI SLMA  
################################################################################
png(
  file = here("figures", "ndvi_slma_forest.png"), 
  width = word_full, 
  height = 20, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = ndvi_slma.plotdata  %>% pull(beta), 
  ci.lb = ndvi_slma.plotdata %>% pull(ci_5),
  ci.ub = ndvi_slma.plotdata %>% pull(ci_95), 
  slab = ndvi_slma.plotdata %>% pull(cohort_neat), 
  xlab = "Difference in childhood BMI by IQR change in NDVI", 
  ilab = cbind(
    ndvi.fit$slma %>% map(slmaN) %>% unlist %>% as.numeric, 
    ndvi_slma.plotdata %>% pull(weight_scaled) %>% round(2)), 
  ilab.x = c(-2, -1.5),
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3, 2),
  ylim = c(0, 57),
  alim = c(-1, 1), 
  steps = 5, 
  digits = c(2, 2), 
  rows = c(53:44, 40:32, 28:19, 15:6, 2:1))

text(-2, 56, "N participants", cex = 0.6, font = 2)
text(-1.5, 56, "Study weight", cex = 0.6, font = 2)

text(-2.77, 54.3, "0-1 years", cex = 0.6, font = 2)
text(-2.77, 41.3, "2-3 years", cex = 0.6, font = 2)
text(-2.77, 29.3, "4-7 years", cex = 0.6, font = 2)
text(-2.75, 16.3, "8-13 years", cex = 0.6, font = 2)
text(-2.74, 3.3, "14-17 years", cex = 0.6, font = 2)

polyWrap(meta = ndvi.meta$bmi_24$ndvi300_preg_iqr_c, row = 43)
polyWrap(meta = ndvi.meta$bmi_48$ndvi300_preg_iqr_c, row = 31)
polyWrap(meta = ndvi.meta$bmi_96$ndvi300_preg_iqr_c, row = 18)
polyWrap(meta = ndvi.meta$bmi_168$ndvi300_preg_iqr_c, row = 5)
polyWrap(meta = ndvi.meta$bmi_215$ndvi300_preg_iqr_c, row = 0)

dev.off()

################################################################################
# Figure S6: Pregnancy diabetes SLMA  
################################################################################
k_age_ad <- preg_dia_slma.plotdata %>% 
  group_by(age) %>%
  summarise(n = n())

## ---- ALSPAC disclosure issue ------------------------------------------------
preg_dia_slma_disc.plotdata <- preg_dia_slma.plotdata %>%
  mutate(count = ifelse(cohort == "alspac" & count == 3, "<5", count))

png(
  file = here("figures", "preg_dia_slma_forest.png"), 
  width = word_full, 
  height = 25, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = preg_dia_slma_disc.plotdata  %>% pull(beta), 
  ci.lb = preg_dia_slma_disc.plotdata %>% pull(ci_5),
  ci.ub = preg_dia_slma_disc.plotdata %>% pull(ci_95), 
  slab = preg_dia_slma_disc.plotdata %>% pull(cohort_neat), 
  ilab = cbind(
    preg_dia_slma_disc.plotdata %>% pull(ref), 
    preg_dia_slma_disc.plotdata %>% pull(count), 
    preg_dia_slma_disc.plotdata %>% pull(weight_scaled) %>% round(2)), 
  ilab.x = c(-3.0, -2.1, -1.2),
  xlab = "Difference in childhood BMI where preganancy diabetes reported", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-4.0, 3),
  ylim = c(1, 74),
  alim = c(-1, 2), 
  steps = 6, 
  digits = c(2, 2), 
  rows = c(70:57, 53:41, 37:24, 20:7, 3:1))

text(-3.0, 73, "Unexposed", cex = 0.6, font = 2)
text(-2.1, 73, "Exposed", cex = 0.6, font = 2)
text(-1.2, 73, "Study weight", cex = 0.6, font = 2)
text(-2.55, 74, "N participants", cex = 0.6, font = 2)

text(-3.67, 71.3, "0-1 years", cex = 0.6, font = 2)
text(-3.67, 54.3, "2-3 years", cex = 0.6, font = 2)
text(-3.67, 38.3, "4-7 years", cex = 0.6, font = 2)
text(-3.65, 21.3, "8-13 years", cex = 0.6, font = 2)
text(-3.63, 4.3, "14-17 years", cex = 0.6, font = 2)

polyWrap(meta = preg_dia.meta$bmi_24$preg_dia1, row = 56)
polyWrap(meta = preg_dia.meta$bmi_48$preg_dia1, row = 40)
polyWrap(meta = preg_dia.meta$bmi_96$preg_dia1, row = 23)
polyWrap(meta = preg_dia.meta$bmi_168$preg_dia1, row = 6)
polyWrap(meta = preg_dia.meta$bmi_215$preg_dia1, row = 0)

dev.off()









################################################################################
# Sex-stratified figures  
################################################################################
################################################################################
# Prepare data  
################################################################################

## ---- Maternal education -----------------------------------------------------
mat_ed_m.pdata <- mat_ed_m.fit$ipd %>%
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "edu_m2",
    lev_1_lab = "Medium education (ref = high)",
    lev_2_name = "edu_m3",
    lev_2_lab = "Low education (ref = high)") %>%
  mutate(sex = "Male")

mat_ed_f.pdata <- mat_ed_f.fit$ipd %>%
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "edu_m2",
    lev_1_lab = "Medium education (ref = high)",
    lev_2_name = "edu_m3",
    lev_2_lab = "Low education (ref = high)") %>%
  mutate(sex = "Female")

mat_ed_sex.pdata <- list(mat_ed_m.pdata, mat_ed_f.pdata) %>%
  bind_rows %>%
  arrange(variable, desc(age), desc(sex)) 

## ---- Area deprivation -------------------------------------------------------
area_dep_m.pdata <- area_dep_m.fit$ipd %>% 
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "area_dep2",
    lev_1_lab = "Medium deprivation (ref = low)",
    lev_2_name = "area_dep3",
    lev_2_lab = "High deprivation (ref = low)") %>%
  mutate(sex = "Male")

area_dep_f.pdata <- area_dep_f.fit$ipd %>% 
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "area_dep2",
    lev_1_lab = "Medium deprivation (ref = low)",
    lev_2_name = "area_dep3",
    lev_2_lab = "High deprivation (ref = low)") %>%
  mutate(sex = "Female")

area_dep_sex.pdata <- list(area_dep_m.pdata, area_dep_f.pdata) %>%
  bind_rows %>%
  arrange(variable, desc(age), desc(sex))

## ---- NDVI -------------------------------------------------------------------
ndvi_m.plotdata <- ndvi_m.fit$ipd %>% 
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "ndvi300_preg_iqr_s") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(
    variable = "NDVI", 
    sex = "Male")     

ndvi_f.plotdata <- ndvi_f.fit$ipd %>% 
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "ndvi300_preg_iqr_s") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(
    variable = "NDVI", 
    sex = "Female")    

ndvi_sex.pdata <- list(ndvi_m.plotdata, ndvi_f.plotdata) %>%
  bind_rows %>%
  arrange(desc(age), variable, desc(sex))
  
## ---- Pregnancy diabetes -----------------------------------------------------
preg_dia_m.pdata <- preg_dia_m.fit$ipd %>%
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "preg_dia1") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(
    variable = "Pregnancy diabetes reported", 
    sex = "Male")

preg_dia_f.pdata <- preg_dia_f.fit$ipd %>%
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "preg_dia1") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(
    variable = "Pregnancy diabetes reported", 
    sex = "Female")

preg_dia_sex.pdata <- list(preg_dia_m.pdata, preg_dia_f.pdata) %>%
  bind_rows %>%
  arrange(desc(age), variable, desc(sex))

################################################################################
# Figure S7: Maternal education stratified by sex  
################################################################################
png(
  file = here("figures", "mat_ed_sex_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = mat_ed_sex.pdata %>% pull(est), 
  ci.lb = mat_ed_sex.pdata %>% pull(lowci),
  ci.ub = mat_ed_sex.pdata %>% pull(uppci), 
  slab = mat_ed_sex.pdata %>% pull(age),
  xlab = "Difference in childhood BMI by category of maternal education", 
  ilab =  cbind(
    c(
      rbind(
        rep(mat_ed_m.fit$ipd %>% map_int(function(x){x$Nvalid}), 2),
        rep(mat_ed_f.fit$ipd %>% map_int(function(x){x$Nvalid}), 2))
      ),
    c(
      rbind(
      rep(mat_ed_m.fit[[1]] %>% map(function(x){length(x$disclosure.risk)}), 2),
      rep(mat_ed_f.fit[[1]] %>% map(function(x){length(x$disclosure.risk)}), 2)))
  ),
  ilab.xpos = c(-2, -1.2),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 3),
  ylim = c(1, 27),
  alim = c(-1, 1.2), 
  steps = 6, 
  digits = c(2, 2), 
  rows = c(22:13, 10:1),
  col = rep(c("#F4A261", "#264653"), 10))

text(-2, 26, "N participants", cex = 0.8, font = 2)
text(-1.2, 26, "N studies", cex = 0.8, font = 2)

text(-2.87, 23.5, "Medium education", cex = 0.8, font = 2)
text(-2.98, 11.5, "Low education", cex = 0.8, font = 2)

dev.off()

################################################################################
# Figure S8: Area deprivation stratified by sex  
################################################################################
png(
  file = here("figures", "area_dep_sex_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = area_dep_sex.pdata %>% pull(est), 
  ci.lb = area_dep_sex.pdata %>% pull(lowci),
  ci.ub = area_dep_sex.pdata %>% pull(uppci), 
  slab = area_dep_sex.pdata %>% pull(age),
  xlab = "Difference in childhood BMI by category of area deprivation", 
  ilab =  cbind(
    c(
      rbind(
        rep(area_dep_m.fit$ipd %>% map_int(function(x){x$Nvalid}), 2),
        rep(area_dep_f.fit$ipd %>% map_int(function(x){x$Nvalid}), 2))
    ),
    c(
      rbind(
        rep(area_dep_m.fit[[1]] %>% map(function(x){length(x$disclosure.risk)}), 2),
        rep(area_dep_f.fit[[1]] %>% map(function(x){length(x$disclosure.risk)}), 2)))
  ),
  ilab.xpos = c(-2, -1.2),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 3),
  ylim = c(1, 26),
  alim = c(-1, 1.5), 
  steps = 6, 
  digits = c(2, 2), 
  rows = c(22:13, 10:1),
  col = rep(c("#F4A261", "#264653"), 10))

text(-2, 25, "N participants", cex = 0.8, font = 2)
text(-1.2, 25, "N studies", cex = 0.8, font = 2)

text(-2.84, 23.3, "Medium deprivation", cex = 0.8, font = 2)
text(-2.93, 11.3, "High deprivation", cex = 0.8, font = 2)

dev.off()

################################################################################
# Figure S9: NDVI stratified by sex   
################################################################################
png(
  file = here("figures", "ndvi_ipd_sex_forest.png"), 
  width = word_full, 
  height = 10, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = ndvi_sex.pdata %>% pull(est), 
  ci.lb = ndvi_sex.pdata %>% pull(lowci),
  ci.ub = ndvi_sex.pdata %>% pull(uppci), 
  slab = ndvi_sex.pdata %>% pull(age), 
  xlab = "Difference in childhood BMI by one unit change NDVI", 
  ilab =  cbind(
    c(
      rbind(
        area_dep_m.fit$ipd %>% map_int(function(x){x$Nvalid}),
        area_dep_f.fit$ipd %>% map_int(function(x){x$Nvalid}))
    ),
    c(
      rbind(
        area_dep_m.fit[[1]] %>% map(function(x){length(x$disclosure.risk)}),
        area_dep_f.fit[[1]] %>% map(function(x){length(x$disclosure.risk)})))
  ),
  ilab.xpos = c(-2.5, -1.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-4.0, 2.5),
  ylim = c(0, 13),
  alim = c(-1.5, 1), 
  steps = 6, 
  digits = c(2, 2), 
  rows = 10:1,
  col = rep(c("#F4A261", "#264653"), 5))

text(-2.5, 12, "N participants", cex = 0.8, font = 2)
text(-1.7, 12, "N studies", cex = 0.8, font = 2)

dev.off()

################################################################################
# Figure S10: Pregnancy diabetes  
################################################################################
png(
  file = here("figures", "preg_dia_sex_forest.png"), 
  width = word_full, 
  height = 12, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = preg_dia_sex.pdata %>% pull(est), 
  ci.lb = preg_dia_sex.pdata %>% pull(lowci),
  ci.ub = preg_dia_sex.pdata %>% pull(uppci), 
  slab = preg_dia_sex.pdata %>% pull(age), 
  xlab = "Difference in childhood BMI where pregnancy diabetes reported", 
  ilab =  cbind(
    c(
      rbind(
        area_dep_m.fit$ipd %>% map_int(function(x){x$Nvalid}),
        area_dep_f.fit$ipd %>% map_int(function(x){x$Nvalid}))
    ),
    c(
      rbind(
        area_dep_m.fit[[1]] %>% map(function(x){length(x$disclosure.risk)}),
        area_dep_f.fit[[1]] %>% map(function(x){length(x$disclosure.risk)})))
  ),
  ilab.xpos = c(-2.5, -1.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-4.0, 2.5),
  ylim = c(0, 13),
  alim = c(-1.5, 1), 
  steps = 6, 
  digits = c(2, 2), 
  rows = 10:1,
  col = rep(c("#F4A261", "#264653"), 5))

text(-2.5, 12, "N participants", cex = 0.8, font = 2)
text(-1.7, 12, "N studies", cex = 0.8, font = 2)

dev.off()


################################################################################
# Ethnicity sensitivity analysis
################################################################################
################################################################################
# Prepare data
################################################################################
mat_ed_eth.plotdata <- list(mat_ed_eth.fit$slma, mat_ed_eth.mod) %>% 
  pmap(forestDataSLMA) %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "edu_m2",
    lev_1_lab = "Medium education (ref = high)",
    lev_2_name = "edu_m3",
    lev_2_lab = "Low education (ref = high)") %>%
  left_join(., ref_tab, by = "cohort") %>%
  left_join(., ns_cat_all)

## Get reference ns
mat_ed_eth.plotdata <- mat_ed_eth.plotdata %>%
  left_join(., refNCoh("High education (ref)"), by = c("age", "cohort"))
  mutate(adjust = "ethnicity")

mat_ed_main.plotdata <- mat_ed_slma.plotdata %>%
  dplyr::filter(
    (age %in% c("0-1", "2-3", "4-7", "8-13") & cohort %in% eth_coh) |
    age == "14-17" & cohort == "alspac") %>%
  mutate(adjust = "main")

mat_ed_eth.pdata <- bind_rows(mat_ed_eth.plotdata, mat_ed_main.plotdata) %>%
  dplyr::filter(cohort != "combined") %>%
  arrange(desc(variable), desc(age), cohort_neat, desc(adjust))


## ---- Area deprivation -------------------------------------------------------
area_dep_eth.plotdata <- list(area_dep_eth.fit$slma, area_dep_eth.mod) %>% 
  pmap(forestDataSLMA) %>%
  bind_rows(.id = "age") %>%
  prepCatPlotdata(
    lev_1_name = "area_dep2",
    lev_1_lab = "Medium deprivation (ref = low)",
    lev_2_name = "area_dep3",
    lev_2_lab = "High deprivation (ref = low)") %>%
  arrange(desc(variable), desc(age)) %>%
  left_join(., ref_tab, by = "cohort") %>%
  left_join(., ord_cat_slma.tab, by = c("age", "cohort", "variable")) %>%
  mutate(adjust = "ethnicity")

area_dep_main.plotdata <- area_dep_slma.plotdata %>%
  dplyr::filter(
    (age %in% c("0-1", "2-3", "4-7", "8-13") & cohort %in% env_eth_coh) |
      age == "14-17" & cohort == "alspac") %>%
  mutate(adjust = "main")

area_dep_eth.pdata <- bind_rows(area_dep_eth.plotdata, area_dep_main.plotdata) %>%
  dplyr::filter(cohort != "combined") %>%
  arrange(desc(variable), desc(age), cohort_neat, desc(adjust)) 

## ---- NDVI -------------------------------------------------------------------
ndvi_eth.plotdata <- list(ndvi_eth.fit$slma, ndvi_eth.mod) %>% 
  pmap(forestDataSLMA) %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "ndvi300_preg_iqr_s") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(variable = "NDVI") %>%
  left_join(., ref_tab, by = "cohort") %>%
  mutate(adjust = "ethnicity")

ndvi_main.plotdata <- ndvi_slma.plotdata %>%
  dplyr::filter(
    (age %in% c("0-1", "2-3", "4-7", "8-13") & cohort %in% env_eth_coh) |
      age == "14-17" & cohort == "alspac") %>%
  mutate(adjust = "main")

ndvi_eth.pdata <- bind_rows(ndvi_eth.plotdata, ndvi_main.plotdata) %>%
  dplyr::filter(cohort != "combined") %>%
  arrange(desc(variable), desc(age), cohort_neat, desc(adjust)) 

## ---- Pregnancy diabetes -----------------------------------------------------
preg_eth_slma.plotdata <-list(preg_dia_eth.fit$slma, preg_dia_eth.mod) %>% 
  pmap(forestDataSLMA) %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "preg_dia1" & cohort != "combined") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(variable = "Pregnancy diabetes reported") %>%
  left_join(., ref_tab, by = "cohort") %>%
  mutate(adjust = "ethnicity")

pred_dia_main.plotdata <- preg_dia_slma.plotdata %>%
  dplyr::filter(
    (age %in% c("0-1", "2-3", "4-7", "8-13") & cohort %in% eth_coh) |
      age == "14-17" & cohort == "alspac") %>%
  mutate(adjust = "main")

preg_dia_eth.pdata <- bind_rows(pred_dia_main.plotdata, preg_eth_slma.plotdata) %>%
  dplyr::filter(cohort != "combined") %>%
  arrange(desc(variable), desc(age), cohort_neat, desc(adjust)) 

################################################################################
# Make plots
################################################################################
################################################################################
# Neater function to get valid Ns
################################################################################
getEthNs <- function(mod, fit){
  
  c_names <- mod %>%
    map(function(x){
      
      tibble(
        cohort = x$cohort, 
        study = paste0("study", seq(1, length(cohort)))
      )
      
    }) %>% 
    bind_rows(.id = "age")
  
  c_ns <- fit %>%
    map(function(x){
      
      n_study <- paste0("study", 1:x$num.valid.studies)
      
      out <- tibble(
        study = n_study,
        n = n_study %>% map_int(function(y){x$output.summary[[y]][["Nvalid"]]})
      )
      
    }) %>%
    bind_rows(.id = "age")
  
  out <- left_join(c_names, c_ns, by = c("age", "study")) %>%
    dplyr::select(age, cohort, n) %>%
    mutate(age = case_when(
      
      age == "bmi_24" ~ "0-1",
      age == "bmi_48" ~ "2-3",
      age == "bmi_96" ~ "4-7",
      age == "bmi_168" ~ "8-13",
      age == "bmi_215" ~ "14-17"
    ))
  
  return(out)
  
}

################################################################################
# Figure S10a & S10b: Maternal education SLMA
################################################################################

## ---- Medium vs high ---------------------------------------------------------
mat_ed_eth_med.pdata <- mat_ed_eth.pdata %>% 
  dplyr::filter(variable == "Medium education (ref = high)")

png(
  file = here("figures", "mat_ed_eth_med_forest.png"), 
  width = word_full, 
  height = 25, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = mat_ed_eth_med.pdata  %>% pull(beta), 
  ci.lb = mat_ed_eth_med.pdata %>% pull(ci_5),
  ci.ub = mat_ed_eth_med.pdata %>% pull(ci_95), 
  slab = mat_ed_eth_med.pdata %>% pull(cohort_neat), 
  ilab = mat_ed_eth_med.pdata %>% pull(count),  
  ilab.x = -1,
  xlab = "Difference in childhood BMI by category of maternal education", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-2, 3.5),
  ylim = c(0, 74),
  alim = c(-0.5, 2.5), 
  steps = 7, 
  digits = c(2, 2), 
  rows = c(70:57, 53:40, 36:23, 19:6, 2:1),
  col = rep(c("#264653", "#F4A261"), 29))
  
text(-1, 73, "N subjects", cex = 0.6, font = 2)

text(-1.74, 71.3, "0-1 years", cex = 0.6, font = 2)
text(-1.74, 54.3, "2-3 years", cex = 0.6, font = 2)
text(-1.74, 37.3, "4-7 years", cex = 0.6, font = 2)
text(-1.71, 20.3, "8-13 years", cex = 0.6, font = 2)
text(-1.69, 3.3, "14-17 years", cex = 0.6, font = 2)

dev.off()


## ---- Low vs high ------------------------------------------------------------
mat_ed_eth_low.pdata <- mat_ed_eth.pdata %>% 
  dplyr::filter(variable == "Low education (ref = high)")

png(
  file = here("figures", "mat_ed_eth_low_forest.png"), 
  width = word_full, 
  height = 25, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = mat_ed_eth_low.pdata  %>% pull(beta), 
  ci.lb = mat_ed_eth_low.pdata %>% pull(ci_5),
  ci.ub = mat_ed_eth_low.pdata %>% pull(ci_95), 
  slab = mat_ed_eth_low.pdata %>% pull(cohort_neat), 
  ilab = mat_ed_eth_low.pdata %>% pull(count),  
  ilab.x = -1,
  xlab = "Difference in childhood BMI by category of maternal education", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-2, 3.5),
  ylim = c(0, 74),
  alim = c(-0.5, 2.5), 
  steps = 7, 
  digits = c(2, 2), 
  rows = c(70:57, 53:40, 36:23, 19:6, 2:1),
  col = rep(c("#264653", "#F4A261"), 29))

text(-1, 73, "N subjects", cex = 0.6, font = 2)

text(-1.74, 71.3, "0-1 years", cex = 0.6, font = 2)
text(-1.74, 54.3, "2-3 years", cex = 0.6, font = 2)
text(-1.74, 37.3, "4-7 years", cex = 0.6, font = 2)
text(-1.71, 20.3, "8-13 years", cex = 0.6, font = 2)
text(-1.69, 3.3, "14-17 years", cex = 0.6, font = 2)

dev.off()


################################################################################
# Figures S11a & S11b: Area deprivation SLMA  
################################################################################

## ---- Medium vs high ---------------------------------------------------------
area_dep_eth_med.plotdata <- area_dep_eth.pdata %>% 
  dplyr::filter(variable == "Medium deprivation (ref = low)" & cohort != "combined")

png(
  file = here("figures", "area_dep_eth_med_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = area_dep_eth_med.plotdata  %>% pull(beta), 
  ci.lb = area_dep_eth_med.plotdata %>% pull(ci_5),
  ci.ub = area_dep_eth_med.plotdata %>% pull(ci_95), 
  slab = area_dep_eth_med.plotdata %>% pull(cohort_neat), 
  ilab = area_dep_eth_med.plotdata %>% pull(count), 
  ilab.x = -1,
  xlab = "Difference in childhood BMI by category of area deprivation", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-2, 4),
  ylim = c(0, 50),
  alim = c(-1.5, 2.5), 
  steps = 9, 
  digits = c(2, 2), 
  rows = c(46:39, 35:28, 24:17, 13:6, 2:1),
  col = rep(c("#264653", "#F4A261"), 17))

text(-1, 49, "N participants", cex = 0.6, font = 2)

text(-1.74, 47.3, "0-1 years", cex = 0.6, font = 2)
text(-1.74, 36.3, "2-3 years", cex = 0.6, font = 2)
text(-1.74, 25.3, "4-7 years", cex = 0.6, font = 2)
text(-1.71, 14.3, "8-13 years", cex = 0.6, font = 2)
text(-1.69, 3.3, "14-17 years", cex = 0.6, font = 2)

dev.off()

## ---- Low vs high ------------------------------------------------------------
area_dep_eth_low.plotdata <- area_dep_eth.pdata %>% 
  dplyr::filter(variable == "High deprivation (ref = low)" & 
                  cohort != "combined")

png(
  file = here("figures", "area_dep_eth_high_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = area_dep_eth_low.plotdata  %>% pull(beta), 
  ci.lb = area_dep_eth_low.plotdata %>% pull(ci_5),
  ci.ub = area_dep_eth_low.plotdata %>% pull(ci_95), 
  slab = area_dep_eth_low.plotdata %>% pull(cohort_neat), 
  ilab = area_dep_eth_low.plotdata %>% pull(count), 
  ilab.x = -1,
  xlab = "Difference in childhood BMI by category of area deprivation", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-2, 4),
  ylim = c(0, 50),
  alim = c(-1.5, 2.5), 
  steps = 9, 
  digits = c(2, 2), 
  rows = c(46:39, 35:28, 24:17, 13:6, 2:1),
  col = rep(c("#264653", "#F4A261"), 17))

text(-1, 49, "N participants", cex = 0.6, font = 2)

text(-1.74, 47.3, "0-1 years", cex = 0.6, font = 2)
text(-1.74, 36.3, "2-3 years", cex = 0.6, font = 2)
text(-1.74, 25.3, "4-7 years", cex = 0.6, font = 2)
text(-1.71, 14.3, "8-13 years", cex = 0.6, font = 2)
text(-1.69, 3.3, "14-17 years", cex = 0.6, font = 2)

dev.off()

################################################################################
# Figure S12: NDVI SLMA  
################################################################################
ndvi_ns_eth <- getEthNs(mod = ndvi_eth.mod, fit = ndvi_eth.fit$slma) %>%
  mutate(adjust = "ethnicity")

ndvi_ns_main <- getEthNs(mod = ndvi.mod, fit = ndvi.fit$slma) %>%
  mutate(adjust = "main")

ndvi_ns <- bind_rows(ndvi_ns_eth, ndvi_ns_main)

ndvi_eth.pd <- ndvi_eth.pdata %>%
  left_join(., ndvi_ns, by = c("age", "cohort", "adjust")) 

png(
  file = here("figures", "ndvi_eth_forest.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = ndvi_eth.pd %>% pull(beta), 
  ci.lb = ndvi_eth.pd %>% pull(ci_5),
  ci.ub = ndvi_eth.pd %>% pull(ci_95), 
  slab = ndvi_eth.pd %>% pull(cohort_neat), 
  xlab = "Difference in childhood BMI by IQR change in NDVI", 
  ilab = ndvi_eth.pd$n,
  ilab.x = -1,
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-2, 2),
  ylim = c(0, 52),
  alim = c(-1, 1), 
  steps = 5, 
  digits = c(2, 2), 
  rows = c(46:39, 35:28, 24:17, 13:6, 2:1),
  col = rep(c("#264653", "#F4A261"), 17))


text(-1, 51, "N participants", cex = 0.6, font = 2)

text(-1.82, 47.3, "0-1 years", cex = 0.6, font = 2)
text(-1.81, 36.3, "2-3 years", cex = 0.6, font = 2)
text(-1.81, 25.3, "4-7 years", cex = 0.6, font = 2)
text(-1.80, 14.3, "8-13 years", cex = 0.6, font = 2)
text(-1.78, 3.3, "14-17 years", cex = 0.6, font = 2)

dev.off()

################################################################################
# Figure S13: Pregnancy diabetes SLMA  
################################################################################
preg_dia_ns_eth <- getEthNs(mod = preg_dia_eth.mod, fit = preg_dia_eth.fit$slma) %>%
  mutate(adjust = "ethnicity")

preg_dia_ns_main <- getEthNs(mod = preg_dia.mod, fit = preg_dia.fit$slma) %>%
  mutate(adjust = "main")

preg_dia_ns <- bind_rows(preg_dia_ns_eth, preg_dia_ns_main)

preg_dia_eth.pd <- preg_dia_eth.pdata %>%
  left_join(., preg_dia_ns, by = c("age", "cohort", "adjust")) 


png(
  file = here("figures", "preg_dia_slma_forest.png"), 
  width = word_full, 
  height = 25, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = preg_dia_eth.pd  %>% pull(beta), 
  ci.lb = preg_dia_eth.pd %>% pull(ci_5),
  ci.ub = preg_dia_eth.pd %>% pull(ci_95), 
  slab = preg_dia_eth.pd %>% pull(cohort_neat), 
  ilab = preg_dia_eth.pd$n,
  ilab.x = -2,
  xlab = "Difference in childhood BMI where preganancy diabetes reported", 
  cex = 0.6, 
  cex.axis = 0.8,
  cex.lab = 0.8,
  header = c("Cohort", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3, 3),
  ylim = c(1, 74),
  alim = c(-1, 2), 
  steps = 4, 
  digits = c(2, 2), 
  rows = c(70:56, 52:40, 36:23, 19:6, 2:1),
  col = rep(c("#264653", "#F4A261"), 29))

text(-2, 73, "N participants", cex = 0.6, font = 2)

text(-2.72, 71.3, "0-1 years", cex = 0.6, font = 2)
text(-2.72, 53.3, "2-3 years", cex = 0.6, font = 2)
text(-2.72, 37.3, "4-7 years", cex = 0.6, font = 2)
text(-2.70, 20.3, "8-13 years", cex = 0.6, font = 2)
text(-2.68, 3.3, "14-17 years", cex = 0.6, font = 2)

dev.off()

################################################################################
# GDM sensitivity plot  
################################################################################

## ---- Get estimates ----------------------------------------------------------
gdm_sens <- tribble(
  ~cohort, ~age, ~est, ~lowci, ~uppci,
  "alspac", "0_1", 1.00, -0.15, 2.16,
  "bib", "0_1", -0.29, -0.42, -0.16,
  "dnbc", "0_1", -0.10, -0.20, 0.01,
  "eden", "0_1", -0.06, -0.35, -0.22,
  "gecko", "0_1", 0.38, 0.13, 0.62,
  "genr", "0_1", 0.26, -0.12, 0.64,
  "inma", "0_1", 0.20, -0.01, 0.41,
  "moba", "0_1", -0.05, -0.15, 0.06,
  "ninfea", "0_1", -0.02, -0.16, 0.11,
  "raine", "0_1", -0.21, -0.50, 0.09,
  "rhea", "0_1", 0.26, -0.01, 0.53,
  "sws", "0_1", -0.01, -0.33, 0.31, 
  "alspac", "2_3", 0.39, -0.66, 1.44,
  "bib", "2_3", -0.02, 0.21, 0.16,
  "eden", "2_3", 0.00, -0.20, 0.20,
  "elfe", "2_3", 0.01, -0.08, 0.09,
  "gecko", "2_3", 0.02, -0.26, 0.29,
  "genr", "2_3", 0.31, -0.03, 0.66,
  "inma", "2_3", -0.00, -0.28, 0.27,
  "moba", "2_3", -0.02, -0.18, 0.14,
  "ninfea", "2_3", -0.49, -0.98, 0.00,
  "raine", "2_3", 0.08, -0.53, 0.69,
  "rhea", "2_3", -0.02, -0.40, 0.35,
  "sws", "2_3", -0.07, -0.41, 0.26,
  "alspac", "4_7", 0.00, -0.06, 0.06,
  "bib", "4_7", -0.07, -0.22, 0.08, 
  "dnbc", "4_7", 0.01, -0.12, 0.13,
  "eden", "4_7", 0.09, -0.12, 0.29,
  "elfe", "4_7", -0.09, -0.17, -0.00,
  "gecko", "4_7", 0.07, -0.19, 0.32,
  "genr", "4_7", 0.05, -0.23, 0.32,
  "inma", "4_7", -0.03, -0.28, 0.22,
  "moba", "4_7", 0.10, -0.05, 0.25,
  "ninfea", "4_7", -0.10, -0.24, 0.04,
  "raine", "4_7", 0.20, -0.12, 0.52,
  "rhea", "4_7", -0.18, -0.49, 0.12,
  "sws", "4_7", 0.20, -0.17, 0.57,
  "alspac", "8_13", -0.25, -0.72, 0.23,
  "bib", "8_13", 0.16, -0.10, 0.42,
  "dnbc", "8_13", 0.07, -0.05, 0.18,
  "eden", "8_13", 0.00, -0.29, 0.29,
  "elfe", "8_13", -0.00, -0.29, 0.29,
  "gecko", "8_13", 0.10, -0.20, 0.41,
  "genr", "8_13", 0.20, -0.10, 0.51,
  "inma", "8_13", 0.10, -0.26, 0.46,
  "moba", "8_13", 0.19, 0.01, 0.37,
  "ninfea", "8_13", 0.22, -0.11, 0.54,
  "raine", "8_13", 0.33, -0.05, 0.70,
  "rhea", "8_13", 0.27, -0.21, 0.74,
  "sws", "8_13", 0.15, -0.38, 0.68) %>%
  mutate(se = (uppci - lowci) / 3.92)

gdm_coh <- c("bib", "eden")

gdm_test <- gdm_sens %>% dplyr::filter(!cohort %in% gdm_coh)
gdm_no_test <- gdm_sens %>% dplyr::filter(cohort %in% gdm_coh)

metaGroup <- function(x){
  
x %>%
  group_by(age) %>%
  group_split %>%
  map(function(x){
    
    meta <- rma.uni(yi = x$est, sei = x$se)
    out <- tibble(
      est = meta$beta[1, 1],
      lowci = meta$ci.lb,
      uppci = meta$ci.ub)
    
    return(out)
    
  }) 
}

ages <- c("0_1", "2_3", "4_7", "8_13")

gdm_test.pdata <- metaGroup(gdm_test) %>% 
  set_names(ages) %>% 
  bind_rows(.id = "age") %>%
  mutate(type = "test")

gdm_no_test.pdata <- metaGroup(gdm_no_test) %>% 
  set_names(ages) %>% 
  bind_rows(.id = "age") %>%
  mutate(type = "no_test")

gdm_ns <- tibble(
  age = ages,
  exposed_tot = c(3105, 1384, 2148, 1120),
  unexposed_tot = c(165422, 66498, 117082, 92925),
  exposed_test = c(428, 268, 350, 187),
  unexposed_test = c(3287, 2287, 2580, 1518),
  exposed_no_test = exposed_tot - exposed_test,
  unexposed_no_test = unexposed_tot - unexposed_test)

gdm_sens.pdata <- bind_rows(gdm_test.pdata, gdm_no_test.pdata) %>%
  mutate(
    exposed = c(428, 268, 350, 187, 2677, 1116, 1798, 933),
    unexposed = c(3287, 2287, 2580, 1518, 162135, 64211, 114502, 91407))


################################################################################
# REVISED MAIN PLOTS  
################################################################################
## ---- Combined Ns ------------------------------------------------------------
ns_comb_sel <- sens_cat %>%
  set_names(sens_n.ref$variable) %>%
  
  map(function(x){
    
    x$output.list$TABLES.COMBINED_all.sources_counts %>%
      as_tibble(rownames = "category") %>%
      dplyr::select(category, "1") %>%
      dplyr::rename(count = "1")
  }) %>%
  bind_rows(.id = "variable") %>%
  mutate(cohort = "combined") %>%
  separate(
    col = variable, 
    sep = "_zscores.", 
    into = c("exposure", "age")) %>%
  mutate(age = str_remove(age, pattern = "_s")) %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig_days),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  dplyr::filter(category != "NA") %>%
  mutate(variable = case_when(
    exposure == "edu_m" & category == "1" ~ "High education (ref)",
    exposure == "edu_m" & category == "2" ~ "Medium education (ref = high)",
    exposure == "edu_m" & category == "3" ~ "Low education (ref = high)",
    exposure == "a_d_s" & category == "1" ~ "Low deprivation (ref)",
    exposure == "a_d_s" & category == "2" ~ "Medium deprivation (ref = low)",
    exposure == "a_d_s" & category == "3" ~ "High deprivation (ref = low)", 
    exposure == "p_d_s" & category == "0" ~ "No GDM (ref)",
    exposure == "p_d_s" & category == "1" ~ "preg_dia1")) 

################################################################################
# Maternal education  
################################################################################

## ---- Main results -----------------------------------------------------------
mat_ed_ipd_main.pdata <- mat_ed.fit$ipd %>%
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  mutate(type = "main") %>%
  prepCatPlotdata(
    lev_1_name = "edu_m2",
    lev_1_lab = "Medium education (ref = high)",
    lev_2_name = "edu_m3",
    lev_2_lab = "Low education (ref = high)") %>%
  left_join(., refN("High education (ref)"), by = "age") %>%
  left_join(., ns_cat_all %>% dplyr::filter(cohort == "combined"), 
            ord_cat_dum, by = c("age", "variable")) 
  
## ---- Sensitivity ------------------------------------------------------------
mat_ed_ipd_sens.pdata <- mat_ed_sel.fit %>%
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  mutate(type = "sens") %>%
  prepCatPlotdata(
    lev_1_name = "edu_m2",
    lev_1_lab = "Medium education (ref = high)",
    lev_2_name = "edu_m3",
    lev_2_lab = "Low education (ref = high)") %>%
  left_join(., refN("High education (ref)", ns_comb_sel), by = "age") %>%
  left_join(., ns_comb_sel, by = c("age", "variable"))

ns_cat_all %>% dplyr::filter(cohort == "combined") %>% print(n = Inf)

## ---- Combined ---------------------------------------------------------------
mat_ed_ipd.pdata <- bind_rows(mat_ed_ipd_main.pdata, mat_ed_ipd_sens.pdata) %>%
  arrange(variable, desc(age)) %>%
  mutate(age = ifelse(type == "sens", "", as.character(age)))

################################################################################
# Area deprivation  
################################################################################

## ---- Main results -----------------------------------------------------------
area_dep_ipd_main.pdata <- area_dep.fit$ipd %>%
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  mutate(type = "main") %>%
  prepCatPlotdata(
    lev_1_name = "area_dep2",
    lev_1_lab = "Medium deprivation (ref = low)",
    lev_2_name = "area_dep3",
    lev_2_lab = "High deprivation (ref = low)") %>%
  left_join(., refN("High education (ref)"), by = "age") %>%
  left_join(., ns_cat_all %>% dplyr::filter(cohort == "combined"), 
            ord_cat_dum, by = c("age", "variable")) 

## ---- Sensitivity ------------------------------------------------------------
area_dep_ipd_sens.pdata <- area_dep_sel.fit %>%
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  mutate(type = "sens") %>%
  prepCatPlotdata(
    lev_1_name = "area_dep2",
    lev_1_lab = "Medium deprivation (ref = low)",
    lev_2_name = "area_dep3",
    lev_2_lab = "High deprivation (ref = low)") %>%
  left_join(., refN("High education (ref)", ns_comb_sel), by = "age") %>%
  left_join(., ns_comb_sel, by = c("age", "variable"))

area_dep_ipd.pdata <- bind_rows(area_dep_ipd_main.pdata, area_dep_ipd_sens.pdata) %>%
  arrange(variable, desc(age)) %>%
  mutate(age = ifelse(type == "sens", "", as.character(age)))

################################################################################
# NDVI  
################################################################################

## ---- Main results -----------------------------------------------------------
ndvi_ipd_main.pdata <- ndvi.fit$ipd %>% 
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "ndvi300_preg_iqr_c") %>%
  mutate(type = "main")

## ---- Sensitivity ------------------------------------------------------------
ndvi_ipd_sens.pdata <- ndvi_sel.fit %>% 
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "ndvi300_preg_iqr_c") %>%
  mutate(type = "sens")
  
ndvi_ipd.pdata <- bind_rows(ndvi_ipd_main.pdata, ndvi_ipd_sens.pdata) %>%  
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  mutate(variable = "NDVI") %>%
  arrange(desc(age), type) %>%
  mutate(age = ifelse(type == "sens", "", as.character(age)))


################################################################################
# Pregnancy diabetes  
################################################################################

## ---- Main results -----------------------------------------------------------
preg_dia_main.pdata <- preg_dia.fit$ipd %>%
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "preg_dia1") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  left_join(., refN("preg_dia1"), by = "age") %>%
  left_join(., ns_cat_all %>% dplyr::filter(cohort == "combined"), 
            ord_cat_dum, by = c("age", "variable")) %>%
  mutate(type = "main")

## ---- Sensitivity ------------------------------------------------------------
preg_dia_sens.pdata <- preg_dia_sel.fit %>%
  map(dh.lmTab, type = "glm_ipd", ci_format = "separate", direction = "wide") %>%
  bind_rows(.id = "age") %>%
  dplyr::filter(variable == "preg_dia1") %>%
  mutate(
    age = factor(
      age, 
      levels = rev(age_orig),
      labels = rev(age_new),
      ordered = TRUE)) %>%
  left_join(., refN("preg_dia1", ns_comb_sel), by = "age", ) %>%
  left_join(., ns_comb_sel, by = c("age", "variable")) %>%
  mutate(type = "sens")

preg_dia.pdata <- bind_rows(preg_dia_main.pdata, preg_dia_sens.pdata) %>%
  arrange(desc(age), type) %>%
  mutate(age = ifelse(type == "sens", "", as.character(age)))


################################################################################
# Plots  
################################################################################
################################################################################
# Maternal education  
################################################################################
png(
  file = here("figures", "mat_ed_ipd_sel.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = mat_ed_ipd.pdata %>% pull(est), 
  ci.lb = mat_ed_ipd.pdata %>% pull(lowci),
  ci.ub = mat_ed_ipd.pdata %>% pull(uppci), 
  slab = mat_ed_ipd.pdata %>% pull(age), 
  xlab = "Difference in childhood BMI by category of maternal education", 
  ilab =  cbind(
    mat_ed_ipd.pdata %>% pull(ref),
    mat_ed_ipd.pdata %>% pull(count),
    c(rbind(
      unlist(mat_ed.fit[[1]] %>% map(function(x){length(x$disclosure.risk)})), 
      unlist(mat_ed_sel.fit %>% map(function(x){length(x$disclosure.risk)}))))), 
  ilab.xpos = c(-2.5, -1.6, -0.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 3),
  ylim = c(1, 34),
  alim = c(-0.5, 1.5), 
  steps = 5, 
  digits = c(2, 2), 
  rows = c(13, 12, 10.5, 9.5, 8, 7, 5.5, 4.5, 3, 2, 28.5, 27.5, 26, 25, 23.5, 
           22.5, 21, 20, 18.5, 17.5), 
  psize = 0.8, 
  pch = rep(c(15, 0), 10))

text(-2.5, 33, "Ref (high)", cex = 0.8, font = 2)
text(-1.6, 33, "Exposed", cex = 0.8, font = 2)
text(-0.7, 33, "N studies", cex = 0.8, font = 2)
text(-2.05, 34.5, "N participants", cex = 0.8, font = 2)

text(-2.92, 30.3, "Medium education", cex = 0.8, font = 2)
text(-3.02, 14.8, "Low education", cex = 0.8, font = 2)

dev.off()

################################################################################
# Area deprivation  
################################################################################
png(
  file = here("figures", "area_dep_ipd_sel.png"), 
  width = word_full, 
  height = 15, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = area_dep_ipd.pdata %>% pull(est), 
  ci.lb = area_dep_ipd.pdata %>% pull(lowci),
  ci.ub = area_dep_ipd.pdata %>% pull(uppci), 
  slab = area_dep_ipd.pdata %>% pull(age), 
  xlab = "Difference in childhood BMI by category of maternal education", 
  ilab =  cbind(
    area_dep_ipd.pdata %>% pull(ref),
    area_dep_ipd.pdata %>% pull(count),
    c(rbind(
      unlist(area_dep.fit[[1]] %>% map(function(x){length(x$disclosure.risk)})), 
      unlist(area_dep_sel.fit %>% map(function(x){length(x$disclosure.risk)}))))), 
  ilab.xpos = c(-2.5, -1.6, -0.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-3.5, 3),
  ylim = c(1, 34),
  alim = c(-0.5, 1.5), 
  steps = 5, 
  digits = c(2, 2), 
  rows = c(13, 12, 10.5, 9.5, 8, 7, 5.5, 4.5, 3, 2, 28.5, 27.5, 26, 25, 23.5, 
           22.5, 21, 20, 18.5, 17.5), 
  psize = 0.8, 
  pch = rep(c(15, 0), 10))

text(-2.5, 33, "Ref (high)", cex = 0.8, font = 2)
text(-1.6, 33, "Exposed", cex = 0.8, font = 2)
text(-0.7, 33, "N studies", cex = 0.8, font = 2)
text(-2.05, 34.5, "N participants", cex = 0.8, font = 2)

text(-2.88, 30.3, "Medium deprivation", cex = 0.8, font = 2)
text(-2.96, 14.8, "High deprivation", cex = 0.8, font = 2)

dev.off()

################################################################################
# NDVI  
################################################################################
png(
  file = here("figures", "ndvi_ipd_sel.png"), 
  width = word_full, 
  height = 10, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = ndvi_ipd.pdata %>% pull(est), 
  ci.lb = ndvi_ipd.pdata %>% pull(lowci),
  ci.ub = ndvi_ipd.pdata %>% pull(uppci), 
  slab = ndvi_ipd.pdata %>% pull(age), 
  xlab = "Difference in childhood BMI by interquartile change NDVI", 
  ilab =  cbind(
    c(rbind(
      ndvi.fit[[1]] %>% map_int(function(x){x$Nvalid}), 
      ndvi_sel.fit %>% map_int(function(x){x$Nvalid}))),
    c(rbind(
      unlist(ndvi.fit[[1]] %>% map(function(x){length(x$disclosure.risk)})),
      unlist(ndvi_sel.fit %>% map(function(x){length(x$disclosure.risk)}))))),
  ilab.xpos = c(-2.5, -1.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-4.0, 2.5),
  ylim = c(13, 24),
  alim = c(-1.5, 1), 
  steps = 6, 
  digits = c(2, 2), 
  rows = c(21, 20.2, 19.2, 18.4, 17.4, 16.6, 15.6, 14.8, 13.8, 13), 
  psize = 0.8, 
  pch = rep(c(15, 0), 5))

text(-2.5, 24, "N participants", cex = 0.8, font = 2)
text(-1.7, 24, "N studies", cex = 0.8, font = 2)

dev.off()

################################################################################
# Pregnancy diabetes  
################################################################################
png(
  file = here("figures", "preg_dia_ipd_sens.png"), 
  width = word_full, 
  height = 10, 
  units = "cm",
  res = 300)

par(mar=c(5,0,0,0))

forest(
  x = preg_dia.pdata %>% pull(est), 
  ci.lb = preg_dia.pdata %>% pull(lowci),
  ci.ub = preg_dia.pdata %>% pull(uppci), 
  slab = preg_dia.pdata %>% pull(age), 
  xlab = "Difference in childhood BMI where pregnancy diabetes reported", 
  ilab =  cbind(
    preg_dia.pdata %>% pull(ref),
    preg_dia.pdata %>% pull(count),
    c(rbind(
      preg_dia.fit$ipd %>% map(function(x){length(x$disclosure.risk)}), 
      preg_dia_sel.fit %>% map(function(x){length(x$disclosure.risk)})))),
  ilab.xpos = c(-2.5, -1.6, -0.7),
  cex = 0.8, 
  cex.axis = 0.8,
  header = c("Age (months)", "Estimate [95% CI]"), 
  refline = 0, 
  xlim = c(-4.0, 2.5),
  ylim = c(8, 20),
  alim = c(-0.5, 1), 
  steps = 4, 
  digits = c(2, 2), 
  rows = c(17, 16, 15, 14, 13, 12, 11, 10, 9, 8), 
  psize = 0.8, 
  pch = rep(c(15, 0), 5))

text(-2.5, 19, "Unexposed", cex = 0.8, font = 2)
text(-1.6, 19, "Exposed", cex = 0.8, font = 2)
text(-0.7, 19, "N studies", cex = 0.8, font = 2)
text(-2.05, 20, "N participants", cex = 0.8, font = 2)

dev.off()







