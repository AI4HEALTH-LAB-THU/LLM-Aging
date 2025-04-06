# install.packages("BiocManager")
# install.packages("RSQLite")
# install.packages("ppcor")
# library(BiocManager)
# BiocManager::install("enrichplot")
# BiocManager::install("GEOquery")



######### ------ The LLM effectively predicts overall and organ-specific ages ------
library(arrow)
library(jsonlite)
library(survival)
library(survminer)
library(survcomp)
library(gridExtra)
library(pROC)
library(Hmisc)
library(rms)
library(hexbin)
library(caret)
library(scales)
library(grid)
library(ppcor)
library(tidyverse)
library(lubridate)
library(svglite)
###### prepare data, UKB
dat_age <- read_csv("Data/Models/llama3_70b/llama3-70b-result_only_age.csv")
dat_cov <- read_rds("Data/covariates_outcomes/panel_indicators.rds")
dat_outcome <- read_rds("Data/covariates_outcomes/overall_aging_outcomes.rds")

### merge data
dat_age <- dat_age %>% inner_join(dat_cov, by = "eid")
dat_age <- dat_age %>% inner_join(dat_outcome, by = "eid")

# calculate age gap
dat_age <- dat_age %>% mutate(llm_overall_acc = llm_overall_age - Age)
dat_age <- dat_age %>% mutate(llm_cardiovascular_acc = llm_cardiovascular_age - Age)
dat_age <- dat_age %>% mutate(llm_hepatic_acc = llm_hepatic_age - Age)
dat_age <- dat_age %>% mutate(llm_pulmonary_acc = llm_pulmonary_age - Age)
dat_age <- dat_age %>% mutate(llm_renal_acc = llm_renal_age - Age)
dat_age <- dat_age %>% mutate(llm_metabolic_acc = llm_metabolic_age - Age)
dat_age <- dat_age %>% mutate(llm_musculoskeletal_acc = llm_musculoskeletal_age - Age)
dat_age <- na.omit(dat_age)


###### 1-1.compare C-index on aging-related health outcomes, UKB
dat_telomere <- read_csv("Data/covariates_outcomes/telomere.csv")
dat_fi <- read_rds("Data/covariates_outcomes/frailty_index_52.rds")
dat_age <- dat_age %>% inner_join(dat_fi, by = "eid")
dat_telomere <- dplyr::select(dat_telomere, 1:2, 5)
names(dat_telomere)[c(2, 3)] <- c("telomere_adjusted", "z_adjusted_telomere")
dat_telomere <- na.omit(dat_telomere)
dat_age <- dat_age %>% inner_join(dat_telomere, by = "eid")

# Other sota machine learning models
dat_svm <- read_csv("Data/Models/svm_res/test_svm_overall_res_250105.csv")
dat_rf <- read_csv("Data/Models/rf_res/test_rf_overall_res_250105.csv")
dat_xgboost <- read_csv("Data/Models/xgboost_res/test_xgboost_overall_res_250105.csv")
dat_dnn <- read_csv("Data/Models/dnn_res/test_dnn_overall_res_250105.csv")

dat_svm$svm_overall_age <- round(dat_svm$svm_overall_age, digits = 0)
dat_rf$rf_overall_age <- round(dat_rf$rf_overall_age, digits = 0)
dat_xgboost$xgboost_overall_age <- round(dat_xgboost$xgboost_overall_age, digits = 0)
dat_dnn$dnn_overall_age <- round(dat_dnn$dnn_overall_age, digits = 0)

dat_svm$svm_age_gap <- dat_svm$svm_overall_age - dat_svm$Age
dat_rf$rf_age_gap <- dat_rf$rf_overall_age - dat_rf$Age
dat_xgboost$xgboost_age_gap <- dat_xgboost$xgboost_overall_age - dat_xgboost$Age
dat_dnn$dnn_age_gap <- dat_dnn$dnn_overall_age - dat_dnn$Age

### calculate adjusted age gap
# svm
model <- lm(svm_age_gap ~ Age, data = dat_svm)
predicted_gap <- predict(model, newdata = dat_svm)
dat_svm$adj_svm_overall_acc <- dat_svm$svm_age_gap - predicted_gap
dat_svm$adj_svm_overall_acc <- round(dat_svm$adj_svm_overall_acc, digits = 0)
# # Or the code could be written as follows, and the result would be the same
# model <- lm(svm_overall_age ~ Age, data = dat_svm)
# predicted_age <- predict(model, newdata = dat_svm)
# dat_svm$adj_svm_overall_acc <- dat_svm$svm_overall_age - predicted_age
# dat_svm$adj_svm_overall_acc <- round(dat_svm$adj_svm_overall_acc, digits = 0)

# random forest
model <- lm(rf_age_gap ~ Age, data = dat_rf)
predicted_gap <- predict(model, newdata = dat_rf)
dat_rf$adj_rf_overall_acc <- dat_rf$rf_age_gap - predicted_gap
dat_rf$adj_rf_overall_acc <- round(dat_rf$adj_rf_overall_acc, digits = 0)

# xgboost
model <- lm(xgboost_age_gap ~ Age, data = dat_xgboost)
predicted_gap <- predict(model, newdata = dat_xgboost)
dat_xgboost$adj_xgboost_overall_acc <- dat_xgboost$xgboost_age_gap - predicted_gap
dat_xgboost$adj_xgboost_overall_acc <- round(dat_xgboost$adj_xgboost_overall_acc, digits = 0)

# dnn
model <- lm(dnn_age_gap ~ Age, data = dat_dnn)
predicted_gap <- predict(model, newdata = dat_dnn)
dat_dnn$adj_dnn_overall_acc <- dat_dnn$dnn_age_gap - predicted_gap
dat_dnn$adj_dnn_overall_acc <- round(dat_dnn$adj_dnn_overall_acc, digits = 0)

dat_svm$Age <- NULL
dat_rf$Age <- NULL
dat_xgboost$Age <- NULL
dat_dnn$Age <- NULL

dat_svm$svm_age_gap <- NULL
dat_rf$rf_age_gap <- NULL
dat_xgboost$xgboost_age_gap <- NULL
dat_dnn$dnn_age_gap <- NULL

### merge and validate on the test set
dat_age <- dat_age %>%
  inner_join(dat_svm, by = "eid") %>%
  inner_join(dat_rf, by = "eid") %>%
  inner_join(dat_xgboost, by = "eid") %>%
  inner_join(dat_dnn, by = "eid")

### define the interest of outcomes
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")
### define the variables
var_ls <- c("telomere_adjusted", "frailty_index", "Age", "svm_overall_age",
            "rf_overall_age", "xgboost_overall_age", 
            "dnn_overall_age", "llm_overall_age",
            "adj_svm_overall_acc", "adj_rf_overall_acc", 
            "adj_xgboost_overall_acc", 
            "adj_dnn_overall_acc", "llm_overall_acc")

### run the procedures
var_mean_c_index <- c()
var_mean_c_index_lower <- c()
var_mean_c_index_upper <- c()
outcome_ls <- c()

set.seed(2024)
for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]
  
  # only include disease-free participants
  if (item == "CHD" | item == "Stroke") {
    dat_cox <- subset(dat_age, `MACE duration` > 0)
  }
  else if (item == "Renal failure") {
    dat_cox <- subset(dat_age, `Renal diseases duration` > 0)
  }
  else if (item == "T2D") {
    dat_cox <- subset(dat_age, `Diabetes duration` > 0)
  }
  else {
    dat_cox <- subset(dat_age, time > 0) 
  }
  
  folds <- createFolds(dat_cox$event, k = 3)
  for (i in 1:length(var_ls)) {
    var <- var_ls[i]
    c_index_values <- c()
    c_index_lower_ls <- c()
    c_index_upper_ls <- c()
    
    for(j in 1:3) {
      # train and test set
      test_indices <- folds[[j]]
      train_data <- dat_cox[-test_indices, ]
      test_data <- dat_cox[test_indices, ]
      
      # Cox models
      formula_covariates <- paste0("survobj ~ ", var)
      f <- as.formula(formula_covariates)
      survobj <- with(train_data, Surv(time, event))
      cox_fit <- coxph(formula = f, data = train_data, na.action = na.omit)
      
      # predict the risk score
      test_data$predicted_risk <- predict(cox_fit, newdata = test_data, 
                                          type = "risk")
      
      # calculate the c-index
      concordance_result <- concordance.index(x = test_data$predicted_risk,
                                              surv.time = test_data$time,
                                              surv.event = test_data$event)
      c_index <- concordance_result$c.index
      c_index_lower <- concordance_result$lower
      c_index_upper <- concordance_result$upper
      # save the c-index
      c_index_values <- c(c_index_values, c_index)
      c_index_lower_ls <- c(c_index_lower_ls, c_index_lower)
      c_index_upper_ls <- c(c_index_upper_ls, c_index_upper)
      print(paste0(item, " ------------ ", var, " ------------ fold ", j))
    }
    mean_c_index <- round(mean(c_index_values), digits = 3)
    mean_c_index_lower <- round(mean(c_index_lower_ls), digits = 3)
    mean_c_index_upper <- round(mean(c_index_upper_ls), digits = 3)
    
    var_mean_c_index <- c(var_mean_c_index, mean_c_index)
    var_mean_c_index_lower <- c(var_mean_c_index_lower, mean_c_index_lower)
    var_mean_c_index_upper <- c(var_mean_c_index_upper, mean_c_index_upper)
    outcome_ls <- c(outcome_ls, item)
  }
}

dat_plot <- data.frame(
  outcome = outcome_ls,
  var_name = var_ls,
  c_index = var_mean_c_index,
  c_index_lower = var_mean_c_index_lower,
  c_index_upper = var_mean_c_index_upper
)

dat_plot <- dat_plot %>%
  mutate(var_name = case_when(
    var_name == "telomere_adjusted" ~ "Telomere",
    var_name == "frailty_index" ~ "Frailty index",
    var_name == "Age" ~ "Chronological age",
    var_name == "svm_overall_age" ~ "SVM overall age",
    var_name == "rf_overall_age" ~ "RF overall age",
    var_name == "xgboost_overall_age" ~ "XGBoost overall age",
    var_name == "dnn_overall_age" ~ "DNN overall age",
    var_name == "llm_overall_age" ~ "LLM overall age",
    var_name == "adj_svm_overall_acc" ~ "SVM overall age gap",
    var_name == "adj_rf_overall_acc" ~ "RF overall age gap",
    var_name == "adj_xgboost_overall_acc" ~ "XGBoost overall age gap",
    var_name == "adj_dnn_overall_acc" ~ "DNN overall age gap",
    var_name == "llm_overall_acc" ~ "LLM overall age gap"
  ))

### plot
dat_plot_backup <- dat_plot
# age
dat_plot <- subset(dat_plot, (grepl("overall age", var_name) & 
                                !grepl("gap", var_name)) |
                     grepl("Frailty index", var_name) |
                     grepl("Telomere", var_name) |
                     grepl("Chronological age", var_name))

dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c("Telomere", 
                                       "Frailty index", 
                                       "Chronological age", 
                                       "SVM overall age",
                                       "RF overall age",
                                       "XGBoost overall age",
                                       "DNN overall age",
                                       "LLM overall age"))

plots_c_index <- list()
disease <- unique(dat_plot$outcome)
var_name_ls <- as.character(unique(dat_plot$var_name))

# main fig
for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(dat_plot, outcome == item)
  p <- ggplot(dat_sub, aes(x = var_name, y = c_index, color = var_name)) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = c_index_lower, ymax = c_index_upper), width = 0.2) +
    geom_segment(aes(x = 0, xend = var_name, y = c_index, yend = c_index),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% var_name_ls)) +
    scale_color_manual(values = c("#878787", "#f4a582", "#fee08b", "#e4d1d1",
                                  "#b9b0b0", "#d9ecd0", "#77a8a8", "#4480B3")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 22),
          axis.text.x = element_text(angle = 90, size = 22, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 22, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
  
  if (i < 5) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.line.x = element_blank())
  }
  plots_c_index[[i]] <- p
}

arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 4, 
                            nrow = 2,
                            heights = c(1, 1.8))

combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Absolute C-index", size = 22, rot = 90))

ggsave("fig2b_overall_aging_proxy_cindex.pdf", plot = combined_plot, width = 20, height = 10)

# acc
dat_plot <- dat_plot_backup
dat_plot <- subset(dat_plot, grepl("overall age gap", var_name) |
                     grepl("Frailty index", var_name) |
                     grepl("Telomere", var_name))

dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c("Telomere", 
                                       "Frailty index", 
                                       "SVM overall age gap",
                                       "RF overall age gap",
                                       "XGBoost overall age gap",
                                       "DNN overall age gap",
                                       "LLM overall age gap"))

plots_c_index <- list()
disease <- unique(dat_plot$outcome)
var_name_ls <- as.character(unique(dat_plot$var_name))

# extended data fig
for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(dat_plot, outcome == item)
  p <- ggplot(dat_sub, aes(x = var_name, y = c_index, color = var_name)) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = c_index_lower, ymax = c_index_upper), width = 0.2) +
    geom_segment(aes(x = 0, xend = var_name, y = c_index, yend = c_index),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% var_name_ls)) +
    scale_color_manual(values = c("#878787", "#f4a582", "#e4d1d1",
                                  "#b9b0b0", "#d9ecd0", "#77a8a8", "#4480B3")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 22),
          axis.text.x = element_text(angle = 90, size = 22, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 22, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
  
  if (i < 5) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.line.x = element_blank())
  }
  plots_c_index[[i]] <- p
}

arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 4, 
                            nrow = 2,
                            heights = c(1, 2.05))

combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Absolute C-index", size = 22, rot = 90))

ggsave("extended_fig3a_overall_acc_cindex.pdf", plot = combined_plot, width = 20, height = 10)




###### 1-2.validation on NHANES, compare C-index
# install.packages("haven")
library(haven)
library(tidyverse)
library(survival)

### 1.read the DNA Methylation data, 1999-2002
sas_data <- read_sas("NHANES/DNAM/dnmepi.sas7bdat")

### 2.read the mortality data
srvyin_1 <- paste("NHANES/Mortality/NHANES_1999_2000_MORT_2019_PUBLIC.dat")
srvyout_1 <- "NHANES_1999_2000"  
# read in the fixed-width format ASCII file
dsn_1 <- read_fwf(file=srvyin_1,
                  col_types = "iiiiiiii",
                  fwf_cols(seqn = c(1,6),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = c("", ".")
)
names(dsn_1)[1] <- "SEQN"
# select Eligibility Status for mortality follow-up
dsn_1 <- subset(dsn_1, eligstat == 1)

srvyin_2 <- paste("NHANES/Mortality/NHANES_2001_2002_MORT_2019_PUBLIC.dat")
srvyout_2 <- "NHANES_2001_2002"  
# read in the fixed-width format ASCII file
dsn_2 <- read_fwf(file=srvyin_2,
                  col_types = "iiiiiiii",
                  fwf_cols(seqn = c(1,6),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = c("", ".")
)
names(dsn_2)[1] <- "SEQN"
# select Eligibility Status for mortality follow-up
dsn_2 <- subset(dsn_2, eligstat == 1)
## merge data
dsn <- rbind(dsn_1, dsn_2)

###### 3.read the demographic data
dat_demo_1 <- read_xpt("NHANES/1999-2000/Demographic/DEMO.xpt")
dat_demo_2 <- read_xpt("NHANES/2001-2002/Demographic/DEMO_B.xpt")
### select variable
dat_demo_1 <- select(dat_demo_1, 1, 6:8, 5)
dat_demo_2 <- select(dat_demo_2, 1, 6:8, 5)
dat_demo <- rbind(dat_demo_1, dat_demo_2)

sas_data <- subset(sas_data, !is.na(HorvathAge))

# dat_test <- dat_demo %>% inner_join(dsn, by = "SEQN")

### merge data
dat_merge <- sas_data %>%
  left_join(dat_demo, by = "SEQN") %>%
  left_join(dsn, by = "SEQN")

names(dat_merge)[33] <- "Age"

dat_merge <- subset(dat_merge, Age >= 50 & Age <= 75)

### read llm results
dat_llm <- read_csv("NHANES/nhanes_res/dat_nhanes.csv")
dat_llm <- subset(dat_llm, !grepl("Due to the lack of", `inference process`))
dat_llm$`inference process` <- NULL
dat_llm <- na.omit(dat_llm)
names(dat_llm) <- c("SEQN", "llm_overall_age")

dat_merge_analysis <- dat_merge %>% inner_join(dat_llm, by = "SEQN")

# calculate age gap
dat_merge_analysis <- dat_merge_analysis %>%
  mutate(HorvathAge_acc = HorvathAge - Age) %>%
  mutate(HannumAge_acc = HannumAge - Age) %>%
  mutate(SkinBloodAge_acc = SkinBloodAge - Age) %>%
  mutate(PhenoAge_acc = PhenoAge - Age) %>%
  mutate(ZhangAge_acc = ZhangAge - Age) %>%
  mutate(LinAge_acc = LinAge - Age) %>%
  mutate(WeidnerAge_acc = WeidnerAge - Age) %>%
  mutate(VidalBraloAge_acc = VidalBraloAge - Age) %>%
  mutate(llm_overall_acc = llm_overall_age - Age)

# HorvathAge_acc
model <- lm(HorvathAge_acc ~ Age, data = dat_merge_analysis)
predicted_gap <- predict(model, newdata = dat_merge_analysis)
dat_merge_analysis$adj_HorvathAge_acc <- dat_merge_analysis$HorvathAge_acc - predicted_gap
dat_merge_analysis$adj_HorvathAge_acc <- round(dat_merge_analysis$adj_HorvathAge_acc, digits = 0)

# HannumAge_acc
model <- lm(HannumAge_acc ~ Age, data = dat_merge_analysis)
predicted_gap <- predict(model, newdata = dat_merge_analysis)
dat_merge_analysis$adj_HannumAge_acc <- dat_merge_analysis$HannumAge_acc - predicted_gap
dat_merge_analysis$adj_HannumAge_acc <- round(dat_merge_analysis$adj_HannumAge_acc, digits = 0)

# SkinBloodAge_acc
model <- lm(SkinBloodAge_acc ~ Age, data = dat_merge_analysis)
predicted_gap <- predict(model, newdata = dat_merge_analysis)
dat_merge_analysis$adj_SkinBloodAge_acc <- dat_merge_analysis$SkinBloodAge_acc - predicted_gap
dat_merge_analysis$adj_SkinBloodAge_acc <- round(dat_merge_analysis$adj_SkinBloodAge_acc, digits = 0)

# PhenoAge_acc
model <- lm(PhenoAge_acc ~ Age, data = dat_merge_analysis)
predicted_gap <- predict(model, newdata = dat_merge_analysis)
dat_merge_analysis$adj_PhenoAge_acc <- dat_merge_analysis$PhenoAge_acc - predicted_gap
dat_merge_analysis$adj_PhenoAge_acc <- round(dat_merge_analysis$adj_PhenoAge_acc, digits = 0)

# ZhangAge_acc
model <- lm(ZhangAge_acc ~ Age, data = dat_merge_analysis)
predicted_gap <- predict(model, newdata = dat_merge_analysis)
dat_merge_analysis$adj_ZhangAge_acc <- dat_merge_analysis$ZhangAge_acc - predicted_gap
dat_merge_analysis$adj_ZhangAge_acc <- round(dat_merge_analysis$adj_ZhangAge_acc, digits = 0)

# LinAge_acc
model <- lm(LinAge_acc ~ Age, data = dat_merge_analysis)
predicted_gap <- predict(model, newdata = dat_merge_analysis)
dat_merge_analysis$adj_LinAge_acc <- dat_merge_analysis$LinAge_acc - predicted_gap
dat_merge_analysis$adj_LinAge_acc <- round(dat_merge_analysis$adj_LinAge_acc, digits = 0)

# WeidnerAge_acc
model <- lm(WeidnerAge_acc ~ Age, data = dat_merge_analysis)
predicted_gap <- predict(model, newdata = dat_merge_analysis)
dat_merge_analysis$adj_WeidnerAge_acc <- dat_merge_analysis$WeidnerAge_acc - predicted_gap
dat_merge_analysis$adj_WeidnerAge_acc <- round(dat_merge_analysis$adj_WeidnerAge_acc, digits = 0)

# VidalBraloAge_acc
model <- lm(VidalBraloAge_acc ~ Age, data = dat_merge_analysis)
predicted_gap <- predict(model, newdata = dat_merge_analysis)
dat_merge_analysis$adj_VidalBraloAge_acc <- dat_merge_analysis$VidalBraloAge_acc - predicted_gap
dat_merge_analysis$adj_VidalBraloAge_acc <- round(dat_merge_analysis$adj_VidalBraloAge_acc, digits = 0)


dat_merge_analysis <- dat_merge_analysis %>%
  mutate(ucod_leading = case_when(
    is.na(ucod_leading) ~ 99,
    TRUE ~ ucod_leading
  )) %>%
  mutate(hyperten = case_when(
    is.na(hyperten) ~ 99,
    TRUE ~ hyperten
  )) %>%
  mutate(diabetes = case_when(
    is.na(diabetes) ~ 99,
    TRUE ~ diabetes
  ))

# table(dat_merge_analysis$ucod_leading)

### cause-specific death
dat_merge_analysis <- dat_merge_analysis %>%
  mutate(`All-cause death diagnose` = case_when(
    mortstat == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(`All-cause death duration` = permth_exm) %>%
  mutate(`Death of major NCDs diagnose` = case_when(
    mortstat == 1 & (ucod_leading == 1 | ucod_leading == 2 | ucod_leading == 3 | ucod_leading == 5 | ucod_leading == 6 | ucod_leading == 9 | hyperten == 1 | diabetes == 1) ~ 1,
    mortstat == 1 & !(ucod_leading == 1 | ucod_leading == 2 | ucod_leading == 3 | ucod_leading == 5 | ucod_leading == 6 | ucod_leading == 9 | hyperten == 1 | diabetes == 1) ~ 2,
    TRUE ~ 0
  )) %>%
  mutate(`Death of major NCDs duration` = permth_exm) %>%
  mutate(`Death of heart diseases diagnose` = case_when(
    mortstat == 1 & ucod_leading == 1 ~ 1,
    mortstat == 1 & ucod_leading != 1 ~ 2,
    TRUE ~ 0
  )) %>%
  mutate(`Death of heart diseases duration` = permth_exm) %>%
  mutate(`Death of hypertension diagnose` = case_when(
    mortstat == 1 & hyperten == 1 ~ 1,
    mortstat == 1 & hyperten == 0 ~ 2,
    TRUE ~ 0
  )) %>%
  mutate(`Death of hypertension duration` = permth_exm) %>%
  mutate(`Death of cancer diagnose` = case_when(
    mortstat == 1 & ucod_leading == 2 ~ 1,
    mortstat == 1 & ucod_leading != 2 ~ 2,
    TRUE ~ 0
  )) %>%
  mutate(`Death of cancer duration` = permth_exm) %>%
  mutate(`Death of diabetes diagnose` = case_when(
    mortstat == 1 & diabetes == 1 ~ 1,
    mortstat == 1 & diabetes == 0 ~ 2,
    TRUE ~ 0
  )) %>%
  mutate(`Death of diabetes duration` = permth_exm)

### cox models
library(arrow)
library(jsonlite)
library(survival)
library(survminer)
library(survcomp)
library(gridExtra)
library(pROC)
library(Hmisc)
library(rms)
library(hexbin)
library(caret)
library(scales)
library(grid)
library(ppcor)
library(tidyverse)
library(lubridate)
library(svglite)

var_ls <- c("Age",
            "HorvathAge",
            "HannumAge",
            "SkinBloodAge",
            "PhenoAge",
            "ZhangAge",
            "LinAge",
            "WeidnerAge",
            "VidalBraloAge",
            "llm_overall_age",
            "adj_HorvathAge_acc",
            "adj_HannumAge_acc",
            "adj_SkinBloodAge_acc",
            "adj_PhenoAge_acc",
            "adj_ZhangAge_acc",
            "adj_LinAge_acc",
            "adj_WeidnerAge_acc",
            "adj_VidalBraloAge_acc",
            "llm_overall_acc")

disease <- c("All-cause death", 
             "Death of heart diseases", 
             "Death of cancer", 
             "Death of diabetes")

var_mean_c_index <- c()
var_mean_c_index_lower <- c()
var_mean_c_index_upper <- c()
outcome_ls <- c()

set.seed(2024)
for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_merge_analysis$event <- dat_merge_analysis[[item_diagnose]]
  dat_merge_analysis$time <- dat_merge_analysis[[item_duration]]
  
  # cause-specific cox
  dat_cox <- dat_merge_analysis
  
  ## data is small, use all data; results are similar
  # folds <- createFolds(dat_cox$event, k = 3)
  for (i in 1:length(var_ls)) {
    var <- var_ls[i]
    c_index_values <- c()
    c_index_lower_ls <- c()
    c_index_upper_ls <- c()
    
    for(j in 1:1) {
      # test_indices <- folds[[j]]
      # train_data <- dat_cox[-test_indices, ]
      # test_data <- dat_cox[test_indices, ]
      train_data <- dat_cox
      test_data <- dat_cox
      
      # build cause-specific cox model
      formula_covariates <- paste0("survobj ~ ", var)
      f <- as.formula(formula_covariates)
      survobj <- with(train_data, Surv(time, event==1))
      cox_fit <- coxph(formula = f, data = train_data, na.action = na.omit)
      
      # predict risk score
      test_data$predicted_risk <- predict(cox_fit, newdata = test_data, 
                                          type = "risk")
      
      # calculate c-index
      concordance_result <- concordance.index(x = test_data$predicted_risk,
                                              surv.time = test_data$time,
                                              surv.event = test_data$event)
      c_index <- concordance_result$c.index
      c_index_lower <- concordance_result$lower
      c_index_upper <- concordance_result$upper
      
      # save c-index
      c_index_values <- c(c_index_values, c_index)
      c_index_lower_ls <- c(c_index_lower_ls, c_index_lower)
      c_index_upper_ls <- c(c_index_upper_ls, c_index_upper)
      print(paste0(item, " ------------ ", var, " ------------ fold ", j))
    }
    mean_c_index <- round(mean(c_index_values), digits = 3)
    mean_c_index_lower <- round(mean(c_index_lower_ls), digits = 3)
    mean_c_index_upper <- round(mean(c_index_upper_ls), digits = 3)
    
    var_mean_c_index <- c(var_mean_c_index, mean_c_index)
    var_mean_c_index_lower <- c(var_mean_c_index_lower, mean_c_index_lower)
    var_mean_c_index_upper <- c(var_mean_c_index_upper, mean_c_index_upper)
    outcome_ls <- c(outcome_ls, item)
  }
}

dat_plot <- data.frame(
  outcome = outcome_ls,
  var_name = var_ls,
  c_index = var_mean_c_index,
  c_index_lower = var_mean_c_index_lower,
  c_index_upper = var_mean_c_index_upper
)

dat_plot <- dat_plot %>%
  mutate(var_name = case_when(
    var_name == "Age" ~ "Chronological age",
    var_name == "HorvathAge" ~ "HorvathAge",
    var_name == "HannumAge" ~ "HannumAge",
    var_name == "SkinBloodAge" ~ "SkinBloodAge",
    var_name == "PhenoAge" ~ "PhenoAge", 
    var_name == "ZhangAge" ~ "ZhangAge",
    var_name == "LinAge" ~ "LinAge", 
    var_name == "WeidnerAge" ~ "WeidnerAge", 
    var_name == "VidalBraloAge" ~ "VidalBraloAge", 
    var_name == "llm_overall_age" ~ "LLM overall age",
    var_name == "adj_HorvathAge_acc" ~ "HorvathAge age gap", 
    var_name == "adj_HannumAge_acc" ~ "HannumAge age gap",
    var_name == "adj_SkinBloodAge_acc" ~ "SkinBloodAge age gap", 
    var_name == "adj_PhenoAge_acc" ~ "PhenoAge age gap",
    var_name == "adj_ZhangAge_acc" ~ "ZhangAge age gap",
    var_name == "adj_LinAge_acc" ~ "LinAge age gap",
    var_name == "adj_WeidnerAge_acc" ~ "WeidnerAge age gap", 
    var_name == "adj_VidalBraloAge_acc" ~ "VidalBraloAge age gap",
    var_name == "llm_overall_acc" ~ "LLM overall age gap"
  ))

# write_rds(dat_plot, "nhanes_compare_cindex_250107.rds")

### plot
# dat_plot <- read_rds("nhanes_compare_cindex_250107.rds")
dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c(
                              "Chronological age",
                              "HorvathAge",
                              "HannumAge",
                              "SkinBloodAge",
                              "PhenoAge",
                              "ZhangAge",
                              "LinAge",
                              "WeidnerAge",
                              "VidalBraloAge",
                              "LLM overall age",
                              "HorvathAge age gap",
                              "HannumAge age gap",
                              "SkinBloodAge age gap",
                              "PhenoAge age gap",
                              "ZhangAge age gap",
                              "LinAge age gap",
                              "WeidnerAge age gap",
                              "VidalBraloAge age gap",
                              "LLM overall age gap"
                            ))

### plot follow fig respectively
# age
dat_plot_age <- subset(dat_plot, !grepl("age gap", var_name))
# acc
dat_plot_age <- subset(dat_plot, grepl("age gap", var_name))

plots_c_index <- list()
disease <- c("All-cause death", 
             "Death of heart diseases", 
             "Death of cancer", 
             "Death of diabetes")

var_name_ls <- as.character(unique(dat_plot_age$var_name))

for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(dat_plot_age, outcome == item)
  p <- ggplot(dat_sub, aes(x = var_name, y = c_index, color = var_name)) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = c_index_lower, ymax = c_index_upper), width = 0.2) +
    geom_segment(aes(x = 0, xend = var_name, y = c_index, yend = c_index),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% var_name_ls)) +
    # # age
    # scale_color_manual(values = c("#fee08b", "#e06377",
    #                               "#f9ccac", "#f9d5e5",
    #                               "#e0876a", "#588c7e",
    #                               "#c2d4dd", "#b0aac0",
    #                               "#87bdd8", "#4480B3")) +
    # # acc
    scale_color_manual(values = c("#e06377",
                                  "#f9ccac", "#f9d5e5",
                                  "#e0876a", "#588c7e",
                                  "#c2d4dd", "#b0aac0",
                                  "#87bdd8", "#4480B3")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 22),
          axis.text.x = element_text(angle = 90, size = 22, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 22, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
  
  plots_c_index[[i]] <- p
}

arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 4, 
                            nrow = 1)
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Absolute C-index", 
                                                  size = 22, rot = 90))

# age
ggsave("fig2c_nhanes_age_cindex.pdf", plot = combined_plot, width = 19, height = 6)
# acc
ggsave("extended_fig3b_nhanes_acc_cindex.pdf", plot = combined_plot, width = 19, height = 6.7)



###### 1-3.validation on organ-specific ages
library(tidyverse)
library(arrow)
library(survival)
library(survminer)
library(survcomp)
library(caret)
library(gridExtra)
library(pROC)
library(Hmisc)
library(rms)
library(hexbin)
library(scales)
library(grid)
library(ppcor)
library(tidyverse)
library(lubridate)
library(svglite)

### read data
dat_llm <- read_csv("Data/Models/llama3_70b/llama3-70b-result_only_age.csv")

dat_svm_overall <- read_csv("Data/Models/svm_res/test_svm_overall_res_250105.csv")
dat_svm_cardiovascular <- read_csv("Data/Models/svm_res/test_svm_cardiovascular_res.csv")
dat_svm_hepatic <- read_csv("Data/Models/svm_res/test_svm_hepatic_res.csv")
dat_svm_pulmonary <- read_csv("Data/Models/svm_res/test_svm_pulmonary_res.csv")
dat_svm_musculoskeletal <- read_csv("Data/Models/svm_res/test_svm_musculoskeletal_res.csv")
dat_svm_renal <- read_csv("Data/Models/svm_res/test_svm_renal_res.csv")
dat_svm_metabolic <- read_csv("Data/Models/svm_res/test_svm_metabolic_res.csv")

dat_xgboost_overall <- read_csv("Data/Models/xgboost_res/test_xgboost_overall_res_250105.csv")
dat_xgboost_cardiovascular <- read_csv("Data/Models/xgboost_res/test_xgboost_cardiovascular_res.csv")
dat_xgboost_hepatic <- read_csv("Data/Models/xgboost_res/test_xgboost_hepatic_res.csv")
dat_xgboost_pulmonary <- read_csv("Data/Models/xgboost_res/test_xgboost_pulmonary_res.csv")
dat_xgboost_musculoskeletal <- read_csv("Data/Models/xgboost_res/test_xgboost_musculoskeletal_res.csv")
dat_xgboost_renal <- read_csv("Data/Models/xgboost_res/test_xgboost_renal_res.csv")
dat_xgboost_metabolic <- read_csv("Data/Models/xgboost_res/test_xgboost_metabolic_res.csv")

dat_dnn_overall <- read_csv("Data/Models/dnn_res/test_dnn_overall_res_250105.csv")
dat_dnn_cardiovascular <- read_csv("Data/Models/dnn_res/test_dnn_cardiovascular_res.csv")
dat_dnn_hepatic <- read_csv("Data/Models/dnn_res/test_dnn_hepatic_res.csv")
dat_dnn_pulmonary <- read_csv("Data/Models/dnn_res/test_dnn_pulmonary_res.csv")
dat_dnn_musculoskeletal <- read_csv("Data/Models/dnn_res/test_dnn_musculoskeletal_res.csv")
dat_dnn_renal <- read_csv("Data/Models/dnn_res/test_dnn_renal_res.csv")
dat_dnn_metabolic <- read_csv("Data/Models/dnn_res/test_dnn_metabolic_res.csv")

dat_rf_overall <- read_csv("Data/Models/rf_res/test_rf_overall_res_250105.csv")
dat_rf_cardiovascular <- read_csv("Data/Models/rf_res/test_rf_cardiovascular_res.csv")
dat_rf_hepatic <- read_csv("Data/Models/rf_res/test_rf_hepatic_res.csv")
dat_rf_pulmonary <- read_csv("Data/Models/rf_res/test_rf_pulmonary_res.csv")
dat_rf_musculoskeletal <- read_csv("Data/Models/rf_res/test_rf_musculoskeletal_res.csv")
dat_rf_renal <- read_csv("Data/Models/rf_res/test_rf_renal_res.csv")
dat_rf_metabolic <- read_csv("Data/Models/rf_res/test_rf_metabolic_res.csv")

dat_cov <- read_rds("Data/covariates_outcomes/panel_indicators.rds")
dat_outcome <- read_rds("Data/covariates_outcomes/organ_aging_outcomes.rds")

# preprocess data
dat_svm_overall$Age <- NULL
dat_svm_cardiovascular$Age <- NULL
dat_svm_hepatic$Age <- NULL
dat_svm_pulmonary$Age <- NULL
dat_svm_musculoskeletal$Age <- NULL
dat_svm_renal$Age <- NULL
dat_svm_metabolic$Age <- NULL

dat_svm_overall$svm_overall_age <- round(dat_svm_overall$svm_overall_age, digits = 0)
dat_svm_cardiovascular$svm_cardiovascular_age <- round(dat_svm_cardiovascular$svm_cardiovascular_age, digits = 0)
dat_svm_hepatic$svm_hepatic_age <- round(dat_svm_hepatic$svm_hepatic_age, digits = 0)
dat_svm_pulmonary$svm_pulmonary_age <- round(dat_svm_pulmonary$svm_pulmonary_age, digits = 0)
dat_svm_musculoskeletal$svm_musculoskeletal_age <- round(dat_svm_musculoskeletal$svm_musculoskeletal_age, digits = 0)
dat_svm_renal$svm_renal_age <- round(dat_svm_renal$svm_renal_age, digits = 0)
dat_svm_metabolic$svm_metabolic_age <- round(dat_svm_metabolic$svm_metabolic_age, digits = 0)

dat_dnn_overall$Age <- NULL
dat_dnn_cardiovascular$Age <- NULL
dat_dnn_hepatic$Age <- NULL
dat_dnn_pulmonary$Age <- NULL
dat_dnn_musculoskeletal$Age <- NULL
dat_dnn_renal$Age <- NULL
dat_dnn_metabolic$Age <- NULL

dat_dnn_overall$dnn_overall_age <- round(dat_dnn_overall$dnn_overall_age, digits = 0)
dat_dnn_cardiovascular$dnn_cardiovascular_age <- round(dat_dnn_cardiovascular$dnn_cardiovascular_age, digits = 0)
dat_dnn_hepatic$dnn_hepatic_age <- round(dat_dnn_hepatic$dnn_hepatic_age, digits = 0)
dat_dnn_pulmonary$dnn_pulmonary_age <- round(dat_dnn_pulmonary$dnn_pulmonary_age, digits = 0)
dat_dnn_musculoskeletal$dnn_musculoskeletal_age <- round(dat_dnn_musculoskeletal$dnn_musculoskeletal_age, digits = 0)
dat_dnn_renal$dnn_renal_age <- round(dat_dnn_renal$dnn_renal_age, digits = 0)
dat_dnn_metabolic$dnn_metabolic_age <- round(dat_dnn_metabolic$dnn_metabolic_age, digits = 0)

dat_xgboost_overall$Age <- NULL
dat_xgboost_cardiovascular$Age <- NULL
dat_xgboost_hepatic$Age <- NULL
dat_xgboost_pulmonary$Age <- NULL
dat_xgboost_musculoskeletal$Age <- NULL
dat_xgboost_renal$Age <- NULL
dat_xgboost_metabolic$Age <- NULL

dat_xgboost_overall$xgboost_overall_age <- round(dat_xgboost_overall$xgboost_overall_age, digits = 0)
dat_xgboost_cardiovascular$xgboost_cardiovascular_age <- round(dat_xgboost_cardiovascular$xgboost_cardiovascular_age, digits = 0)
dat_xgboost_hepatic$xgboost_hepatic_age <- round(dat_xgboost_hepatic$xgboost_hepatic_age, digits = 0)
dat_xgboost_pulmonary$xgboost_pulmonary_age <- round(dat_xgboost_pulmonary$xgboost_pulmonary_age, digits = 0)
dat_xgboost_musculoskeletal$xgboost_musculoskeletal_age <- round(dat_xgboost_musculoskeletal$xgboost_musculoskeletal_age, digits = 0)
dat_xgboost_renal$xgboost_renal_age <- round(dat_xgboost_renal$xgboost_renal_age, digits = 0)
dat_xgboost_metabolic$xgboost_metabolic_age <- round(dat_xgboost_metabolic$xgboost_metabolic_age, digits = 0)

dat_rf_overall$Age <- NULL
dat_rf_cardiovascular$Age <- NULL
dat_rf_hepatic$Age <- NULL
dat_rf_pulmonary$Age <- NULL
dat_rf_musculoskeletal$Age <- NULL
dat_rf_renal$Age <- NULL
dat_rf_metabolic$Age <- NULL

dat_rf_overall$rf_overall_age <- round(dat_rf_overall$rf_overall_age, digits = 0)
dat_rf_cardiovascular$rf_cardiovascular_age <- round(dat_rf_cardiovascular$rf_cardiovascular_age, digits = 0)
dat_rf_hepatic$rf_hepatic_age <- round(dat_rf_hepatic$rf_hepatic_age, digits = 0)
dat_rf_pulmonary$rf_pulmonary_age <- round(dat_rf_pulmonary$rf_pulmonary_age, digits = 0)
dat_rf_musculoskeletal$rf_musculoskeletal_age <- round(dat_rf_musculoskeletal$rf_musculoskeletal_age, digits = 0)
dat_rf_renal$rf_renal_age <- round(dat_rf_renal$rf_renal_age, digits = 0)
dat_rf_metabolic$rf_metabolic_age <- round(dat_rf_metabolic$rf_metabolic_age, digits = 0)

# merge
dat_age <- dat_svm_overall %>%
  inner_join(dat_svm_cardiovascular, by = "eid") %>%
  inner_join(dat_svm_hepatic, by = "eid") %>%
  inner_join(dat_svm_pulmonary, by = "eid") %>%
  inner_join(dat_svm_renal, by = "eid") %>%
  inner_join(dat_svm_metabolic, by = "eid") %>%
  inner_join(dat_svm_musculoskeletal, by = "eid") %>%
  inner_join(dat_xgboost_overall, by = "eid") %>%
  inner_join(dat_xgboost_cardiovascular, by = "eid") %>%
  inner_join(dat_xgboost_hepatic, by = "eid") %>%
  inner_join(dat_xgboost_pulmonary, by = "eid") %>%
  inner_join(dat_xgboost_renal, by = "eid") %>%
  inner_join(dat_xgboost_metabolic, by = "eid") %>%
  inner_join(dat_xgboost_musculoskeletal, by = "eid") %>%
  inner_join(dat_dnn_overall, by = "eid") %>%
  inner_join(dat_dnn_cardiovascular, by = "eid") %>%
  inner_join(dat_dnn_hepatic, by = "eid") %>%
  inner_join(dat_dnn_pulmonary, by = "eid") %>%
  inner_join(dat_dnn_renal, by = "eid") %>%
  inner_join(dat_dnn_metabolic, by = "eid") %>%
  inner_join(dat_dnn_musculoskeletal, by = "eid") %>%
  inner_join(dat_rf_overall, by = "eid") %>%
  inner_join(dat_rf_cardiovascular, by = "eid") %>%
  inner_join(dat_rf_hepatic, by = "eid") %>%
  inner_join(dat_rf_pulmonary, by = "eid") %>%
  inner_join(dat_rf_renal, by = "eid") %>%
  inner_join(dat_rf_metabolic, by = "eid") %>%
  inner_join(dat_rf_musculoskeletal, by = "eid") %>%
  inner_join(dat_llm, by = "eid") %>%
  inner_join(dat_cov, by = "eid") %>%
  inner_join(dat_outcome, by = "eid")

# original age gap
dat_age <- dat_age %>% 
  mutate(svm_overall_acc = svm_overall_age - Age) %>%
  mutate(svm_cardiovascular_acc = svm_cardiovascular_age - Age) %>%
  mutate(svm_hepatic_acc = svm_hepatic_age - Age) %>%
  mutate(svm_pulmonary_acc = svm_pulmonary_age - Age) %>%
  mutate(svm_renal_acc = svm_renal_age - Age) %>%
  mutate(svm_metabolic_acc = svm_metabolic_age - Age) %>%
  mutate(svm_musculoskeletal_acc = svm_musculoskeletal_age - Age) %>%
  mutate(xgboost_overall_acc = xgboost_overall_age - Age) %>%
  mutate(xgboost_cardiovascular_acc = xgboost_cardiovascular_age - Age) %>%
  mutate(xgboost_hepatic_acc = xgboost_hepatic_age - Age) %>%
  mutate(xgboost_pulmonary_acc = xgboost_pulmonary_age - Age) %>%
  mutate(xgboost_renal_acc = xgboost_renal_age - Age) %>%
  mutate(xgboost_metabolic_acc = xgboost_metabolic_age - Age) %>%
  mutate(xgboost_musculoskeletal_acc = xgboost_musculoskeletal_age - Age) %>%
  mutate(dnn_overall_acc = dnn_overall_age - Age) %>%
  mutate(dnn_cardiovascular_acc = dnn_cardiovascular_age - Age) %>%
  mutate(dnn_hepatic_acc = dnn_hepatic_age - Age) %>%
  mutate(dnn_pulmonary_acc = dnn_pulmonary_age - Age) %>%
  mutate(dnn_renal_acc = dnn_renal_age - Age) %>%
  mutate(dnn_metabolic_acc = dnn_metabolic_age - Age) %>%
  mutate(dnn_musculoskeletal_acc = dnn_musculoskeletal_age - Age) %>%
  mutate(rf_overall_acc = rf_overall_age - Age) %>%
  mutate(rf_cardiovascular_acc = rf_cardiovascular_age - Age) %>%
  mutate(rf_hepatic_acc = rf_hepatic_age - Age) %>%
  mutate(rf_pulmonary_acc = rf_pulmonary_age - Age) %>%
  mutate(rf_renal_acc = rf_renal_age - Age) %>%
  mutate(rf_metabolic_acc = rf_metabolic_age - Age) %>%
  mutate(rf_musculoskeletal_acc = rf_musculoskeletal_age - Age) %>%
  mutate(llm_overall_acc = llm_overall_age - Age) %>%
  mutate(llm_cardiovascular_acc = llm_cardiovascular_age - Age) %>%
  mutate(llm_hepatic_acc = llm_hepatic_age - Age) %>%
  mutate(llm_pulmonary_acc = llm_pulmonary_age - Age) %>%
  mutate(llm_renal_acc = llm_renal_age - Age) %>%
  mutate(llm_metabolic_acc = llm_metabolic_age - Age) %>%
  mutate(llm_musculoskeletal_acc = llm_musculoskeletal_age - Age)

### adjusted age gap
# svm overall acc
model <- lm(svm_overall_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_svm_overall_acc <- dat_age$svm_overall_acc - predicted_gap
dat_age$adj_svm_overall_acc <- round(dat_age$adj_svm_overall_acc, digits = 0)

# svm cardiovascular acc
model <- lm(svm_cardiovascular_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_svm_cardiovascular_acc <- dat_age$svm_cardiovascular_acc - predicted_gap
dat_age$adj_svm_cardiovascular_acc <- round(dat_age$adj_svm_cardiovascular_acc, digits = 0)

# svm hepatic acc
model <- lm(svm_hepatic_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_svm_hepatic_acc <- dat_age$svm_hepatic_acc - predicted_gap
dat_age$adj_svm_hepatic_acc <- round(dat_age$adj_svm_hepatic_acc, digits = 0)

# svm pulmonary acc
model <- lm(svm_pulmonary_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_svm_pulmonary_acc <- dat_age$svm_pulmonary_acc - predicted_gap
dat_age$adj_svm_pulmonary_acc <- round(dat_age$adj_svm_pulmonary_acc, digits = 0)

# svm renal acc
model <- lm(svm_renal_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_svm_renal_acc <- dat_age$svm_renal_acc - predicted_gap
dat_age$adj_svm_renal_acc <- round(dat_age$adj_svm_renal_acc, digits = 0)

# svm metabolic acc
model <- lm(svm_metabolic_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_svm_metabolic_acc <- dat_age$svm_metabolic_acc - predicted_gap
dat_age$adj_svm_metabolic_acc <- round(dat_age$adj_svm_metabolic_acc, digits = 0)

# svm musculoskeletal acc
model <- lm(svm_musculoskeletal_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_svm_musculoskeletal_acc <- dat_age$svm_musculoskeletal_acc - predicted_gap
dat_age$adj_svm_musculoskeletal_acc <- round(dat_age$adj_svm_musculoskeletal_acc, digits = 0)

# xgboost overall acc
model <- lm(xgboost_overall_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_xgboost_overall_acc <- dat_age$xgboost_overall_acc - predicted_gap
dat_age$adj_xgboost_overall_acc <- round(dat_age$adj_xgboost_overall_acc, digits = 0)

# xgboost cardiovascular acc
model <- lm(xgboost_cardiovascular_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_xgboost_cardiovascular_acc <- dat_age$xgboost_cardiovascular_acc - predicted_gap
dat_age$adj_xgboost_cardiovascular_acc <- round(dat_age$adj_xgboost_cardiovascular_acc, digits = 0)

# xgboost hepatic acc
model <- lm(xgboost_hepatic_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_xgboost_hepatic_acc <- dat_age$xgboost_hepatic_acc - predicted_gap
dat_age$adj_xgboost_hepatic_acc <- round(dat_age$adj_xgboost_hepatic_acc, digits = 0)

# xgboost pulmonary acc
model <- lm(xgboost_pulmonary_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_xgboost_pulmonary_acc <- dat_age$xgboost_pulmonary_acc - predicted_gap
dat_age$adj_xgboost_pulmonary_acc <- round(dat_age$adj_xgboost_pulmonary_acc, digits = 0)

# xgboost renal acc
model <- lm(xgboost_renal_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_xgboost_renal_acc <- dat_age$xgboost_renal_acc - predicted_gap
dat_age$adj_xgboost_renal_acc <- round(dat_age$adj_xgboost_renal_acc, digits = 0)

# xgboost metabolic acc
model <- lm(xgboost_metabolic_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_xgboost_metabolic_acc <- dat_age$xgboost_metabolic_acc - predicted_gap
dat_age$adj_xgboost_metabolic_acc <- round(dat_age$adj_xgboost_metabolic_acc, digits = 0)

# xgboost musculoskeletal acc
model <- lm(xgboost_musculoskeletal_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_xgboost_musculoskeletal_acc <- dat_age$xgboost_musculoskeletal_acc - predicted_gap
dat_age$adj_xgboost_musculoskeletal_acc <- round(dat_age$adj_xgboost_musculoskeletal_acc, digits = 0)

# dnn overall acc
model <- lm(dnn_overall_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_dnn_overall_acc <- dat_age$dnn_overall_acc - predicted_gap
dat_age$adj_dnn_overall_acc <- round(dat_age$adj_dnn_overall_acc, digits = 0)

# dnn cardiovascular acc
model <- lm(dnn_cardiovascular_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_dnn_cardiovascular_acc <- dat_age$dnn_cardiovascular_acc - predicted_gap
dat_age$adj_dnn_cardiovascular_acc <- round(dat_age$adj_dnn_cardiovascular_acc, digits = 0)

# dnn hepatic acc
model <- lm(dnn_hepatic_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_dnn_hepatic_acc <- dat_age$dnn_hepatic_acc - predicted_gap
dat_age$adj_dnn_hepatic_acc <- round(dat_age$adj_dnn_hepatic_acc, digits = 0)

# dnn pulmonary acc
model <- lm(dnn_pulmonary_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_dnn_pulmonary_acc <- dat_age$dnn_pulmonary_acc - predicted_gap
dat_age$adj_dnn_pulmonary_acc <- round(dat_age$adj_dnn_pulmonary_acc, digits = 0)

# dnn renal acc
model <- lm(dnn_renal_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_dnn_renal_acc <- dat_age$dnn_renal_acc - predicted_gap
dat_age$adj_dnn_renal_acc <- round(dat_age$adj_dnn_renal_acc, digits = 0)

# dnn metabolic acc
model <- lm(dnn_metabolic_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_dnn_metabolic_acc <- dat_age$dnn_metabolic_acc - predicted_gap
dat_age$adj_dnn_metabolic_acc <- round(dat_age$adj_dnn_metabolic_acc, digits = 0)

# dnn musculoskeletal acc
model <- lm(dnn_musculoskeletal_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_dnn_musculoskeletal_acc <- dat_age$dnn_musculoskeletal_acc - predicted_gap
dat_age$adj_dnn_musculoskeletal_acc <- round(dat_age$adj_dnn_musculoskeletal_acc, digits = 0)

# rf overall acc
model <- lm(rf_overall_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_rf_overall_acc <- dat_age$rf_overall_acc - predicted_gap
dat_age$adj_rf_overall_acc <- round(dat_age$adj_rf_overall_acc, digits = 0)

# rf cardiovascular acc
model <- lm(rf_cardiovascular_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_rf_cardiovascular_acc <- dat_age$rf_cardiovascular_acc - predicted_gap
dat_age$adj_rf_cardiovascular_acc <- round(dat_age$adj_rf_cardiovascular_acc, digits = 0)

# rf hepatic acc
model <- lm(rf_hepatic_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_rf_hepatic_acc <- dat_age$rf_hepatic_acc - predicted_gap
dat_age$adj_rf_hepatic_acc <- round(dat_age$adj_rf_hepatic_acc, digits = 0)

# rf pulmonary acc
model <- lm(rf_pulmonary_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_rf_pulmonary_acc <- dat_age$rf_pulmonary_acc - predicted_gap
dat_age$adj_rf_pulmonary_acc <- round(dat_age$adj_rf_pulmonary_acc, digits = 0)

# rf renal_acc
model <- lm(rf_renal_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_rf_renal_acc <- dat_age$rf_renal_acc - predicted_gap
dat_age$adj_rf_renal_acc <- round(dat_age$adj_rf_renal_acc, digits = 0)

# rf metabolic acc
model <- lm(rf_metabolic_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_rf_metabolic_acc <- dat_age$rf_metabolic_acc - predicted_gap
dat_age$adj_rf_metabolic_acc <- round(dat_age$adj_rf_metabolic_acc, digits = 0)

# rf musculoskeletal acc
model <- lm(rf_musculoskeletal_acc ~ Age, data = dat_age)
predicted_gap <- predict(model, newdata = dat_age)
dat_age$adj_rf_musculoskeletal_acc <- dat_age$rf_musculoskeletal_acc - predicted_gap
dat_age$adj_rf_musculoskeletal_acc <- round(dat_age$adj_rf_musculoskeletal_acc, digits = 0)

# write_csv(dat_age, "test_full_data_compare_ukb.csv")

dat_age <- read_csv("Data/Models/final_compare/test_full_data_compare_ukb.csv")

### cox models
var_ls <- c("Age", 
            "svm_overall_age", "svm_cardiovascular_age", 
            "svm_hepatic_age", "svm_pulmonary_age", "svm_renal_age", 
            "svm_metabolic_age", "svm_musculoskeletal_age",
            "adj_svm_all_acc", "adj_svm_cardiovascular_acc", 
            "adj_svm_hepatic_acc",  "adj_svm_pulmonary_acc", 
            "adj_svm_renal_acc", "adj_svm_metabolic_acc",
            "adj_svm_musculoskeletal_acc",
            "rf_cardiovascular_age", "rf_hepatic_age", 
            "rf_pulmonary_age", "rf_renal_age", 
            "rf_metabolic_age", "rf_musculoskeletal_age",
            "adj_rf_cardiovascular_acc", "adj_rf_hepatic_acc",  
            "adj_rf_pulmonary_acc", "adj_rf_renal_acc", 
            "adj_rf_metabolic_acc", "adj_rf_musculoskeletal_acc",
            "xgboost_cardiovascular_age", "xgboost_hepatic_age", 
            "xgboost_pulmonary_age", "xgboost_renal_age", 
            "xgboost_metabolic_age", "xgboost_musculoskeletal_age",
            "adj_xgboost_cardiovascular_acc", "adj_xgboost_hepatic_acc",  
            "adj_xgboost_pulmonary_acc", "adj_xgboost_renal_acc", 
            "adj_xgboost_metabolic_acc", "adj_xgboost_musculoskeletal_acc",
            "dnn_cardiovascular_age", "dnn_hepatic_age", 
            "dnn_pulmonary_age", "dnn_renal_age", 
            "dnn_metabolic_age", "dnn_musculoskeletal_age",
            "adj_dnn_cardiovascular_acc", "adj_dnn_hepatic_acc",  
            "adj_dnn_pulmonary_acc", "adj_dnn_renal_acc", 
            "adj_dnn_metabolic_acc", "adj_dnn_musculoskeletal_acc",
            "llm_overall_age", "llm_cardiovascular_age", "llm_hepatic_age",
            "llm_pulmonary_age", "llm_renal_age", "llm_metabolic_age",
            "llm_musculoskeletal_age", "llm_all_acc", "llm_cardiovascular_acc",
            "llm_hepatic_acc", "llm_pulmonary_acc", "llm_renal_acc",
            "llm_metabolic_acc", "llm_musculoskeletal_acc")

disease <- c("CHD", "Stroke", 
             "COPD", "Asthma",
             "Liver diseases", "Gallbladder diseases",
             "Renal failure", "Nephrotic syndrome",
             "T2D", "Thyroid disorder",
             "Arthritis", "Systemic connective tissue diseases")

var_mean_c_index <- c()
var_mean_c_index_lower <- c()
var_mean_c_index_upper <- c()
outcome_ls <- c()

set.seed(2024)
for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]
  
  # include disease-free participants
  if (item == "CHD" | item == "Stroke") {
    dat_cox <- subset(dat_age, `MACE duration` > 0)
  }
  else if (item == "Renal failure" | item == "Nephrotic syndrome") {
    dat_cox <- subset(dat_age, `Renal diseases duration` > 0)
  }
  else if (item == "T2D") {
    dat_cox <- subset(dat_age, `Diabetes duration` > 0)
  }
  else {
    dat_cox <- subset(dat_age, time > 0) 
  }
  
  folds <- createFolds(dat_cox$event, k = 3)
  for (i in 1:length(var_ls)) {
    var <- var_ls[i]
    c_index_values <- c()
    c_index_lower_ls <- c()
    c_index_upper_ls <- c()
    
    for(j in 1:3) {
      test_indices <- folds[[j]]
      train_data <- dat_cox[-test_indices, ]
      test_data <- dat_cox[test_indices, ]

      # build cox models
      formula_covariates <- paste0("survobj ~ ", var)
      f <- as.formula(formula_covariates)
      survobj <- with(train_data, Surv(time, event))
      cox_fit <- coxph(formula = f, data = train_data, na.action = na.omit)
      
      # predict risk score
      test_data$predicted_risk <- predict(cox_fit, newdata = test_data, 
                                          type = "risk")
      
      # calculate c-index
      concordance_result <- concordance.index(x = test_data$predicted_risk,
                                              surv.time = test_data$time,
                                              surv.event = test_data$event)
      c_index <- concordance_result$c.index
      c_index_lower <- concordance_result$lower
      c_index_upper <- concordance_result$upper
      # 存储c-index
      c_index_values <- c(c_index_values, c_index)
      c_index_lower_ls <- c(c_index_lower_ls, c_index_lower)
      c_index_upper_ls <- c(c_index_upper_ls, c_index_upper)
      print(paste0(item, " ------------ ", var, " ------------ fold ", j))
    }
    mean_c_index <- round(mean(c_index_values), digits = 3)
    mean_c_index_lower <- round(mean(c_index_lower_ls), digits = 3)
    mean_c_index_upper <- round(mean(c_index_upper_ls), digits = 3)
    
    var_mean_c_index <- c(var_mean_c_index, mean_c_index)
    var_mean_c_index_lower <- c(var_mean_c_index_lower, mean_c_index_lower)
    var_mean_c_index_upper <- c(var_mean_c_index_upper, mean_c_index_upper)
    outcome_ls <- c(outcome_ls, item)
  }
}

dat_plot <- data.frame(
  outcome = outcome_ls,
  var_name = var_ls,
  c_index = var_mean_c_index,
  c_index_lower = var_mean_c_index_lower,
  c_index_upper = var_mean_c_index_upper
)

dat_plot <- dat_plot %>%
  mutate(var_name = case_when(
    var_name == "Age" ~ "Chronological age",
    var_name == "svm_overall_age" ~ "SVM overall age",
    var_name == "svm_cardiovascular_age" ~ "SVM cardiovascular age",
    var_name == "svm_hepatic_age" ~ "SVM hepatic age",
    var_name == "svm_pulmonary_age" ~ "SVM pulmonary age", 
    var_name == "svm_renal_age" ~ "SVM renal age",
    var_name == "svm_metabolic_age" ~ "SVM metabolic age", 
    var_name == "svm_musculoskeletal_age" ~ "SVM musculoskeletal age", 
    var_name == "adj_svm_all_acc" ~ "SVM adj overall age gap", 
    var_name == "adj_svm_cardiovascular_acc" ~ "SVM adj cardiovascular age gap",
    var_name == "adj_svm_hepatic_acc" ~ "SVM adj hepatic age gap", 
    var_name == "adj_svm_pulmonary_acc" ~ "SVM adj pulmonary age gap",
    var_name == "adj_svm_renal_acc" ~ "SVM adj renal age gap", 
    var_name == "adj_svm_metabolic_acc" ~ "SVM adj metabolic age gap",
    var_name == "adj_svm_musculoskeletal_acc" ~ "SVM adj musculoskeletal age gap",
    var_name == "xgboost_cardiovascular_age" ~ "XGBoost cardiovascular age",
    var_name == "xgboost_hepatic_age" ~ "XGBoost hepatic age",
    var_name == "xgboost_pulmonary_age" ~ "XGBoost pulmonary age", 
    var_name == "xgboost_renal_age" ~ "XGBoost renal age",
    var_name == "xgboost_metabolic_age" ~ "XGBoost metabolic age", 
    var_name == "xgboost_musculoskeletal_age" ~ "XGBoost musculoskeletal age", 
    var_name == "adj_xgboost_cardiovascular_acc" ~ "XGBoost adj cardiovascular age gap",
    var_name == "adj_xgboost_hepatic_acc" ~ "XGBoost adj hepatic age gap", 
    var_name == "adj_xgboost_pulmonary_acc" ~ "XGBoost adj pulmonary age gap",
    var_name == "adj_xgboost_renal_acc" ~ "XGBoost adj renal age gap", 
    var_name == "adj_xgboost_metabolic_acc" ~ "XGBoost adj metabolic age gap",
    var_name == "adj_xgboost_musculoskeletal_acc" ~ "XGBoost adj musculoskeletal age gap",
    var_name == "dnn_cardiovascular_age" ~ "DNN cardiovascular age",
    var_name == "dnn_hepatic_age" ~ "DNN hepatic age",
    var_name == "dnn_pulmonary_age" ~ "DNN pulmonary age", 
    var_name == "dnn_renal_age" ~ "DNN renal age",
    var_name == "dnn_metabolic_age" ~ "DNN metabolic age", 
    var_name == "dnn_musculoskeletal_age" ~ "DNN musculoskeletal age", 
    var_name == "adj_dnn_cardiovascular_acc" ~ "DNN adj cardiovascular age gap",
    var_name == "adj_dnn_hepatic_acc" ~ "DNN adj hepatic age gap", 
    var_name == "adj_dnn_pulmonary_acc" ~ "DNN adj pulmonary age gap",
    var_name == "adj_dnn_renal_acc" ~ "DNN adj renal age gap", 
    var_name == "adj_dnn_metabolic_acc" ~ "DNN adj metabolic age gap",
    var_name == "adj_dnn_musculoskeletal_acc" ~ "DNN adj musculoskeletal age gap",
    var_name == "rf_cardiovascular_age" ~ "RF cardiovascular age",
    var_name == "rf_hepatic_age" ~ "RF hepatic age",
    var_name == "rf_pulmonary_age" ~ "RF pulmonary age", 
    var_name == "rf_renal_age" ~ "RF renal age",
    var_name == "rf_metabolic_age" ~ "RF metabolic age", 
    var_name == "rf_musculoskeletal_age" ~ "RF musculoskeletal age", 
    var_name == "adj_rf_cardiovascular_acc" ~ "RF adj cardiovascular age gap",
    var_name == "adj_rf_hepatic_acc" ~ "RF adj hepatic age gap", 
    var_name == "adj_rf_pulmonary_acc" ~ "RF adj pulmonary age gap",
    var_name == "adj_rf_renal_acc" ~ "RF adj renal age gap", 
    var_name == "adj_rf_metabolic_acc" ~ "RF adj metabolic age gap",
    var_name == "adj_rf_musculoskeletal_acc" ~ "RF adj musculoskeletal age gap",
    var_name == "llm_overall_age" ~ "LLM overall age",
    var_name == "llm_cardiovascular_age" ~ "LLM cardiovascular age",
    var_name == "llm_hepatic_age" ~ "LLM hepatic age",
    var_name == "llm_pulmonary_age" ~ "LLM pulmonary age", 
    var_name == "llm_renal_age" ~ "LLM renal age",
    var_name == "llm_metabolic_age" ~ "LLM metabolic age", 
    var_name == "llm_musculoskeletal_age" ~ "LLM musculoskeletal age", 
    var_name == "llm_all_acc" ~ "LLM overall age gap", 
    var_name == "llm_cardiovascular_acc" ~ "LLM cardiovascular age gap",
    var_name == "llm_hepatic_acc" ~ "LLM hepatic age gap", 
    var_name == "llm_pulmonary_acc" ~ "LLM pulmonary age gap",
    var_name == "llm_renal_acc" ~ "LLM renal age gap", 
    var_name == "llm_metabolic_acc" ~ "LLM metabolic age gap",
    var_name == "llm_musculoskeletal_acc" ~ "LLM musculoskeletal age gap"
  ))

# write_rds(dat_plot, "organ_age_acc_compare_ukb.rds")

### prepare the plot data, respectively
# age
dat_plot_age <- subset(dat_plot_age,
                       (grepl("cardiovascular age", var_name) & (grepl("CHD", outcome) | grepl("Stroke", outcome))) | 
                       (grepl("pulmonary age", var_name) & (grepl("COPD", outcome) | grepl("Asthma", outcome))) |
                       (grepl("hepatic age", var_name) & (grepl("Liver diseases", outcome) | grepl("Gallbladder diseases", outcome))) |
                       (grepl("renal age", var_name) & (grepl("Renal failure", outcome) | grepl("Nephrotic syndrome", outcome))) |
                       (grepl("metabolic age", var_name) & (grepl("T2D", outcome) | grepl("Thyroid disorder", outcome))) |
                       (grepl("musculoskeletal age", var_name) & (grepl("Arthritis", outcome) | grepl("Systemic connective tissue diseases", outcome))))

dat_plot_age$var_name <- gsub("cardiovascular ", "", dat_plot_age$var_name)
dat_plot_age$var_name <- gsub("pulmonary ", "", dat_plot_age$var_name)
dat_plot_age$var_name <- gsub("hepatic ", "", dat_plot_age$var_name)
dat_plot_age$var_name <- gsub("renal ", "", dat_plot_age$var_name)
dat_plot_age$var_name <- gsub("metabolic ", "", dat_plot_age$var_name)
dat_plot_age$var_name <- gsub("musculoskeletal ", "", dat_plot_age$var_name)

dat_plot_age <- dat_plot_age %>%
  mutate(outcome = case_when(
    outcome == "Systemic connective tissue diseases" ~ "SCTDs",
    TRUE ~ outcome
  )) %>%
  mutate(outcome = case_when(
    outcome == "CHD" | outcome == "Stroke" ~ paste0(outcome, " (cardiovascular age)"),
    outcome == "COPD" | outcome == "Asthma" ~ paste0(outcome, " (pulmonary age)"),
    outcome == "Liver diseases" | outcome == "Gallbladder diseases" ~ paste0(outcome, " (hepatic age)"),
    outcome == "Renal failure" | outcome == "Nephrotic syndrome" ~ paste0(outcome, " (renal age)"),
    outcome == "T2D" | outcome == "Thyroid disorder" ~ paste0(outcome, " (metabolic age)"),
    outcome == "Arthritis" | outcome == "SCTDs" ~ paste0(outcome, " (musculoskeletal age)")
  ))

dat_plot_age$var_name <- factor(dat_plot_age$var_name, 
                                levels = c("SVM age", 
                                           "RF age",
                                           "XGBoost age", 
                                           "DNN age", 
                                           "LLM age"))

# acc
dat_plot_age <- subset(dat_plot_age,
                       (grepl("cardiovascular age gap", var_name) & (grepl("CHD", outcome) | grepl("Stroke", outcome))) | 
                       (grepl("pulmonary age gap", var_name) & (grepl("COPD", outcome) | grepl("Asthma", outcome))) |
                       (grepl("hepatic age gap", var_name) & (grepl("Liver diseases", outcome) | grepl("Gallbladder diseases", outcome))) |
                       (grepl("renal age gap", var_name) & (grepl("Renal failure", outcome) | grepl("Nephrotic syndrome", outcome))) |
                       (grepl("metabolic age gap", var_name) & (grepl("T2D", outcome) | grepl("Thyroid disorder", outcome))) |
                       (grepl("musculoskeletal age gap", var_name) & (grepl("Arthritis", outcome) | grepl("Systemic connective tissue diseases", outcome))))

dat_plot_age$var_name <- gsub("adj cardiovascular ", "", dat_plot_age$var_name)
dat_plot_age$var_name <- gsub("adj pulmonary ", "", dat_plot_age$var_name)
dat_plot_age$var_name <- gsub("adj hepatic ", "", dat_plot_age$var_name)
dat_plot_age$var_name <- gsub("adj renal ", "", dat_plot_age$var_name)
dat_plot_age$var_name <- gsub("adj metabolic ", "", dat_plot_age$var_name)
dat_plot_age$var_name <- gsub("adj musculoskeletal ", "", dat_plot_age$var_name)

dat_plot_age <- dat_plot_age %>%
  mutate(outcome = case_when(
    outcome == "Systemic connective tissue diseases" ~ "SCTDs",
    TRUE ~ outcome
  )) %>%
  mutate(outcome = case_when(
    outcome == "CHD" | outcome == "Stroke" ~ paste0(outcome, " (cardiovascular)"),
    outcome == "COPD" | outcome == "Asthma" ~ paste0(outcome, " (pulmonary)"),
    outcome == "Liver diseases" | outcome == "Gallbladder diseases" ~ paste0(outcome, " (hepatic)"),
    outcome == "Renal failure" | outcome == "Nephrotic syndrome" ~ paste0(outcome, " (renal)"),
    outcome == "T2D" | outcome == "Thyroid disorder" ~ paste0(outcome, " (metabolic)"),
    outcome == "Arthritis" | outcome == "SCTDs" ~ paste0(outcome, " (musculoskeletal)")
  ))

dat_plot_age$var_name <- factor(dat_plot_age$var_name, 
                                levels = c("SVM age gap", 
                                           "RF age gap",
                                           "XGBoost age gap", 
                                           "DNN age gap", 
                                           "LLM age gap"))

### plot
plots_c_index <- list()
disease <- unique(dat_plot_age$outcome)
var_name_ls <- as.character(unique(dat_plot_age$var_name))

# fixed fourth colors
fixed_colors <- c("#e4d1d1", "#b9b0b0", "#d9ecd0", "#77a8a8")
# the fifth color
color_groups <- c("#E71D1D", "#E71D1D",
                  "#FF7D01", "#FF7D01",
                  "#4BB04A", "#4BB04A",
                  "#F980BE", "#F980BE",
                  "#A35628", "#A35628",
                  "#9850A6", "#9850A6")

for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(dat_plot_age, outcome == item)
  organ_color <- color_groups[i]
  plot_colors <- c(fixed_colors, organ_color)
  p <- ggplot(dat_sub, aes(x = var_name, y = c_index, color = var_name)) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = c_index_lower, ymax = c_index_upper), width = 0.2) +
    geom_segment(aes(x = 0, xend = var_name, y = c_index, yend = c_index),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% var_name_ls)) +
    scale_color_manual(values = plot_colors) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 22),
          axis.text.x = element_text(angle = 90, size = 22, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 22, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
  
  if (i < 9) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.line.x = element_blank())
  }
  plots_c_index[[i]] <- p
}

# age
arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 4, 
                            nrow = 3,
                            heights = c(1, 1, 1.7))
# acc
arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 4, 
                            nrow = 3,
                            heights = c(1, 1, 1.9))

combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Absolute C-index", size = 22, rot = 90))

ggsave("fig2d_organ_age_cindex.pdf", plot = combined_plot, width = 20, height = 12)
ggsave("extended_fig3c_organ_acc_cindex.pdf", plot = combined_plot, width = 20, height = 12)


###### 1-4.compare Beta on aging-related phenotypes, UKB
library(tidyverse)
library(arrow)
library(survival)
library(survminer)
library(survcomp)
library(caret)
library(gridExtra)
library(pROC)
library(Hmisc)
library(rms)
library(hexbin)
library(scales)
library(grid)
library(ppcor)
library(tidyverse)
library(lubridate)
library(svglite)
library(broom)

dat_llm <- read_csv("Data/Models/final_compare/test_full_data_compare_ukb.csv")
dat_phenotype <- read_csv("Data/Phenotype/phenotype_ukb.csv")

dat_telomere <- read_csv("Data/covariates_outcomes/telomere.csv")
dat_fi <- read_rds("Data/covariates_outcomes/frailty_index_52.rds")
dat_telomere <- dplyr::select(dat_telomere, 1:2, 5)
names(dat_telomere)[c(2, 3)] <- c("telomere_adjusted", "z_adjusted_telomere")
dat_telomere <- na.omit(dat_telomere)

dat_llm <- dat_llm %>% 
  inner_join(dat_phenotype, by = "eid") %>%
  inner_join(dat_telomere, by = "eid") %>%
  inner_join(dat_fi, by = "eid")
  
# data standardize
dat_llm <- dat_llm %>%
  mutate(across(c(llm_overall_age,
                  llm_overall_acc,
                  svm_overall_age,
                  adj_svm_overall_acc,
                  xgboost_overall_age,
                  adj_xgboost_overall_acc,
                  rf_overall_age,
                  adj_rf_overall_acc,
                  dnn_overall_age,
                  adj_dnn_overall_acc,
                  Age,
                  BMI,
                  `Heel bone mineral density (BMD)`,
                  `Pulse wave Arterial Stiffness index`,
                  overall_health_rating,
                  `Mean time to correctly identify matches`,
                  telomere_adjusted,
                  frailty_index), scale))

# covariates
covariates_default <- c("Sex", "UKB_assessment_center",
                        "Education", "Employment", "Income", "Ethnicity",
                        "BMI", "Current_smoker", 
                        "Daily_alcohol_intake",
                        "Hypertension_history")
covariates_with_acc <- c("Age", "Sex", "UKB_assessment_center",
                         "Education", "Employment", "Income", "Ethnicity",
                         "BMI", "Current_smoker", 
                         "Daily_alcohol_intake",
                         "Hypertension_history")

aging_outcomes <- c("`Heel bone mineral density (BMD)`",
                    "`Pulse wave Arterial Stiffness index`",
                    "telomere_adjusted",
                    "hearing_problems",
                    "dental_problem",
                    "Slowness",
                    "falls_in_the_last_year",
                    "general_pain",
                    "long_standing_illness",
                    "frailty_index",
                    "overall_health_rating",
                    "`Mean time to correctly identify matches`")

tag_ls <- c("lm", "lm", "lm", "glm", "glm", "glm",
            "glm", "glm", "glm", "lm", "lm", "lm")

var_name_ls <- c("llm_overall_age",
                 "llm_overall_acc",
                 "svm_overall_age",
                 "adj_svm_overall_acc",
                 "xgboost_overall_age",
                 "adj_xgboost_overall_acc",
                 "rf_overall_age",
                 "adj_rf_overall_acc",
                 "dnn_overall_age",
                 "adj_dnn_overall_acc",
                 "Age")

### regression analysis
aging_phenotype_reg <- function(dat, predictor, covariates, outcome, tag) {
  formula <- as.formula(paste(outcome, "~", predictor, "+", 
                              paste(covariates, collapse = "+")))
  if (tag == "lm") {
    model <- lm(formula, data = dat, na.action = na.omit)
  } else {
    model <- glm(formula, data = dat, family = binomial, na.action = na.omit)
  }
  
  # extract beta and 95% CI
  coef_info <- tidy(model, conf.int = TRUE) %>%
    filter(term == predictor) %>%
    mutate(outcome = outcome,
           predictor = predictor,
           model_type = tag)
  
  print(paste0(outcome, "---", predictor, "---over!"))
  return(coef_info)
}

### reg
results <- data.frame()
for (i in 1:length(aging_outcomes)) {
  outcome <- aging_outcomes[i]
  tag <- tag_ls[i]
  for (j in 1:length(var_name_ls)) {
    predictor <- var_name_ls[j]
    # covariates
    if (grepl("acc", predictor)) {
      covariates <- covariates_with_acc
    } else {
      covariates <- covariates_default
    }
    coef_info <- aging_phenotype_reg(dat_llm,
                                     predictor,
                                     covariates,
                                     outcome,
                                     tag)
    results <- bind_rows(results, coef_info)
  }
}

### extract
res_overall_age <- subset(res, (grepl("overall", term) |
                                grepl("Age", term)) &
                                !grepl("acc", term))
res_overall_acc <- subset(res, grepl("acc", term) & 
                               (grepl("overall", term) | 
                               grepl("all", term)))

# process
res_overall_age$outcome <- gsub("`", "", res_overall_age$outcome)
res_overall_acc$outcome <- gsub("`", "", res_overall_acc$outcome)

res_overall_age <- res_overall_age %>%
  mutate(outcome = case_when(
    outcome == "Heel bone mineral density (BMD)" ~ "Heel bone mineral density",
    outcome == "Pulse wave Arterial Stiffness index" ~ "Arterial stiffness index",
    outcome == "overall_health_rating" ~ "Overall health rating",
    outcome == "Mean time to correctly identify matches" ~ "Reaction time",
    outcome == "telomere_adjusted" ~ "Adjusted telomere length",
    outcome == "frailty_index" ~ "Frailty index",
    outcome == "Slowness" ~ "Slow walking pace",
    outcome == "hearing_problems" ~ "Hearing problems",
    outcome == "dental_problem" ~ "Dental problems",
    outcome == "long_standing_illness" ~ "Long standing illness",
    outcome == "general_pain" ~ "General pain",
    outcome == "falls_in_the_last_year" ~ "Falls in the last year",
    TRUE ~ outcome
  ))

res_overall_acc <- res_overall_acc %>%
  mutate(outcome = case_when(
    outcome == "Heel bone mineral density (BMD)" ~ "Heel bone mineral density",
    outcome == "Pulse wave Arterial Stiffness index" ~ "Arterial stiffness index",
    outcome == "overall_health_rating" ~ "Overall health rating",
    outcome == "Mean time to correctly identify matches" ~ "Reaction time",
    outcome == "adjusted_telomere" ~ "Adjusted telomere length",
    outcome == "telomere_adjusted" ~ "Frailty index",
    outcome == "Slowness" ~ "Slow walking pace",
    outcome == "hearing_problems" ~ "Hearing problems",
    outcome == "dental_problem" ~ "Dental problems",
    outcome == "long_standing_illness" ~ "Long standing illness",
    outcome == "general_pain" ~ "General pain",
    outcome == "falls_in_the_last_year" ~ "Falls in the last year",
    TRUE ~ outcome
  ))

res_overall_age$FDR <- p.adjust(res_overall_age$p.value, method = "fdr")
res_overall_acc$FDR <- p.adjust(res_overall_acc$p.value, method = "fdr")


res_overall_age <- res_overall_age %>%
  mutate(term = case_when(
    term == "llm_overall_age" ~ "LLM overall age",
    term == "svm_overall_age" ~ "SVM overall age",
    term == "xgboost_overall_age" ~ "XGBoost overall age",
    term == "rf_overall_age" ~ "RF overall age",
    term == "dnn_overall_age" ~ "DNN overall age",
    term == "Age" ~ "Chronological age",
  ))

res_overall_acc <- res_overall_acc %>%
  mutate(term = case_when(
    term == "llm_overall_acc" ~ "LLM overall age gap",
    term == "adj_svm_overall_acc" ~ "SVM overall age gap",
    term == "adj_xgboost_overall_acc" ~ "XGBoost overall age gap",
    term == "adj_rf_overall_acc" ~ "RF overall age gap",
    term == "adj_dnn_overall_acc" ~ "DNN overall age gap"
  ))

### compare difference
compare_beta_age <- function(dat, 
                             outcome, 
                             predictor_1, 
                             predictor_2, 
                             predictor_3,
                             predictor_4,
                             predictor_5,
                             predictor_6,
                             covariates,
                             tag) {
  formula_1 <- as.formula(paste(outcome, "~", predictor_1, "+", 
                                paste(covariates, collapse = "+")))
  formula_2 <- as.formula(paste(outcome, "~", predictor_2, "+", 
                                paste(covariates, collapse = "+")))
  formula_3 <- as.formula(paste(outcome, "~", predictor_3, "+", 
                                paste(covariates, collapse = "+")))
  formula_4 <- as.formula(paste(outcome, "~", predictor_4, "+", 
                                paste(covariates, collapse = "+")))
  formula_5 <- as.formula(paste(outcome, "~", predictor_5, "+", 
                                paste(covariates, collapse = "+")))
  formula_6 <- as.formula(paste(outcome, "~", predictor_6, "+", 
                                paste(covariates, collapse = "+")))
  if (tag == "lm") {
    model_1 <- lm(formula_1, data = dat, na.action = na.omit)
    model_2 <- lm(formula_2, data = dat, na.action = na.omit)
    model_3 <- lm(formula_3, data = dat, na.action = na.omit)
    model_4 <- lm(formula_4, data = dat, na.action = na.omit)
    model_5 <- lm(formula_5, data = dat, na.action = na.omit)
    model_6 <- lm(formula_6, data = dat, na.action = na.omit)
  } else {
    model_1 <- glm(formula_1, data = dat, family = binomial, na.action = na.omit)
    model_2 <- glm(formula_2, data = dat, family = binomial, na.action = na.omit)
    model_3 <- glm(formula_3, data = dat, family = binomial, na.action = na.omit)
    model_4 <- glm(formula_4, data = dat, family = binomial, na.action = na.omit)
    model_5 <- glm(formula_5, data = dat, family = binomial, na.action = na.omit)
    model_6 <- glm(formula_6, data = dat, family = binomial, na.action = na.omit)
  }
  
  ### extract beta
  coef1 <- summary(model_1)$coefficients[2, 1]  # age1 beta
  se1 <- summary(model_1)$coefficients[2, 2]    # age1 se
  
  coef2 <- summary(model_2)$coefficients[2, 1]  # age2 beta
  se2 <- summary(model_2)$coefficients[2, 2]    # age2 se
  
  coef3 <- summary(model_3)$coefficients[2, 1]  # age3 beta
  se3 <- summary(model_3)$coefficients[2, 2]    # age3 se
  
  coef4 <- summary(model_4)$coefficients[2, 1]  # age4 beta
  se4 <- summary(model_4)$coefficients[2, 2]    # age4 se
  
  coef5 <- summary(model_5)$coefficients[2, 1]  # age5 beta
  se5 <- summary(model_5)$coefficients[2, 2]    # age5 se
  
  coef6 <- summary(model_6)$coefficients[2, 1]  # age6 beta
  se6 <- summary(model_6)$coefficients[2, 2]    # age6 se
  
  ### calculate difference
  coef_diff <- coef1 - coef6
  se_diff <- sqrt(se1^2 + se6^2)
  t_value <- coef_diff / se_diff
  p_value_1 <- 2 * (1 - pt(abs(t_value), df = nrow(dat) - 2))
  
  coef_diff <- coef2 - coef6
  se_diff <- sqrt(se2^2 + se6^2)
  t_value <- coef_diff / se_diff
  p_value_2 <- 2 * (1 - pt(abs(t_value), df = nrow(dat) - 2))
  
  coef_diff <- coef3 - coef6
  se_diff <- sqrt(se3^2 + se6^2)
  t_value <- coef_diff / se_diff
  p_value_3 <- 2 * (1 - pt(abs(t_value), df = nrow(dat) - 2))
  
  coef_diff <- coef4 - coef6
  se_diff <- sqrt(se4^2 + se6^2)
  t_value <- coef_diff / se_diff
  p_value_4 <- 2 * (1 - pt(abs(t_value), df = nrow(dat) - 2))
  
  coef_diff <- coef5 - coef6
  se_diff <- sqrt(se5^2 + se6^2)
  t_value <- coef_diff / se_diff
  p_value_5 <- 2 * (1 - pt(abs(t_value), df = nrow(dat) - 2))
  
  p_values <- c(p_value_1, p_value_2, p_value_3, p_value_4, p_value_5)
  p_adjusted_bh <- p.adjust(p_values, method = "BH")
  
  # output
  cat("p value:", p_adjusted_bh[1], "\n")
  
  return(p_adjusted_bh)
}

compare_beta_acc <- function(dat, 
                             outcome, 
                             predictor_1, 
                             predictor_2, 
                             predictor_3,
                             predictor_4,
                             predictor_6,
                             covariates,
                             tag) {
  formula_1 <- as.formula(paste(outcome, "~", predictor_1, "+", 
                                paste(covariates, collapse = "+")))
  formula_2 <- as.formula(paste(outcome, "~", predictor_2, "+", 
                                paste(covariates, collapse = "+")))
  formula_3 <- as.formula(paste(outcome, "~", predictor_3, "+", 
                                paste(covariates, collapse = "+")))
  formula_4 <- as.formula(paste(outcome, "~", predictor_4, "+", 
                                paste(covariates, collapse = "+")))
  formula_6 <- as.formula(paste(outcome, "~", predictor_6, "+", 
                                paste(covariates, collapse = "+")))
  if (tag == "lm") {
    model_1 <- lm(formula_1, data = dat, na.action = na.omit)
    model_2 <- lm(formula_2, data = dat, na.action = na.omit)
    model_3 <- lm(formula_3, data = dat, na.action = na.omit)
    model_4 <- lm(formula_4, data = dat, na.action = na.omit)
    model_6 <- lm(formula_6, data = dat, na.action = na.omit)
  } else {
    model_1 <- glm(formula_1, data = dat, family = binomial, na.action = na.omit)
    model_2 <- glm(formula_2, data = dat, family = binomial, na.action = na.omit)
    model_3 <- glm(formula_3, data = dat, family = binomial, na.action = na.omit)
    model_4 <- glm(formula_4, data = dat, family = binomial, na.action = na.omit)
    model_6 <- glm(formula_6, data = dat, family = binomial, na.action = na.omit)
  }
  
  coef1 <- summary(model_1)$coefficients[2, 1]  # acc1 beta
  se1 <- summary(model_1)$coefficients[2, 2]    # acc1 se
  
  coef2 <- summary(model_2)$coefficients[2, 1]  # acc2 beta
  se2 <- summary(model_2)$coefficients[2, 2]    # acc2 se
  
  coef3 <- summary(model_3)$coefficients[2, 1]  # acc3 beta
  se3 <- summary(model_3)$coefficients[2, 2]    # acc3 se
  
  coef4 <- summary(model_4)$coefficients[2, 1]  # acc4 beta
  se4 <- summary(model_4)$coefficients[2, 2]    # acc4 se
  
  coef6 <- summary(model_6)$coefficients[2, 1]  # acc6 beta
  se6 <- summary(model_6)$coefficients[2, 2]    # acc6 se
  
  
  coef_diff <- coef1 - coef6
  se_diff <- sqrt(se1^2 + se6^2)
  t_value <- coef_diff / se_diff
  p_value_1 <- 2 * (1 - pt(abs(t_value), df = nrow(dat) - 2))
  
  
  coef_diff <- coef2 - coef6
  se_diff <- sqrt(se2^2 + se6^2)
  t_value <- coef_diff / se_diff
  p_value_2 <- 2 * (1 - pt(abs(t_value), df = nrow(dat) - 2))
  
  
  coef_diff <- coef3 - coef6
  se_diff <- sqrt(se3^2 + se6^2)
  t_value <- coef_diff / se_diff
  p_value_3 <- 2 * (1 - pt(abs(t_value), df = nrow(dat) - 2))
  
  
  coef_diff <- coef4 - coef6
  se_diff <- sqrt(se4^2 + se6^2)
  t_value <- coef_diff / se_diff
  p_value_4 <- 2 * (1 - pt(abs(t_value), df = nrow(dat) - 2))
  
  
  p_values <- c(p_value_1, p_value_2, p_value_3, p_value_4)
  p_adjusted_bh <- p.adjust(p_values, method = "BH")
  
  # output
  cat("p value:", p_adjusted_bh[1], "\n")
  
  return(p_adjusted_bh)
}


outcome_ls <- c("`Heel bone mineral density (BMD)`",
                "`Pulse wave Arterial Stiffness index`",
                "telomere_adjusted",
                "hearing_problems",
                "dental_problem",
                "Slowness",
                "falls_in_the_last_year",
                "general_pain",
                "long_standing_illness",
                "frailty_index",
                "overall_health_rating",
                "`Mean time to correctly identify matches`")

tag_ls <- c("lm", "lm", "lm", "glm", "glm", "glm", "glm", "glm", "glm",
            "lm", "lm", "lm")

### age
var_name_ls <- c("Age",
                 "svm_overall_age",
                 "rf_overall_age",
                 "xgboost_overall_age",
                 "dnn_overall_age",
                 "llm_overall_age")

fdr_ls <- c()
order_final <- c()
aging_indicators_ls <- c()

for (i in 1:12) {
  outcome <- outcome_ls[i]
  tag <- tag_ls[i]
  p_values_ls <- compare_beta_age(dat_llm,
                                  outcome,
                                  var_name_ls[1],
                                  var_name_ls[2],
                                  var_name_ls[3],
                                  var_name_ls[4],
                                  var_name_ls[5],
                                  var_name_ls[6],
                                  covariates_default,
                                  tag)
  order_final <- c(order_final, rep(i, 5))
  fdr_ls <- c(fdr_ls, p_values_ls)
  aging_indicators_ls <- c(aging_indicators_ls, var_name_ls[1:5])
}

res_fdr <- data.frame(plot_order = order_final,
                      aging_indicator = aging_indicators_ls,
                      fdr = fdr_ls)


### acc
var_name_ls <- c("adj_svm_overall_acc",
                 "adj_rf_overall_acc",
                 "adj_xgboost_overall_acc",
                 "adj_dnn_overall_acc",
                 "llm_overall_acc")
fdr_ls <- c()
order_final <- c()
aging_indicators_ls <- c()

for (i in 1:12) {
  outcome <- outcome_ls[i]
  tag <- tag_ls[i]
  p_values_ls <- compare_beta_acc(dat_llm,
                                  outcome,
                                  var_name_ls[1],
                                  var_name_ls[2],
                                  var_name_ls[3],
                                  var_name_ls[4],
                                  var_name_ls[5],
                                  covariates_with_acc,
                                  tag)
  order_final <- c(order_final, rep(i, 4))
  fdr_ls <- c(fdr_ls, p_values_ls)
  aging_indicators_ls <- c(aging_indicators_ls, var_name_ls[1:4])
}

res_fdr_acc <- data.frame(plot_order = order_final,
                          aging_indicator = aging_indicators_ls,
                          fdr = fdr_ls)

### plot single age bar
plot_single_bar <- function(data, title, p_values) {
  data$star_annotation <- ""
  # note term
  for (i in 1:nrow(data)) {
    if (data$term[i] == "LLM overall age") {
      data$star_annotation[i] <- "Ref"
    } 
    else {
      p_value <- p_values[match(data$term[i], c("Chronological age", 
                                                "SVM overall age", 
                                                "RF overall age", 
                                                "XGBoost overall age", 
                                                "DNN overall age"))]
      data$star_annotation[i] <- ifelse(p_value < 0.001, "***", 
                                        ifelse(p_value < 0.01, "**", 
                                               ifelse(p_value < 0.05, "*", "")))
    }
  }
  
  # calculate x_position
  x_position <- as.numeric(factor(data$term))
  
  # calculate y_position
  data$star_y_position <- ifelse(data$estimate > 0, 
                                 data$conf.high + (data$conf.high - data$conf.low) * 0.25, 
                                 data$conf.low - (data$conf.high - data$conf.low) * 0.3) 
  
  # fig
  ggplot(data, aes(x = term, y = estimate, fill = term)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.1, position = position_dodge(width = 0.6)) +
    scale_fill_manual(values = c("LLM overall age" = "#4480B3", 
                                 "SVM overall age" = "#e4d1d1", 
                                 "XGBoost overall age" = "#d9ecd0", 
                                 "RF overall age" = "#b9b0b0", 
                                 "DNN overall age" = "#77a8a8", 
                                 "Chronological age" = "#fee08b")) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      axis.ticks = element_line(color = "black"),
      plot.title = element_text(size = 22, hjust = 0.5),
      axis.title = element_text(size = 20),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 20, color = "black"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 20)
    ) +
    labs(title = title, x = "", y = "") +
    geom_text(aes(x = x_position, y = star_y_position, label = star_annotation), 
              size = ifelse(data$star_annotation == "Ref", 6, 8),
              color = "black", hjust = 0.5)
}


### plot single acc bar
plot_single_bar_acc <- function(data, title, p_values) {
  data$star_annotation <- ""
  
  for (i in 1:nrow(data)) {
    if (data$term[i] == "LLM overall age gap") {
      data$star_annotation[i] <- "Ref"
    } 
    else {
      p_value <- p_values[match(data$term[i], c("SVM overall age gap", 
                                                "RF overall age gap", 
                                                "XGBoost overall age gap", 
                                                "DNN overall age gap"))]
      data$star_annotation[i] <- ifelse(p_value < 0.001, "***", 
                                        ifelse(p_value < 0.01, "**", 
                                               ifelse(p_value < 0.05, "*", "")))
    }
  }
  
  x_position <- as.numeric(factor(data$term)) 
  
  data$star_y_position <- ifelse(data$estimate > 0, 
                                 data$conf.high + (data$conf.high - data$conf.low) * 0.25, 
                                 data$conf.low - (data$conf.high - data$conf.low) * 0.3) 
  
  # fig
  ggplot(data, aes(x = term, y = estimate, fill = term)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.1, position = position_dodge(width = 0.6)) +
    scale_fill_manual(values = c("LLM overall age gap" = "#4480B3", 
                                 "SVM overall age gap" = "#e4d1d1", 
                                 "XGBoost overall age gap" = "#d9ecd0", 
                                 "RF overall age gap" = "#b9b0b0", 
                                 "DNN overall age gap" = "#77a8a8")) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      axis.ticks = element_line(color = "black"),
      plot.title = element_text(size = 22, hjust = 0.5),
      axis.title = element_text(size = 20),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 20, color = "black"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 20)
    ) +
    labs(title = title, x = "", y = "") +

    geom_text(aes(x = x_position, y = star_y_position, label = star_annotation), 
              size = ifelse(data$star_annotation == "Ref", 6, 8), 
              color = "black", hjust = 0.5)
}


c_outcomes <- c("Heel bone mineral density",
                "Arterial stiffness index",
                "Adjusted telomere length",
                "Hearing problems",
                "Dental problems",
                "Slow walking pace",
                "Falls in the last year",
                "General pain",
                "Long standing illness",
                "Frailty index",
                "Overall health rating",
                "Reaction time")

### age
plots <- vector("list", length = 12)
for (i in 1:12) {
  outcome_name <- c_outcomes[i]
  dat_test <- subset(res_overall_age,
                     outcome == outcome_name)
  order_terms <- c("Chronological age", "SVM overall age", 
                   "RF overall age", "XGBoost overall age", 
                   "DNN overall age", "LLM overall age")
  dat_test$term <- factor(dat_test$term, levels = order_terms)
  dat_fdr <- subset(res_fdr, plot_order == i)
  p_ls <- dat_fdr$fdr
  plots[[i]] <- plot_single_bar(dat_test, outcome_name, p_ls)
}

### acc
plots <- vector("list", length = 12)
for (i in 1:12) {
  outcome_name <- c_outcomes[i]
  dat_test <- subset(res_overall_acc_combine,
                     outcome == outcome_name)
  order_terms <- c("SVM overall age gap", 
                   "RF overall age gap", "XGBoost overall age gap", 
                   "DNN overall age gap", "LLM overall age gap")
  dat_test$term <- factor(dat_test$term, levels = order_terms)
  dat_fdr <- subset(res_fdr_acc, plot_order == i)
  p_ls <- dat_fdr$fdr
  plots[[i]] <- plot_single_bar_acc(dat_test, outcome_name, p_ls)
}

arranged_plots <- ggarrange(plotlist = plots, 
                            ncol = 6,
                            nrow = 2,
                            heights = c(1, 1),
                            widths = rep(1, 6),
                            common.legend = TRUE, 
                            legend = "bottom")

# arranged_plots
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Beta", 
                                                  size = 24, rot = 90))

ggsave("fig2a_age_phenotype.pdf", plot = combined_plot, width = 22, height = 9)
ggsave("fig3a_acc_phenotype.pdf", plot = combined_plot, width = 22, height = 9)



######### ------ Age gaps are strong predictors for adverse outcomes onset ------
###### 2-1.compare Beta on aging-related phenotypes, age gap, UKB
### the codes are as above

###### 2-2.KM curve, UKB
library(arrow)
library(jsonlite)
library(survival)
library(survminer)
library(survcomp)
library(gridExtra)
library(pROC)
library(Hmisc)
library(rms)
library(hexbin)
library(caret)
library(scales)
library(grid)
library(ppcor)
library(tidyverse)
library(lubridate)
library(svglite)
###### prepare data, UKB
dat_age <- read_csv("Data/Models/llama3_70b/llama3-70b-result_only_age.csv")
dat_cov <- read_rds("Data/covariates_outcomes/panel_indicators.rds")
dat_outcome <- read_rds("Data/covariates_outcomes/overall_aging_outcomes.rds")

### merge data
dat_age <- dat_age %>% inner_join(dat_cov, by = "eid")
dat_age <- dat_age %>% inner_join(dat_outcome, by = "eid")

# calculate age gap
dat_age <- dat_age %>% mutate(llm_overall_acc = llm_overall_age - Age)
dat_age <- dat_age %>% mutate(llm_cardiovascular_acc = llm_cardiovascular_age - Age)
dat_age <- dat_age %>% mutate(llm_hepatic_acc = llm_hepatic_age - Age)
dat_age <- dat_age %>% mutate(llm_pulmonary_acc = llm_pulmonary_age - Age)
dat_age <- dat_age %>% mutate(llm_renal_acc = llm_renal_age - Age)
dat_age <- dat_age %>% mutate(llm_metabolic_acc = llm_metabolic_age - Age)
dat_age <- dat_age %>% mutate(llm_musculoskeletal_acc = llm_musculoskeletal_age - Age)
dat_age <- na.omit(dat_age)

# rank overall age gap
dat_age <- dat_age[order(dat_age$llm_overall_acc), ]
# calculate the boundaries of groups
n <- nrow(dat_age)
top_10_boundary <- n * 0.9
median_10_boundary_low <- n * 0.45
median_10_boundary_high <- n * 0.55
# assign label
dat_age$group <- "Other"
dat_age$group[1:(n * 0.1)] <- "Bottom 10%"
dat_age$group[(top_10_boundary+1):n] <- "Top 10%"
dat_age$group[(median_10_boundary_low+1):median_10_boundary_high] <- "Median 10%"

### disease
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")

# plots
plots <- list()

for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]
  
  # data exclude
  if (item == "CHD" | item == "Stroke") {
    dat_cox <- subset(dat_age, `MACE duration` > 0)
  }
  else if (item == "Renal failure") {
    dat_cox <- subset(dat_age, `Renal diseases duration` > 0)
  }
  else if (item == "T2D") {
    dat_cox <- subset(dat_age, `Diabetes duration` > 0)
  }
  else {
    dat_cox <- subset(dat_age, time > 0)
  }
  
  dat_cox <- subset(dat_cox, group == "Bottom 10%" | group == "Top 10%" | group == "Median 10%")
  
  # km curve
  fit <- survfit(Surv(time, event) ~ group, data = dat_cox)
  
  ggsurv <- ggsurvplot(fit,
                       data = dat_cox,
                       # pval = TRUE, 
                       conf.int = TRUE,
                       # risk.table = TRUE,
                       fun = "event",
                       xlab = "",
                       ylab = "",
                       xlim = c(0, 15),
                       palette = c("#90D3C7", "#80B1D3", "#ca0020"),
                       legend.title = "Overall age gap group",
                       legend.labs = c("Bottom 10%", "Median 10%", "Top 10%"),
                       legend = "bottom",
                       title = item,
                       ggtheme = theme_minimal())
  
  # other plot settings
  ggsurv$plot <- ggsurv$plot + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          # plot.margin = margin(10, 10, 10, 10),
          axis.line = element_line(),
          axis.title = element_text(size = 22),
          axis.text = element_text(size = 22, color = "black"),
          legend.title = element_text(size = 22),
          legend.text = element_text(size = 22),
          plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)) +
    scale_x_continuous(breaks = c(5, 10)) + 
    scale_y_continuous(labels = function(x) x * 100)
  
  plots[[i]] <- ggsurv$plot
}

# merge data plots
combined_plot <- ggarrange(plotlist = plots, 
                           ncol = 4, 
                           nrow = 2,
                           common.legend = TRUE, 
                           legend = "bottom")

# add axis
combined_plot <- annotate_figure(combined_plot,
                                 bottom = text_grob("Time (years)", size = 22),
                                 left = text_grob("Cumulative event (%)", size = 22, rot = 90))

ggsave("fig3b_age_gap_km.pdf", plot = combined_plot, width = 18, height = 9)



###### 2-3.organ-specific age gaps, UKB
Cox_analysis <- function(dat_baseline, disease_ls, var_ls) {
  disease_name_ls <- c()
  res_name_ls <- c()
  hr_ls <- c()
  conf_lower_ls <- c()
  conf_upper_ls <- c()
  pvalue_ls <- c()
  
  for (item in disease_ls) {
    item_diagnose <- paste0(item, " diagnose")
    item_duration <- paste0(item, " duration")
    dat_baseline$event <- dat_baseline[[item_diagnose]]
    dat_baseline$time <- dat_baseline[[item_duration]]
    
    # data exclude
    dat_cox <- subset(dat_baseline,
                      `MACE duration` > 0 &
                      `Renal failure duration` > 0 &
                      `T2D duration` > 0 &
                      `COPD duration` > 0 &
                      `Liver diseases duration` > 0 &
                      `Arthritis duration` > 0)
    
    for (i in 1:length(var_ls)) {
      var_name <- var_ls[i]
      formula_covariates <- paste0("survobj ~ Age + Sex + Income + Employment + 
                                    Education + UKB_assessment_center + Ethnicity +
                                   Current_smoker + Daily_alcohol_intake +
                                    BMI + Hypertension_history + ", var_name)
      f <- as.formula(formula_covariates)
      survobj <- with(dat_cox, Surv(time, event==1))
      
      cox_fit <- coxph(formula = f, data = dat_cox, na.action = na.omit)
      
      hr <- round(summary(cox_fit)$coefficients[var_name, "exp(coef)"], 3)
      conf_interval <- exp(confint(cox_fit)[var_name, ])
      conf_lower <- round(conf_interval[1], 3)
      conf_upper <- round(conf_interval[2], 3)
      p_value <- summary(cox_fit)$coefficients[var_name, "Pr(>|z|)"]
      
      disease_name_ls <- c(disease_name_ls, item)
      res_name_ls <- c(res_name_ls, var_name)
      hr_ls <- c(hr_ls, hr)
      conf_lower_ls <- c(conf_lower_ls, conf_lower)
      conf_upper_ls <- c(conf_upper_ls, conf_upper)
      pvalue_ls <- c(pvalue_ls, p_value)
      
      print(paste0(item, ": ", var_name, " Over!"))
    }
  }
  
  res <- data.frame(disease = disease_name_ls,
                    var = res_name_ls,
                    HR = hr_ls,
                    Lower = conf_lower_ls,
                    Upper = conf_upper_ls,
                    p_value = pvalue_ls)
  return(res)
}

disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")

# age gap
var_ls <- c("llm_cardiovascular_acc", "llm_hepatic_acc", "llm_pulmonary_acc",
            "llm_renal_acc", "llm_metabolic_acc", "llm_musculoskeletal_acc")

age_results_hr <- Cox_analysis(dat_baseline = dat_age,
                               disease_ls = disease,
                               var_ls = var_ls)

names(age_results_hr)[c(1, 2)] <- c("outcome", "var_name")
age_results_hr <- age_results_hr %>%
  mutate(var_name = case_when(
    var_name == "llm_cardiovascular_acc" ~ "Cardiovascular",
    var_name == "llm_hepatic_acc" ~ "Hepatic",
    var_name == "llm_pulmonary_acc" ~ "Pulmonary",
    var_name == "llm_renal_acc" ~ "Renal",
    var_name == "llm_metabolic_acc" ~ "Metabolic",
    var_name == "llm_musculoskeletal_acc" ~ "Musculoskeletal",
  ))

age_results_hr$var_name <- factor(age_results_hr$var_name, 
                                  levels = c("Cardiovascular", 
                                             "Hepatic", 
                                             "Pulmonary", 
                                             "Renal", 
                                             "Metabolic", 
                                             "Musculoskeletal"))

library(scales)
plots_HR <- list()

for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(age_results_hr, outcome == item)
  
  p <- ggplot(dat_sub, aes(x = var_name, y = HR, color = var_name)) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
    geom_segment(aes(x = 0, xend = var_name, y = HR, yend = HR),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% c(
                   "Cardiovascular",
                   "Hepatic",
                   "Pulmonary",
                   "Renal",
                   "Metabolic",
                   "Musculoskeletal"))) +
    scale_color_manual(values = c(
      "#E71D1D",
      "#4BB04A",
      "#FF7D01",
      "#F980BE",
      "#A35628",
      "#9850A6")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 22),
          axis.text.x = element_text(angle = 90, size = 22, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 22, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(limits = c(min(dat_sub$Lower) - 0.01, max(dat_sub$Upper) + 0.01), 
                       labels = scales::number_format(accuracy = 0.01))
  
  if (i < 5) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.line.x = element_blank())
  }
  plots_HR[[i]] <- p
}

arranged_plots <- ggarrange(plotlist = plots_HR, 
                            ncol = 4, 
                            nrow = 2,
                            heights = c(1, 1.8))

# arranged_plots
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Adjusted hazard ratio (HR)", 
                                                  size = 22, rot = 90))

ggsave("fig3c_acc_hr.pdf", plot = combined_plot, width = 18, height = 9)



###### 2-4.organ-overall age gaps, UKB
dat_age <- dat_age %>%
  dplyr::mutate(cardiovascular_overall_acc = llm_cardiovascular_age - llm_overall_age) %>%
  dplyr::mutate(hepatic_overall_acc = llm_hepatic_acc - llm_overall_age) %>%
  dplyr::mutate(pulmonary_overall_acc = llm_pulmonary_acc - llm_overall_age) %>%
  dplyr::mutate(renal_overall_acc = llm_renal_acc - llm_overall_age) %>%
  dplyr::mutate(metabolic_overall_acc = llm_metabolic_acc - llm_overall_age) %>%
  dplyr::mutate(musculoskeletal_overall_acc = llm_musculoskeletal_acc - llm_overall_age)

# disease
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")

# organ-overall age gap
var_ls <- c("cardiovascular_overall_acc", "hepatic_overall_acc",
            "pulmonary_overall_acc", "renal_overall_acc",
            "metabolic_overall_acc", "musculoskeletal_overall_acc")

age_results_hr <- Cox_analysis(dat_baseline = dat_age,
                               disease_ls = disease,
                               var_ls = var_ls)

names(age_results_hr)[c(1, 2)] <- c("outcome", "var_name")
age_results_hr <- age_results_hr %>%
  mutate(var_name = case_when(
    var_name == "cardiovascular_overall_acc" ~ "CV-Overall",
    var_name == "hepatic_overall_acc" ~ "Hep-Overall",
    var_name == "pulmonary_overall_acc" ~ "Pulm-Overall",
    var_name == "renal_overall_acc" ~ "Ren-Overall",
    var_name == "metabolic_overall_acc" ~ "Met-Overall",
    var_name == "musculoskeletal_overall_acc" ~ "MSK-Overall"
  ))

age_results_hr$var_name <- factor(age_results_hr$var_name, 
                                  levels = c("CV-Overall",
                                             "Hep-Overall",
                                             "Pulm-Overall",
                                             "Ren-Overall",
                                             "Met-Overall",
                                             "MSK-Overall"))

library(scales)
plots_HR <- list()

for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(age_results_hr, outcome == item)
  
  p <- ggplot(dat_sub, aes(x = var_name, y = HR, color = var_name)) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
    geom_segment(aes(x = 0, xend = var_name, y = HR, yend = HR),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% c("CV-Overall",
                                                        "Hep-Overall",
                                                        "Pulm-Overall",
                                                        "Ren-Overall",
                                                        "Met-Overall",
                                                        "MSK-Overall"))) +
    scale_color_manual(values = c(
      "#E71D1D",
      "#4BB04A",
      "#FF7D01",
      "#F980BE",
      "#A35628",
      "#9850A6")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 22),
          axis.text.x = element_text(angle = 90, size = 22, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 22, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(limits = c(min(dat_sub$Lower) - 0.01, max(dat_sub$Upper) + 0.01), 
                       labels = scales::number_format(accuracy = 0.01))
  
  if (i < 5) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.line.x = element_blank())
  }
  plots_HR[[i]] <- p
}

arranged_plots <- ggarrange(plotlist = plots_HR, 
                            ncol = 4, 
                            nrow = 2,
                            heights = c(1, 1.8))

# arranged_plots
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Adjusted hazard ratio (HR)", 
                                                  size = 22, rot = 90))

ggsave("fig3d_organ_overall_acc_hr.pdf", plot = combined_plot, width = 18, height = 8)




######### ------ Comparing performance of different LLMs ------
dat_age_llama3_70b <- read.csv("Data/Models/llama3_70b/llama3-70b-result_only_age.csv")
dat_age_llama3_70b <- dplyr::select(dat_age_llama3_70b, 1, 2)
names(dat_age_llama3_70b)[2] <- "llama3-70b biological age"
dat_age_llama3_70b$`llama3-70b biological age` <- as.numeric(dat_age_llama3_70b$`llama3-70b biological age`)
dat_age_llama3_70b <- na.omit(dat_age_llama3_70b)

dat_age_llama3_8b <- read.csv("Data/Models/llama3_8b/llama3-8b-result_only_age.csv")
dat_age_llama3_8b <- dplyr::select(dat_age_llama3_8b, 1, 2)
names(dat_age_llama3_8b)[2] <- "llama3-8b biological age"
dat_age_llama3_8b$`llama3-8b biological age` <- as.numeric(dat_age_llama3_8b$`llama3-8b biological age`)
dat_age_llama3_8b <- na.omit(dat_age_llama3_8b)

dat_age_qwen1.5_14b <- read.csv("Data/Models/qwen1.5_14b/qwen1.5-14b-result_only_age.csv")
dat_age_qwen1.5_14b <- dplyr::select(dat_age_qwen1.5_14b, 1, 2)
names(dat_age_qwen1.5_14b)[2] <- "qwen1.5-14b biological age"
dat_age_qwen1.5_14b$`qwen1.5-14b biological age` <- as.numeric(dat_age_qwen1.5_14b$`qwen1.5-14b biological age`)
dat_age_qwen1.5_14b <- na.omit(dat_age_qwen1.5_14b)

dat_age_qwen1.5_32b <- read.csv("Data/Models/qwen1.5_32b/qwen1.5-32b-result_only_age.csv")
dat_age_qwen1.5_32b <- dplyr::select(dat_age_qwen1.5_32b, 1, 2)
names(dat_age_qwen1.5_32b)[2] <- "qwen1.5-32b biological age"
dat_age_qwen1.5_32b$`qwen1.5-32b biological age` <- as.numeric(dat_age_qwen1.5_32b$`qwen1.5-32b biological age`)
dat_age_qwen1.5_32b <- na.omit(dat_age_qwen1.5_32b)

dat_age_qwen1.5_72b <- read.csv("Data/Models/qwen1.5_72b/qwen1.5-72b-result_only_age.csv")
dat_age_qwen1.5_72b <- dplyr::select(dat_age_qwen1.5_72b, 1, 2)
names(dat_age_qwen1.5_72b)[2] <- "qwen1.5-72b biological age"
dat_age_qwen1.5_72b$`qwen1.5-72b biological age` <- as.numeric(dat_age_qwen1.5_72b$`qwen1.5-72b biological age`)
dat_age_qwen1.5_72b <- na.omit(dat_age_qwen1.5_72b)

dat_age_qwen1.5_110b <- read.csv("Data/Models/qwen1.5_110b/qwen1.5-110b-result_only_age.csv")
dat_age_qwen1.5_110b <- dplyr::select(dat_age_qwen1.5_110b, 1, 2)
names(dat_age_qwen1.5_110b)[2] <- "qwen1.5-110b biological age"
dat_age_qwen1.5_110b$`qwen1.5-110b biological age` <- as.numeric(dat_age_qwen1.5_110b$`qwen1.5-110b biological age`)
dat_age_qwen1.5_110b <- na.omit(dat_age_qwen1.5_110b)

dat_age_qwen2_7b <- read.csv("Data/Models/qwen2_7b/qwen2-7b-result_only_age.csv")
dat_age_qwen2_7b <- dplyr::select(dat_age_qwen2_7b, 1, 2)
names(dat_age_qwen2_7b)[2] <- "qwen2-7b biological age"
dat_age_qwen2_7b$`qwen2-7b biological age` <- as.numeric(dat_age_qwen2_7b$`qwen2-7b biological age`)
dat_age_qwen2_7b <- na.omit(dat_age_qwen2_7b)

dat_age_qwen2_72b <- read.csv("Data/Models/qwen2_72b/qwen2-72b-result_only_age.csv")
dat_age_qwen2_72b <- dplyr::select(dat_age_qwen2_72b, 1, 2)
names(dat_age_qwen2_72b)[2] <- "qwen2-72b biological age"
dat_age_qwen2_72b$`qwen2-72b biological age` <- as.numeric(dat_age_qwen2_72b$`qwen2-72b biological age`)
dat_age_qwen2_72b <- na.omit(dat_age_qwen2_72b)

dat_age <- dat_age_llama3_8b %>%
  inner_join(dat_age_llama3_70b, by = "eid") %>%
  inner_join(dat_age_qwen1.5_14b, by = "eid") %>%
  inner_join(dat_age_qwen1.5_32b, by = "eid") %>%
  inner_join(dat_age_qwen1.5_72b, by = "eid") %>%
  inner_join(dat_age_qwen1.5_110b, by = "eid") %>%
  inner_join(dat_age_qwen2_7b, by = "eid") %>%
  inner_join(dat_age_qwen2_72b, by = "eid")

dat_cov <- read_rds("Data/covariates_outcomes/panel_indicators.rds")
dat_outcome <- read_rds("Data/covariates_outcomes/overall_aging_outcomes.rds")

### merge data
dat_age <- dat_age %>% inner_join(dat_cov, by = "eid")
dat_age <- dat_age %>% inner_join(dat_outcome, by = "eid")

# disease
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")

### test data
dat_svm <- read_csv("Data/Models/svm_res/test_svm_overall_res_250105.csv")
dat_svm <- dplyr::select(dat_svm, 1)
dat_age <- dat_age %>% inner_join(dat_svm, by = "eid")

# different llms
var_ls <- c("`llama3-8b biological age`", 
            "`llama3-70b biological age`", 
            "`qwen1.5-14b biological age`",
            "`qwen1.5-32b biological age`", 
            "`qwen1.5-72b biological age`",
            "`qwen1.5-110b biological age`",
            "`qwen2-7b biological age`",
            "`qwen2-72b biological age`")

var_mean_c_index <- c()
var_mean_c_index_lower <- c()
var_mean_c_index_upper <- c()
outcome_ls <- c()

# run
set.seed(2024)
for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]
  
  # exclude data
  if (item == "CHD" | item == "Stroke") {
    dat_cox <- subset(dat_age, `MACE duration` > 0)
  }
  else if (item == "Renal failure") {
    dat_cox <- subset(dat_age, `Renal diseases duration` > 0)
  }
  else if (item == "T2D") {
    dat_cox <- subset(dat_age, `Diabetes duration` > 0)
  }
  else {
    dat_cox <- subset(dat_age, time > 0) 
  }

  folds <- createFolds(dat_cox$event, k = 3)
  for (i in 1:length(var_ls)) {
    var <- var_ls[i]
    c_index_values <- c()
    c_index_lower_ls <- c()
    c_index_upper_ls <- c()
    
    for(j in 1:3) {
      # train and test split
      test_indices <- folds[[j]]
      train_data <- dat_cox[-test_indices, ]
      test_data <- dat_cox[test_indices, ]
      
      # Cox models
      formula_covariates <- paste0("survobj ~ ", var)
      f <- as.formula(formula_covariates)
      survobj <- with(train_data, Surv(time, event))
      cox_fit <- coxph(formula = f, data = train_data, na.action = na.omit)
      
      # predict risk
      test_data$predicted_risk <- predict(cox_fit, newdata = test_data, 
                                          type = "risk")
      
      # calculate c-index
      concordance_result <- concordance.index(x = test_data$predicted_risk,
                                              surv.time = test_data$time,
                                              surv.event = test_data$event)
      c_index <- concordance_result$c.index
      c_index_lower <- concordance_result$lower
      c_index_upper <- concordance_result$upper
      # 存储c-index
      c_index_values <- c(c_index_values, c_index)
      c_index_lower_ls <- c(c_index_lower_ls, c_index_lower)
      c_index_upper_ls <- c(c_index_upper_ls, c_index_upper)
      print(paste0(item, " ------------ ", var, " ------------ fold ", j))
    }
    mean_c_index <- round(mean(c_index_values), digits = 3)
    mean_c_index_lower <- round(mean(c_index_lower_ls), digits = 3)
    mean_c_index_upper <- round(mean(c_index_upper_ls), digits = 3)
    
    var_mean_c_index <- c(var_mean_c_index, mean_c_index)
    var_mean_c_index_lower <- c(var_mean_c_index_lower, mean_c_index_lower)
    var_mean_c_index_upper <- c(var_mean_c_index_upper, mean_c_index_upper)
    outcome_ls <- c(outcome_ls, item)
  }
}

dat_plot <- data.frame(
  outcome = outcome_ls,
  var_name = var_ls,
  c_index = var_mean_c_index,
  c_index_lower = var_mean_c_index_lower,
  c_index_upper = var_mean_c_index_upper
)

dat_plot <- dat_plot %>%
  mutate(var_name = case_when(
    var_name == "`llama3-8b biological age`" ~ "Llama3-8b",
    var_name == "`llama3-70b biological age`" ~ "Llama3-70b",
    var_name == "`qwen1.5-14b biological age`" ~ "Qwen1.5-14b",
    var_name == "`qwen1.5-32b biological age`" ~ "Qwen1.5-32b",
    var_name == "`qwen1.5-72b biological age`" ~ "Qwen1.5-72b",
    var_name == "`qwen1.5-110b biological age`" ~ "Qwen1.5-110b",
    var_name == "`qwen2-7b biological age`" ~ "Qwen2-7b",
    var_name == "`qwen2-72b biological age`" ~ "Qwen2-72b"
  ))

dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c("Llama3-8b", 
                                       "Llama3-70b", 
                                       "Qwen1.5-14b", 
                                       "Qwen1.5-32b",
                                       "Qwen1.5-72b", 
                                       "Qwen1.5-110b",
                                       "Qwen2-7b", 
                                       "Qwen2-72b"))

### plots
plots_c_index <- list()

for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(dat_plot, outcome == item)
  p <- ggplot(dat_sub, aes(x = var_name, y = c_index, color = var_name)) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = c_index_lower, ymax = c_index_upper), width = 0.2) +
    geom_segment(aes(x = 0, xend = var_name, y = c_index, yend = c_index),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% c("Llama3-8b", 
                                                        "Llama3-70b", 
                                                        "Qwen1.5-14b", 
                                                        "Qwen1.5-32b",
                                                        "Qwen1.5-72b", 
                                                        "Qwen1.5-110b",
                                                        "Qwen2-7b", 
                                                        "Qwen2-72b"))) +
    scale_color_manual(values = c("#4480B3", "#4480B3", 
                                  "#fdae6b", "#fdae6b", "#fdae6b", "#fdae6b",
                                  "#31a354", "#31a354")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 22),
          axis.text.x = element_text(angle = 90, size = 22, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 22, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
  
  if (i < 5) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.line.x = element_blank())
  }
  plots_c_index[[i]] <- p
}

arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 4, 
                            nrow = 2,
                            heights = c(1, 1.7))

# arranged_plots
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Absolute C-index", 
                                                  size = 22, rot = 90))

ggsave("fig4c_different_llms.pdf", plot = combined_plot, width = 20, height = 9)




######### ------ Identify proteomic biomarkers associated with accelerated aging ------
### 4-1. Load required libraries
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)
library(survcomp)
library(Hmisc)
library(ggrepel)
library(grid)
library(gridExtra)
library(arrow)
library(gtable)
library(enrichplot)
library(clusterProfiler)
library(limma)
library(org.Hs.eg.db)
library(DOSE)
library(ggplot2)
library(readxl)
library(data.table)

#############################################-
# Proteomics Differential Expression Analysis and Enrichment Analysis
#############################################-

# Read in the baseline data and proteomics data
dat_age <- read_csv("Data/Biological_Insight/ukb_baseline.csv")
dat_protein <- read_csv("Data/Biological_Insight/proteome_0.csv")

# Apply the median replacement function to each column of the proteomics data
dat_protein <- as.data.frame(apply(dat_protein, 2, replace_na_with_median))

# Merge datasets by the 'eid' column
dat_age <- dat_age %>% inner_join(dat_protein, by = "eid")

# Order the data by overall predicted age
dat_age <- dat_age[order(dat_age$llm_overall_acc), ]

# Calculate boundaries for grouping (top 10% and bottom 10%)
n <- nrow(dat_age)
top_boundary <- n * 0.9
bottom_boundary <- n * 0.1

# Assign group labels
dat_age$group <- "Other"
dat_age$group[1:(bottom_boundary + 1)] <- "Bottom"
dat_age$group[(top_boundary + 1):n] <- "Top"

# Extract the Top and Bottom groups and combine them for analysis
dat_age_high <- subset(dat_age, group == "Top")
dat_age_low  <- subset(dat_age, group == "Bottom")
dat_merge_analysis <- rbind(dat_age_high, dat_age_low)

# Define variables for further analysis
group <- factor(dat_merge_analysis$group)
Age   <- dat_merge_analysis$Age
Sex   <- factor(dat_merge_analysis$Sex)

#### Proteomics Differential Expression Analysis
# Remove unnecessary columns (here, columns 1 to 61 and column 1525 are removed)
dat_merge_analysis <- dat_merge_analysis %>% dplyr::select(-c(1:61, 1525))
# Transpose the data so that rows represent proteins
dat_merge_analysis <- t(dat_merge_analysis)

# Construct the design matrix
design <- model.matrix(~ 0 + group + Age + Sex)

# Create a contrast matrix: comparing Top vs. Bottom groups
contrast.matrix <- makeContrasts("groupTop-groupBottom", levels = design)

# Fit the linear model and perform differential expression analysis
fit <- lmFit(dat_merge_analysis, design)
fit2 <- contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit2)
results <- topTable(fit2, adjust = "BH", number = Inf)  # Get all results

#### GSEA Analysis: Based on the ranked list of all proteins
# Protein names are stored as the rownames of 'results'
protein_names <- rownames(results)

# Convert protein names to gene IDs using the bitr function
protein_to_gene <- bitr(protein_names, fromType = "SYMBOL", 
                        toType = "ENTREZID", OrgDb = org.Hs.eg.db)

# Add gene IDs to the results table
results <- results %>%
  rownames_to_column("Protein") %>%
  left_join(protein_to_gene, by = c("Protein" = "SYMBOL"))

# Remove entries that do not have a matching gene ID
results <- results[!is.na(results$ENTREZID), ]

# Prepare the gene list for GSEA using logFC values and sort in descending order
gene_list <- results$logFC
names(gene_list) <- results$ENTREZID
gene_list <- sort(gene_list, decreasing = TRUE)

# Optionally, select significant proteins (adj.P.Val < 0.01 & |logFC| > 0.2)
significant_proteins <- subset(results, adj.P.Val < 0.01 & abs(logFC) > 0.2)

# GSEA enrichment analysis using the GO database (ALL includes BP, CC, and MF)
GO_database <- 'org.Hs.eg.db'
GSEA_GO <- gseGO(
  geneList = gene_list,
  OrgDb = GO_database,
  ont = "ALL",
  keyType = "ENTREZID",
  minGSSize = 10,
  maxGSSize = 500,
  pvalueCutoff = 0.05,
  verbose = FALSE,
  seed = 2024
)

# Simplify the GSEA results to reduce redundant GO terms
GSEA_GO_SIMPLE <- simplify(GSEA_GO)
terms <- GSEA_GO_SIMPLE@result$ID[1:3]

# Define a function to plot a GSEA dot plot
# dotplotGsea <- function(gsea_result, topn = 25) {
#   p <- enrichplot::dotplot(gsea_result, showCategory = topn)
#   return(p)  # Return a ggplot object
# }
# Generate and save the dot plot
p_dot <- dotplotGsea(GSEA_GO_SIMPLE, topn = 20)
ggsave("fig5b_GSEA_Dotplot.pdf", plot = p_dot, width = 12, height = 6)

#############################################-
# 3. Proteomics Differential Expression Volcano Plot
#############################################-

# Set a minimum p-value to avoid -Inf when taking logarithms
min_p_value <- 5e-323
results$adj.P.Val <- pmax(results$adj.P.Val, min_p_value)

# Calculate -log10(FDR)
results <- results %>% mutate(Minus_Log10_FDR = -log10(adj.P.Val))

# Set a threshold for logFC
threshold <- 0.1

# Label significance based on logFC and FDR criteria
results <- results %>% mutate(Significance = case_when(
  logFC > 0 & adj.P.Val < 0.01 ~ "Upregulated",
  logFC < 0 & adj.P.Val < 0.01 ~ "Downregulated",
  TRUE ~ "Not Significant"
))

# Select the top upregulated and downregulated proteins for labeling (adjust numbers if needed)
top_n <- 30
bottom_n <- 15 
upregulated_labels <- results %>%
  filter(Significance == "Upregulated") %>%
  arrange(desc(abs(logFC)), Minus_Log10_FDR) %>%
  head(top_n)
downregulated_labels <- results %>%
  filter(Significance == "Downregulated") %>%
  arrange(desc(abs(logFC)), Minus_Log10_FDR) %>%
  head(bottom_n)
label_df <- bind_rows(upregulated_labels, downregulated_labels)

# Create the volcano plot using ggplot2
p_val <- ggplot(results, aes(x = logFC, y = Minus_Log10_FDR, color = Significance)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = c("Not Significant" = "grey",
                                "Upregulated" = "#d73027",
                                "Downregulated" = "#4575b4")) +
  geom_vline(xintercept = c(-threshold, threshold), col = "black", linetype = "dashed") +
  geom_hline(yintercept = -log10(0.01), col = "black", linetype = "dashed") +
  theme_minimal() +
  labs(x = "Log2 (Fold Change)",
       y = "-Log10 (FDR)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        panel.border = element_blank(),
        axis.line = element_line()) +
  geom_text_repel(data = label_df, 
                  aes(label = Protein), 
                  size = 4, 
                  box.padding = 0.3, 
                  point.padding = 0.3, 
                  segment.color = 'grey50') +
  scale_x_continuous(breaks = seq(-1, 1, by = 1), limits = c(-1.2, 1.8))

# Save the volcano plot
ggsave("Fig4a_volcano.pdf", plot = p_val, width = 7, height = 6)


###### 4-3. Venn Diagram
### 1. Export Protein Data
library(readxl)
library(data.table)

# Export the Protein column from "significant_proteins"
Protein <- as.data.frame(significant_proteins$Protein)

### 2. Process DunedinPACE Annotation
library(biomaRt)
library(minfi)
library(IlluminaHumanMethylation450kanno.ilmn12.hg19)

# Get the annotation for the Illumina 450k array
annotation <- getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19)

# Define the CpG IDs of interest
cpg_ids <- c("cg27165794", "cg27113548", "cg26981978", "cg26497348", "cg26470501",
             "cg26275850", "cg26264318", "cg26180383", "cg26094651", "cg25923729",
             "cg25772418", "cg25751054", "cg25683495", "cg25427524", "cg25368647",
             "cg25243766", "cg24891125", "cg24865132", "cg24737193", "cg24531955",
             "cg24403644", "cg24396875", "cg24352561", "cg24318091", "cg24268161",
             "cg24174557", "cg24125710", "cg23997508", "cg23966795", "cg23798387",
             "cg23152235", "cg23119487", "cg22912834", "cg22891595", "cg22488164",
             "cg22367678", "cg22157099", "cg22036538", "cg22029879", "cg21885107",
             "cg21787176", "cg21566642", "cg21486834", "cg21439498", "cg21355338",
             "cg20964064", "cg20025658", "cg18519757", "cg18513344", "cg18500368",
             "cg18181703", "cg17966227", "cg17901584", "cg17841545", "cg17804112",
             "cg17765387", "cg17749946", "cg17501210", "cg17460386", "cg17439800",
             "cg17061862", "cg17018786", "cg16969872", "cg16924010", "cg16739178",
             "cg16609260", "cg16416715", "cg16413763", "cg16098955", "cg15948836",
             "cg15919431", "cg15829969", "cg15815084", "cg15192750", "cg14893274",
             "cg14876603", "cg14851284", "cg14816825", "cg14702960", "cg14200569",
             "cg14110709", "cg13945148", "cg13702222", "cg13614083", "cg13586425",
             "cg13548189", "cg13300580", "cg13274938", "cg12820481", "cg12547807",
             "cg12446629", "cg12041401", "cg11835347", "cg11787522", "cg11452501",
             "cg11202345", "cg11103390", "cg11095122", "cg10919522", "cg10365246",
             "cg10053507", "cg10026495", "cg10017843", "cg09974965", "cg09933458",
             "cg09867945", "cg09760057", "cg09756865", "cg09626521", "cg09423875",
             "cg09349128", "cg09233395", "cg09022325", "cg08790676", "cg08335662",
             "cg08225398", "cg08011245", "cg07589381", "cg07550819", "cg07471256",
             "cg07244253", "cg07123481", "cg07075169", "cg07019072", "cg06961233",
             "cg06797880", "cg06570125", "cg06500161", "cg06458557", "cg06230206",
             "cg06182811", "cg05991820", "cg05671350", "cg05492170", "cg05487507",
             "cg05304729", "cg05239308", "cg05085844", "cg05068951", "cg04927537",
             "cg04624885", "cg04600984", "cg04565261", "cg04305539", "cg04105250",
             "cg04051458", "cg03868770", "cg03810769", "cg03776935", "cg03604011",
             "cg02997983", "cg02949067", "cg02650017", "cg02571857", "cg02307277",
             "cg02300147", "cg02229095", "cg02079413", "cg02004723", "cg01936220",
             "cg01554316", "cg01360413", "cg01101459", "cg01055871", "cg00835193",
             "cg00782811", "cg00668559", "cg00574958", "cg00532802", "cg00513564",
             "cg00359421", "cg00151250", "cg00112187")

# Subset the annotation to include only the CpG sites of interest
matched_genes <- as.data.frame(annotation[rownames(annotation) %in% cpg_ids, ])

# Extract gene names, split by ";" and remove duplicates
gene_names <- matched_genes$UCSC_RefGene_Name
split_genes <- unlist(strsplit(gene_names, ";"))
unique_genes <- as.data.frame(unique(split_genes))

### 3. Process Lehallier et al. (2019) Data
# Read nomenclature and linear modeling sheets from the Excel file
names_dt <- read_excel("Data/Biological_Insight/Review/41591_2019_673_MOESM3_ESM.xlsx",
                       col_names = TRUE, sheet = "ST1 Nomenclature 2,925 proteins")
setDT(names_dt)

protein_dt <- read_excel("Data/Biological_Insight/Review/41591_2019_673_MOESM3_ESM.xlsx",
                         col_names = TRUE, sheet = "ST4 Linear modeling - Human")
setDT(protein_dt)

# Merge by ID and filter for significant proteins (q.Age < 0.01)
protein_end <- merge(names_dt[, .(ID, EntrezGeneSymbol)], protein_dt[, .(ID, q.Age)], all = TRUE)
protein_end <- protein_end[q.Age < 0.01, .(EntrezGeneSymbol)]

# Split entries containing a dot into multiple rows and export results
protein_end <- protein_end[, .(EntrezGeneSymbol = unlist(strsplit(EntrezGeneSymbol, "\\."))), by = .I]
write.csv(protein_end, "Data/Biological_Insight/Review/Lehallier_protein.csv")

### 4. Venn Diagram Analysis
library(VennDiagram)
library(ggVennDiagram)

# Read proteomics data from Excel and set working directory
data <- read_excel("Data/Biological_Insight/蛋白组学.xlsx",
                   col_names = TRUE, sheet = "Sheet1")
setDT(data)

# Extract gene groups (removing NAs)
Group1 <- unique(na.omit(data$significant_proteins))
Group2 <- unique(na.omit(data$Proteomic_aging_clock))
Group3 <- unique(na.omit(data$Horvath_overlap))
Group4 <- unique(na.omit(data$PhenoAge_overlap))
Group5 <- unique(na.omit(data$DunedinPACE))
Group7 <- unique(na.omit(data$`Coenen et al.(2023)`))
Group8 <- unique(na.omit(data$`Johnson et al.(2020)`))
Group9 <- unique(na.omit(data$`Lehallier et al.(2019)`))

# Venn Diagram 1: Five gene sets
venn_list1 <- list(
  "LLMs aging" = Group1,
  "ProtAge" = Group2,
  "Horvath clock" = Group3,
  "DNAm PhenoAge" = Group4,
  "DunedinPACE" = Group5
)
VN1 <- ggVennDiagram(
  venn_list1,
  label = "count",
  label_size = 4,
  set_size = 4,
  edge_size = 0.5,
  set_color = "black"
) +
  scale_fill_gradient(low = "white", high = "skyblue") +
  coord_fixed(ratio = 0.9) +
  theme(
    legend.position = "none",
    plot.margin = unit(c(2, 2, 2, 2), "cm")
  )
pdf("VN.pdf", width = 6.5, height = 4.5)
print(VN1)
dev.off()

# Venn Diagram 2: Five gene sets with alternate groups
venn_list2 <- list(
  "LLMs aging" = Group1,
  "Coenen et al.(2023)" = Group7,
  "Johnson et al.(2020)" = Group8,
  "Lehallier et al.(2019)" = Group9,
  "Argentieri et al.(2024)" = Group2
)
VN2 <- ggVennDiagram(
  venn_list2,
  label = "count",
  label_size = 4,
  set_size = 4,
  edge_size = 0.5
) +
  scale_fill_gradient(low = "white", high = "skyblue") +
  coord_fixed(ratio = 0.9) +
  theme(
    legend.position = "none",
    plot.margin = unit(c(2, 2, 2, 2), "cm")
  )
pdf("VN2.pdf", width = 6.5, height = 4.5)
print(VN2)
dev.off()

# Extract intersection regions for venn_list1 and export to CSV
venn_obj1 <- Venn(venn_list1)
venn_region1 <- process_region_data(venn_obj1)
venn_region1$item <- sapply(venn_region1$item, function(x) paste(x, collapse = ", "))
venn_region1$id <- paste0("A", venn_region1$id)
write.csv(venn_region1, "venn1_classic.csv")

# Extract intersection regions for venn_list2 and export to CSV
venn_obj2 <- Venn(venn_list2)
venn_region2 <- process_region_data(venn_obj2)
venn_region2$item <- sapply(venn_region2$item, function(x) paste(x, collapse = ", "))
venn_region2$id <- paste0("A", venn_region2$id)
write.csv(venn_region2, "venn2_review.csv")

# Venn Diagram 3: Classic + Recent gene sets
venn_list3 <- list(
  "LLMs aging" = Group1,
  "ProtAge" = Group2,
  "Horvath clock" = Group3,
  "DNAm PhenoAge" = Group4,
  "Coenen et al.(2023)" = Group7,
  "Johnson et al.(2020)" = Group8,
  "Lehallier et al.(2019)" = Group9
)
VN3 <- ggVennDiagram(
  venn_list3,
  label = "count",
  label_size = 4,
  set_size = 4
) +
  scale_fill_gradient(low = "white", high = "skyblue") +
  coord_fixed(ratio = 0.9) +
  theme(
    legend.position = "none",
    plot.margin = unit(c(2, 2, 2, 2), "cm")
  )
# Export intersection regions for venn_list3 to CSV
venn_obj3 <- Venn(venn_list3)
venn_region3 <- process_region_data(venn_obj3)
venn_region3$item <- sapply(venn_region3$item, function(x) paste(x, collapse = ", "))
venn_region3$id <- paste0("A", venn_region3$id)
write.csv(venn_region3, "venn3_classic.csv")

pdf("VN3.pdf", width = 9, height = 7)
print(VN3)
dev.off()


###### 4-4. C-index
library(survival)
library(data.table)
library(readr)
library(glmnet)

# Set working directory and read in data
dat_age <- read_csv("Data/Biological_Insight/Cindex/data_age.csv")
setDT(dat_age)
dat_age <- dat_age[, -1]
setnames(dat_age, "HLA-DRA", "HLADRA")
setnames(dat_age, "HLA-E", "HLAE")

# Read in all-cause survival metrics for protein variables
allcause <- read.csv("Data/Biological_Insight/Cindex/LLM_organ_specific_cindex.csv")
setDT(allcause)

# Define the lasso_rank function
lasso_rank <- function(disease_list, dat_age, allcause) {
  library(survival)
  library(glmnet)
  library(data.table)

  # Initialize results list
  results_list <- list()
  set.seed(2024)

  for (item in disease_list) {
    cat("Processing:", item, "\n")

    # Construct column names for diagnosis and duration
    item_diagnose <- paste0(item, "_diagnose")
    item_duration <- paste0(item, "_duration")

    dat_age$event <- as.numeric(dat_age[[item_diagnose]])
    dat_age$time <- dat_age[[item_duration]]

    # Filter data based on disease-specific duration conditions
    if (item %in% c("CHD", "Stroke")) {
      dat_cox <- subset(dat_age, MACE_duration > 0 & time > 0)
    } else if (item == "Renal_failure") {
      dat_cox <- subset(dat_age, Renal_diseases_duration > 0 & time > 0)
    } else if (item == "T2D") {
      dat_cox <- subset(dat_age, Diabetes_duration > 0 & time > 0)
    } else {
      dat_cox <- subset(dat_age, time > 0)
    }

    # Check for invalid time values
    if (any(dat_cox$time <= 0)) {
      cat("Invalid time values detected for", item, ":\n")
      print(dat_cox[dat_cox$time <= 0, ])
      next
    }
    allcause_item <- allcause[outcome == item & c_index_lower >= 0.5, ]
    protein_columns <- intersect(names(dat_age), allcause_item$var_name)

    # Build model matrix
    x <- as.matrix(dat_cox[, ..protein_columns])
    y <- Surv(dat_cox$time, dat_cox$event)

    # Define alpha values to test
    alpha_values <- seq(0, 1, by = 0.1)
    cv_results <- list()

    # Perform cross-validation for each alpha value
    for (alpha in alpha_values) {
      cv_fit <- cv.glmnet(x, y, family = "cox", alpha = alpha, nfolds = 10)
      best_lambda <- cv_fit$lambda.min
      cvm <- cv_fit$cvm[cv_fit$lambda == cv_fit$lambda.min]
      cv_results[[as.character(alpha)]] <- list(
        alpha = alpha,
        lambda.min = best_lambda,
        cvm = cvm
      )
    }

    # Convert CV results to a data frame to determine the optimal alpha
    cv_results_df <- do.call(rbind, lapply(cv_results, data.frame))
    cv_results_df$alpha <- as.numeric(rownames(cv_results_df))
    cv_results_df$cvm <- as.numeric(cv_results_df$cvm)
    optimal_alpha <- cv_results_df$alpha[which.min(cv_results_df$cvm)]

    # Train final model using the optimal alpha and corresponding lambda
    final_model <- glmnet(x, y, family = "cox", alpha = optimal_alpha)
    optimal_lambda <- cv_results[[as.character(optimal_alpha)]]$lambda.min
    coefficients <- coef(final_model, s = optimal_lambda)

    # Extract non-zero coefficients and sort by absolute value
    coeff_matrix <- as.matrix(coefficients)
    coeff_df <- data.frame(
      Protein = rownames(coeff_matrix),
      Coefficient = coeff_matrix[, 1]
    )
    coeff_df <- coeff_df[coeff_df$Coefficient != 0, ]
    coeff_df <- coeff_df[order(abs(coeff_df$Coefficient), decreasing = TRUE), ]

    setDT(coeff_df)
    coeff_df[, outcome := item]
    coeff_df[, Rank_lasso := .I]

    # Save results for the current disease
    results_list[[item]] <- coeff_df
  }

  # Combine results from all diseases
  results_df <- rbindlist(results_list, fill = TRUE)
  return(results_df)
}

# Define disease list and run lasso_rank function
disease <- c("All-cause_death")
results <- lasso_rank(disease, dat_age, allcause)

write.csv(results, "results_allcause_lassoRank.csv")

###### 4-4. Elastic net regularization
library(survival)
library(caret)
library(Hmisc)
library(dplyr)
library(data.table)
library(readr)
library(limma)
library(survcomp)
library(readxl)
library(parallel)
library(future)
library(future.apply)
library(glmnet)

message("Libraries loaded", Sys.time())

# ---------------------------#
# Read Data
# ---------------------------#
dat_age <- read_csv("Data/Biological_Insight/Cindex/data_age.csv")
setDT(dat_age)
# Remove first column (e.g., an index) and rename columns for consistency
dat_age <- dat_age[, -1]
setnames(dat_age, "HLA-DRA", "HLADRA")
setnames(dat_age, "HLA-E", "HLAE")
message("dat_age loaded", Sys.time())

# Load protein ranking and all-cause survival metrics data
allprotein <- read_csv("allproteincindex.csv")
setDT(allprotein)

allcause <- read.csv("Data/Biological_Insight/Cindex/LLM_organ_specific_cindex.csv")
setDT(allcause)

# ---------------------------#
# Function Definitions
# ---------------------------#

# Parallel LASSO Cox Regression Ranking Function
lasso_rank_parallel <- function(disease_list, dat_age, allcause) {
  # Set up parallel processing with 15 workers
  plan(multisession, workers = 15)

  results_list <- future_lapply(disease_list, function(item) {
    # Load necessary packages explicitly within each worker
    library(survival)
    library(glmnet)
    library(data.table)

    cat("Processing:", item, "\n")

    # Construct diagnosis and duration column names
    item_diagnose <- paste0(item, "_diagnose")
    item_duration <- paste0(item, "_duration")

    dat_age$event <- as.numeric(dat_age[[item_diagnose]])
    dat_age$time <- dat_age[[item_duration]]

    # Filter data based on disease-specific conditions
    if (item %in% c("CHD", "Stroke")) {
      dat_cox <- subset(dat_age, MACE_duration > 0 & time > 0)
    } else if (item == "Renal_failure") {
      dat_cox <- subset(dat_age, Renal_diseases_duration > 0 & time > 0)
    } else if (item == "T2D") {
      dat_cox <- subset(dat_age, Diabetes_duration > 0 & time > 0)
    } else {
      dat_cox <- subset(dat_age, time > 0)
    }

    # Skip if any invalid time values are found
    if (any(dat_cox$time <= 0)) {
      cat("Invalid time values detected for", item, "\n")
      return(NULL)
    }

    # Get protein columns relevant to the current disease from the allcause table
    allcause_item <- allcause[outcome == item, ]
    protein_columns <- intersect(names(dat_age), allcause_item$var_name)

    # Build model matrix and survival object
    x <- as.matrix(dat_cox[, ..protein_columns])
    y <- Surv(dat_cox$time, dat_cox$event)

    # Define a sequence of alpha values to test
    alpha_values <- seq(0, 1, by = 0.1)
    cv_results <- list()

    # Perform cross-validation for each alpha
    for (alpha in alpha_values) {
      cv_fit <- cv.glmnet(x, y, family = "cox", alpha = alpha, nfolds = 10)
      best_lambda <- cv_fit$lambda.min
      cvm <- cv_fit$cvm[cv_fit$lambda == best_lambda]
      cv_results[[as.character(alpha)]] <- list(alpha = alpha, lambda.min = best_lambda, cvm = cvm)
    }

    # Determine optimal alpha (minimizing cross-validation error)
    cv_results_df <- do.call(rbind, lapply(cv_results, data.frame))
    cv_results_df$alpha <- as.numeric(rownames(cv_results_df))
    cv_results_df$cvm <- as.numeric(cv_results_df$cvm)
    optimal_alpha <- cv_results_df$alpha[which.min(cv_results_df$cvm)]

    # Train final model using the optimal alpha and lambda
    final_model <- glmnet(x, y, family = "cox", alpha = optimal_alpha)
    optimal_lambda <- cv_results[[as.character(optimal_alpha)]]$lambda.min
    coefficients <- coef(final_model, s = optimal_lambda)

    # Extract nonzero coefficients and rank them by absolute value
    coeff_matrix <- as.matrix(coefficients)
    coeff_df <- data.frame(Protein = rownames(coeff_matrix), Coefficient = coeff_matrix[, 1])
    coeff_df <- coeff_df[coeff_df$Coefficient != 0, ]
    coeff_df <- coeff_df[order(abs(coeff_df$Coefficient), decreasing = TRUE), ]

    setDT(coeff_df)
    coeff_df[, outcome := item]
    coeff_df[, Rank_lasso := .I]

    return(coeff_df)
  },
  future.globals = list(dat_age = dat_age, allcause = allcause),
  future.packages = c("survival", "glmnet", "data.table"),
  future.seed = TRUE)

  results_df <- rbindlist(results_list, fill = TRUE)
  return(results_df)
}

# Cross-validated Cox Model Evaluation Function
perform_cox_cross_validation <- function(var_ls, disease, data) {
  var_mean_c_index <- c()
  var_mean_c_index_lower <- c()
  var_mean_c_index_upper <- c()
  formula_ls <- c()
  outcome_ls <- c()
  num_vars <- c()  # Number of variables used

  set.seed(2024)

  for (item in disease) {
    # Construct column names for diagnosis and duration
    item_diagnose <- paste0(item, "_diagnose")
    item_duration <- paste0(item, "_duration")

    if (!(item_diagnose %in% colnames(data))) stop(paste("Column", item_diagnose, "not found!"))
    if (!(item_duration %in% colnames(data))) stop(paste("Column", item_duration, "not found!"))

    data$event <- data[[item_diagnose]]
    data$time <- data[[item_duration]]

    # Subset data with valid times
    dat_cox <- subset(data, time > 0)
    if (nrow(dat_cox) == 0) {
      message(paste("No data available for disease:", item))
      next
    }

    # 10-fold cross-validation
    folds <- createFolds(dat_cox$event, k = 10)
    c_index_values <- c()
    c_index_lower_ls <- c()
    c_index_upper_ls <- c()

    for (k in 1:10) {
      test_indices <- folds[[k]]
      train_data <- dat_cox[-test_indices, ]
      test_data <- dat_cox[test_indices, ]

      # Build Cox model formula
      formula <- as.formula(paste("Surv(time, event) ~", var_ls))

      tryCatch({
        cox_fit <- coxph(formula = formula, data = train_data, na.action = na.omit)
        test_data$predicted_risk <- predict(cox_fit, newdata = test_data, type = "risk")
        concordance_result <- concordance.index(
          x = test_data$predicted_risk,
          surv.time = test_data$time,
          surv.event = test_data$event
        )
        c_index_values <- c(c_index_values, concordance_result$c.index)
        c_index_lower_ls <- c(c_index_lower_ls, concordance_result$lower)
        c_index_upper_ls <- c(c_index_upper_ls, concordance_result$upper)
      }, error = function(e) {
        message(paste("Error in Cox model for formula:", var_ls, "on fold", k))
      })
    }

    mean_c_index <- round(mean(c_index_values, na.rm = TRUE), 3)
    mean_c_index_lower <- round(mean(c_index_lower_ls, na.rm = TRUE), 3)
    mean_c_index_upper <- round(mean(c_index_upper_ls, na.rm = TRUE), 3)

    var_mean_c_index <- c(var_mean_c_index, mean_c_index)
    var_mean_c_index_lower <- c(var_mean_c_index_lower, mean_c_index_lower)
    var_mean_c_index_upper <- c(var_mean_c_index_upper, mean_c_index_upper)
    formula_ls <- c(formula_ls, var_ls)
    outcome_ls <- c(outcome_ls, item)
    num_vars <- c(num_vars, length(strsplit(var_ls, " \\+ ")[[1]]))
  }

  dat_plot <- data.frame(
    outcome = outcome_ls,
    formula = formula_ls,
    c_index = var_mean_c_index,
    c_index_lower = var_mean_c_index_lower,
    c_index_upper = var_mean_c_index_upper,
    num = num_vars
  )

  return(dat_plot)
}

# Dynamic Protein Variable Combination Analysis Function
analyze_protein <- function(disease, column_name, output_file, allcause, allprotein, dat_age, column_value = 1) {
  if (is.null(allcause)) stop("allcause dataset must be provided")
  if (is.null(allprotein)) stop("allprotein dataset must be provided")
  if (is.null(dat_age)) stop("dat_age dataset must be provided")

  # Filter allcause for the specified disease and merge with allprotein data
  allcause <- allcause[outcome == disease, ]
  allprotein <- merge(allprotein, allcause[, .(Protein, Rank_lasso)],
                      by.x = "all_protein", by.y = "Protein", all.x = TRUE)
  setDT(allprotein)
  allprotein <- allprotein[order(allprotein$Rank_lasso), ]
  allprotein <- allprotein[!is.na(Rank_lasso), ]

  # Get variable combinations for rows where column_name equals column_value
  var_ls_combinations <- lapply(1:nrow(allprotein[get(column_name) == column_value]), function(i) {
    allprotein[get(column_name) == column_value, all_protein[1:i]]
  })

  # Select indices for combinations
  n <- length(var_ls_combinations)
  selected_indices <- unique(c(1:min(40, n), if (n > 40) seq(40, min(100, n), by = 5) else numeric(0)))
  var_ls_combinations <- var_ls_combinations[selected_indices]

  # Set up parallel processing (3 workers)
  options(future.globals.maxSize = 2 * 1024^3)
  plan(multisession, workers = 3)

  results_list <- future_lapply(seq_along(var_ls_combinations), function(idx) {
    start_time <- Sys.time()
    message(sprintf("Processing combination %d of %d", idx, length(var_ls_combinations)))

    var_ls <- var_ls_combinations[[idx]]
    var_ls_formula <- paste(var_ls, collapse = " + ")

    result <- tryCatch({
      perform_cox_cross_validation(var_ls = var_ls_formula, disease = disease, data = dat_age)
    }, error = function(e) {
      message(sprintf("Error in combination %d: %s", idx, e$message))
      return(NULL)
    })

    end_time <- Sys.time()
    message(sprintf("Combination %d completed in %f seconds", idx, as.numeric(end_time - start_time)))
    return(result)
  },
  future.globals = list(
    dat_age = dat_age,
    perform_cox_cross_validation = perform_cox_cross_validation,
    var_ls_combinations = var_ls_combinations
  ),
  future.packages = c("caret", "survival", "survcomp"),
  future.seed = TRUE)

  results_list <- results_list[!sapply(results_list, is.null)]
  results_df <- do.call(rbind, results_list)
  write.csv(results_df, output_file)
  message(sprintf("Results saved to %s", output_file))
}

# ---------------------------#
# Run Analyses
# ---------------------------#

# Run Parallel LASSO Ranking Analysis
disease_list <- c("All-cause_death", "CHD", "Stroke", "COPD", 
                  "Liver_diseases", "Renal_failure", "T2D", "Arthritis")
lasso_results <- lasso_rank_parallel(disease_list, dat_age, allcause)
write.csv(lasso_results, "Data/Biological_Insight/Cindex/renosingcox/results_lassoRank.csv")
message("Parallel LASSO ranking completed.", Sys.time())

# Run Protein Variable Combination Analysis
# For each disease and corresponding protein grouping, run the analysis.
# Adjust the column_name values and output file names as needed.

analyze_protein("All-cause_death", "LLM_aging_overall_protein", "CV_All_LLM.csv",
                allcause, allprotein, dat_age)
analyze_protein("All-cause_death", "Proteomic_aging_clock", "CV_All_Pro.csv",
                allcause, allprotein, dat_age)
analyze_protein("All-cause_death", "Horvath_overlap", "CV_All_Hor.csv",
                allcause, allprotein, dat_age)
analyze_protein("All-cause_death", "PhenoAge_overlap", "CV_All_Phe.csv",
                allcause, allprotein, dat_age)

analyze_protein("CHD", "LLM_aging_cardiovascular", "CV_CHD_LLM.csv",
                allcause, allprotein, dat_age)
analyze_protein("COPD", "LLM_aging_pulmonary", "CV_COPD_LLM.csv",
                allcause, allprotein, dat_age)
analyze_protein("Liver_diseases", "LLM_aging_hepatic", "CV_Liver_LLM.csv",
                allcause, allprotein, dat_age)
analyze_protein("Renal_failure", "LLM_aging_renal", "CV_Renal_LLM.csv",
                allcause, allprotein, dat_age)
analyze_protein("Stroke", "LLM_aging_cardiovascular", "CV_Stroke_LLM.csv",
                allcause, allprotein, dat_age)
analyze_protein("Arthritis", "LLM_aging_musculoskeletal", "CV_Arthritis_LLM.csv",
                allcause, allprotein, dat_age)
analyze_protein("T2D", "LLM_aging_metabolic", "CV_T2D_LLM.csv",
                allcause, allprotein, dat_age)

analyze_protein("CHD", "Proteomic_aging_clock", "CV_CHD_Pro.csv",
                allcause, allprotein, dat_age)
analyze_protein("CHD", "Horvath_overlap", "CV_CHD_Hor.csv",
                allcause, allprotein, dat_age)
analyze_protein("CHD", "PhenoAge_overlap", "CV_CHD_Phe.csv",
                allcause, allprotein, dat_age)

analyze_protein("COPD", "Proteomic_aging_clock", "CV_COPD_Pro.csv",
                allcause, allprotein, dat_age)
analyze_protein("COPD", "Horvath_overlap", "CV_COPD_Hor.csv",
                allcause, allprotein, dat_age)
analyze_protein("COPD", "PhenoAge_overlap", "CV_COPD_Phe.csv",
                allcause, allprotein, dat_age)

analyze_protein("Liver_diseases", "Proteomic_aging_clock", "CV_Liver_Pro.csv",
                allcause, allprotein, dat_age)
analyze_protein("Liver_diseases", "Horvath_overlap", "CV_Liver_Hor.csv",
                allcause, allprotein, dat_age)
analyze_protein("Liver_diseases", "PhenoAge_overlap", "CV_Liver_Phe.csv",
                allcause, allprotein, dat_age)

analyze_protein("Renal_failure", "Proteomic_aging_clock", "CV_Renal_Pro.csv",
                allcause, allprotein, dat_age)
analyze_protein("Renal_failure", "Horvath_overlap", "CV_Renal_Hor.csv",
                allcause, allprotein, dat_age)
analyze_protein("Renal_failure", "PhenoAge_overlap", "CV_Renal_Phe.csv",
                allcause, allprotein, dat_age)

analyze_protein("Stroke", "Proteomic_aging_clock", "CV_Stroke_Pro.csv",
                allcause, allprotein, dat_age)
analyze_protein("Stroke", "Horvath_overlap", "CV_Stroke_Hor.csv",
                allcause, allprotein, dat_age)
analyze_protein("Stroke", "PhenoAge_overlap", "CV_Stroke_Phe.csv",
                allcause, allprotein, dat_age)

analyze_protein("Arthritis", "Proteomic_aging_clock", "CV_Arthritis_Pro.csv",
                allcause, allprotein, dat_age)
analyze_protein("Arthritis", "Horvath_overlap", "CV_Arthritis_Hor.csv",
                allcause, allprotein, dat_age)
analyze_protein("Arthritis", "PhenoAge_overlap", "CV_Arthritis_Phe.csv",
                allcause, allprotein, dat_age)

analyze_protein("T2D", "Proteomic_aging_clock", "CV_T2D_Pro.csv",
                allcause, allprotein, dat_age)
analyze_protein("T2D", "Horvath_overlap", "CV_T2D_Hor.csv",
                allcause, allprotein, dat_age)
analyze_protein("T2D", "PhenoAge_overlap", "CV_T2D_Phe.csv",
                allcause, allprotein, dat_age)

message("All analyses completed.", Sys.time())



######### ------ Model interpretation ------
###### 1.shap
# install.packages("iml")
library(iml)
library(MASS)
library(caret)

dat_age <- read_csv("Data/Models/llama3_70b_interpretation/llama3-70B-shap-analyisis.csv")
dat_age <- dplyr::select(dat_age, 1, 4)
dat_cov <- read_rds("Data/covariates_outcomes/panel_indicators.rds")
dat_age <- dat_age %>% inner_join(dat_cov, by = "eid")
dat_age$llm_overall_acc <- dat_age$llm_overall_age - dat_age$Age

dat_age <- dat_age %>%
  dplyr::mutate(Current_smoker = case_when(
    Current_smoker == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(Daily_alcohol_intake = case_when(
    Daily_alcohol_intake == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(Daily_healthy_food = case_when(
    Daily_healthy_food == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(Frequently_processed_meat = case_when(
    Frequently_processed_meat == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(Frequently_red_meat = case_when(
    Frequently_red_meat == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(Frequently_salt_intake = case_when(
    Frequently_salt_intake == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(Family_cardiovascular_disease_history = case_when(
    Family_cardiovascular_disease_history == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(Family_diabetes_history = case_when(
    Family_diabetes_history == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(Hypertension_history = case_when(
    Hypertension_history == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(Diabetes_history = case_when(
    Diabetes_history == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(Hypotensive_drugs = case_when(
    Hypotensive_drugs == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(Insulin = case_when(
    Insulin == 1 ~ "yes",
    TRUE ~ "no"
  ))

dat_age$Current_smoker <- factor(dat_age$Current_smoker)
dat_age$Daily_alcohol_intake <- factor(dat_age$Daily_alcohol_intake)
dat_age$Daily_healthy_food <- factor(dat_age$Daily_healthy_food)
dat_age$Frequently_processed_meat <- factor(dat_age$Frequently_processed_meat)
dat_age$Frequently_red_meat <- factor(dat_age$Frequently_red_meat)
dat_age$Frequently_salt_intake <- factor(dat_age$Frequently_salt_intake)
dat_age$Family_cardiovascular_disease_history <- factor(dat_age$Family_cardiovascular_disease_history)
dat_age$Family_diabetes_history <- factor(dat_age$Family_diabetes_history)
dat_age$Hypertension_history <- factor(dat_age$Hypertension_history)
dat_age$Diabetes_history <- factor(dat_age$Diabetes_history)
dat_age$Hypotensive_drugs <- factor(dat_age$Hypotensive_drugs)
dat_age$Insulin <- factor(dat_age$Insulin)

# linear model
dat_age_shap <- dplyr::select(dat_age, 3:48)
old_names <- colnames(dat_age_shap)
old_names <- gsub(" \\(HbA1c\\)| \\(erythrocyte\\)| \\(leukocyte\\)", "", old_names)
old_names <- gsub(" |-", "_", old_names)
names(dat_age_shap) <- old_names

# fit linear model
model <- lm(llm_overall_acc ~ ., data = dat_age_shap)

# predict function
predict_function <- function(model, newdata) {
  predict(model, newdata)
}

# predictor
predictor <- Predictor$new(
  model = model, 
  data = dat_age_shap[ , -46],
  y = dat_age_shap$llm_overall_acc, 
  predict.function = predict_function
)

### 1.individual analysis
# individual shap analysis
shapley <- Shapley$new(predictor, x.interest = dat_age_shap[1, -46])
shap_values <- shapley$results

shap_values_df <- data.frame(
  feature = shap_values$feature.value,
  shap_value = shap_values$phi
)

old_feature <- shap_values_df$feature
old_feature <- gsub("_", " ", old_feature)
old_feature <- gsub("=", " = ", old_feature)
shap_values_df$feature <- old_feature

# rename variable
shap_values_df$feature <- gsub("Daily moderate to vigorous physical activity",
                               "Daily MVPA",
                               shap_values_df$feature)
shap_values_df$feature <- gsub("Family cardiovascular disease history",
                               "Family CVD history",
                               shap_values_df$feature)
shap_values_df$feature <- gsub("Mean corpuscular haemoglobin concentration",
                               "MCHC",
                               shap_values_df$feature)

# bar plot
p <- ggplot(shap_values_df, 
            aes(x = reorder(feature, shap_value), 
                y = shap_value, fill = shap_value)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "#92a8d1", high = "#c94c4c") +
  labs(title = "SHAP values for a single participant",
       x = "Features",
       y = "SHAP value") +
  theme_minimal() +
  theme(panel.border = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 22, color = "black"),
        axis.text.y = element_text(size = 22, color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "none",
        plot.title = element_text(size = 26, hjust = 0, vjust = 2))

ggsave("fig6c_shap_individual.pdf", p, width = 12, height = 12)


### 2.global analysis
feature_imp <- FeatureImp$new(predictor, loss = "mae")
feature_imp_df <- feature_imp$results

old_feature <- feature_imp_df$feature
old_feature <- gsub("_", " ", old_feature)
feature_imp_df$feature <- old_feature

# rename variable
feature_imp_df$feature <- gsub("Daily moderate to vigorous physical activity",
                               "Daily MVPA",
                               feature_imp_df$feature)
feature_imp_df$feature <- gsub("Family cardiovascular disease history",
                               "Family CVD history",
                               feature_imp_df$feature)
feature_imp_df$feature <- gsub("Mean corpuscular haemoglobin concentration",
                               "MCHC",
                               feature_imp_df$feature)

# dot plot
p <- ggplot(feature_imp_df, aes(x = reorder(feature, importance), 
                                y = importance)) +
  geom_point(size = 5, color = "#e6550d") + 
  coord_flip() +
  labs(title = "Feature importance based on SHAP values",
       x = "Features",
       y = "Importance (MAE)") +
  theme_minimal() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title = element_text(size = 22),
    axis.text.x = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none",
    plot.title = element_text(size = 26, hjust = 0, vjust = 2)
  )

ggsave("fig6d_shap_global.pdf", p, width = 12, height = 12)



###### 3.global surrogate model
dat_age <- dplyr::select(dat_age, 2:48)
old_names <- colnames(dat_age)
old_names <- gsub(" \\(HbA1c\\)| \\(erythrocyte\\)| \\(leukocyte\\)", "", old_names)
old_names <- gsub(" |-", "_", old_names)
names(dat_age) <- old_names

names(dat_age)[c(11,12,45)] <- c("Daily_MVPA", "Family_CVD_history", "MCHC")
dat_age$llm_overall_acc <- NULL

### construct linear model as global surrogate model
lm_model <- lm(
  formula = as.formula(paste("llm_overall_age", "~ .")),
  data = dat_age
)
summary(lm_model)
# beta
conf_interval <- confint(lm_model, level = 0.95)

lm_summary <- summary(lm_model)
coef_info <- as.data.frame(coef(lm_summary))
coef_info$feature <- rownames(coef_info)
colnames(coef_info) <- c("coefficient", "std_error", "t_value", "p_value", 
                         "feature")

# p value
coef_info <- coef_info %>%
  mutate(significance = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    TRUE            ~ ""
  ))

coef_info <- coef_info %>%
  filter(feature != "(Intercept)")

# color
coef_info$color <- ifelse(coef_info$coefficient > 0, "#92a8d1", "#c94c4c")

old_feature <- coef_info$feature
old_feature <- gsub("_", " ", old_feature)
old_feature[2] <- "Sex (male)"
old_feature[3] <- "Education (below undergraduate)"
old_feature[4] <- "Education (college)"
old_feature[5] <- "Current smoker (yes)"
old_feature[6] <- "Daily alcohol intake (yes)"
old_feature[7] <- "Daily healthy food (yes)"
old_feature[8] <- "Frequently processed meat (yes)"
old_feature[9] <- "Frequently red meat (yes)"
old_feature[10] <- "Frequently salt intake (yes)"
old_feature[12] <- "Family CVD history (yes)"
old_feature[13] <- "Family diabetes history (yes)"
old_feature[14] <- "Hypertension history (yes)"
old_feature[15] <- "Diabetes history (yes)"
old_feature[16] <- "Hypotensive drugs (yes)"
old_feature[17] <- "Insulin (yes)"

coef_info$feature <- old_feature

# bar plot
p <- ggplot(coef_info, aes(x = reorder(feature, coefficient), 
                           y = coefficient, 
                           fill = color)) +
  geom_bar(stat = "identity") +
  geom_text(aes(
    label = paste(round(coefficient, 3), significance), 
    x = feature, 
    y = coefficient, 
    hjust = ifelse(coefficient > 0, -0.2, 1.2)
  ), size = 6, color = "black") + 
  scale_fill_identity() + 
  coord_flip() +
  labs(title = "Global surrogate model (Linear model)",
       x = "Features",
       y = "Regression coefficient") +
  theme_minimal() +
  theme(panel.border = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 20, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "none",
        plot.title = element_text(size = 24, hjust = 0, vjust = 2)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2)))

ggsave("fig6e_global_surrogate_model.pdf", p, width = 13, height = 12)



###### 4.counterfactual simulation
# install.packages("effsize")
library(effsize)
dat_analysis <- read_csv("Data/Models/llama3_70b_interpretation/llama3-70B-counterfactual-analyisis.csv")
dat_analysis <- dplyr::select(dat_analysis, 1, 4)
names(dat_analysis)[2] <- "old_BA"
dat_cov <- read_rds("Data/covariates_outcomes/panel_indicators.rds")
dat_telomere <- read_csv("Data/covariates_outcomes/telomere.csv")
dat_telomere <- select(dat_telomere, 1:2, 5)
names(dat_telomere)[c(2, 3)] <- c("telomere_adjusted", "z_adjusted_telomere")
dat_telomere <- na.omit(dat_telomere)
dat_analysis <- dat_analysis %>% inner_join(dat_cov, by = "eid")
dat_analysis$acc <- dat_analysis$old_BA - dat_analysis$Age
dat_analysis <- dat_analysis %>% inner_join(dat_telomere, by = "eid")

### Traverse the folder
folder_path <- "Data/Models/llama3_70b_interpretation/perturbation"
file_names <- list.files(folder_path, full.names = FALSE)
file_names <- gsub(".csv", "", file_names)

file_ls <- c()
var_ls <- c()
cohen_d_estimate_ls <- c()
cohen_d_lower_ls <- c()
cohen_d_upper_ls <- c()
cohen_d_p_ls <- c()
beta_ls <- c()
beta_lower_ls <- c()
beta_upper_ls <- c()
beta_p_ls <- c()

for (file in file_names) {
  file_path <- paste0(folder_path, "/", file, ".csv")
  dat_var <- read_csv(file_path)
  dat_var <- dplyr::select(dat_var, 1, 4) # after perturbation
  names(dat_var)[2] <- "perturbation_BA" # perturbation BA
  dat_cov_cohen <- dplyr::select(dat_cov, 1, 2)
  dat_var <- dat_var %>% inner_join(dat_cov_cohen, by = "eid")
  
  ### Cohen's d
  dat_var$perturbation_acc <- dat_var$perturbation_BA - dat_var$Age
  dat_var <- dplyr::select(dat_var, 1, 4)
  # merge dat_analysis
  dat_var_cohen <- dat_var
  dat_var_cohen <- dat_var_cohen %>% inner_join(dat_analysis, by = "eid")
  # calculate
  cohen_d <- cohen.d(dat_var_cohen$perturbation_acc, 
                     dat_var_cohen$acc, 
                     paired = TRUE)
  cohen_d_estimate <- round(cohen_d$estimate, 3)
  cohen_d_lower <- round(cohen_d$conf.int["lower"], 3)
  cohen_d_upper <- round(cohen_d$conf.int["upper"], 3)
  t_test_result <- t.test(dat_var_cohen$perturbation_acc, 
                          dat_var_cohen$acc, 
                          paired = TRUE)
  cohen_d_p <- t_test_result$p.value
  
  ###### regression 
  dat_linear <- dat_analysis %>% semi_join(dat_var, by = "eid")
  dat_linear$group <- "old_var"
  dat_linear <- dplyr::select(dat_linear, 1, 50, 53)
  # perturbation data
  dat_var_linear <- dat_var
  names(dat_var_linear)[2] <- "acc"
  dat_var_linear$group <- "new_var"
  # merge
  dat_linear <- rbind(dat_linear, dat_var_linear)
  dat_linear$group <- factor(dat_linear$group, levels = c("old_var", "new_var"))
  dat_linear <- dat_linear %>% inner_join(dat_cov, by = "eid")
  model <- lm(acc ~ group + Age + Sex + Income + Employment + Education, dat_linear)
  # extract results
  beta <- round(model$coefficients["groupnew_var"], 3)
  beta_lower <- round(confint(model)["groupnew_var","2.5 %"], 3)
  beta_upper <- round(confint(model)["groupnew_var","97.5 %"], 3)
  beta_p <- summary(model)$coefficients["groupnew_var","Pr(>|t|)"]
  
  var_name <- gsub("dat_|_yes_2_no|_no_2_yes", "", file)
  # p value
  cohen_d_p <- ifelse(cohen_d_p < 0.001, "<0.001", as.character(round(cohen_d_p, 3)))
  beta_p <- ifelse(beta_p < 0.001, "<0.001", as.character(round(beta_p, 3)))
  
  file_ls <- c(file_ls, file)
  var_ls <- c(var_ls, var_name)
  cohen_d_estimate_ls <- c(cohen_d_estimate_ls, cohen_d_estimate)
  cohen_d_lower_ls <- c(cohen_d_lower_ls, cohen_d_lower)
  cohen_d_upper_ls <- c(cohen_d_upper_ls, cohen_d_upper)
  cohen_d_p_ls <- c(cohen_d_p_ls, cohen_d_p)
  beta_ls <- c(beta_ls, beta)
  beta_lower_ls <- c(beta_lower_ls, beta_lower)
  beta_upper_ls <- c(beta_upper_ls, beta_upper)
  beta_p_ls <- c(beta_p_ls, beta_p)
}

res <- data.frame(
  file = file_ls,
  var = var_ls,
  cohen_d_estimate = cohen_d_estimate_ls,
  cohen_d_lower = cohen_d_lower_ls,
  cohen_d_upper = cohen_d_upper_ls,
  cohen_d_p = cohen_d_p_ls,
  beta = beta_ls,
  beta_lower = beta_lower_ls,
  beta_upper = beta_upper_ls,
  beta_p = beta_p_ls
)

res$var <- c("High systolic blood pressure (>=140 mmHg)",
             "Obesity (BMI>=28)",
             "Current smoker",
             "Daily alcohol",
             "Insufficient daily MVPA (<15 mins)",
             "Diabetes history",
             "Frequently processed meat intake (>=5 times weekly)",
             "Frequently red meat intake (>=5 times weekly)",
             "Frequently salt intake (usually or always)",
             "Hypertension history")

names(res)[2] <- "var_name"
res$var_name <- factor(res$var_name,
                       levels = c("Insufficient daily MVPA (<15 mins)",
                                  "Frequently salt intake (usually or always)",
                                  "Frequently processed meat intake (>=5 times weekly)",
                                  "Frequently red meat intake (>=5 times weekly)",
                                  "Hypertension history",
                                  "Daily alcohol",
                                  "Diabetes history",
                                  "Obesity (BMI>=28)",
                                  "High systolic blood pressure (>=140 mmHg)",
                                  "Current smoker"))

res$beta_abs <- abs(res$beta)
res$beta_lower_abs <- abs(res$beta_upper)
res$beta_upper_abs <- abs(res$beta_lower)

p <- ggplot(res, aes(x = beta, y = var_name)) +
  geom_point(size = 5, color = "#3182bd") +
  geom_errorbar(aes(xmin = beta_lower, xmax = beta_upper), 
                width = 0.2, color = "#3182bd") +
  theme_minimal() +
  labs(title = "Counterfactual simulation: elimination of risk factors",
       y = "",
       x = "Coefficient") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 20, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "none",
        plot.title = element_text(size = 22, hjust = 0.5, vjust = 2))

ggsave("fig6-f1.pdf", p, width = 12, height = 5)


### telomere analysis
telomere_var_ls <- c("smoke", "alcohol", "processed_meat", "red_meat", "salt",
                     "pa", "hypertension", "diabetes", "bmi", "blood_pressure")
names(dat_analysis)[c(8:9, 11:14, 17:18, 23, 26)] <- telomere_var_ls
dat_analysis <- dat_analysis %>%
  dplyr::mutate(pa = case_when(
    pa < 15 ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(bmi = case_when(
    bmi >= 28 ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(blood_pressure = case_when(
    blood_pressure >= 140 ~ 1,
    TRUE ~ 0
  ))

dat_analysis$pa <- factor(dat_analysis$pa)
dat_analysis$bmi <- factor(dat_analysis$bmi)
dat_analysis$blood_pressure <- factor(dat_analysis$blood_pressure)

var_ls <- c()
beta_ls <- c()
beta_lower_ls <- c()
beta_upper_ls <- c()
beta_p_ls <- c()

for (telomere_var in telomere_var_ls) {
  dat_analysis$group <- dat_analysis[[telomere_var]]
  model <- lm(telomere_adjusted ~ group + Age + Sex + Income + Employment + Education, dat_analysis)
  # extract result
  beta <- round(model$coefficients["group1"], 3)
  beta_lower <- round(confint(model)["group1","2.5 %"], 3)
  beta_upper <- round(confint(model)["group1","97.5 %"], 3)
  beta_p <- summary(model)$coefficients["group1","Pr(>|t|)"]
  
  beta_p <- ifelse(beta_p < 0.001, "<0.001", as.character(round(beta_p, 3)))
  
  var_ls <- c(var_ls, telomere_var)
  beta_ls <- c(beta_ls, beta)
  beta_lower_ls <- c(beta_lower_ls, beta_lower)
  beta_upper_ls <- c(beta_upper_ls, beta_upper)
  beta_p_ls <- c(beta_p_ls, beta_p)
}

res_telomere <- data.frame(
  var = var_ls,
  beta = beta_ls,
  beta_lower = beta_lower_ls,
  beta_upper = beta_upper_ls,
  beta_p = beta_p_ls
)

res_telomere$var <- c("Current smoker",
                      "Daily alcohol",
                      "Frequently processed meat intake (>=5 times weekly)",
                      "Frequently red meat intake (>=5 times weekly)",
                      "Frequently salt intake (usually or always)",
                      "Insufficient daily MVPA (<15 mins)",
                      "Hypertension history",
                      "Diabetes history",
                      "Obesity (BMI>=28)",
                      "High systolic blood pressure (>=140 mmHg)")
names(res_telomere)[1] <- "var_name"

### wilcox test
res_sort <- dplyr::arrange(res, beta)
res_telomere_sort <- dplyr::arrange(res_telomere, beta)
res_sort$rank <- c(1:10)
res_telomere_sort$rank <- c(1:10)

res_sort <- dplyr::arrange(res_sort, var_name)
res_telomere_sort <- dplyr::arrange(res_telomere_sort, var_name)

wilcox_result <- wilcox.test(res_sort$rank, 
                             res_telomere_sort$rank, 
                             paired = TRUE)
print(wilcox_result)

### plot
res_telomere$var_name <- factor(res_telomere$var_name,
                                levels = c("Insufficient daily MVPA (<15 mins)",
                                           "Frequently salt intake (usually or always)",
                                           "Frequently processed meat intake (>=5 times weekly)",
                                           "Frequently red meat intake (>=5 times weekly)",
                                           "Hypertension history",
                                           "Daily alcohol",
                                           "Diabetes history",
                                           "Obesity (BMI>=28)",
                                           "High systolic blood pressure (>=140 mmHg)",
                                           "Current smoker"))

p <- ggplot(res_telomere, aes(x = beta, y = var_name)) +
  geom_point(size = 5, color = "#de2d26") +
  geom_errorbar(aes(xmin = beta_lower, xmax = beta_upper), 
                width = 0.2, color = "#de2d26") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(title = "Regression on adjusted telomere lenghth with risk factors",
       y = "",
       x = "Coefficient") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 20, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "none",
        plot.title = element_text(size = 22, hjust = 0.5, vjust = 2))

ggsave("fig6-f2.pdf", p, width = 12, height = 5)



