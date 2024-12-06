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



######################## ------ 1.preprocess ------
### read data
dat_age <- read_csv("Data/Models/llama3_70b/llama3-70b-result_only_age.csv")
names(dat_age)[c(2:9)] <- c("biological age", "cardiovascular age",
                            "hepatic age", "pulmonary age",
                            "renal age", "metabolic system age",
                            "immune system age", "musculoskeletal age")

dat_cov <- read_rds("Data/covariates_outcomes/panel_indicators.rds")
dat_outcome <- read_rds("Data/covariates_outcomes/aging_outcomes.rds")
# dat_outcome2 <- read_rds("Data/covariates_outcomes/aging_research_32_outcome.rds") # more 32 outcomes
# dat_outcome <- read_rds("Data/covariates_outcomes/app_outcome_medhis.rds")

### merge data
dat_cov <- dplyr::select(dat_cov, 1:3, 22) # age, sex, BMI
dat_age <- dat_age %>% inner_join(dat_cov, by = "eid")
dat_age <- dat_age %>% inner_join(dat_outcome, by = "eid")
# dat_age <- dat_age %>% inner_join(dat_outcome2, by = "eid")

dat_age <- dat_age %>% mutate(all_acc = `biological age` - Age)
dat_age <- dat_age %>% mutate(cardiovascular_acc = `cardiovascular age` - Age)
dat_age <- dat_age %>% mutate(hepatic_acc = `hepatic age` - Age)
dat_age <- dat_age %>% mutate(pulmonary_acc = `pulmonary age` - Age)
dat_age <- dat_age %>% mutate(renal_acc = `renal age` - Age)
dat_age <- dat_age %>% mutate(metabolic_acc = `metabolic system age` - Age)
dat_age <- dat_age %>% mutate(immune_acc = `immune system age` - Age)
dat_age <- dat_age %>% mutate(musculoskeletal_acc = `musculoskeletal age` - Age)
dat_age <- dat_age[dat_age$`pulmonary age` > 0 & dat_age$`immune system age` > 0,]
dat_age <- dat_age[!is.na(dat_age$`pulmonary age`) & !is.na(dat_age$`immune system age`),]

dat_age <- na.omit(dat_age)




######################## ------ 2.LLM can effectively predict overall biological age ------
############ 1.density plot, chronological age and biological age
dat_male <- subset(dat_age, Sex=="male")
dat_female <- subset(dat_age, Sex=="female")
cor_male <- cor(dat_male$Age, dat_male$`biological age`, use = "complete")
cor_female <- cor(dat_female$Age, dat_female$`biological age`, use = "complete")
cor_all <- cor(dat_age$Age, dat_age$`biological age`, use = "complete")

p_density <- ggplot(dat_female, aes(x = Age, y = `biological age`)) +
  # geom_point(alpha = 0.1, fill = "#4a1486", color = "black", shape = 21, size = 1) +
  stat_density2d(aes(fill = after_stat(level)), geom = "polygon", h = c(10, 10)) +
  # scale_fill_viridis_c(option = "viridis", direction = 1) +
  # scale_fill_gradient(low = "white", high = "blue", limits = c(0, NA)) +
  scale_fill_gradientn(colors = c("#3288bd", "#99d594", "#e6f598", "#ffffbf", "#fee08b", "#fc8d59", "#d53e4f")) +
  geom_smooth(method = "lm", se = TRUE, color = "#4d4d4d", linewidth = 0.5) +
  # geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#737373", linewidth = 1.2) +
  theme_minimal() +
  labs(
    x = "Chronological age (years)",
    y = "LLM biological age (years)",
    title = "Female"
  ) +
  scale_x_continuous(breaks = seq(35, 85, by = 10), limits = c(35, 85)) +
  scale_y_continuous(breaks = seq(35, 85, by = 10), limits = c(35, 85)) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(size = 20, hjust = 0, vjust = 1, face = "italic"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18, color = "black"),
    legend.position = "none"
  ) + 
  annotate("text", x = 63, y = 40, label = expression(italic("r")~" = 0.90"), color = "#252525", size = 8, hjust = 0)

ggsave("fig2-a2.pdf", plot = p_density, width = 4, height = 4)


p_density <- ggplot(dat_age, aes(x = Age, y = `biological age`)) +
  # geom_point(alpha = 0.1, fill = "#4a1486", color = "black", shape = 21, size = 1) +
  stat_density2d(aes(fill = after_stat(level)), geom = "polygon", h = c(10, 10)) +
  # scale_fill_viridis_c(option = "viridis", direction = 1) +
  # scale_fill_gradient(low = "white", high = "blue", limits = c(0, NA)) +
  scale_fill_gradientn(colors = c("#3288bd", "#99d594", "#e6f598", "#ffffbf", "#fee08b", "#fc8d59", "#d53e4f")) +
  geom_smooth(method = "lm", se = TRUE, color = "#4d4d4d", linewidth = 1) +
  # geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#737373", linewidth = 1.2) +
  theme_minimal() +
  labs(
    x = "Chronological age (years)",
    y = "LLM biological age (years)",
    # title = "Male"
    title = ""
  ) +
  scale_x_continuous(breaks = seq(35, 85, by = 10), limits = c(35, 85)) +
  scale_y_continuous(breaks = seq(35, 85, by = 10), limits = c(35, 85)) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # 添加左边框和下边框并加粗
    axis.line.x = element_line(color = "black", size = 1.5),
    axis.line.y = element_line(color = "black", size = 1.5),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(face = "bold", size = 20, hjust = 0, vjust = 1),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(face = "bold", size = 16, color = "black"),
    legend.position = "none"
  ) + 
  annotate("text", x = 67, y = 40, label = expression(italic("r")~" = 0.91"), fontface = "bold", color = "#252525", size = 7, hjust = 0)

# p_density
ggsave("fig1-b1.svg", plot = p_density, device = "svg", width = 4, height = 4)




############ 2.柱状图---生理年龄与端粒、衰弱指数的相关系数与实际年龄的对比
dat_telomere <- read_csv("Data/covariates_outcomes/telomere.csv")
dat_fi <- read_rds("Data/covariates_outcomes/frailty_index_52.rds")
dat_age <- dat_age %>% inner_join(dat_fi, by = "eid")
dat_telomere <- select(dat_telomere, 1:2, 5)
names(dat_telomere)[c(2, 3)] <- c("telomere", "telomere_adjusted")
dat_telomere <- na.omit(dat_telomere)
dat_age <- dat_age %>% inner_join(dat_telomere, by = "eid")


# install.packages("cocor")
# install.packages("ggsignif")
library(cocor)
library(ggsignif)

r_BA_telomere <- cor.test(dat_age$`biological age`, dat_age$telomere_adjusted, use = "complete")
r_CA_telomere <- cor.test(dat_age$Age, dat_age$telomere_adjusted, use = "complete")
r_BA_frailty <- cor.test(dat_age$`biological age`, dat_age$frailty_index, use = "complete")
r_CA_frailty <- cor.test(dat_age$Age, dat_age$frailty_index, use = "complete")

# 样本大小
n <- nrow(dat_age)
# 使用 cocor 包进行比较
result_1 <- cocor.dep.groups.overlap(r.jk = r_BA_telomere$estimate, 
                                     r.jh = r_CA_telomere$estimate, 
                                     r.kh = cor(dat_age$`biological age`, 
                                                dat_age$Age), n = n)
result_2 <- cocor.dep.groups.overlap(r.jk = r_BA_frailty$estimate, 
                                     r.jh = r_CA_frailty$estimate, 
                                     r.kh = cor(dat_age$`biological age`, 
                                                dat_age$Age), n = n)
# 提取显著性检验结果
p_value_1 <- result_1@pearson1898$p.value
p_value_2 <- result_2@pearson1898$p.value

r_BA_telomere_lower <- r_BA_telomere$conf.int[1]
r_BA_telomere_upper <- r_BA_telomere$conf.int[2]
r_CA_telomere_lower <- r_CA_telomere$conf.int[1]
r_CA_telomere_upper <- r_CA_telomere$conf.int[2]

r_BA_frailty_lower <- r_BA_frailty$conf.int[1]
r_BA_frailty_upper <- r_BA_frailty$conf.int[2]
r_CA_frailty_lower <- r_CA_frailty$conf.int[1]
r_CA_frailty_upper <- r_CA_frailty$conf.int[2]

df_res <- data.frame(
  Group = c("Telomere", "Telomere", "Frailty index", "Frailty index"),
  Comparison = rep(c("LLM biological age", "Chronological age"), 2),
  Value = c(r_BA_telomere$estimate, r_CA_telomere$estimate,
            r_BA_frailty$estimate, r_CA_frailty$estimate),
  Lower = c(r_BA_telomere_lower, r_CA_telomere_lower,
            r_BA_frailty_lower, r_CA_frailty_lower),
  Upper = c(r_BA_telomere_upper, r_CA_telomere_upper,
            r_BA_frailty_upper, r_CA_frailty_upper)
)
df_res$Value <- abs(df_res$Value)
df_res$Lower <- abs(df_res$Lower)
df_res$Upper <- abs(df_res$Upper)

# 绘制柱状图 c("#fee08b", "#4480B3")
p <- ggplot(df_res, aes(x = Group, y = Value, fill = Comparison)) +
  geom_bar(stat = "identity", width = 0.4, alpha = 0.8, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                width = 0.1, position = position_dodge(width = 0.6)) +
  geom_text(aes(label = round(Value, 3), vjust = ifelse(Value < 0, 1.5, -0.5)), 
            position = position_dodge(0.6), size = 6) +
  scale_fill_manual(values = c("LLM biological age" = "#4480B3", "Chronological age" = "#fee08b")) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(size = 22),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20, color = "black"),
    legend.text = element_text(size = 20),
    legend.position = "right"
  ) +
  guides(fill = guide_legend(title = NULL)) +
  labs(title = "",
       x = "",
       y = "Absolute Correlation Coefficient")

ggsave("fig2-b.pdf", p, width = 10, height = 7)



############ 3.绘制误差线图---c-index比较, 十折交叉验证
dat_telomere <- read_csv("Data/covariates_outcomes/telomere.csv")
dat_fi <- read_rds("Data/covariates_outcomes/frailty_index_52.rds")
dat_age <- dat_age %>% inner_join(dat_fi, by = "eid")
dat_telomere <- dplyr::select(dat_telomere, 1:2, 5)
names(dat_telomere)[c(2, 3)] <- c("telomere", "telomere_adjusted")
dat_telomere <- na.omit(dat_telomere)
dat_age <- dat_age %>% inner_join(dat_telomere, by = "eid")
names(dat_age)[2] <- "BA"

# 定义要跑的疾病
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")
# 定义要跑的变量
var_ls <- c("telomere", "frailty_index", "Age", "BA")

# 另外28种健康结局
disease <- colnames(dat_outcome)[2:65]
disease <- disease[seq(1, 64, 2)]
disease <- gsub(" diagnose", "", disease)
disease <- disease[c(1:26, 31:32)]
disease[3] <- "immunodeficiencies"
disease[8] <- "structural heart diseases"
disease[11] <- "arteries disorders"
disease[12] <- "venous thrombosis"
disease[14] <- "colitis"
disease[15] <- "gallbladder diseases"

names(dat_age)[c(16:17, 26:27, 32:33, 34:35, 38:39, 40:41)] <- c("immunodeficiencies diagnose",
                                                                "immunodeficiencies duration",
                                                                "structural heart diseases diagnose",
                                                                "structural heart diseases duration",
                                                                "arteries disorders diagnose",
                                                                "arteries disorders duration",
                                                                "venous thrombosis diagnose",
                                                                "venous thrombosis duration",
                                                                "colitis diagnose",
                                                                "colitis duration",
                                                                "gallbladder diseases diagnose",
                                                                "gallbladder diseases duration")

names(dat_age)[c(2:9)] <- c("BA", "cardiovascular_age", "hepatic_age",
                            "pulmonary_age", "renal_age", "metabolic_age",
                            "immune_age", "musculoskeletal_age")
# 定义要跑的变量
var_ls <- c("telomere", "frailty_index", "Age", "BA",
            "cardiovascular_age", "hepatic_age",
            "pulmonary_age", "renal_age", "metabolic_age", "musculoskeletal_age")

# 接受结果
var_mean_c_index <- c()
var_mean_c_index_lower <- c()
var_mean_c_index_upper <- c()
outcome_ls <- c()

# 开始跑结果
# 亚组分析：低收入群体
dat_age_backup <- dat_age
dat_age <- dat_age_backup
dat_age <- subset(dat_age, Income == "middle")
var_ls <- c("BA")
set.seed(2024)
for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]

  # 选择符合要求的数据
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
  # 把数据分成十份
  folds <- createFolds(dat_cox$event, k = 10)
  for (i in 1:length(var_ls)) {
    var <- var_ls[i]
    # 初始化存储c-index的向量
    c_index_values <- c()
    c_index_lower_ls <- c()
    c_index_upper_ls <- c()

    # 十折交叉验证
    for(j in 1:10) {
      # 划分训练集和测试集
      test_indices <- folds[[j]]
      train_data <- dat_cox[-test_indices, ]
      test_data <- dat_cox[test_indices, ]

      # 构建Cox模型
      formula_covariates <- paste0("survobj ~ ", var)
      f <- as.formula(formula_covariates)
      survobj <- with(train_data, Surv(time, event))
      cox_fit <- coxph(formula = f, data = train_data, na.action = na.omit)

      # 预测风险评分
      test_data$predicted_risk <- predict(cox_fit, newdata = test_data, 
                                          type = "risk")

      # 计算c-index
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
    var_name == "telomere" ~ "Telomere",
    var_name == "frailty_index" ~ "Frailty index",
    var_name == "Age" ~ "Chronological age",
    var_name == "BA" ~ "LLM biological age"
  ))

# 另外28种结局
dat_plot <- dat_plot %>%
  mutate(var_name = case_when(
    var_name == "telomere" ~ "Telomere",
    var_name == "frailty_index" ~ "Frailty index",
    var_name == "Age" ~ "Chronological age",
    var_name == "BA" ~ "LLM biological age",
    var_name == "cardiovascular_age" ~ "LLM cardiovascular age", 
    var_name == "hepatic_age" ~ "LLM hepatic age",
    var_name == "pulmonary_age" ~ "LLM pulmonary age", 
    var_name == "renal_age" ~ "LLM renal age", 
    var_name == "metabolic_age" ~ "LLM metabolic age", 
    var_name == "musculoskeletal_age" ~ "LLM musculoskeletal age"
  ))

dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c("Telomere", 
                                       "Frailty index", 
                                       "Chronological age", 
                                       "LLM biological age"))

write_rds(dat_plot, "c_index_plot_overall_8disease.rds")

# 另外28种结局
dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c("Telomere", 
                                       "Frailty index", 
                                       "Chronological age", 
                                       "LLM biological age",
                                       "LLM cardiovascular age",
                                       "LLM hepatic age",
                                       "LLM pulmonary age",
                                       "LLM renal age",
                                       "LLM metabolic age",
                                       "LLM musculoskeletal age"))
write_rds(dat_plot, "c_index_plot_all_28disease_240929.rds")

###### Plot: 已存好的结果
dat_plot <- read_rds("Data/tempplot/c_index_plot_overall_8disease.rds")
# dat_plot <- read_rds("Data/tempplot/c_index_plot_all_28disease_240929.rds")
plots_c_index <- list()
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")
# disease <- unique(dat_plot$outcome)
var_name_ls <- as.character(unique(dat_plot$var_name))
# c("#fee08b", "#4480B3")
for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(dat_plot, outcome == item)
  p <- ggplot(dat_sub, aes(x = var_name, y = c_index, color = var_name)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = c_index_lower, ymax = c_index_upper), width = 0.1) +
    # geom_segment(aes(x = 0, xend = var_name, y = c_index, yend = c_index),
    #              linetype = "dashed",
    #              data = subset(dat_sub, var_name %in% c("Telomere", "Frailty index", "Chronological age", "LLM biological age"))) +
    # scale_color_manual(values = c("#878787", "#f4a582", "#fee08b", "#4480B3")) +
    geom_segment(aes(x = 0, xend = var_name, y = c_index, yend = c_index),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% var_name_ls)) +
    scale_color_manual(values = c("#878787", "#f4a582", "#fee08b", "#4480B3",
                                  "#E71D1D", "#4BB04A", "#FF7D01", "#F980BE", "#A35628", "#9850A6")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(angle = 90, size = 18, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 18, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 20, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
  
  # 主分析是5
  if (i < 5) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.line.x = element_blank())
  }
  plots_c_index[[i]] <- p
}

plots_c_index[5]

arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 4, 
                            nrow = 2,
                            heights = c(1, 1.8))

arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 4, 
                            nrow = 7,
                            heights = c(1, 1, 1, 1, 1, 1, 1.7))

# arranged_plots
# 添加纵坐标标题
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Absolute C-index", size = 18, rot = 90))

# 主分析
ggsave("fig2-c.pdf", plot = combined_plot, width = 16, height = 8)
ggsave("fig2-c.svg", plot = combined_plot, device = "svg", width = 16, height = 8)
# 副分析
ggsave("extended_fig2.pdf", plot = combined_plot, width = 16, height = 28)

# 使用annotation_custom添加标题
# title_grob <- textGrob("d", 
#                        x = unit(0, "npc"), 
#                        y = unit(1, "npc"), 
#                        just = c("left", "top"), 
#                        gp = gpar(fontsize = 20, fontface = "bold"))
# 创建一个空的ggplot对象，用于放置标题
# empty_plot <- ggplot() + theme_void() +
#   annotation_custom(title_grob)
# 使用arrangeGrob将标题和合并图组合
# final_plot_d <- arrangeGrob(empty_plot, combined_plot, ncol = 1, 
#                             heights = c(0.5, 20),
#                             padding = unit(0, "line"))
# grid.newpage()
# grid.draw(final_plot_d)

# final_plot
# ggsave("c_index.pdf", plot = final_plot_d, width = 16, height = 12)



############ 4.绘制箱线图---LLM得到的age gap与基线时疾病数量显著相关
### 计算共病
# dat_all_outcomes <- read_feather("Data/all_outcomes.feather")
# dat_all_outcomes <- select(dat_all_outcomes, 1:613)
# all_outcomes_col <- colnames(dat_all_outcomes)[c(2:613)]
# all_outcomes_col <- all_outcomes_col[seq(2, length(all_outcomes_col), by = 2)]
# dat_all_outcomes[, number_of_diseases := rowSums(.SD <= 0), .SDcols = all_outcomes_col]
# dat_all_outcomes <- select(dat_all_outcomes, 1, 614)
# write_rds(dat_all_outcomes, "multimorbidity_stat.rds")
dat_multimorbidity <- read_rds("Data/tempplot/multimorbidity_stat.rds")
dat_multimorbidity <- dat_multimorbidity %>%
  mutate(number_of_diseases = case_when(
    number_of_diseases > 15 ~ 15,
    TRUE ~ number_of_diseases
  ))

dat_age <- dat_age %>% inner_join(dat_multimorbidity, by = "eid")
dat_age$number_of_diseases <- as.character(dat_age$number_of_diseases)
dat_age <- dat_age %>%
  mutate(number_of_diseases = case_when(
    number_of_diseases == "15" ~ ">=15",
    TRUE ~ number_of_diseases
  ))

dat_age$number_of_diseases <- factor(dat_age$number_of_diseases, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", ">=15"))

dat_age$plot_color <- "acc"
# 画柱状图、误差线和箱线图
p <- ggplot(data = dat_age, aes(x = number_of_diseases, y = all_acc, fill = plot_color)) +
  # 添加箱线图 , outlier.shape = NA
  geom_boxplot(width = 0.2, alpha = 0.8, outliers = FALSE) +
  geom_smooth(method = "lm", aes(group = 1), linetype = "dashed", alpha = 0.5,
              color = "#fdae6b", se = FALSE) +
  # geom_smooth(method = "loess", aes(group = 1), color = "#cb181d", linetype = "dashed", alpha = 0.5, se = FALSE) +
  labs(x = "Number of diseases at baseline", y = "Overall age gap (years)") +
  theme_minimal() +
  scale_fill_manual(values = c("acc" = "#6a51a3"),
                    labels = c("acc")) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(size = 24),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22, color = "black"),
    legend.position = "none"
  )
  # scale_y_continuous(breaks = seq(0, 90, by = 20), limits = c(0, 90))

ggsave("fig3-a.pdf", p, width = 16, height = 5)

# 使用annotation_custom添加标题
# title_grob <- textGrob("c", 
#                        x = unit(0, "npc"), 
#                        y = unit(1, "npc"), 
#                        just = c("left", "top"), 
#                        gp = gpar(fontsize = 20, fontface = "bold"))
# 创建一个空的ggplot对象，用于放置标题
# empty_plot <- ggplot() + theme_void() +
#   annotation_custom(title_grob)
# 使用arrangeGrob将标题和合并图组合
# final_plot_c <- arrangeGrob(empty_plot, p_bar, ncol = 1, 
#                             heights = c(1, 20),
#                             padding = unit(0, "line"))
# grid.newpage()
# grid.draw(final_plot_c)

# p_bar
# ggsave("num_disease.pdf", plot = p_bar, width = 9, height = 6)



############ 5.KM 曲线
### 分配标签
# 对 acc 进行排序
dat_age <- dat_age[order(dat_age$all_acc), ]
# 计算分组的边界
n <- nrow(dat_age)
top_10_boundary <- n * 0.9
median_10_boundary_low <- n * 0.45
median_10_boundary_high <- n * 0.55
# 分配分组标签
dat_age$group <- "Other"
dat_age$group[1:(n * 0.1)] <- "Bottom 10%"
dat_age$group[(top_10_boundary+1):n] <- "Top 10%"
dat_age$group[(median_10_boundary_low+1):median_10_boundary_high] <- "Median 10%"

### 开始拟合
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")
disease <- colnames(dat_outcome)[2:65]
disease <- disease[seq(1, 64, 2)]
disease <- gsub(" diagnose", "", disease)
disease <- disease[c(1:26, 31:32)]
disease[3] <- "immunodeficiencies"
disease[8] <- "structural heart diseases"
disease[11] <- "arteries disorders"
disease[12] <- "venous thrombosis"
disease[14] <- "colitis"
disease[15] <- "gallbladder diseases"

names(dat_age)[c(9:10, 19:20, 25:26, 27:28, 31:32, 33:34)] <- c("immunodeficiencies diagnose",
                                                                "immunodeficiencies duration",
                                                                "structural heart diseases diagnose",
                                                                "structural heart diseases duration",
                                                                "arteries disorders diagnose",
                                                                "arteries disorders duration",
                                                                "venous thrombosis diagnose",
                                                                "venous thrombosis duration",
                                                                "colitis diagnose",
                                                                "colitis duration",
                                                                "gallbladder diseases diagnose",
                                                                "gallbladder diseases duration")


plots <- list()

for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]
  
  # 选择符合要求的数据
  # # 主分析
  # if (item == "CHD" | item == "Stroke") {
  #   dat_cox <- subset(dat_age, `MACE duration` > 0)
  # }
  # else if (item == "Renal failure") {
  #   dat_cox <- subset(dat_age, `Renal diseases duration` > 0)
  # }
  # else if (item == "T2D") {
  #   dat_cox <- subset(dat_age, `Diabetes duration` > 0)
  # }
  # else {
  #   dat_cox <- subset(dat_age, time > 0) 
  # }
  dat_cox <- subset(dat_age, time > 0) # 更多28种结局
  dat_cox <- subset(dat_cox, group == "Bottom 10%" | group == "Top 10%" | group == "Median 10%")
  
  # 拟合生存曲线
  fit <- survfit(Surv(time, event) ~ group, data = dat_cox)
  
  # 绘制生存曲线
  ggsurv <- ggsurvplot(fit,
                       data = dat_cox,
                       # pval = TRUE, 
                       conf.int = FALSE,
                       # risk.table = TRUE,
                       fun = "event",
                       xlab = "",
                       ylab = "",
                       xlim = c(0, 15),
                       palette = c("#90D3C7", "#80B1D3", "#ca0020"),
                       legend.title = "Age-gap group",
                       legend.labs = c("Bottom 10%", "Median 10%", "Top 10%"),
                       legend = "bottom",
                       title = item,
                       ggtheme = theme_minimal())
  
  # 更改其他设置
  ggsurv$plot <- ggsurv$plot + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          # plot.margin = margin(10, 10, 10, 10),
          axis.line = element_line(),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18, color = "black"),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5, vjust = 2)) +
    scale_x_continuous(breaks = c(5, 10)) + 
    scale_y_continuous(labels = function(x) x * 100)
  
  plots[[i]] <- ggsurv$plot
}

# 合并图形并保留一个图例和轴标题
combined_plot <- ggarrange(plotlist = plots, 
                           ncol = 4, 
                           nrow = 2,
                           common.legend = TRUE, 
                           legend = "right")
# 32种结局
combined_plot <- ggarrange(plotlist = plots, 
                           ncol = 4, 
                           nrow = 7,
                           common.legend = TRUE, 
                           legend = "right")

# 添加横纵坐标标题
combined_plot <- annotate_figure(combined_plot,
                                 bottom = text_grob("Time (years)", size = 18),
                                 left = text_grob("Cumulative event (%)", size = 18, rot = 90))

# print(combined_plot)
ggsave("fig3-b.pdf", plot = combined_plot, width = 16, height = 8)
ggsave("extend_fig9.pdf", plot = combined_plot, width = 16, height = 28)



############ 6.Scaling law
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

### 合并数据
dat_cov <- read_rds("Data/covariates_outcomes/panel_indicators.rds")
dat_cov <- dplyr::select(dat_cov, 1:3)
dat_outcome <- read_rds("Data/covariates_outcomes/aging_outcomes.rds")
dat_outcome_2 <- read_rds("Data/covariates_outcomes/aging_research_32_outcome.rds")
dat_outcome <- dat_outcome %>% inner_join(dat_outcome_2, by = "eid")

# 合并
dat_age <- dat_age %>% inner_join(dat_cov, by = "eid")
dat_age <- dat_age %>% inner_join(dat_outcome, by = "eid")
dat_age <- na.omit(dat_age)

# 构造age gap指标
dat_age <- dat_age %>% 
  mutate(`llama3-8b overall age gap` = `llama3-8b biological age` - Age) %>%
  mutate(`llama3-70b overall age gap` = `llama3-70b biological age` - Age) %>%
  mutate(`qwen1.5-14b overall age gap` = `qwen1.5-14b biological age` - Age) %>%
  mutate(`qwen1.5-32b overall age gap` = `qwen1.5-32b biological age` - Age) %>%
  mutate(`qwen1.5-72b overall age gap` = `qwen1.5-72b biological age` - Age) %>%
  mutate(`qwen1.5-110b overall age gap` = `qwen1.5-110b biological age` - Age) %>%
  mutate(`qwen2-7b overall age gap` = `qwen2-7b biological age` - Age) %>%
  mutate(`qwen2-72b overall age gap` = `qwen2-72b biological age` - Age)


### 开始拟合
# 定义要跑的疾病
disease <- c("All-cause death") # 主图fig-2
# disease <- c("CHD", "Stroke", "COPD", "Liver diseases", "Renal failure", "T2D", "Arthritis")
disease <- colnames(dat_outcome)[2:87]
disease <- disease[seq(1, 86, 2)]
disease <- disease[-c(1, 2, 10, 11, 38:41)]
disease <- gsub(" diagnose", "", disease)

disease[10] <- "immunodeficiencies"
disease[15] <- "structural heart diseases"
disease[18] <- "arteries disorders"
disease[19] <- "venous thrombosis"
disease[21] <- "colitis"
disease[22] <- "gallbladder diseases"

names(dat_age)[c(38:39, 48:49, 54:55, 56:57, 60:61, 62:63)] <- c("immunodeficiencies diagnose",
                                                                 "immunodeficiencies duration",
                                                                 "structural heart diseases diagnose",
                                                                 "structural heart diseases duration",
                                                                 "arteries disorders diagnose",
                                                                 "arteries disorders duration",
                                                                 "venous thrombosis diagnose",
                                                                 "venous thrombosis duration",
                                                                 "colitis diagnose",
                                                                 "colitis duration",
                                                                 "gallbladder diseases diagnose",
                                                                 "gallbladder diseases duration")

# 定义要跑的变量
var_ls <- c("`llama3-8b biological age`", 
            "`llama3-70b biological age`", 
            "`qwen1.5-14b biological age`",
            "`qwen1.5-32b biological age`", 
            "`qwen1.5-72b biological age`",
            "`qwen1.5-110b biological age`",
            "`qwen2-7b biological age`",
            "`qwen2-72b biological age`")
# 接受结果
var_mean_c_index <- c()
var_mean_c_index_lower <- c()
var_mean_c_index_upper <- c()
outcome_ls <- c()

# 开始跑结果
set.seed(2024)
for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]
  
  # 选择符合要求的数据
  if (item == "CHD" | item == "Stroke" | 
      item == "angina pectoris") {
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
  # 把数据分成十份
  folds <- createFolds(dat_cox$event, k = 10)
  for (i in 1:length(var_ls)) {
    var <- var_ls[i]
    # 初始化存储c-index的向量
    c_index_values <- c()
    c_index_lower_ls <- c()
    c_index_upper_ls <- c()
    
    # 十折交叉验证
    for(j in 1:10) {
      # 划分训练集和测试集
      test_indices <- folds[[j]]
      train_data <- dat_cox[-test_indices, ]
      test_data <- dat_cox[test_indices, ]
      
      # 构建Cox模型
      formula_covariates <- paste0("survobj ~ ", var)
      f <- as.formula(formula_covariates)
      survobj <- with(train_data, Surv(time, event))
      cox_fit <- coxph(formula = f, data = train_data, na.action = na.omit)
      
      # 预测风险评分
      test_data$predicted_risk <- predict(cox_fit, newdata = test_data, 
                                          type = "risk")
      
      # 计算c-index
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
    var_name == "`llama3-8b biological age`" ~ "llama3-8b biological age",
    var_name == "`llama3-70b biological age`" ~ "llama3-70b biological age",
    var_name == "`qwen1.5-14b biological age`" ~ "qwen1.5-14b biological age",
    var_name == "`qwen1.5-32b biological age`" ~ "qwen1.5-32b biological age",
    var_name == "`qwen1.5-72b biological age`" ~ "qwen1.5-72b biological age",
    var_name == "`qwen1.5-110b biological age`" ~ "qwen1.5-110b biological age",
    var_name == "`qwen2-7b biological age`" ~ "qwen2-7b biological age",
    var_name == "`qwen2-72b biological age`" ~ "qwen2-72b biological age"
  ))

write_rds(dat_plot, "scaling_law.rds")
dat_plot <- read_rds("Data/tempplot/scaling_law.rds")

# 35种结局
write_rds(dat_plot, "scaling_law_35_outcomes.rds")
dat_plot <- read_rds("Data/tempplot/scaling_law_35_outcomes.rds")

# 重新布局
dat_plot_2 <- read_rds("Data/tempplot/scaling_law_35_outcomes.rds")
dat_plot <- rbind(dat_plot, dat_plot_2)

dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c("llama3-8b biological age", 
                                       "llama3-70b biological age", 
                                       "qwen1.5-14b biological age",
                                       "qwen1.5-32b biological age", 
                                       "qwen1.5-72b biological age",
                                       "qwen1.5-110b biological age",
                                       "qwen2-7b biological age",
                                       "qwen2-72b biological age"))

write_rds(dat_plot, "scaling_law_36_outcomes.rds")

### 读取
dat_plot <- read_rds("Data/tempplot/scaling_law_36_outcomes.rds")
dat_plot$var_name <- rep(c("llama3-8b BA", "llama3-70b BA", 
                           "qwen1.5-14b BA", "qwen1.5-32b BA",
                           "qwen1.5-72b BA", "qwen1.5-110b BA",
                           "qwen2-7b BA", "qwen2-72b BA"), times = 36)
dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c("llama3-8b BA", "llama3-70b BA", 
                                       "qwen1.5-14b BA", "qwen1.5-32b BA",
                                       "qwen1.5-72b BA", "qwen1.5-110b BA",
                                       "qwen2-7b BA", "qwen2-72b BA"))

plots_c_index <- list()
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")

disease <- unique(dat_plot$outcome)
disease <- disease[1:8]
disease <- disease[9:36]


for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(dat_plot, outcome == item)
  p <- ggplot(dat_sub, aes(x = var_name, y = c_index, color = var_name)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = c_index_lower, ymax = c_index_upper), width = 0.1) +
    geom_segment(aes(x = 0, xend = var_name, y = c_index, yend = c_index),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% c("llama3-8b BA", "llama3-70b BA", 
                                                        "qwen1.5-14b BA", "qwen1.5-32b BA",
                                                        "qwen1.5-72b BA", "qwen1.5-110b BA",
                                                        "qwen2-7b BA", "qwen2-72b BA"))) +
    scale_color_manual(values = c("#6a51a3", "#6a51a3", "#fdae6b", "#fdae6b", "#fdae6b", "#fdae6b",
                                  "#31a354", "#31a354")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 24),
          axis.text.x = element_text(angle = 90, size = 24, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 24, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 26, hjust = 0.5, vjust = 2)) +
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

arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 4, 
                            nrow = 7,
                            heights = c(1, 1, 1, 1, 1, 1, 2.2))
# arranged_plots
# 添加纵坐标标题
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Absolute C-index", size = 24, rot = 90))

ggsave("fig2-e.pdf", plot = combined_plot, width = 20, height = 10)
ggsave("extended_fig4.pdf", plot = combined_plot, width = 24, height = 28)


###### 2.各个模型 age gap 的 HR
Cox_analysis <- function(dat_baseline, disease_ls, var_ls) {
  ### 感兴趣结果整合到列表
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
    
    # 选择符合要求的数据
    if (item == "CHD" | item == "Stroke" | item == "angina pectoris") {
      dat_cox <- subset(dat_baseline, `MACE duration` > 0)
    }
    else if (item == "Renal failure") {
      dat_cox <- subset(dat_baseline, `Renal diseases duration` > 0)
    }
    else if (item == "T2D") {
      dat_cox <- subset(dat_baseline, `Diabetes duration` > 0)
    }
    else {
      dat_cox <- subset(dat_baseline, time > 0) 
    }

    
    for (i in 1:length(var_ls)) {
      var_name <- var_ls[i]
      # formula_covariates <- paste0("survobj ~ ", var_name)
      formula_covariates <- paste0("survobj ~ Age + Sex + ", var_name)
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

# 定义要跑的疾病
# disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
#              "Liver diseases", "Renal failure", "T2D", "Arthritis")
disease <- c("All-cause death", disease)
# 定义要跑的变量
var_ls <- c("`llama3-8b overall age gap`", 
            "`llama3-70b overall age gap`", 
            "`qwen1.5-14b overall age gap`",
            "`qwen1.5-32b overall age gap`", 
            "`qwen1.5-72b overall age gap`",
            "`qwen1.5-110b overall age gap`",
            "`qwen2-7b overall age gap`",
            "`qwen2-72b overall age gap`")

age_results_hr <- Cox_analysis(dat_baseline = dat_age,
                               disease_ls = disease,
                               var_ls = var_ls)

names(age_results_hr)[c(1, 2)] <- c("outcome", "var_name")
age_results_hr <- age_results_hr %>%
  mutate(var_name = case_when(
    var_name == "`llama3-8b overall age gap`" ~ "llama3-8b overall age gap",
    var_name == "`llama3-70b overall age gap`" ~ "llama3-70b overall age gap",
    var_name == "`qwen1.5-14b overall age gap`" ~ "qwen1.5-14b overall age gap",
    var_name == "`qwen1.5-32b overall age gap`" ~ "qwen1.5-32b overall age gap",
    var_name == "`qwen1.5-72b overall age gap`" ~ "qwen1.5-72b overall age gap",
    var_name == "`qwen1.5-110b overall age gap`" ~ "qwen1.5-110b overall age gap",
    var_name == "`qwen2-7b overall age gap`" ~ "qwen2-7b overall age gap",
    var_name == "`qwen2-72b overall age gap`" ~ "qwen2-72b overall age gap"
  ))

age_results_hr$var_name <- factor(age_results_hr$var_name, 
                                  levels = c("llama3-8b overall age gap", 
                                             "llama3-70b overall age gap", 
                                             "qwen1.5-14b overall age gap", 
                                             "qwen1.5-32b overall age gap", 
                                             "qwen1.5-72b overall age gap", 
                                             "qwen1.5-110b overall age gap",
                                             "qwen2-7b overall age gap",
                                             "qwen2-72b overall age gap"))

write_rds(age_results_hr, "age_gap_hr_36_outcomes_models.rds")

plots_HR <- list()
# disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
#              "Liver diseases", "Renal failure", "T2D", "Arthritis")

for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(age_results_hr, outcome == item)
  p <- ggplot(dat_sub, aes(x = var_name, y = HR, color = var_name)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
    geom_segment(aes(x = 0, xend = var_name, y = HR, yend = HR),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% c("llama3-8b overall age gap", 
                                                        "llama3-70b overall age gap", 
                                                        "qwen1.5-14b overall age gap", 
                                                        "qwen1.5-32b overall age gap", 
                                                        "qwen1.5-72b overall age gap", 
                                                        "qwen1.5-110b overall age gap",
                                                        "qwen2-7b overall age gap",
                                                        "qwen2-72b overall age gap"))) +
    scale_color_manual(values = c("#6a51a3", "#6a51a3", "#fdae6b", "#fdae6b", "#fdae6b", "#fdae6b",
                                  "#31a354", "#31a354")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 16),
          axis.text.x = element_text(angle = 90, size = 14, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 14, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 18, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
  
  if (i < 33) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.line.x = element_blank())
  }
  plots_HR[[i]] <- p
}

plots_HR[5]

arranged_plots <- ggarrange(plotlist = plots_HR, 
                            ncol = 6, 
                            nrow = 6,
                            heights = c(1, 1, 1, 1, 1, 1, 1, 1, 1.8))
# arranged_plots
# 添加纵坐标标题
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Adjusted hazard ratio (HR) with per age gap", size = 16, rot = 90))
ggsave("extended_fig5.pdf", plot = combined_plot, width = 20, height = 36)




############ 7.extendedfig10: 改变prompt分析, 改变特征分析
dat_age_new <- read_csv("Data/Models/llama3_70b/llama3-70b-result_only_age.csv")
dat_age_old <- read_csv("Data/Models/llama3_70b/llama3-70B-result-oldprompt_full.csv")
dat_age_analysis <- read_csv("Data/Models/llama3_70b/llama3-70B-analyisis_oldprompt_full.csv")

dat_age_new <- dplyr::select(dat_age_new, 1, 2)
dat_age_old <- dplyr::select(dat_age_old, 1, 4)
dat_age_analysis <- dplyr::select(dat_age_analysis, 1, 4)

names(dat_age_new)[2] <- "overall biological age"
names(dat_age_old)[2] <- "overall biological age (new prompt)"
names(dat_age_analysis)[2] <- "overall biological age (47 indicators)"

dat_cov <- read_rds("Data/covariates_outcomes/panel_indicators.rds")
dat_cov <- dplyr::select(dat_cov, 1:3)
dat_outcome <- read_rds("Data/covariates_outcomes/aging_outcomes.rds")

### 1.分析不同prompt
dat_age_1 <- dat_age_new %>%
  inner_join(dat_age_old, by = "eid") %>%
  inner_join(dat_cov, by = "eid") %>%
  inner_join(dat_outcome, by = "eid")

### 2.分析不同特征
dat_age_2 <- dat_age_new %>%
  inner_join(dat_age_analysis, by = "eid") %>%
  inner_join(dat_cov, by = "eid") %>%
  inner_join(dat_outcome, by = "eid")

dat_age_1 <- dat_age_1 %>%
  dplyr::mutate(`overall age gap` = `overall biological age` - Age) %>%
  dplyr::mutate(`overall age gap (new prompt)` = `overall biological age (new prompt)` - Age)

dat_age_2 <- dat_age_2 %>%
  dplyr::mutate(`overall age gap` = `overall biological age` - Age) %>%
  dplyr::mutate(`overall age gap (47 indicators)` = `overall biological age (47 indicators)` - Age)

### 1.生理年龄的c-index
# 定义要跑的疾病
disease <- c("All-cause death", "CHD", "Stroke", "COPD", "Liver diseases", "Renal failure", "T2D", "Arthritis")
# 定义要跑的变量
var_ls <- c("`overall biological age`", 
            "`overall biological age (new prompt)`")
var_ls <- c("`overall biological age`", 
            "`overall biological age (47 indicators)`")
# 接受结果
var_mean_c_index <- c()
var_mean_c_index_lower <- c()
var_mean_c_index_upper <- c()
outcome_ls <- c()

# 开始跑结果
set.seed(2024)
# dat_age <- dat_age_1
dat_age <- dat_age_2
for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]
  
  # 选择符合要求的数据
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
  # 把数据分成十份
  folds <- createFolds(dat_cox$event, k = 10)
  for (i in 1:length(var_ls)) {
    var <- var_ls[i]
    # 初始化存储c-index的向量
    c_index_values <- c()
    c_index_lower_ls <- c()
    c_index_upper_ls <- c()
    
    # 十折交叉验证
    for(j in 1:10) {
      # 划分训练集和测试集
      test_indices <- folds[[j]]
      train_data <- dat_cox[-test_indices, ]
      test_data <- dat_cox[test_indices, ]
      
      # 构建Cox模型
      formula_covariates <- paste0("survobj ~ ", var)
      f <- as.formula(formula_covariates)
      survobj <- with(train_data, Surv(time, event))
      cox_fit <- coxph(formula = f, data = train_data, na.action = na.omit)
      
      # 预测风险评分
      test_data$predicted_risk <- predict(cox_fit, newdata = test_data, 
                                          type = "risk")
      
      # 计算c-index
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
    var_name == "`overall biological age`" ~ "overall biological age",
    var_name == "`overall biological age (new prompt)`" ~ "overall biological age (new prompt)"
  ))

dat_plot <- dat_plot %>%
  mutate(var_name = case_when(
    var_name == "`overall biological age`" ~ "overall biological age",
    var_name == "`overall biological age (47 indicators)`" ~ "overall biological age (47 indicators)"
  ))

dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c("overall biological age", 
                                       "overall biological age (new prompt)"))
dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c("overall biological age", 
                                       "overall biological age (47 indicators)"))

write_rds(dat_plot, "old_new_prompt_extend_10.rds")
write_rds(dat_plot, "overall_partial_indicators_extend_10.rds")

plots_c_index <- list()
disease <- c("All-cause death", "CHD", "Stroke", "COPD", "Liver diseases", "Renal failure", "T2D", "Arthritis")

for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(dat_plot, outcome == item)
  p <- ggplot(dat_sub, aes(x = var_name, y = c_index, color = var_name)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = c_index_lower, ymax = c_index_upper), width = 0.1) +
    # geom_segment(aes(x = 0, xend = var_name, y = c_index, yend = c_index),
    #              linetype = "dashed",
    #              data = subset(dat_sub, var_name %in% c("overall biological age", 
    #                                                     "overall biological age (new prompt)"))) +
    geom_segment(aes(x = 0, xend = var_name, y = c_index, yend = c_index),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% c("overall biological age", 
                                                        "overall biological age (47 indicators)"))) +
    # scale_color_manual(values = c("#6a51a3", "#fdae6b")) +
    scale_color_manual(values = c("#6a51a3", "#31a354")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 16),
          axis.text.x = element_text(angle = 30, size = 14, color = "black", hjust = 1, vjust = 1),
          axis.text.y = element_text(size = 14, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 18, hjust = 0.5, vjust = 2)) +
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
                            heights = c(1, 1.5))

# arranged_plots
# 添加纵坐标标题
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Absolute C-index", size = 16, rot = 90))

ggsave("extend_fig10-b1.pdf", plot = combined_plot, width = 18, height = 8)


###### 2.敏感性分析 age gap 的 HR
Cox_analysis <- function(dat_baseline, disease_ls, var_ls) {
  ### 感兴趣结果整合到列表
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
    
    # 选择符合要求的数据
    if (item == "CHD" | item == "Stroke") {
      dat_cox <- subset(dat_baseline, `MACE duration` > 0)
    }
    else if (item == "Renal failure") {
      dat_cox <- subset(dat_baseline, `Renal diseases duration` > 0)
    }
    else if (item == "T2D") {
      dat_cox <- subset(dat_baseline, `Diabetes duration` > 0)
    }
    else {
      dat_cox <- subset(dat_baseline, time > 0) 
    }
    
    # if (item != "All-cause death") {
    #   dat_cox <- dat_cox %>%
    #     mutate(event = case_when(
    #       `All-cause death diagnose` == 1 ~ 2,
    #       TRUE ~ event
    #     )) 
    # }
    
    for (i in 1:length(var_ls)) {
      var_name <- var_ls[i]
      # formula_covariates <- paste0("survobj ~ ", var_name)
      formula_covariates <- paste0("survobj ~ Age + Sex + ", var_name)
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

# 定义要跑的疾病
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")
# 定义要跑的变量
var_ls <- c("`overall age gap`", 
            "`overall age gap (new prompt)`")
var_ls <- c("`overall age gap`", 
            "`overall age gap (47 indicators)`")

# dat_age <- dat_age_1
dat_age <- dat_age_2
age_results_hr <- Cox_analysis(dat_baseline = dat_age,
                               disease_ls = disease,
                               var_ls = var_ls)

names(age_results_hr)[c(1, 2)] <- c("outcome", "var_name")
age_results_hr <- age_results_hr %>%
  mutate(var_name = case_when(
    var_name == "`overall age gap`" ~ "overall age gap",
    var_name == "`overall age gap (new prompt)`" ~ "overall age gap (new prompt)"
  ))
age_results_hr <- age_results_hr %>%
  mutate(var_name = case_when(
    var_name == "`overall age gap`" ~ "overall age gap",
    var_name == "`overall age gap (47 indicators)`" ~ "overall age gap (47 indicators)"
  ))

age_results_hr$var_name <- factor(age_results_hr$var_name, 
                                  levels = c("overall age gap", 
                                             "overall age gap (new prompt)"))
age_results_hr$var_name <- factor(age_results_hr$var_name, 
                                  levels = c("overall age gap", 
                                             "overall age gap (47 indicators)"))


plots_HR <- list()
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")

for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(age_results_hr, outcome == item)
  p <- ggplot(dat_sub, aes(x = var_name, y = HR, color = var_name)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
    # geom_segment(aes(x = 0, xend = var_name, y = HR, yend = HR),
    #              linetype = "dashed",
    #              data = subset(dat_sub, var_name %in% c("overall age gap", 
    #                                                     "overall age gap (new prompt)"))) +
    # scale_color_manual(values = c("#6a51a3", "#fdae6b")) +
    geom_segment(aes(x = 0, xend = var_name, y = HR, yend = HR),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% c("overall age gap",
                                                        "overall age gap (47 indicators)"))) +
    scale_color_manual(values = c("#6a51a3", "#31a354")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 16),
          axis.text.x = element_text(angle = 30, size = 14, color = "black", hjust = 1, vjust = 1),
          axis.text.y = element_text(size = 14, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 18, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(labels = number_format(accuracy = 0.001))
  
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
                            heights = c(1, 1.5))
# arranged_plots
# 添加纵坐标标题
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Adjusted hazard ratio (HR) with per age gap", size = 16, rot = 90))

ggsave("extend_fig10-b2.pdf", plot = combined_plot, width = 18, height = 8)










######################## ------ 3.LLM can predict organ-specific ages ------
############ 1.比较多器官 age gap 预测各种疾病的 c-index
# 定义要跑的疾病
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")
# 定义要跑的变量
var_ls <- c("cardiovascular_acc", "hepatic_acc", "pulmonary_acc",
            "renal_acc", "metabolic_acc", "musculoskeletal_acc")
# 接受结果
var_mean_c_index <- c()
var_mean_c_index_lower <- c()
var_mean_c_index_upper <- c()
outcome_ls <- c()

# 开始跑结果
set.seed(2024)
for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]
  
  # 选择符合要求的数据
  dat_cox <- subset(dat_age, 
                    `MACE duration` > 0 &
                    `Renal failure duration` > 0 &
                    `T2D duration` > 0 & 
                    `COPD duration` > 0 &
                    `Liver diseases duration` > 0 &
                    `Arthritis duration` > 0)
  # 把数据分成十份
  folds <- createFolds(dat_cox$event, k = 10)
  for (i in 1:length(var_ls)) {
    var <- var_ls[i]
    # 初始化存储c-index的向量
    c_index_values <- c()
    c_index_lower_ls <- c()
    c_index_upper_ls <- c()
    
    # 十折交叉验证
    for(j in 1:10) {
      # 划分训练集和测试集
      test_indices <- folds[[j]]
      train_data <- dat_cox[-test_indices, ]
      test_data <- dat_cox[test_indices, ]
      
      # 构建Cox模型
      formula_covariates <- paste0("survobj ~ ", var)
      f <- as.formula(formula_covariates)
      survobj <- with(train_data, Surv(time, event))
      cox_fit <- coxph(formula = f, data = train_data, na.action = na.omit)
      
      # 预测风险评分
      test_data$predicted_risk <- predict(cox_fit, newdata = test_data, 
                                          type = "risk")
      
      # 计算c-index
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
    var_name == "cardiovascular_acc" ~ "Cardiovascular",
    var_name == "hepatic_acc" ~ "Hepatic",
    var_name == "pulmonary_acc" ~ "Pulmonary",
    var_name == "renal_acc" ~ "Renal",
    var_name == "metabolic_acc" ~ "Metabolic",
    var_name == "musculoskeletal_acc" ~ "Musculoskeletal",
  ))

dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c("Cardiovascular", 
                                       "Hepatic", 
                                       "Pulmonary", 
                                       "Renal", 
                                       "Metabolic", 
                                       "Musculoskeletal"))

write_rds(dat_plot, "c_index_plot_organ_acc_8disease.rds")

###### Plot: 已存好的结果
dat_plot <- read_rds("Data/tempplot/c_index_plot_organ_acc_8disease.rds")
plots_c_index <- list()
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")
dat_plot$var_name <- rep(c("Cardiovascular age", "Hepatic age",
                           "Pulmonary age", "Renal age",
                           "Metabolic age", "Musculoskeletal age"), times = 8)
dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c("Cardiovascular age", "Hepatic age",
                                       "Pulmonary age", "Renal age",
                                       "Metabolic age", "Musculoskeletal age"))
 
for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(dat_plot, outcome == item)
  p <- ggplot(dat_sub, aes(x = var_name, y = c_index, color = var_name)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = c_index_lower, ymax = c_index_upper), width = 0.1) +
    geom_segment(aes(x = 0, xend = var_name, y = c_index, yend = c_index),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% c("Cardiovascular age", "Hepatic age",
                                                        "Pulmonary age", "Renal age",
                                                        "Metabolic age", "Musculoskeletal age"))) +
    scale_color_manual(values = c("#E71D1D", "#4BB04A", "#FF7D01", "#F980BE", "#A35628", "#9850A6")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(angle = 90, size = 18, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 18, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 20, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
  
  if (i < 5) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.line.x = element_blank())
  }
  plots_c_index[[i]] <- p
}

plots_c_index[5]

arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 4, 
                            nrow = 2,
                            heights = c(1, 1.9))
# arranged_plots
# 添加纵坐标标题
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Absolute C-index", size = 18, rot = 90))

ggsave("fig2-d.pdf", plot = combined_plot, width = 16, height = 8)



############ 2.多器官的 age gap 之间的关联性
dat_age_male <- subset(dat_age, Sex == "male")
dat_age_female <- subset(dat_age, Sex == "female")

dat_age_male <- dplyr::select(dat_age_male, 35:39, 41)
dat_age_female <- dplyr::select(dat_age_female, 35:39, 41)


dat_age_all <- dplyr::select(dat_age, 13:18, 20)
new_col_name <- c("Overall age gap",
                  "Cardiovascular age gap",
                  "Hepatic age gap",
                  "Pulmonary age gap",
                  "Renal age gap",
                  "Metabolic age gap",
                  "Musculoskeletal age gap")
colnames(dat_age_male) <- new_col_name
colnames(dat_age_female) <- new_col_name

colnames(dat_age_all) <- new_col_name

corr_male <- round(cor(dat_age_male), 2)
corr_female <- round(cor(dat_age_female), 2)

corr_all <- round(cor(dat_age_all), 2)

library(corrplot)
mycol <- colorRampPalette(c("#06a7cd", "white", "#e74a32"), alpha = TRUE)

# 绘图
pdf("fig-s3.pdf", width = 12, height = 12)
# 绘制图形
print(corrplot(corr_all, 
               method = c('pie'),
               type = c('lower'), 
               col = mycol(100),
               outline = 'black',
               # order = c('original'),
               diag = TRUE,
               tl.cex = 1.8, 
               tl.col = 'black',
               addCoef.col = 'black', # 在现有样式中添加相关性系数数字，并指定颜色
               number.cex = 1.5, # 相关性系数数字标签大小
))
# 关闭设备
dev.off()

# 绘图
pdf("fig3-b2.pdf", width = 10, height = 10)
# 绘制图形
print(corrplot(corr_female, 
               method = c('pie'),
               type = c('lower'), 
               col = mycol(100),
               outline = 'black',
               # order = c('original'),
               diag = TRUE,
               tl.cex = 1.8, 
               tl.col = 'black',
               addCoef.col = 'black', # 在现有样式中添加相关性系数数字，并指定颜色
               number.cex = 1.5, # 相关性系数数字标签大小
))
# 关闭设备
dev.off()


############ 3.基线疾病人群的各器官的 age gap
# dat_medhis <- read_rds("Data/medhis_240510.rds")
# dat_no_disease <- dat_medhis[dat_medhis$`Medical history`=="diseases suffered for 10 years or more: no diseases\ndiseases suffered 5 to 10 years ago: no diseases\ndiseases suffered 1 to 5 years ago: no diseases\ndiseases suffered within 1 year: no diseases",]
# dat_no_disease <- dat_no_disease %>% inner_join(dat_age, by = "eid")
# dat_no_disease$`Medical history` <- NULL
dat_cvd_hepatic <- read_rds("Data/covariates_outcomes/cvd_hepatic_medhis.rds")
dat_age <- dat_age %>% inner_join(dat_cvd_hepatic, by = "eid")

dat_age <- dat_age %>%
  dplyr::mutate(baseline_cvd = case_when(
    `CVD duration` <= 0 ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(baseline_COPD = case_when(
    `COPD duration` <= 0 ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(baseline_liver_diseases = case_when(
    `Hepatic duration` <= 0 ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(baseline_renal_disease = case_when(
    `Renal failure duration` <= 0 ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(baseline_diabetes = case_when(
    `Diabetes duration` <= 0 ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(baseline_arthritis = case_when(
    `Arthritis duration` <= 0 ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(future_cvd = case_when(
    `CVD duration` > 0 & `CVD duration` <= 10 & `CVD diagnose` == 1 ~ 1,
    `CVD duration` <= 0 ~ 2,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(future_COPD = case_when(
    `COPD duration` > 0 & `COPD duration` <= 10 & `COPD diagnose` == 1 ~ 1,
    `COPD duration` <= 0 ~ 2,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(future_liver_diseases = case_when(
    `Liver diseases duration` > 0 & `Liver diseases duration` <= 10 & `Liver diseases diagnose` == 1 ~ 1,
    `Liver diseases duration` <= 0 ~ 2,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(future_renal_disease = case_when(
    `Renal failure duration` > 0 & `Renal failure duration` <= 10 & `Renal failure diagnose` == 1 ~ 1,
    `Renal failure duration` <= 0 ~ 2,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(future_diabetes = case_when(
    `T2D duration` > 0 & `T2D duration` <= 10 & `T2D diagnose` == 1 ~ 1,
    `T2D duration` <= 0 ~ 2,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(future_arthritis = case_when(
    `Arthritis duration` > 0 & `Arthritis duration` <= 10 & `Arthritis diagnose` == 1 ~ 1,
    `Arthritis duration` <= 0 ~ 2,
    TRUE ~ 0
  ))

# 定义一个函数来识别变量类型并计算相关性矩阵
compute_mixed_correlation <- function(risk_factors, age_vars) {
  # 自动识别哪些是顺序分类变量（因子类型）
  is_ordered_factor <- function(x) {
    is.factor(x) && is.ordered(x)
  }
  
  # 将顺序分类变量转换为数值
  risk_factors_numeric <- risk_factors %>% mutate_if(is_ordered_factor, as.numeric)
  
  # 初始化空的相关性矩阵
  correlation_matrix <- matrix(NA, nrow = ncol(risk_factors), ncol = ncol(age_vars))
  rownames(correlation_matrix) <- colnames(risk_factors)
  colnames(correlation_matrix) <- colnames(age_vars)
  
  # 计算相关性矩阵
  for (i in 1:ncol(risk_factors)) {
    for (j in 1:ncol(age_vars)) {
      if (is_ordered_factor(risk_factors[, i])) {
        # 使用Spearman相关系数
        correlation_matrix[i, j] <- cor(risk_factors_numeric[, i], age_vars[, j], method = "spearman", use = "pairwise.complete.obs")
      } else {
        # 使用Pearson相关系数
        correlation_matrix[i, j] <- cor(risk_factors[, i], age_vars[, j], method = "pearson", use = "pairwise.complete.obs")
      }
    }
  }
  
  return(correlation_matrix)
}

# 计算相关性矩阵
# install.packages("corrr")
library(corrr)

### baseline diseases
age_vars <- dat_age[, c(35:39, 41)]
risk_factors <- dat_age[, c(46:51)]

correlation_matrix <- compute_mixed_correlation(risk_factors, age_vars)
new_row_name <- c("Baseline CVD",
                  "Baseline COPD",
                  "Baseline Liver Diseases",
                  "Baseline Renal Diseases",
                  "Baseline Diabetes",
                  "Baseline Arthritis")
new_col_name <- c("Cardiovascular",
                  "Hepatic",
                  "Pulmonary",
                  "Renal",
                  "Metabolic",
                  "Musculoskeletal")

rownames(correlation_matrix) <- new_row_name
colnames(correlation_matrix) <- new_col_name

library(corrplot)
correlation_matrix <- round(correlation_matrix, 2)

mycol <- colorRampPalette(c("#06a7cd", "white", "#e74a32"), alpha = TRUE)

# 绘图
pdf("fig3-c.pdf", width = 10, height = 10)
# 绘制图形
print(corrplot(correlation_matrix, 
               method = c('ellipse'),
               type = c('full'), 
               col = mycol(100),
               outline = 'black',
               # order = c('original'),
               diag = TRUE,
               tl.cex = 1.8, 
               tl.col = 'black',
               addCoef.col = 'black', # 在现有样式中添加相关性系数数字，并指定颜色
               number.cex = 1.5, # 相关性系数数字标签大小
))
# 关闭设备
dev.off()




############ 4.探讨多器官的 age gap 分别对不同健康结局的影响（加协变量调整）
Cox_analysis <- function(dat_baseline, disease_ls, var_ls) {
  ### 感兴趣结果整合到列表
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
    
    # 选择符合要求的数据: 主分析
    dat_cox <- subset(dat_baseline,
                      `MACE duration` > 0 &
                      `Renal failure duration` > 0 &
                      `T2D duration` > 0 &
                      `COPD duration` > 0 &
                      `Liver diseases duration` > 0 &
                      `Arthritis duration` > 0)
    
    # 另外28个结局
    # dat_cox <- subset(dat_baseline,
    #                   time > 0)
    
    # if (item != "All-cause death") {
    #   dat_cox <- dat_cox %>%
    #     mutate(event = case_when(
    #       `All-cause death diagnose` == 1 ~ 2,
    #       TRUE ~ event
    #     ))
    # }
    
    for (i in 1:length(var_ls)) {
      var_name <- var_ls[i]
      # formula_covariates <- paste0("survobj ~ ", var_name)
      # 主分析
      # formula_covariates <- paste0("survobj ~ Age + Sex + ", var_name)
      formula_covariates <- paste0("survobj ~ Age + Sex + Income + Employment +
                                   Education + `Current smoker` + `Daily alcohol intake` +
                                   `Daily healthy food` + `Frequently processed meat` +
                                   `Hypertension history` + `Diabetes history` +
                                   BMI + `Waist-hip ratio` + `Systolic blood pressure` + ", var_name)
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

# 定义要跑的疾病
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")

# 另外28种疾病
# 另外28种健康结局
disease <- colnames(dat_outcome2)[2:65]
disease <- disease[seq(1, 64, 2)]
disease <- gsub(" diagnose", "", disease)
disease <- disease[c(1:26, 31:32)]
disease[3] <- "immunodeficiencies"
disease[8] <- "structural heart diseases"
disease[11] <- "arteries disorders"
disease[12] <- "venous thrombosis"
disease[14] <- "colitis"
disease[15] <- "gallbladder diseases"

names(dat_age)[c(39:40, 49:50, 55:56, 57:58, 61:62, 63:64)] <- c("immunodeficiencies diagnose",
                                                                 "immunodeficiencies duration",
                                                                 "structural heart diseases diagnose",
                                                                 "structural heart diseases duration",
                                                                 "arteries disorders diagnose",
                                                                 "arteries disorders duration",
                                                                 "venous thrombosis diagnose",
                                                                 "venous thrombosis duration",
                                                                 "colitis diagnose",
                                                                 "colitis duration",
                                                                 "gallbladder diseases diagnose",
                                                                 "gallbladder diseases duration")


# 定义要跑的变量
var_ls <- c("all_acc", "cardiovascular_acc", "hepatic_acc", "pulmonary_acc",
            "renal_acc", "metabolic_acc", "musculoskeletal_acc")

age_results_hr <- Cox_analysis(dat_baseline = dat_age,
                               disease_ls = disease,
                               var_ls = var_ls)

names(age_results_hr)[c(1, 2)] <- c("outcome", "var_name")
age_results_hr <- age_results_hr %>%
  mutate(var_name = case_when(
    var_name == "all_acc" ~ "Overall",
    var_name == "cardiovascular_acc" ~ "Cardiovascular",
    var_name == "hepatic_acc" ~ "Hepatic",
    var_name == "pulmonary_acc" ~ "Pulmonary",
    var_name == "renal_acc" ~ "Renal",
    var_name == "metabolic_acc" ~ "Metabolic",
    var_name == "musculoskeletal_acc" ~ "Musculoskeletal",
  ))

age_results_hr$var_name <- factor(age_results_hr$var_name, 
                                  levels = c("Overall",
                                             "Cardiovascular", 
                                             "Hepatic", 
                                             "Pulmonary", 
                                             "Renal", 
                                             "Metabolic", 
                                             "Musculoskeletal"))

plots_HR <- list()
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")

for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(age_results_hr, outcome == item)
  p <- ggplot(dat_sub, aes(x = var_name, y = HR, color = var_name)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
    geom_segment(aes(x = 0, xend = var_name, y = HR, yend = HR),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% c("Overall",
                                                        "Cardiovascular", 
                                                        "Hepatic", 
                                                        "Pulmonary", 
                                                        "Renal", 
                                                        "Metabolic", 
                                                        "Musculoskeletal"))) +
    scale_color_manual(values = c("#4480B3", "#E71D1D", "#4BB04A", "#FF7D01", "#F980BE", "#A35628", "#9850A6")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(angle = 90, size = 18, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 18, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 20, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
  
  # 主分析为5, 附加分析为 25
  if (i < 5) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.line.x = element_blank())
  }
  plots_HR[[i]] <- p
}

plots_HR[5]

arranged_plots <- ggarrange(plotlist = plots_HR, 
                            ncol = 4, 
                            nrow = 2,
                            heights = c(1, 1.8))

arranged_plots <- ggarrange(plotlist = plots_HR, 
                            ncol = 4, 
                            nrow = 7,
                            heights = c(1, 1, 1, 1, 1, 1, 1.8))
# arranged_plots
# 添加纵坐标标题
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Adjusted hazard ratio (HR)", size = 18, rot = 90))

ggsave("fig3-c.pdf", plot = combined_plot, width = 16, height = 8)
ggsave("extended_fig6.pdf", plot = combined_plot, width = 16, height = 24)


ggsave("fig-s2.pdf", plot = combined_plot, width = 16, height = 8)


############ 5.探讨器官之间的加速衰老差值与多种疾病之间的关联
dat_age <- read_csv("Data/Models/llama3_70b/original/llama3-70b-result_only_age.csv")
names(dat_age)[c(2:9)] <- c("biological age", "cardiovascular age",
                            "hepatic age", "pulmonary age",
                            "renal age", "metabolic system age",
                            "immune system age", "musculoskeletal age")

dat_cov <- read_rds("Data/covariates_outcomes/panel_indicators.rds")
dat_outcome <- read_rds("Data/covariates_outcomes/aging_outcomes.rds")

### 合并数据
dat_cov <- dplyr::select(dat_cov, 1:3, 22) # 计算暴露组与各个年龄的关联时用这个
dat_age <- dat_age %>% inner_join(dat_cov, by = "eid")
dat_age <- dat_age %>% inner_join(dat_outcome, by = "eid") # 做暴露组关联时不需要合并这个

dat_age <- dat_age %>% mutate(all_acc = `biological age` - Age)
dat_age <- dat_age %>% mutate(cardiovascular_acc = `cardiovascular age` - Age)
dat_age <- dat_age %>% mutate(hepatic_acc = `hepatic age` - Age)
dat_age <- dat_age %>% mutate(pulmonary_acc = `pulmonary age` - Age)
dat_age <- dat_age %>% mutate(renal_acc = `renal age` - Age)
dat_age <- dat_age %>% mutate(metabolic_acc = `metabolic system age` - Age)
dat_age <- dat_age %>% mutate(immune_acc = `immune system age` - Age)
dat_age <- dat_age %>% mutate(musculoskeletal_acc = `musculoskeletal age` - Age)
dat_age <- dat_age[dat_age$`pulmonary age` > 0 & dat_age$`immune system age` > 0,]
dat_age <- dat_age[!is.na(dat_age$`pulmonary age`) & !is.na(dat_age$`immune system age`),]

dat_age <- na.omit(dat_age)


dat_age <- dat_age %>%
  dplyr::mutate(cardiovascular_all_acc = `cardiovascular age` - `biological age`) %>%
  dplyr::mutate(hepatic_all_acc = `hepatic age` - `biological age`) %>%
  dplyr::mutate(pulmonary_all_acc = `pulmonary age` - `biological age`) %>%
  dplyr::mutate(renal_all_acc = `renal age` - `biological age`) %>%
  dplyr::mutate(metabolic_all_acc = `metabolic system age` - `biological age`) %>%
  dplyr::mutate(immune_all_acc = `immune system age` - `biological age`) %>%
  dplyr::mutate(musculoskeletal_all_acc = `musculoskeletal age` - `biological age`)

Cox_analysis <- function(dat_baseline, disease_ls) {
  ### 感兴趣结果整合到列表
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
    
    # 选择符合要求的数据
    dat_cox <- subset(dat_baseline, 
                      `MACE duration` > 0 &
                      `Renal diseases duration` > 0 &
                      `T2D duration` > 0 & 
                      `COPD duration` > 0 &
                      `Liver diseases duration` > 0 &
                      `Arthritis duration` > 0)
    
    var_ls <- c("cardiovascular_all_acc", "hepatic_all_acc",
                "pulmonary_all_acc", "renal_all_acc",
                "metabolic_all_acc", "musculoskeletal_all_acc")
    
    for (i in 1:length(var_ls)) {
      var_name <- var_ls[i]
      # formula_covariates <- paste0("survobj ~ ", var_name)
      formula_covariates <- paste0("survobj ~ Age + Sex + BMI + ", var_name)
      f <- as.formula(formula_covariates)
      # survobj <- with(dat_cox, Surv(time, event==1))
      survobj <- with(dat_cox, Surv(time, event))
      
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

# 定义要跑的疾病
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")

res_multi_organ_age_acc <- Cox_analysis(dat_age, disease)

names(res_multi_organ_age_acc)[c(1, 2)] <- c("outcome", "var_name")

res_multi_organ_age_acc <- res_multi_organ_age_acc %>%
  mutate(var_name = case_when(
    var_name == "cardiovascular_all_acc" ~ "Cardiovascular-Overall",
    var_name == "hepatic_all_acc" ~ "Hepatic-Overall",
    var_name == "pulmonary_all_acc" ~ "Pulmonary-Overall",
    var_name == "renal_all_acc" ~ "Renal-Overall",
    var_name == "metabolic_all_acc" ~ "Metabolic-Overall",
    var_name == "musculoskeletal_all_acc" ~ "Musculoskeletal-Overall"
  ))

res_multi_organ_age_acc$var_name <- factor(res_multi_organ_age_acc$var_name, 
                                           levels = c("Cardiovascular-Overall",
                                                      "Hepatic-Overall",
                                                      "Pulmonary-Overall",
                                                      "Renal-Overall",
                                                      "Metabolic-Overall",
                                                      "Musculoskeletal-Overall"))

plots_HR <- list()
disease <- c("All-cause death", "CHD", "Stroke", "COPD", 
             "Liver diseases", "Renal failure", "T2D", "Arthritis")
for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(res_multi_organ_age_acc, outcome == item)

  var_ls <- c("Cardiovascular-Overall",
              "Hepatic-Overall",
              "Pulmonary-Overall",
              "Renal-Overall",
              "Metabolic-Overall",
              "Musculoskeletal-Overall")
  
  p <- ggplot(dat_sub, aes(x = var_name, y = HR, color = var_name)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.15) +
    geom_segment(aes(x = 0, xend = var_name, y = HR, yend = HR),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% var_ls)) +
    scale_color_manual(values = c("#E71D1D", "#4BB04A", "#FF7D01", 
                                  "#F980BE", "#A35628", "#9850A6")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 18),
          axis.text.x = element_text(angle = 90, size = 18, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 18, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 20, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
  
  if (i < 5) {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.line.x = element_blank())
  }
  plots_HR[[i]] <- p
}

# plots_HR[5]

arranged_plots <- ggarrange(plotlist = plots_HR, 
                            ncol = 4, 
                            nrow = 2,
                            heights = c(1, 2.1))
# arranged_plots
# 添加纵坐标标题
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Adjusted hazard ratio (HR) with per age gap", size = 18, rot = 90))

ggsave("fig3-d.pdf", plot = combined_plot, width = 16, height = 9)





######################## ------ 4.correlation with risk factors ------
############ 1.社会经济、生活方式、环境与加速衰老的相关热图
# install.packages("pheatmap")
# install.packages("corrplot")
# install.packages("ambient")
# devtools::install_github("Github-Yilei/ggcor")
# devtools::install_github("jokergoo/ComplexHeatmap")
library(ggcor)
# library(ambient)
dat_env <- read_rds("Data/covariates_outcomes/exposure/exposure_lifestyle_social_env.rds")
dat_env <- dplyr::select(dat_env, -c(2:3))
dat_age <- dat_age %>% inner_join(dat_env, by = "eid")
dat_med <- read_rds("Data/covariates_outcomes/exposure/exposure_medhis.rds")
dat_age <- dat_age %>% inner_join(dat_med, by = "eid")

age_vars <- dat_age[, c(12:17, 19)]
risk_factors <- dat_age[, c(20:152, 154:170)]


### 进行偏相关分析：控制时序年龄、性别
dat_age$Sex <- as.numeric(dat_age$Sex) - 1
age_vars_ls <- colnames(age_vars)

# 初始化一个空的dataframe，用于合并
merge_res <- data.frame()
for (age_var in age_vars_ls) {
  partial_correlation_results <- list()
  for (factor in colnames(risk_factors)) {
    print(paste0(age_var, "-------", factor))
    # 删除包含NA的行
    complete_cases_data <- na.omit(data.frame(age_vars[[age_var]], risk_factors[[factor]], dat_age[, c("Age", "Sex")]))
    
    # 提取处理后的变量
    age_gap <- complete_cases_data[[1]]
    risk_factor <- complete_cases_data[[2]]
    risk_factor <- as.numeric(risk_factor)
    covariates <- complete_cases_data[, -c(1:2)]
    
    # 计算偏相关
    partial_correlation_result <- pcor.test(x = age_gap, 
                                            y = risk_factor, 
                                            z = covariates, 
                                            method = "pearson")
    
    # 保存结果
    partial_correlation_results[[factor]] <- partial_correlation_result
  }
  
  # 获取未校正的p值
  p_values <- sapply(partial_correlation_results, function(x) x$p.value)
  
  # Bonferroni校正
  p_adj_bonferroni <- p.adjust(p_values, method = "bonferroni")
  
  # FDR校正
  p_adj_bh <- p.adjust(p_values, method = "BH")
  
  # 效果量阈值设定
  effect_sizes <- sapply(partial_correlation_results, function(x) x$estimate)
  
  # 输出结果
  results_temp <- data.frame(
    age_gap = age_var,
    risk_factor = names(p_values),
    pearson_estimate = round(effect_sizes, 3),
    p_value = p_values,
    p_adj_bonferroni = p_adj_bonferroni,
    p_adj_bh = p_adj_bh
  )
  
  if (nrow(merge_res) == 0) {
    merge_res <- results_temp
  } 
  else {
    merge_res <- rbind(merge_res, results_temp)
  }
}

### 计算相关性矩阵
# install.packages("corrr")
library(corrr)
new_row_name <- c("Accommodation type (A house)",
                  "Accommodation type (A flat)",
                  "Accommodation type (Others)",
                  "Own outright of an accommodation",
                  "Own an accommodation with a mortgage",
                  "Rent an accommodation",
                  "Using gas hob",
                  "Using gas fire",
                  "Using open solid fuel fire",
                  "Heating types (Gas central heating)",
                  "Heating types (Electric storage heaters)",
                  "Heating types (Oil central heating)",
                  "Heating types (Portable gas)",
                  "Heating types (Solid fuel central heating)",
                  "Heating types (Open fire)",
                  "Average household income (Low level)",
                  "Average household income (Middle level)",
                  "Average household income (High level)",
                  "Employment (In paid employment)",
                  "Employment (Retired)",
                  "Employment (Looking after home)",
                  "Employment (Unable to work because of disability)",
                  "Employment (Unemployed)",
                  "Employment (Doing unpaid work)",
                  "Employment (Part-time work)",
                  "Pack years of smoking (proportion of life)",
                  "Number of days/week of MPA 10+ minutes",
                  "Number of days/week of VPA 10+ minutes",
                  "MET minutes per week for MPA",
                  "MET minutes per week for VPA",
                  "PM10",
                  "PM2.5",
                  "PM2.5 absorbance",
                  "PM2.5-10",
                  "Average daytime sound level of noise",
                  "Average evening sound level of noise",
                  "Average night-time sound level of noise",
                  "Greenspace percentage (1000m)",
                  "Domestic garden percentage (1000m)",
                  "Water percentage (1000m)",
                  "Greenspace percentage (300m)",
                  "Domestic garden percentage (300m)",
                  "Water percentage (300m)",
                  "Natural environment percentage (1000m)",
                  "Natural environment percentage (300m)",
                  "Illnesses of father (Dementia)",
                  "Illnesses of father (Hypertension)",
                  "Illnesses of father (COPD)",
                  "Illnesses of mother (Dementia)",
                  "Illnesses of mother (Hypertension)",
                  "Illnesses of mother (COPD)")
new_col_name <- c("Overall age gap",
                  "Cardiovascular age gap",
                  "Hepatic age gap",
                  "Pulmonary age gap",
                  "Renal age gap",
                  "Metabolic age gap",
                  "Musculoskeletal age gap")

# 修改名称
risk_factors_names <- colnames(risk_factors)
risk_factors_names[c(1:25, 29, 55:56, 58:59, 65:71, 72:79, 128, 130,
                     131, 138, 140, 141)] <- new_row_name
age_gaps_names <- new_col_name

# 修改结果表名称
merge_res$age_gap <- rep(age_gaps_names, each = 150)
merge_res$risk_factor <- rep(risk_factors_names, times = 7)

# 先保存结果
write_csv(merge_res, "exposome_agegap_cor.csv")

merge_res_r <- dplyr::select(merge_res, 1:3)
wide_df <- merge_res_r %>%
  pivot_wider(names_from = age_gap, values_from = pearson_estimate, values_fill = list(pearson_estimate = 0))

correlation_matrix <- as.matrix(wide_df[,-1])
rownames(correlation_matrix) <- risk_factors_names
colnames(correlation_matrix) <- age_gaps_names
correlation_matrix_ls <- cor_tbl(correlation_matrix, cluster = F)

p <- correlation_matrix_ls %>%
  quickcor(circular = T, 
           cluster = F,
           open = 45, 
           outer = 1, 
           inner = 1
           ) +
  geom_colour(colour = "white", size = 0.25) +
  scale_fill_gradient2(low = "#3288bd", mid = "white", high = "#d53e4f") +
  set_p_xaxis(size = 6) +
  set_p_yaxis(size = 6) +
  theme(
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  )

# 打开PDF设备
pdf("Fig4-a.pdf", width = 22, height = 20)
# 绘制图形
print(p)
# 关闭设备
dev.off()




############ 2.蛋白质组富集分析
library(enrichplot)
library(clusterProfiler)
library(limma)
library(org.Hs.eg.db)
library(ggrepel)
# BiocManager::install("msigdbr")
library(msigdbr)
library(GseaVis)

# 读取蛋白组数据
### 读取数据
dat_age <- read_csv("Data/Models/llama3_70b/llama3-70b-result_only_age.csv")
names(dat_age)[c(2:9)] <- c("biological age", "cardiovascular age",
                            "hepatic age", "pulmonary age",
                            "renal age", "metabolic system age",
                            "immune system age", "musculoskeletal age")

dat_cov <- read_rds("Data/covariates_outcomes/panel_indicators.rds")
dat_cov <- dplyr::select(dat_cov, 1:3)
dat_age <- dat_age %>% inner_join(dat_cov, by = "eid")

dat_age <- dat_age %>% mutate(all_acc = `biological age` - Age)
dat_age <- dat_age %>% mutate(cardiovascular_acc = `cardiovascular age` - Age)
dat_age <- dat_age %>% mutate(hepatic_acc = `hepatic age` - Age)
dat_age <- dat_age %>% mutate(pulmonary_acc = `pulmonary age` - Age)
dat_age <- dat_age %>% mutate(renal_acc = `renal age` - Age)
dat_age <- dat_age %>% mutate(metabolic_acc = `metabolic system age` - Age)
dat_age <- dat_age %>% mutate(immune_acc = `immune system age` - Age)
dat_age <- dat_age %>% mutate(musculoskeletal_acc = `musculoskeletal age` - Age)
dat_age <- dat_age[dat_age$`pulmonary age` > 0 & dat_age$`immune system age` > 0,]
dat_age <- dat_age[!is.na(dat_age$`pulmonary age`) & !is.na(dat_age$`immune system age`),]

dat_age <- na.omit(dat_age)



# 定义一个函数，用中位数替换NA
dat_protein <- read_csv("Data/covariates_outcomes/proteome_0.csv")
replace_na_with_median <- function(column) {
  median_value <- median(column, na.rm = TRUE)
  column[is.na(column)] <- median_value
  return(column)
}

# 使用apply函数应用到数据框的每一列
dat_protein <- as.data.frame(apply(dat_protein, 2, replace_na_with_median))

# 合并数据
dat_age <- dat_age %>% inner_join(dat_protein, by = "eid")

# 备份数据
dat_age_backup <- dat_age
dat_age <- dat_age_backup
dat_age <- dplyr::select(dat_age, 1, 10:1482)

# 对 acc 进行排序
dat_age <- dat_age[order(dat_age$all_acc), ]

# 计算分组的边界
n <- nrow(dat_age)
top_boundary <- n * 0.9
bottom_boundary <- n * 0.1

# 分配分组标签, 合并数据
dat_age$group <- "Other"
dat_age$group[1:(bottom_boundary + 1)] <- "Bottom"
dat_age$group[(top_boundary + 1):n] <- "Top"

dat_age_high <- subset(dat_age, group == "Top")
dat_age_low <- subset(dat_age, group == "Bottom")
dat_merge_analysis <- rbind(dat_age_high, dat_age_low)

# 生成分组信息列
group <- factor(dat_merge_analysis$group)
Age <- dat_merge_analysis$Age
Sex <- factor(dat_merge_analysis$Sex)

###### 蛋白质差异表达分析
### limma 进行差异表达分析
# 整体年龄
# 器官年龄
dat_merge_analysis <- dplyr::select(dat_merge_analysis, -c(1:11, 1475))
dat_merge_analysis <- t(dat_merge_analysis)

# 设计矩阵
design <- model.matrix(~ 0 + group + Age + Sex)

# 比较组别
contrast.matrix <- makeContrasts("groupTop-groupBottom", levels = design)

# 差异表达分析
fit <- lmFit(dat_merge_analysis, design)
fit2 <- contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit2)
results <- topTable(fit2, adjust = "BH", number = Inf)  # 获取全部结果


###### GSEA analysis: 基于所有基因的排序列表进行 GSEA 分析
# 假设您的蛋白质名称在结果中存储在 rownames(results)
protein_names <- rownames(results)

# 将蛋白质名称转换为基因名称
protein_to_gene <- bitr(protein_names, fromType = "SYMBOL", 
                        toType = "ENTREZID", OrgDb = org.Hs.eg.db)

# 将基因ID添加到结果中
results <- results %>%
  rownames_to_column("Protein") %>%
  left_join(protein_to_gene, by = c("Protein" = "SYMBOL"))

# 检查是否有未匹配的蛋白质
sum(is.na(results$ENTREZID))

# 移除未匹配的蛋白质
results <- results[!is.na(results$ENTREZID), ]

# 准备基因列表
gene_list <- results$logFC
names(gene_list) <- results$ENTREZID
gene_list <- sort(gene_list, decreasing = TRUE)



### GSEA 富集分析, GO database
# verbose 是否显示详细的运行信息
# Ont: BP: Biological Process, MF: Molecular Function, CC: Cellular Component
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

GSEA_GO_SIMPLE <- simplify(GSEA_GO)
terms <- GSEA_GO_SIMPLE@result$ID[1:3]

# 气泡图
p_dot <- dotplotGsea(GSEA_GO_SIMPLE, topn = 25)
p_dot <- dotplotGsea(GSEA_GO_SIMPLE, topn = 20) # 总图
ggsave("GSEA_Dotplot.pdf", plot = p_dot$plot, width = 12, height = 6)


### GO analysis: 基于显著差异表达基因进行 GO 富集分析
# 选择显著差异表达的蛋白质
significant_proteins <- subset(results, adj.P.Val < 0.01 & abs(logFC) > 0.2)
# 这里用bitr函数进行转换，可以根据你的数据进行调整
GO_database <- 'org.Hs.eg.db'
GO <- enrichGO(significant_proteins$ENTREZID, # GO 富集分析
               OrgDb = GO_database,
               keyType = "ENTREZID", # 设定读取的 gene ID 类型
               ont = c("BP"), # (Biological Process, Cellular Component, Mollecular Function）
               pvalueCutoff = 0.01, # 设定 p 值阈值
               qvalueCutoff = 0.01, # 设定 q 值阈值
               readable = T)

GO_SIMPLE <- simplify(GO)

p_bar <- barplot(GO_SIMPLE, showCategory = 10, font.size = 18) + 
  theme(legend.position = "none")
p_bar
ggsave("Fig4-e.pdf", plot = p_bar, width = 10, height = 6)
ggsave("extend_GO_Plot_cardiovascular.pdf", plot = p_bar, width = 10, height = 6)



############ 3.蛋白质差异表达火山图
# 设置p-value的最小阈值
min_p_value <- 5e-323
results$adj.P.Val <- pmax(results$adj.P.Val, min_p_value)

# 计算 -log10 FDR
results <- results %>% mutate(Minus_Log10_FDR = -log10(adj.P.Val))

# 设置logFC的阈值
threshold <- 0.1

# 添加显著性标记
results <- results %>% mutate(Significance = case_when(
  logFC > 0 & adj.P.Val < 0.01 ~ "Upregulated",
  logFC < 0 & adj.P.Val < 0.01 ~ "Downregulated",
  TRUE ~ "Not Significant"
))

# 筛选出显著上调和下调的蛋白质用于标记
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

# 绘制火山图
p_val <- ggplot(results, aes(x = logFC, y = Minus_Log10_FDR, color = Significance)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = c("Not Significant" = "grey", "Upregulated" = "#d73027", "Downregulated" = "#4575b4")) +
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
        axis.line = element_line()
        # panel.border = element_rect(color = "black", fill = NA),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank()
        ) +
  geom_text_repel(data = label_df, 
                  aes(label = Protein), 
                  size = 4, 
                  box.padding = 0.3, 
                  point.padding = 0.3, 
                  segment.color = 'grey50') +
  scale_x_continuous(breaks = seq(-1, 1, by = 1), limits = c(-1.2, 1.8))

ggsave("Fig4-b.pdf", plot = p_val, width = 7, height = 6)



############ 4.代谢组差异分析
# 读取蛋白组数据
dat_metabolism <- read_csv("Data/covariates_outcomes/NMR_instance_0.csv")
# 合并数据
dat_age <- dat_age %>% inner_join(dat_metabolism, by = "eid")

# 备份数据
dat_age_backup <- dat_age
dat_age <- dat_age_backup
dat_age <- dplyr::select(dat_age, 1, 10, 11, 76, 77:325)
names(dat_age)[4] <- "all_acc"

# 对 acc 进行排序
dat_age <- dat_age[order(dat_age$all_acc), ]

# 计算分组的边界
n <- nrow(dat_age)
top_boundary <- n * 0.9
bottom_boundary <- n * 0.1

# 分配分组标签, 合并数据
dat_age$group <- "Other"
dat_age$group[1:(bottom_boundary + 1)] <- "Bottom"
dat_age$group[(top_boundary + 1):n] <- "Top"

dat_age_high <- subset(dat_age, group == "Top")
dat_age_low <- subset(dat_age, group == "Bottom")
dat_merge_analysis <- rbind(dat_age_high, dat_age_low)

# 生成分组信息列
group <- factor(dat_merge_analysis$group)
Age <- dat_merge_analysis$Age
Sex <- factor(dat_merge_analysis$Sex)

###### 代谢组差异表达分析
### limma 进行差异表达分析
# 整体生理年龄
# 器官生理年龄
dat_merge_analysis <- dplyr::select(dat_merge_analysis, -c(1:4, 254))
dat_merge_analysis <- t(dat_merge_analysis) 

# 设计矩阵
design <- model.matrix(~ 0 + group + Age + Sex)

# 比较组别
contrast.matrix <- makeContrasts("groupTop-groupBottom", levels = design)

# 差异表达分析
fit <- lmFit(dat_merge_analysis, design)
fit2 <- contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit2)
results <- topTable(fit2, adjust = "BH", number = Inf)  # 获取全部结果
results <-  results %>% rownames_to_column("Metabolite")

# 换缩写名字
# meta_name <- read_csv("Data/varnames/metabolic_newname.csv")
results$Metabolite <- meta_name$Abbreviations
results <- subset(results, 
                  Metabolite != "Degree of Unsaturation" &
                    Metabolite != "Average Diameter for HDL Particles" &
                    Metabolite != "L-HDL Particles Concentration" &
                    Metabolite != "Average Diameter for VLDL Particles" &
                    Metabolite != "XL-HDL Particles Concentration" &
                    Metabolite != "Average Diameter for LDL Particles" &
                    Metabolite != "S-HDL Particles Concentration" &
                    Metabolite != "M-HDL Particles Concentration" &
                    Metabolite != "VLDL Particles Concentration" &
                    Metabolite != "HDL Particles Concentration" &
                    Metabolite != "Total Concentration of Lipoprotein Particles" &
                    Metabolite != "M-LDL Particles Concentration" &
                    Metabolite != "S-VLDL Particles Concentration" &
                    Metabolite != "IDL Particles Concentration" &
                    Metabolite != "L-LDL Particles Concentration" &
                    Metabolite != "L-VLDL Particles Concentration" &
                    Metabolite != "S-LDL Particles Concentration" &
                    Metabolite != "M-VLDL Particles Concentration" &
                    Metabolite != "XS-VLDL Particles Concentration" &
                    Metabolite != "Remnant Cholesterol (Non-HDL, Non-LDL -Cholesterol)" &
                    Metabolite != "XL-VLDL Particles Concentration" &
                    Metabolite != "LDL Particles Concentration" &
                    Metabolite != "XXL-VLDL Particles Concentration" &
                    Metabolite != "Total Concentration of Branched-Chain Amino Acids (Leucine + Isoleucine + Valine)")

# 添加显著性标记
results_sig <- results %>% mutate(Significance = case_when(
  logFC > 0 & adj.P.Val < 0.01 ~ "Upregulated",
  logFC < 0 & adj.P.Val < 0.01 ~ "Downregulated",
  TRUE ~ "Not Significant"
))

### 绘制上调与下调柱状图
upregulated_res <- subset(results_sig, Significance == "Upregulated")
downregulated_res <- subset(results_sig, Significance == "Downregulated")

upregulated_res <- upregulated_res[order(-upregulated_res$logFC), ][1:15, ]
downregulated_res <- downregulated_res[order(downregulated_res$logFC), ][1:15, ]

upregulated_res$Metabolite <- factor(upregulated_res$Metabolite, levels = upregulated_res$Metabolite[order(upregulated_res$logFC)])
downregulated_res$Metabolite <- factor(downregulated_res$Metabolite, levels = downregulated_res$Metabolite[order(-downregulated_res$logFC)])

# 创建两个单独的柱状图
plot_down <- ggplot(downregulated_res, aes(x = Metabolite, y = logFC)) +
  geom_bar(stat = "identity", fill = "#3288bd", alpha = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 15 Downregulated Metabolites", y = "", x = "") +
  theme(axis.text = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 20)) +
  scale_y_continuous(limits = c(-4.5, 0), breaks = seq(-4, 0, by = 1))
# 整体y坐标：-6.5, -5

plot_up <- ggplot(upregulated_res, aes(x = Metabolite, y = logFC)) +
  geom_bar(stat = "identity", fill = "#d53e4f", alpha = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 15 Upregulated Metabolites", y = "", x = "") +
  theme(axis.text = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 20)) +
  scale_y_continuous(limits = c(0, 4.1), breaks = seq(0, 4, by = 1)) +
  scale_x_discrete(position = "top")
# 整体y坐标：7.5, 7
# 合并两个图，并添加公共的横坐标标题
bottom_text <- grid.text("log2 (Fold Change)", x = 0.5, y = 0.5, 
                         gp = gpar(fontsize = 20, col = "black"), 
                         hjust = 0.5)

combined_plot <- grid.arrange(plot_down,
                              plot_up, ncol = 2,
                              widths = c(1, 1), bottom = bottom_text)
# 打印合并后的图
# print(combined_plot)
ggsave("Fig4-c.pdf", combined_plot, height = 8, width = 16)




######################## ------ 5.applications of age gap ------
############ 1.衰老时钟
dat_age <- subset(dat_age, all_acc >= -10 & all_acc <= 15 &
                    cardiovascular_acc >= -10 & cardiovascular_acc <= 15 &
                    hepatic_acc >= -10 & hepatic_acc <= 15 &
                    pulmonary_acc >= -10 & pulmonary_acc <= 15 &
                    renal_acc >= -10 & renal_acc <= 15 &
                    metabolic_acc >= -10 & metabolic_acc <= 15 &
                    musculoskeletal_acc >= -10 & musculoskeletal_acc <= 15)
dat_age <- subset(dat_age, Age >= 40 & Age <= 70)

# 计算每个年龄组的平均值和方差
age_gap_summary <- dat_age %>%
  group_by(Age) %>%
  summarize(
    mean_age_gap = mean(all_acc, na.rm = TRUE),
    sd_age_gap = sd(all_acc, na.rm = TRUE)
  )

# 将结果保存为CSV文件
write_csv(age_gap_summary, "age_gap_summary_ukb.csv")



######################## 找出衰老的关键时间点：241025
dat_age <- subset(dat_age, Age >= 40 & Age < 70)
dat_age <- subset(dat_age, all_acc >= -10 & all_acc <= 15 &
                    cardiovascular_acc >= -10 & cardiovascular_acc <= 15 &
                    hepatic_acc >= -10 & hepatic_acc <= 15 &
                    pulmonary_acc >= -10 & pulmonary_acc <= 15 &
                    renal_acc >= -10 & renal_acc <= 15 &
                    metabolic_acc >= -10 & metabolic_acc <= 15 &
                    musculoskeletal_acc >= -10 & musculoskeletal_acc <= 15)

dat_age$Age <- as.factor(dat_age$Age)


###### 方差分析与Tukey's HSD
# Perform ANOVA
anova_model <- aov(all_acc ~ Age, data = dat_age)
# Perform Tukey's HSD
tukey_hsd <- TukeyHSD(anova_model)
# Convert Tukey's HSD results into a data frame
tukey_results <- as.data.frame(tukey_hsd$Age)
tukey_results$Comparison <- rownames(tukey_results)
# Add ANOVA summary to the data frame
anova_summary <- summary(anova_model)
anova_table <- anova_summary[[1]]  # Extracting the ANOVA table
anova_results <- as.data.frame(anova_table)
anova_results$Source <- rownames(anova_results)

write_csv(tukey_results, "tukey_results_ukb.csv")
write_csv(anova_results, "anova_results_ukb.csv")


# Combine line plot and boxplot in a single plot
p <- ggplot(dat_age, aes(x = as.numeric(as.character(Age)), y = all_acc)) +
  # geom_boxplot(aes(group = Age), color = "#045a8d", alpha = 0.5, outlier.shape = NA) +
  # 使用 annotate 添加阴影框
  annotate("rect", xmin = 49, xmax = 51, ymin = -Inf, ymax = Inf, 
           fill = "#fee391", alpha = 0.3) + # 第一段阴影框
  annotate("rect", xmin = 59, xmax = 62, ymin = -Inf, ymax = Inf, 
           fill = "#fee391", alpha = 0.3) + # 第二段阴影框
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", size = 1.2, linetype = "solid") +
  stat_summary(fun = mean, geom = "point", color = "#ef3b2c", size = 3) +
  labs(# title = "Age gap distribution by chronological age (UKB)",
       x = "Chronological age (years)", 
       y = "Age gap (years)") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.grid.major = element_blank(),  # 移除主网格线
        panel.grid.minor = element_blank(),  # 移除次网格线
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18, color = "black"),
        plot.title = element_text(size = 22))

p
ggsave("Fig5-e1.pdf", p, width = 15, height = 8)



############ 2.预测疾病
library(caret)
library(pROC)
library(ROSE)

### 构造类别
dat_age <- dat_age %>%
  dplyr::mutate(death_1yrs = case_when(
    `Death duration` <= 1 & `Death diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(death_5yrs = case_when(
    `Death duration` <= 5 & `Death diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(death_10yrs = case_when(
    `Death duration` <= 10 & `Death diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(death_15yrs = case_when(
    `Death duration` <= 15 & `Death diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(mi_1yrs = case_when(
    `Myocardial Infarction duration` <= 1 & `Myocardial Infarction diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(mi_5yrs = case_when(
    `Myocardial Infarction duration` <= 5 & `Myocardial Infarction diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(mi_10yrs = case_when(
    `Myocardial Infarction duration` <= 10 & `Myocardial Infarction diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(mi_15yrs = case_when(
    `Myocardial Infarction duration` <= 15 & `Myocardial Infarction diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(hf_1yrs = case_when(
    `Heart Failure duration` <= 1 & `Heart Failure diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(hf_5yrs = case_when(
    `Heart Failure duration` <= 5 & `Heart Failure diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(hf_10yrs = case_when(
    `Heart Failure duration` <= 10 & `Heart Failure diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(hf_15yrs = case_when(
    `Heart Failure duration` <= 15 & `Heart Failure diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(liver_1yrs = case_when(
    `Liver Cirrhosis and Failure duration` <= 1 & `Liver Cirrhosis and Failure diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(liver_5yrs = case_when(
    `Liver Cirrhosis and Failure duration` <= 5 & `Liver Cirrhosis and Failure diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(liver_10yrs = case_when(
    `Liver Cirrhosis and Failure duration` <= 10 & `Liver Cirrhosis and Failure diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  )) %>%
  dplyr::mutate(liver_15yrs = case_when(
    `Liver Cirrhosis and Failure duration` <= 15 & `Liver Cirrhosis and Failure diagnose` == 1 ~ "Positive",
    TRUE ~ "Negative"
  ))

dat_age <- dat_age %>%
  dplyr::mutate(cvd_his = case_when(
    `CVD duration` <= 0 ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::mutate(hepatic_his = case_when(
    `Hepatic duration` <= 0 ~ 1,
    TRUE ~ 0
  ))

dat_age$cvd_his <- factor(dat_age$cvd_his)
dat_age$hepatic_his <- factor(dat_age$hepatic_his)

# 采样函数，减少点数
sample_roc <- function(roc_curve, num_points = 1000) {
  indices <- seq(1, length(roc_curve$sensitivities), 
                 length.out = num_points)
  data.frame(
    TPR = roc_curve$sensitivities[indices],
    FPR = 1 - roc_curve$specificities[indices],
    stringsAsFactors = FALSE
  )
}

names(dat_age)[c(50, 51)] <- c("All-cause death diagnose",
                               "All-cause death duration")
names(dat_age)[c(54:57)] <- c("Heart failure diagnose",
                              "Heart failure duration",
                              "Liver cirrhosis and failure diagnose",
                              "Liver cirrhosis and failure duration")

### 开始跑模型
set.seed(2024)
disease_ls <- c("All-cause death",
                "Heart failure",
                "Liver cirrhosis and failure")
outcome_ls <- c("death",
                "hf",
                "liver")
age_gap_ls <- c("all_acc",
                "all_acc",
                "all_acc")

z <- 1
for (i in 1:length(disease_ls)) {
  item <- disease_ls[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]
  
  dat_age_analysis <- subset(dat_age, time > 0)
  
  # 结局变量名
  outcome_name <- outcome_ls[i]
  # age gap
  age_gap <- age_gap_ls[i]
  # 循环不同时间窗口
  outcome_year_ls <- c("_1yrs", "_5yrs", "_10yrs")
  outcome_year_plot_ls <- c("1-year", "5-years", "10-years")
  
  for (j in 1:length(outcome_year_ls)) {
    outcome_year <- outcome_year_ls[j]
    outcome <- paste0(outcome_name, outcome_year)
    
    outcome_year_plot <- outcome_year_plot_ls[j]
    
    # 定义训练控制，使用五折交叉验证
    train_control <- trainControl(
      method = "cv", 
      number = 5,
      summaryFunction = twoClassSummary, # 用于计算AUC
      classProbs = TRUE # 需要计算类概率
    )
    
    formatted_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    print(formatted_time)
    
    # 训练多个 logistic 模型
    f_model1 <- as.formula(paste0(outcome, " ~ ", age_gap))
    f_model2 <- as.formula(paste0(outcome, " ~ Age + Sex"))
    f_model3 <- as.formula(paste0(outcome, " ~ Age + Sex + ", age_gap))
    f_model4 <- as.formula(paste0(outcome, " ~ Age + Sex + BMI + `Waist-hip ratio` + `Daily alcohol intake` + `Current smoker` + cvd_his + hepatic_his"))
    f_model5 <- as.formula(paste0(outcome, " ~ Age + Sex + BMI + `Waist-hip ratio` + `Daily alcohol intake` + `Current smoker` + cvd_his + hepatic_his + ", age_gap))
    
    models <- list(
      model1 = train(f_model1, data = dat_age_analysis, method = "glm", family = "binomial", trControl = train_control, metric = "ROC"),
      model2 = train(f_model2, data = dat_age_analysis, method = "glm", family = "binomial", trControl = train_control, metric = "ROC"),
      model3 = train(f_model3, data = dat_age_analysis, method = "glm", family = "binomial", trControl = train_control, metric = "ROC"),
      model4 = train(f_model4, data = dat_age_analysis, method = "glm", family = "binomial", trControl = train_control, metric = "ROC"),
      model5 = train(f_model5, data = dat_age_analysis, method = "glm", family = "binomial", trControl = train_control, metric = "ROC")
    )
    
    print("---------------------------------------------------")
    formatted_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    print(formatted_time)
    
    # 得到roc曲线相应的值
    roc_curves <- lapply(models, function(model) {
      predictions <- predict(model, dat_age_analysis, type = "prob")[,2]
      roc(dat_age_analysis[[outcome]], predictions)
    })
    
    ### 计算AUC值, 原始的，没有95%置信区间
    auc_values <- sapply(roc_curves, function(roc) auc(roc))
    
    # 计算AUC值并得到95%置信区间
    auc_values <- sapply(roc_curves, function(roc) {
      ci <- ci.auc(roc)  # 计算AUC的95%置信区间
      return(c(AUC = auc(roc), Lower_CI = ci[1], Upper_CI = ci[3]))
    })
    
    # 合并ROC曲线数据
    roc_data <- do.call(rbind, lapply(names(roc_curves), function(name) {
      roc_curve <- roc_curves[[name]]
      
      if (name %in% c("model4", "model5")) {
        sampled_roc_curve <- sample_roc(roc_curve, num_points = 6000) # 采样到10000个点
      } 
      else {
        sampled_roc_curve <- data.frame(
          TPR = roc_curve$sensitivities,
          FPR = 1 - roc_curve$specificities,
          stringsAsFactors = FALSE
        )
      }
      
      sampled_roc_curve$Model <- name
      return(sampled_roc_curve)
    }))
    
    # 确保每个模型的FPR和TPR长度一致
    roc_data <- roc_data[complete.cases(roc_data), ]
    
    # 绘制多个模型的ROC曲线
    colors <- c("model1" = "#878787", 
                "model2" = "#a6d96a",
                "model3" = "#abd9e9",
                "model4" = "#f46d43",
                "model5" = "#a50026")
    
    # 保存一份
    write_rds(roc_data, paste0(outcome, "_roc_data.rds"))
    auc_table <- data.frame(auc_values)
    auc_table$Model <- rownames(auc_table)
    write_rds(auc_table, paste0(outcome, "_auc.rds"))
    # 转换为数据框并重命名列
    auc_table <- as.data.frame(t(auc_values))
    colnames(auc_table) <- c("AUC", "Lower_CI", "Upper_CI")
    auc_table$Model <- rownames(auc_table)
    # write_rds(auc_table, paste0(outcome, "_auc_with_CI.rds"))
    write_csv(auc_table, paste0(outcome, "_auc_with_CI.csv"))
    
    ### 作图
    p <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors, labels = c(
        paste("Age gap (AUC =", round(auc_values["model1"], 2), ")"),
        paste("Age + Sex (AUC =", round(auc_values["model2"], 2), ")"),
        paste("Age + Sex + Age gap (AUC =", round(auc_values["model3"], 2), ")"),
        paste("Age + Sex + Body + Lifestyle + Diagnoses (AUC =", round(auc_values["model4"], 2), ")"),
        paste("Age + Sex + Body + Lifestyle + Diagnoses + Age gap (AUC =", round(auc_values["model5"], 2), ")")
      )) +
      ggtitle(paste0(item, " (", outcome_year_plot, ")")) +
      theme_minimal() +
      theme(legend.title = element_blank(),
            legend.position = c(0.59, 0.13),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            panel.grid.major = element_blank(),  # 移除主网格线
            panel.grid.minor = element_blank(),  # 移除次网格线
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14, color = "black"),
            legend.text = element_text(size = 13, hjust = 1),
            plot.title = element_text(size = 18)
      ) +
      guides(color = guide_legend(label.position = "left", label.hjust = 1, keywidth = 1, keyheight = 1)) +
      labs(x = "False Positive Rate", y = "True Positive Rate") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey")

    # # 存到 plots 里面
    pdf_name <- paste0("fig5-c", as.character(z), ".pdf")
    ggsave(pdf_name, plot = p, width = 8, height = 8)
    
    # 循环
    print("-------------------- over ---------------------------")
    formatted_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    print(formatted_time)
    print(outcome)
    z <- z + 1
  }
}




######################## ------ 6.interpretability of LLM ------
############ 1.扰动分析
# install.packages("effsize")
library(effsize)
### 原始数据
dat_analysis <- read_csv("Data/Models/llama3_70b_perturbation/llama3-70B-analyisis_newprompt.csv")
dat_analysis <- dplyr::select(dat_analysis, 1, 4)
names(dat_analysis)[2] <- "old_BA"
### 协变量
dat_cov <- read_rds("Data/covariates_outcomes/panel_indicators.rds")
# dat_cov <- dplyr::select(dat_cov, 1, 2)
dat_telomere <- read_csv("Data/covariates_outcomes/telomere.csv")
dat_telomere <- select(dat_telomere, 1:2, 5)
names(dat_telomere)[c(2, 3)] <- c("telomere", "telomere_adjusted")
dat_telomere <- na.omit(dat_telomere)
# 合并
dat_analysis <- dat_analysis %>% inner_join(dat_cov, by = "eid")
dat_analysis$acc <- dat_analysis$old_BA - dat_analysis$Age
# dat_analysis <- dplyr::select(dat_analysis, 1, 4)
dat_analysis <- dat_analysis %>% inner_join(dat_telomere, by = "eid")

### 遍历获取文件夹名
folder_path <- "Data/Models/llama3_70b_perturbation/perturbation_241012"
file_names <- list.files(folder_path, full.names = FALSE)
# file_names <- file_names[1:14]
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
  dat_var <- dplyr::select(dat_var, 1, 4) # 扰动后的数据
  # 重命名
  names(dat_var)[2] <- "perturbation_BA" # 扰动后的 BA
  dat_cov_cohen <- dplyr::select(dat_cov, 1, 2)
  dat_var <- dat_var %>% inner_join(dat_cov_cohen, by = "eid") # 合并协变量
  
  ### 计算 Cohen's d 效应量
  # Cohen's d 效应量：小效应量（d ≈ 0.2），中等效应量（d ≈ 0.5），大效应量（d ≈ 0.8 或更大）
  # Cohen's d 效应量：d = (x1均值 - x2均值) / 标准差
  dat_var$perturbation_acc <- dat_var$perturbation_BA - dat_var$Age # 计算扰动后的 acc
  dat_var <- dplyr::select(dat_var, 1, 4) # 只剩下扰动后的 acc
  # 合并 dat_analysis，从而得到扰动人群原始的 acc 与扰动后的 acc
  dat_var_cohen <- dat_var
  dat_var_cohen <- dat_var_cohen %>% inner_join(dat_analysis, by = "eid")
  # 计算
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
  
  ###### 计算 线性回归 的效应量
  # 原始的数据, 与 inner_join 不同, semi_join 不会返回右侧数据框中的列 
  dat_linear <- dat_analysis %>% semi_join(dat_var, by = "eid")
  dat_linear$group <- "old_var"
  dat_linear <- dplyr::select(dat_linear, 1, 50, 53)
  # 扰动的数据
  dat_var_linear <- dat_var
  names(dat_var_linear)[2] <- "acc"
  dat_var_linear$group <- "new_var"
  # 合并
  dat_linear <- rbind(dat_linear, dat_var_linear)
  dat_linear$group <- factor(dat_linear$group, levels = c("old_var", "new_var"))
  dat_linear <- dat_linear %>% inner_join(dat_cov, by = "eid")
  model <- lm(acc ~ group + Age + Sex + Income + Employment + Education, dat_linear)
  # 提取结果
  beta <- round(model$coefficients["groupnew_var"], 3)
  beta_lower <- round(confint(model)["groupnew_var","2.5 %"], 3)
  beta_upper <- round(confint(model)["groupnew_var","97.5 %"], 3)
  beta_p <- summary(model)$coefficients["groupnew_var","Pr(>|t|)"]
  
  # 变量名
  var_name <- gsub("dat_|_yes_2_no|_no_2_yes", "", file)
  # var_name <- ifelse(grepl("_no", var_name), gsub("_no", "_yes", var_name),
  #                    gsub("_yes", "_no", var_name))
  # 修饰 p 值
  cohen_d_p <- ifelse(cohen_d_p < 0.001, "<0.001", as.character(round(cohen_d_p, 3)))
  beta_p <- ifelse(beta_p < 0.001, "<0.001", as.character(round(beta_p, 3)))
  
  # 添加结果
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

res_young <- res
res_young$var <- c("High systolic blood pressure (>=140 mmHg)",
                   "Obesity (BMI>=28)",
                   "Current smoker",
                   "Daily alcohol",
                   "Insufficient daily MVPA (<15 mins)",
                   "Diabetes history",
                   "Frequently processed meat intake (>=5 times weekly)",
                   "Frequently red meat intake (>=5 times weekly)",
                   "Frequently salt intake (usually or always)",
                   "Hypertension history")
names(res_young)[2] <- "var_name"
res_young$var_name <- factor(res_young$var_name,
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

res_young$beta_abs <- abs(res_young$beta)
res_young$beta_lower_abs <- abs(res_young$beta_upper)
res_young$beta_upper_abs <- abs(res_young$beta_lower)

p <- ggplot(res_young, aes(x = beta, y = var_name)) +
  geom_point(size = 1.5, color = "#3182bd") +
  geom_errorbar(aes(xmin = beta_lower, xmax = beta_upper), 
                width = 0.2, color = "#3182bd") +
  theme_minimal() +
  labs(title = "Perturbation analysis: elimination of risk factors",
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
p
ggsave("fig6-e1.pdf", p, width = 12, height = 5)


###### 2.流行病学证据：端粒与各因素的回归
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
  # 提取结果
  beta <- round(model$coefficients["group1"], 3)
  beta_lower <- round(confint(model)["group1","2.5 %"], 3)
  beta_upper <- round(confint(model)["group1","97.5 %"], 3)
  beta_p <- summary(model)$coefficients["group1","Pr(>|t|)"]
  
  beta_p <- ifelse(beta_p < 0.001, "<0.001", as.character(round(beta_p, 3)))
  
  # 添加结果
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

res_young_telomere <- res_telomere
res_young_telomere$var <- c("Current smoker",
                            "Daily alcohol",
                            "Frequently processed meat intake (>=5 times weekly)",
                            "Frequently red meat intake (>=5 times weekly)",
                            "Frequently salt intake (usually or always)",
                            "Insufficient daily MVPA (<15 mins)",
                            "Hypertension history",
                            "Diabetes history",
                            "Obesity (BMI>=28)",
                            "High systolic blood pressure (>=140 mmHg)")
names(res_young_telomere)[1] <- "var_name"

### wilcox test
res_young_sort <- dplyr::arrange(res_young, beta)
res_young_telomere_sort <- dplyr::arrange(res_young_telomere, beta)
res_young_sort$rank <- c(1:10)
res_young_telomere_sort$rank <- c(1:10)

res_young_sort <- dplyr::arrange(res_young_sort, var_name)
res_young_telomere_sort <- dplyr::arrange(res_young_telomere_sort, var_name)

wilcox_result <- wilcox.test(res_young_sort$rank, 
                             res_young_telomere_sort$rank, 
                             paired = TRUE)
print(wilcox_result)

### 作图
res_young_telomere$var_name <- factor(res_young_telomere$var_name,
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

p <- ggplot(res_young_telomere, aes(x = beta, y = var_name)) +
  geom_point(size = 1.5, color = "#de2d26") +
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
p
ggsave("fig6-e2.pdf", p, width = 12, height = 5)



############ 2.解释变量特征的重要性
# install.packages("iml")
library(iml)
library(MASS)
library(caret)

dat_age <- read_csv("Data/Models/llama3_70b_perturbation/llama3-70B-analyisis_newprompt.csv")
dat_age <- dplyr::select(dat_age, 1, 4)
dat_cov <- read_rds("Data/covariates_outcomes/panel_indicators_analysis.rds")
dat_age <- dat_age %>% inner_join(dat_cov, by = "eid")
dat_age$all_acc <- dat_age$`biological age` - dat_age$Age

dat_age <- dat_age %>%
  dplyr::mutate(`Current smoker` = case_when(
    `Current smoker` == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(`Daily alcohol intake` = case_when(
    `Daily alcohol intake` == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(`Daily healthy food` = case_when(
    `Daily healthy food` == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(`Frequently processed meat` = case_when(
    `Frequently processed meat` == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(`Frequently salt intake` = case_when(
    `Frequently salt intake` == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(`Family cardiovascular disease history` = case_when(
    `Family cardiovascular disease history` == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(`Family diabetes history` = case_when(
    `Family diabetes history` == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(`Hypertension history` = case_when(
    `Hypertension history` == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(`Diabetes history` = case_when(
    `Diabetes history` == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(`Hypotensive drugs` = case_when(
    `Hypotensive drugs` == 1 ~ "yes",
    TRUE ~ "no"
  )) %>%
  dplyr::mutate(Insulin = case_when(
    Insulin == 1 ~ "yes",
    TRUE ~ "no"
  ))

dat_age$`Current smoker` <- factor(dat_age$`Current smoker`)
dat_age$`Daily alcohol intake` <- factor(dat_age$`Daily alcohol intake`)
dat_age$`Daily healthy food` <- factor(dat_age$`Daily healthy food`)
dat_age$`Frequently processed meat` <- factor(dat_age$`Frequently processed meat`)
dat_age$`Frequently red meat` <- factor(dat_age$`Frequently red meat`)
dat_age$`Frequently salt intake` <- factor(dat_age$`Frequently salt intake`)
dat_age$`Family cardiovascular disease history` <- factor(dat_age$`Family cardiovascular disease history`)
dat_age$`Family diabetes history` <- factor(dat_age$`Family diabetes history`)
dat_age$`Hypertension history` <- factor(dat_age$`Hypertension history`)
dat_age$`Diabetes history` <- factor(dat_age$`Diabetes history`)
dat_age$`Hypotensive drugs` <- factor(dat_age$`Hypotensive drugs`)
dat_age$Insulin <- factor(dat_age$Insulin)

# 选择线性模型需要的变量
dat_age_shap <- dplyr::select(dat_age, 3:48)
old_names <- colnames(dat_age_shap)
old_names <- gsub(" \\(HbA1c\\)| \\(erythrocyte\\)| \\(leukocyte\\)", "", old_names)
old_names <- gsub(" |-", "_", old_names)
names(dat_age_shap) <- old_names

# 训练线性回归模型
model <- lm(all_acc ~ ., data = dat_age_shap)

# 创建预测函数
predict_function <- function(model, newdata) {
  predict(model, newdata)
}

# 创建预测对象
predictor <- Predictor$new(
  model = model, 
  data = dat_age_shap[ , -46],  # 移除目标变量
  y = dat_age_shap$all_acc, 
  predict.function = predict_function
)


### 1.个体水平上的特征重要性
# 计算 shap 值
shapley <- Shapley$new(predictor, x.interest = dat_age_shap[1, -46])
shap_values <- shapley$results

# 计算特征平均SHAP值的绝对值
shap_values_df <- data.frame(
  feature = shap_values$feature.value,
  shap_value = shap_values$phi
)

old_feature <- shap_values_df$feature
old_feature <- gsub("_", " ", old_feature)
old_feature <- gsub("=", " = ", old_feature)
shap_values_df$feature <- old_feature

# 使用 ggplot2 绘制条形图
p <- ggplot(shap_values_df, 
       aes(x = reorder(feature, shap_value), 
           y = shap_value, fill = shap_value)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "#3182bd", high = "#de2d26") +
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
p
ggsave("fig6-c1.pdf", p, width = 14, height = 12)


### 2.群体水平上的特征重要性
# 计算特征重要性
feature_imp <- FeatureImp$new(predictor, loss = "mae")
# 打印特征重要性
# print(feature_imp)
# 可视化特征重要性
# plot(feature_imp)

# 提取特征重要性数据框
feature_imp <- read_rds("Data/tempplot/feature_imp.rds")
feature_imp_df <- feature_imp$results

old_feature <- feature_imp_df$feature
old_feature <- gsub("_", " ", old_feature)
feature_imp_df$feature <- old_feature

# 使用 ggplot2 美化图表
p <- ggplot(feature_imp_df, aes(x = reorder(feature, importance), 
                           y = importance)) +
  # geom_bar(stat = "identity", fill = "#e6550d") +
  geom_point(size = 4, color = "#e6550d") + 
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
p
ggsave("fig6-d.pdf", p, width = 14, height = 12)

# write_rds(feature_imp, "feature_imp.rds")
# test <- read_rds("feature_imp.rds")



######################## ------ 七.构造删减特征的数据、扰动分析数据 ------
dat <- read_rds("Data/Backup_AnaData/Panel_indicators/panel_indicators.rds")
### 计算每列的缺失率
missing_rates <- colSums(is.na(dat)) / nrow(dat)
missing_rates_percent <- missing_rates * 100
missing_rates_percent <- data.frame(missing_rates_percent)

### 删除一些变量
dat$Income <- NULL
dat$Employment <- NULL
dat <- dat %>%
  mutate(Education = case_when(
    Education == "unknown" ~ "uneducated",
    TRUE ~ Education
  ))

# 重新处理 身高、体重、BMI等变量
dat <- dat %>%
  mutate(`Standing height` = case_when(
    !is.na(`Standing height`) ~ `Standing height`,
    is.na(`Standing height`) & !is.na(Weight) & !is.na(BMI) ~ round(sqrt(Weight / BMI) * 100, digits = 1),
    TRUE ~ `Standing height`
  ))
dat <- dat %>%
  mutate(Weight = case_when(
    !is.na(Weight) ~ Weight,
    is.na(Weight) & !is.na(`Standing height`) & !is.na(BMI) ~ round(BMI * ((`Standing height` / 100) ^ 2), digits = 1),
    TRUE ~ Weight
  ))
dat <- dat %>%
  mutate(BMI = case_when(
    !is.na(BMI) ~ BMI,
    is.na(BMI) & !is.na(`Standing height`) & !is.na(Weight) ~ round(Weight / ((`Standing height` / 100) ^ 2), digits = 1),
    TRUE ~ BMI
  ))
dat <- dplyr::select(dat, 1:15, 45:46, 16:44)
dat_nomiss <- na.omit(dat)

### 先保存并处理一份用于正常分析的数据
dat_nomiss_backup <- dat_nomiss

dat_nomiss_backup$Sex <- factor(dat_nomiss_backup$Sex, levels = c("female", "male"))
dat_nomiss_backup$Education <- factor(dat_nomiss_backup$Education, levels = c("uneducated", "below undergraduate", "college"))
dat_nomiss_backup$`Current smoker` <- factor(dat_nomiss_backup$`Current smoker`)
dat_nomiss_backup$`Daily alcohol intake` <- factor(dat_nomiss_backup$`Daily alcohol intake`)
dat_nomiss_backup$`Daily healthy food` <- factor(dat_nomiss_backup$`Daily healthy food`)
dat_nomiss_backup$`Frequently processed meat` <- factor(dat_nomiss_backup$`Frequently processed meat`)
dat_nomiss_backup$`Frequently red meat` <- factor(dat_nomiss_backup$`Frequently red meat`)
dat_nomiss_backup$`Frequently salt intake` <- factor(dat_nomiss_backup$`Frequently salt intake`)
dat_nomiss_backup$`Daily moderate to vigorous physical activity` <- round(dat_nomiss_backup$`Daily moderate to vigorous physical activity` / 60, digits = 1)
dat_nomiss_backup <- dat_nomiss_backup %>%
  mutate(`Daily moderate to vigorous physical activity` = case_when(
    `Daily moderate to vigorous physical activity` >= 8 ~ 8,
    TRUE ~ `Daily moderate to vigorous physical activity`
  ))
dat_nomiss_backup$`Family cardiovascular disease history` <- factor(dat_nomiss_backup$`Family cardiovascular disease history`)
dat_nomiss_backup$`Family diabetes history` <- factor(dat_nomiss_backup$`Family diabetes history`)
dat_nomiss_backup$`Hypertension history` <- factor(dat_nomiss_backup$`Hypertension history`)
dat_nomiss_backup$`Diabetes history` <- factor(dat_nomiss_backup$`Diabetes history`)
dat_nomiss_backup$`Hypotensive drugs` <- factor(dat_nomiss_backup$`Hypotensive drugs`)
dat_nomiss_backup$Insulin <- factor(dat_nomiss_backup$Insulin)

write_rds(dat_nomiss_backup, "panel_indicators_analysis.rds")


###### 扰动变量, 10个变量
dat_current_smoke_yes_2_no <- subset(dat_nomiss, `Current smoker` == 1)
dat_daily_alcohol_yes_2_no <- subset(dat_nomiss, `Daily alcohol intake` == 1)
dat_frequently_processed_meat_yes_2_no <- subset(dat_nomiss, `Frequently processed meat` == 1)
dat_frequently_red_meat_yes_2_no <- subset(dat_nomiss, `Frequently red meat` == 1)
dat_frequently_salt_yes_2_no <- subset(dat_nomiss, `Frequently salt intake` == 1)
dat_daily_pa_no_2_yes <- subset(dat_nomiss, `Daily moderate to vigorous physical activity` < 15)
dat_bmi_overweight_yes_2_no <- subset(dat_nomiss, BMI > 28)
dat_blood_pressure_yes_2_no <- subset(dat_nomiss, `Systolic blood pressure` > 140)
dat_hypertension_yes_2_no <- subset(dat_nomiss, `Hypertension history` == 1)
dat_diabetes_yes_2_no <- subset(dat_nomiss, `Diabetes history` == 1)


###### 处理成 LLM 能接受的文本数据
process_text <- function(dat_nomiss) {
  # age
  dat_nomiss <- dat_nomiss %>%
    mutate(Age = paste0(Age, " years"))
  
  # lifestyle var
  dat_nomiss$`Daily moderate to vigorous physical activity` <- round(dat_nomiss$`Daily moderate to vigorous physical activity` / 60, digits = 1)
  dat_nomiss <- dat_nomiss %>%
    mutate(`Daily moderate to vigorous physical activity` = case_when(
      `Daily moderate to vigorous physical activity` >= 8 ~ 8,
      TRUE ~ `Daily moderate to vigorous physical activity`
    ))
  dat_nomiss <- dat_nomiss %>%
    mutate(`Current smoker` = case_when(
      `Current smoker` == 1 ~ "yes",
      TRUE ~ "no"
    )) %>%
    mutate(`Daily alcohol intake` = case_when(
      `Daily alcohol intake` == 1 ~ "yes",
      TRUE ~ "no"
    )) %>%
    mutate(`Daily healthy food` = case_when(
      `Daily healthy food` == 1 ~ "yes",
      TRUE ~ "no"
    )) %>%
    mutate(`Frequently processed meat` = case_when(
      `Frequently processed meat` == 1 ~ "yes",
      TRUE ~ "no"
    )) %>%
    mutate(`Frequently red meat` = case_when(
      `Frequently red meat` == 1 ~ "yes",
      TRUE ~ "no"
    )) %>%
    mutate(`Frequently salt intake` = case_when(
      `Frequently salt intake` == 1 ~ "yes",
      TRUE ~ "no"
    )) %>%
    mutate(`Daily moderate to vigorous physical activity` = paste0(`Daily moderate to vigorous physical activity`, " hours"))
  
  # familiy history, medical history and medication history
  dat_nomiss <- dat_nomiss %>%
    mutate(`Family cardiovascular disease history` = case_when(
      `Family cardiovascular disease history` == 1 ~ "yes",
      TRUE ~ "no"
    )) %>%
    mutate(`Family diabetes history` = case_when(
      `Family diabetes history` == 1 ~ "yes",
      TRUE ~ "no"
    )) %>%
    mutate(`Hypertension history` = case_when(
      `Hypertension history` == 1 ~ "yes",
      TRUE ~ "no"
    )) %>%
    mutate(`Diabetes history` = case_when(
      `Diabetes history` == 1 ~ "yes",
      TRUE ~ "no"
    )) %>%
    mutate(`Hypotensive drugs` = case_when(
      `Hypotensive drugs` == 1 ~ "yes",
      TRUE ~ "no"
    )) %>%
    mutate(Insulin = case_when(
      Insulin == 1 ~ "yes",
      TRUE ~ "no"
    ))
  
  dat_nomiss <- dat_nomiss %>%
    mutate(`Standing height` = paste0("height ", `Standing height`, " cm")) %>%
    mutate(Weight = paste0("weight ", Weight, " kg")) %>%
    mutate(`Systolic blood pressure` = paste0("systolic blood pressure ", `Systolic blood pressure`, " mmHg")) %>%
    mutate(`Waist circumference` = paste0("waist circumference ", `Waist circumference`, " cm")) %>%
    mutate(BMI = paste0("BMI ", BMI)) %>%
    mutate(`Waist-hip ratio` = paste0("waist-hip ratio ", `Waist-hip ratio`))
  
  dat_nomiss <- dat_nomiss %>%
    mutate(`Haemoglobin concentration` = case_when(
      is.na(`Haemoglobin concentration`) ~ "",
      TRUE ~ paste0("haemoglobin concentration ", `Haemoglobin concentration`, " grams/decilitre")
    )) %>%
    mutate(`Mean corpuscular haemoglobin` = case_when(
      is.na(`Mean corpuscular haemoglobin`) ~ "",
      TRUE ~ paste0("mean corpuscular haemoglobin ", `Mean corpuscular haemoglobin`, " picograms")
    )) %>%
    mutate(`Mean corpuscular haemoglobin concentration` = case_when(
      is.na(`Mean corpuscular haemoglobin concentration`) ~ "",
      TRUE ~ paste0("mean corpuscular haemoglobin concentration ", `Mean corpuscular haemoglobin concentration`, " grams/decilitre")
    )) %>%
    mutate(`Mean corpuscular volume` = case_when(
      is.na(`Mean corpuscular volume`) ~ "",
      TRUE ~ paste0("mean corpuscular volume ", `Mean corpuscular volume`, " picograms")
    )) %>%
    mutate(`Red blood cell (erythrocyte) count` = case_when(
      is.na(`Red blood cell (erythrocyte) count`) ~ "",
      TRUE ~ paste0("red blood cell (erythrocyte) count ", `Red blood cell (erythrocyte) count`, "*10^12 cells/litre")
    )) %>%
    mutate(`White blood cell (leukocyte) count` = case_when(
      is.na(`White blood cell (leukocyte) count`) ~ "",
      TRUE ~ paste0("leukocyte count ", `White blood cell (leukocyte) count`, "*10^9 cells/litre")
    )) %>%
    mutate(`Platelet count` = case_when(
      is.na(`Platelet count`) ~ "",
      TRUE ~ paste0("platelet count ", `Platelet count`, "*10^9 cells/litre")
    )) %>%
    mutate(`Haematocrit percentage` = paste0("haematocrit percentage ", `Haematocrit percentage`, "%"))
  
  
  dat_nomiss <- dat_nomiss %>%
    mutate(`Alanine aminotransferase` = case_when(
      is.na(`Alanine aminotransferase`) ~ "",
      TRUE ~ paste0("alanine aminotransferase ", `Alanine aminotransferase`, " U/L")
    )) %>%
    mutate(`Aspartate aminotransferase` = case_when(
      is.na(`Aspartate aminotransferase`) ~ "",
      TRUE ~ paste0("aspartate aminotransferase ", `Aspartate aminotransferase`, " U/L")
    )) %>%
    mutate(`Alkaline phosphatase` = case_when(
      is.na(`Alkaline phosphatase`) ~ "",
      TRUE ~ paste0("alkaline phosphatase ", `Alkaline phosphatase`, " U/L")
    )) %>%
    mutate(Creatinine = case_when(
      is.na(Creatinine) ~ "",
      TRUE ~ paste0("creatinine ", Creatinine, " umol/L")
    )) %>%
    mutate(`Cystatin C` = case_when(
      is.na(`Cystatin C`) ~ "",
      TRUE ~ paste0("cystatin C ", `Cystatin C`, " mg/L")
    )) %>%
    mutate(Urea = case_when(
      is.na(Urea) ~ "",
      TRUE ~ paste0("urea ", Urea, " mmol/L")
    )) %>%
    mutate(Cholesterol = case_when(
      is.na(Cholesterol) ~ "",
      TRUE ~ paste0("cholesterol ", Cholesterol, " mmol/L")
    )) %>%
    mutate(`HDL cholesterol` = case_when(
      is.na(`HDL cholesterol`) ~ "",
      TRUE ~ paste0("HDL cholesterol ", `HDL cholesterol`, " mmol/L")
    )) %>%
    mutate(`LDL direct` = case_when(
      is.na(`LDL direct`) ~ "",
      TRUE ~ paste0("LDL direct cholesterol ", `LDL direct`, " mmol/L")
    )) %>%
    mutate(Triglycerides = case_when(
      is.na(Triglycerides) ~ "",
      TRUE ~ paste0("triglycerides ", Triglycerides, " mmol/L")
    )) %>%
    mutate(Glucose = case_when(
      is.na(Glucose) ~ "",
      TRUE ~ paste0("glucose ", Glucose, " mmol/L")
    )) %>%
    mutate(`Glycated haemoglobin (HbA1c)` = case_when(
      is.na(`Glycated haemoglobin (HbA1c)`) ~ "",
      TRUE ~ paste0("glycated haemoglobin (HbA1c) ", `Glycated haemoglobin (HbA1c)`, " mmol/mol")
    )) %>%
    mutate(Albumin = case_when(
      is.na(Albumin) ~ "",
      TRUE ~ paste0("albumin ", Albumin, " g/L")
    )) %>%
    mutate(`C-reactive protein` = case_when(
      is.na(`C-reactive protein`) ~ "",
      TRUE ~ paste0("C-reactive protein ", `C-reactive protein`, " mg/L")
    )) %>%
    mutate(Urate = case_when(
      is.na(Urate) ~ "",
      TRUE ~ paste0("urate ", Urate, " umol/L")
    ))
  
  dat_nomiss <- dat_nomiss %>%
    mutate(basic_info = paste0("Age: ", Age, "\nGender: ", Sex, "\nEducation: ", Education))
  
  dat_nomiss <- dat_nomiss %>%
    mutate(lifestyle = paste0("Current smoker: ", `Current smoker`, 
                              "\nDaily alcohol intake: ", `Daily alcohol intake`, 
                              "\nDaily healthy food intake: ", `Daily healthy food`, 
                              "\nFrequently processed meat intake: ", `Frequently processed meat`,
                              "\nFrequently red meat intake: ", `Frequently red meat`,
                              "\nFrequently salt intake: ", `Frequently salt intake`,
                              "\nDaily moderate to vigorous physical activity time: ", `Daily moderate to vigorous physical activity`))
  
  dat_nomiss <- dat_nomiss %>%
    mutate(family_his_medical_his = paste0("Family cardiovascular disease history: ", `Family cardiovascular disease history`,
                                           "\nFamily diabetes history: ", `Family diabetes history`,
                                           "\nHypertension history: ", `Hypertension history`,
                                           "\nDiabetes history: ", `Diabetes history`,
                                           "\nTake hypotensive drugs: ", `Hypotensive drugs`,
                                           "\nTake insulin: ", Insulin))
  
  dat_nomiss <- dat_nomiss %>%
    mutate(physical_examination = paste0(`Standing height`, ", ",
                                         Weight, ", ",
                                         BMI, ", ",
                                         `Waist circumference`, ", ",
                                         `Waist-hip ratio`, ", ",
                                         `Systolic blood pressure`))
  
  dat_nomiss <- dat_nomiss %>%
    mutate(blood_test = paste0(`Alanine aminotransferase`, ", ",
                               `Aspartate aminotransferase`, ", ",
                               `Alkaline phosphatase`, ", ",
                               Creatinine, ", ",
                               `Cystatin C`, ", ",
                               Urea, ", ",
                               Cholesterol, ", ",
                               `HDL cholesterol`, ", ",
                               `LDL direct`, ", ",
                               Triglycerides, ", ",
                               Glucose, ", ",
                               `Glycated haemoglobin (HbA1c)`, ", ",
                               Albumin, ", ",
                               `C-reactive protein`, ", ",
                               Urate, ", ",
                               `Haematocrit percentage`, ", ",
                               `Haemoglobin concentration`, ", ",
                               `Mean corpuscular haemoglobin`, ", ",
                               `Mean corpuscular haemoglobin concentration`, ", ",
                               `Mean corpuscular volume`, ", ",
                               `Red blood cell (erythrocyte) count`, ", ",
                               `White blood cell (leukocyte) count`, ", ",
                               `Platelet count`))
  
  dat_nomiss <- dplyr::select(dat_nomiss, 1, 47:51)
  
  dat_nomiss <- dat_nomiss %>%
    mutate(input = paste0("[Basic information]\n", basic_info, 
                          "\n[Lifestyle]\n", lifestyle, 
                          "\n[Family history and medical history]\n", family_his_medical_his,
                          "\n[Physical examination]\n", physical_examination,
                          "\n[Blood test]\n", blood_test))
  # cat(dat_nomiss$input[1])
  dat_nomiss <- dplyr::select(dat_nomiss, 1, 7)
  
  return(dat_nomiss)
}


### 进行扰动
dat_current_smoke_yes_2_no$`Current smoker` <- 0
dat_daily_alcohol_yes_2_no$`Daily alcohol intake` <- 0
dat_frequently_processed_meat_yes_2_no$`Frequently processed meat` <- 0
dat_frequently_red_meat_yes_2_no$`Frequently red meat` <- 0
dat_frequently_salt_yes_2_no$`Frequently salt intake` <- 0
dat_daily_pa_no_2_yes$`Daily moderate to vigorous physical activity` <- 15
dat_bmi_overweight_yes_2_no$BMI <- 27.9
dat_bmi_overweight_yes_2_no$Weight <- round(dat_bmi_overweight_yes_2_no$BMI * (dat_bmi_overweight_yes_2_no$`Standing height` / 100) * (dat_bmi_overweight_yes_2_no$`Standing height` / 100), 1)
dat_blood_pressure_yes_2_no$`Systolic blood pressure` <- 139
dat_hypertension_yes_2_no$`Hypertension history` <- 0
dat_diabetes_yes_2_no$`Diabetes history` <- 0


dat_current_smoke_yes_2_no <- process_text(dat_current_smoke_yes_2_no)
dat_daily_alcohol_yes_2_no <- process_text(dat_daily_alcohol_yes_2_no)
dat_frequently_processed_meat_yes_2_no <- process_text(dat_frequently_processed_meat_yes_2_no)
dat_frequently_red_meat_yes_2_no <- process_text(dat_frequently_red_meat_yes_2_no)
dat_frequently_salt_yes_2_no <- process_text(dat_frequently_salt_yes_2_no)
dat_daily_pa_no_2_yes <- process_text(dat_daily_pa_no_2_yes)
dat_bmi_overweight_yes_2_no <- process_text(dat_bmi_overweight_yes_2_no)
dat_blood_pressure_yes_2_no <- process_text(dat_blood_pressure_yes_2_no)
dat_hypertension_yes_2_no <- process_text(dat_hypertension_yes_2_no)
dat_diabetes_yes_2_no <- process_text(dat_diabetes_yes_2_no)

write_csv(dat_current_smoke_yes_2_no, "dat_current_smoke_yes_2_no.csv")
write_csv(dat_daily_alcohol_yes_2_no, "dat_daily_alcohol_yes_2_no.csv")
write_csv(dat_frequently_processed_meat_yes_2_no, "dat_frequently_processed_meat_yes_2_no.csv")
write_csv(dat_frequently_red_meat_yes_2_no, "dat_frequently_red_meat_yes_2_no.csv")
write_csv(dat_frequently_salt_yes_2_no, "dat_frequently_salt_yes_2_no.csv")
write_csv(dat_daily_pa_no_2_yes, "dat_daily_pa_no_2_yes.csv")
write_csv(dat_bmi_overweight_yes_2_no, "dat_bmi_overweight_yes_2_no.csv")
write_csv(dat_blood_pressure_yes_2_no, "dat_blood_pressure_yes_2_no.csv")
write_csv(dat_hypertension_yes_2_no, "dat_hypertension_yes_2_no.csv")
write_csv(dat_diabetes_yes_2_no, "dat_diabetes_yes_2_no.csv")






######################## ------  八.额外验证 ------
######### 1.CLHLS
###### 1.处理数据
# 44620
dat_clhls <- read_rds("Data/Models/OtherCohorts/CLHLS_aging_cohort.rds")
# 44620
dat_clhls_pred <- read_csv("Data/Models/OtherCohorts/llama3-70b_CLHLS_full.csv")
dat_clhls_pred <- dplyr::select(dat_clhls_pred, 1, 4)
# 44620
dat_clhls_pred <- dat_clhls_pred %>% inner_join(dat_clhls, by = "id")
dat_clhls_pred$all_acc <- dat_clhls_pred$`biological age` - dat_clhls_pred$Age
names(dat_clhls_pred)[2] <- "BA"
# 选择1998、2000、2002、2005、2008年进入队列的: 41419
dat_clhls_pred <- subset(dat_clhls_pred, entra_year != 2009 & 
                           entra_year != 2012 &
                           entra_year != 2014)
dat_clhls_pred_backup <- dat_clhls_pred
dat_clhls_pred <- dat_clhls_pred_backup

# 选择80岁以上的: 33173
dat_clhls_pred <- subset(dat_clhls_pred, Age >= 80)


###### 2.做年龄相关性密度图
dat_male <- subset(dat_clhls_pred, Sex=="male")
dat_female <- subset(dat_clhls_pred, Sex=="female")
cor_male <- cor(dat_male$Age, dat_male$BA, use = "complete")
cor_female <- cor(dat_female$Age, dat_female$BA, use = "complete")
cor_all <- cor(dat_clhls_pred$Age, dat_clhls_pred$BA, use = "complete")

p_density <- ggplot(dat_clhls_pred, aes(x = Age, y = BA)) +
  stat_density2d(aes(fill = after_stat(level)), geom = "polygon", 
                 h = c(10, 10)) +
  scale_fill_gradientn(colors = c("#3288bd", "#99d594", "#e6f598", "#ffffbf", "#fee08b", "#fc8d59", "#d53e4f")) +
  geom_smooth(method = "lm", se = TRUE, color = "#4d4d4d", linewidth = 0.5) +
  theme_minimal() +
  labs(
    x = "Chronological age (years)",
    y = "LLM biological age (years)",
    title = "The CLHLS Cohort (Age>=80)"
  ) +
  scale_x_continuous(breaks = seq(70, 130, by = 10), limits = c(68, 132)) +
  scale_y_continuous(breaks = seq(70, 130, by = 10), limits = c(68, 132)) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(size = 16, hjust = 0, vjust = 1, face = "italic"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14, color = "black"),
    legend.position = "none"
  ) + 
  annotate("text", x = 110, y = 75, label = expression(italic("r")~" = 0.76"), color = "#252525", size = 6, hjust = 0)

# p_density
ggsave("extend_clhls_age_cor.pdf", plot = p_density, width = 4, height = 4)


###### 3.进行分析 BA
# 31053
dat_clhls_pred <- subset(dat_clhls_pred, `All-cause death duration` > 0)
# 定义要跑的疾病
disease <- c("All-cause death", "Heart diseases", 
             "Cerebrovascular diseases", "Diabetes")
# 定义要跑的变量
var_ls <- c("Age", "BA")
# 接受结果
var_mean_c_index <- c()
var_mean_c_index_lower <- c()
var_mean_c_index_upper <- c()
outcome_ls <- c()

# 开始跑结果
set.seed(2024)
dat_age <- dat_clhls_pred
for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]
  
  # 选择符合要求的数据
  dat_cox <- subset(dat_age, time > 0)
  
  # 把数据分成五份
  folds <- createFolds(dat_cox$event, k = 5)
  for (i in 1:length(var_ls)) {
    var <- var_ls[i]
    # 初始化存储c-index的向量
    c_index_values <- c()
    c_index_lower_ls <- c()
    c_index_upper_ls <- c()
    
    # 五折交叉验证
    for(j in 1:5) {
      # 划分训练集和测试集
      test_indices <- folds[[j]]
      train_data <- dat_cox[-test_indices, ]
      test_data <- dat_cox[test_indices, ]
      
      # 构建Cox模型
      formula_covariates <- paste0("survobj ~ ", var)
      f <- as.formula(formula_covariates)
      survobj <- with(train_data, Surv(time, event))
      cox_fit <- coxph(formula = f, data = train_data, na.action = na.omit)
      
      # 预测风险评分
      test_data$predicted_risk <- predict(cox_fit, newdata = test_data, 
                                          type = "risk")
      
      # 计算c-index
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
    var_name == "BA" ~ "LLM biological age"
  ))

dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c("Chronological age", 
                                       "LLM biological age"))
### Plot
plots_c_index <- list()
disease <- c("All-cause death", "Heart diseases", 
             "Cerebrovascular diseases", "Diabetes")
for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(dat_plot, outcome == item)
  p <- ggplot(dat_sub, aes(x = var_name, y = c_index, color = var_name)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = c_index_lower, ymax = c_index_upper), width = 0.1) +
    geom_segment(aes(x = 0, xend = var_name, y = c_index, yend = c_index),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% c("Chronological age", 
                                                        "LLM biological age"))) +
    scale_color_manual(values = c("#fee08b", "#4480B3")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 16),
          axis.text.x = element_text(angle = 90, size = 14, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 14, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 18, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
  
  # if (i < 4) {
  #   p <- p + theme(axis.text.x = element_blank(),
  #                  axis.title.x = element_blank(),
  #                  axis.ticks.x = element_blank(),
  #                  axis.line.x = element_blank())
  # }
  plots_c_index[[i]] <- p
}


arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 4, 
                            nrow = 1)
# arranged_plots
# 添加纵坐标标题
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Absolute C-index", size = 16, rot = 90))

ggsave("extend_clhls_ba_cindex.pdf", plot = combined_plot, width = 16, height = 5)


###### 4.画KM曲线
dat_age <- dat_clhls_pred
# 31053
dat_age <- subset(dat_age, `All-cause death duration` > 0)
# 定义要跑的疾病
disease <- c("All-cause death", "Heart diseases", 
             "Cerebrovascular diseases", "Diabetes")
# 对 acc 进行排序
dat_age <- dat_age[order(dat_age$all_acc), ]
# 计算分组的边界
n <- nrow(dat_age)
top_10_boundary <- n * 0.9
median_10_boundary_low <- n * 0.45
median_10_boundary_high <- n * 0.55
# 分配分组标签
dat_age$group <- "Other"
dat_age$group[1:(n * 0.1)] <- "Bottom 10%"
dat_age$group[(top_10_boundary+1):n] <- "Top 10%"
dat_age$group[(median_10_boundary_low+1):median_10_boundary_high] <- "Median 10%"

plots <- list()

for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]
  
  # 选择符合要求的数据
  dat_cox <- subset(dat_age, `Heart diseases duration` > 0 &
                      `Cerebrovascular diseases duration` > 0 &
                      `Diabetes duration` > 0)
  dat_cox <- subset(dat_cox, group == "Bottom 10%" | group == "Top 10%" | group == "Median 10%")
  
  # 拟合生存曲线
  fit <- survfit(Surv(time, event) ~ group, data = dat_cox)
  
  # 绘制生存曲线
  ggsurv <- ggsurvplot(fit,
                       data = dat_cox,
                       # pval = TRUE, 
                       conf.int = FALSE,
                       # risk.table = TRUE,
                       fun = "event",
                       xlab = "",
                       ylab = "",
                       xlim = c(0, 20),
                       palette = c("#90D3C7", "#80B1D3", "#ca0020"),
                       legend.title = "Age-gap group",
                       legend.labs = c("Bottom 10%", "Median 10%", "Top 10%"),
                       legend = "bottom",
                       title = item,
                       ggtheme = theme_minimal())
  
  # 更改其他设置
  ggsurv$plot <- ggsurv$plot + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          # plot.margin = margin(10, 10, 10, 10),
          axis.line = element_line(),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14, color = "black"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0.5, vjust = 2)) +
    scale_x_continuous(breaks = c(5, 15)) + 
    scale_y_continuous(labels = function(x) x * 100)
  
  plots[[i]] <- ggsurv$plot
}

# 合并图形并保留一个图例和轴标题
combined_plot <- ggarrange(plotlist = plots, 
                           ncol = 4, 
                           nrow = 1,
                           common.legend = TRUE, 
                           legend = "right")

# 添加横纵坐标标题
combined_plot <- annotate_figure(combined_plot,
                                 bottom = text_grob("Time (years)", size = 16),
                                 left = text_grob("Cumulative event (%)", size = 16, rot = 90))

# print(combined_plot)
ggsave("extend_clhls_KM.pdf", plot = combined_plot, width = 16, height = 4)






######### 2.CHARLS
###### 1.预处理、合并数据
# 22141
dat_charls <- read_rds("Data/Models/OtherCohorts/CHARLS_aging_cohort.rds")
# 21117
dat_charls_pred <- read_csv("Data/Models/OtherCohorts/qwen2-72b_CHARLS_full.csv")
dat_charls_pred <- dplyr::select(dat_charls_pred, 1, 4)
names(dat_charls_pred) <- c("pid", "BA")
dat_charls_pred$pid <- as.character(dat_charls_pred$pid)
dat_charls_pred <- dat_charls_pred %>% inner_join(dat_charls, by = "pid")
dat_charls_pred$BA <- as.integer(dat_charls_pred$BA)
# 22141-21117-20375
dat_charls_pred <- na.omit(dat_charls_pred)
dat_charls_pred$all_acc <- dat_charls_pred$BA - dat_charls_pred$Age
# 选40-70岁的人群: 17870
dat_charls_pred <- subset(dat_charls_pred, Age >= 40 & Age <= 70)

###### 2.做年龄相关性密度图
dat_male <- subset(dat_charls_pred, Sex=="male")
dat_female <- subset(dat_charls_pred, Sex=="female")
cor_male <- cor(dat_male$Age, dat_male$BA, use = "complete")
cor_female <- cor(dat_female$Age, dat_female$BA, use = "complete")

p_density <- ggplot(dat_female, aes(x = Age, y = BA)) +
  stat_density2d(aes(fill = after_stat(level)), geom = "polygon", 
                 h = c(10, 10)) +
  scale_fill_gradientn(colors = c("#3288bd", "#99d594", "#e6f598", "#ffffbf", "#fee08b", "#fc8d59", "#d53e4f")) +
  geom_smooth(method = "lm", se = TRUE, color = "#4d4d4d", linewidth = 0.5) +
  theme_minimal() +
  labs(
    x = "Chronological age (years)",
    y = "LLM biological age (years)",
    title = "Female"
  ) +
  scale_x_continuous(breaks = seq(35, 85, by = 10), limits = c(35, 85)) +
  scale_y_continuous(breaks = seq(35, 85, by = 10), limits = c(35, 85)) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(size = 18, hjust = 0, vjust = 1, face = "italic"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14, color = "black"),
    legend.position = "none"
  ) + 
  annotate("text", x = 67, y = 40, label = expression(italic("r")~" = 0.76"), color = "#252525", size = 6, hjust = 0)

# p_density
ggsave("extend_charls_female_age_cor.pdf", plot = p_density, width = 4, height = 4)


###### 3.进行分析 BA
# 定义要跑的疾病
disease <- c("Heart diseases", "Stroke", "COPD",
             "Liver diseases", "Renal diseases", "Diabetes")
# 定义要跑的变量
var_ls <- c("Age", "BA")
# 接受结果
var_mean_c_index <- c()
var_mean_c_index_lower <- c()
var_mean_c_index_upper <- c()
outcome_ls <- c()

# 开始跑结果
set.seed(2024)
dat_age <- dat_charls_pred
for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]
  
  # 选择符合要求的数据
  dat_cox <- subset(dat_age, time > 0)
  
  # 把数据分成五份
  folds <- createFolds(dat_cox$event, k = 5)
  for (i in 1:length(var_ls)) {
    var <- var_ls[i]
    # 初始化存储c-index的向量
    c_index_values <- c()
    c_index_lower_ls <- c()
    c_index_upper_ls <- c()
    
    # 五折交叉验证
    for(j in 1:5) {
      # 划分训练集和测试集
      test_indices <- folds[[j]]
      train_data <- dat_cox[-test_indices, ]
      test_data <- dat_cox[test_indices, ]
      
      # 构建Cox模型
      formula_covariates <- paste0("survobj ~ ", var)
      f <- as.formula(formula_covariates)
      survobj <- with(train_data, Surv(time, event))
      cox_fit <- coxph(formula = f, data = train_data, na.action = na.omit)
      
      # 预测风险评分
      test_data$predicted_risk <- predict(cox_fit, newdata = test_data, 
                                          type = "risk")
      
      # 计算c-index
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
    var_name == "BA" ~ "LLM biological age"
  ))

dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c("Chronological age", 
                                       "LLM biological age"))
### Plot
plots_c_index <- list()
disease <- c("Heart diseases", "Stroke", "COPD",
             "Liver diseases", "Renal diseases", "Diabetes")
for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(dat_plot, outcome == item)
  p <- ggplot(dat_sub, aes(x = var_name, y = c_index, color = var_name)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = c_index_lower, ymax = c_index_upper), width = 0.1) +
    geom_segment(aes(x = 0, xend = var_name, y = c_index, yend = c_index),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% c("Chronological age", 
                                                        "LLM biological age"))) +
    scale_color_manual(values = c("#fee08b", "#4480B3")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 20),
          axis.text.x = element_text(angle = 90, size = 20, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 20, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
  
  # if (i < 4) {
  #   p <- p + theme(axis.text.x = element_blank(),
  #                  axis.title.x = element_blank(),
  #                  axis.ticks.x = element_blank(),
  #                  axis.line.x = element_blank())
  # }
  plots_c_index[[i]] <- p
}

plots_c_index[5]

arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 3, 
                            nrow = 2,
                            heights = c(1, 1.5))
arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 6, 
                            nrow = 1)
# arranged_plots
# 添加纵坐标标题
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Absolute C-index", size = 22, rot = 90))

ggsave("extend_charls_ba_cindex.pdf", plot = combined_plot, width = 12, height = 8)
ggsave("Fig4-f-charls-cindex.pdf", plot = combined_plot, width = 24, height = 5.5)


###### 4.画各疾病的HR误差线（横向）
Cox_analysis <- function(dat_baseline, disease_ls, var_ls) {
  ### 感兴趣结果整合到列表
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
    
    # 选择符合要求的数据
    dat_cox <- subset(dat_baseline, time > 0)
    
    for (i in 1:length(var_ls)) {
      var_name <- var_ls[i]
      # formula_covariates <- paste0("survobj ~ ", var_name)
      formula_covariates <- paste0("survobj ~ Age + Sex + ", var_name)
      f <- as.formula(formula_covariates)
      survobj <- with(dat_cox, Surv(time, event))
      
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

# 定义要跑的疾病
disease <- c("Heart diseases", "Stroke", "COPD",
             "Liver diseases", "Renal diseases", "Diabetes")
# 定义要跑的变量
var_ls <- c("all_acc")

age_results_hr <- Cox_analysis(dat_baseline = dat_age,
                               disease_ls = disease,
                               var_ls = var_ls)

p <- ggplot(age_results_hr, aes(x = disease, y = HR)) +
  geom_point(size = 3, color = "#4480B3") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "#4480B3") +
  theme_minimal() +
  labs(title = "",
       y = "Adjusted hazard ratio (HR) with per age gap",
       x = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(angle = 90, size = 20, color = "black", 
                                   hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "none",
        plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  coord_flip()

ggsave("Fig4-f-charls-hr.pdf", plot = p, width = 8, height = 5)


###### 5.画KM曲线
dat_age <- dat_charls_pred
# 定义要跑的疾病
disease <- c("Heart diseases", "Stroke", "COPD",
             "Liver diseases", "Renal diseases", "Diabetes")
# 对 acc 进行排序
dat_age <- dat_age[order(dat_age$all_acc), ]
# 计算分组的边界
n <- nrow(dat_age)
top_10_boundary <- n * 0.9
median_10_boundary_low <- n * 0.45
median_10_boundary_high <- n * 0.55
# 分配分组标签
dat_age$group <- "Other"
dat_age$group[1:(n * 0.1)] <- "Bottom 10%"
dat_age$group[(top_10_boundary+1):n] <- "Top 10%"
dat_age$group[(median_10_boundary_low+1):median_10_boundary_high] <- "Median 10%"

plots <- list()

for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]
  
  # 选择符合要求的数据
  dat_cox <- subset(dat_age, time > 0)
  dat_cox <- subset(dat_cox, group == "Bottom 10%" | group == "Top 10%" | group == "Median 10%")
  
  # 拟合生存曲线
  fit <- survfit(Surv(time, event) ~ group, data = dat_cox)
  
  # 绘制生存曲线
  ggsurv <- ggsurvplot(fit,
                       data = dat_cox,
                       # pval = TRUE, 
                       conf.int = FALSE,
                       # risk.table = TRUE,
                       fun = "event",
                       xlab = "",
                       ylab = "",
                       xlim = c(0, 20),
                       palette = c("#90D3C7", "#80B1D3", "#ca0020"),
                       legend.title = "Age-gap group",
                       legend.labs = c("Bottom 10%", "Median 10%", "Top 10%"),
                       legend = "bottom",
                       title = item,
                       ggtheme = theme_minimal())
  
  # 更改其他设置
  ggsurv$plot <- ggsurv$plot + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          # plot.margin = margin(10, 10, 10, 10),
          axis.line = element_line(),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16, color = "black"),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16),
          plot.title = element_text(size = 20, hjust = 0.5, vjust = 2)) +
    scale_x_continuous(breaks = c(5, 15)) + 
    scale_y_continuous(labels = function(x) x * 100)
  
  plots[[i]] <- ggsurv$plot
}

# 合并图形并保留一个图例和轴标题
combined_plot <- ggarrange(plotlist = plots, 
                           ncol = 6, 
                           nrow = 1,
                           common.legend = TRUE, 
                           legend = "right")

# 添加横纵坐标标题
combined_plot <- annotate_figure(combined_plot,
                                 bottom = text_grob("Time (years)", size = 18),
                                 left = text_grob("Cumulative event (%)", size = 18, rot = 90))

# print(combined_plot)
ggsave("Fig-4_charls_KM.pdf", plot = combined_plot, width = 24, height = 4)




######### 3.CFPS
###### 1.预处理数据
# 48336
dat_CFPS <- read_rds("Data/Models/OtherCohorts/CFPS_aging_cohort.rds")
# 38733
dat_CFPS_pred <- read_csv("Data/Models/OtherCohorts/qwen2-72b_CFPS_full.csv")
dat_CFPS_pred <- dplyr::select(dat_CFPS_pred, 1, 3)
names(dat_CFPS_pred) <- c("pid", "BA")
dat_CFPS_pred$pid <- as.character(dat_CFPS_pred$pid)
dat_CFPS <- dat_CFPS %>% 
  dplyr::arrange(pid, entrayear) %>% 
  distinct(pid, .keep_all = TRUE)
# 33975
dat_CFPS_pred <- dat_CFPS_pred %>% inner_join(dat_CFPS, by = "pid")
dat_CFPS_pred$BA <- as.integer(dat_CFPS_pred$BA)
# 33971
dat_CFPS_pred <- na.omit(dat_CFPS_pred)
dat_CFPS_pred$all_acc <- dat_CFPS_pred$BA - dat_CFPS_pred$Age
### 分析
# 选择 Age 在 40-70之间的: 19242
dat_CFPS_pred <- subset(dat_CFPS_pred, Age >= 40 & Age <= 70)


###### 2.做年龄相关性密度图
dat_male <- subset(dat_CFPS_pred, Sex=="male")
dat_female <- subset(dat_CFPS_pred, Sex=="female")
cor_male <- cor(dat_male$Age, dat_male$BA, use = "complete")
cor_female <- cor(dat_female$Age, dat_female$BA, use = "complete")

p_density <- ggplot(dat_male, aes(x = Age, y = BA)) +
  stat_density2d(aes(fill = after_stat(level)), geom = "polygon", 
                 h = c(10, 10)) +
  scale_fill_gradientn(colors = c("#3288bd", "#99d594", "#e6f598", "#ffffbf", "#fee08b", "#fc8d59", "#d53e4f")) +
  geom_smooth(method = "lm", se = TRUE, color = "#4d4d4d", linewidth = 0.5) +
  theme_minimal() +
  labs(
    x = "Chronological age (years)",
    y = "LLM biological age (years)",
    title = "Male"
  ) +
  scale_x_continuous(breaks = seq(35, 85, by = 10), limits = c(35, 85)) +
  scale_y_continuous(breaks = seq(35, 85, by = 10), limits = c(35, 85)) +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(size = 18, hjust = 0, vjust = 1, face = "italic"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14, color = "black"),
    legend.position = "none"
  ) + 
  annotate("text", x = 67, y = 40, label = expression(italic("r")~" = 0.95"), color = "#252525", size = 6, hjust = 0)

# p_density
ggsave("extend_cfps_male_age_cor.pdf", plot = p_density, width = 4, height = 4)


###### 3.进行分析 BA
# 定义要跑的疾病
disease <- c("All-cause death", "CHD", "Stroke",
             "Liver diseases", "Renal diseases", "Diabetes")
# 定义要跑的变量
var_ls <- c("Age", "BA")
# 接受结果
var_mean_c_index <- c()
var_mean_c_index_lower <- c()
var_mean_c_index_upper <- c()
outcome_ls <- c()

# 开始跑结果
set.seed(2024)
dat_age <- dat_CFPS_pred
for(i in 1:length(disease)) {
  item <- disease[i]
  item_diagnose <- paste0(item, " diagnose")
  item_duration <- paste0(item, " duration")
  dat_age$event <- dat_age[[item_diagnose]]
  dat_age$time <- dat_age[[item_duration]]
  
  # 选择符合要求的数据
  dat_cox <- subset(dat_age, time > 0)
  
  # 把数据分成五份
  folds <- createFolds(dat_cox$event, k = 5)
  for (i in 1:length(var_ls)) {
    var <- var_ls[i]
    # 初始化存储c-index的向量
    c_index_values <- c()
    c_index_lower_ls <- c()
    c_index_upper_ls <- c()
    
    # 五折交叉验证
    for(j in 1:5) {
      # 划分训练集和测试集
      test_indices <- folds[[j]]
      train_data <- dat_cox[-test_indices, ]
      test_data <- dat_cox[test_indices, ]
      
      # 构建Cox模型
      formula_covariates <- paste0("survobj ~ ", var)
      f <- as.formula(formula_covariates)
      survobj <- with(train_data, Surv(time, event))
      cox_fit <- coxph(formula = f, data = train_data, na.action = na.omit)
      
      # 预测风险评分
      test_data$predicted_risk <- predict(cox_fit, newdata = test_data, 
                                          type = "risk")
      
      # 计算c-index
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
    var_name == "BA" ~ "LLM biological age"
  ))

dat_plot$var_name <- factor(dat_plot$var_name, 
                            levels = c("Chronological age", 
                                       "LLM biological age"))
### Plot
plots_c_index <- list()
disease <- c("All-cause death", "CHD", "Stroke",
             "Liver diseases", "Renal diseases", "Diabetes")
for(i in 1:length(disease)) {
  item <- disease[i]
  dat_sub <- subset(dat_plot, outcome == item)
  p <- ggplot(dat_sub, aes(x = var_name, y = c_index, color = var_name)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = c_index_lower, ymax = c_index_upper), width = 0.1) +
    geom_segment(aes(x = 0, xend = var_name, y = c_index, yend = c_index),
                 linetype = "dashed",
                 data = subset(dat_sub, var_name %in% c("Chronological age", 
                                                        "LLM biological age"))) +
    scale_color_manual(values = c("#fee08b", "#4480B3")) +
    theme_minimal() +
    labs(title = item,
         y = "",
         x = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(),
          axis.title = element_text(size = 20),
          axis.text.x = element_text(angle = 90, size = 20, color = "black", hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 20, color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "none",
          plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)) +
    scale_y_continuous(labels = number_format(accuracy = 0.01))
  
  # if (i < 4) {
  #   p <- p + theme(axis.text.x = element_blank(),
  #                  axis.title.x = element_blank(),
  #                  axis.ticks.x = element_blank(),
  #                  axis.line.x = element_blank())
  # }
  plots_c_index[[i]] <- p
}

plots_c_index[5]

arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 3, 
                            nrow = 2,
                            heights = c(1, 1.5))
arranged_plots <- ggarrange(plotlist = plots_c_index, 
                            ncol = 6, 
                            nrow = 1)
# arranged_plots
# 添加纵坐标标题
combined_plot <- annotate_figure(arranged_plots,
                                 left = text_grob("Absolute C-index", size = 22, rot = 90))

ggsave("Fig4-c-cfps-cindex.pdf", plot = combined_plot, width = 24, height = 5.5)


###### 4.画HR图
# 定义要跑的疾病
disease <- c("All-cause death", "CHD", "Stroke",
             "Liver diseases", "Renal diseases", "Diabetes")
# 定义要跑的变量
var_ls <- c("all_acc")
dat_age <- dat_CFPS_pred
age_results_hr <- Cox_analysis(dat_baseline = dat_age,
                               disease_ls = disease,
                               var_ls = var_ls)

p <- ggplot(age_results_hr, aes(x = disease, y = HR)) +
  geom_point(size = 3, color = "#4480B3") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "#4480B3") +
  theme_minimal() +
  labs(title = "",
       y = "Adjusted hazard ratio (HR) with per age gap",
       x = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(angle = 90, size = 20, color = "black", 
                                   hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "none",
        plot.title = element_text(size = 24, hjust = 0.5, vjust = 2)) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  coord_flip()

ggsave("Fig4-g-cfps-hr.pdf", plot = p, width = 8, height = 5)






