########## 安装和载入需要的包
# install.packages(c("tidyverse", "BayesFactor", "here"))
library(BayesFactor)#计算T检验和方差分析的贝叶斯因子
library(tidyverse)
library(here)
here()
# library(here)
########### 导入数据
df <- read_csv(here("2_Data/df.js.sum_jasp.csv"))

options(scipen = 9)
########## 数据的基本信息
subj_num <- unique(df$subj_idx) # 每个被试的编号
n <- length(unique(df$subj_idx)) # 一共有20个被试

# T检验（配对样本T检验）
###### 筛选数据：good_match condition compare to bad_match condition



bf_output <- rep(NA, length(subj_num)) ### 先建立一个列表


for (i in seq_along(subj_num)) {
  if (i == 1) {
    next
  }
  df$subj_idx <- as.character(df$subj_idx)
  id <- unique(df$subj_idx)[1:i]
  df.selected <- df %>% filter(subj_idx %in% id)
  df.selected$subj_idx <- as.factor(df.selected$subj_idx)
  bayesfactors <- ttestBF(
    x = df.selected$Match_Bad, y = df.selected$Match_Good,
    paired = TRUE
  )
  bf_output[i] <- bayesfactors[1]
}

bf_output


########## （重复测量）方差分析

df_anova <- df %>%
  select(subj_idx:Mismatch_Neutral) %>%
  pivot_longer(
    cols = Match_Bad:Mismatch_Neutral, names_to = c("Matchness", "Valence"),
    names_sep = "_", values_to = "rt"
  )

########## 数据的基本信息
subj_num <- unique(df_anova$subj_idx) # 每个被试的编号
n <- length(unique(df_anova$subj_idx)) # 一共有20个被试


BFs_match <- rep(NA, length(subj_num))

BFs_valence <- rep(NA, length(subj_num))

BFs_int <- rep(NA, length(subj_num))


options(scipen = 9)

for (i in seq_along(subj_num)) {
  if (i == 1) {
    next
  }
  df_anova$subj_idx <- as.character(df_anova$subj_idx)
  id <- unique(df_anova$subj_idx)[1:i]
  df.selected <- df_anova %>% filter(subj_idx %in% id)
  df.selected$subj_idx <- as.factor(df.selected$subj_idx)
  df.selected$Matchness <- as.factor(df.selected$Matchness)
  df.selected$Valence <- as.factor(df.selected$Valence)
  bayesfactors <- generalTestBF(rt ~ Valence*Matchness*subj_idx - subj_idx:Valence:Matchness,
                                       data = data.frame(df.selected), whichRandom = "subj_idx",
                                       neverExclude = "subj_idx", whichModels = "all", progress = FALSE)
  BFs_match[i] <- bayesfactors[4]/bayesfactors[1]
  BFs_valence[i] <- bayesfactors[4]/bayesfactors[2]
  BFs_int[i] <- bayesfactors[7] / bayesfactors[4]
}

output <- tibble(BFs_int, BFs_valence, BFs_match)
output$BFs_int <- round(output$BFs_int, digits = 2)
output$BFs_valence <- round(output$BFs_valence, digits = 2)
output$BFs_match <- round(output$BFs_match, digits = 2)

head(output)

output %>% pivot_longer(BFs_int:BFs_match, names_to = "Effect", values_to = "Bayes Factor")


