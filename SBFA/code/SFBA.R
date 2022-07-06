########## 安装和载入需要的包
# install.packages(c("tidyverse", "BayesFactor", "here"))
library(BayesFactor)
library(tidyverse)
# library(here)
########### 导入数据
df <- read_csv("/Users/zhengyuanrui/Desktop/SBFA/data/df.js.sum_jasp.csv")


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
  select(subj_idx:Mismatch_Good) %>%
  pivot_longer(
    cols = Match_Bad:Mismatch_Good, names_to = c("Matchness", "Valence"),
    names_sep = "_", values_to = "rt"
  )

########## 数据的基本信息
subj_num <- unique(df_anova$subj_idx) # 每个被试的编号
n <- length(unique(df_anova$subj_idx)) # 一共有20个被试


BFs_match <- rep(NA, length(subj_num))

BFs_valence <- rep(NA, length(subj_num))

BFs_int <- rep(NA, length(subj_num))



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
  bayesfactors <- bf <- anovaBF(rt ~ Valence * Matchness + subj_idx,
    data = data.frame(df.selected),
    whichRandom = "subj_idx"
  )
  BFs_match[i] <- bayesfactors[1]
  BFs_valence[i] <- bayesfactors[2]
  BFs_int[i] <- bayesfactors[4] / bayesfactors[3]
}

BFs_match

BFs_valence

BFs_int
