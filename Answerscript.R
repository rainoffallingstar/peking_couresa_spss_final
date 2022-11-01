library(tidyverse)
library(readxl)
data <- read_excel("/data/data.xlsx", col_types = c("numeric", 
                                              "numeric", "text", "text", "text", 
                                              "numeric"))
View(data)
str(data)
race_level <- c("白人", "黑人", "其他")
smoke_level <- c("不吸烟", "吸烟")
ht_level <- c("无高血压", "有高血压")
# 清理数据并统计总出生体重
#data$RACE = factor(data$RACE, levels = c("1", "2", "3") , labels = race_level )
#data$SMOKE <- factor(data$SMOKE, levels =c("0", "1"),labels = smoke_level)
#data$HT <- factor(data$HT, levels =c("0","1"), labels = ht_level)
str(data)
total <- nrow(data)

# 总的出生体重的统计
attach(data)
mean(BWT)
sd(BWT)

# 低出生组
low_bw <- data %>% 
  filter(BWT < 2500) %>%
  summarise(mean(BWT), sd(BWT))
low_bw 
# 高出生组

high_bw <- data %>% 
  filter(BWT >= 2500) %>%
  summarise(mean(BWT), sd(BWT))
high_bw 

# 母亲人种比例函数选取

sortdata <- function(x,y) {
  race_num <- data %>% 
    filter(x == y)
}
per <- rep(NA,3)
for (i in 1:3) {
  per[i] <- 100 * nrow(sortdata(RACE, i))/ total
  #colnames(per) <- race_level
}
print(per)

# 比较高血压分组的妇女

ht_group <- as.numeric(unlist(sortdata(HT, 1)))
none_ht <- as.numeric(unlist(sortdata(HT, 0)))
# 首先进行F方差齐性检验
#library(car)
#bartlett.test(ht_group, none_ht)
var.test(ht_group, none_ht)
#HT <- as.factor(data$HT)
#leveneTest(HT, x = data)
# 进行独立样本t检验
t.test(ht_group, none_ht, paired = FALSE)

# 不同种族妇女的婴儿出生体重是否有差别
# 单因素方差分析
data$RACE = factor(data$RACE, levels = c("1", "2", "3") , labels = race_level )
res.aov <- aov(BWT ~ RACE, data = data)
summary(res.aov)

# SNK多重比较检验 
library(agricolae)
aov_snk <- SNK.test(res.aov, "RACE",group=FALSE)
aov_snk$comparison


# 种族、 高血压与卡方检验

weight = rep(NA, total)

for (i in 1:total){
  if (BWT[i] >= 2500) {
    weight[i] <- 1
  } else {
    weight[i] <- 0
  }
}
weight <- factor(weight, levels = c("0", "1"), labels = c("low", "high"))
ka <- data.frame(data,weight)

ka_race <- xtabs(~ RACE + weight, data = ka) %>% 
  chisq.test()
str(ka)  

# 高血压
data$HT <- factor(data$HT, levels =c("0","1"), labels = ht_level)
ka_ht <- xtabs(~ HT + weight, data = ka) %>% 
  chisq.test()
ka_ht


# 年龄与出生体重线性相关

fdata <- data %>% 
  select(AGE, BWT)
str(fdata)
cor(AGE, BWT)
cor.test(AGE,BWT)

# 回归方程
result=lm(BWT~AGE,data=fdata)
result
summary(result)

# 回归方程显著性检验
library(gvlma)
gvmodel <- gvlma(result)
summary(gvmodel)
