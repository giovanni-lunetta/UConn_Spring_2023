library("readxl")
library(tidyverse)
df = read_excel("/Users/giovanni-lunetta/POLS2073Q-Spring-2023/height_data_spr_2022.xlsx")

# 1. mean, median, and standard deviation of mother's height.
mean(df$`mother inches`, na.rm=TRUE)
median(df$`mother inches`, na.rm=TRUE)
sd(df$`mother inches`, na.rm=TRUE)

sapply(df$`mother inches`, na.rm=TRUE)

# 2. mean, median, and standard deviation of student's height.
mean(df$`student inches`, na.rm=TRUE)
median(df$`student inches`, na.rm=TRUE)
sd(df$`student inches`, na.rm=TRUE)

# 3. a scatter plot with mother's height on the x axis and student height on the Y axis; 
# AND on this graph you should have the regression line… your line should look like this
x <- df$`mother inches`
y <- df$`student inches`
df %>% 
  ggplot(aes(x, y))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title="Student's Height vs Mother's Height", y="Student Inches", x="Mother inches")