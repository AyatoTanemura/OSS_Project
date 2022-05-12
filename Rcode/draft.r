#Library ----
library(tidyverse)
library(ggplot2)

# Data Load----
df <- read.csv("Data/prjDetPanel-Jan2011.csv", header = TRUE) %>% 
  mutate(prjId = factor(prjId))

# Data Check----
dim(df)
str(df)
head(df)
view(df)

# Sampling

sample <- sample(df$prjId, size = 5)

df.sam <- df %>% filter(prjId %in% sample)

head(df.sam)
dim(df.sam)
view(df.sam)

# Data Viz----
?ggplot2

a <- ggplot(df, aes())





















