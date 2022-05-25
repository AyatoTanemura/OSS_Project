# How the total number of problems/bugs raised or requests for new features

#Library ----
library(tidyverse)
library(dplyr)
library(lattice)
library(ggplot2)
library(gplots)
library(nparLD)
library(tidyverse)
library(nlme)

# Data Load----
df <- read.csv("Data/prjDetPanel-Jan2011.csv", header = TRUE) %>% 
  mutate(prjId = factor(prjId)
         , Time_1 = Time -1)

names(df)

col.order <- c("X.",              "prjId",           "Period",          "Time",    "Time_1",        "StartDate",       "EndDate",        
                "forks"       ,    "members"     ,    "commits"   ,      "issues"  ,        "watchers" ,       "pullReq",        
                "CmtCmnt"  ,       "pullReqCmnt"   ,  "PR.Issue.Cmnt" ,  "issueCmnt" ,      "committers"   ,   "MemCommitters",  
                "PRClosedCnt"  ,   "IssueClosedCnt"  ,"PRClosedTime"  ,  "IssueClosedTime" ,"Health" ,         "Licence",        
                "ContribFile"  ,   "OwnerFollower" ,  "AvgFollower"   ,  "OwnerType" ) 

df <- df[, col.order]


# Data Check----
dim(df)
str(df)
head(df)
view(df)

# First five project

df5 <- df[1:40,]

View(df5)

# Sampling

sample <- sample(df$prjId, size = 5)

df.sam <- df %>% filter(prjId %in% sample)

head(df.sam)
dim(df.sam)
view(df.sam)

# Data Viz----
names(df)

xyplot(issues ~ Time_1 | prjId, data = df5,
       panel = function(x,y){
         panel.xyplot(x, y)
         panel.lmline(x, y)
       }, as.table = T)

# unconditional means model

a.model <- lme(issues ~ 1, df, random = ~1 |prjId)
summary(a.model)

a.varcorr <- VarCorr(a.model)

a.ICC<-as.numeric(a.varcorr[1,1])/(as.numeric(a.varcorr[2,1])+as.numeric(a.varcorr[1,1]))

# Unconditional growth model

b.model <- lme(issues ~ Time_1, data = df, random = ~ Time_1 | prjId, method = "ML")

summary(b.model)

View(df)












