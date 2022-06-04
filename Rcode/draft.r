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
?read.csv
df <- read.csv("Data/prjDetPanel-Jan2011.csv", header = TRUE) %>% 
  mutate(prjId = factor(prjId)
         , Time_1 = Time -1
         #, Licence = factor(Licence)
         #, ContribFile = factor(ContribFile)
         ) %>% 
  dplyr::select(Time, Time_1, everything()) 

#col.order <- c("X.",              "prjId",           "Period",          "Time",    "Time_1",        "StartDate",       "EndDate",        
#                "forks"       ,    "members"     ,    "commits"   ,      "issues"  ,        "watchers" ,       "pullReq",        
#                "CmtCmnt"  ,       "pullReqCmnt"   ,  "PR.Issue.Cmnt" ,  "issueCmnt" ,      "committers"   ,   "MemCommitters",  
#                "PRClosedCnt"  ,   "IssueClosedCnt"  ,"PRClosedTime"  ,  "IssueClosedTime" ,"Health" ,         "Licence",        
#                "ContribFile"  ,   "OwnerFollower" ,  "AvgFollower"   ,  "OwnerType" ) 
#
#df <- df[, col.order]


# Data Check----
dim(df)
str(df)
head(df)
view(df)

# First five project
df5 <- df %>% filter(prjId %in% unique(df$prjId)[1:5])
#df5 <- df[1:40,]
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
names(df)

# Unconditional growth model with all predictors

c.model <- lme(issues ~ forks*Time_1 + members*Time_1 + commits*Time_1 + watchers*Time_1 + pullReq*Time_1 
               + CmtCmnt*Time_1 + pullReqCmnt*Time_1 + PR.Issue.Cmnt*Time_1 + issueCmnt*Time_1 
               + committers*Time_1 + MemCommitters*Time_1 + PRClosedCnt*Time_1 + IssueClosedCnt*Time_1 
               + PRClosedTime*Time_1 + IssueClosedTime*Time_1 + Health*Time_1 + Licence*Time_1
               + ContribFile*Time_1 + OwnerFollower*Time_1 + AvgFollower*Time_1 + OwnerType*Time_1
               , data = df, random = ~ Time_1 | prjId, method = "ML", na.action=na.exclude)
summary(c.model)

## p< 0.1 = members, watchers, pullReq, issueCmnt, MemCommitters, PRClosedCnt, IssueClosedCnt, LicenceApache License 2.0, 
##          LicenceGNU General Public License v2.0, ContribFilehttps://api.github.com/repos/lift/framework/contents/CONTRIBUTING.md, 
##          ContribFilehttps://api.github.com/repos/mne-tools/mne-python/contents/.github/CONTRIBUTING.md,
##          

# Unconditional growth model with predictors at 0.1 significance level

d.model <- lme(issues ~ members*Time_1 + watchers*Time_1 + pullReq*Time_1
               + CmtCmnt*Time_1 + issueCmnt*Time_1 
               + MemCommitters*Time_1 + PRClosedCnt*Time_1 + IssueClosedCnt*Time_1 
               + Licence*Time_1
               + ContribFile*Time_1
               , data = df, random = ~ Time_1 | prjId, method = "ML", na.action=na.exclude)

summary(d.model)

# Unconditional growth model with predictors at 0.05 significance level

#e.model <- lme(issues ~ watchers*Time_1 + pullReq*Time_1
#               + CmtCmnt*Time_1 + issueCmnt*Time_1 
#               + MemCommitters*Time_1 + PRClosedCnt*Time_1 + IssueClosedCnt*Time_1 
#               + "LicenceGNU Affero General Public License v3.0"*Time_1
#               + "LicenceGNU General Public License v2.0"*Time_1
#               + "ContribFilehttps://api.github.com/repos/Bukkit/Bukkit/contents/CONTRIBUTING.md"*Time_1
#               + "ContribFilehttps://api.github.com/repos/dlang/phobos/contents/CONTRIBUTING.md"*Time_1
#               + "ContribFilehttps://api.github.com/repos/enthought/enable/contents/CONTRIBUTING.rst"*Time_1
#               + "ContribFilehttps://api.github.com/repos/lift/framework/contents/CONTRIBUTING.md"*Time_1
#               + "ContribFilehttps://api.github.com/repos/request/request/contents/CONTRIBUTING.md"*Time_1
#               + "ContribFilehttps://api.github.com/repos/xbmc/xbmc/contents/docs/CONTRIBUTING.md"*Time_1
#               , data = df, random = ~ Time_1 | prjId, method = "ML", na.action=na.exclude)

e.model <- lme(issues ~ watchers*Time_1 + pullReq*Time_1
               + CmtCmnt*Time_1 + issueCmnt*Time_1 
               + MemCommitters*Time_1 + PRClosedCnt*Time_1 + IssueClosedCnt*Time_1 
               , data = df, random = ~ Time_1 | prjId, method = "ML", na.action=na.exclude)

summary(e.model)


# Do the trajectories of the number of commits differ by numbers of watchers and owner types.

names(df)

f.model <-lme(issues ~ members*Time_1 + commits*Time_1 + watchers*Time_1 + pullReq*Time_1
              , data = df, random = ~ Time_1 | prjId, method = "ML", na.action=na.exclude)

summary(f.model)


g.model <-lme(issues ~ members + commits*Time_1 + watchers*Time_1 + pullReq*Time_1
              , data = df, random = ~ Time_1 | prjId, method = "ML", na.action=na.exclude)

summary(g.model)

#h.model <-lme(issues ~ members + commits*Time_1 + pullReq*Time_1
#              , data = df, random = ~ Time_1 | prjId, method = "ML", na.action=na.exclude)
#
#summary(h.model)


