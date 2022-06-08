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
         , Time_1 = Time -1
         #, Licence = factor(Licence)
         #, ContribFile = factor(ContribFile)
         ) %>% 
  dplyr::select(Time, Time_1, everything()) 

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
names(df)
dim(df.sam)
view(df.sam)

# Data Viz----
names(df)

xyplot(issues ~ Time_1 | prjId, data = df5,
       panel = function(x,y){
         panel.xyplot(x, y)
         panel.lmline(x, y)
       }, as.table = T)


interaction.plot(df5$Time_1, df5$prjId, df5$issues)


plotmeans(df$issues~ df$Time_1, ylab="issues", main="The total number of issues over time", 
          data=df, lwd = 10, barwidth=5,  n.label=FALSE)

s <- geom_smooth(method = lm, se=FALSE)

ggplot(df, aes(members, issues)) + geom_point() + s

ggplot(df, aes(commits, issues)) + geom_point() + s

ggplot(df, aes(watchers, issues)) + geom_point() + s

ggplot(df, aes(pullReq, issues)) + geom_point() + s #log?


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


# Do the trajectories of the number of issues differ by the number of members, commits, watchers, and pull requests?

names(df)

c.model <-lme(issues ~ members*Time_1 + commits*Time_1 + watchers*Time_1 + pullReq*Time_1
              , data = df, random = ~ Time_1 | prjId, method = "ML", na.action=na.exclude)

summary(c.model)



# Remove member's ROC
d.model <-lme(issues ~ members + commits*Time_1 + watchers*Time_1 + pullReq*Time_1
              , data = df, random = ~ Time_1 | prjId, method = "ML", na.action=na.exclude)

summary(d.model)

# log the model

e.model <-lme(issues ~ members + commits*Time_1 + watchers*Time_1 + log(pullReq+1)*Time_1
              , data = df, random = ~ Time_1 | prjId, method = "ML", na.action=na.exclude)

summary(e.model)

# remove roc from log(pullReq)

f.model <-lme(issues ~ members + commits*Time_1 + watchers*Time_1 + log(pullReq+1)
              , data = df, random = ~ Time_1 | prjId, method = "ML", na.action=na.exclude)

summary(f.model)

# remove watchers predictors

g.model <-lme(issues ~ members + commits*Time_1  + log(pullReq+1)
              , data = df, random = ~ Time_1 | prjId, method = "ML", na.action=na.exclude)

summary(g.model)


## remove time variable
#
#
#f.model <-lme(issues ~ members + commits + watchers + log(pullReq+1)
#              , data = df, random = ~ Time_1 | prjId, method = "ML", na.action=na.exclude)
#
#summary(f.model)
