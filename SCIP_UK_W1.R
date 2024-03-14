# Perceived Discrimination and Political Behavior Replication Code
# British Journal of Political Science
# Date: Feb. 6, 2018
# Author: Kassra A.R. Oskooii (oskooiik@udel.edu)

# Replicated by Andrej Cvetic
# Dept. of Political Science, Trinity College Dublin
# 29/9/2023

rm(list=ls())
gc()

#Load Packages (Install Needed Packages)
library(MASS)
library(simcf) #Download Package: https://faculty.washington.edu/cadolph/index.php?page=60
library(tile) #Download Package: https://faculty.washington.edu/cadolph/index.php?page=60
library(nlme)
library(mgcv)
library(RColorBrewer)
library(stargazer)
library(psych)
library(dplyr)

# Solution for stargazer problem (read in just once if necessary):
# source: https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53

## Quick fix for stargazer <= 5.2.3 is.na() issue with long model names in R >= 4.2
# Unload stargazer if loaded
#detach("package:stargazer",unload=T)
# Delete it
#remove.packages("stargazer")
# Download the source
#download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# Unpack
#untar("stargazer_5.2.3.tar.gz")
# Read the sourcefile with .inside.bracket fun
#stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# Move the length check 5 lines up so it precedes is.na(.)
#stargazer_src[1990] <- stargazer_src[1995]
#stargazer_src[1995] <- ""
# Save back
#writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# Compile and install the patched package
#install.packages("stargazer", repos = NULL, type="source")
# read in the library again
#library(stargazer)

# additional packages not in original data analysis:
# install.packages("ggpubr")
# install.packages("rlang")
library(rlang)
library(ggpubr)

# variance inflation calculations 
library(car)
# libraries for multinomial models
library(nnet)
library(broom)
library(effects)


#Load Dataset
data<-haven::read_dta("SCIP_UK_W1.dta")
# alternative way 
#data<-haven::read_dta("C:/Users/Andrej Cvetic/Desktop/PhD/2023_TT_Replication Paper/3_Analysis/2_Analysis of SCIP Dataset/SCIP_UK_W1.dta")

stargazer(as.data.frame(data[,1:646]), type = "text")

## Correlation between political and societal discrimination 
## just to see if it is justified to have both variables in one model.  

# First tab the data provide histograms 

stargazer(as.data.frame(data[,c("race_pol_disc_1", "race_pol_disc_2")]), 
          covariate.labels = c("Political Discrimination 1", "Political Discrimination 2"), 
          title = "Summary Statistrics for Specific Measures (Political Discrimination)",
          style = "APSR", 
          out.header = T
)

stargazer(as.data.frame(data[,c("race_soc_disc_1", "race_soc_disc_2")]), 
          covariate.labels = c("Societal Discrimination 1", "Societal Discrimination 2"), 
          title = "Summary Statistrics for Specific Measures (Societal Discrimination)",
          style = "APSR", 
          out.header = T
)

table(data$race_pol_disc_1)
table(data$race_pol_disc_2)
table(data$race_soc_disc_1)
table(data$race_soc_disc_2)


# Check for correlations
# spearman seems more adapted for these variables 
cor.test(data$race_soc_disc_1, data$race_pol_disc_1, method = c("spearman"), use = "complete.obs")
cor.test(data$race_soc_disc_2, data$race_pol_disc_2, method = c("spearman"), use = "complete.obs")

# 0.2489576 
# 0.2823714 


## ETHNIC BASED ENGAGEMENT - BROAD DISC. I [all racial discrimination combined]
## no native born var. (because no variation in this var)
## no party id cause there is no var in the dataset)
## replicating the narrow model because vars for efficacy, dem satisfaction and vote duty are not available).

## ebe is a binary variable so, logit link should be used. 
formula<-(ebe ~ broad_discrimination_i +
                rel_att + pol_int + political_knowledge + val_three +
                lan_home + female + age + educ_yrs + highinc + medinc + misinc +
                pakistani)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

ebe.broad.i <- glm(formula, family=binomial(link=logit), data=mdata)
summary(ebe.broad.i)

pe.1 <- ebe.broad.i$coefficients  # point estimates
vc.1 <- vcov(ebe.broad.i)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.1, vc.1)

# nscen = 1 cause I am interested only in borad disc. i
xhyp1 <- cfMake(formula, mdata, nscen=1, f=mean)

xhyp1 <- cfName(xhyp1, "broad_discrimination_i", scen=1) 
xhyp1 <-cfChange(xhyp1, "broad_discrimination_i", x=1, xpre=0, scen=1)

# use the same CI as in the original study
# most relaxed CI that can be used 
xhyp1
yhyp1<-logitsimfd(xhyp1, simbetas, ci=0.90)
yhyp1
                


## ETHNIC BASED ENGAGEMENT - BROAD DISC. II [all racial discrimination combined + freq.]

formula<-(ebe ~ broad_discrimination_ii +
          rel_att + pol_int + political_knowledge + val_three +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            pakistani)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

ebe.broad.ii <- glm(formula, family=binomial(link=logit), data=mdata)
summary(ebe.broad.ii)

pe.2 <- ebe.broad.ii$coefficients  # point estimates
vc.2 <- vcov(ebe.broad.ii)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.2, vc.2)

# nscen = 1 cause I am interested only in borad disc. i
xhyp2 <- cfMake(formula, mdata, nscen=1, f=mean)

xhyp2 <- cfName(xhyp2, "broad_discrimination_ii", scen=1) 
xhyp2 <-cfChange(xhyp2, "broad_discrimination_ii", x=1, xpre=0, scen=1)

# use the same CI as in the original study
# most relaxed CI that can be used 
xhyp2
yhyp2<-logitsimfd(xhyp2, simbetas, ci=0.90)
yhyp2

## ETHNIC BASED ENGAGEMENT - SPECIFIC MEASURES [societal and political discrimination - race,  without frequencies]

cor(data$race_soc_disc_1, data$race_pol_disc_1, method = "spearman")

formula<-(ebe ~ race_soc_disc_1 + race_pol_disc_1 +
          rel_att + pol_int + political_knowledge + val_three +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            pakistani            
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

ebe.spec.1 <- glm(formula, family=binomial(link=logit), data=mdata)
summary(ebe.spec.1)

pe.3 <- ebe.spec.1$coefficients  # point estimates
vc.3 <- vcov(ebe.spec.1)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.3, vc.3)

# nscen = 1 cause I am interested only in borad disc. i
xhyp3 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp3 <- cfName(xhyp3, "race_soc_disc_1", scen=1) 
xhyp3 <-cfChange(xhyp3, "race_soc_disc_1", x=2, xpre=0, scen=1)

xhyp3 <- cfName(xhyp3, "race_pol_disc_1", scen=2) 
xhyp3 <-cfChange(xhyp3, "race_pol_disc_1", x=2, xpre=0, scen=2)

xhyp3
yhyp3 <-logitsimfd(xhyp3, simbetas, ci=0.90)
yhyp3


## ETHNIC BASED ENGAGEMENT - SPECIFIC MEASURES [societal and political discrimination - race,  with frequencies]

cor(data$race_soc_disc_2, data$race_pol_disc_2, method = "spearman")

formula<-(ebe ~ race_soc_disc_2 + race_pol_disc_2 +
            rel_att + pol_int + political_knowledge + val_three +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            pakistani
          
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

ebe.spec.2 <- glm(formula, family=binomial(link=logit), data=mdata)
summary(ebe.spec.2)

pe.4 <- ebe.spec.2$coefficients  # point estimates
vc.4 <- vcov(ebe.spec.2)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.4, vc.4)

# nscen = 1 cause I am interested only in borad disc. i
xhyp4 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp4 <- cfName(xhyp4, "race_soc_disc_2", scen=1) 
xhyp4 <-cfChange(xhyp4, "race_soc_disc_2", x=6, xpre=0, scen=1)

xhyp4 <- cfName(xhyp4, "race_pol_disc_2", scen=2) 
xhyp4 <-cfChange(xhyp4, "race_pol_disc_2", x=6, xpre=0, scen=2)

xhyp4
yhyp4 <-logitsimfd(xhyp4, simbetas, ci=0.90)
yhyp4


## ETHNIC BASED ENGAGEMENT - SPECIFIC MEASURES [societal and political discrimination - all,  without frequencies]

cor(data$soc_disc_1, data$pol_disc_1, method = "spearman")

formula<-(ebe ~ soc_disc_1 + pol_disc_1 +
            rel_att + pol_int + political_knowledge + val_three +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            pakistani
          
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

ebe.spec.3 <- glm(formula, family=binomial(link=logit), data=mdata)
summary(ebe.spec.3)

pe.5 <- ebe.spec.3$coefficients  # point estimates
vc.5 <- vcov(ebe.spec.3)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.5, vc.4)

# nscen = 1 cause I am interested only in borad disc. i
xhyp5 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp5 <- cfName(xhyp5, "soc_disc_1", scen=1) 
xhyp5 <-cfChange(xhyp5, "soc_disc_1", x=2, xpre=0, scen=1)

xhyp5 <- cfName(xhyp5, "pol_disc_1", scen=2) 
xhyp5 <-cfChange(xhyp5, "pol_disc_1", x=2, xpre=0, scen=2)

xhyp5
yhyp5 <-logitsimfd(xhyp5, simbetas, ci=0.90)
yhyp5


## ETHNIC BASED ENGAGEMENT - SPECIFIC MEASURES [societal and political discrimination - all,  with frequencies]

cor(data$soc_disc_2, data$pol_disc_2, method = "spearman")

formula<-(ebe ~ soc_disc_2 + pol_disc_2+ 
            rel_att + pol_int + political_knowledge + val_three +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            pakistani
)


mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

ebe.spec.4 <- glm(formula, family=binomial(link=logit), data=mdata)
summary(ebe.spec.4)

pe.6 <- ebe.spec.4$coefficients  # point estimates
vc.6 <- vcov(ebe.spec.4)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.6, vc.6)

# nscen = 1 cause I am interested only in borad disc. i
xhyp6 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp6 <- cfName(xhyp6, "soc_disc_2", scen=1) 
xhyp6 <-cfChange(xhyp6, "soc_disc_2", x=6, xpre=0, scen=1)

xhyp6 <- cfName(xhyp6, "pol_disc_2", scen=2) 
xhyp6 <-cfChange(xhyp6, "pol_disc_2", x=6, xpre=0, scen=2)

xhyp6
yhyp6 <-logitsimfd(xhyp6, simbetas, ci=0.90)
yhyp6

## TABLES
## TABLE 1: RACIAL DISCRIMINATION

modellabels<-c("Societal Discrimination 1", "Political Discrimination 1",
               "Societal Discrimination 2", "Political Discrimination 2",
               "Worship Attendance", "Political Interest", "Political Knowledge", 
               "Ireconcilable values", "Language at home",
               "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
               "Pakistani"
)

dvlabel<-c("Ethnic-Based Engagement")

stargazer(ebe.spec.1, ebe.spec.2,
          style="APSR", 
          covariate.labels = modellabels, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Ethnic Based Engagement: Specific Measures (Political and Societal Discrimination) for UK",
          dep.var.labels = dvlabel)

## TABLE 2: BROAD DISCRIMINATION 

modellabels<-c("Broad Discrimination 1", "Broad Discrimination 2",
               "Worship Attendance", "Political Interest", "Political Knowledge", 
               "Ireconcilable values", "Language at home",
               "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
               "Pakistani"
)

dvlabel<-c("Ethnic-Based Engagement")

stargazer(ebe.broad.i, ebe.broad.ii,
          style="APSR", 
          covariate.labels = modellabels, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Ethnic Based Engagement (Broad discrimination): UK",
          dep.var.labels = dvlabel)

## TABLE 3: ALL FORMS OF POLITICAL AND SOCIAL DISCRIMINATION >> DO NOT INCLUDE THIS!

modellabels<-c("Societal Discrimination 3", "Political Discrimination 3",
               "Societal Discrimination 4", "Political Discrimination 4",
               "Worship Attendance", "Political Interest", "Political Knowledge", 
               "Ireconcilable values", "Language at home",
               "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
               "Pakistani"
)

dvlabel<-c("Ethnic-Based Engagement")

stargazer(ebe.spec.3, ebe.spec.4,
          style="APSR", 
          covariate.labels = modellabels, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Ethnic Based Engagement (Specific discrimination - all types)",
          dep.var.labels = dvlabel)

## GRAPHS

## GRAPH 1: RACIAL DISCRIMINATION (WITH AND WITHOUT FREQUENCIES)

trace1 <- ropeladder(
  x=yhyp3$pe,
  lower=yhyp3$lower,
  upper=yhyp3$upper,
  labels=c("Societal Discrimination 1", "Political Discrimination 1"), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=1)

trace2 <- ropeladder(
  x=yhyp4$pe,
  lower=yhyp4$lower,
  upper=yhyp4$upper,
  labels=c("Societal Discrimination 2", "Political Discrimination 2"), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=2)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:2
)

# Widen space between titles on left
trace1$entryheight <- .12
trace2$entryheight <- .12

at.x1 <- seq(-.1, .2,.1)
at.x2 <- seq(-.1, .4,.1)



tc_uk_scip_1<-tile(trace1, trace2,
          vertmark,
          RxC=c(2,1),
          xaxis=list(at1 = at.x1, 
                     at2= at.x2, 
                     cex=.8),
          topaxis=list(at1 = at.x1, 
                       at2= at.x2,
                       cex=.8,
                       add = rep(TRUE,2)),
          topaxistitle = list(labels=c("Ethnic Based Engagement (Specific Measures): UK"), cex=1),
          undertitle = list(labels="Change in Predicted Probability (Min-Max)", cex=.7, x=1),
          output = list(file = "uk_scip_1", type="pdf", width=12), 
          frame=FALSE,
          gridlines=list(type="t", col="grey65")
)

## GRAPH 2: BROAD DISCRIMINATION (WITH AND WITHOUT FREQUENCIES)

trace3 <- ropeladder(
  x=yhyp1$pe,
  lower=yhyp1$lower,
  upper=yhyp1$upper,
  labels=c("Broad Discrimination 1"), 
  col= c("Black"),
  pch=c(16),
  cex=2, 
  plot=1)

trace4 <- ropeladder(
  x=yhyp2$pe,
  lower=yhyp2$lower,
  upper=yhyp2$upper,
  labels=c("Broad Discrimination 2"), 
  col= c("Black"),
  pch=c(16),
  cex=2, 
  plot=2)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:2
)

# Widen space between titles on left
trace3$entryheight <- .20
trace4$entryheight <- .20

at.x1 <- seq(-.1, .1,.1)
at.x2 <- seq(-.1, .1,.1)



tc_uk_scip_2<-tile(trace3, trace4,
                    vertmark,
                    RxC=c(2,1),
                    xaxis=list(at1 = at.x1, 
                               at2= at.x2, 
                               cex=.8),
                    topaxis=list(at1 = at.x1, 
                                 at2= at.x2,
                                 cex=.8,
                                 add = rep(TRUE,2)),
                    topaxistitle = list(labels=c("Ethnic Based Engagement (Broad discrimination)"), cex=1),
                    undertitle = list(labels="Change in Predicted Probability (Min-Max)", cex=.7, x=1),
                    output = list(file = "uk_scip_2", type="pdf", width=12), 
                    frame=FALSE,
                    gridlines=list(type="t", col="grey65")
)

## GRAPH 3: ALL TYPES OF DISCRIMINATION DISCRIMINATION (WITH AND WITHOUT FREQUENCIES) >> DO NOT INCLUDE THIS!


trace5 <- ropeladder(
  x=yhyp5$pe,
  lower=yhyp5$lower,
  upper=yhyp5$upper,
  labels=c("Societal Discrimination 3", "Political Discrimination 3"), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=1)

trace6 <- ropeladder(
  x=yhyp6$pe,
  lower=yhyp6$lower,
  upper=yhyp6$upper,
  labels=c("Societal Discrimination 4", "Political Discrimination 4"), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=2)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:2
)

# Widen space between titles on left
trace5$entryheight <- .12
trace6$entryheight <- .12

at.x1 <- seq(-.1, .1,.1)
at.x2 <- seq(-.1, .3,.1)



tc_uk_scip_3<-tile(trace5, trace6,
                    vertmark,
                    RxC=c(2,1),
                    xaxis=list(at1 = at.x1, 
                               at2= at.x2, 
                               cex=.8),
                    topaxis=list(at1 = at.x1, 
                                 at2= at.x2,
                                 cex=.8,
                                 add = rep(TRUE,2)),
                    topaxistitle = list(labels=c("Ethnic Based Engagement (Specific discrimination - all types)"), cex=1),
                    undertitle = list(labels="Change in Predicted Probability (Min-Max)", cex=.7, x=1),
                    output = list(file = "uk_scip_3", type="pdf", width=12), 
                    frame=FALSE,
                    gridlines=list(type="t", col="grey65")
)

###############################################################################
###################### IN-GROUP ATTACHMENT (3 POINT) ##########################
###############################################################################

## IN-GROUP ATTACHMENT (3 POINT) - BROAD DISC. I [all racial discrimination combined]
## no need to run these

formula<-(val_three ~ broad_discrimination_i + 
            rel_att + pol_int + political_knowledge +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            pakistani
          
)

## IN-GROUP ATTACHMENT (3 POINT) - BROAD DISC. II [all racial discrimination combined + freq]
## no need to run these

formula<-(val_three ~ broad_discrimination_ii + 
            rel_att + pol_int + political_knowledge +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            pakistani
          
)

## IN-GROUP ATTACHMENT (3 POINT) - SPECIFIC MEASURES [societal and political discrimination - race,  without frequencies]

formula<-(val_three ~ race_soc_disc_1 + race_pol_disc_1 +
            rel_att + pol_int + political_knowledge +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            pakistani
          
)

mdata <- extractdata(formula, data, na.rm=TRUE) 

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mdata$val_three <- factor(mdata$val_three, 
                          levels= c(1, 2, 3),
                          labels= c("Disagree", "Neutral", "Agree")
)

# because val_three is factored here, formula cannot be used as such, but 
# its content has to be rewritten. 

table(data$race_pol_disc_1)
table(data$race_soc_disc_1)

val3.spec.1 <- multinom(val_three ~ race_soc_disc_1 + race_pol_disc_1 +
                          rel_att + pol_int + political_knowledge +
                          lan_home + female + age + educ_yrs + highinc + medinc + misinc +
                          pakistani, Hess = T, data=mdata)


summary(val3.spec.1)


## TABLE - just for overview - table for paper and appendix at the end of the script

modellabels<-c("Societal Discrimination", "Political Discrimination",
                "Worship Attendance", "Political Interest", "Political Knowledge", "Language at home",
                "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
                "Pakistani"
                )


dvlabel<-c("Irreconcilable values")

stargazer(val3.spec.1,
          style="APSR", 
          covariate.labels = modellabels, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Irreconcilable values - racial discrimination without frequencies",
          dep.var.labels = dvlabel)

## GRAPHING

## This function should be used to simulate what Oskooii did with margins in Stata!
## Since the computing algorithm is not completely the same, some differences would
## A check should be done with estimating original Oskooii models using this function. 


#install.packages("MNLpred")
library(MNLpred)

## quick reminder: 
## https://www.rdocumentation.org/packages/MNLpred/versions/0.0.8/topics/mnl_fd2_ova

## 10% confidence interval
val3.spec.1.pp.pol <- mnl_fd2_ova(
  model = val3.spec.1,
  data = mdata,
  x = 'race_pol_disc_1',
  value1 = min(mdata$race_pol_disc_1),
  value2 = max(mdata$race_pol_disc_1),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

## also 10% confidence interval
val3.spec.1.pp.soc <- mnl_fd2_ova(
  model = val3.spec.1,
  data = mdata,
  x = 'race_soc_disc_1',
  value1 = min(mdata$race_soc_disc_1),
  value2 = max(mdata$race_soc_disc_1),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

val3.spec.1.pp.soc
val3.spec.1.pp.pol

## I retyped the result from the output

# Neutral relative to disagreement 
yhype5m <- c(-0.014459294, -0.0879958425) 
yhype5l<- c(-0.12913713, -0.15914584)
yhype5u<- c(0.1563583, 0.02489745)

# Agree relative to disagree
yhype5.2m <- c(0.002982346, 0.0871162451) 
yhype5.2l<- c(-0.18015294, -0.06162728)
yhype5.2u<- c(0.1543785, 0.20476891)

trace7 <- ropeladder(
  x=yhype5m,
  lower=yhype5l,
  upper=yhype5u,
  labels=c("Societal Discrimination 1", "Political Discrimination 1"), 
  col="Black", 
  pch=c(5,16),
  cex=2, 
  plot=1)

trace8 <- ropeladder(
  x=yhype5.2m,
  lower=yhype5.2l,
  upper=yhype5.2u,
  col="Black",
  pch=c(5,16),
  cex=2,
  plot=2)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:2)

trace7$entryheight <- .25
trace8$entryheight <- .25

at.x1 <- seq(-.2,.2,.1)
at.x2 <-seq(-.2,.3,.1)

tc_uk_scip_4<-tile(trace7,
         trace8,
         vertmark,
         xaxis=list(at1 = at.x1, 
                    at2= at.x2, 
                    cex=.8), 
         topaxis=list(at1 = at.x1, 
                      at2= at.x2, 
                      cex=.8,
                      add = rep(TRUE,2)),
         topaxistitle = list(labels1= "Neutral to Disagreement", 
                             labels2= "Agreement to Disagreement"),
         undertitle = list(labels="Change in Predicted Probability (Min-Max)", cex=.7, x=1),
         plottitle= list(label=c("Irreconcilable values (Specific Measures): UK)"), cex=1),
         output = list(file = "uk_scip_4", width=12, type="pdf"), 
         frame=FALSE,
         gridlines=list(type="x", col="grey65")
)

## IN-GROUP ATTACHMENT (3 POINT) - SPECIFIC MEASURES [societal and political discrimination - race,  with frequencies]

formula<-(val_three ~ race_soc_disc_2 + race_pol_disc_2 +
            rel_att + pol_int + political_knowledge +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            pakistani
          
)

mdata <- extractdata(formula, data, na.rm=TRUE) 

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mdata$val_three <- factor(mdata$val_three, 
                          levels= c(1, 2, 3),
                          labels= c("Disagree", "Neutral", "Agree")
)

# because val_three is factored here, formula cannot be used as such, but 
# its content has to be rewritten. 

val3.spec.2 <- multinom(val_three ~ race_soc_disc_2 + race_pol_disc_2 +
                          rel_att + pol_int + political_knowledge +
                          lan_home + female + age + educ_yrs + highinc + medinc + misinc +
                          pakistani, Hess = T, data=mdata)

summary(val3.spec.2)

## TABLE 

modellabels<-c("Societal Discrimination 2", "Political Discrimination 2",
               "Worship Attendance", "Political Interest", "Political Knowledge", "Language at home",
               "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
               "Pakistani"
)


dvlabel<-c("Irreconcilable values")

stargazer(val3.spec.2,
          style="APSR", 
          covariate.labels = modellabels, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Irreconcilable values - racial discrimination without frequencies",
          dep.var.labels = dvlabel)

## GRAPHING

## 10% confidence interval
val3.spec.2.pp.pol <- mnl_fd2_ova(
  model = val3.spec.2,
  data = mdata,
  x = 'race_pol_disc_2',
  value1 = min(mdata$race_pol_disc_2),
  value2 = max(mdata$race_pol_disc_2),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

## also 10% confidence interval
val3.spec.2.pp.soc <- mnl_fd2_ova(
  model = val3.spec.2,
  data = mdata,
  x = 'race_soc_disc_2',
  value1 = min(mdata$race_soc_disc_2),
  value2 = max(mdata$race_soc_disc_2),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

val3.spec.2.pp.soc
val3.spec.2.pp.pol

## Again I retyped the result from the output

# Neutral relative to disagreement 
yhype6m <- c(0.024166368, -0.08819801) 
yhype6l<- c(-0.10151574, -0.16414653)
yhype6u<- c(0.1979090, 0.03947863)

# Agree relative to disagree
yhype6.2m <- c(-0.016777547, 0.11448047) 
yhype6.2l<- c(-0.19708888, -0.05573421)
yhype6.2u<- c(0.1371499, 0.23482349)

trace9 <- ropeladder(
  x=yhype6m,
  lower=yhype6l,
  upper=yhype6u,
  labels=c("Societal Discrimination 2", "Political Discrimination 2"), 
  col="Black", 
  pch=c(5,16),
  cex=2, 
  plot=1)

trace10 <- ropeladder(
  x=yhype6.2m,
  lower=yhype6.2l,
  upper=yhype6.2u,
  col="Black",
  pch=c(5,16),
  cex=2,
  plot=2)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:2)

trace9$entryheight <- .25
trace10$entryheight <- .25

at.x1 <- seq(-.2,.2,.1)
at.x2 <-seq(-.2,.3,.1)

tc_uk1_scip_5<-tile(trace9,
                    trace10,
                    vertmark,
                    xaxis=list(at1 = at.x1, 
                               at2= at.x2, 
                               cex=.8), 
                    topaxis=list(at1 = at.x1, 
                                 at2= at.x2, 
                                 cex=.8,
                                 add = rep(TRUE,2)),
                    undertitle = list(labels="Change in Predicted Probability (Min-Max)", cex=.7, x=1),
                    topaxistitle = list(labels1= "Neutral to Disagreement", 
                                    labels2= "Agreement to Disagreement"),
                    output = list(file = "uk_scip_5", width=12, type="pdf"), 
                    frame=FALSE,
                    gridlines=list(type="x", col="grey65")
)

## IN-GROUP ATTACHMENT (3 POINT) - SPECIFIC MEASURES [societal and political discrimination - all,  without frequencies]
## >> DO NOT INCLUDE THIS!
formula<-(val_three ~ soc_disc_1 + pol_disc_1 +
            rel_att + pol_int + political_knowledge +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            pakistani
          
)

mdata <- extractdata(formula, data, na.rm=TRUE) 

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mdata$val_three <- factor(mdata$val_three, 
                          levels= c(1, 2, 3),
                          labels= c("Disagree", "Neutral", "Agree")
)

# because val_three is factored here, formula cannot be used as such, but 
# its content has to be rewritten. 

val3.spec.3 <- multinom(val_three ~ soc_disc_1 + pol_disc_1 +
                          rel_att + pol_int + political_knowledge +
                          lan_home + female + age + educ_yrs + highinc + medinc + misinc +
                          pakistani, Hess = T, data=mdata)

summary(val3.spec.3)

## TABLE 

modellabels<-c("Societal Discrimination 3", "Political Discrimination 3",
               "Worship Attendance", "Political Interest", "Political Knowledge", "Language at home",
               "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
               "Pakistani"
)


dvlabel<-c("Irreconcilable values")

stargazer(val3.spec.3,
          style="APSR", 
          covariate.labels = modellabels, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Irreconcilable values - any type of discrimination without frequencies",
          dep.var.labels = dvlabel)

## GRAPHING

## 10% confidence interval
val3.spec.3.pp.pol <- mnl_fd2_ova(
  model = val3.spec.3,
  data = mdata,
  x = 'pol_disc_1',
  value1 = min(mdata$pol_disc_1),
  value2 = max(mdata$pol_disc_1),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

## also 10% confidence interval
val3.spec.3.pp.soc <- mnl_fd2_ova(
  model = val3.spec.3,
  data = mdata,
  x = 'soc_disc_1',
  value1 = min(mdata$soc_disc_1),
  value2 = max(mdata$soc_disc_1),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

val3.spec.3.pp.soc
val3.spec.3.pp.pol

## Again I retyped the result from the output

# Neutral relative to disagreement 
yhype7m <- c(-0.02147015, -0.06000922) 
yhype7l<- c(-0.11114188, -0.116153899)
yhype7u<- c(0.09454766, 0.00300885)

# Agree relative to disagree
yhype7.2m <- c(0.03865089, 0.07123149) 
yhype7.2l<- c(-0.08892456, -0.005539904)
yhype7.2u<- c(0.15000812, 0.14257922)

trace11 <- ropeladder(
  x=yhype7m,
  lower=yhype7l,
  upper=yhype7u,
  labels=c("Societal Discrimination 3", "Political Discrimination 3"), 
  col="Black", 
  pch=c(5,16),
  cex=2, 
  plot=1)

trace12 <- ropeladder(
  x=yhype7.2m,
  lower=yhype7.2l,
  upper=yhype7.2u,
  col="Black",
  pch=c(5,16),
  cex=2,
  plot=2)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:2)

trace11$entryheight <- .25
trace12$entryheight <- .25

at.x1 <- seq(-.2,.2,.1)
at.x2 <-seq(-.2,.3,.1)

tc_uk_scip_6<-tile(trace11,
                    trace12,
                    vertmark,
                    xaxis=list(at1 = at.x1, 
                               at2= at.x2, 
                               cex=.8), 
                    topaxis=list(at1 = at.x1, 
                                 at2= at.x2, 
                                 cex=.8,
                                 add = rep(TRUE,2)),
                    undertitle = list(labels="Change in Predicted Probability (Min-Max)", cex=.7, x=1),
                    topaxistitle = list(labels1= "Neutral to Disagreement", 
                                    labels2= "Agreement to Disagreement"),
                    output = list(file = "uk_scip_6", width=12, type="pdf"), 
                    frame=FALSE,
                    gridlines=list(type="x", col="grey65")
)

## IN-GROUP ATTACHMENT (3 POINT) - SPECIFIC MEASURES [societal and political discrimination - all,  with frequencies]
## >> DO NOT INCLUDE THIS!
formula<-(val_three ~ soc_disc_2 + pol_disc_2 +
            rel_att + pol_int + political_knowledge +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            pakistani
)


mdata <- extractdata(formula, data, na.rm=TRUE) 

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mdata$val_three <- factor(mdata$val_three, 
                          levels= c(1, 2, 3),
                          labels= c("Disagree", "Neutral", "Agree")
)

# because val_three is factored here, formula cannot be used as such, but 
# its content has to be rewritten. 

val3.spec.4 <- multinom(val_three ~ soc_disc_2 + pol_disc_2 +
                          rel_att + pol_int + political_knowledge +
                          lan_home + female + age + educ_yrs + highinc + medinc + misinc +
                          pakistani, Hess = T, data=mdata)

summary(val3.spec.4)

modellabels<-c("Societal Discrimination 4", "Political Discrimination 4",
               "Worship Attendance", "Political Interest", "Political Knowledge", "Language at home",
               "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
               "Pakistani"
)


dvlabel<-c("Irreconcilable values")

stargazer(val3.spec.4,
          style="APSR", 
          covariate.labels = modellabels, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Irreconcilable values - any type of discrimination without frequencies",
          dep.var.labels = dvlabel)

## GRAPHING

## 10% confidence interval
val3.spec.4.pp.pol <- mnl_fd2_ova(
  model = val3.spec.4,
  data = mdata,
  x = 'pol_disc_2',
  value1 = min(mdata$pol_disc_2),
  value2 = max(mdata$pol_disc_2),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

## also 10% confidence interval
val3.spec.4.pp.soc <- mnl_fd2_ova(
  model = val3.spec.4,
  data = mdata,
  x = 'soc_disc_2',
  value1 = min(mdata$soc_disc_2),
  value2 = max(mdata$soc_disc_2),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

val3.spec.4.pp.soc
val3.spec.4.pp.pol

## Again I retyped the result from the output

# Neutral relative to disagreement 
yhype8m <- c(0.033984545, -0.06421420) 
yhype8l<- c(-0.1052392, -0.13182875)
yhype8u<- c(0.23395949, 0.0197175115)

# Agree relative to disagree
yhype8.2m <- c(0.002869182, 0.12850683) 
yhype8.2l<- c(-0.1971057, 0.03018817)
yhype8.2u<- c(0.16930993, 0.2114703675)

trace13 <- ropeladder(
  x=yhype8m,
  lower=yhype8l,
  upper=yhype8u,
  labels=c("Societal Discrimination 4", "Political Discrimination 4"), 
  col="Black", 
  pch=c(5,16),
  cex=2, 
  plot=1)

trace14 <- ropeladder(
  x=yhype8.2m,
  lower=yhype8.2l,
  upper=yhype8.2u,
  col="Black",
  pch=c(5,16),
  cex=2,
  plot=2)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:2)

trace13$entryheight <- .25
trace14$entryheight <- .25

at.x1 <- seq(-.2,.2,.1)
at.x2 <-seq(-.2,.3,.1)

tc_uk_scip_7<-tile(trace13,
                    trace14,
                    vertmark,
                    xaxis=list(at1 = at.x1, 
                               at2= at.x2, 
                               cex=.8), 
                    topaxis=list(at1 = at.x1, 
                                 at2= at.x2, 
                                 cex=.8,
                                 add = rep(TRUE,2)),
                    undertitle = list(labels="Change in Predicted Probability (Min-Max)", cex=.7, x=1),
                    topaxistitle = list(labels1= "Neutral to Disagreement", 
                                    labels2= "Agreement to Disagreement"),
                    output = list(file = "uk_scip_7", width=12, type="pdf"), 
                    frame=FALSE,
                    gridlines=list(type="x", col="grey65")
)


### MULTINOMIAL MODELS FOR RACIAL DISCRIMINATION VARIABLES

modellabels<-c("Societal Discrimination 1", "Political Discrimination 1",
               "Societal Discrimination 2", "Political Discrimination 2",
               "Worship Attendance", "Political Interest", "Political Knowledge", "Language at home",
               "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
               "Pakistani"
)

dvlabel<-c("Irreconcilable values")

stargazer(val3.spec.1, val3.spec.2,
          style="APSR", 
          covariate.labels = modellabels, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Irreconcilable values (Specific discrimination - racial)",
          dep.var.labels = dvlabel)

### MULTINOMIAL MODELS FOR DISCRIMINATION OF ALL TYPES >> DO NOT INCLUDE THIS!

modellabels<-c("Societal Discrimination 3", "Political Discrimination 3",
               "Societal Discrimination 4", "Political Discrimination 4",
               "Worship Attendance", "Political Interest", "Political Knowledge", "Language at home",
               "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
               "Pakistani"
)

dvlabel<-c("Irreconcilable values")

stargazer(val3.spec.3, val3.spec.4,
          style="APSR", 
          covariate.labels = modellabels, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Irreconcilable values (Specific discrimination - all types)",
          dep.var.labels = dvlabel)


################################################################################

## FREQUENCY GRAPHS

rm(list=ls())

#Load Dataset
data<-haven::read_dta("SCIP_UK_W1.dta")
#data<-haven::read_dta("C:/Users/Andrej Cvetic/Desktop/PhD/2023_TT_Replication Paper/3_Analysis/2_Analysis of SCIP Dataset/SCIP_NL_W1.dta")

## Graphs for variables 

table(data$race_pol_disc_1)
table(data$race_pol_disc_2)
table(data$race_soc_disc_1)
table(data$race_soc_disc_2)

values1 <- c(0, 1, 2)
frequencies1 <- c(1447, 71, 11)

values2 <- c(0, 1, 2, 3, 4, 6)
frequencies2 <- c(1451, 5, 28, 34, 2, 9)

values3 <- c(0, 1, 2)
frequencies3 <- c(1438, 89, 2)

values4 <- c(0, 1, 2, 3, 4)
frequencies4 <- c(1448, 11, 41, 27, 2)

## SAVE THE GRAPH FROM THE GRAPH EDIOR IN R

# Race pol disc 1
# Create a data frame
data1 <- data.frame(values1, frequencies1)
data2 <- data.frame(values2, frequencies2)
data3 <- data.frame(values3, frequencies3)
data4 <- data.frame(values4, frequencies4)

# Plotting the bar graph
p1 <- ggplot(data1, aes(x = factor(values1), y = frequencies1)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(labels = c("0", "1", "2")) +
  labs(x = "Values", y = "Frequency", title = "Frequencies for Political Discrimination 1 (UK)")

p1 + geom_text(aes(label = frequencies1), vjust = -0.5, color = "black", size = 3)

# Plotting the bar graph
p2 <- ggplot(data2, aes(x = factor(values2), y = frequencies2)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(labels = c("0", "1", "2", "3", "4", "5", "6")) +
  labs(x = "Values", y = "Frequency", title = "Frequencies for Political Discrimination 2 (UK)")

p2 + geom_text(aes(label = frequencies2), vjust = -0.5, color = "black", size = 3)

# Plotting the bar graph
p3 <- ggplot(data3, aes(x = factor(values3), y = frequencies3)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(labels = c("0", "1", "2")) +
  labs(x = "Values", y = "Frequency", title = "Frequencies for Societal Discrimination 1 (UK)")

p3 + geom_text(aes(label = frequencies3), vjust = -0.5, color = "black", size = 3)


# Plotting the bar graph
p4 <- ggplot(data4, aes(x = factor(values4), y = frequencies4)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(labels = c("0", "1", "2", "3", "4", "5", "6")) +
  labs(x = "Values", y = "Frequency", title = "Frequencies for Societal Discrimination 2 (UK)")

p4 + geom_text(aes(label = frequencies4), vjust = -0.5, color = "black", size = 3)



