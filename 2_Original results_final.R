# Perceived Discrimination and Political Behavior 
# Extended Replication Code

# Author of original results: Kassra A.R. Oskooii (oskooiik@udel.edu)

# Replicated by Andrej Cvetic (cvetica@tcd.ie)
# Dept. of Political Science, Trinity College Dublin
# 29/9/2023

# Software specifications:

# Version of R software: 4.2.0 "Vigorous Calisthenics"
# Important for simcf and tile package which were not updated to work with 
# newer versions of R. 
# R studio: RStudio-2022.02.3-492


##### PACKAGES #####

# clean the environment

rm(list=ls())
gc()

# same as original

library(MASS)
library(simcf) #Download Package: https://faculty.washington.edu/cadolph/index.php?page=60
library(tile) #Download Package: https://faculty.washington.edu/cadolph/index.php?page=60
library(nlme)
library(mgcv)
library(RColorBrewer)
library(stargazer)
library(psych)
library(dplyr)
library(nnet) #for multinomial models
library(MNLpred) #for predicted probabilities and first difference plotting


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
library(ggplot2)
library(rlang)
library(ggpubr)

# correlations 
#instal only the first time 
#install.packages("Hmisc")
library("Hmisc")

# read in the data - using haven package

data<-haven::read_dta("EMBES_BJPS_DATA.dta")

# check the frequencies
# tibble needs to be treated as data frame to run descriptives
stargazer(as.data.frame(data[,1:47]), type = "text")
table(data$socdisc)
table(data$poldisc)

cor.res <- rcorr(as.matrix(data), type = "spearman")
cor.res
# better presentation 
# function 
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flat.matrix <- flattenCorrMatrix(cor.res$r, cor.res$P)
flat.matrix
# stargaze corrrelations


#correlation of just the most important discrimination vriables:
## Correlation between political and societal discrimination 
## just to see if it is justified to have both variables in one model.  

# First tab the data provide histograms 

tab.poldisc <- table(data$poldisc)
tab.socdisc <- table(data$socdisc)
tab.poldisc
tab.socdisc

table(data$disc_race_binary)
table(data$disc_race)
summary(data$disc_race)
mean(data$disc_race)

stargazer(as.data.frame(data[,c("poldisc", "socdisc")]), 
          covariate.labels = c("Political Discrimination", "Societal Discrimination"), 
          title = "Summary Statistics for Specific Measures (Political and Societal Discrimination",
          style = "APSR", 
          out.header = T
)


# Check for correlations
# spearman seems more adapted for these variables 
cor.test(data$socdisc, data$poldisc, method = c("spearman"), use = "complete.obs")
# 0.4243991

cor.test(data$socdisc, data$poldisc, method = c("pearson"), use = "complete.obs")
# 0.5403


ggscatter(data, x = "poldisc", y = "socdisc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          add.params = list(color = "blue",
                            fill = "lightgray"),
          xlab = "Political Discrimination", ylab = "Societal Discrimination")

################################################################################
############### GRAPHS OF FREQUENCIES - NOT TO BE USED #########################
################################################################################


poldisc_hist <- gghistogram(data, x = "poldisc",
                            add = "mean", 
                            rug = TRUE,
                            fill = "lightblue", 
                            color = "black",
                            bins = 11,
                            binwidth = 1 
)

ggpar(poldisc_hist, 
      xlab = "Political Discrimination", 
      ylab = "Number of respondents", 
      xlim = c(0, 12))

socdisc_hist <- gghistogram(data, x = "socdisc",
                            add = "mean", 
                            rug = TRUE,
                            fill = "lightblue", 
                            color = "black",
                            bins = 11,
                            binwidth = 1 
)
ggpar(socdisc_hist, 
      xlab = "Societal Discrimination", 
      ylab = "Number of respondents", 
      xlim = c(0, 12))


################################################################################
################## GRAPHS OF FREQUENCIES -  TO BE USED #########################
################################################################################

tab.poldisc
tab.socdisc

values_pol <- c(0, 1, 2, 3, 4, 6, 8, 9, 12)
frequencies_pol <- c(2143, 99, 230, 50, 71, 42, 7, 12, 4)

values_soc <- c(0, 1, 2, 3, 4, 6, 9)
frequencies_soc <- c(2125, 116, 214, 55, 87, 45, 16)

data_pol <- data.frame(values_pol, frequencies_pol)
data_soc <- data.frame(values_soc, frequencies_soc)

# Plotting the bar graph
p_pol <- ggplot(data_pol, aes(x = factor(values_pol), y = frequencies_pol)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(labels = c("0", "1", "2", "3", "4", "6", "8", "9", "12")) +
  labs(x = "Values", y = "Frequency", title = "Frequencies for Political Discrimination (Original Data)")

p_pol + geom_text(aes(label = frequencies_pol), vjust = -0.5, color = "black", size = 3)

# Plotting the bar graph
p_soc <- ggplot(data_soc, aes(x = factor(values_soc), y = frequencies_soc)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(labels = c("0", "1", "2", "3", "4", "6", "9")) +
  labs(x = "Values", y = "Frequency", title = "Frequencies for Societal Discrimination (Original Data)")

p_soc + geom_text(aes(label = frequencies_soc), vjust = -0.5, color = "black", size = 3)

#############################################################
############### ALTERNATIVE SPECIFICATIONS ##################
#############################################################

# CLEAN THE ENVIRONMENT 
rm(list=ls())

# READ IN THE NEW DATASET
data<-haven::read_dta("EMBES_REPLICATION_DATASET.dta")
View(data)

# GENERAL ELECTIONS (Specific Measures) - Identity and Migration

formula<-(voted2010 ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + strength_partyid +
            closeness_brit +english+ citizen + dur_of_stay+ female+ age+ education2+ 
            highinc+ medinc+ misinc+
            black_caribbean+ indian+ pakistani+ bangladeshi +
            vote_duty+ efficacy+ democ_satis + trust_parliament)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mod_sum <- glm(formula, family=binomial(link=logit), data=mdata)
mod_sum

pe.1 <- mod_sum$coefficients  # point estimates
vc.1 <- vcov(mod_sum)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.1, vc.1)

xhyp1 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp1 <- cfName(xhyp1, "socdisc", scen=1) 
xhyp1 <-cfChange(xhyp1, "socdisc", x=9, xpre=0, scen=1)

xhyp1 <- cfName(xhyp1, "poldisc", scen=2) 
xhyp1 <-cfChange(xhyp1, "poldisc", x=12, xpre=0, scen=2)

xhyp1
yhyp1<-logitsimfd(xhyp1, simbetas, ci=0.90)
yhyp1

dvlabel1<-c("Vote in General Election")
dvlabel2<-c("Vote in Local Election")


# GENERAL ELECTIONS (Specific Measures - simplified) - Identity and Migration

formula<-(voted2010 ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + strength_partyid +
            closeness_brit +english+ citizen + dur_of_stay+ female+ age+ education2+ 
            highinc+ medinc+ misinc+
            black_caribbean+ indian+ pakistani+ bangladeshi
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}


mod_sum2 <- glm(formula, family=binomial(link=logit), data=mdata)
mod_sum2 

pe.2 <- mod_sum2$coefficients  # point estimates
vc.2 <- vcov(mod_sum2)

# add predicted coef. 

sims <- 10000
simbetas <- mvrnorm(sims, pe.2, vc.2)

xhyp2 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp2 <- cfName(xhyp2, "socdisc", scen=1) 
xhyp2 <-cfChange(xhyp2, "socdisc", x=9, xpre=0, scen=1)

xhyp2 <- cfName(xhyp2, "poldisc", scen=2) 
xhyp2 <-cfChange(xhyp2, "poldisc", x=12, xpre=0, scen=2)

xhyp2
yhyp2<-logitsimfd(xhyp2, simbetas, ci=0.90)
yhyp2

# GENERAL ELECTIONS (Specific Measures) -  Personal econ predictions

formula<-(voted2010 ~ 
            socdisc + poldisc +
            relatt_oth_r + pol_interest+ polknowledge+ partyid + english+ native_born+
            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi +
            personal_econ_retro + personal_econ_future
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mod_sum3 <- glm(formula = formula, family=binomial(link=logit), data=mdata)
mod_sum3

pe.3 <- mod_sum3$coefficients  # point estimates
vc.3 <- vcov(mod_sum3)

# add predicted coef. 

sims <- 10000
simbetas <- mvrnorm(sims, pe.3, vc.3)

xhyp3 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp3 <- cfName(xhyp3, "socdisc", scen=1) 
xhyp3 <-cfChange(xhyp3, "socdisc", x=9, xpre=0, scen=1)

xhyp3 <- cfName(xhyp3, "poldisc", scen=2) 
xhyp3 <-cfChange(xhyp3, "poldisc", x=12, xpre=0, scen=2)

xhyp3
yhyp3<-logitsimfd(xhyp3, simbetas, ci=0.90)
yhyp3


# GENERAL ELECTIONS (Specific Measures) -  National econ predictions

formula<-(voted2010 ~ 
            socdisc + poldisc +
            relatt_oth_r + pol_interest+ polknowledge+ partyid + english+ native_born+
            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi +
            national_econ_retro + national_econ_future
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}


mod_sum4 <- glm(formula, family=binomial(link=logit), data=mdata)
mod_sum4

# add predicted coefs. 

pe.4 <- mod_sum4$coefficients  # point estimates
vc.4 <- vcov(mod_sum4)

sims <- 10000
simbetas <- mvrnorm(sims, pe.4, vc.4)

xhyp4 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp4 <- cfName(xhyp4, "socdisc", scen=1) 
xhyp4 <-cfChange(xhyp4, "socdisc", x=9, xpre=0, scen=1)

xhyp4 <- cfName(xhyp4, "poldisc", scen=2) 
xhyp4 <-cfChange(xhyp4, "poldisc", x=12, xpre=0, scen=2)

xhyp4
yhyp4<-logitsimfd(xhyp4, simbetas, ci=0.90)
yhyp4

# GENERAL ELECTIONS (Specific Measures) -  SIT controls

formula<-(voted2010 ~ 
            socdisc + poldisc +
            relatt_oth_r + pol_interest+ polknowledge+ partyid + english+ native_born+
            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi +
            comm_affairs + social_net + more_prejudice
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mod_sum5 <- glm(formula, family=binomial(link=logit), data=mdata)
mod_sum5

# add predicted coef. 

pe.5 <- mod_sum5$coefficients  # point estimates
vc.5 <- vcov(mod_sum5)

sims <- 10000
simbetas <- mvrnorm(sims, pe.5, vc.5)

xhyp5 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp5 <- cfName(xhyp5, "socdisc", scen=1) 
xhyp5 <-cfChange(xhyp5, "socdisc", x=9, xpre=0, scen=1)

xhyp5 <- cfName(xhyp5, "poldisc", scen=2) 
xhyp5 <-cfChange(xhyp5, "poldisc", x=12, xpre=0, scen=2)

xhyp5
yhyp5 <- logitsimfd(xhyp5, simbetas, ci=0.90)
yhyp5

## TABLE 1: GENERAL ELECTIONS

modellabels5<-c("Societal Discrimination", "Political Discrimination",
                "Worship Attendance", "Political Interest", "Political Knowledge", "Strength of Party ID", 
                "Close to British ID", "Party ID (Yes=1)", "English (Main Lang)","Citizen", "Duration of Stay", "Native Born",
                "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
                "Black Caribbean", "Indian", "Pakistani", "Bangladeshi",
                "Vote Duty", "Political Efficacy","Democratic Satisfaction","Trust Parliament", 
                "Personal finance past", "Personal finance future",
                "National economic past", "National economic future",
                "Community affairs", "Social Network", "More prejudice")


dvlabel1<-c("Vote in General Election")
dvlabel2<-c("Vote in Local Election")

stargazer(mod_sum, mod_sum2, mod_sum3, mod_sum4, mod_sum5, 
          style="APSR", 
          covariate.labels = modellabels5, 
          out.header=T,
          model.numbers = TRUE, 
          title = "General Elections (Specific Measures) - Alternative Specifications",
          dep.var.labels = dvlabel1
)

## GRAPH 1: GENERAL ELECTIONS 

trace1 <- ropeladder(
  x=yhyp1$pe,
  lower=yhyp1$lower,
  upper=yhyp1$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=1)

trace2 <- ropeladder(
  x=yhyp2$pe,
  lower=yhyp2$lower,
  upper=yhyp2$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=2)

trace3 <- ropeladder(
  x=yhyp3$pe,
  lower=yhyp3$lower,
  upper=yhyp3$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=3)

trace4 <- ropeladder(
  x=yhyp4$pe,
  lower=yhyp4$lower,
  upper=yhyp4$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=4)

trace5 <- ropeladder(
  x=yhyp5$pe,
  lower=yhyp5$lower,
  upper=yhyp5$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=5)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:5
)

# Widen space between titles on left
trace1$entryheight <- .12
trace2$entryheight <- .12
trace3$entryheight <- .12
trace4$entryheight <- .12
trace5$entryheight <- .12

at.x1 <- seq(-.5, .2,.1)
at.x2 <- seq(-.5, .2,.1)
at.x3 <- seq(-.4, .3,.1)
at.x4 <- seq(-.4, .2,.1)
at.x5 <- seq(-.5, .2,.1)


tc1<-tile(trace1, trace2, trace3, trace4, trace5,
         vertmark,
         RxC=c(5,1),
         xaxis=list(at1 = at.x1, 
                    at2= at.x2, 
                    at3= at.x3,
                    at4= at.x4,
                    at5= at.x5,
                    cex=.8),
         topaxis=list(at1 = at.x1, 
                      at2= at.x2, 
                      at3= at.x3,
                      at4= at.x4,
                      at5= at.x5,
                      cex=.8,
                      add = rep(TRUE,5)),
         topaxistitle = list(labels=c("General Elections: Model 1", "General Elections: Model 2", "General Elections: Model 3", "General Elections: Model 4", "General Elections: Model 5"), cex=1),
         undertitle = list(labels1="Change in Predicted Probability (Min-Max)",
              labels2="Change in Predicted Probability (Min-Max)",
              labels3="Change in Predicted Probability (Min-Max)",
              labels4="Change in Predicted Probability (Min-Max)",
              labels5="Change in Predicted Probability (Min-Max)",
              cex=.7),
         output = list(file = "rep_fig1", type="pdf", width=10), 
         frame=FALSE,
         gridlines=list(type="t", col="grey65")
)

# LOCAL ELECTIONS 
# LOCAL ELECTIONS (Specific Measures) - Identity and Migration

formula<-(voted2010_local ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + strength_partyid +
            closeness_brit +english+ citizen + dur_of_stay+ female+ age+ education2+ 
            highinc+ medinc+ misinc+
            black_caribbean+ indian+ pakistani+ bangladeshi +
            vote_duty+ efficacy+ democ_satis + trust_parliament)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}


mod_sum6 <- glm(formula, family=binomial(link=logit), data=mdata)
mod_sum6

pe.6 <- mod_sum6$coefficients  # point estimates
vc.6 <- vcov(mod_sum6)

# add predicted coef. 

sims <- 10000
simbetas <- mvrnorm(sims, pe.6, vc.6)

xhyp6 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp6 <- cfName(xhyp6, "socdisc", scen=1) 
xhyp6 <-cfChange(xhyp6, "socdisc", x=9, xpre=0, scen=1)

xhyp6 <- cfName(xhyp6, "poldisc", scen=2) 
xhyp6 <-cfChange(xhyp6, "poldisc", x=12, xpre=0, scen=2)

xhyp6
yhyp6 <- logitsimfd(xhyp6, simbetas, ci=0.90)
yhyp6

# Key problem 
# Approx. 1000 (half of the 
# observations is lost.)


# LOCAL ELECTIONS (Specific Measures - simplified) - Identity and Migration

formula<-(voted2010_local ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + strength_partyid +
            closeness_brit +english+ citizen + dur_of_stay+ female+ age+ education2+ 
            highinc+ medinc+ misinc+
            black_caribbean+ indian+ pakistani+ bangladeshi
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}


mod_sum7 <- glm(formula, family=binomial(link=logit), data=mdata)
mod_sum7

pe.7 <- mod_sum7$coefficients  # point estimates
vc.7 <- vcov(mod_sum7)

# add predicted coef. 

sims <- 10000
simbetas <- mvrnorm(sims, pe.7, vc.7)

xhyp7 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp7 <- cfName(xhyp7, "socdisc", scen=1) 
xhyp7 <-cfChange(xhyp7, "socdisc", x=9, xpre=0, scen=1)

xhyp7 <- cfName(xhyp7, "poldisc", scen=2) 
xhyp7 <-cfChange(xhyp7, "poldisc", x=12, xpre=0, scen=2)

xhyp7
yhyp7 <- logitsimfd(xhyp7, simbetas, ci=0.90)
yhyp7

# LOCAL ELECTIONS (Specific Measures) -  Personal econ predictions

formula<-(voted2010_local ~ 
            socdisc + poldisc +
            relatt_oth_r + pol_interest+ polknowledge+ partyid + english+ native_born+
            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi +
            personal_econ_retro + personal_econ_future
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}


mod_sum8 <- glm(formula, family=binomial(link=logit), data=mdata)
mod_sum8

pe.8 <- mod_sum8$coefficients  # point estimates
vc.8 <- vcov(mod_sum8)

# add predicted coef. 

sims <- 10000
simbetas <- mvrnorm(sims, pe.8, vc.8)

xhyp8 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp8 <- cfName(xhyp8, "socdisc", scen=1) 
xhyp8 <-cfChange(xhyp8, "socdisc", x=9, xpre=0, scen=1)

xhyp8 <- cfName(xhyp8, "poldisc", scen=2) 
xhyp8 <-cfChange(xhyp8, "poldisc", x=12, xpre=0, scen=2)

xhyp8
yhyp8 <- logitsimfd(xhyp7, simbetas, ci=0.90)
yhyp8

# LOCAL ELECTIONS (Specific Measures) -  National econ predictions

formula<-(voted2010_local ~ 
            socdisc + poldisc +
            relatt_oth_r + pol_interest+ polknowledge+ partyid + english+ native_born+
            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi +
            national_econ_retro + national_econ_future
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mod_sum9 <- glm(formula, family=binomial(link=logit), data=mdata)
mod_sum9

pe.9 <- mod_sum9$coefficients  # point estimates
vc.9 <- vcov(mod_sum9)

# add predicted coef. 

sims <- 10000
simbetas <- mvrnorm(sims, pe.9, vc.9)

xhyp9 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp9 <- cfName(xhyp9, "socdisc", scen=1) 
xhyp9 <-cfChange(xhyp9, "socdisc", x=9, xpre=0, scen=1)

xhyp9 <- cfName(xhyp9, "poldisc", scen=2) 
xhyp9 <-cfChange(xhyp9, "poldisc", x=12, xpre=0, scen=2)

xhyp9
yhyp9 <- logitsimfd(xhyp9, simbetas, ci=0.90)
yhyp9


# LOCAL ELECTIONS (Specific Measures) -  SIT controls

formula<-(voted2010_local ~ 
            socdisc + poldisc +
            relatt_oth_r + pol_interest+ polknowledge+ partyid + english+ native_born+
            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi +
            comm_affairs + social_net + more_prejudice
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}


mod_sum10 <- glm(formula, family=binomial(link=logit), data=mdata)
mod_sum10

pe.10 <- mod_sum10$coefficients  # point estimates
vc.10 <- vcov(mod_sum10)

# add predicted coef. 

sims <- 10000
simbetas <- mvrnorm(sims, pe.10, vc.10)

xhyp10 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp10 <- cfName(xhyp10, "socdisc", scen=1) 
xhyp10 <-cfChange(xhyp10, "socdisc", x=9, xpre=0, scen=1)

xhyp10 <- cfName(xhyp10, "poldisc", scen=2) 
xhyp10 <-cfChange(xhyp10, "poldisc", x=12, xpre=0, scen=2)

xhyp10
yhyp10 <- logitsimfd(xhyp10, simbetas, ci=0.90)
yhyp10

## TABLE 2: LOCAL ELECTIONS

stargazer(mod_sum6, mod_sum7, mod_sum8, mod_sum9, mod_sum10,
          style="APSR", 
          covariate.labels = modellabels5, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Local Elections (Specific Measures) - Alternative Specifications",
          dep.var.labels = dvlabel2
)

## GRAPH 2: LOCAL ELECTIONS

trace6 <- ropeladder(
  x=yhyp6$pe,
  lower=yhyp6$lower,
  upper=yhyp6$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=1)

trace7 <- ropeladder(
  x=yhyp7$pe,
  lower=yhyp7$lower,
  upper=yhyp7$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=2)

trace8 <- ropeladder(
  x=yhyp8$pe,
  lower=yhyp8$lower,
  upper=yhyp8$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=3)

trace9 <- ropeladder(
  x=yhyp9$pe,
  lower=yhyp9$lower,
  upper=yhyp9$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=4)

trace10 <- ropeladder(
  x=yhyp10$pe,
  lower=yhyp10$lower,
  upper=yhyp10$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=5)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:5
)

# Widen space between titles on left
trace6$entryheight <- .12
trace7$entryheight <- .12
trace8$entryheight <- .12
trace9$entryheight <- .12
trace10$entryheight <- .12

at.x1 <- seq(-.5, .3,.1)
at.x2 <- seq(-.5, .3,.1)
at.x3 <- seq(-.1, .1,.1)
at.x4 <- seq(-.4, .3,.1)
at.x5 <- seq(-.4, .3,.1)


tc2<-tile(trace6, trace7, trace8, trace9, trace10,
         vertmark,
         RxC=c(5,1),
         xaxis=list(at1 = at.x1, 
                    at2= at.x2, 
                    at3= at.x3,
                    at4= at.x4,
                    at5= at.x5,
                    cex=.8),
         topaxis=list(at1 = at.x1, 
                      at2= at.x2, 
                      at3= at.x3,
                      at4= at.x4,
                      at5= at.x5,
                      cex=.8,
                      add = rep(TRUE,5)),
         topaxistitle = list(labels=c("Local Elections: Model 1", "Local Elections: Model 2", "Local Elections: Model 3", "Local Elections: Model 4", "Local Elections: Model 5"), cex=1),
         undertitle = list(labels1="Change in Predicted Probability (Min-Max)",
                           labels2="Change in Predicted Probability (Min-Max)",
                           labels3="Change in Predicted Probability (Min-Max)",
                           labels4="Change in Predicted Probability (Min-Max)",
                           labels5="Change in Predicted Probability (Min-Max)",
                           cex=.7),
         output = list(file = "rep_fig2", type="pdf", width=10), 
         frame=FALSE,
         gridlines=list(type="t", col="grey65")
)

##############################################################
################# ETHNIC BASED ENGAGEMENTS ###################
##############################################################

## ETHNIC BASED ENGAGEMENT (Specific Measures - full model) - Identity and Migration

formula<-(ethnic_active~ socdisc + poldisc+
                         relatt_oth_r + pol_interest + polknowledge + strength_partyid +
                         closeness_brit +english+ citizen + dur_of_stay+ female+ age+ education2+ 
                         highinc+ medinc+ misinc+
                         black_caribbean+ indian+ pakistani+ bangladeshi +
                         vote_duty+ efficacy+ democ_satis + trust_parliament)


mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mod_sum11 <- glm(formula, family=binomial(link=logit), data=mdata)
summary(mod_sum11)

pe.11 <- mod_sum11$coefficients  # point estimates
vc.11 <- vcov(mod_sum11)

# add predicted coef. 

sims <- 10000
simbetas <- mvrnorm(sims, pe.11, vc.11)

xhyp11 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp11 <- cfName(xhyp11, "socdisc", scen=1) 
xhyp11 <-cfChange(xhyp11, "socdisc", x=9, xpre=0, scen=1)

xhyp11 <- cfName(xhyp11, "poldisc", scen=2) 
xhyp11 <-cfChange(xhyp11, "poldisc", x=12, xpre=0, scen=2)

xhyp11
yhyp11 <- logitsimfd(xhyp11, simbetas, ci=0.90)
yhyp11


# ETHNIC BASED ENGAGEMENT (Specific Measures - simplified) - Identity and Migration

formula<-(ethnic_active ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + strength_partyid +
            closeness_brit +english+ citizen + dur_of_stay+ female+ age+ education2+ 
            highinc+ medinc+ misinc+
            black_caribbean+ indian+ pakistani+ bangladeshi
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}


mod_sum12 <- glm(formula, family=binomial(link=logit), data=mdata)
mod_sum12 

pe.12 <- mod_sum12$coefficients  # point estimates
vc.12 <- vcov(mod_sum12)

# add predicted coef. 

sims <- 10000
simbetas <- mvrnorm(sims, pe.12, vc.12)

xhyp12 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp12 <- cfName(xhyp12, "socdisc", scen=1) 
xhyp12 <-cfChange(xhyp12, "socdisc", x=9, xpre=0, scen=1)

xhyp12 <- cfName(xhyp12, "poldisc", scen=2) 
xhyp12 <-cfChange(xhyp12, "poldisc", x=12, xpre=0, scen=2)

xhyp12
yhyp12<-logitsimfd(xhyp12, simbetas, ci=0.90)
yhyp12

# ETHNIC BASED ENGAGEMENT (Specific Measures) -  Personal econ predictions

formula<-(ethnic_active ~ 
            socdisc + poldisc +
            relatt_oth_r + pol_interest+ polknowledge+ partyid + english+ native_born+
            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi +
            personal_econ_retro + personal_econ_future
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mod_sum13 <- glm(formula = formula, family=binomial(link=logit), data=mdata)
mod_sum13

pe.13 <- mod_sum13$coefficients  # point estimates
vc.13 <- vcov(mod_sum13)

# add predicted coef. 

sims <- 10000
simbetas <- mvrnorm(sims, pe.13, vc.13)

xhyp13 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp13 <- cfName(xhyp13, "socdisc", scen=1) 
xhyp13 <-cfChange(xhyp13, "socdisc", x=9, xpre=0, scen=1)

xhyp13 <- cfName(xhyp13, "poldisc", scen=2) 
xhyp13 <-cfChange(xhyp13, "poldisc", x=12, xpre=0, scen=2)

xhyp13
yhyp13<-logitsimfd(xhyp13, simbetas, ci=0.90)
yhyp13


# ETHNIC BASED ENGAGEMENT (Specific Measures) -  National econ predictions

formula<-(ethnic_active ~ 
            socdisc + poldisc +
            relatt_oth_r + pol_interest+ polknowledge+ partyid + english+ native_born+
            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi +
            national_econ_retro + national_econ_future
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mod_sum14 <- glm(formula, family=binomial(link=logit), data=mdata)
mod_sum14

# add predicted coefs. 

pe.14 <- mod_sum14$coefficients  # point estimates
vc.14 <- vcov(mod_sum14)

# add predicted coef. 

sims <- 10000
simbetas <- mvrnorm(sims, pe.14, vc.14)

xhyp14 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp14 <- cfName(xhyp14, "socdisc", scen=1) 
xhyp14 <-cfChange(xhyp14, "socdisc", x=9, xpre=0, scen=1)

xhyp14 <- cfName(xhyp14, "poldisc", scen=2) 
xhyp14 <-cfChange(xhyp14, "poldisc", x=12, xpre=0, scen=2)

xhyp14
yhyp14<-logitsimfd(xhyp14, simbetas, ci=0.90)
yhyp14

# ETHNIC BASED ENGAGEMENT (Specific Measures) -  SIT controls

formula<-(ethnic_active ~ 
            socdisc + poldisc +
            relatt_oth_r + pol_interest+ polknowledge+ partyid + english+ native_born+
            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi +
            comm_affairs + social_net + more_prejudice
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mod_sum15 <- glm(formula, family=binomial(link=logit), data=mdata)
mod_sum15

# add predicted coef. 

pe.15 <- mod_sum15$coefficients  # point estimates
vc.15 <- vcov(mod_sum15)

# add predicted coef. 

sims <- 10000
simbetas <- mvrnorm(sims, pe.15, vc.15)

xhyp15 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp15 <- cfName(xhyp15, "socdisc", scen=1) 
xhyp15 <-cfChange(xhyp15, "socdisc", x=9, xpre=0, scen=1)

xhyp15 <- cfName(xhyp15, "poldisc", scen=2) 
xhyp15 <-cfChange(xhyp15, "poldisc", x=12, xpre=0, scen=2)

xhyp15
yhyp15 <- logitsimfd(xhyp15, simbetas, ci=0.90)
yhyp15

## TABLE 3: ETHNIC BASED ENGAGEMENT

## Because theory does not give us any guidance on how economic predictions influence variables such as 
## ethnic based engagement, it makes no sense to include these variables in the final set of results. 
## Therefore, I leave them out from the tables, even though the code is going to remain. 


modellabels5<-c("Societal Discrimination", "Political Discrimination",
                "Worship Attendance", "Political Interest", "Political Knowledge", "Strength of Party ID", 
                "Close to British ID", "Party ID (Yes=1)", "English (Main Lang)","Citizen", "Duration of Stay", "Native Born",
                "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
                "Black Caribbean", "Indian", "Pakistani", "Bangladeshi",
                "Vote Duty", "Political Efficacy","Democratic Satisfaction","Trust Parliament",
                "Community affairs", "Social Network", "More prejudice")


dvlabel3<-c("Ethnic-Based Participation")

stargazer(mod_sum11, mod_sum12, mod_sum15,
          style="APSR", 
          covariate.labels = modellabels5, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Ethnic Based Engagement (Specific Measures) - Alternative Specifications",
          dep.var.labels = dvlabel3)


## GRAPH 3: ETHNIC BASED ENGAGEMENT 

trace11 <- ropeladder(
  x=yhyp11$pe,
  lower=yhyp11$lower,
  upper=yhyp11$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=1)

trace12 <- ropeladder(
  x=yhyp12$pe,
  lower=yhyp12$lower,
  upper=yhyp12$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=2)

trace15 <- ropeladder(
  x=yhyp15$pe,
  lower=yhyp15$lower,
  upper=yhyp15$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=3)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:3
)

# Widen space between titles on left
trace11$entryheight <- .12
trace12$entryheight <- .12
trace15$entryheight <- .12

at.x1 <- seq(-.1, .5,.1)
at.x2 <- seq(-.1, .5,.1)
at.x3 <- seq(-.1, .5,.1)


tc3<-tile(trace11, trace12, trace15,
         vertmark,
         RxC=c(3,1),
         xaxis=list(at1 = at.x1, 
                    at2= at.x2, 
                    at3= at.x3,
                    cex=.8),
         topaxis=list(at1 = at.x1, 
                      at2= at.x2, 
                      at3= at.x3,
                      cex=.8,
                      add = rep(TRUE,5)),
         topaxistitle = list(labels=c("Ethnic Based Engagement: Model 1", "Ethnic Based Engagement: Model 2", "Ethnic Based Engagement: Model 3"), cex=1),
         undertitle = list(labels1="Change in Predicted Probability (Min-Max)",
                           labels2="Change in Predicted Probability (Min-Max)",
                           labels3="Change in Predicted Probability (Min-Max)",
                           cex=0.7, x=0.5),
         output = list(file = "rep_fig3", type="pdf", width=10), 
         frame=FALSE,
         gridlines=list(type="t", col="grey65")
)

##############################################################
##################### IDENTITY CHOICE  #######################
##############################################################


## IDENTITY CHOICE (Specific Measures) - Identity and Migration

formula<- (identity ~ socdisc + poldisc+
             relatt_oth_r + pol_interest + polknowledge + strength_partyid +
             english+ citizen + dur_of_stay+ female+ age+ education2+ 
             highinc+ medinc+ misinc+
             black_caribbean+ indian+ pakistani+ bangladeshi +
             vote_duty+ efficacy+ democ_satis + trust_parliament)

mdata <- extractdata(formula, data, na.rm=TRUE) 

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mdata$identity <- factor(mdata$identity, 
                         levels= c(0, 1, 2),
                         labels=c("BlackAsian", "Both", "British"))


mlogitresult1 <- multinom(identity ~ socdisc + poldisc +
                            relatt_oth_r + pol_interest + polknowledge + strength_partyid +
                            english+ citizen + dur_of_stay+ female+ age+ education2+ 
                            highinc+ medinc+ misinc+
                            black_caribbean+ indian+ pakistani+ bangladeshi +
                            vote_duty+ efficacy+ democ_satis + trust_parliament, Hess = TRUE, data=mdata)

summary(mlogitresult1)

## PREDICTED PROBABILITIES - FIRST DIFFERENCE

mlogitresult1.fdpp.soc <- mnl_fd2_ova(
  model = mlogitresult1,
  data = mdata,
  x = "socdisc",
  value1 = min(mdata$socdisc),
  value2 = max(mdata$socdisc),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

mlogitresult1.fdpp.pol <- mnl_fd2_ova(
  model = mlogitresult1,
  data = mdata,
  x = 'poldisc',
  value1 = min(mdata$poldisc),
  value2 = max(mdata$poldisc),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

## IDENTITY CHOICE (Specific Measures - simplified model) - Identity and Migration

formula<-(identity ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + strength_partyid +
            english+ citizen + dur_of_stay+ female+ age+ education2+ 
            highinc+ medinc+ misinc+
            black_caribbean+ indian+ pakistani+ bangladeshi
)

mdata <- extractdata(formula, data, na.rm=TRUE) 

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mdata$identity <- factor(mdata$identity, 
                         levels= c(0, 1, 2),
                         labels=c("BlackAsian", "Both", "British"))


mlogitresult2 <- multinom(identity ~ 
                            socdisc + poldisc+
                            relatt_oth_r + pol_interest + polknowledge + strength_partyid +
                            english+ citizen + dur_of_stay+ female+ age+ education2+ 
                            highinc+ medinc+ misinc+
                            black_caribbean+ indian+ pakistani+ bangladeshi, Hess = TRUE, data=mdata)

summary(mlogitresult2)

## PREDICTED PROBABILITIES - FIRST DIFFERENCE

mlogitresult2.fdpp.soc <- mnl_fd2_ova(
  model = mlogitresult2,
  data = mdata,
  x = 'socdisc',
  value1 = min(mdata$socdisc),
  value2 = max(mdata$socdisc),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

mlogitresult2.fdpp.pol <- mnl_fd2_ova(
  model = mlogitresult2,
  data = mdata,
  x = 'poldisc',
  value1 = min(mdata$poldisc),
  value2 = max(mdata$poldisc),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

## IDENTITY CHOICE (Specific Measures) - SIT controls

formula<-(identity ~ 
            socdisc + poldisc +
            relatt_oth_r + pol_interest+ polknowledge+ partyid + english+ native_born+
            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi +
            comm_affairs + social_net + more_prejudice
)

mdata <- extractdata(formula, data, na.rm=TRUE) 

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mdata$identity <- factor(mdata$identity, 
                         levels= c(0, 1, 2),
                         labels=c("BlackAsian", "Both", "British"))


mlogitresult3 <- multinom(identity ~ 
                            socdisc + poldisc +
                            relatt_oth_r + pol_interest+ polknowledge+ partyid + english+ native_born+
                            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi +
                            comm_affairs + social_net + more_prejudice, Hess = TRUE, data=mdata)

summary(mlogitresult3)

## PREDICTED PROBABILITIES - FIRST DIFFERENCE

mlogitresult3.fdpp.soc <- mnl_fd2_ova(
  model = mlogitresult3,
  data = mdata,
  x = 'socdisc',
  value1 = min(mdata$socdisc),
  value2 = max(mdata$socdisc),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

mlogitresult3.fdpp.pol <- mnl_fd2_ova(
  model = mlogitresult3,
  data = mdata,
  x = 'poldisc',
  value1 = min(mdata$poldisc),
  value2 = max(mdata$poldisc),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)


modellabels6<-c("Societal Discrimination", "Political Discrimination",
                "Worship Attendance", "Political Interest", "Political Knowledge", "Strength of Party ID", 
                "Party ID (Yes=1)", "English (Main Lang)","Citizen", "Duration of Stay", "Native Born",
                "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
                "Black Caribbean", "Indian", "Pakistani", "Bangladeshi",
                "Vote Duty", "Political Efficacy","Democratic Satisfaction","Trust Parliament",
                "Community affairs", "Social Network", "More prejudice")

dvlabel4<-c("Identity Choice")

summary(mlogitresult1)
summary(mlogitresult2)
summary(mlogitresult3)

stargazer(mlogitresult1, mlogitresult2, mlogitresult3,
          style="APSR", 
          covariate.labels = modellabels6, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Identity Choice (Specific Measures) - Alternative Specifications",
          dep.var.labels = dvlabel4)


## GRAPHING 

mlogitresult1.fdpp.soc
mlogitresult1.fdpp.pol
mlogitresult2.fdpp.soc
mlogitresult2.fdpp.pol
mlogitresult3.fdpp.soc
mlogitresult3.fdpp.pol

#mlogitresult1
#Identifying Asian/Black relative to British 
yhype1p <- c(0.37742699, 0.10829187) 
yhype1l<- c(0.1838802, -0.1361604)
yhype1u<- c(0.53571794, 0.3631874)

#Identifying Both Relative to Asian/Black
yhype2p <- c(-0.31528732, -0.14895100) 
yhype2l<- c(-0.4344240, -0.3671330)
yhype2u<- c(-0.14403716, 0.1242746)

#mlogitresult2
#Identifying Asian/Black relative to British 
yhype3p <- c(0.35382839, 0.176407129) 
yhype3l<- c(0.1589841, -0.07649749)
yhype3u<- c(0.51559320, 0.41665443)

#Identifying Both Relative to Asian/Black
yhype4p <- c(-0.28641225, -0.182160903) 
yhype4l<- c(-0.4188167, -0.38487430)
yhype4u<- c(-0.10946900, 0.07645821)

#mlogitresult3
#Identifying Asian/Black relative to British 
yhype5p <- c(0.21739293, 0.2495949) 
yhype5l<- c(0.07115121, 0.06034842)
yhype5u<- c(0.36005022, 0.42583539)

#Identifying Both Relative to Asian/Black
yhype6p <- c(-0.22750431, -0.1320849) 
yhype6l<- c(-0.34793645, -0.30212492)
yhype6u<- c(-0.08782565, 0.05570787)

trace16 <- ropeladder(
  x=yhype1p,
  lower=yhype1l,
  upper=yhype1u,
  labels=c("Societal Disc.", "Political Disc."), 
  col="Black", 
  pch=c(5,16),
  cex=2, 
  plot=1)

list(labels5="Change in Predicted Probability (Min-Max)",
     labels6="Change in Predicted Probability (Min-Max)",
     cex=0.5, x=0.5)

trace17 <- ropeladder(
  x=yhype2p,
  lower=yhype2l,
  upper=yhype2u,
  col="Black", 
  pch=c(5,16),
  cex=2, 
  plot=2)

trace18 <- ropeladder(
  x=yhype3p,
  lower=yhype3l,
  upper=yhype3u,
  labels=c("Societal Disc.", "Political Disc."), 
  col="Black", 
  pch=c(5,16),
  cex=2, 
  plot=3)

trace19 <- ropeladder(
  x=yhype4p,
  lower=yhype4l,
  upper=yhype4u,
  col="Black", 
  pch=c(5,16),
  cex=2, 
  plot=4)

trace20 <- ropeladder(
  x=yhype5p,
  lower=yhype5l,
  upper=yhype5u,
  labels=c("Societal Disc.", "Political Disc."), 
  col="Black", 
  pch=c(5,16),
  cex=2, 
  plot=5)

trace21 <- ropeladder(
  x=yhype6p,
  lower=yhype6l,
  upper=yhype6u,
  col="Black", 
  pch=c(5,16),
  cex=2, 
  plot=6)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:6)

trace16$entryheight <- .25
trace17$entryheight <- .25
trace18$entryheight <- .25
trace19$entryheight <- .25
trace20$entryheight <- .25
trace21$entryheight <- .25

at.x1 <- seq(-.2,.5,.1)
at.x2 <-seq(-.5,.1,.1)
at.x3 <-seq(-.2,.5,.1)
at.x4 <-seq(-.5,.1,.1)
at.x5 <-seq(-.2,.5,.1)
at.x6 <-seq(-.4,.1,.1)

tc_4<-tile(trace16,
                    trace17,
                    trace18,
                    trace19,
                    trace20,
                    trace21,
                    vertmark,
                    RxC=c(3,2),
                    xaxis=list(at1 = at.x1, 
                               at2= at.x2,
                               at3= at.x3,
                               at4= at.x4,
                               at5= at.x5,
                               at6= at.x6,
                               cex=.8), 
                    topaxis=list(at1 = at.x1, 
                                 at2= at.x2,
                                 at3= at.x3,
                                 at4= at.x4,
                                 at5= at.x5,
                                 at6= at.x6,
                                 cex=.8,
                                 add = rep(TRUE,6)),
                    topaxistitle = list(labels1= "Black/Asian vs. British", 
                                    labels2= "Equally Both vs. Black/Asian"
                                    ),
                    undertitle = list(labels1="Change in Predicted Probability (Min-Max)",
                                      labels2="Change in Predicted Probability (Min-Max)",
                                      labels3="Change in Predicted Probability (Min-Max)",
                                      labels4="Change in Predicted Probability (Min-Max)",
                                      labels5="Change in Predicted Probability (Min-Max)",
                                      labels6="Change in Predicted Probability (Min-Max)",
                                      cex=0.5, x=0.5),
                    output = list(file = "rep_fig4", width=10, type="pdf"), 
                    frame=FALSE,
                    gridlines=list(type="x", col="grey65")
)


################################################################################
##################### NEW OUTCOME VARIABLES ####################################
################################################################################


## SUPPORT FOR VIOLENT DEMONSTRATIONS (Specific Measures) - full models

formula<-(sup_dem_binary ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
            vote_duty+ efficacy+ democ_satis+ trust_parliament)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mod_sum16<- glm(formula, family=binomial(link=logit), data=mdata)
summary(mod_sum16)

pe.16 <- mod_sum16$coefficients  
vc.16 <- vcov(mod_sum16)
pe.16
vc.16

sims <- 10000
simbetas <- mvrnorm(sims, pe.16, vc.16)

xhyp16 <- cfMake(formula, mdata, nscen=2, f=mean) #nscen = 2 because of soc and pol disc

xhyp16 <- cfName(xhyp16, "socdisc", scen=1) 
xhyp16 <-cfChange(xhyp16, "socdisc", x=9, xpre=0, scen=1)

xhyp16 <- cfName(xhyp16, "poldisc", scen=2) 
xhyp16 <-cfChange(xhyp16, "poldisc", x=12, xpre=0, scen=2)

xhyp16
yhyp16 <- logitsimfd(xhyp16, simbetas, ci=0.90) #use the 90 CI same as in original study
yhyp16                                    #cause socdisc is sig. on 0.1 lvl?  


## Non-electoral political participation (Specific Measures) - full models


formula<-(non_elec_partic_binary ~ 
            socdisc + poldisc +
            relatt_oth_r + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
            vote_duty+ efficacy+ democ_satis+ trust_parliament)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mod_sum17<- glm(formula, family=binomial(link=logit), data=mdata)
summary(mod_sum17)

pe.17 <- mod_sum17$coefficients  
vc.17 <- vcov(mod_sum17)

sims <- 10000
simbetas <- mvrnorm(sims, pe.17, vc.17)

xhyp17 <- cfMake(formula, mdata, nscen=2, f=mean) #nscen = 2 because of soc and pol disc

xhyp17 <- cfName(xhyp17, "socdisc", scen=1) 
xhyp17 <-cfChange(xhyp17, "socdisc", x=9, xpre=0, scen=1)

xhyp17 <- cfName(xhyp17, "poldisc", scen=2) 
xhyp17 <-cfChange(xhyp17, "poldisc", x=12, xpre=0, scen=2)

xhyp17
yhyp17 <- logitsimfd(xhyp17, simbetas, ci=0.90) #use the 90 CI same as in original study
yhyp17



## PROBLEM - CONTROLS IN THE FULL MODEL ARE NOT ADAPTED FOR ANALYSIS OF SUPPORT FOR VIOLENT DEMONSTRATIONS
## NOR NON-ELECTORAL BEHAVIOUR
## SOLUTIONS ESTIMATE SIMPLIFIED MODELS:

## SUPPORT FOR VIOLENT DEMONSTRATIONS (Specific Measures) - simplified

formula<-(sup_dem_binary ~ 
            socdisc + poldisc +
            relatt_oth_r + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mod_sum18<- glm(formula, family=binomial(link=logit), data=mdata)
summary(mod_sum18)

pe.18 <- mod_sum18$coefficients  
vc.18 <- vcov(mod_sum18)

sims <- 10000
simbetas <- mvrnorm(sims, pe.18, vc.18)

xhyp18 <- cfMake(formula, mdata, nscen=2, f=mean) #nscen = 2 because of soc and pol disc

xhyp18 <- cfName(xhyp18, "socdisc", scen=1) 
xhyp18 <-cfChange(xhyp18, "socdisc", x=9, xpre=0, scen=1)

xhyp18 <- cfName(xhyp18, "poldisc", scen=2) 
xhyp18 <-cfChange(xhyp18, "poldisc", x=12, xpre=0, scen=2)

xhyp18
yhyp18 <-logitsimfd(xhyp18, simbetas, ci=0.90) #use the 90 CI same as in original study
yhyp18


## Non-electoral political participation (Specific Measures) - simplified models


formula<-(non_elec_partic_binary ~ 
            socdisc + poldisc +
            relatt_oth_r + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mod_sum19<- glm(formula, family=binomial(link=logit), data=mdata)
summary(mod_sum19)

pe.19 <- mod_sum19$coefficients  
vc.19 <- vcov(mod_sum19)

sims <- 10000
simbetas <- mvrnorm(sims, pe.19, vc.19)

xhyp19 <- cfMake(formula, mdata, nscen=2, f=mean) #nscen = 2 because of soc and pol disc

xhyp19 <- cfName(xhyp19, "socdisc", scen=1) 
xhyp19 <-cfChange(xhyp19, "socdisc", x=9, xpre=0, scen=1)

xhyp19 <- cfName(xhyp19, "poldisc", scen=2) 
xhyp19 <-cfChange(xhyp19, "poldisc", x=12, xpre=0, scen=2)

xhyp19
yhyp19 <- logitsimfd(xhyp19, simbetas, ci=0.90) #use the 90 CI same as in original study
yhyp19


## CHANGE THE MODELS TO GET SOME MORE APPROPRIATE CONTROLS
## ATTENDANCE TO RELIGIOUS SERVICE >> PRIVATE RELIGIOUS BELIEVES (Bloom and Arikan 2019)
## VOTE DUTY >> EXCLUDE
## INCLUDE >> NATIONAL ECNOMIC EXPECTATIONS (Kurer et al 2019)


## SUPPORT FOR VIOLENT DEMONSTRATIONS (Specific Measures) - logit models - NEW MODELS
## ADD INTERNET USE

formula<-(sup_dem_binary ~ 
            socdisc + poldisc +
            social_net + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
            efficacy+ democ_satis+ trust_parliament+ national_econ_future + internet)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mod_sum20<- glm(formula, family=binomial(link=logit), data=mdata)
summary(mod_sum20)

pe.20 <- mod_sum20$coefficients  
vc.20 <- vcov(mod_sum20)

sims <- 10000
simbetas <- mvrnorm(sims, pe.20, vc.20)

xhyp20 <- cfMake(formula, mdata, nscen=2, f=mean) #nscen = 2 because of soc and pol disc

xhyp20 <- cfName(xhyp20, "socdisc", scen=1) 
xhyp20 <-cfChange(xhyp20, "socdisc", x=9, xpre=0, scen=1)

xhyp20 <- cfName(xhyp20, "poldisc", scen=2) 
xhyp20 <-cfChange(xhyp20, "poldisc", x=12, xpre=0, scen=2)

xhyp20
yhyp20 <-logitsimfd(xhyp20, simbetas, ci=0.90) #use the 90 CI same as in original study
yhyp20                                      #cause socdisc is sig. on 0.1 lvl?  


## Non-electoral political participation (Specific Measures) - logit models - NEW MODELS


formula<-(non_elec_partic_binary ~ 
            socdisc + poldisc +
            social_net + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
            efficacy+ democ_satis+ trust_parliament+ national_econ_future + internet)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mod_sum21<- glm(formula, family=binomial(link=logit), data=mdata)
summary(mod_sum21)

pe.21 <- mod_sum21$coefficients  
vc.21 <- vcov(mod_sum21)

sims <- 10000
simbetas <- mvrnorm(sims, pe.21, vc.21)

xhyp21 <- cfMake(formula, mdata, nscen=2, f=mean) #nscen = 2 because of soc and pol disc

xhyp21 <- cfName(xhyp21, "socdisc", scen=1) 
xhyp21 <-cfChange(xhyp21, "socdisc", x=9, xpre=0, scen=1)

xhyp21 <- cfName(xhyp21, "poldisc", scen=2) 
xhyp21 <-cfChange(xhyp21, "poldisc", x=12, xpre=0, scen=2)

xhyp21
yhyp21 <-logitsimfd(xhyp21, simbetas, ci=0.90) #use the 90 CI same as in original study
yhyp21


## TABLES 

modellabels8<-c("Societal Discrimination", "Political Discrimination",
                "Worship Attendance","Participation in Social Networks", "Political Interest", "Political Knowledge", 
                "Party ID (Yes=1)", "Close to British ID", "English (Main Lang)", "Native Born",
                "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
                "Black Caribbean", "Indian", "Pakistani", "Bangladeshi",
                "Vote Duty", "Political Efficacy","Democratic Satisfaction","Trust Parliament", 
                "National economic future", "Use of Internet"
)

dvlabel7<-c("Support for violent demonstrations")
dvlabel8<-c("Non-electoral political participation")

stargazer(mod_sum16, mod_sum18, mod_sum20, 
          style="APSR", 
          covariate.labels = modellabels8, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Alternative outcomes (Specific Measures) - Support for Violend Demonstrations",
          dep.var.labels = dvlabel7
)

stargazer(mod_sum17, mod_sum19, mod_sum21, 
          style="APSR", 
          covariate.labels = modellabels8, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Alternative outcomes (Specific Measures) - Non-electoral Political Participation",
          dep.var.labels = dvlabel8
)


## GRAPHS

trace22 <- ropeladder(
  x=yhyp16$pe,
  lower=yhyp16$lower,
  upper=yhyp16$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=1)

trace23 <- ropeladder(
  x=yhyp18$pe,
  lower=yhyp18$lower,
  upper=yhyp18$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=2)


trace24 <- ropeladder(
  x=yhyp20$pe,
  lower=yhyp20$lower,
  upper=yhyp20$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=3)


vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:3
)

# Widen space between titles on left
trace22$entryheight <- .12
trace23$entryheight <- .12
trace24$entryheight <- .12

at.x1 <- seq(-.2, .5,.1)
at.x2 <- seq(-.2, .5,.1)
at.x3 <- seq(-.2, .5,.1)



tc_5<-tile(trace22, trace23, trace24,
          vertmark,
          RxC=c(3,1),
          xaxis=list(at1 = at.x1, 
                     at2= at.x2, 
                     at3= at.x3,
                     cex=.8),
          topaxis=list(at1 = at.x1, 
                       at2= at.x2, 
                       at3= at.x3,
                       cex=.8,
                       add = rep(TRUE,3)),
          undertitle=list(labels1="Change in Predicted Probability (Min-Max)",
                            labels2="Change in Predicted Probability (Min-Max)",
                            labels3="Change in Predicted Probability (Min-Max)",
                            cex=.7),
          topaxistitle = list(labels=c("Support for Violent Demonstrations: Model 1", "Support for Violent Demonstrations: Model 2", "Support for Violent Demonstrations: Model 3"), cex=0.8),
          output = list(file = "rep_fig5", type="pdf", width=10), 
          frame=FALSE,
          gridlines=list(type="t", col="grey65")
)


trace25 <- ropeladder(
  x=yhyp17$pe,
  lower=yhyp17$lower,
  upper=yhyp17$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=1)

trace26 <- ropeladder(
  x=yhyp19$pe,
  lower=yhyp19$lower,
  upper=yhyp19$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=2)

trace27 <- ropeladder(
  x=yhyp21$pe,
  lower=yhyp21$lower,
  upper=yhyp21$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=3)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:3
)

trace25$entryheight <- .12
trace26$entryheight <- .12
trace27$entryheight <- .12

at.x1 <- seq(-.1, .4,.1)
at.x2 <- seq(-.1, .4,.1)
at.x3 <- seq(-.1, .4,.1)

tc_6<-tile(trace25, trace26, trace27,
          vertmark,
          RxC=c(3,1),
          xaxis=list(at1 = at.x1, 
                     at2= at.x2, 
                     at3= at.x3,
                     cex=.8),
          topaxis=list(at1 = at.x1, 
                       at2= at.x2, 
                       at3= at.x3,
                       cex=.8,
                       add = rep(TRUE,3)),
          topaxistitle = list(labels=c("Non-electoral Political Participation: Model 1", "Non-electoral Political Participation: Model 2", "Non-electoral Political Participation: Model 3"),
                              cex=0.8),
          undertitle = list(labels1="Change in Predicted Probability (Min-Max)",
                            labels2="Change in Predicted Probability (Min-Max)",
                            labels3="Change in Predicted Probability (Min-Max)",
                            cex=.7, x=.5),
          output = list(file = "rep_fig6", type="pdf", width=10), 
          frame=FALSE,
          gridlines=list(type="t", col="grey65")
)

############################################################################
################## REPRODUCTION OF ORIGINAL RESULTS ########################
############ ADAPTED TO VARIABLES AVAILABLE IN THE SCIP DATASET ############
########################### for comparison #################################
############################################################################


## ETHNIC BASED ENGAGEMENT - ADAPTED MODEL TO VARIABLES AVAILABLE IN SCIP
## NARROW MODEL WITH EXCLUDED PARTY ID AND NATIVE BORN

formula<-(ethnic_active~ socdisc + poldisc +
            relatt_oth_r + pol_interest+ polknowledge + identity+ english+
            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi)


mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

ethnic_active_adapted<- glm(formula, family=binomial(link=logit), data=mdata)
summary(ethnic_active_adapted)

# add predicted coef. 

eaa.coef <- ethnic_active_adapted$coefficients  # point estimates
vc.eaa <- vcov(ethnic_active_adapted)

# add predicted coef. 

sims <- 10000
simbetas <- mvrnorm(sims, eaa.coef, vc.eaa)

xhyp.eaa <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp.eaa <- cfName(xhyp.eaa, "socdisc", scen=1) 
xhyp.eaa <-cfChange(xhyp.eaa, "socdisc", x=9, xpre=0, scen=1)

xhyp.eaa <- cfName(xhyp.eaa, "poldisc", scen=2) 
xhyp.eaa <-cfChange(xhyp.eaa, "poldisc", x=12, xpre=0, scen=2)

xhyp.eaa
xhyp.eaa <- logitsimfd(xhyp.eaa, simbetas, ci=0.90)
xhyp.eaa

## TABLE: ETHNIC BASED ENGAGEMENT (SPECIFIC MEASURES) - ADAPTED MODEL

modellabels5<-c("Societal Discrimination", "Political Discrimination",
                "Worship Attendance", "Political Interest", "Political Knowledge", 
                "Close to British ID", "English (Main Lang)",
                "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
                "Black Caribbean", "Indian", "Pakistani", "Bangladeshi"
                )

dvlabel3<-c("Ethnic-Based Participation")

stargazer(ethnic_active_adapted,
          style="APSR", 
          covariate.labels = modellabels5, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Ethnic Based Engagement (Specific Measures) - Adapted Model",
          dep.var.labels = dvlabel3)


## GRAPH: ETHNIC BASED ENGAGEMENT - ADAPTED MODEL

trace.eaa <- ropeladder(
  x=xhyp.eaa$pe,
  lower=xhyp.eaa$lower,
  upper=xhyp.eaa$upper,
  labels=c("Societal Disc.", "Political Disc."), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=1)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:1
)

trace.eaa$entryheight <- .12

at.x1 <- seq(-.1, .4,.1)


tc_eaa<-tile(trace.eaa,
          vertmark,
          xaxis=list(at1 = at.x1,
                     cex=.8),
          topaxis=list(at1 = at.x1,
                       cex=.8,
                       add = rep(TRUE,1)),
          topaxistitle = list(labels=c("Ethnic Based Engagement (Specific Measures) - Adapted Model"), cex=1),
          undertitle = list(labels1="Change in Predicted Probability (Min-Max)", cex=.7, x=.5),
          output = list(file = "rep_fig8", type="pdf", width=10), 
          frame=FALSE,
          gridlines=list(type="t", col="grey65")
)


## IDENTITY CHOICE - ADAPTED MODEL TO VARIABLES AVAILABLE IN SCIP
## NARROW MODEL WITH EXCLUDED PARTY ID AND NATIVE BORN

formula<- (identity ~ socdisc + poldisc +
             relatt_oth_r + pol_interest+ polknowledge + english+
             female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi)

mdata <- extractdata(formula, data, na.rm=TRUE) 

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mdata$identity <- factor(mdata$identity, 
                         levels= c(0, 1, 2),
                         labels=c("BlackAsian", "Both", "British"))


identity_adapted <- multinom(identity ~ socdisc + poldisc +
                            relatt_oth_r + pol_interest+ polknowledge + english+
                            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ 
                            bangladeshi, Hess = TRUE, data=mdata)

summary(identity_adapted)

## TABLE 

modellabels5<-c("Societal Discrimination", "Political Discrimination",
                "Worship Attendance", "Political Interest", "Political Knowledge", 
                "English (Main Lang)",
                "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
                "Black Caribbean", "Indian", "Pakistani", "Bangladeshi"
)

dvlabel4<-c("Identity Choice")

stargazer(identity_adapted,
          style="APSR", 
          covariate.labels = modellabels5, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Identity Choice (Specific Measures) - Adapted Model",
          dep.var.labels = dvlabel4)

## predicted probabilities

id_ad_soc <- mnl_fd2_ova(
  model = identity_adapted,
  data = mdata,
  x = 'socdisc',
  value1 = min(mdata$socdisc),
  value2 = max(mdata$socdisc),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

id_ad_pol <- mnl_fd2_ova(
  model = identity_adapted,
  data = mdata,
  x = 'poldisc',
  value1 = min(mdata$poldisc),
  value2 = max(mdata$poldisc),
  nsim = 10000,
  seed = 1234,
  probs = c(0.050, 0.950)
)

id_ad_soc
id_ad_pol

#Identifying Asian/Black relative to British 
yhype.ba.p <- c(0.22653086, 0.2688663) 
yhype.ba.l<- c(0.1045293, 0.1054963)
yhype.ba.u<- c(0.34662200, 0.42219076)

#Identifying Both Relative to Asian/Black
yhype.bt.p <- c(-0.20649505, -0.1288331) 
yhype.bt.l<- c(-0.3109604, -0.2773251)
yhype.bt.u<- c(-0.08694266, 0.03327125)

#GRAPH 

trace_id_1 <- ropeladder(
  x=yhype.ba.p,
  lower=yhype.ba.l,
  upper=yhype.ba.u,
  labels=c("Societal Discrimination", "Political Discrimination"), 
  col="Black", 
  pch=c(5,16),
  cex=1.8, 
  plot=1)

trace_id_2 <- ropeladder(
  x=yhype.bt.p,
  lower=yhype.bt.l,
  upper=yhype.bt.u,
  col="Black",
  pch=c(5,16),
  cex=1.8,
  plot=2)

vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:2)

trace_id_1$entryheight <- .20
trace_id_2$entryheight <- .20

at.x1 <- seq(-.1,.5,.1)
at.x2 <-seq(-.4,.1,.1)

tc_id<-tile(trace_id_1,
         trace_id_2,
         vertmark,
         xaxis=list(at1 = at.x1, 
                    at2= at.x2, 
                    cex=.8), 
         topaxis=list(at1 = at.x1, 
                      at2= at.x2, 
                      cex=.8,
                      add = rep(TRUE,2)),
         undertitle = list(labels="Change in Predicted Probability (Min-Max)", cex=.8, x=1),
         plottitle= list(labels1= "Black/Asian vs. British", 
                         labels2= "Equally Both vs. Black/Asian"),
         output = list(file = "rep_fig9", width=12, type="pdf"), 
         frame=FALSE,
         gridlines=list(type="x", col="grey65")
)


