# Discrimination that Matters: Replication with extensions of 
# Perceived Discrimination and Political Behavior (2020)

# Replicated by Andrej Cvetic (cvetica@tcd.ie)
# Dept. of Political Science, Trinity College Dublin
# Original paper authored by Kassra A.R. Oskooii (oskooiik@udel.edu)

# Code was last run and checked on 27/6/2024

# Software specifications reminder:

# Version of R software: 4.2.0 "Vigorous Calisthenics"
# Important for simcf and tile package which were not updated to work with 
# R versions above 4.2.0 when the code for the replciation was created, 
# but by the time the replication was done, Chris Adolph 
# (University of Washington) updated the packages, so they are able to work with
# R 4.3.2.
# R studio: RStudio-2022.02.3-492

# In this script you will find all the code to reproduce tables and graphs
# from the paper document of Discrimination that Matters: 
# Replication with extensions of 
# Perceived Discrimination and Political Behavior (2020)
# 


# Before the start clean the environment: 

rm(list=ls())
gc()

# Check if missing, install and load the packages: 

install_and_load_packages <- function(packages, repos = "https://cran.rstudio.com/") {
  # Check for missing packages and install them
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(missing_packages)) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    tryCatch(
      install.packages(missing_packages, repos = repos),
      error = function(e) { 
        message("Error installing packages: ", e$message) 
      }
    )
  }
  
  # Load all required packages
  lapply(packages, function(pkg) {
    tryCatch(
      library(pkg, character.only = TRUE),
      error = function(e) { 
        message("Error loading package: ", pkg, "\n", e$message) 
      }
    )
  })
}

# All packages are here: 
required_packages <- c("MASS", "nlme", "mgcv", "RColorBrewer", "stargazer", "psych", "dplyr", 
                       "nnet", "MNLpred", "ggplot2", "rlang", "ggpubr", "Hmisc",
                       "MatchIt", "marginaleffects", "rlang", "car", "broom", "effects",
                       "tictoc"
)
install_and_load_packages(required_packages)

# As recommended, please download the following packages from links provided and load them
# as local source packages. 
library(simcf) #Download Package: https://faculty.washington.edu/cadolph/index.php?page=60
library(tile) #Download Package: https://faculty.washington.edu/cadolph/index.php?page=60


# Provide the dataframe of the packages and their versions

info <- sessionInfo()
loaded_pkgs <- info$otherPkgs
loaded_pkgs <- lapply(loaded_pkgs, function(pkg) pkg$Version)
loaded_pkgs_df <- data.frame(package = names(loaded_pkgs), version = unlist(loaded_pkgs))
print("Extracted package versions:")
print(loaded_pkgs_df)

# Recommended method is to download the dataset from 
# 02_working_dataset on GitHub and load from your computer
# Set up the working directory setwd() to ease the process. 

########################
### FIGURE 1 (P. 10) ###
########################

tic()

data<-haven::read_dta("EMBES_REPLICATION_DATASET.dta")

tab.poldisc <- table(data$poldisc)
tab.socdisc <- table(data$socdisc)
tab.poldisc
tab.socdisc

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

###################################
### FIGURE 2, panel (a) (P. 15) ###
###################################

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

## GRAPH: GENERAL ELECTIONS 

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
          output = list(file = "fig2_pa", type="pdf", width=10), 
          frame=FALSE,
          gridlines=list(type="t", col="grey65")
)

###################################
### FIGURE 2, panel (b) (P. 15) ###
###################################

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

## GRAPH: LOCAL ELECTIONS

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
          output = list(file = "fig2_pb", type="pdf", width=10), 
          frame=FALSE,
          gridlines=list(type="t", col="grey65")
)

###################################
### FIGURE 3, panel (a) (P. 16) ###
###################################

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


# GRAPH: ETHNIC BASED ENGAGEMENT 

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
          output = list(file = "fig3_pa", type="pdf", width=10), 
          frame=FALSE,
          gridlines=list(type="t", col="grey65")
)


###################################
### FIGURE 3, panel (b) (P. 16) ###
###################################

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
           output = list(file = "fig3_pb", width=10, type="pdf"), 
           frame=FALSE,
           gridlines=list(type="x", col="grey65")
)

###################################
########## TABLE 1 (P. 17) ########
###################################

formula<-(voted2010 ~ 
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

mdata$socdisc_b <- ifelse(mdata$socdisc > 1, 1, 0)
mdata$poldisc_b <- ifelse(mdata$poldisc > 1, 1, 0)
table(mdata$socdisc_b)
table(mdata$poldisc_b)

#######################
### SOCIETAL DISC. ###
######################

ndata <- mdata %>%
  select(voted2010, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, vote_duty, efficacy, democ_satis, trust_parliament) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

### CHECK INITIAL IMBALANCE:

m.out.iib <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                       vote_duty+ efficacy+ democ_satis+ trust_parliament,
                     data = ndata, method = NULL, distance = "glm")
m.out.iib
summary(m.out.iib)

### MATCHING DATASET TO BE USED: full matching with probit link
m.out.fp <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

### OTHER TYPES OF MATCHING JUST FOR COMPARISON

m.out.fl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

### SAVE THE MATCHED DATASETS 
mat.data1<-match.data(m.out.fp)

head(mat.data1)

mat.data2<-match.data(m.out.fl)

head(mat.data2)

mat.data3<-match.data(m.out.np)

head(mat.data3)

mat.data4<-match.data(m.out.nl)

head(mat.data4)

mat.data5<-match.data(m.out.op)

head(mat.data5)

mat.data6<-match.data(m.out.ol)

head(mat.data6)


### RE-ESTIMATE THE MODEL FOR THE TREATED 

model1.1 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                       vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data1, weights=weights, 
                family=quasibinomial())
summary(model1.1)

model1.2 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                       vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data2, weights=weights, 
                family=quasibinomial())
summary(model1.2)

model1.3 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                       vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data3, weights=weights, 
                family=quasibinomial())
summary(model1.3)

model1.4 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                       vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data4, weights=weights, 
                family=quasibinomial())
summary(model1.4)

model1.5 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                       vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data5, weights=weights, 
                family=quasibinomial())
summary(model1.5)

model1.6 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                       vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data6, weights=weights, 
                family=quasibinomial())
summary(model1.6)


### ESTIMATE ATT OF SOCIETAL DISCRIMINATION - expressed as Risk Ratio 

att1.1.rr<-avg_comparisons(model1.1,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att1.1.rr)

att1.2.rr<-avg_comparisons(model1.2,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att1.2.rr)

att1.3.rr<-avg_comparisons(model1.3,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att1.3.rr)


att1.4.rr<-avg_comparisons(model1.4,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att1.4.rr)

att1.5.rr<-avg_comparisons(model1.5,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att1.5.rr)

att1.6.rr<-avg_comparisons(model1.6,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att1.6.rr)

### Vote in General Elections (Societal Discrimination): ATT Estimates as Risk Ratios - Complete Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att1.1.rr$estimate, att1.2.rr$estimate, att1.3.rr$estimate, 
          att1.4.rr$estimate, att1.5.rr$estimate, att1.6.rr$estimate) 
col2 <- c(att1.1.rr$p.value, att1.2.rr$p.value, att1.3.rr$p.value, 
          att1.4.rr$p.value, att1.5.rr$p.value, att1.6.rr$p.value)
col3 <- c(att1.1.rr$conf.low, att1.2.rr$conf.low, att1.3.rr$conf.low, 
          att1.4.rr$conf.low, att1.5.rr$conf.low, att1.6.rr$conf.low)
col4 <- c(att1.1.rr$conf.high, att1.2.rr$conf.high, att1.3.rr$conf.high, 
          att1.4.rr$conf.high, att1.5.rr$conf.high, att1.6.rr$conf.high)

table1.1.rr<-cbind(col0, col1, col2, col3, col4)  
#rownames(table1.1.rr) <- c("Full Probit", "Full Logit", 
#                        "Nearest Probit", "Nearest Logit", 
#                       "Optimal Probit", "Optimal Pobit")
table1.1.rr<-as.data.frame(table1.1.rr) 
table1.1.rr <- table1.1.rr %>%
  mutate(across(-col0, as.numeric))
table1.1.rr <- table1.1.rr %>% mutate(across(-col0, ~ round(., 6)))
#table1.1.rr <- as.data.frame(lapply(table1.1.rr, function(x) round(x, 3)))
colnames(table1.1.rr)[1] <- "Model"
colnames(table1.1.rr)[2] <- "Estimate"
colnames(table1.1.rr)[3] <- "P-value"
colnames(table1.1.rr)[4] <- "CI Lower"
colnames(table1.1.rr)[5] <- "CI Upper"
print(table1.1.rr)


stargazer(table1.1.rr, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in General Elections (Societal Discrimination): ATT Estimates as Risk Ratios - Complete Model"
)

###################################
########## TABLE 2 (P. 18) ########
###################################

ndata <- mdata %>%
  select(voted2010, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, vote_duty, efficacy, democ_satis, trust_parliament) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

### SAVE THE MATCHED DATASET 
mat.data1<-match.data(m.out.fp)

head(mat.data1)

mat.data2<-match.data(m.out.fl)

head(mat.data2)

mat.data3<-match.data(m.out.np)

head(mat.data3)

mat.data4<-match.data(m.out.nl)

head(mat.data4)

mat.data5<-match.data(m.out.op)

head(mat.data5)

mat.data6<-match.data(m.out.ol)

head(mat.data6)


### RE-ESTIMATE THE MODEL FOR THE TREATED 

model2.1 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                       vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data1, weights=weights, 
                family=quasibinomial())
summary(model2.1)

model2.2 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                       vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data2, weights=weights, 
                family=quasibinomial())
summary(model2.2)

model2.3 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                       vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data3, weights=weights, 
                family=quasibinomial())
summary(model2.3)

model2.4 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                       vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data4, weights=weights, 
                family=quasibinomial())
summary(model2.4)

model2.5 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                       vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data5, weights=weights, 
                family=quasibinomial())
summary(model2.5)

model2.6 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                       vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data6, weights=weights, 
                family=quasibinomial())
summary(model2.6)


### ESTIMATE ATT OF SOCIETAL DISCRIMINATION

att2.1.rr<-avg_comparisons(model2.1,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att2.1.rr)

att2.2.rr<-avg_comparisons(model2.2,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att2.2.rr)

att2.3.rr<-avg_comparisons(model2.3,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att2.3.rr)

att2.4.rr<-avg_comparisons(model2.4,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att2.4.rr)

att2.5.rr<-avg_comparisons(model2.5,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att2.5.rr)

att2.6.rr<-avg_comparisons(model2.6,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att2.6.rr)

### Vote in General Elections (Political Discrimination): ATT Estimates as Risk Ratios - Complete Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att2.1.rr$estimate, att2.2.rr$estimate, att2.3.rr$estimate, 
          att2.4.rr$estimate, att2.5.rr$estimate, att2.6.rr$estimate) 
col2 <- c(att2.1.rr$p.value, att2.2.rr$p.value, att2.3.rr$p.value, 
          att2.4.rr$p.value, att2.5.rr$p.value, att2.6.rr$p.value)
col3 <- c(att2.1.rr$conf.low, att2.2.rr$conf.low, att2.3.rr$conf.low, 
          att2.4.rr$conf.low, att2.5.rr$conf.low, att2.6.rr$conf.low)
col4 <- c(att2.1.rr$conf.high, att2.2.rr$conf.high, att2.3.rr$conf.high, 
          att2.4.rr$conf.high, att2.5.rr$conf.high, att2.6.rr$conf.high)

table2.1.rr<-cbind(col0, col1, col2, col3, col4)  
#rownames(table1.1.rr) <- c("Full Probit", "Full Logit", 
#                        "Nearest Probit", "Nearest Logit", 
#                       "Optimal Probit", "Optimal Pobit")
table2.1.rr<-as.data.frame(table2.1.rr)
table2.1.rr <- table2.1.rr %>%
  mutate(across(-col0, as.numeric))
table2.1.rr <- table2.1.rr %>% mutate(across(-col0, ~ round(., 6)))
colnames(table2.1.rr)[1] <- "Model"
colnames(table2.1.rr)[2] <- "Estimate"
colnames(table2.1.rr)[3] <- "P-value"
colnames(table2.1.rr)[4] <- "CI Lower"
colnames(table2.1.rr)[5] <- "CI Upper"
print(table2.1.rr)


stargazer(table2.1.rr, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in General Elections (Political Discrimination): ATT Estimates as Risk Ratios - Complete Model"
)

###################################
########## TABLE 3 (P. 18) ########
###################################

data<-haven::read_dta("EMBES_REPLICATION_DATASET.dta")

formula<-(voted2010_local ~ 
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

mdata$socdisc_b <- ifelse(mdata$socdisc > 1, 1, 0)
mdata$poldisc_b <- ifelse(mdata$poldisc > 1, 1, 0)
table(mdata$socdisc_b)
table(mdata$poldisc_b)

#######################
### SOCIETAL DISC. ###
######################

ndata <- mdata %>%
  select(voted2010_local, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, vote_duty, efficacy, democ_satis, trust_parliament) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

### SAVE THE MATCHED DATASET 
mat.data1<-match.data(m.out.fp)

head(mat.data1)

mat.data2<-match.data(m.out.fl)

head(mat.data2)

mat.data3<-match.data(m.out.np)

head(mat.data3)

mat.data4<-match.data(m.out.nl)

head(mat.data4)

mat.data5<-match.data(m.out.op)

head(mat.data5)

mat.data6<-match.data(m.out.ol)

head(mat.data6)


### RE-ESTIMATE THE MODEL FOR THE TREATED 

model5.1 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                             vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data1, weights=weights, 
                family=quasibinomial())
summary(model5.1)

model5.2 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                             vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data2, weights=weights, 
                family=quasibinomial())
summary(model5.2)

model5.3 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                             vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data3, weights=weights, 
                family=quasibinomial())
summary(model5.3)

model5.4 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                             vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data4, weights=weights, 
                family=quasibinomial())
summary(model5.4)

model5.5 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                             vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data5, weights=weights, 
                family=quasibinomial())
summary(model5.5)

model5.6 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                             vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data6, weights=weights, 
                family=quasibinomial())
summary(model5.6)

### ESTIMATE ATT OF SOCIETAL DISCRIMINATION

att5.1.rr<-avg_comparisons(model5.1,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att5.1.rr)

att5.2.rr<-avg_comparisons(model5.2,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att5.2.rr)

att5.3.rr<-avg_comparisons(model5.3,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att5.3.rr)

att5.4.rr<-avg_comparisons(model5.4,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att5.4.rr)

att5.5.rr<-avg_comparisons(model5.5,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att5.5.rr)

att5.6.rr<-avg_comparisons(model5.6,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att5.6.rr)

### Vote in Local Elections (Societal Discrimination): ATT Estimates as Risk Ratios - Complete Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att5.1.rr$estimate, att5.2.rr$estimate, att5.3.rr$estimate, 
          att5.4.rr$estimate, att5.5.rr$estimate, att5.6.rr$estimate) 
col2 <- c(att5.1.rr$p.value, att5.2.rr$p.value, att5.3.rr$p.value, 
          att5.4.rr$p.value, att5.5.rr$p.value, att5.6.rr$p.value)
col3 <- c(att5.1.rr$conf.low, att5.2.rr$conf.low, att5.3.rr$conf.low, 
          att5.4.rr$conf.low, att5.5.rr$conf.low, att5.6.rr$conf.low)
col4 <- c(att5.1.rr$conf.high, att5.2.rr$conf.high, att5.3.rr$conf.high, 
          att5.4.rr$conf.high, att5.5.rr$conf.high, att5.6.rr$conf.high)

table5.1.rr<-cbind(col0, col1, col2, col3, col4)  
#rownames(table1.1.rr) <- c("Full Probit", "Full Logit", 
#                        "Nearest Probit", "Nearest Logit", 
#                       "Optimal Probit", "Optimal Pobit")
table5.1.rr<-as.data.frame(table5.1.rr)
table5.1.rr <- table5.1.rr %>%
  mutate(across(-col0, as.numeric))
table5.1.rr <- table5.1.rr %>% mutate(across(-col0, ~ round(., 6)))
colnames(table5.1.rr)[1] <- "Model"
colnames(table5.1.rr)[2] <- "Estimate"
colnames(table5.1.rr)[3] <- "P-value"
colnames(table5.1.rr)[4] <- "CI Lower"
colnames(table5.1.rr)[5] <- "CI Upper"
print(table5.1.rr)

stargazer(table5.1.rr, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in Local Elections (Societal Discrimination): ATT Estimates as Risk Ratios - Complete Model"
)

###################################
########## TABLE 4 (P. 18) ########
###################################

ndata <- mdata %>%
  select(voted2010_local, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, vote_duty, efficacy, democ_satis, trust_parliament) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

### SAVE THE MATCHED DATASET 
mat.data1<-match.data(m.out.fp)

head(mat.data1)

mat.data2<-match.data(m.out.fl)

head(mat.data2)

mat.data3<-match.data(m.out.np)

head(mat.data3)

mat.data4<-match.data(m.out.nl)

head(mat.data4)

mat.data5<-match.data(m.out.op)

head(mat.data5)

mat.data6<-match.data(m.out.ol)

head(mat.data6)

### RE-ESTIMATE THE MODEL FOR THE TREATED 

model6.1 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                             vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data1, weights=weights, 
                family=quasibinomial())
summary(model6.1)

model6.2 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                             vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data2, weights=weights, 
                family=quasibinomial())
summary(model6.2)

model6.3 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                             vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data3, weights=weights, 
                family=quasibinomial())
summary(model6.3)

model6.4 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                             vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data4, weights=weights, 
                family=quasibinomial())
summary(model6.4)

model6.5 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                             vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data5, weights=weights, 
                family=quasibinomial())
summary(model6.5)

model6.6 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                             vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data6, weights=weights, 
                family=quasibinomial())
summary(model6.6)

### ESTIMATE ATT OF POLITICAL DISCRIMINATION

att6.1.rr<-avg_comparisons(model6.1,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att6.1.rr)

att6.2.rr<-avg_comparisons(model6.2,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att6.2.rr)

att6.3.rr<-avg_comparisons(model6.3,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att6.3.rr)

att6.4.rr<-avg_comparisons(model6.4,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att6.4.rr)

att6.5.rr<-avg_comparisons(model6.5,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att6.5.rr)

att6.6.rr<-avg_comparisons(model6.6,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att6.6.rr)

### Vote in Local Elections (Political Discrimination): ATT Estimates as Risk Ratios - Complete Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att6.1.rr$estimate, att6.2.rr$estimate, att6.3.rr$estimate, 
          att6.4.rr$estimate, att6.5.rr$estimate, att6.6.rr$estimate) 
col2 <- c(att6.1.rr$p.value, att6.2.rr$p.value, att6.3.rr$p.value, 
          att6.4.rr$p.value, att6.5.rr$p.value, att6.6.rr$p.value)
col3 <- c(att6.1.rr$conf.low, att6.2.rr$conf.low, att6.3.rr$conf.low, 
          att6.4.rr$conf.low, att6.5.rr$conf.low, att6.6.rr$conf.low)
col4 <- c(att6.1.rr$conf.high, att6.2.rr$conf.high, att6.3.rr$conf.high, 
          att6.4.rr$conf.high, att6.5.rr$conf.high, att6.6.rr$conf.high)

table6.1.rr<-cbind(col0, col1, col2, col3, col4)  
#rownames(table1.1.rr) <- c("Full Probit", "Full Logit", 
#                        "Nearest Probit", "Nearest Logit", 
#                       "Optimal Probit", "Optimal Pobit")
table6.1.rr<-as.data.frame(table6.1.rr)
table6.1.rr <- table6.1.rr %>%
  mutate(across(-col0, as.numeric))
table6.1.rr <- table6.1.rr %>% mutate(across(-col0, ~ round(., 6)))
colnames(table6.1.rr)[1] <- "Model"
colnames(table6.1.rr)[2] <- "Estimate"
colnames(table6.1.rr)[3] <- "P-value"
colnames(table6.1.rr)[4] <- "CI Lower"
colnames(table6.1.rr)[5] <- "CI Upper"
print(table6.1.rr)


stargazer(table6.1.rr, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in Local Elections (Political Discrimination): ATT Estimates as Risk Ratios - Complete Model"
)

###################################
########## TABLE 5 (P. 19) ########
###################################

data<-haven::read_dta("EMBES_REPLICATION_DATASET.dta")

formula<-(ethnic_active ~ 
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

mdata$socdisc_b <- ifelse(mdata$socdisc > 1, 1, 0)
mdata$poldisc_b <- ifelse(mdata$poldisc > 1, 1, 0)
table(mdata$socdisc_b)
table(mdata$poldisc_b)

#######################
### SOCIETAL DISC. ###
######################

ndata <- mdata %>%
  select(ethnic_active, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, vote_duty, efficacy, democ_satis, trust_parliament) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

### MATCHING DATASET TO BE USED: full matching with probit link
m.out.fp <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

### OTHER TYPES OF MATCHING JUST FOR COMPARISON

m.out.fl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

### SAVE THE MATCHED DATASET 
mat.data1<-match.data(m.out.fp)

head(mat.data1)

mat.data2<-match.data(m.out.fl)

head(mat.data2)

mat.data3<-match.data(m.out.np)

head(mat.data3)

mat.data4<-match.data(m.out.nl)

head(mat.data4)

mat.data5<-match.data(m.out.op)

head(mat.data5)

mat.data6<-match.data(m.out.ol)

head(mat.data6)


### RE-ESTIMATE THE MODEL FOR THE TREATED 

model9.1 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                           identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                           medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                           vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data1, weights=weights, 
                family=quasibinomial())
summary(model9.1)

model9.2 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                           identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                           medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                           vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data2, weights=weights, 
                family=quasibinomial())
summary(model9.2)

model9.3 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                           identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                           medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                           vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data3, weights=weights, 
                family=quasibinomial())
summary(model9.3)

model9.4 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                           identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                           medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                           vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data4, weights=weights, 
                family=quasibinomial())
summary(model9.4)

model9.5 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                           identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                           medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                           vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data5, weights=weights, 
                family=quasibinomial())
summary(model9.5)

model9.6 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                           identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                           medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                           vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                data=mat.data6, weights=weights, 
                family=quasibinomial())
summary(model9.6)

### ESTIMATE ATT OF SOCIETAL DISCRIMINATION

att9.1.rr<-avg_comparisons(model9.1,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att9.1.rr)

att9.2.rr<-avg_comparisons(model9.2,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att9.2.rr)

att9.3.rr<-avg_comparisons(model9.3,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att9.3.rr)

att9.4.rr<-avg_comparisons(model9.4,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att9.4.rr)

att9.5.rr<-avg_comparisons(model9.5,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att9.5.rr)

att9.6.rr<-avg_comparisons(model9.6,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att9.6.rr)

### Ethnic based engagement (Societal Discrimination): ATT Estimates as Risk Ratios - Complete Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att9.1.rr$estimate, att9.2.rr$estimate, att9.3.rr$estimate, 
          att9.4.rr$estimate, att9.5.rr$estimate, att9.6.rr$estimate) 
col2 <- c(att9.1.rr$p.value, att9.2.rr$p.value, att9.3.rr$p.value, 
          att9.4.rr$p.value, att9.5.rr$p.value, att9.6.rr$p.value)
col3 <- c(att9.1.rr$conf.low, att9.2.rr$conf.low, att9.3.rr$conf.low, 
          att9.4.rr$conf.low, att9.5.rr$conf.low, att9.6.rr$conf.low)
col4 <- c(att9.1.rr$conf.high, att9.2.rr$conf.high, att9.3.rr$conf.high, 
          att9.4.rr$conf.high, att9.5.rr$conf.high, att9.6.rr$conf.high)

table9.1.rr<-cbind(col0, col1, col2, col3, col4)  
#rownames(table1.1.rr) <- c("Full Probit", "Full Logit", 
#                        "Nearest Probit", "Nearest Logit", 
#                       "Optimal Probit", "Optimal Pobit")
table9.1.rr<-as.data.frame(table9.1.rr)
table9.1.rr <- table9.1.rr %>%
  mutate(across(-col0, as.numeric))
table9.1.rr <- table9.1.rr %>% mutate(across(-col0, ~ round(., 6)))
colnames(table9.1.rr)[1] <- "Model"
colnames(table9.1.rr)[2] <- "Estimate"
colnames(table9.1.rr)[3] <- "P-value"
colnames(table9.1.rr)[4] <- "CI Lower"
colnames(table9.1.rr)[5] <- "CI Upper"
print(table9.1.rr)


stargazer(table9.1.rr, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Ethnic based engagement (Societal Discrimination): ATT Estimates as Risk Ratios - Complete Model"
)

###################################
########## TABLE 6 (P. 19) ########
###################################

ndata <- mdata %>%
  select(ethnic_active, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, vote_duty, efficacy, democ_satis, trust_parliament) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

### SAVE THE MATCHED DATASET 
mat.data1<-match.data(m.out.fp)

head(mat.data1)

mat.data2<-match.data(m.out.fl)

head(mat.data2)

mat.data3<-match.data(m.out.np)

head(mat.data3)

mat.data4<-match.data(m.out.nl)

head(mat.data4)

mat.data5<-match.data(m.out.op)

head(mat.data5)

mat.data6<-match.data(m.out.ol)

head(mat.data6)

### RE-ESTIMATE THE MODEL FOR THE TREATED 

model10.1 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                            vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                 data=mat.data1, weights=weights, 
                 family=quasibinomial())
summary(model10.1)

model10.2 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                            vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                 data=mat.data2, weights=weights, 
                 family=quasibinomial())
summary(model10.2)

model10.3 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                            vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                 data=mat.data3, weights=weights, 
                 family=quasibinomial())
summary(model10.3)

model10.4 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                            vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                 data=mat.data4, weights=weights, 
                 family=quasibinomial())
summary(model10.4)

model10.5 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                            vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                 data=mat.data5, weights=weights, 
                 family=quasibinomial())
summary(model10.5)

model10.6 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                                            vote_duty+ efficacy+ democ_satis+ trust_parliament), 
                 data=mat.data6, weights=weights, 
                 family=quasibinomial())
summary(model10.6)

### ESTIMATE ATT OF SOCIETAL DISCRIMINATION

att10.1.rr<-avg_comparisons(model10.1,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data1, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att10.1.rr)

att10.2.rr<-avg_comparisons(model10.2,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data2, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att10.2.rr)

att10.3.rr<-avg_comparisons(model10.3,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data3, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att10.3.rr)

att10.4.rr<-avg_comparisons(model10.4,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data4, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att10.4.rr)

att10.5.rr<-avg_comparisons(model10.5,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data5, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att10.5.rr)

att10.6.rr<-avg_comparisons(model10.6,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data6, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att10.6.rr)

### Ethnic based engagement (Political Discrimination): ATT Estimates as Risk Ratios - Complete Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att10.1.rr$estimate, att10.2.rr$estimate, att10.3.rr$estimate, 
          att10.4.rr$estimate, att10.5.rr$estimate, att10.6.rr$estimate) 
col2 <- c(att10.1.rr$p.value, att10.2.rr$p.value, att10.3.rr$p.value, 
          att10.4.rr$p.value, att10.5.rr$p.value, att10.6.rr$p.value)
col3 <- c(att10.1.rr$conf.low, att10.2.rr$conf.low, att10.3.rr$conf.low, 
          att10.4.rr$conf.low, att10.5.rr$conf.low, att10.6.rr$conf.low)
col4 <- c(att10.1.rr$conf.high, att10.2.rr$conf.high, att10.3.rr$conf.high, 
          att10.4.rr$conf.high, att10.5.rr$conf.high, att10.6.rr$conf.high)

table10.1.rr<-cbind(col0, col1, col2, col3, col4)  
#rownames(table1.1.rr) <- c("Full Probit", "Full Logit", 
#                        "Nearest Probit", "Nearest Logit", 
#                       "Optimal Probit", "Optimal Pobit")
table10.1.rr<-as.data.frame(table10.1.rr)
table10.1.rr <- table10.1.rr %>%
  mutate(across(-col0, as.numeric))
table10.1.rr <- table10.1.rr %>% mutate(across(-col0, ~ round(., 6)))
colnames(table10.1.rr)[1] <- "Model"
colnames(table10.1.rr)[2] <- "Estimate"
colnames(table10.1.rr)[3] <- "P-value"
colnames(table10.1.rr)[4] <- "CI Lower"
colnames(table10.1.rr)[5] <- "CI Upper"
print(table10.1.rr)


stargazer(table10.1.rr, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Ethnic based engagement (Political Discrimination): ATT Estimates as Risk Ratios - Complete Model"
)

###################################
### FIGURE 4, panel (a) (P. 21) ###
###################################

data<-haven::read_dta("EMBES_REPLICATION_DATASET.dta")
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
           output = list(file = "fig4_pa", type="pdf", width=10), 
           frame=FALSE,
           gridlines=list(type="t", col="grey65")
)

###################################
### FIGURE 4, panel (b) (P. 21) ###
###################################

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
           output = list(file = "fig4_pb", type="pdf", width=10), 
           frame=FALSE,
           gridlines=list(type="t", col="grey65")
)

###################################
######### TABLE 7 (P. 25) #########
###################################

rm(list=ls())

data<-haven::read_dta("SCIP_UK_W1.dta")

### UK MODELS (1) & (2)

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

xhyp3 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp3 <- cfName(xhyp3, "race_soc_disc_1", scen=1) 
xhyp3 <-cfChange(xhyp3, "race_soc_disc_1", x=2, xpre=0, scen=1)

xhyp3 <- cfName(xhyp3, "race_pol_disc_1", scen=2) 
xhyp3 <-cfChange(xhyp3, "race_pol_disc_1", x=2, xpre=0, scen=2)

xhyp3
yhyp3 <-logitsimfd(xhyp3, simbetas, ci=0.90)
yhyp3

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


### NL MODELS (1) & (2)

data<-haven::read_dta("SCIP_NL_W1.dta")

formula<-(ebe ~ race_soc_disc_1 + race_pol_disc_1 +
            rel_att + pol_int + political_knowledge + val_three +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            bulgarian + moroccan + polish + surinamese + turkish            
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

pe.5 <- ebe.spec.1$coefficients  # point estimates
vc.5 <- vcov(ebe.spec.1)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.5, vc.5)

# nscen = 1 cause I am interested only in borad disc. i
xhyp5 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp5 <- cfName(xhyp5, "race_soc_disc_1", scen=1) 
xhyp5 <-cfChange(xhyp5, "race_soc_disc_1", x=2, xpre=0, scen=1)

xhyp5 <- cfName(xhyp5, "race_pol_disc_1", scen=2) 
xhyp5 <-cfChange(xhyp5, "race_pol_disc_1", x=2, xpre=0, scen=2)

xhyp5
yhyp5 <-logitsimfd(xhyp5, simbetas, ci=0.90)
yhyp5


## ETHNIC BASED ENGAGEMENT - SPECIFIC MEASURES [societal and political discrimination - race,  with frequencies]

formula<-(ebe ~ race_soc_disc_2 + race_pol_disc_2 +
            rel_att + pol_int + political_knowledge + val_three +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            bulgarian + moroccan + polish + surinamese + turkish
          
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

pe.6 <- ebe.spec.2$coefficients  # point estimates
vc.6 <- vcov(ebe.spec.2)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.6, vc.6)

# nscen = 1 cause I am interested only in borad disc. i
xhyp6 <- cfMake(formula, mdata, nscen=2, f=mean)

xhyp6 <- cfName(xhyp6, "race_soc_disc_2", scen=1) 
xhyp6 <-cfChange(xhyp6, "race_soc_disc_2", x=6, xpre=0, scen=1)

xhyp6 <- cfName(xhyp6, "race_pol_disc_2", scen=2) 
xhyp6 <-cfChange(xhyp6, "race_pol_disc_2", x=6, xpre=0, scen=2)

xhyp6
yhyp6 <-logitsimfd(xhyp6, simbetas, ci=0.90)
yhyp6

modellabels<-c("Societal Discrimination 1", "Political Discrimination 1",
               "Societal Discrimination 2", "Political Discrimination 2",
               "Worship Attendance", "Political Interest", "Political Knowledge", 
               "Ireconcilable values", "Language at home",
               "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
               "Bulgarian" ,"Morrocan", "Polish", "Surinamese", "Turkish"
)

dvlabel<-c("Ethnic-Based Engagement")

stargazer(ebe.spec.1, ebe.spec.2,
          style="APSR", 
          covariate.labels = modellabels, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Ethnic Based Engagement: Specific Measures (Political and Societal Discrimination) for NL",
          dep.var.labels = dvlabel)



###################################
### FIGURE 5, panel (a) (P. 26) ###
###################################

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
                   output = list(file = "fig5_pa", type="pdf", width=12), 
                   frame=FALSE,
                   gridlines=list(type="t", col="grey65")
)


###################################
### FIGURE 4, panel (b) (P. 26) ###
###################################

trace3 <- ropeladder(
  x=yhyp5$pe,
  lower=yhyp5$lower,
  upper=yhyp5$upper,
  labels=c("Societal Discrimination. 1", "Political Discrimination 1"), 
  col= c("Black", "Black"),
  pch=c(5, 16),
  cex=2, 
  plot=1)

trace4 <- ropeladder(
  x=yhyp6$pe,
  lower=yhyp6$lower,
  upper=yhyp6$upper,
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
at.x2 <- seq(-.1, .3,.1)



tc_nl_scip_1<-tile(trace3, trace4,
                   vertmark,
                   RxC=c(2,1),
                   xaxis=list(at1 = at.x1, 
                              at2= at.x2, 
                              cex=.8),
                   topaxis=list(at1 = at.x1, 
                                at2= at.x2,
                                cex=.8,
                                add = rep(TRUE,2)),
                   topaxistitle = list(labels=c("Ethnic Based Engagement (Specific discrimination - racial)"), cex=1),
                   undertitle = list(labels="Change in Predicted Probability (Min-Max)", cex=.7, x=1),
                   output = list(file = "fig5_pb", type="pdf", width=12), 
                   frame=FALSE,
                   gridlines=list(type="t", col="grey65")
)

toc()
# 187.329 sec elapsed = 3.12215 min. to execute the code.

###############
### THE END ###
###############
###########