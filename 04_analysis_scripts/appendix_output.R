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
# from the appendix document of Discrimination that Matters: 
# Replication with extensions of 
# Perceived Discrimination and Political Behavior (2020)
# 

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
                       "tcltk", "lattice", "cem", "brglm2", "grid", "gridExtra", "cobalt",
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

#######################
### TABLE 1 (P. 2) ###
#######################

tic()

data<-haven::read_dta("EMBES_REPLICATION_DATASET.dta")

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


#######################
### FIGURE 1 (P. 3) ###
#######################

cor.test(data$socdisc, data$poldisc, method = c("spearman"), use = "complete.obs")
# 0.4243991

ggscatter(data, x = "poldisc", y = "socdisc", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          add.params = list(color = "blue",
                            fill = "lightgray"),
          xlab = "Political Discrimination", ylab = "Societal Discrimination")

#######################
### TABLE 2 (P. 6) ###
#######################

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

#######################
### TABLE 3 (P. 7) ###
#######################

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

stargazer(mod_sum6, mod_sum7, mod_sum8, mod_sum9, mod_sum10,
          style="APSR", 
          covariate.labels = modellabels5, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Local Elections (Specific Measures) - Alternative Specifications",
          dep.var.labels = dvlabel2
)

#######################
### TABLE 4 (P. 8) ###
#######################

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

#######################
### TABLE 5 (P. 9) ###
#######################

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

#######################
### TABLE 6 (P. 11) ###
#######################

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

tab.poldisc <- table(mdata$poldisc)
tab.socdisc <- table(mdata$socdisc)
tab.poldisc
tab.socdisc

mdata$socdisc_b <- ifelse(mdata$socdisc >= 1, 1, 0)
mdata$poldisc_b <- ifelse(mdata$poldisc >= 1, 1, 0)
table(mdata$socdisc_b)
table(mdata$poldisc_b)

todrop <- c("socdisc_b", "poldisc_b", "voted2010", "socdisc")
imb_m <- imbalance(group=mdata$socdisc_b, data=mdata, drop=todrop)

#export the table: 
col<-c("Political Disc.",
       "Worship Attendance", "Political Interest", "Political Knowledge", "Party ID (Yes=1)", 
       "Identity (Brit=2)", "English (Main Lang)", "Native Born", "Female", 
       "Age", "Education", "High Income", "Med Income", "Missing Income",
       "Black Caribbean", "Indian", "Pakistani", "Bangladeshi",
       "Vote Duty", "Political Efficacy","Democratic Satisfaction","Trust Parliament")

d_mean<-imb_m$tab$statistic
l1<-imb_m$tab$L1
imbalance_table1<-data.frame(col, d_mean, l1)
colnames(imbalance_table1)[1] <- "Variables"
colnames(imbalance_table1)[2] <- "Diff. in mean"
colnames(imbalance_table1)[3] <- "L1"
imbalance_table1

stargazer(imbalance_table1, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in General Elections (Societal Discrimination) - Imbalance before Coerced Exact Matching")

#######################
### TABLE 7 (P. 12) ###
#######################

todrop <- c("socdisc_b", "poldisc_b", "voted2010", "poldisc")
imb_m <- imbalance(group=mdata$poldisc_b, data=mdata, drop=todrop)

#export the table: 
col<-c("Societal Disc",
       "Worship Attendance", "Political Interest", "Political Knowledge", "Party ID (Yes=1)", 
       "Identity (Brit=2)", "English (Main Lang)", "Native Born", "Female", 
       "Age", "Education", "High Income", "Med Income", "Missing Income",
       "Black Caribbean", "Indian", "Pakistani", "Bangladeshi",
       "Vote Duty", "Political Efficacy","Democratic Satisfaction","Trust Parliament")

d_mean<-imb_m$tab$statistic
l1<-imb_m$tab$L1
imbalance_table2<-data.frame(col, d_mean, l1)
colnames(imbalance_table2)[1] <- "Variables"
colnames(imbalance_table2)[2] <- "Diff. in mean"
colnames(imbalance_table2)[3] <- "L1"
imbalance_table2

stargazer(imbalance_table2, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in General Elections (Political Discrimination) - Imbalance before Coerced Exact Matching")

#######################
### TABLE 8 (P. 13) ###
#######################

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

# BINARY INDICATORS FOR TREATMENT VARIABLES 

mdata$socdisc_b <- ifelse(mdata$socdisc >= 1, 1, 0)
mdata$poldisc_b <- ifelse(mdata$poldisc >= 1, 1, 0)
table(mdata$socdisc_b)
table(mdata$poldisc_b)

todrop <- c("socdisc_b", "poldisc_b", "voted2010_local", "socdisc")
imb_m2 <- imbalance(group=mdata$socdisc_b, data=mdata, drop=todrop)

#export the table: 
col<-c("Political Disc.",
       "Worship Attendance", "Political Interest", "Political Knowledge", "Party ID (Yes=1)", 
       "Identity (Brit=2)", "English (Main Lang)", "Native Born", "Female", 
       "Age", "Education", "High Income", "Med Income", "Missing Income",
       "Black Caribbean", "Indian", "Pakistani", "Bangladeshi",
       "Vote Duty", "Political Efficacy","Democratic Satisfaction","Trust Parliament")

d_mean<-imb_m2$tab$statistic
l1<-imb_m2$tab$L1
imbalance_table3<-data.frame(col, d_mean, l1)
colnames(imbalance_table3)[1] <- "Variables"
colnames(imbalance_table3)[2] <- "Diff. in mean"
colnames(imbalance_table3)[3] <- "L1"
imbalance_table3

stargazer(imbalance_table3, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in Local Elections (Societal Discrimination) - Imbalance before Coerced Exact Matching")

#######################
### TABLE 9 (P. 14) ###
#######################

todrop <- c("socdisc_b", "poldisc_b", "voted2010_local", "poldisc")
imb_m2 <- imbalance(group=mdata$poldisc_b, data=mdata, drop=todrop)

#export the table: 
col<-c("Societal Disc",
       "Worship Attendance", "Political Interest", "Political Knowledge", "Party ID (Yes=1)", 
       "Identity (Brit=2)", "English (Main Lang)", "Native Born", "Female", 
       "Age", "Education", "High Income", "Med Income", "Missing Income",
       "Black Caribbean", "Indian", "Pakistani", "Bangladeshi",
       "Vote Duty", "Political Efficacy","Democratic Satisfaction","Trust Parliament")

d_mean<-imb_m2$tab$statistic
l1<-imb_m2$tab$L1
imbalance_table4<-data.frame(col, d_mean, l1)
colnames(imbalance_table4)[1] <- "Variables"
colnames(imbalance_table4)[2] <- "Diff. in mean"
colnames(imbalance_table4)[3] <- "L1"
imbalance_table4

stargazer(imbalance_table4, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in Local Elections (Political Discrimination) - Imbalance before Coerced Exact Matching")

##########################
#### TABLE 10 (P. 15) ####
##########################

formula<-(ethnic_active~ socdisc + poldisc +
            relatt_oth_r + pol_interest+ polknowledge+ partyid + identity+ english+ native_born+
            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi + efficacy+ democ_satis+ trust_parliament
)


mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

# BINARY INDICATORS FOR TREATMENT VARIABLES 

mdata$socdisc_b <- ifelse(mdata$socdisc >= 1, 1, 0)
mdata$poldisc_b <- ifelse(mdata$poldisc >= 1, 1, 0)
table(mdata$socdisc_b)
table(mdata$poldisc_b)

## SOCIETAL DISCRIMINATION BINARY VAR
# IMBALANCE CHECKS

todrop <- c("socdisc_b", "poldisc_b", "ethnic_active", "socdisc")
imb_m3 <- imbalance(group=mdata$socdisc_b, data=mdata, drop=todrop)
imb_m3

#export the table: 
col<-c("Political Disc.",
       "Worship Attendance", "Political Interest", "Political Knowledge", "Party ID (Yes=1)", 
       "Identity (Brit=2)", "English (Main Lang)", "Native Born", "Female", 
       "Age", "Education", "High Income", "Med Income", "Missing Income",
       "Black Caribbean", "Indian", "Pakistani", "Bangladeshi",
       "Political Efficacy", "Democratic Satisfaction","Trust Parliament")

d_mean<-imb_m3$tab$statistic
l1<-imb_m3$tab$L1
imbalance_table5<-data.frame(col, d_mean, l1)
colnames(imbalance_table5)[1] <- "Variables"
colnames(imbalance_table5)[2] <- "Diff. in mean"
colnames(imbalance_table5)[3] <- "L1"
imbalance_table5

stargazer(imbalance_table5, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Ethnic Based Engagement (Societal Discrimination) - Imbalance before Coerced Exact Matching")

##########################
#### TABLE 11 (P. 16) ####
##########################

todrop <- c("socdisc_b", "poldisc_b", "ethnic_active", "poldisc")
imb_m3 <- imbalance(group=mdata$poldisc_b, data=mdata, drop=todrop)

#export the table: 
col<-c("Societal Disc",
       "Worship Attendance", "Political Interest", "Political Knowledge", "Party ID (Yes=1)", 
       "Identity (Brit=2)", "English (Main Lang)", "Native Born", "Female", 
       "Age", "Education", "High Income", "Med Income", "Missing Income",
       "Black Caribbean", "Indian", "Pakistani", "Bangladeshi",
       "Political Efficacy","Democratic Satisfaction","Trust Parliament")

d_mean<-imb_m3$tab$statistic
l1<-imb_m3$tab$L1
imbalance_table6<-data.frame(col, d_mean, l1)
colnames(imbalance_table6)[1] <- "Variables"
colnames(imbalance_table6)[2] <- "Diff. in mean"
colnames(imbalance_table6)[3] <- "L1"
imbalance_table6

stargazer(imbalance_table6, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Ethnic Based Engagement (Political Discrimination) - Imbalance before Coerced Exact Matching")

##########################
#### TABLE 12 (P. 17) ####
##########################

data<-haven::read_dta("EMBES_REPLICATION_DATASET.dta")

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

tab.poldisc <- table(mdata$poldisc)
tab.socdisc <- table(mdata$socdisc)
tab.poldisc
tab.socdisc

mdata$socdisc_b <- ifelse(mdata$socdisc >= 1, 1, 0)
mdata$poldisc_b <- ifelse(mdata$poldisc >= 1, 1, 0)
table(mdata$socdisc_b)
table(mdata$poldisc_b)

ndata <- mdata %>%
  select(voted2010, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, vote_duty, efficacy, democ_satis, trust_parliament) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

## check which variables are binary: 

identify_binary_indicators <- function(nata) {
  binary_indicators <- sapply(ndata, function(x) length(unique(x)) == 2 && all(x %in% c(0, 1)))
  binary_variables <- names(binary_indicators)[binary_indicators]
  num_binary_variables <- sum(binary_indicators)
  return(list(binary_variables = binary_variables, num_binary_variables = num_binary_variables))
}

# Call the function to identify binary indicator variables in 'data'
binary_info <- identify_binary_indicators(data)

# Print the results
cat("Number of binary indicator variables:", binary_info$num_binary_variables, "\n")
cat("Binary indicator variables:", paste(binary_info$binary_variables, collapse = ", "))

# CUTS
pol.cut<-c(0, 1, 5, 9) #equal bins (no vs low vs mid vs high)
rel.cut<-c(0, 2, 4) #every day and once a week vs month and several times a y vs festivals and none
polintr.cut<-c(0, 2, 4) #equal bins
polknow.cut<-c(0, 2, 4) #equal bins because index was rendered through summation
party.cut<-c(0,1)
identity.cut<-c(0, 1, 2) #table
english.cut<-c(0, 1)
native.cut<-c(0, 1)
female.cut<-c(0, 1)
age.cut<-c(18, 35, 50, 65) # fifteen years diff. 
education.cut<-c(0, 1, 4) #No education vs mid educ. vs higher educ. 
highinc.cut<-c(0, 1)
medinc.cut<-c(0, 1)
misinc.cut<-c(0, 1)
bcar.cut<-c(0, 1)
ind.cut<-c(0, 1)
pak.cut<-c(0, 1)
bang.cut<-c(0, 1)
vote.cut<-c(1, 4) #not agreeing or neutral in one bin and agreeing in another 
efficacy.cut<-c(0, 1, 5) # those who think they have 0 efficacy vs mid vs top
dem.cut<-c(0,2) #dissatisfied vs. satisfied
trust.cut<-c(0,4,7) ## no or low vs. mid vs. high
new.cuts<-list(poldisc=pol.cut,
               relatt_oth_r=rel.cut, 
               pol_interest=polintr.cut,
               polknowledge=polknow.cut, 
               partyid=party.cut,
               identity=identity.cut, 
               english=english.cut, 
               native_born=native.cut, 
               female=female.cut,
               age=age.cut,
               education2=education.cut,
               high.inc=highinc.cut,
               medinc=medinc.cut,
               misinc=misinc.cut,
               black_caribean=bcar.cut, 
               indian=ind.cut,
               pakistani=pak.cut,
               bangladeshi=bang.cut,
               vote_duty=vote.cut,
               efficacy=efficacy.cut,
               democ_satis=dem.cut,
               trust_parliament=trust.cut)

cem.match1 <- cem(treatment="socdisc_b", data=ndata, drop="voted2010", cutpoints=new.cuts, keep.all = T, eval.imbalance=T)
cem.match1

ndata <- ndata %>% 
  mutate(cem_weight = cem.match1$w)
class(cem.match1$w)
print(cem.match1$w)

res1 <- glm(voted2010~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
              identity+ english+ native_born+ female+ age+ education2+ highinc+ 
              medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
              vote_duty+ efficacy+ democ_satis+ trust_parliament, family=binomial(link=logit), data=ndata, weights = cem_weight)
summary(res1)

res_pml1 <- glm(voted2010~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                  vote_duty+ efficacy+ democ_satis+ trust_parliament, family=binomial(link=logit), data=ndata, weights = cem_weight, method = "brglmFit")
summary(res_pml1)
confint(res_pml1)
confint(res_pml1, level = 0.90)

pe.1 <- res1$coefficients  # point estimates
vc.1 <- vcov(res1)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.1, vc.1)

xhyp1 <- cfMake(voted2010~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                  vote_duty+ efficacy+ democ_satis+ trust_parliament, ndata, nscen=1, f=mean)

xhyp1 <- cfName(xhyp1, "socdisc_b", scen=1) 
xhyp1 <-cfChange(xhyp1, "socdisc_b", x=1, xpre=0, scen=1)

xhyp1
yhyp1<-logitsimfd(xhyp1, simbetas, ci=0.90)
yhyp1

ndata <- mdata %>%
  select(voted2010, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, vote_duty, efficacy, democ_satis, trust_parliament) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

## check which variables are binary: 

identify_binary_indicators <- function(nata) {
  binary_indicators <- sapply(ndata, function(x) length(unique(x)) == 2 && all(x %in% c(0, 1)))
  binary_variables <- names(binary_indicators)[binary_indicators]
  num_binary_variables <- sum(binary_indicators)
  return(list(binary_variables = binary_variables, num_binary_variables = num_binary_variables))
}

# Call the function to identify binary indicator variables in 'data'
binary_info <- identify_binary_indicators(data)

# Print the results
cat("Number of binary indicator variables:", binary_info$num_binary_variables, "\n")
cat("Binary indicator variables:", paste(binary_info$binary_variables, collapse = ", "))

## CUTS 
soc.cut<-c(0, 1, 5, 9) #equal bins (no vs low vs mid vs high)
rel.cut<-c(0, 2, 4) #every day and once a week vs month and several times a y vs festivals and none
polintr.cut<-c(0, 2, 4) #equal bins
polknow.cut<-c(0, 2, 4) #equal bins because index was rendered through summation
party.cut<-c(0,1)
identity.cut<-c(0, 1, 2) #table
english.cut<-c(0, 1)
native.cut<-c(0, 1)
female.cut<-c(0, 1)
age.cut<-c(18, 35, 50, 65) # fifteen years diff. 
education.cut<-c(0, 1, 4) #No education vs mid educ. vs higher educ. 
highinc.cut<-c(0, 1)
medinc.cut<-c(0, 1)
misinc.cut<-c(0, 1)
bcar.cut<-c(0, 1)
ind.cut<-c(0, 1)
pak.cut<-c(0, 1)
bang.cut<-c(0, 1)
vote.cut<-c(1, 4) #not agreeing or neutral in one bin and agreeing in another 
efficacy.cut<-c(0, 1, 5) # those who think they have 0 efficacy vs mid vs top
dem.cut<-c(0,2) #dissatisfied vs. satisfied
trust.cut<-c(0,4,7) ## no or low vs. mid vs. high
new.cuts<-list(socdisc=soc.cut,
               relatt_oth_r=rel.cut, 
               pol_interest=polintr.cut,
               polknowledge=polknow.cut, 
               partyid=party.cut,
               identity=identity.cut, 
               english=english.cut, 
               native_born=native.cut, 
               female=female.cut,
               age=age.cut,
               education2=education.cut,
               high.inc=highinc.cut,
               medinc=medinc.cut,
               misinc=misinc.cut,
               black_caribean=bcar.cut, 
               indian=ind.cut,
               pakistani=pak.cut,
               bangladeshi=bang.cut,
               vote_duty=vote.cut,
               efficacy=efficacy.cut,
               democ_satis=dem.cut,
               trust_parliament=trust.cut)


cem.match2 <- cem(treatment="poldisc_b", data=ndata, drop="voted2010", cutpoints=new.cuts, keep.all = T, eval.imbalance=T)
cem.match2

ndata <- ndata %>% 
  mutate(cem_weight = cem.match2$w)
class(cem.match2$w)
print(cem.match2$w)

res2 <- glm(voted2010~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
              identity+ english+ native_born+ female+ age+ education2+ highinc+ 
              medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
              vote_duty+ efficacy+ democ_satis+ trust_parliament, family=binomial(link=logit), data=ndata, weights = cem_weight)
summary(res2)

res_pml2 <- glm(voted2010~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                  vote_duty+ efficacy+ democ_satis+ trust_parliament, family=binomial(link=logit), data=ndata, weights = cem_weight, method = "brglmFit")
summary(res_pml2)
confint(res_pml2)
confint(res_pml2, level = 0.90)

pe.2 <- res2$coefficients  # point estimates
vc.2 <- vcov(res2)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.2, vc.2)

xhyp2 <- cfMake(voted2010~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                  vote_duty+ efficacy+ democ_satis+ trust_parliament, ndata, nscen=1, f=mean)

xhyp2 <- cfName(xhyp2, "poldisc_b", scen=1) 
xhyp2 <-cfChange(xhyp2, "poldisc_b", x=1, xpre=0, scen=1)

xhyp2
yhyp2<-logitsimfd(xhyp2, simbetas, ci=0.90)
yhyp2

ndata <- mdata %>%
  select(voted2010, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

pol.cut<-c(0, 1, 5, 9) #equal bins (no vs low vs mid vs high)
rel.cut<-c(0, 2, 4) #every day and once a week vs month and several times a y vs festivals and none
polintr.cut<-c(0, 2, 4) #equal bins
polknow.cut<-c(0, 2, 4) #equal bins because index was rendered through summation
party.cut<-c(0,1)
identity.cut<-c(0, 1, 2) #table
english.cut<-c(0, 1)
native.cut<-c(0, 1)
female.cut<-c(0, 1)
age.cut<-c(18, 35, 50, 65) # fifteen years diff. 
education.cut<-c(0, 1, 4) #No education vs mid educ. vs higher educ. 
highinc.cut<-c(0, 1)
medinc.cut<-c(0, 1)
misinc.cut<-c(0, 1)
bcar.cut<-c(0, 1)
ind.cut<-c(0, 1)
pak.cut<-c(0, 1)
bang.cut<-c(0, 1)
new.cuts<-list(poldisc=pol.cut,
               relatt_oth_r=rel.cut, 
               pol_interest=polintr.cut,
               polknowledge=polknow.cut, 
               partyid=party.cut,
               identity=identity.cut, 
               english=english.cut, 
               native_born=native.cut, 
               female=female.cut,
               age=age.cut,
               education2=education.cut,
               high.inc=highinc.cut,
               medinc=medinc.cut,
               misinc=misinc.cut,
               black_caribean=bcar.cut, 
               indian=ind.cut,
               pakistani=pak.cut,
               bangladeshi=bang.cut
)

cem.match5 <- cem(treatment="socdisc_b", data=ndata, drop="voted2010", cutpoints=new.cuts, keep.all = T, eval.imbalance=T)
cem.match5

# add weights: and re-run logit
ndata <- ndata %>% 
  mutate(cem_weight = cem.match5$w)
class(cem.match5$w)
print(cem.match5$w)

res5 <- glm(voted2010~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
              identity+ english+ native_born+ female+ age+ education2+ highinc+ 
              medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, family=binomial(link=logit), data=ndata, weights = cem_weight)
summary(res5)

res_pml5 <- glm(voted2010~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, family=binomial(link=logit), data=ndata, weights = cem_weight, method = "brglmFit")
summary(res_pml5)
confint(res_pml5)
confint(res_pml5, level = 0.90)

pe.5 <- res5$coefficients  # point estimates
vc.5 <- vcov(res5)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.5, vc.5)

xhyp5 <- cfMake(voted2010~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, ndata, nscen=1, f=mean)

xhyp5 <- cfName(xhyp5, "socdisc_b", scen=1) 
xhyp5 <-cfChange(xhyp5, "socdisc_b", x=1, xpre=0, scen=1)

xhyp5
yhyp5<-logitsimfd(xhyp5, simbetas, ci=0.90)
yhyp5

ndata <- mdata %>%
  select(voted2010, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

soc.cut<-c(0, 1, 5, 9) #equal bins (no vs low vs mid vs high)
rel.cut<-c(0, 2, 4) #every day and once a week vs month and several times a y vs festivals and none
polintr.cut<-c(0, 2, 4) #equal bins
polknow.cut<-c(0, 2, 4) #equal bins because index was rendered through summation
party.cut<-c(0,1)
identity.cut<-c(0, 1, 2) #table
english.cut<-c(0, 1)
native.cut<-c(0, 1)
female.cut<-c(0, 1)
age.cut<-c(18, 35, 50, 65) # fifteen years diff. 
education.cut<-c(0, 1, 4) #No education vs mid educ. vs higher educ. 
highinc.cut<-c(0, 1)
medinc.cut<-c(0, 1)
misinc.cut<-c(0, 1)
bcar.cut<-c(0, 1)
ind.cut<-c(0, 1)
pak.cut<-c(0, 1)
bang.cut<-c(0, 1)

new.cuts<-list(socdisc=soc.cut,
               relatt_oth_r=rel.cut, 
               pol_interest=polintr.cut,
               polknowledge=polknow.cut, 
               partyid=party.cut,
               identity=identity.cut, 
               english=english.cut, 
               native_born=native.cut, 
               female=female.cut,
               age=age.cut,
               education2=education.cut,
               high.inc=highinc.cut,
               medinc=medinc.cut,
               misinc=misinc.cut,
               black_caribean=bcar.cut, 
               indian=ind.cut,
               pakistani=pak.cut,
               bangladeshi=bang.cut
)


cem.match6 <- cem(treatment="poldisc_b", data=ndata, drop="voted2010", cutpoints=new.cuts, keep.all = T, eval.imbalance=T)
cem.match6

ndata <- ndata %>% 
  mutate(cem_weight = cem.match6$w)
class(cem.match6$w)
print(cem.match6$w)

res6 <- glm(voted2010~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
              identity+ english+ native_born+ female+ age+ education2+ highinc+ 
              medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, family=binomial(link=logit), data=ndata, weights = cem_weight)
summary(res6)

res_pml6 <- glm(voted2010~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, family=binomial(link=logit), data=ndata, weights = cem_weight, method = "brglmFit")
summary(res_pml6)
confint(res_pml6)
confint(res_pml6, level = 0.90)

pe.6 <- res6$coefficients  # point estimates
vc.6 <- vcov(res6)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.6, vc.6)

xhyp6 <- cfMake(voted2010~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, ndata, nscen=1, f=mean)

xhyp6 <- cfName(xhyp6, "poldisc_b", scen=1) 
xhyp6 <-cfChange(xhyp6, "poldisc_b", x=1, xpre=0, scen=1)

xhyp6
yhyp6<-logitsimfd(xhyp6, simbetas, ci=0.90)
yhyp6

modellabels<-c("Societal Disc. Binary", "Political Disc.", "Political Disc. Binary", "Societal Disc.",
               "Worship Attendance", "Political Interest", "Political Knowledge", 
               "Party ID (Yes=1)", "Identity (Brit=2)", "English (Main Lang)", "Native Born",
               "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
               "Black Caribbean", "Indian", "Pakistani", "Bangladeshi",
               "Vote Duty", "Political Efficacy","Democratic Satisfaction","Trust Parliament" 
)
dvlabel1<-c("Vote in General Election")

stargazer(res1, res_pml1, res2, res_pml2, res5, res_pml5, res6, res_pml6, 
          style="APSR", 
          covariate.labels = modellabels, 
          out.header=T,
          model.numbers = TRUE, 
          title = "General Elections (Specific Measures) - Matched models",
          dep.var.labels = dvlabel1
)

##########################
#### TABLE 13 (P. 18) ####
##########################

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

# BINARY INDICATORS FOR TREATMENT VARIABLES 

tab.poldisc <- table(mdata$poldisc)
tab.socdisc <- table(mdata$socdisc)
tab.poldisc
tab.socdisc

mdata$socdisc_b <- ifelse(mdata$socdisc >= 1, 1, 0)
mdata$poldisc_b <- ifelse(mdata$poldisc >= 1, 1, 0)
table(mdata$socdisc_b)
table(mdata$poldisc_b)

## create ndata 

ndata <- mdata %>%
  select(voted2010_local, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, vote_duty, efficacy, democ_satis, trust_parliament) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

pol.cut<-c(0, 1, 5, 9) #equal bins (no vs low vs mid vs high)
rel.cut<-c(0, 2, 4) #every day and once a week vs month and several times a y vs festivals and none
polintr.cut<-c(0, 2, 4) #equal bins
polknow.cut<-c(0, 2, 4) #equal bins because index was rendered through summation
party.cut<-c(0,1)
identity.cut<-c(0, 1, 2) #table
english.cut<-c(0, 1)
native.cut<-c(0, 1)
female.cut<-c(0, 1)
age.cut<-c(18, 35, 50, 65) # fifteen years diff. 
education.cut<-c(0, 1, 4) #No education vs mid educ. vs higher educ. 
highinc.cut<-c(0, 1)
medinc.cut<-c(0, 1)
misinc.cut<-c(0, 1)
bcar.cut<-c(0, 1)
ind.cut<-c(0, 1)
pak.cut<-c(0, 1)
bang.cut<-c(0, 1)
vote.cut<-c(1, 4) #not agreeing or neutral in one bin and agreeing in another 
efficacy.cut<-c(0, 1, 5) # those who think they have 0 efficacy vs mid vs top
dem.cut<-c(0,2) #dissatisfied vs. satisfied
trust.cut<-c(0,4,7) ## no or low vs. mid vs. high
new.cuts<-list(poldisc=pol.cut,
               relatt_oth_r=rel.cut, 
               pol_interest=polintr.cut,
               polknowledge=polknow.cut, 
               partyid=party.cut,
               identity=identity.cut, 
               english=english.cut, 
               native_born=native.cut, 
               female=female.cut,
               age=age.cut,
               education2=education.cut,
               high.inc=highinc.cut,
               medinc=medinc.cut,
               misinc=misinc.cut,
               black_caribean=bcar.cut, 
               indian=ind.cut,
               pakistani=pak.cut,
               bangladeshi=bang.cut,
               vote_duty=vote.cut,
               efficacy=efficacy.cut,
               democ_satis=dem.cut,
               trust_parliament=trust.cut)

cem.match3 <- cem(treatment="socdisc_b", data=ndata, drop="voted2010_local", cutpoints=new.cuts, keep.all = T, eval.imbalance=T)
cem.match3

# add weights: and re-run logit
ndata <- ndata %>% 
  mutate(cem_weight = cem.match3$w)
class(cem.match3$w)
print(cem.match3$w)

res3 <- glm(voted2010_local~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
              identity+ english+ native_born+ female+ age+ education2+ highinc+ 
              medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
              vote_duty+ efficacy+ democ_satis+ trust_parliament, family=binomial(link=logit), data=ndata, weights = cem_weight)
summary(res3)

res_pml3 <- glm(voted2010_local~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                  vote_duty+ efficacy+ democ_satis+ trust_parliament, family=binomial(link=logit), data=ndata, weights = cem_weight, method = "brglmFit")
summary(res_pml3)
confint(res_pml3)
confint(res_pml3, level = 0.90)

pe.3 <- res3$coefficients  # point estimates
vc.3 <- vcov(res3)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.3, vc.3)

xhyp3 <- cfMake(voted2010_local~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                  vote_duty+ efficacy+ democ_satis+ trust_parliament, ndata, nscen=1, f=mean)

xhyp3 <- cfName(xhyp3, "socdisc_b", scen=1) 
xhyp3 <-cfChange(xhyp3, "socdisc_b", x=1, xpre=0, scen=1)

xhyp3
yhyp3<-logitsimfd(xhyp3, simbetas, ci=0.90)
yhyp3

ndata <- mdata %>%
  select(voted2010_local, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, vote_duty, efficacy, democ_satis, trust_parliament) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

soc.cut<-c(0, 1, 5, 9) #equal bins (no vs low vs mid vs high)
rel.cut<-c(0, 2, 4) #every day and once a week vs month and several times a y vs festivals and none
polintr.cut<-c(0, 2, 4) #equal bins
polknow.cut<-c(0, 2, 4) #equal bins because index was rendered through summation
party.cut<-c(0,1)
identity.cut<-c(0, 1, 2) #table
english.cut<-c(0, 1)
native.cut<-c(0, 1)
female.cut<-c(0, 1)
age.cut<-c(18, 35, 50, 65) # fifteen years diff. 
education.cut<-c(0, 1, 4) #No education vs mid educ. vs higher educ. 
highinc.cut<-c(0, 1)
medinc.cut<-c(0, 1)
misinc.cut<-c(0, 1)
bcar.cut<-c(0, 1)
ind.cut<-c(0, 1)
pak.cut<-c(0, 1)
bang.cut<-c(0, 1)
vote.cut<-c(1, 4) #not agreeing or neutral in one bin and agreeing in another 
efficacy.cut<-c(0, 1, 5) # those who think they have 0 efficacy vs mid vs top
dem.cut<-c(0,2) #dissatisfied vs. satisfied
trust.cut<-c(0,4,7) ## no or low vs. mid vs. high
new.cuts<-list(socdisc=soc.cut,
               relatt_oth_r=rel.cut, 
               pol_interest=polintr.cut,
               polknowledge=polknow.cut, 
               partyid=party.cut,
               identity=identity.cut, 
               english=english.cut, 
               native_born=native.cut, 
               female=female.cut,
               age=age.cut,
               education2=education.cut,
               high.inc=highinc.cut,
               medinc=medinc.cut,
               misinc=misinc.cut,
               black_caribean=bcar.cut, 
               indian=ind.cut,
               pakistani=pak.cut,
               bangladeshi=bang.cut,
               vote_duty=vote.cut,
               efficacy=efficacy.cut,
               democ_satis=dem.cut,
               trust_parliament=trust.cut)


cem.match4 <- cem(treatment="poldisc_b", data=ndata, drop="voted2010_local", cutpoints=new.cuts, keep.all = T, eval.imbalance=T)
cem.match4

ndata <- ndata %>% 
  mutate(cem_weight = cem.match4$w)
class(cem.match4$w)
print(cem.match4$w)

res4 <- glm(voted2010_local~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
              identity+ english+ native_born+ female+ age+ education2+ highinc+ 
              medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
              vote_duty+ efficacy+ democ_satis+ trust_parliament, family=binomial(link=logit), data=ndata, weights = cem_weight)
summary(res4)

res_pml4 <- glm(voted2010_local~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                  vote_duty+ efficacy+ democ_satis+ trust_parliament, family=binomial(link=logit), data=ndata, weights = cem_weight, method = "brglmFit")
summary(res_pml4)
confint(res_pml4)
confint(res_pml4, level = 0.90)

pe.4 <- res4$coefficients  # point estimates
vc.4 <- vcov(res4)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.4, vc.4)

xhyp4 <- cfMake(voted2010_local~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                  vote_duty+ efficacy+ democ_satis+ trust_parliament, ndata, nscen=1, f=mean)

xhyp4 <- cfName(xhyp4, "poldisc_b", scen=1) 
xhyp4 <-cfChange(xhyp4, "poldisc_b", x=1, xpre=0, scen=1)

xhyp4
yhyp4<-logitsimfd(xhyp4, simbetas, ci=0.90)
yhyp4

formula<-(voted2010_local ~ 
            socdisc + poldisc+
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

mdata$socdisc_b <- ifelse(mdata$socdisc >= 1, 1, 0)
mdata$poldisc_b <- ifelse(mdata$poldisc >= 1, 1, 0)
table(mdata$socdisc_b)
table(mdata$poldisc_b)

ndata <- mdata %>%
  select(voted2010_local, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

pol.cut<-c(0, 1, 5, 9) #equal bins (no vs low vs mid vs high)
rel.cut<-c(0, 2, 4) #every day and once a week vs month and several times a y vs festivals and none
polintr.cut<-c(0, 2, 4) #equal bins
polknow.cut<-c(0, 2, 4) #equal bins because index was rendered through summation
party.cut<-c(0,1)
identity.cut<-c(0, 1, 2) #table
english.cut<-c(0, 1)
native.cut<-c(0, 1)
female.cut<-c(0, 1)
age.cut<-c(18, 35, 50, 65) # fifteen years diff. 
education.cut<-c(0, 1, 4) #No education vs mid educ. vs higher educ. 
highinc.cut<-c(0, 1)
medinc.cut<-c(0, 1)
misinc.cut<-c(0, 1)
bcar.cut<-c(0, 1)
ind.cut<-c(0, 1)
pak.cut<-c(0, 1)
bang.cut<-c(0, 1)
new.cuts<-list(poldisc=pol.cut,
               relatt_oth_r=rel.cut, 
               pol_interest=polintr.cut,
               polknowledge=polknow.cut, 
               partyid=party.cut,
               identity=identity.cut, 
               english=english.cut, 
               native_born=native.cut, 
               female=female.cut,
               age=age.cut,
               education2=education.cut,
               high.inc=highinc.cut,
               medinc=medinc.cut,
               misinc=misinc.cut,
               black_caribean=bcar.cut, 
               indian=ind.cut,
               pakistani=pak.cut,
               bangladeshi=bang.cut
)


cem.match7 <- cem(treatment="socdisc_b", data=ndata, drop="voted2010_local", cutpoints=new.cuts, keep.all = T, eval.imbalance=T)
cem.match7

ndata <- ndata %>% 
  mutate(cem_weight = cem.match7$w)
class(cem.match7$w)
print(cem.match7$w)

res7 <- glm(voted2010_local~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
              identity+ english+ native_born+ female+ age+ education2+ highinc+ 
              medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, family=binomial(link=logit), data=ndata, weights = cem_weight)
summary(res7)

res_pml7 <- glm(voted2010_local~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, family=binomial(link=logit), data=ndata, weights = cem_weight, method = "brglmFit")
summary(res_pml7)
confint(res_pml7)
confint(res_pml7, level = 0.90)

pe.7 <- res7$coefficients  # point estimates
vc.7 <- vcov(res7)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.7, vc.7)

xhyp7 <- cfMake(voted2010_local~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, ndata, nscen=1, f=mean)

xhyp7 <- cfName(xhyp7, "socdisc_b", scen=1) 
xhyp7 <-cfChange(xhyp7, "socdisc_b", x=1, xpre=0, scen=1)

xhyp7
yhyp7<-logitsimfd(xhyp7, simbetas, ci=0.90)
yhyp7

ndata <- mdata %>%
  select(voted2010_local, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

soc.cut<-c(0, 1, 5, 9) #equal bins (no vs low vs mid vs high)
rel.cut<-c(0, 2, 4) #every day and once a week vs month and several times a y vs festivals and none
polintr.cut<-c(0, 2, 4) #equal bins
polknow.cut<-c(0, 2, 4) #equal bins because index was rendered through summation
party.cut<-c(0,1)
identity.cut<-c(0, 1, 2) #table
english.cut<-c(0, 1)
native.cut<-c(0, 1)
female.cut<-c(0, 1)
age.cut<-c(18, 35, 50, 65) # fifteen years diff. 
education.cut<-c(0, 1, 4) #No education vs mid educ. vs higher educ. 
highinc.cut<-c(0, 1)
medinc.cut<-c(0, 1)
misinc.cut<-c(0, 1)
bcar.cut<-c(0, 1)
ind.cut<-c(0, 1)
pak.cut<-c(0, 1)
bang.cut<-c(0, 1)

new.cuts<-list(socdisc=soc.cut,
               relatt_oth_r=rel.cut, 
               pol_interest=polintr.cut,
               polknowledge=polknow.cut, 
               partyid=party.cut,
               identity=identity.cut, 
               english=english.cut, 
               native_born=native.cut, 
               female=female.cut,
               age=age.cut,
               education2=education.cut,
               high.inc=highinc.cut,
               medinc=medinc.cut,
               misinc=misinc.cut,
               black_caribean=bcar.cut, 
               indian=ind.cut,
               pakistani=pak.cut,
               bangladeshi=bang.cut
)


cem.match8 <- cem(treatment="poldisc_b", data=ndata, drop="voted2010_local", cutpoints=new.cuts, keep.all = T, eval.imbalance=T)
cem.match8

ndata <- ndata %>% 
  mutate(cem_weight = cem.match8$w)
class(cem.match8$w)
print(cem.match8$w)

res8 <- glm(voted2010_local~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
              identity+ english+ native_born+ female+ age+ education2+ highinc+ 
              medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, family=binomial(link=logit), data=ndata, weights = cem_weight)
summary(res8)

res_pml8 <- glm(voted2010_local~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, family=binomial(link=logit), data=ndata, weights = cem_weight, method = "brglmFit")
summary(res_pml8)
confint(res_pml8)
confint(res_pml8, level = 0.90)

pe.8 <- res8$coefficients  # point estimates
vc.8 <- vcov(res8)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.8, vc.8)

xhyp8 <- cfMake(voted2010_local~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, ndata, nscen=1, f=mean)

xhyp8 <- cfName(xhyp8, "poldisc_b", scen=1) 
xhyp8 <-cfChange(xhyp8, "poldisc_b", x=1, xpre=0, scen=1)

xhyp8
yhyp8<-logitsimfd(xhyp8, simbetas, ci=0.90)
yhyp8


modellabels<-c("Societal Disc. Binary", "Political Disc.", "Political Disc. Binary", "Societal Disc.",
               "Worship Attendance", "Political Interest", "Political Knowledge", 
               "Party ID (Yes=1)", "Identity (Brit=2)", "English (Main Lang)", "Native Born",
               "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
               "Black Caribbean", "Indian", "Pakistani", "Bangladeshi",
               "Vote Duty", "Political Efficacy","Democratic Satisfaction","Trust Parliament" 
)

dvlabel2<-c("Vote in Local Election")

### VOTE IN LOCAL ELECTIONS
stargazer(res3, res_pml3, res4, res_pml4, res7, res_pml7, res8, res_pml8, 
          style="APSR", 
          covariate.labels = modellabels, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Local Elections (Specific Measures) - Matched models",
          dep.var.labels = dvlabel2
)

##########################
#### TABLE 14 (P. 19) ####
##########################

data<-haven::read_dta("EMBES_REPLICATION_DATASET.dta")

formula<-(ethnic_active~ socdisc + poldisc +
            relatt_oth_r + pol_interest+ polknowledge+ partyid + identity+ english+ native_born+
            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi + efficacy+ democ_satis+ trust_parliament
)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

mdata$socdisc_b <- ifelse(mdata$socdisc >= 1, 1, 0)
mdata$poldisc_b <- ifelse(mdata$poldisc >= 1, 1, 0)
table(mdata$socdisc_b)
table(mdata$poldisc_b)

ndata <- mdata %>%
  select(ethnic_active, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, efficacy, democ_satis, trust_parliament) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

pol.cut<-c(0, 1, 5, 9) #equal bins (no vs low vs mid vs high)
rel.cut<-c(0, 2, 4) #every day and once a week vs month and several times a y vs festivals and none
polintr.cut<-c(0, 2, 4) #equal bins
polknow.cut<-c(0, 2, 4) #equal bins because index was rendered through summation
party.cut<-c(0,1)
identity.cut<-c(0, 1, 2) #table
english.cut<-c(0, 1)
native.cut<-c(0, 1)
female.cut<-c(0, 1)
age.cut<-c(18, 35, 50, 65) # fifteen years diff. 
education.cut<-c(0, 1, 4) #No education vs mid educ. vs higher educ. 
highinc.cut<-c(0, 1)
medinc.cut<-c(0, 1)
misinc.cut<-c(0, 1)
bcar.cut<-c(0, 1)
ind.cut<-c(0, 1)
pak.cut<-c(0, 1)
bang.cut<-c(0, 1)
efficacy.cut<-c(0, 1, 5) # those who think they have 0 efficacy vs mid vs top
dem.cut<-c(0,2) #dissatisfied vs. satisfied
trust.cut<-c(0,4,7) ## no or low vs. mid vs. high
new.cuts<-list(poldisc=pol.cut,
               relatt_oth_r=rel.cut, 
               pol_interest=polintr.cut,
               polknowledge=polknow.cut, 
               partyid=party.cut,
               identity=identity.cut, 
               english=english.cut, 
               native_born=native.cut, 
               female=female.cut,
               age=age.cut,
               education2=education.cut,
               high.inc=highinc.cut,
               medinc=medinc.cut,
               misinc=misinc.cut,
               black_caribean=bcar.cut, 
               indian=ind.cut,
               pakistani=pak.cut,
               bangladeshi=bang.cut,
               efficacy=efficacy.cut,
               democ_satis=dem.cut,
               trust_parliament=trust.cut)

cem.match9 <- cem(treatment="socdisc_b", data=ndata, drop="ethnic_active", cutpoints=new.cuts, keep.all = T, eval.imbalance=T)
cem.match9

# add weights: and re-run logit
ndata <- ndata %>% 
  mutate(cem_weight = cem.match9$w)
class(cem.match9$w)
print(cem.match9$w)

res9 <- glm(ethnic_active~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
              identity+ english+ native_born+ female+ age+ education2+ highinc+ 
              medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi + efficacy + democ_satis + trust_parliament, family=binomial(link=logit), data=ndata, weights = cem_weight)
summary(res9)

res_pml9 <- glm(ethnic_active~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi + efficacy + democ_satis + trust_parliament, family=binomial(link=logit), data=ndata, weights = cem_weight, method = "brglmFit")
summary(res_pml9)
confint(res_pml9)
confint(res_pml9, level = 0.90)

pe.9 <- res9$coefficients  # point estimates
vc.9 <- vcov(res9)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.9, vc.9)

xhyp9 <- cfMake(ethnic_active~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                  identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                  medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi + efficacy + democ_satis + trust_parliament, ndata, nscen=1, f=mean)

xhyp9 <- cfName(xhyp9, "socdisc_b", scen=1) 
xhyp9 <-cfChange(xhyp9, "socdisc_b", x=1, xpre=0, scen=1)

xhyp9
yhyp9<-logitsimfd(xhyp9, simbetas, ci=0.90)
yhyp9

ndata <- mdata %>%
  select(ethnic_active, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, efficacy, democ_satis, trust_parliament) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

soc.cut<-c(0, 1, 5, 9) #equal bins (no vs low vs mid vs high)
rel.cut<-c(0, 2, 4) #every day and once a week vs month and several times a y vs festivals and none
polintr.cut<-c(0, 2, 4) #equal bins
polknow.cut<-c(0, 2, 4) #equal bins because index was rendered through summation
party.cut<-c(0,1)
identity.cut<-c(0, 1, 2) #table
english.cut<-c(0, 1)
native.cut<-c(0, 1)
female.cut<-c(0, 1)
age.cut<-c(18, 35, 50, 65) # fifteen years diff. 
education.cut<-c(0, 1, 4) #No education vs mid educ. vs higher educ. 
highinc.cut<-c(0, 1)
medinc.cut<-c(0, 1)
misinc.cut<-c(0, 1)
bcar.cut<-c(0, 1)
ind.cut<-c(0, 1)
pak.cut<-c(0, 1)
bang.cut<-c(0, 1)
efficacy.cut<-c(0, 1, 5) # those who think they have 0 efficacy vs mid vs top
dem.cut<-c(0,2) #dissatisfied vs. satisfied
trust.cut<-c(0,4,7) ## no or low vs. mid vs. high

new.cuts<-list(socdisc=soc.cut,
               relatt_oth_r=rel.cut, 
               pol_interest=polintr.cut,
               polknowledge=polknow.cut, 
               partyid=party.cut,
               identity=identity.cut, 
               english=english.cut, 
               native_born=native.cut, 
               female=female.cut,
               age=age.cut,
               education2=education.cut,
               high.inc=highinc.cut,
               medinc=medinc.cut,
               misinc=misinc.cut,
               black_caribean=bcar.cut, 
               indian=ind.cut,
               pakistani=pak.cut,
               bangladeshi=bang.cut,
               efficacy=efficacy.cut,
               democ_satis=dem.cut,
               trust_parliament=trust.cut
)


cem.match10 <- cem(treatment="poldisc_b", data=ndata, drop="ethnic_active", cutpoints=new.cuts, keep.all = T, eval.imbalance=T)
cem.match10

ndata <- ndata %>% 
  mutate(cem_weight = cem.match10$w)
class(cem.match10$w)
print(cem.match10$w)

res10 <- glm(ethnic_active~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
               identity+ english+ native_born+ female+ age+ education2+ highinc+ 
               medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi + efficacy + democ_satis + trust_parliament, family=binomial(link=logit), data=ndata, weights = cem_weight)
summary(res10)

res_pml10 <- glm(ethnic_active~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                   identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                   medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi + efficacy + democ_satis + trust_parliament, family=binomial(link=logit), data=ndata, weights = cem_weight, method = "brglmFit")
summary(res_pml10)
confint(res_pml10)
confint(res_pml10, level = 0.90)

pe.10 <- res10$coefficients  # point estimates
vc.10 <- vcov(res10)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.10, vc.10)

xhyp10 <- cfMake(ethnic_active~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                   identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                   medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi + efficacy + democ_satis + trust_parliament, ndata, nscen=1, f=mean)

xhyp10 <- cfName(xhyp10, "poldisc_b", scen=1) 
xhyp10 <-cfChange(xhyp10, "poldisc_b", x=1, xpre=0, scen=1)

xhyp10
yhyp10<-logitsimfd(xhyp10, simbetas, ci=0.90)
yhyp10

formula<-(ethnic_active~ socdisc + poldisc +
            relatt_oth_r + pol_interest+ polknowledge+ partyid + identity+ english+ native_born+
            female+ age+ education2+ highinc+ medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi)


mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

# BINARY INDICATORS FOR TREATMENT VARIABLES 

mdata$socdisc_b <- ifelse(mdata$socdisc >= 1, 1, 0)
mdata$poldisc_b <- ifelse(mdata$poldisc >= 1, 1, 0)
table(mdata$socdisc_b)
table(mdata$poldisc_b)

ndata <- mdata %>%
  select(ethnic_active, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

pol.cut<-c(0, 1, 5, 9) #equal bins (no vs low vs mid vs high)
rel.cut<-c(0, 2, 4) #every day and once a week vs month and several times a y vs festivals and none
polintr.cut<-c(0, 2, 4) #equal bins
polknow.cut<-c(0, 2, 4) #equal bins because index was rendered through summation
party.cut<-c(0,1)
identity.cut<-c(0, 1, 2) #table
english.cut<-c(0, 1)
native.cut<-c(0, 1)
female.cut<-c(0, 1)
age.cut<-c(18, 35, 50, 65) # fifteen years diff. 
education.cut<-c(0, 1, 4) #No education vs mid educ. vs higher educ. 
highinc.cut<-c(0, 1)
medinc.cut<-c(0, 1)
misinc.cut<-c(0, 1)
bcar.cut<-c(0, 1)
ind.cut<-c(0, 1)
pak.cut<-c(0, 1)
bang.cut<-c(0, 1)

new.cuts<-list(poldisc=pol.cut,
               relatt_oth_r=rel.cut, 
               pol_interest=polintr.cut,
               polknowledge=polknow.cut, 
               partyid=party.cut,
               identity=identity.cut, 
               english=english.cut, 
               native_born=native.cut, 
               female=female.cut,
               age=age.cut,
               education2=education.cut,
               high.inc=highinc.cut,
               medinc=medinc.cut,
               misinc=misinc.cut,
               black_caribean=bcar.cut, 
               indian=ind.cut,
               pakistani=pak.cut,
               bangladeshi=bang.cut
)

cem.match11 <- cem(treatment="socdisc_b", data=ndata, drop="ethnic_active", cutpoints=new.cuts, keep.all = T, eval.imbalance=T)
cem.match11

# add weights: and re-run logit
ndata <- ndata %>% 
  mutate(cem_weight = cem.match11$w)
class(cem.match11$w)
print(cem.match11$w)

res11 <- glm(ethnic_active~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
               identity+ english+ native_born+ female+ age+ education2+ highinc+ 
               medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, family=binomial(link=logit), data=ndata, weights = cem_weight)
summary(res11)

res_pml11 <- glm(ethnic_active~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                   identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                   medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, family=binomial(link=logit), data=ndata, weights = cem_weight, method = "brglmFit")
summary(res_pml11)
confint(res_pml11)
confint(res_pml11, level = 0.90)

pe.11 <- res11$coefficients  # point estimates
vc.11 <- vcov(res11)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.11, vc.11)

xhyp11 <- cfMake(ethnic_active~socdisc_b+ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                   identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                   medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, ndata, nscen=1, f=mean)

xhyp11 <- cfName(xhyp11, "socdisc_b", scen=1) 
xhyp11 <-cfChange(xhyp11, "socdisc_b", x=1, xpre=0, scen=1)

xhyp11
yhyp11<-logitsimfd(xhyp11, simbetas, ci=0.90)
yhyp11


ndata <- mdata %>%
  select(ethnic_active, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

soc.cut<-c(0, 1, 5, 9) #equal bins (no vs low vs mid vs high)
rel.cut<-c(0, 2, 4) #every day and once a week vs month and several times a y vs festivals and none
polintr.cut<-c(0, 2, 4) #equal bins
polknow.cut<-c(0, 2, 4) #equal bins because index was rendered through summation
party.cut<-c(0,1)
identity.cut<-c(0, 1, 2) #table
english.cut<-c(0, 1)
native.cut<-c(0, 1)
female.cut<-c(0, 1)
age.cut<-c(18, 35, 50, 65) # fifteen years diff. 
education.cut<-c(0, 1, 4) #No education vs mid educ. vs higher educ. 
highinc.cut<-c(0, 1)
medinc.cut<-c(0, 1)
misinc.cut<-c(0, 1)
bcar.cut<-c(0, 1)
ind.cut<-c(0, 1)
pak.cut<-c(0, 1)
bang.cut<-c(0, 1)

new.cuts<-list(socdisc=soc.cut,
               relatt_oth_r=rel.cut, 
               pol_interest=polintr.cut,
               polknowledge=polknow.cut, 
               partyid=party.cut,
               identity=identity.cut, 
               english=english.cut, 
               native_born=native.cut, 
               female=female.cut,
               age=age.cut,
               education2=education.cut,
               high.inc=highinc.cut,
               medinc=medinc.cut,
               misinc=misinc.cut,
               black_caribean=bcar.cut, 
               indian=ind.cut,
               pakistani=pak.cut,
               bangladeshi=bang.cut
)


cem.match12 <- cem(treatment="poldisc_b", data=ndata, drop="ethnic_active", cutpoints=new.cuts, keep.all = T, eval.imbalance=T)
cem.match12

ndata <- ndata %>% 
  mutate(cem_weight = cem.match12$w)
class(cem.match12$w)
print(cem.match12$w)

res12 <- glm(ethnic_active~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
               identity+ english+ native_born+ female+ age+ education2+ highinc+ 
               medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, family=binomial(link=logit), data=ndata, weights = cem_weight)
summary(res12)

res_pml12 <- glm(ethnic_active~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                   identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                   medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, family=binomial(link=logit), data=ndata, weights = cem_weight, method = "brglmFit")
summary(res_pml12)
confint(res_pml12)
confint(res_pml12, level = 0.90)

pe.12 <- res12$coefficients  # point estimates
vc.12 <- vcov(res12)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.12, vc.12)

xhyp12 <- cfMake(ethnic_active~poldisc_b+ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                   identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                   medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi, ndata, nscen=1, f=mean)

xhyp12 <- cfName(xhyp12, "poldisc_b", scen=1) 
xhyp12 <-cfChange(xhyp12, "poldisc_b", x=1, xpre=0, scen=1)

xhyp12
yhyp12<-logitsimfd(xhyp12, simbetas, ci=0.90)
yhyp12

modellabels2<-c("Societal Disc. Binary", "Political Disc.", "Political Disc. Binary", "Societal Disc.",
                "Worship Attendance", "Political Interest", "Political Knowledge", 
                "Party ID (Yes=1)", "Identity (Brit=2)", "English (Main Lang)", "Native Born",
                "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
                "Black Caribbean", "Indian", "Pakistani", "Bangladeshi",
                "Political Efficacy","Democratic Satisfaction","Trust Parliament" 
)

dvlabel3<-c("Ethnic-Based Participation")

stargazer(res9, res_pml9, 
          res10, res_pml10, 
          res11, res_pml11,
          res12, res_pml12,
          style="APSR", 
          covariate.labels = modellabels2, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Ethnic-Based Engagement (Specific Measures) - Matched models",
          dep.var.labels = dvlabel3
)

##########################
#### FIGURE 2 (P. 20) ####
##########################

trace1 <- ropeladder(
  x=yhyp1$pe,
  lower=yhyp1$lower,
  upper=yhyp1$upper,
  labels=c("Societal Disc."), 
  col= c("Black"),
  pch=c(5),
  cex=2, 
  plot=1)

trace2 <- ropeladder(
  x=yhyp2$pe,
  lower=yhyp2$lower,
  upper=yhyp2$upper,
  labels=c("Political Disc."), 
  col= c("Black"),
  pch=c(16),
  cex=2, 
  plot=2)

trace3 <- ropeladder(
  x=yhyp5$pe,
  lower=yhyp5$lower,
  upper=yhyp5$upper,
  labels=c("Societal Disc."), 
  col= c("Black"),
  pch=c(5),
  cex=2, 
  plot=3)

trace4 <- ropeladder(
  x=yhyp6$pe,
  lower=yhyp6$lower,
  upper=yhyp6$upper,
  labels=c("Political Disc."), 
  col= c("Black"),
  pch=c(16),
  cex=2, 
  plot=4)


vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:4
)

# Widen space between titles on left
trace1$entryheight <- .17
trace2$entryheight <- .17
trace3$entryheight <- .17
trace4$entryheight <- .17

at.x1 <- seq(-.1, .3,.1)
at.x2 <- seq(-.1, .3,.1)
at.x3 <- seq(-.2, .1,.1)
at.x4 <- seq(-.1, .1,.1)



tc1<-tile(trace1, trace2, trace3, trace4,
          vertmark,
          RxC=c(4,1),
          xaxis=list(at1 = at.x1, 
                     at2= at.x2, 
                     at3= at.x3,
                     at4= at.x4,
                     cex=.8),
          topaxis=list(at1 = at.x1, 
                       at2= at.x2, 
                       at3= at.x3,
                       at4= at.x4,
                       cex=.8,
                       add = rep(TRUE,4)),
          topaxistitle = list(labels=c("Vote in General Elections: Complete Model", "Vote in General Elections: Complete Model", "Vote in General Elections: Simplified Model", "Vote in General Elections: Simplified Model"), cex=1),
          undertitle = list(labels1="Change in Predicted Probability (Min-Max)",
                            labels2="Change in Predicted Probability (Min-Max)",
                            labels3="Change in Predicted Probability (Min-Max)",
                            labels4="Change in Predicted Probability (Min-Max)",
                            cex=.7),
          output = list(file = "match_fig1", type="pdf", width=10), 
          frame=FALSE,
          gridlines=list(type="t", col="grey65")
)

##########################
#### FIGURE 3 (P. 21) ####
##########################

trace5 <- ropeladder(
  x=yhyp3$pe,
  lower=yhyp3$lower,
  upper=yhyp3$upper,
  labels=c("Societal Disc."), 
  col= c("Black"),
  pch=c(5),
  cex=2, 
  plot=1)

trace6 <- ropeladder(
  x=yhyp4$pe,
  lower=yhyp4$lower,
  upper=yhyp4$upper,
  labels=c("Political Disc."), 
  col= c("Black"),
  pch=c(16),
  cex=2, 
  plot=2)

trace7 <- ropeladder(
  x=yhyp7$pe,
  lower=yhyp7$lower,
  upper=yhyp7$upper,
  labels=c("Societal Disc."), 
  col= c("Black"),
  pch=c(5),
  cex=2, 
  plot=3)

trace8 <- ropeladder(
  x=yhyp8$pe,
  lower=yhyp8$lower,
  upper=yhyp8$upper,
  labels=c("Political Disc."), 
  col= c("Black"),
  pch=c(16),
  cex=2, 
  plot=4)


vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:4
)

# Widen space between titles on left
trace1$entryheight <- .17
trace2$entryheight <- .17
trace3$entryheight <- .17
trace4$entryheight <- .17

at.x1 <- seq(-.2, .1,.1)
at.x2 <- seq(-.2, .1,.1)
at.x3 <- seq(-.2, .1,.1)
at.x4 <- seq(-.1, .1,.1)



tc2<-tile(trace5, trace6, trace7, trace8,
          vertmark,
          RxC=c(4,1),
          xaxis=list(at1 = at.x1, 
                     at2= at.x2, 
                     at3= at.x3,
                     at4= at.x4,
                     cex=.8),
          topaxis=list(at1 = at.x1, 
                       at2= at.x2, 
                       at3= at.x3,
                       at4= at.x4,
                       cex=.8,
                       add = rep(TRUE,4)),
          topaxistitle = list(labels=c("Vote in Local Elections: Complete Model", "Vote in Local Elections: Complete Model", "Vote in Local Elections: Simplified Model", "Vote in Local Elections: Simplified Model"), cex=1),
          undertitle = list(labels1="Change in Predicted Probability (Min-Max)",
                            labels2="Change in Predicted Probability (Min-Max)",
                            labels3="Change in Predicted Probability (Min-Max)",
                            labels4="Change in Predicted Probability (Min-Max)",
                            cex=.7),
          output = list(file = "match_fig2", type="pdf", width=10), 
          frame=FALSE,
          gridlines=list(type="t", col="grey65")
)

##########################
#### FIGURE 4 (P. 22) ####
##########################

trace9 <- ropeladder(
  x=yhyp9$pe,
  lower=yhyp9$lower,
  upper=yhyp9$upper,
  labels=c("Societal Disc."), 
  col= c("Black"),
  pch=c(5),
  cex=2, 
  plot=1)

trace10 <- ropeladder(
  x=yhyp10$pe,
  lower=yhyp10$lower,
  upper=yhyp10$upper,
  labels=c("Political Disc."), 
  col= c("Black"),
  pch=c(16),
  cex=2, 
  plot=2)

trace11 <- ropeladder(
  x=yhyp11$pe,
  lower=yhyp11$lower,
  upper=yhyp11$upper,
  labels=c("Societal Disc."), 
  col= c("Black"),
  pch=c(5),
  cex=2, 
  plot=3)

trace12 <- ropeladder(
  x=yhyp12$pe,
  lower=yhyp12$lower,
  upper=yhyp12$upper,
  labels=c("Political Disc."), 
  col= c("Black"),
  pch=c(16),
  cex=2, 
  plot=4)


vertmark <- linesTile(x = c(0,0),
                      y = c(0,1),
                      lty = "dashed",
                      col= "Black",
                      plot = 1:4
)

# Widen space between titles on left
trace9$entryheight <- .17
trace10$entryheight <- .17
trace11$entryheight <- .17
trace12$entryheight <- .17

at.x1 <- seq(-.1, .2,.1)
at.x2 <- seq(-.1, .2,.1)
at.x3 <- seq(-.1, .1,.1)
at.x4 <- seq(-.1, .1,.1)



tc3<-tile(trace9, trace10, trace11, trace12,
          vertmark,
          RxC=c(4,1),
          xaxis=list(at1 = at.x1, 
                     at2= at.x2, 
                     at3= at.x3,
                     at4= at.x4,
                     cex=.8),
          topaxis=list(at1 = at.x1, 
                       at2= at.x2, 
                       at3= at.x3,
                       at4= at.x4,
                       cex=.8,
                       add = rep(TRUE,4)),
          topaxistitle = list(labels=c("Ethnic Based Engagement: Complete Model", "Ethnic Based Engagement: Complete Model", "Ethnic Based Engagement: Simplified Model", "Ethnic Based Engagement: Simplified Model"), cex=1),
          undertitle = list(labels1="Change in Predicted Probability (Min-Max)",
                            labels2="Change in Predicted Probability (Min-Max)",
                            labels3="Change in Predicted Probability (Min-Max)",
                            labels4="Change in Predicted Probability (Min-Max)",
                            cex=.7),
          output = list(file = "match_fig3", type="pdf", width=10), 
          frame=FALSE,
          gridlines=list(type="t", col="grey65")
)

##########################
#### TABLE 15 (P. 23) ####
##########################

data<-haven::read_dta("EMBES_REPLICATION_DATASET.dta")

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

## Make new plots for the CDF tables:

plot_full_probit <- plot(summary(m.out.fp))
plot_full_logit <- plot(summary(m.out.fl))
plot_nearest_probit <- plot(summary(m.out.np))
plot_nearest_logit <- plot(summary(m.out.nl))
plot_optimal_probit <- plot(summary(m.out.op))
plot_optimal_logit <- plot(summary(m.out.ol))

## Mean CDF table: 
c1<-plot_full_probit$sum.matched[,5]
c2<-plot_full_logit$sum.matched[,5]
c3<-plot_nearest_probit$sum.matched[,5]
c4<-plot_nearest_logit$sum.matched[,5]
c5<-plot_optimal_probit$sum.matched[,5]
c6<-plot_optimal_logit$sum.matched[,5]
#c7<-plot_mahal$sum.matched[,5]

table1<-rbind(c1, c2, c3, c4, c5, c6)
rownames(table1) <- c("Full Probit", "Full Logit", "Nearest Probit", "Nearest Logit", "Optimal Probit", "Optimal Logit")
table1<-as.data.frame(table1)
colnames(table1)[1] <- "Distance"
colnames(table1)[2] <- "Political Discrimination"
colnames(table1)[3] <- "Worship Attendance"
colnames(table1)[4] <- "Political Interest"
colnames(table1)[5] <- "Political Knowledge"
colnames(table1)[6] <- "Party ID"
colnames(table1)[7] <- "Identity"
colnames(table1)[8] <- "English (Main Lang)"
colnames(table1)[9] <- "Native Born"
colnames(table1)[10] <- "Female"
colnames(table1)[11] <- "Age"
colnames(table1)[12] <- "Education"
colnames(table1)[13] <- "High Income"
colnames(table1)[14] <- "Med Income"
colnames(table1)[15] <- "Missing Income"
colnames(table1)[16] <- "Black Caribbean"
colnames(table1)[17] <- "Indian"
colnames(table1)[18] <- "Pakistani"
colnames(table1)[19] <- "Bangladeshi"
colnames(table1)[20] <- "Vote Duty"
colnames(table1)[21] <- "Political Efficacy"
colnames(table1)[22] <- "Democratic Satisfaction"
colnames(table1)[23] <- "Trust Parliament"

stargazer(table1, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in General Elections (Societal Discrimination): CDF Comparison - Complete Model"
)

##########################
#### FIGURE 5 (P. 24) ####
##########################

df <- data.frame(old = c("poldisc", "relatt_oth_r", "pol_interest", "polknowledge", "partyid",
                         "identity", "english", "native_born", "female", "age", "education2", "highinc",
                         "medinc", "misinc", "black_caribbean", "indian", "pakistani", "bangladeshi",
                         "vote_duty", "efficacy", "democ_satis", "trust_parliament"), 
                 new = c("Political Discrimination", "Worship Attendance", "Political Interest", 
                         "Political Knowledge", "Party ID", "Identity", "English (Main Lang)", 
                         "Native Born", "Female", "Age", "Education", "High Income", "Med Income", 
                         "Missing Income", "Black Caribbean", "Indian", "Pakistani", "Bangladeshi", 
                         "Vote Duty", "Political Efficacy", "Democratic Satisfaction", 
                         "Trust Parliament")
)

plot_full_probit <- love.plot(m.out.fp, 
                              binary = "std",
                              thresholds = c(m=.1),
                              var.names = df,
                              abs = T,
                              grid = T,
                              line = T,
                              sample.names = c("Unmatched", "Matched"),
                              shapes = c("circle filled", "triangle filled"),
                              colors = c("red", "blue"),
                              title = "Full Probit",
                              wrap = 45,
                              size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_full_logit <- love.plot(m.out.fl, 
                             binary = "std",
                             thresholds = c(m=.1),
                             var.names = df,
                             abs = T,
                             grid = T,
                             line = T,
                             sample.names = c("Unmatched", "Matched"),
                             shapes = c("circle filled", "triangle filled"),
                             colors = c("red", "blue"),
                             title = "Full Logit",
                             wrap = 45,
                             size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_probit <- love.plot(m.out.np, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Nearest Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_logit <- love.plot(m.out.nl, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Nearest Logit",
                                wrap = 45,
                                size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_optimal_probit <- love.plot(m.out.op, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Optimal Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_optimal_logit <- love.plot(m.out.ol, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Optimal Logit",
                                wrap = 45,
                                size = 2.5
) + 
  #ggtitle("<span style='font-size: 10pt;'>Optimal Logit</font>") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_probit)
print(plot_optimal_logit)
#print(plot_mahal)

bplot.ge.sd.c <- grid.arrange(
  plot_full_probit,
  plot_full_logit,
  plot_optimal_probit,
  plot_optimal_logit,
  plot_nearest_probit,
  plot_nearest_logit,
  nrow = 2,
  top = textGrob("Vote in General Elections (Societal Discrimination): Balance Plots - Complete Model",
                 gp = gpar(fontsize = 13, fontface = 1)),
  bottom = textGrob(
    "Individual plot titles indicate method and link function used in the matching procedure.",
    gp = gpar(fontface = 1, fontsize = 8),
    hjust = 1,
    x = 1
  )
)
ggsave("bplot.ge.sd.c.pdf", plot = bplot.ge.sd.c, width = 14, height = 8)

##########################
#### TABLE 16 (P. 24) ####
##########################

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

plot_full_probit <- plot(summary(m.out.fp))
plot_full_logit <- plot(summary(m.out.fl))
plot_nearest_probit <- plot(summary(m.out.np))
plot_nearest_logit <- plot(summary(m.out.nl))
plot_optimal_probit <- plot(summary(m.out.op))
plot_optimal_logit <- plot(summary(m.out.ol))
#plot_mahal <- plot(summary(m.out.fp))

print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_logit)
print(plot_optimal_probit)
#print(plot_mahal)

## Mean CDF table: 
c1<-plot_full_probit$sum.matched[,5]
c2<-plot_full_logit$sum.matched[,5]
c3<-plot_nearest_probit$sum.matched[,5]
c4<-plot_nearest_logit$sum.matched[,5]
c5<-plot_optimal_probit$sum.matched[,5]
c6<-plot_optimal_logit$sum.matched[,5]
#c7<-plot_mahal$sum.matched[,5]

table2<-rbind(c1, c2, c3, c4, c5, c6)
rownames(table2) <- c("Full Probit", "Full Logit", "Nearest Probit", "Nearest Logit", "Optimal Probit", "Optimal Pobit")
table2<-as.data.frame(table2)
colnames(table2)[1] <- "Distance"
colnames(table2)[2] <- "Societal Discrimination"
colnames(table2)[3] <- "Worship Attendance"
colnames(table2)[4] <- "Political Interest"
colnames(table2)[5] <- "Political Knowledge"
colnames(table2)[6] <- "Party ID"
colnames(table2)[7] <- "Identity"
colnames(table2)[8] <- "English (Main Lang)"
colnames(table2)[9] <- "Native Born"
colnames(table2)[10] <- "Female"
colnames(table2)[11] <- "Age"
colnames(table2)[12] <- "Education"
colnames(table2)[13] <- "High Income"
colnames(table2)[14] <- "Med Income"
colnames(table2)[15] <- "Missing Income"
colnames(table2)[16] <- "Black Caribbean"
colnames(table2)[17] <- "Indian"
colnames(table2)[18] <- "Pakistani"
colnames(table2)[19] <- "Bangladeshi"
colnames(table2)[20] <- "Vote Duty"
colnames(table2)[21] <- "Political Efficacy"
colnames(table2)[22] <- "Democratic Satisfaction"
colnames(table2)[23] <- "Trust Parliament"

stargazer(table2, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in General Elections (Political Discrimination): CDF Comparison - Complete Model"
)

##########################
#### FGIURE 6 (P. 25) ####
##########################

df <- data.frame(old = c("socdisc", "relatt_oth_r", "pol_interest", "polknowledge", "partyid",
                         "identity", "english", "native_born", "female", "age", "education2", "highinc",
                         "medinc", "misinc", "black_caribbean", "indian", "pakistani", "bangladeshi",
                         "vote_duty", "efficacy", "democ_satis", "trust_parliament"), 
                 new = c("Societal Discrimination", "Worship Attendance", "Political Interest", 
                         "Political Knowledge", "Party ID", "Identity", "English (Main Lang)", 
                         "Native Born", "Female", "Age", "Education", "High Income", "Med Income", 
                         "Missing Income", "Black Caribbean", "Indian", "Pakistani", "Bangladeshi", 
                         "Vote Duty", "Political Efficacy", "Democratic Satisfaction", 
                         "Trust Parliament")
)

plot_full_probit <- love.plot(m.out.fp, 
                              binary = "std",
                              thresholds = c(m=.1),
                              var.names = df,
                              abs = T,
                              grid = T,
                              line = T,
                              sample.names = c("Unmatched", "Matched"),
                              shapes = c("circle filled", "triangle filled"),
                              colors = c("red", "blue"),
                              title = "Full Probit",
                              wrap = 45,
                              size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_full_logit <- love.plot(m.out.fl, 
                             binary = "std",
                             thresholds = c(m=.1),
                             var.names = df,
                             abs = T,
                             grid = T,
                             line = T,
                             sample.names = c("Unmatched", "Matched"),
                             shapes = c("circle filled", "triangle filled"),
                             colors = c("red", "blue"),
                             title = "Full Logit",
                             wrap = 45,
                             size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_probit <- love.plot(m.out.np, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Nearest Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_logit <- love.plot(m.out.nl, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Nearest Logit",
                                wrap = 45,
                                size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_optimal_probit <- love.plot(m.out.op, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Optimal Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_optimal_logit <- love.plot(m.out.ol, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Optimal Logit",
                                wrap = 45,
                                size = 2.5
) + 
  #ggtitle("<span style='font-size: 10pt;'>Optimal Logit</font>") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_probit)
print(plot_optimal_logit)
#print(plot_mahal)

bplot.ge.pd.c <- grid.arrange(
  plot_full_probit,
  plot_full_logit,
  plot_optimal_probit,
  plot_optimal_logit,
  plot_nearest_probit,
  plot_nearest_logit,
  nrow = 2,
  top = textGrob("Vote in General Elections (Political Discrimination): Balance Plots - Complete Model",
                 gp = gpar(fontsize = 13, fontface = 1)),
  bottom = textGrob(
    "Individual plot titles indicate method and link function used in the matching procedure.",
    gp = gpar(fontface = 1, fontsize = 8),
    hjust = 1,
    x = 1
  )
)
ggsave("bplot.ge.pd.c.pdf", plot = bplot.ge.pd.c, width = 14, height = 8)

##########################
#### TABLE 17 (P. 25) ####
##########################

formula<-(voted2010 ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi)


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

ndata <- mdata %>%
  select(voted2010, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

plot_full_probit <- plot(summary(m.out.fp))
plot_full_logit <- plot(summary(m.out.fl))
plot_nearest_probit <- plot(summary(m.out.np))
plot_nearest_logit <- plot(summary(m.out.nl))
plot_optimal_probit <- plot(summary(m.out.op))
plot_optimal_logit <- plot(summary(m.out.ol))

print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_logit)
print(plot_optimal_probit)

## Mean CDF table: 
c1<-plot_full_probit$sum.matched[,5]
c2<-plot_full_logit$sum.matched[,5]
c3<-plot_nearest_probit$sum.matched[,5]
c4<-plot_nearest_logit$sum.matched[,5]
c5<-plot_optimal_probit$sum.matched[,5]
c6<-plot_optimal_logit$sum.matched[,5]

table3<-rbind(c1, c2, c3, c4, c5, c6)
rownames(table3) <- c("Full Probit", "Full Logit", "Nearest Probit", "Nearest Logit", "Optimal Probit", "Optimal Pobit")
table3<-as.data.frame(table3)
colnames(table3)[1] <- "Distance"
colnames(table3)[2] <- "Political Discrimination"
colnames(table3)[3] <- "Worship Attendance"
colnames(table3)[4] <- "Political Interest"
colnames(table3)[5] <- "Political Knowledge"
colnames(table3)[6] <- "Party ID"
colnames(table3)[7] <- "Identity"
colnames(table3)[8] <- "English (Main Lang)"
colnames(table3)[9] <- "Native Born"
colnames(table3)[10] <- "Female"
colnames(table3)[11] <- "Age"
colnames(table3)[12] <- "Education"
colnames(table3)[13] <- "High Income"
colnames(table3)[14] <- "Med Income"
colnames(table3)[15] <- "Missing Income"
colnames(table3)[16] <- "Black Caribbean"
colnames(table3)[17] <- "Indian"
colnames(table3)[18] <- "Pakistani"
colnames(table3)[19] <- "Bangladeshi"

stargazer(table3, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in General Elections (Societal Discrimination): CDF Comparison - Simplified Model"
)

##########################
#### FIGURE 7 (P. 26) ####
##########################

df <- data.frame(old = c("poldisc", "relatt_oth_r", "pol_interest", "polknowledge", "partyid",
                         "identity", "english", "native_born", "female", "age", "education2", "highinc",
                         "medinc", "misinc", "black_caribbean", "indian", "pakistani", "bangladeshi"
), 
new = c("Political Discrimination", "Worship Attendance", "Political Interest", 
        "Political Knowledge", "Party ID", "Identity", "English (Main Lang)", 
        "Native Born", "Female", "Age", "Education", "High Income", "Med Income", 
        "Missing Income", "Black Caribbean", "Indian", "Pakistani", "Bangladeshi" 
)
)

plot_full_probit <- love.plot(m.out.fp, 
                              binary = "std",
                              thresholds = c(m=.1),
                              var.names = df,
                              abs = T,
                              grid = T,
                              line = T,
                              sample.names = c("Unmatched", "Matched"),
                              shapes = c("circle filled", "triangle filled"),
                              colors = c("red", "blue"),
                              title = "Full Probit",
                              wrap = 45,
                              size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_full_logit <- love.plot(m.out.fl, 
                             binary = "std",
                             thresholds = c(m=.1),
                             var.names = df,
                             abs = T,
                             grid = T,
                             line = T,
                             sample.names = c("Unmatched", "Matched"),
                             shapes = c("circle filled", "triangle filled"),
                             colors = c("red", "blue"),
                             title = "Full Logit",
                             wrap = 45,
                             size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_probit <- love.plot(m.out.np, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Nearest Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_logit <- love.plot(m.out.nl, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Nearest Logit",
                                wrap = 45,
                                size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_optimal_probit <- love.plot(m.out.op, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Optimal Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_optimal_logit <- love.plot(m.out.ol, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Optimal Logit",
                                wrap = 45,
                                size = 2.5
) + 
  #ggtitle("<span style='font-size: 10pt;'>Optimal Logit</font>") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_probit)
print(plot_optimal_logit)
#print(plot_mahal)

bplot.ge.sd.s <- grid.arrange(
  plot_full_probit,
  plot_full_logit,
  plot_optimal_probit,
  plot_optimal_logit,
  plot_nearest_probit,
  plot_nearest_logit,
  nrow = 2,
  top = textGrob("Vote in General Elections (Societal Discrimination): Balance Plots - Simplified Model",
                 gp = gpar(fontsize = 13, fontface = 1)),
  bottom = textGrob(
    "Individual plot titles indicate method and link function used in the matching procedure.",
    gp = gpar(fontface = 1, fontsize = 8),
    hjust = 1,
    x = 1
  )
)
ggsave("bplot.ge.sd.s.pdf", plot = bplot.ge.sd.s, width = 14, height = 8)

##########################
#### TABLE 18 (P. 26) ####
##########################

ndata <- mdata %>%
  select(voted2010, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

### MATCHING DATASET TO BE USED: full matching with probit link
m.out.fp <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

### OTHER TYPES OF MATCHING JUST FOR COMPARISON

m.out.fl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

plot_full_probit <- plot(summary(m.out.fp))
plot_full_logit <- plot(summary(m.out.fl))
plot_nearest_probit <- plot(summary(m.out.np))
plot_nearest_logit <- plot(summary(m.out.nl))
plot_optimal_probit <- plot(summary(m.out.op))
plot_optimal_logit <- plot(summary(m.out.ol))
#plot_mahal <- plot(summary(m.out.fp))

print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_logit)
print(plot_optimal_probit)
#print(plot_mahal)

## Mean CDF table: 
c1<-plot_full_probit$sum.matched[,5]
c2<-plot_full_logit$sum.matched[,5]
c3<-plot_nearest_probit$sum.matched[,5]
c4<-plot_nearest_logit$sum.matched[,5]
c5<-plot_optimal_probit$sum.matched[,5]
c6<-plot_optimal_logit$sum.matched[,5]
#c7<-plot_mahal$sum.matched[,5]

table4<-rbind(c1, c2, c3, c4, c5, c6)
rownames(table4) <- c("Full Probit", "Full Logit", "Nearest Probit", "Nearest Logit", "Optimal Probit", "Optimal Pobit")
table4<-as.data.frame(table4)
colnames(table4)[1] <- "Distance"
colnames(table4)[2] <- "Societal Discrimination"
colnames(table4)[3] <- "Worship Attendance"
colnames(table4)[4] <- "Political Interest"
colnames(table4)[5] <- "Political Knowledge"
colnames(table4)[6] <- "Party ID"
colnames(table4)[7] <- "Identity"
colnames(table4)[8] <- "English (Main Lang)"
colnames(table4)[9] <- "Native Born"
colnames(table4)[10] <- "Female"
colnames(table4)[11] <- "Age"
colnames(table4)[12] <- "Education"
colnames(table4)[13] <- "High Income"
colnames(table4)[14] <- "Med Income"
colnames(table4)[15] <- "Missing Income"
colnames(table4)[16] <- "Black Caribbean"
colnames(table4)[17] <- "Indian"
colnames(table4)[18] <- "Pakistani"
colnames(table4)[19] <- "Bangladeshi"

stargazer(table4, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in General Elections (Political Discrimination): CDF Comparison - Simplified Model"
)

##########################
#### FIGURE 8 (P. 27) ####
##########################

df <- data.frame(old = c("socdisc", "relatt_oth_r", "pol_interest", "polknowledge", "partyid",
                         "identity", "english", "native_born", "female", "age", "education2", "highinc",
                         "medinc", "misinc", "black_caribbean", "indian", "pakistani", "bangladeshi"
), 
new = c("Societal Discrimination", "Worship Attendance", "Political Interest", 
        "Political Knowledge", "Party ID", "Identity", "English (Main Lang)", 
        "Native Born", "Female", "Age", "Education", "High Income", "Med Income", 
        "Missing Income", "Black Caribbean", "Indian", "Pakistani", "Bangladeshi" 
)
)

plot_full_probit <- love.plot(m.out.fp, 
                              binary = "std",
                              thresholds = c(m=.1),
                              var.names = df,
                              abs = T,
                              grid = T,
                              line = T,
                              sample.names = c("Unmatched", "Matched"),
                              shapes = c("circle filled", "triangle filled"),
                              colors = c("red", "blue"),
                              title = "Full Probit",
                              wrap = 45,
                              size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_full_logit <- love.plot(m.out.fl, 
                             binary = "std",
                             thresholds = c(m=.1),
                             var.names = df,
                             abs = T,
                             grid = T,
                             line = T,
                             sample.names = c("Unmatched", "Matched"),
                             shapes = c("circle filled", "triangle filled"),
                             colors = c("red", "blue"),
                             title = "Full Logit",
                             wrap = 45,
                             size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_probit <- love.plot(m.out.np, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Nearest Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_logit <- love.plot(m.out.nl, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Nearest Logit",
                                wrap = 45,
                                size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_optimal_probit <- love.plot(m.out.op, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Optimal Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_optimal_logit <- love.plot(m.out.ol, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Optimal Logit",
                                wrap = 45,
                                size = 2.5
) + 
  #ggtitle("<span style='font-size: 10pt;'>Optimal Logit</font>") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_probit)
print(plot_optimal_logit)
#print(plot_mahal)

bplot.ge.pd.s <- grid.arrange(
  plot_full_probit,
  plot_full_logit,
  plot_optimal_probit,
  plot_optimal_logit,
  plot_nearest_probit,
  plot_nearest_logit,
  nrow = 2,
  top = textGrob("Vote in General Elections (Political Discrimination): Balance Plots - Simplified Model",
                 gp = gpar(fontsize = 13, fontface = 1)),
  bottom = textGrob(
    "Individual plot titles indicate method and link function used in the matching procedure.",
    gp = gpar(fontface = 1, fontsize = 8),
    hjust = 1,
    x = 1
  )
)
ggsave("bplot.ge.pd.s.pdf", plot = bplot.ge.pd.s, width = 14, height = 8)

##########################
#### TABLE 19 (P. 27) ####
##########################

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

plot_full_probit <- plot(summary(m.out.fp))
plot_full_logit <- plot(summary(m.out.fl))
plot_nearest_probit <- plot(summary(m.out.np))
plot_nearest_logit <- plot(summary(m.out.nl))
plot_optimal_probit <- plot(summary(m.out.op))
plot_optimal_logit <- plot(summary(m.out.ol))

print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_logit)
print(plot_optimal_probit)

## Mean CDF table: 
c1<-plot_full_probit$sum.matched[,5]
c2<-plot_full_logit$sum.matched[,5]
c3<-plot_nearest_probit$sum.matched[,5]
c4<-plot_nearest_logit$sum.matched[,5]
c5<-plot_optimal_probit$sum.matched[,5]
c6<-plot_optimal_logit$sum.matched[,5]

table5<-rbind(c1, c2, c3, c4, c5, c6)
rownames(table5) <- c("Full Probit", "Full Logit", "Nearest Probit", "Nearest Logit", "Optimal Probit", "Optimal Pobit")
table1<-as.data.frame(table5)
colnames(table5)[1] <- "Distance"
colnames(table5)[2] <- "Political Discrimination"
colnames(table5)[3] <- "Worship Attendance"
colnames(table5)[4] <- "Political Interest"
colnames(table5)[5] <- "Political Knowledge"
colnames(table5)[6] <- "Party ID"
colnames(table5)[7] <- "Identity"
colnames(table5)[8] <- "English (Main Lang)"
colnames(table5)[9] <- "Native Born"
colnames(table5)[10] <- "Female"
colnames(table5)[11] <- "Age"
colnames(table5)[12] <- "Education"
colnames(table5)[13] <- "High Income"
colnames(table5)[14] <- "Med Income"
colnames(table5)[15] <- "Missing Income"
colnames(table5)[16] <- "Black Caribbean"
colnames(table5)[17] <- "Indian"
colnames(table5)[18] <- "Pakistani"
colnames(table5)[19] <- "Bangladeshi"
colnames(table5)[20] <- "Vote Duty"
colnames(table5)[21] <- "Political Efficacy"
colnames(table5)[22] <- "Democratic Satisfaction"
colnames(table5)[23] <- "Trust Parliament"

stargazer(table5, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in Local Elections (Societal Discrimination): CDF Comparison - Complete Model"
)

##########################
#### FIGURE 9 (P. 28) ####
##########################

df <- data.frame(old = c("poldisc", "relatt_oth_r", "pol_interest", "polknowledge", "partyid",
                         "identity", "english", "native_born", "female", "age", "education2", "highinc",
                         "medinc", "misinc", "black_caribbean", "indian", "pakistani", "bangladeshi",
                         "vote_duty", "efficacy", "democ_satis", "trust_parliament"), 
                 new = c("Political Discrimination", "Worship Attendance", "Political Interest", 
                         "Political Knowledge", "Party ID", "Identity", "English (Main Lang)", 
                         "Native Born", "Female", "Age", "Education", "High Income", "Med Income", 
                         "Missing Income", "Black Caribbean", "Indian", "Pakistani", "Bangladeshi", 
                         "Vote Duty", "Political Efficacy", "Democratic Satisfaction", 
                         "Trust Parliament")
)

plot_full_probit <- love.plot(m.out.fp, 
                              binary = "std",
                              thresholds = c(m=.1),
                              var.names = df,
                              abs = T,
                              grid = T,
                              line = T,
                              sample.names = c("Unmatched", "Matched"),
                              shapes = c("circle filled", "triangle filled"),
                              colors = c("red", "blue"),
                              title = "Full Probit",
                              wrap = 45,
                              size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_full_logit <- love.plot(m.out.fl, 
                             binary = "std",
                             thresholds = c(m=.1),
                             var.names = df,
                             abs = T,
                             grid = T,
                             line = T,
                             sample.names = c("Unmatched", "Matched"),
                             shapes = c("circle filled", "triangle filled"),
                             colors = c("red", "blue"),
                             title = "Full Logit",
                             wrap = 45,
                             size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_probit <- love.plot(m.out.np, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Nearest Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_logit <- love.plot(m.out.nl, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Nearest Logit",
                                wrap = 45,
                                size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_optimal_probit <- love.plot(m.out.op, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Optimal Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_optimal_logit <- love.plot(m.out.ol, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Optimal Logit",
                                wrap = 45,
                                size = 2.5
) + 
  #ggtitle("<span style='font-size: 10pt;'>Optimal Logit</font>") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_probit)
print(plot_optimal_logit)
#print(plot_mahal)

bplot.le.sd.c <- grid.arrange(
  plot_full_probit,
  plot_full_logit,
  plot_optimal_probit,
  plot_optimal_logit,
  plot_nearest_probit,
  plot_nearest_logit,
  nrow = 2,
  top = textGrob("Vote in Local Elections (Societal Discrimination): Balance Plots - Complete Model",
                 gp = gpar(fontsize = 13, fontface = 1)),
  bottom = textGrob(
    "Individual plot titles indicate method and link function used in the matching procedure.",
    gp = gpar(fontface = 1, fontsize = 8),
    hjust = 1,
    x = 1
  )
)
ggsave("bplot.le.sd.c.pdf", plot = bplot.le.sd.c, width = 14, height = 8)

##########################
#### TABLE 20 (P. 28) ####
##########################

ndata <- mdata %>%
  select(voted2010_local, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, vote_duty, efficacy, democ_satis, trust_parliament) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

### MATCHING DATASET TO BE USED: full matching with probit link
m.out.fp <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                      vote_duty+ efficacy+ democ_satis+ trust_parliament,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

### OTHER TYPES OF MATCHING JUST FOR COMPARISON

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

plot_full_probit <- plot(summary(m.out.fp))
plot_full_logit <- plot(summary(m.out.fl))
plot_nearest_probit <- plot(summary(m.out.np))
plot_nearest_logit <- plot(summary(m.out.nl))
plot_optimal_probit <- plot(summary(m.out.op))
plot_optimal_logit <- plot(summary(m.out.ol))
#plot_mahal <- plot(summary(m.out.fp))

print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_logit)
print(plot_optimal_probit)
#print(plot_mahal)

## Mean CDF table: 
c1<-plot_full_probit$sum.matched[,5]
c2<-plot_full_logit$sum.matched[,5]
c3<-plot_nearest_probit$sum.matched[,5]
c4<-plot_nearest_logit$sum.matched[,5]
c5<-plot_optimal_probit$sum.matched[,5]
c6<-plot_optimal_logit$sum.matched[,5]
#c7<-plot_mahal$sum.matched[,5]

table6<-rbind(c1, c2, c3, c4, c5, c6)
rownames(table6) <- c("Full Probit", "Full Logit", "Nearest Probit", "Nearest Logit", "Optimal Probit", "Optimal Pobit")
table6<-as.data.frame(table6)
colnames(table6)[1] <- "Distance"
colnames(table6)[2] <- "Societal Discrimination"
colnames(table6)[3] <- "Worship Attendance"
colnames(table6)[4] <- "Political Interest"
colnames(table6)[5] <- "Political Knowledge"
colnames(table6)[6] <- "Party ID"
colnames(table6)[7] <- "Identity"
colnames(table6)[8] <- "English (Main Lang)"
colnames(table6)[9] <- "Native Born"
colnames(table6)[10] <- "Female"
colnames(table6)[11] <- "Age"
colnames(table6)[12] <- "Education"
colnames(table6)[13] <- "High Income"
colnames(table6)[14] <- "Med Income"
colnames(table6)[15] <- "Missing Income"
colnames(table6)[16] <- "Black Caribbean"
colnames(table6)[17] <- "Indian"
colnames(table6)[18] <- "Pakistani"
colnames(table6)[19] <- "Bangladeshi"
colnames(table6)[20] <- "Vote Duty"
colnames(table6)[21] <- "Political Efficacy"
colnames(table6)[22] <- "Democratic Satisfaction"
colnames(table6)[23] <- "Trust Parliament"

stargazer(table6, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in Local Elections (Political Discrimination): CDF Comparison - Complete Model"
)

##########################
#### FIGURE 10 (P. 29) ###
##########################

df <- data.frame(old = c("socdisc", "relatt_oth_r", "pol_interest", "polknowledge", "partyid",
                         "identity", "english", "native_born", "female", "age", "education2", "highinc",
                         "medinc", "misinc", "black_caribbean", "indian", "pakistani", "bangladeshi",
                         "vote_duty", "efficacy", "democ_satis", "trust_parliament"), 
                 new = c("Societal Discrimination", "Worship Attendance", "Political Interest", 
                         "Political Knowledge", "Party ID", "Identity", "English (Main Lang)", 
                         "Native Born", "Female", "Age", "Education", "High Income", "Med Income", 
                         "Missing Income", "Black Caribbean", "Indian", "Pakistani", "Bangladeshi", 
                         "Vote Duty", "Political Efficacy", "Democratic Satisfaction", 
                         "Trust Parliament")
)

plot_full_probit <- love.plot(m.out.fp, 
                              binary = "std",
                              thresholds = c(m=.1),
                              var.names = df,
                              abs = T,
                              grid = T,
                              line = T,
                              sample.names = c("Unmatched", "Matched"),
                              shapes = c("circle filled", "triangle filled"),
                              colors = c("red", "blue"),
                              title = "Full Probit",
                              wrap = 45,
                              size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_full_logit <- love.plot(m.out.fl, 
                             binary = "std",
                             thresholds = c(m=.1),
                             var.names = df,
                             abs = T,
                             grid = T,
                             line = T,
                             sample.names = c("Unmatched", "Matched"),
                             shapes = c("circle filled", "triangle filled"),
                             colors = c("red", "blue"),
                             title = "Full Logit",
                             wrap = 45,
                             size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_probit <- love.plot(m.out.np, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Nearest Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_logit <- love.plot(m.out.nl, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Nearest Logit",
                                wrap = 45,
                                size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_optimal_probit <- love.plot(m.out.op, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Optimal Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_optimal_logit <- love.plot(m.out.ol, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Optimal Logit",
                                wrap = 45,
                                size = 2.5
) + 
  #ggtitle("<span style='font-size: 10pt;'>Optimal Logit</font>") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_probit)
print(plot_optimal_logit)
#print(plot_mahal)

bplot.le.pd.c <- grid.arrange(
  plot_full_probit,
  plot_full_logit,
  plot_optimal_probit,
  plot_optimal_logit,
  plot_nearest_probit,
  plot_nearest_logit,
  nrow = 2,
  top = textGrob("Vote in Local Elections (Political Discrimination): Balance Plots - Complete Model",
                 gp = gpar(fontsize = 13, fontface = 1)),
  bottom = textGrob(
    "Individual plot titles indicate method and link function used in the matching procedure.",
    gp = gpar(fontface = 1, fontsize = 8),
    hjust = 1,
    x = 1
  )
)
ggsave("bplot.le.pd.c.pdf", plot = bplot.le.pd.c, width = 14, height = 8)

##########################
#### TABLE 21 (P. 29) ####
##########################

formula<-(voted2010_local ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi)


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

ndata <- mdata %>%
  select(voted2010_local, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

plot_full_probit <- plot(summary(m.out.fp))
plot_full_logit <- plot(summary(m.out.fl))
plot_nearest_probit <- plot(summary(m.out.np))
plot_nearest_logit <- plot(summary(m.out.nl))
plot_optimal_probit <- plot(summary(m.out.op))
plot_optimal_logit <- plot(summary(m.out.ol))

print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_logit)
print(plot_optimal_probit)

## Mean CDF table: 
c1<-plot_full_probit$sum.matched[,5]
c2<-plot_full_logit$sum.matched[,5]
c3<-plot_nearest_probit$sum.matched[,5]
c4<-plot_nearest_logit$sum.matched[,5]
c5<-plot_optimal_probit$sum.matched[,5]
c6<-plot_optimal_logit$sum.matched[,5]

table7<-rbind(c1, c2, c3, c4, c5, c6)
rownames(table7) <- c("Full Probit", "Full Logit", "Nearest Probit", "Nearest Logit", "Optimal Probit", "Optimal Pobit")
table7<-as.data.frame(table7)
colnames(table7)[1] <- "Distance"
colnames(table7)[2] <- "Political Discrimination"
colnames(table7)[3] <- "Worship Attendance"
colnames(table7)[4] <- "Political Interest"
colnames(table7)[5] <- "Political Knowledge"
colnames(table7)[6] <- "Party ID"
colnames(table7)[7] <- "Identity"
colnames(table7)[8] <- "English (Main Lang)"
colnames(table7)[9] <- "Native Born"
colnames(table7)[10] <- "Female"
colnames(table7)[11] <- "Age"
colnames(table7)[12] <- "Education"
colnames(table7)[13] <- "High Income"
colnames(table7)[14] <- "Med Income"
colnames(table7)[15] <- "Missing Income"
colnames(table7)[16] <- "Black Caribbean"
colnames(table7)[17] <- "Indian"
colnames(table7)[18] <- "Pakistani"
colnames(table7)[19] <- "Bangladeshi"

stargazer(table7, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in Local Elections (Societal Discrimination): CDF Comparison - Simplified Model"
)

##########################
#### FIGURE 11 (P. 30) ###
##########################

df <- data.frame(old = c("poldisc", "relatt_oth_r", "pol_interest", "polknowledge", "partyid",
                         "identity", "english", "native_born", "female", "age", "education2", "highinc",
                         "medinc", "misinc", "black_caribbean", "indian", "pakistani", "bangladeshi"
), 
new = c("Political Discrimination", "Worship Attendance", "Political Interest", 
        "Political Knowledge", "Party ID", "Identity", "English (Main Lang)", 
        "Native Born", "Female", "Age", "Education", "High Income", "Med Income", 
        "Missing Income", "Black Caribbean", "Indian", "Pakistani", "Bangladeshi" 
)
)

plot_full_probit <- love.plot(m.out.fp, 
                              binary = "std",
                              thresholds = c(m=.1),
                              var.names = df,
                              abs = T,
                              grid = T,
                              line = T,
                              sample.names = c("Unmatched", "Matched"),
                              shapes = c("circle filled", "triangle filled"),
                              colors = c("red", "blue"),
                              title = "Full Probit",
                              wrap = 45,
                              size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_full_logit <- love.plot(m.out.fl, 
                             binary = "std",
                             thresholds = c(m=.1),
                             var.names = df,
                             abs = T,
                             grid = T,
                             line = T,
                             sample.names = c("Unmatched", "Matched"),
                             shapes = c("circle filled", "triangle filled"),
                             colors = c("red", "blue"),
                             title = "Full Logit",
                             wrap = 45,
                             size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_probit <- love.plot(m.out.np, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Nearest Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_logit <- love.plot(m.out.nl, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Nearest Logit",
                                wrap = 45,
                                size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_optimal_probit <- love.plot(m.out.op, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Optimal Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_optimal_logit <- love.plot(m.out.ol, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Optimal Logit",
                                wrap = 45,
                                size = 2.5
) + 
  #ggtitle("<span style='font-size: 10pt;'>Optimal Logit</font>") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_probit)
print(plot_optimal_logit)
#print(plot_mahal)

bplot.le.sd.s <- grid.arrange(
  plot_full_probit,
  plot_full_logit,
  plot_optimal_probit,
  plot_optimal_logit,
  plot_nearest_probit,
  plot_nearest_logit,
  nrow = 2,
  top = textGrob("Vote in Local Elections (Societal Discrimination): Balance Plots - Simplified Model",
                 gp = gpar(fontsize = 13, fontface = 1)),
  bottom = textGrob(
    "Individual plot titles indicate method and link function used in the matching procedure.",
    gp = gpar(fontface = 1, fontsize = 8),
    hjust = 1,
    x = 1
  )
)
ggsave("bplot.le.sd.s.pdf", plot = bplot.le.sd.s, width = 14, height = 8)

##########################
#### TABLE 22 (P. 30) ####
##########################

ndata <- mdata %>%
  select(voted2010_local, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

### MATCHING DATASET TO BE USED: full matching with probit link
m.out.fp <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

### OTHER TYPES OF MATCHING JUST FOR COMPARISON

m.out.fl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

plot_full_probit <- plot(summary(m.out.fp))
plot_full_logit <- plot(summary(m.out.fl))
plot_nearest_probit <- plot(summary(m.out.np))
plot_nearest_logit <- plot(summary(m.out.nl))
plot_optimal_probit <- plot(summary(m.out.op))
plot_optimal_logit <- plot(summary(m.out.ol))
#plot_mahal <- plot(summary(m.out.fp))

print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_logit)
print(plot_optimal_probit)
#print(plot_mahal)

## Mean CDF table: 
c1<-plot_full_probit$sum.matched[,5]
c2<-plot_full_logit$sum.matched[,5]
c3<-plot_nearest_probit$sum.matched[,5]
c4<-plot_nearest_logit$sum.matched[,5]
c5<-plot_optimal_probit$sum.matched[,5]
c6<-plot_optimal_logit$sum.matched[,5]
#c7<-plot_mahal$sum.matched[,5]

table8<-rbind(c1, c2, c3, c4, c5, c6)
rownames(table8) <- c("Full Probit", "Full Logit", "Nearest Probit", "Nearest Logit", "Optimal Probit", "Optimal Pobit")
table8<-as.data.frame(table8)
colnames(table8)[1] <- "Distance"
colnames(table8)[2] <- "Societal Discrimination"
colnames(table8)[3] <- "Worship Attendance"
colnames(table8)[4] <- "Political Interest"
colnames(table8)[5] <- "Political Knowledge"
colnames(table8)[6] <- "Party ID"
colnames(table8)[7] <- "Identity"
colnames(table8)[8] <- "English (Main Lang)"
colnames(table8)[9] <- "Native Born"
colnames(table8)[10] <- "Female"
colnames(table8)[11] <- "Age"
colnames(table8)[12] <- "Education"
colnames(table8)[13] <- "High Income"
colnames(table8)[14] <- "Med Income"
colnames(table8)[15] <- "Missing Income"
colnames(table8)[16] <- "Black Caribbean"
colnames(table8)[17] <- "Indian"
colnames(table8)[18] <- "Pakistani"
colnames(table8)[19] <- "Bangladeshi"

stargazer(table8, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in Local Elections (Political Discrimination): CDF Comparison - Simplified Model"
)

##########################
#### FIGURE 12 (P. 31) ###
##########################

df <- data.frame(old = c("socdisc", "relatt_oth_r", "pol_interest", "polknowledge", "partyid",
                         "identity", "english", "native_born", "female", "age", "education2", "highinc",
                         "medinc", "misinc", "black_caribbean", "indian", "pakistani", "bangladeshi"
), 
new = c("Societal Discrimination", "Worship Attendance", "Political Interest", 
        "Political Knowledge", "Party ID", "Identity", "English (Main Lang)", 
        "Native Born", "Female", "Age", "Education", "High Income", "Med Income", 
        "Missing Income", "Black Caribbean", "Indian", "Pakistani", "Bangladeshi" 
)
)

plot_full_probit <- love.plot(m.out.fp, 
                              binary = "std",
                              thresholds = c(m=.1),
                              var.names = df,
                              abs = T,
                              grid = T,
                              line = T,
                              sample.names = c("Unmatched", "Matched"),
                              shapes = c("circle filled", "triangle filled"),
                              colors = c("red", "blue"),
                              title = "Full Probit",
                              wrap = 45,
                              size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_full_logit <- love.plot(m.out.fl, 
                             binary = "std",
                             thresholds = c(m=.1),
                             var.names = df,
                             abs = T,
                             grid = T,
                             line = T,
                             sample.names = c("Unmatched", "Matched"),
                             shapes = c("circle filled", "triangle filled"),
                             colors = c("red", "blue"),
                             title = "Full Logit",
                             wrap = 45,
                             size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_probit <- love.plot(m.out.np, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Nearest Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_logit <- love.plot(m.out.nl, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Nearest Logit",
                                wrap = 45,
                                size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_optimal_probit <- love.plot(m.out.op, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Optimal Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_optimal_logit <- love.plot(m.out.ol, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Optimal Logit",
                                wrap = 45,
                                size = 2.5
) + 
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_probit)
print(plot_optimal_logit)

bplot.le.pd.s <- grid.arrange(
  plot_full_probit,
  plot_full_logit,
  plot_optimal_probit,
  plot_optimal_logit,
  plot_nearest_probit,
  plot_nearest_logit,
  nrow = 2,
  top = textGrob("Vote in Local Elections (Political Discrimination): Balance Plots - Simplified Model",
                 gp = gpar(fontsize = 13, fontface = 1)),
  bottom = textGrob(
    "Individual plot titles indicate method and link function used in the matching procedure.",
    gp = gpar(fontface = 1, fontsize = 8),
    hjust = 1,
    x = 1
  )
)
ggsave("bplot.le.pd.s.pdf", plot = bplot.le.pd.s, width = 14, height = 8)


##########################
#### TABLE 23 (P. 31) ####
##########################

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


ndata <- mdata %>%
  select(ethnic_active, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, vote_duty, efficacy, democ_satis, trust_parliament) %>%
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

plot_full_probit <- plot(summary(m.out.fp))
plot_full_logit <- plot(summary(m.out.fl))
plot_nearest_probit <- plot(summary(m.out.np))
plot_nearest_logit <- plot(summary(m.out.nl))
plot_optimal_probit <- plot(summary(m.out.op))
plot_optimal_logit <- plot(summary(m.out.ol))
#plot_mahal <- plot(summary(m.out.fp))

print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_logit)
print(plot_optimal_probit)
#print(plot_mahal)

## Mean CDF table: 
c1<-plot_full_probit$sum.matched[,5]
c2<-plot_full_logit$sum.matched[,5]
c3<-plot_nearest_probit$sum.matched[,5]
c4<-plot_nearest_logit$sum.matched[,5]
c5<-plot_optimal_probit$sum.matched[,5]
c6<-plot_optimal_logit$sum.matched[,5]
#c7<-plot_mahal$sum.matched[,5]

table9<-rbind(c1, c2, c3, c4, c5, c6)
rownames(table9) <- c("Full Probit", "Full Logit", "Nearest Probit", "Nearest Logit", "Optimal Probit", "Optimal Pobit")
table9<-as.data.frame(table9)
colnames(table9)[1] <- "Distance"
colnames(table9)[2] <- "Political Discrimination"
colnames(table9)[3] <- "Worship Attendance"
colnames(table9)[4] <- "Political Interest"
colnames(table9)[5] <- "Political Knowledge"
colnames(table9)[6] <- "Party ID"
colnames(table9)[7] <- "Identity"
colnames(table9)[8] <- "English (Main Lang)"
colnames(table9)[9] <- "Native Born"
colnames(table9)[10] <- "Female"
colnames(table9)[11] <- "Age"
colnames(table9)[12] <- "Education"
colnames(table9)[13] <- "High Income"
colnames(table9)[14] <- "Med Income"
colnames(table9)[15] <- "Missing Income"
colnames(table9)[16] <- "Black Caribbean"
colnames(table9)[17] <- "Indian"
colnames(table9)[18] <- "Pakistani"
colnames(table9)[19] <- "Bangladeshi"
colnames(table9)[20] <- "Vote Duty"
colnames(table9)[21] <- "Political Efficacy"
colnames(table9)[22] <- "Democratic Satisfaction"
colnames(table9)[23] <- "Trust Parliament"

stargazer(table9, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Ethnic Based Engagement (Societal Discrimination): CDF Comparison - Complete Models"
)

##########################
#### FIGURE 13 (P. 32) ###
##########################

df <- data.frame(old = c("poldisc", "relatt_oth_r", "pol_interest", "polknowledge", "partyid",
                         "identity", "english", "native_born", "female", "age", "education2", "highinc",
                         "medinc", "misinc", "black_caribbean", "indian", "pakistani", "bangladeshi",
                         "vote_duty", "efficacy", "democ_satis", "trust_parliament"), 
                 new = c("Political Discrimination", "Worship Attendance", "Political Interest", 
                         "Political Knowledge", "Party ID", "Identity", "English (Main Lang)", 
                         "Native Born", "Female", "Age", "Education", "High Income", "Med Income", 
                         "Missing Income", "Black Caribbean", "Indian", "Pakistani", "Bangladeshi", 
                         "Vote Duty", "Political Efficacy", "Democratic Satisfaction", 
                         "Trust Parliament")
)

plot_full_probit <- love.plot(m.out.fp, 
                              binary = "std",
                              thresholds = c(m=.1),
                              var.names = df,
                              abs = T,
                              grid = T,
                              line = T,
                              sample.names = c("Unmatched", "Matched"),
                              shapes = c("circle filled", "triangle filled"),
                              colors = c("red", "blue"),
                              title = "Full Probit",
                              wrap = 45,
                              size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_full_logit <- love.plot(m.out.fl, 
                             binary = "std",
                             thresholds = c(m=.1),
                             var.names = df,
                             abs = T,
                             grid = T,
                             line = T,
                             sample.names = c("Unmatched", "Matched"),
                             shapes = c("circle filled", "triangle filled"),
                             colors = c("red", "blue"),
                             title = "Full Logit",
                             wrap = 45,
                             size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_probit <- love.plot(m.out.np, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Nearest Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_logit <- love.plot(m.out.nl, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Nearest Logit",
                                wrap = 45,
                                size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_optimal_probit <- love.plot(m.out.op, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Optimal Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_optimal_logit <- love.plot(m.out.ol, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Optimal Logit",
                                wrap = 45,
                                size = 2.5
) + 
  #ggtitle("<span style='font-size: 10pt;'>Optimal Logit</font>") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_probit)
print(plot_optimal_logit)
#print(plot_mahal)

bplot.ebe.sd.c <- grid.arrange(
  plot_full_probit,
  plot_full_logit,
  plot_optimal_probit,
  plot_optimal_logit,
  plot_nearest_probit,
  plot_nearest_logit,
  nrow = 2,
  top = textGrob("Ethnic Based Engagement (Societal Discrimination): Balance Plots - Complete Model",
                 gp = gpar(fontsize = 13, fontface = 1)),
  bottom = textGrob(
    "Individual plot titles indicate method and link function used in the matching procedure.",
    gp = gpar(fontface = 1, fontsize = 8),
    hjust = 1,
    x = 1
  )
)
ggsave("bplot.ebe.sd.c.pdf", plot = bplot.ebe.sd.c, width = 14, height = 8)

##########################
#### TABLE 24 (P. 32) ####
##########################

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

plot_full_probit <- plot(summary(m.out.fp))
plot_full_logit <- plot(summary(m.out.fl))
plot_nearest_probit <- plot(summary(m.out.np))
plot_nearest_logit <- plot(summary(m.out.nl))
plot_optimal_probit <- plot(summary(m.out.op))
plot_optimal_logit <- plot(summary(m.out.ol))

print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_logit)
print(plot_optimal_probit)

## Mean CDF table: 
c1<-plot_full_probit$sum.matched[,5]
c2<-plot_full_logit$sum.matched[,5]
c3<-plot_nearest_probit$sum.matched[,5]
c4<-plot_nearest_logit$sum.matched[,5]
c5<-plot_optimal_probit$sum.matched[,5]
c6<-plot_optimal_logit$sum.matched[,5]

table10<-rbind(c1, c2, c3, c4, c5, c6)
rownames(table10) <- c("Full Probit", "Full Logit", "Nearest Probit", "Nearest Logit", "Optimal Probit", "Optimal Pobit")
table10<-as.data.frame(table10)
colnames(table10)[1] <- "Distance"
colnames(table10)[2] <- "Societal Discrimination"
colnames(table10)[3] <- "Worship Attendance"
colnames(table10)[4] <- "Political Interest"
colnames(table10)[5] <- "Political Knowledge"
colnames(table10)[6] <- "Party ID"
colnames(table10)[7] <- "Identity"
colnames(table10)[8] <- "English (Main Lang)"
colnames(table10)[9] <- "Native Born"
colnames(table10)[10] <- "Female"
colnames(table10)[11] <- "Age"
colnames(table10)[12] <- "Education"
colnames(table10)[13] <- "High Income"
colnames(table10)[14] <- "Med Income"
colnames(table10)[15] <- "Missing Income"
colnames(table10)[16] <- "Black Caribbean"
colnames(table10)[17] <- "Indian"
colnames(table10)[18] <- "Pakistani"
colnames(table10)[19] <- "Bangladeshi"
colnames(table10)[20] <- "Vote Duty"
colnames(table10)[21] <- "Political Efficacy"
colnames(table10)[22] <- "Democratic Satisfaction"
colnames(table10)[23] <- "Trust Parliament"

stargazer(table10, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Ethnic Based Engagement (Political Discrimination): CDF Comparison - Complete Model"
)

##########################
#### FIGURE 14 (P. 33) ###
##########################

df <- data.frame(old = c("socdisc", "relatt_oth_r", "pol_interest", "polknowledge", "partyid",
                         "identity", "english", "native_born", "female", "age", "education2", "highinc",
                         "medinc", "misinc", "black_caribbean", "indian", "pakistani", "bangladeshi",
                         "vote_duty", "efficacy", "democ_satis", "trust_parliament"), 
                 new = c("Societal Discrimination", "Worship Attendance", "Political Interest", 
                         "Political Knowledge", "Party ID", "Identity", "English (Main Lang)", 
                         "Native Born", "Female", "Age", "Education", "High Income", "Med Income", 
                         "Missing Income", "Black Caribbean", "Indian", "Pakistani", "Bangladeshi", 
                         "Vote Duty", "Political Efficacy", "Democratic Satisfaction", 
                         "Trust Parliament")
)

plot_full_probit <- love.plot(m.out.fp, 
                              binary = "std",
                              thresholds = c(m=.1),
                              var.names = df,
                              abs = T,
                              grid = T,
                              line = T,
                              sample.names = c("Unmatched", "Matched"),
                              shapes = c("circle filled", "triangle filled"),
                              colors = c("red", "blue"),
                              title = "Full Probit",
                              wrap = 45,
                              size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_full_logit <- love.plot(m.out.fl, 
                             binary = "std",
                             thresholds = c(m=.1),
                             var.names = df,
                             abs = T,
                             grid = T,
                             line = T,
                             sample.names = c("Unmatched", "Matched"),
                             shapes = c("circle filled", "triangle filled"),
                             colors = c("red", "blue"),
                             title = "Full Logit",
                             wrap = 45,
                             size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_probit <- love.plot(m.out.np, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Nearest Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_logit <- love.plot(m.out.nl, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Nearest Logit",
                                wrap = 45,
                                size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_optimal_probit <- love.plot(m.out.op, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Optimal Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_optimal_logit <- love.plot(m.out.ol, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Optimal Logit",
                                wrap = 45,
                                size = 2.5
) + 
  #ggtitle("<span style='font-size: 10pt;'>Optimal Logit</font>") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_probit)
print(plot_optimal_logit)
#print(plot_mahal)

bplot.ebe.pd.c <- grid.arrange(
  plot_full_probit,
  plot_full_logit,
  plot_optimal_probit,
  plot_optimal_logit,
  plot_nearest_probit,
  plot_nearest_logit,
  nrow = 2,
  top = textGrob("Ethnic Based Engagement (Political Discrimination): Balance Plots - Complete Model",
                 gp = gpar(fontsize = 13, fontface = 1)),
  bottom = textGrob(
    "Individual plot titles indicate method and link function used in the matching procedure.",
    gp = gpar(fontface = 1, fontsize = 8),
    hjust = 1,
    x = 1
  )
)
ggsave("bplot.ebe.pd.c.pdf", plot = bplot.ebe.pd.c, width = 14, height = 8)

##########################
#### TABLE 25 (P. 33) ####
##########################

formula<-(ethnic_active ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi)


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
  select(ethnic_active, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

### MATCHING DATASET TO BE USED: full matching with probit link
m.out.fp <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

### OTHER TYPES OF MATCHING JUST FOR COMPARISON

m.out.fl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

plot_full_probit <- plot(summary(m.out.fp))
plot_full_logit <- plot(summary(m.out.fl))
plot_nearest_probit <- plot(summary(m.out.np))
plot_nearest_logit <- plot(summary(m.out.nl))
plot_optimal_probit <- plot(summary(m.out.op))
plot_optimal_logit <- plot(summary(m.out.ol))
#plot_mahal <- plot(summary(m.out.fp))

print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_logit)
print(plot_optimal_probit)
#print(plot_mahal)

## Mean CDF table: 
c1<-plot_full_probit$sum.matched[,5]
c2<-plot_full_logit$sum.matched[,5]
c3<-plot_nearest_probit$sum.matched[,5]
c4<-plot_nearest_logit$sum.matched[,5]
c5<-plot_optimal_probit$sum.matched[,5]
c6<-plot_optimal_logit$sum.matched[,5]
#c7<-plot_mahal$sum.matched[,5]

table11<-rbind(c1, c2, c3, c4, c5, c6)
rownames(table11) <- c("Full Probit", "Full Logit", "Nearest Probit", "Nearest Logit", "Optimal Probit", "Optimal Pobit")
table11<-as.data.frame(table11)
colnames(table11)[1] <- "Distance"
colnames(table11)[2] <- "Political Discrimination"
colnames(table11)[3] <- "Worship Attendance"
colnames(table11)[4] <- "Political Interest"
colnames(table11)[5] <- "Political Knowledge"
colnames(table11)[6] <- "Party ID"
colnames(table11)[7] <- "Identity"
colnames(table11)[8] <- "English (Main Lang)"
colnames(table11)[9] <- "Native Born"
colnames(table11)[10] <- "Female"
colnames(table11)[11] <- "Age"
colnames(table11)[12] <- "Education"
colnames(table11)[13] <- "High Income"
colnames(table11)[14] <- "Med Income"
colnames(table11)[15] <- "Missing Income"
colnames(table11)[16] <- "Black Caribbean"
colnames(table11)[17] <- "Indian"
colnames(table11)[18] <- "Pakistani"
colnames(table11)[19] <- "Bangladeshi"

stargazer(table11, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Ethnic Based Engagement (Societal Discrimination): CDF Comparison - Simplified Model"
)

##########################
#### FIGURE 15 (P. 34) ###
##########################

df <- data.frame(old = c("poldisc", "relatt_oth_r", "pol_interest", "polknowledge", "partyid",
                         "identity", "english", "native_born", "female", "age", "education2", "highinc",
                         "medinc", "misinc", "black_caribbean", "indian", "pakistani", "bangladeshi"
), 
new = c("Political Discrimination", "Worship Attendance", "Political Interest", 
        "Political Knowledge", "Party ID", "Identity", "English (Main Lang)", 
        "Native Born", "Female", "Age", "Education", "High Income", "Med Income", 
        "Missing Income", "Black Caribbean", "Indian", "Pakistani", "Bangladeshi" 
)
)

plot_full_probit <- love.plot(m.out.fp, 
                              binary = "std",
                              thresholds = c(m=.1),
                              var.names = df,
                              abs = T,
                              grid = T,
                              line = T,
                              sample.names = c("Unmatched", "Matched"),
                              shapes = c("circle filled", "triangle filled"),
                              colors = c("red", "blue"),
                              title = "Full Probit",
                              wrap = 45,
                              size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_full_logit <- love.plot(m.out.fl, 
                             binary = "std",
                             thresholds = c(m=.1),
                             var.names = df,
                             abs = T,
                             grid = T,
                             line = T,
                             sample.names = c("Unmatched", "Matched"),
                             shapes = c("circle filled", "triangle filled"),
                             colors = c("red", "blue"),
                             title = "Full Logit",
                             wrap = 45,
                             size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_probit <- love.plot(m.out.np, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Nearest Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_logit <- love.plot(m.out.nl, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Nearest Logit",
                                wrap = 45,
                                size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_optimal_probit <- love.plot(m.out.op, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Optimal Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_optimal_logit <- love.plot(m.out.ol, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Optimal Logit",
                                wrap = 45,
                                size = 2.5
) + 
  #ggtitle("<span style='font-size: 10pt;'>Optimal Logit</font>") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_probit)
print(plot_optimal_logit)
#print(plot_mahal)

bplot.ebe.sd.s <- grid.arrange(
  plot_full_probit,
  plot_full_logit,
  plot_optimal_probit,
  plot_optimal_logit,
  plot_nearest_probit,
  plot_nearest_logit,
  nrow = 2,
  top = textGrob("Ethnic Based Engagement (Societal Discrimination): Balance Plots - Simplified Model",
                 gp = gpar(fontsize = 13, fontface = 1)),
  bottom = textGrob(
    "Individual plot titles indicate method and link function used in the matching procedure.",
    gp = gpar(fontface = 1, fontsize = 8),
    hjust = 1,
    x = 1
  )
)
ggsave("bplot.ebe.sd.s.pdf", plot = bplot.ebe.sd.s, width = 14, height = 8)

##########################
#### TABLE 26 (P. 34) ####
##########################

ndata <- mdata %>%
  select(ethnic_active, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

plot_full_probit <- plot(summary(m.out.fp))
plot_full_logit <- plot(summary(m.out.fl))
plot_nearest_probit <- plot(summary(m.out.np))
plot_nearest_logit <- plot(summary(m.out.nl))
plot_optimal_probit <- plot(summary(m.out.op))
plot_optimal_logit <- plot(summary(m.out.ol))

print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_logit)
print(plot_optimal_probit)

## Mean CDF table: 
c1<-plot_full_probit$sum.matched[,5]
c2<-plot_full_logit$sum.matched[,5]
c3<-plot_nearest_probit$sum.matched[,5]
c4<-plot_nearest_logit$sum.matched[,5]
c5<-plot_optimal_probit$sum.matched[,5]
c6<-plot_optimal_logit$sum.matched[,5]

table12<-rbind(c1, c2, c3, c4, c5, c6)
rownames(table12) <- c("Full Probit", "Full Logit", "Nearest Probit", "Nearest Logit", "Optimal Probit", "Optimal Pobit")
table12<-as.data.frame(table12)
colnames(table12)[1] <- "Distance"
colnames(table12)[2] <- "Societal Discrimination"
colnames(table12)[3] <- "Worship Attendance"
colnames(table12)[4] <- "Political Interest"
colnames(table12)[5] <- "Political Knowledge"
colnames(table12)[6] <- "Party ID"
colnames(table12)[7] <- "Identity"
colnames(table12)[8] <- "English (Main Lang)"
colnames(table12)[9] <- "Native Born"
colnames(table12)[10] <- "Female"
colnames(table12)[11] <- "Age"
colnames(table12)[12] <- "Education"
colnames(table12)[13] <- "High Income"
colnames(table12)[14] <- "Med Income"
colnames(table12)[15] <- "Missing Income"
colnames(table12)[16] <- "Black Caribbean"
colnames(table12)[17] <- "Indian"
colnames(table12)[18] <- "Pakistani"
colnames(table12)[19] <- "Bangladeshi"

stargazer(table12, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Ethnic Based Engagement (Political Discrimination): CDF Comparison - Simplified Model"
)

##########################
#### FIGURE 16 (P. 35) ###
##########################

df <- data.frame(old = c("socdisc", "relatt_oth_r", "pol_interest", "polknowledge", "partyid",
                         "identity", "english", "native_born", "female", "age", "education2", "highinc",
                         "medinc", "misinc", "black_caribbean", "indian", "pakistani", "bangladeshi"
), 
new = c("Societal Discrimination", "Worship Attendance", "Political Interest", 
        "Political Knowledge", "Party ID", "Identity", "English (Main Lang)", 
        "Native Born", "Female", "Age", "Education", "High Income", "Med Income", 
        "Missing Income", "Black Caribbean", "Indian", "Pakistani", "Bangladeshi" 
)
)

plot_full_probit <- love.plot(m.out.fp, 
                              binary = "std",
                              thresholds = c(m=.1),
                              var.names = df,
                              abs = T,
                              grid = T,
                              line = T,
                              sample.names = c("Unmatched", "Matched"),
                              shapes = c("circle filled", "triangle filled"),
                              colors = c("red", "blue"),
                              title = "Full Probit",
                              wrap = 45,
                              size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_full_logit <- love.plot(m.out.fl, 
                             binary = "std",
                             thresholds = c(m=.1),
                             var.names = df,
                             abs = T,
                             grid = T,
                             line = T,
                             sample.names = c("Unmatched", "Matched"),
                             shapes = c("circle filled", "triangle filled"),
                             colors = c("red", "blue"),
                             title = "Full Logit",
                             wrap = 45,
                             size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_probit <- love.plot(m.out.np, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Nearest Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_nearest_logit <- love.plot(m.out.nl, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Nearest Logit",
                                wrap = 45,
                                size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

plot_optimal_probit <- love.plot(m.out.op, 
                                 binary = "std",
                                 thresholds = c(m=.1),
                                 var.names = df,
                                 abs = T,
                                 grid = T,
                                 line = T,
                                 sample.names = c("Unmatched", "Matched"),
                                 shapes = c("circle filled", "triangle filled"),
                                 colors = c("red", "blue"),
                                 title = "Optimal Probit",
                                 wrap = 45,
                                 size = 2.5
) +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


plot_optimal_logit <- love.plot(m.out.ol, 
                                binary = "std",
                                thresholds = c(m=.1),
                                var.names = df,
                                abs = T,
                                grid = T,
                                line = T,
                                sample.names = c("Unmatched", "Matched"),
                                shapes = c("circle filled", "triangle filled"),
                                colors = c("red", "blue"),
                                title = "Optimal Logit",
                                wrap = 45,
                                size = 2.5
) + 
  #ggtitle("<span style='font-size: 10pt;'>Optimal Logit</font>") +
  theme(plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 7),
        legend.position = c(.75, .50),
        legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))


print(plot_full_probit)
print(plot_full_logit)
print(plot_nearest_probit)
print(plot_nearest_logit)
print(plot_optimal_probit)
print(plot_optimal_logit)
#print(plot_mahal)

bplot.ebe.pd.s <- grid.arrange(
  plot_full_probit,
  plot_full_logit,
  plot_optimal_probit,
  plot_optimal_logit,
  plot_nearest_probit,
  plot_nearest_logit,
  nrow = 2,
  top = textGrob("Ethnic Based Engagement (Political Discrimination): Balance Plots - Simplified Model",
                 gp = gpar(fontsize = 13, fontface = 1)),
  bottom = textGrob(
    "Individual plot titles indicate method and link function used in the matching procedure.",
    gp = gpar(fontface = 1, fontsize = 8),
    hjust = 1,
    x = 1
  )
)
ggsave("bplot.ebe.pd.s.pdf", plot = bplot.ebe.pd.s, width = 14, height = 8)

##########################
#### TABLE 27 (P. 35) ####
##########################

formula<-(voted2010 ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi)


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

ndata <- mdata %>%
  select(voted2010, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

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

model3.1 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data1, weights=weights, 
                family=quasibinomial())
summary(model3.1)

model3.2 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data2, weights=weights, 
                family=quasibinomial())
summary(model3.2)

model3.3 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data3, weights=weights, 
                family=quasibinomial())
summary(model3.3)

model3.4 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data4, weights=weights, 
                family=quasibinomial())
summary(model3.4)

model3.5 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data5, weights=weights, 
                family=quasibinomial())
summary(model3.5)

model3.6 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data6, weights=weights, 
                family=quasibinomial())
summary(model3.6)

att3.1.rr<-avg_comparisons(model3.1,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att3.1.rr)

att3.2.rr<-avg_comparisons(model3.2,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att3.2.rr)

att3.3.rr<-avg_comparisons(model3.3,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att3.3.rr)

att3.4.rr<-avg_comparisons(model3.4,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att3.4.rr)

att3.5.rr<-avg_comparisons(model3.5,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att3.5.rr)

att3.6.rr<-avg_comparisons(model3.6,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att3.6.rr)

### Vote in General Elections (Societal Discrimination): ATT Estimates as Risk Ratios - Simplified Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att3.1.rr$estimate, att3.2.rr$estimate, att3.3.rr$estimate, 
          att3.4.rr$estimate, att3.5.rr$estimate, att3.6.rr$estimate) 
col2 <- c(att3.1.rr$p.value, att3.2.rr$p.value, att3.3.rr$p.value, 
          att3.4.rr$p.value, att3.5.rr$p.value, att3.6.rr$p.value)
col3 <- c(att3.1.rr$conf.low, att3.2.rr$conf.low, att3.3.rr$conf.low, 
          att3.4.rr$conf.low, att3.5.rr$conf.low, att3.6.rr$conf.low)
col4 <- c(att3.1.rr$conf.high, att3.2.rr$conf.high, att3.3.rr$conf.high, 
          att3.4.rr$conf.high, att3.5.rr$conf.high, att3.6.rr$conf.high)

table3.1.rr<-cbind(col0, col1, col2, col3, col4)  
#rownames(table1.1.rr) <- c("Full Probit", "Full Logit", 
#                        "Nearest Probit", "Nearest Logit", 
#                       "Optimal Probit", "Optimal Pobit")
table3.1.rr<-as.data.frame(table3.1.rr)
table3.1.rr <- table3.1.rr %>%
  mutate(across(-col0, as.numeric))
table3.1.rr <- table3.1.rr %>% mutate(across(-col0, ~ round(., 6)))
colnames(table3.1.rr)[1] <- "Model"
colnames(table3.1.rr)[2] <- "Estimate"
colnames(table3.1.rr)[3] <- "P-value"
colnames(table3.1.rr)[4] <- "CI Lower"
colnames(table3.1.rr)[5] <- "CI Upper"
print(table3.1.rr)


stargazer(table3.1.rr, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in General Elections (Societal Discrimination): ATT Estimates as Risk Ratios - Simplified Model"
)

##########################
#### TABLE 28 (P. 35) ####
##########################

ndata <- mdata %>%
  select(voted2010, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

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

model4.1 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data1, weights=weights, 
                family=quasibinomial())
summary(model4.1)

model4.2 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data2, weights=weights, 
                family=quasibinomial())
summary(model4.2)

model4.3 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data3, weights=weights, 
                family=quasibinomial())
summary(model4.3)

model4.4 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data4, weights=weights, 
                family=quasibinomial())
summary(model4.4)

model4.5 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data5, weights=weights, 
                family=quasibinomial())
summary(model4.5)

model4.6 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data6, weights=weights, 
                family=quasibinomial())
summary(model4.6)

att4.1.rr<-avg_comparisons(model4.1,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att4.1.rr)

att4.2.rr<-avg_comparisons(model4.2,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att4.2.rr)

att4.3.rr<-avg_comparisons(model4.3,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att4.3.rr)

att4.4.rr<-avg_comparisons(model4.4,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att4.4.rr)

att4.5.rr<-avg_comparisons(model4.5,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att4.5.rr)

att4.6.rr<-avg_comparisons(model4.6,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att4.6.rr)

### Vote in General Elections (Political Discrimination): ATT Estimates as Risk Ratios - Simplified Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att4.1.rr$estimate, att4.2.rr$estimate, att4.3.rr$estimate, 
          att4.4.rr$estimate, att4.5.rr$estimate, att4.6.rr$estimate) 
col2 <- c(att4.1.rr$p.value, att4.2.rr$p.value, att4.3.rr$p.value, 
          att4.4.rr$p.value, att4.5.rr$p.value, att4.6.rr$p.value)
col3 <- c(att4.1.rr$conf.low, att4.2.rr$conf.low, att4.3.rr$conf.low, 
          att4.4.rr$conf.low, att4.5.rr$conf.low, att4.6.rr$conf.low)
col4 <- c(att4.1.rr$conf.high, att4.2.rr$conf.high, att4.3.rr$conf.high, 
          att4.4.rr$conf.high, att4.5.rr$conf.high, att4.6.rr$conf.high)

table4.1.rr<-cbind(col0, col1, col2, col3, col4)  
table4.1.rr<-as.data.frame(table4.1.rr)
table4.1.rr <- table4.1.rr %>%
  mutate(across(-col0, as.numeric))
table4.1.rr <- table4.1.rr %>% mutate(across(-col0, ~ round(., 6)))
colnames(table4.1.rr)[1] <- "Model"
colnames(table4.1.rr)[2] <- "Estimate"
colnames(table4.1.rr)[3] <- "P-value"
colnames(table4.1.rr)[4] <- "CI Lower"
colnames(table4.1.rr)[5] <- "CI Upper"
print(table4.1.rr)


stargazer(table4.1.rr, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in General Elections (Political Discrimination): ATT Estimates as Risk Ratios - Simplified Model"
)

##########################
#### TABLE 29 (P. 36) ####
##########################

formula<-(voted2010_local ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi)


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

ndata <- mdata %>%
  select(voted2010_local, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol


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

model7.1 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data1, weights=weights, 
                family=quasibinomial())
summary(model7.1)

model7.2 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data2, weights=weights, 
                family=quasibinomial())
summary(model7.2)

model7.3 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data3, weights=weights, 
                family=quasibinomial())
summary(model7.3)

model7.4 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data4, weights=weights, 
                family=quasibinomial())
summary(model7.4)

model7.5 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data5, weights=weights, 
                family=quasibinomial())
summary(model7.5)

model7.6 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data6, weights=weights, 
                family=quasibinomial())
summary(model7.6)

att7.1.rr<-avg_comparisons(model7.1,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att7.1.rr)

att7.2.rr<-avg_comparisons(model7.2,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att7.2.rr)

att7.3.rr<-avg_comparisons(model7.3,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att7.3.rr)

att7.4.rr<-avg_comparisons(model7.4,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att7.4.rr)

att7.5.rr<-avg_comparisons(model7.5,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att7.5.rr)

att7.6.rr<-avg_comparisons(model7.6,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att7.6.rr)

### Vote in Local Elections (Societal Discrimination): ATT Estimates as Risk Ratios - Simplified Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att7.1.rr$estimate, att7.2.rr$estimate, att7.3.rr$estimate, 
          att7.4.rr$estimate, att7.5.rr$estimate, att7.6.rr$estimate) 
col2 <- c(att7.1.rr$p.value, att7.2.rr$p.value, att7.3.rr$p.value, 
          att7.4.rr$p.value, att7.5.rr$p.value, att7.6.rr$p.value)
col3 <- c(att7.1.rr$conf.low, att7.2.rr$conf.low, att7.3.rr$conf.low, 
          att7.4.rr$conf.low, att7.5.rr$conf.low, att7.6.rr$conf.low)
col4 <- c(att7.1.rr$conf.high, att7.2.rr$conf.high, att7.3.rr$conf.high, 
          att7.4.rr$conf.high, att7.5.rr$conf.high, att7.6.rr$conf.high)

table7.1.rr<-cbind(col0, col1, col2, col3, col4)  
table7.1.rr<-as.data.frame(table7.1.rr)
table7.1.rr <- table7.1.rr %>%
  mutate(across(-col0, as.numeric))
table7.1.rr <- table7.1.rr %>% mutate(across(-col0, ~ round(., 6)))
colnames(table7.1.rr)[1] <- "Model"
colnames(table7.1.rr)[2] <- "Estimate"
colnames(table7.1.rr)[3] <- "P-value"
colnames(table7.1.rr)[4] <- "CI Lower"
colnames(table7.1.rr)[5] <- "CI Upper"
print(table7.1.rr)


stargazer(table7.1.rr, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in Local Elections (Societal Discrimination): ATT Estimates as Risk Ratios - Simplified Model"
)

##########################
#### TABLE 30 (P. 36) ####
##########################

ndata <- mdata %>%
  select(voted2010_local, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

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

model8.1 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data1, weights=weights, 
                family=quasibinomial())
summary(model8.1)

model8.2 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data2, weights=weights, 
                family=quasibinomial())
summary(model8.2)

model8.3 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data3, weights=weights, 
                family=quasibinomial())
summary(model8.3)

model8.4 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data4, weights=weights, 
                family=quasibinomial())
summary(model8.4)

model8.5 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data5, weights=weights, 
                family=quasibinomial())
summary(model8.5)

model8.6 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data6, weights=weights, 
                family=quasibinomial())
summary(model8.6)

att8.1.rr<-avg_comparisons(model8.1,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att8.1.rr)

att8.2.rr<-avg_comparisons(model8.2,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att8.2.rr)

att8.3.rr<-avg_comparisons(model8.3,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att8.3.rr)

att8.4.rr<-avg_comparisons(model8.4,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att8.4.rr)

att8.5.rr<-avg_comparisons(model8.5,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att8.5.rr)

att8.6.rr<-avg_comparisons(model8.6,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att8.6.rr)

### Vote in Local Elections (Political Discrimination): ATT Estimates as Risk Ratios - Simplified Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att8.1.rr$estimate, att8.2.rr$estimate, att8.3.rr$estimate, 
          att8.4.rr$estimate, att8.5.rr$estimate, att8.6.rr$estimate) 
col2 <- c(att8.1.rr$p.value, att8.2.rr$p.value, att8.3.rr$p.value, 
          att8.4.rr$p.value, att8.5.rr$p.value, att8.6.rr$p.value)
col3 <- c(att8.1.rr$conf.low, att8.2.rr$conf.low, att8.3.rr$conf.low, 
          att8.4.rr$conf.low, att8.5.rr$conf.low, att8.6.rr$conf.low)
col4 <- c(att8.1.rr$conf.high, att8.2.rr$conf.high, att8.3.rr$conf.high, 
          att8.4.rr$conf.high, att8.5.rr$conf.high, att8.6.rr$conf.high)

table8.1.rr<-cbind(col0, col1, col2, col3, col4)  
table8.1.rr<-as.data.frame(table8.1.rr)
table8.1.rr <- table8.1.rr %>%
  mutate(across(-col0, as.numeric))
table8.1.rr <- table8.1.rr %>% mutate(across(-col0, ~ round(., 6)))
colnames(table8.1.rr)[1] <- "Model"
colnames(table8.1.rr)[2] <- "Estimate"
colnames(table8.1.rr)[3] <- "P-value"
colnames(table8.1.rr)[4] <- "CI Lower"
colnames(table8.1.rr)[5] <- "CI Upper"
print(table8.1.rr)


stargazer(table8.1.rr, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in Local Elections (Political Discrimination): ATT Estimates as Risk Ratios - Simplified Model"
)

##########################
#### TABLE 31 (P. 36) ####
##########################

formula<-(ethnic_active ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi)


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

ndata <- mdata %>%
  select(ethnic_active, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

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

model11.1 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                 data=mat.data1, weights=weights, 
                 family=quasibinomial())
summary(model11.1)

model11.2 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                 data=mat.data2, weights=weights, 
                 family=quasibinomial())
summary(model11.2)

model11.3 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                 data=mat.data3, weights=weights, 
                 family=quasibinomial())
summary(model11.3)

model11.4 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                 data=mat.data4, weights=weights, 
                 family=quasibinomial())
summary(model11.4)

model11.5 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                 data=mat.data5, weights=weights, 
                 family=quasibinomial())
summary(model11.5)

model11.6 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                 data=mat.data6, weights=weights, 
                 family=quasibinomial())
summary(model11.6)

att11.1.rr<-avg_comparisons(model11.1,
                            variables = "socdisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data1, socdisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att11.1.rr)

att11.2.rr<-avg_comparisons(model11.2,
                            variables = "socdisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data2, socdisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att11.2.rr)

att11.3.rr<-avg_comparisons(model11.3,
                            variables = "socdisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data3, socdisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att11.3.rr)

att11.4.rr<-avg_comparisons(model11.4,
                            variables = "socdisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data4, socdisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att11.4.rr)

att11.5.rr<-avg_comparisons(model11.5,
                            variables = "socdisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data5, socdisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att11.5.rr)

att11.6.rr<-avg_comparisons(model11.6,
                            variables = "socdisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data6, socdisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att11.6.rr)

### Ethnic based engagement (Societal Discrimination): ATT Estimates as Risk Ratios - Simplified Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att11.1.rr$estimate, att11.2.rr$estimate, att11.3.rr$estimate, 
          att11.4.rr$estimate, att11.5.rr$estimate, att11.6.rr$estimate) 
col2 <- c(att11.1.rr$p.value, att11.2.rr$p.value, att11.3.rr$p.value, 
          att11.4.rr$p.value, att11.5.rr$p.value, att11.6.rr$p.value)
col3 <- c(att11.1.rr$conf.low, att11.2.rr$conf.low, att11.3.rr$conf.low, 
          att11.4.rr$conf.low, att11.5.rr$conf.low, att11.6.rr$conf.low)
col4 <- c(att11.1.rr$conf.high, att11.2.rr$conf.high, att11.3.rr$conf.high, 
          att11.4.rr$conf.high, att11.5.rr$conf.high, att11.6.rr$conf.high)

table11.1.rr<-cbind(col0, col1, col2, col3, col4)  
table11.1.rr<-as.data.frame(table11.1.rr)
table11.1.rr <- table11.1.rr %>%
  mutate(across(-col0, as.numeric))
table11.1.rr <- table11.1.rr %>% mutate(across(-col0, ~ round(., 6)))
colnames(table11.1.rr)[1] <- "Model"
colnames(table11.1.rr)[2] <- "Estimate"
colnames(table11.1.rr)[3] <- "P-value"
colnames(table11.1.rr)[4] <- "CI Lower"
colnames(table11.1.rr)[5] <- "CI Upper"
print(table11.1.rr)


stargazer(table11.1.rr, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Ethnic based engagement (Societal Discrimination): ATT Estimates as Risk Ratios - Simplified Model"
)

##########################
#### TABLE 32 (P. 36) ####
##########################

ndata <- mdata %>%
  select(ethnic_active, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

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

model12.1 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                 data=mat.data1, weights=weights, 
                 family=quasibinomial())
summary(model12.1)

model12.2 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                 data=mat.data2, weights=weights, 
                 family=quasibinomial())
summary(model12.2)

model12.3 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                 data=mat.data3, weights=weights, 
                 family=quasibinomial())
summary(model12.3)

model12.4 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                 data=mat.data4, weights=weights, 
                 family=quasibinomial())
summary(model12.4)

model12.5 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                 data=mat.data5, weights=weights, 
                 family=quasibinomial())
summary(model12.5)

model12.6 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                 data=mat.data6, weights=weights, 
                 family=quasibinomial())
summary(model12.6)

att12.1.rr<-avg_comparisons(model12.1,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data1, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att12.1.rr)

att12.2.rr<-avg_comparisons(model12.2,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data2, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att12.2.rr)

att12.3.rr<-avg_comparisons(model12.3,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data3, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att12.3.rr)

att12.4.rr<-avg_comparisons(model12.4,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data4, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att12.4.rr)

att12.5.rr<-avg_comparisons(model12.5,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data5, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att12.5.rr)

att12.6.rr<-avg_comparisons(model12.6,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data6, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att12.6.rr)

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att12.1.rr$estimate, att12.2.rr$estimate, att12.3.rr$estimate, 
          att12.4.rr$estimate, att12.5.rr$estimate, att12.6.rr$estimate) 
col2 <- c(att12.1.rr$p.value, att12.2.rr$p.value, att12.3.rr$p.value, 
          att12.4.rr$p.value, att12.5.rr$p.value, att12.6.rr$p.value)
col3 <- c(att12.1.rr$conf.low, att12.2.rr$conf.low, att12.3.rr$conf.low, 
          att12.4.rr$conf.low, att12.5.rr$conf.low, att12.6.rr$conf.low)
col4 <- c(att12.1.rr$conf.high, att12.2.rr$conf.high, att12.3.rr$conf.high, 
          att12.4.rr$conf.high, att12.5.rr$conf.high, att12.6.rr$conf.high)

table12.1.rr<-cbind(col0, col1, col2, col3, col4)  
table12.1.rr<-as.data.frame(table12.1.rr)
table12.1.rr <- table12.1.rr %>%
  mutate(across(-col0, as.numeric))
table12.1.rr <- table12.1.rr %>% mutate(across(-col0, ~ round(., 6)))
colnames(table12.1.rr)[1] <- "Model"
colnames(table12.1.rr)[2] <- "Estimate"
colnames(table12.1.rr)[3] <- "P-value"
colnames(table12.1.rr)[4] <- "CI Lower"
colnames(table12.1.rr)[5] <- "CI Upper"
print(table12.1.rr)


stargazer(table12.1.rr, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Ethnic based engagement (Political Discrimination): ATT Estimates as Risk Ratios - Simplified Model"
)

##########################
#### TABLE 33 (P. 36) ####
##########################

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

ndata <- mdata %>%
  select(voted2010, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, vote_duty, efficacy, democ_satis, trust_parliament) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.iib <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi+
                       vote_duty+ efficacy+ democ_satis+ trust_parliament,
                     data = ndata, method = NULL, distance = "glm")
m.out.iib
summary(m.out.iib)

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

att1.1.or<-avg_comparisons(model1.1,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att1.1.or)

att1.2.or<-avg_comparisons(model1.2,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att1.2.or)

att1.3.or<-avg_comparisons(model1.3,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att1.3.or)

att1.4.or<-avg_comparisons(model1.4,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att1.4.or)

att1.5.or<-avg_comparisons(model1.5,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att1.5.or)

att1.6.or<-avg_comparisons(model1.6,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att1.6.or)

### Vote in General Elections (Societal Discrimination): ATT Estimates as Odds Ratios - Complete Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att1.1.or$estimate, att1.2.or$estimate, att1.3.or$estimate, 
          att1.4.or$estimate, att1.5.or$estimate, att1.6.or$estimate) 
col2 <- c(att1.1.or$p.value, att1.2.or$p.value, att1.3.or$p.value, 
          att1.4.or$p.value, att1.5.or$p.value, att1.6.or$p.value)
col3 <- c(att1.1.or$conf.low, att1.2.or$conf.low, att1.3.or$conf.low, 
          att1.4.or$conf.low, att1.5.or$conf.low, att1.6.or$conf.low)
col4 <- c(att1.1.or$conf.high, att1.2.or$conf.high, att1.3.or$conf.high, 
          att1.4.or$conf.high, att1.5.or$conf.high, att1.6.or$conf.high)

table1.1.or<-cbind(col0, col1, col2, col3, col4)  
table1.1.or<-as.data.frame(table1.1.or)
table1.1.or <- table1.1.or %>%
  mutate(across(-col0, as.numeric))
table1.1.or <- table1.1.or %>% mutate(across(-col0, ~ round(., 6)))
colnames(table1.1.or)[1] <- "Model"
colnames(table1.1.or)[2] <- "Estimate"
colnames(table1.1.or)[3] <- "P-value"
colnames(table1.1.or)[4] <- "CI Lower"
colnames(table1.1.or)[5] <- "CI Upper"
print(table1.1.or)


stargazer(table1.1.or, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in General Elections (Societal Discrimination): ATT Estimates as Odds Ratios - Complete Model"
)

##########################
#### TABLE 34 (P. 37) ####
##########################

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

att2.1.or<-avg_comparisons(model2.1,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att2.1.or)

att2.2.or<-avg_comparisons(model2.2,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att2.2.or)

att2.3.or<-avg_comparisons(model2.3,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att2.3.or)

att2.4.or<-avg_comparisons(model2.4,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att2.4.or)

att2.5.or<-avg_comparisons(model2.5,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att2.5.or)

att2.6.or<-avg_comparisons(model2.6,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att2.6.or)

### Vote in General Elections (Political Discrimination): ATT Estimates as Odds Ratios - Complete Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att2.1.or$estimate, att2.2.or$estimate, att2.3.or$estimate, 
          att2.4.or$estimate, att2.5.or$estimate, att2.6.or$estimate) 
col2 <- c(att2.1.or$p.value, att2.2.or$p.value, att2.3.or$p.value, 
          att2.4.or$p.value, att2.5.or$p.value, att2.6.or$p.value)
col3 <- c(att2.1.or$conf.low, att2.2.or$conf.low, att2.3.or$conf.low, 
          att2.4.or$conf.low, att2.5.or$conf.low, att2.6.or$conf.low)
col4 <- c(att2.1.or$conf.high, att2.2.or$conf.high, att2.3.or$conf.high, 
          att2.4.or$conf.high, att2.5.or$conf.high, att2.6.or$conf.high)

table2.1.or<-cbind(col0, col1, col2, col3, col4)  
table2.1.or<-as.data.frame(table2.1.or)
table2.1.or <- table2.1.or %>%
  mutate(across(-col0, as.numeric))
table2.1.or <- table2.1.or %>% mutate(across(-col0, ~ round(., 6)))
colnames(table2.1.or)[1] <- "Model"
colnames(table2.1.or)[2] <- "Estimate"
colnames(table2.1.or)[3] <- "P-value"
colnames(table2.1.or)[4] <- "CI Lower"
colnames(table2.1.or)[5] <- "CI Upper"
print(table2.1.or)


stargazer(table2.1.or, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in General Elections (Political Discrimination): ATT Estimates as Odds Ratios - Complete Model"
)

##########################
#### TABLE 35 (P. 37) ####
##########################

formula<-(voted2010 ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi)


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

ndata <- mdata %>%
  select(voted2010, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

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

model3.1 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data1, weights=weights, 
                family=quasibinomial())
summary(model3.1)

model3.2 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data2, weights=weights, 
                family=quasibinomial())
summary(model3.2)

model3.3 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data3, weights=weights, 
                family=quasibinomial())
summary(model3.3)

model3.4 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data4, weights=weights, 
                family=quasibinomial())
summary(model3.4)

model3.5 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data5, weights=weights, 
                family=quasibinomial())
summary(model3.5)

model3.6 <- glm(voted2010~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data6, weights=weights, 
                family=quasibinomial())
summary(model3.6)

att3.1.or<-avg_comparisons(model3.1,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att3.1.or)

att3.2.or<-avg_comparisons(model3.2,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att3.2.or)

att3.3.or<-avg_comparisons(model3.3,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att3.3.or)

att3.4.or<-avg_comparisons(model3.4,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att3.4.or)

att3.5.or<-avg_comparisons(model3.5,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att3.5.or)

att3.6.or<-avg_comparisons(model3.6,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att3.6.or)

### Vote in General Elections (Societal Discrimination): ATT Estimates as Odds Ratios - Simplified Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att3.1.or$estimate, att3.2.or$estimate, att3.3.or$estimate, 
          att3.4.or$estimate, att3.5.or$estimate, att3.6.or$estimate) 
col2 <- c(att3.1.or$p.value, att3.2.or$p.value, att3.3.or$p.value, 
          att3.4.or$p.value, att3.5.or$p.value, att3.6.or$p.value)
col3 <- c(att3.1.or$conf.low, att3.2.or$conf.low, att3.3.or$conf.low, 
          att3.4.or$conf.low, att3.5.or$conf.low, att3.6.or$conf.low)
col4 <- c(att3.1.or$conf.high, att3.2.or$conf.high, att3.3.or$conf.high, 
          att3.4.or$conf.high, att3.5.or$conf.high, att3.6.or$conf.high)

table3.1.or<-cbind(col0, col1, col2, col3, col4)  
table3.1.or<-as.data.frame(table3.1.or)
table3.1.or <- table3.1.or %>%
  mutate(across(-col0, as.numeric))
table3.1.or <- table3.1.or %>% mutate(across(-col0, ~ round(., 6)))
colnames(table3.1.or)[1] <- "Model"
colnames(table3.1.or)[2] <- "Estimate"
colnames(table3.1.or)[3] <- "P-value"
colnames(table3.1.or)[4] <- "CI Lower"
colnames(table3.1.or)[5] <- "CI Upper"
print(table3.1.or)


stargazer(table3.1.or, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in General Elections (Societal Discrimination): ATT Estimates as Odds Ratios - Simplified Model"
)

##########################
#### TABLE 36 (P. 37) ####
##########################

ndata <- mdata %>%
  select(voted2010, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

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

model4.1 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data1, weights=weights, 
                family=quasibinomial())
summary(model4.1)

model4.2 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data2, weights=weights, 
                family=quasibinomial())
summary(model4.2)

model4.3 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data3, weights=weights, 
                family=quasibinomial())
summary(model4.3)

model4.4 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data4, weights=weights, 
                family=quasibinomial())
summary(model4.4)

model4.5 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data5, weights=weights, 
                family=quasibinomial())
summary(model4.5)

model4.6 <- glm(voted2010~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                       identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                       medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data6, weights=weights, 
                family=quasibinomial())
summary(model4.6)

att4.1.or<-avg_comparisons(model4.1,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att4.1.or)

att4.2.or<-avg_comparisons(model4.2,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att4.2.or)

att4.3.or<-avg_comparisons(model4.3,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att4.3.or)

att4.4.or<-avg_comparisons(model4.4,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att4.4.or)

att4.5.or<-avg_comparisons(model4.5,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att4.5.or)

att4.6.or<-avg_comparisons(model4.6,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att4.6.or)

### Vote in General Elections (Political Discrimination): ATT Estimates as Odds Ratios - Simplified Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att4.1.or$estimate, att4.2.or$estimate, att4.3.or$estimate, 
          att4.4.or$estimate, att4.5.or$estimate, att4.6.or$estimate) 
col2 <- c(att4.1.or$p.value, att4.2.or$p.value, att4.3.or$p.value, 
          att4.4.or$p.value, att4.5.or$p.value, att4.6.or$p.value)
col3 <- c(att4.1.or$conf.low, att4.2.or$conf.low, att4.3.or$conf.low, 
          att4.4.or$conf.low, att4.5.or$conf.low, att4.6.or$conf.low)
col4 <- c(att4.1.or$conf.high, att4.2.or$conf.high, att4.3.or$conf.high, 
          att4.4.or$conf.high, att4.5.or$conf.high, att4.6.or$conf.high)

table4.1.or<-cbind(col0, col1, col2, col3, col4)  
table4.1.or<-as.data.frame(table4.1.or)
table4.1.or <- table4.1.or %>%
  mutate(across(-col0, as.numeric))
table4.1.or <- table4.1.or %>% mutate(across(-col0, ~ round(., 6)))
colnames(table4.1.or)[1] <- "Model"
colnames(table4.1.or)[2] <- "Estimate"
colnames(table4.1.or)[3] <- "P-value"
colnames(table4.1.or)[4] <- "CI Lower"
colnames(table4.1.or)[5] <- "CI Upper"
print(table4.1.or)


stargazer(table4.1.or, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in General Elections (Political Discrimination): ATT Estimates as Odds Ratios - Simplified Model"
)

##########################
#### TABLE 37 (P. 37) ####
##########################

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

att5.1.or<-avg_comparisons(model5.1,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att5.1.or)

att5.2.or<-avg_comparisons(model5.2,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att5.2.or)

att5.3.or<-avg_comparisons(model5.3,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att5.3.or)

att5.4.or<-avg_comparisons(model5.4,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att5.4.or)

att5.5.or<-avg_comparisons(model5.5,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att5.5.or)

att5.6.or<-avg_comparisons(model5.6,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att5.6.or)

### Vote in Local Elections (Societal Discrimination): ATT Estimates as Odds Ratios - ~Complete Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att5.1.or$estimate, att5.2.or$estimate, att5.3.or$estimate, 
          att5.4.or$estimate, att5.5.or$estimate, att5.6.or$estimate) 
col2 <- c(att5.1.or$p.value, att5.2.or$p.value, att5.3.or$p.value, 
          att5.4.or$p.value, att5.5.or$p.value, att5.6.or$p.value)
col3 <- c(att5.1.or$conf.low, att5.2.or$conf.low, att5.3.or$conf.low, 
          att5.4.or$conf.low, att5.5.or$conf.low, att5.6.or$conf.low)
col4 <- c(att5.1.or$conf.high, att5.2.or$conf.high, att5.3.or$conf.high, 
          att5.4.or$conf.high, att5.5.or$conf.high, att5.6.or$conf.high)

table5.1.or<-cbind(col0, col1, col2, col3, col4)  
table5.1.or<-as.data.frame(table5.1.or)
table5.1.or <- table5.1.or %>%
  mutate(across(-col0, as.numeric))
table5.1.or <- table5.1.or %>% mutate(across(-col0, ~ round(., 6)))
colnames(table5.1.or)[1] <- "Model"
colnames(table5.1.or)[2] <- "Estimate"
colnames(table5.1.or)[3] <- "P-value"
colnames(table5.1.or)[4] <- "CI Lower"
colnames(table5.1.or)[5] <- "CI Upper"
print(table5.1.or)


stargazer(table5.1.or, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in Local Elections (Societal Discrimination): ATT Estimates as Odds Ratios - Complete Model"
)

##########################
#### TABLE 38 (P. 37) ####
##########################

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

att6.1.or<-avg_comparisons(model6.1,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att6.1.or)

att6.2.or<-avg_comparisons(model6.2,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att6.2.or)

att6.3.or<-avg_comparisons(model6.3,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att6.3.or)

att6.4.or<-avg_comparisons(model6.4,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att6.4.or)

att6.5.or<-avg_comparisons(model6.5,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att6.5.or)

att6.6.or<-avg_comparisons(model6.6,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att6.6.or)

### Vote in Local Elections (Political Discrimination): ATT Estimates as Odds Ratios - ~Complete Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att6.1.or$estimate, att6.2.or$estimate, att6.3.or$estimate, 
          att6.4.or$estimate, att6.5.or$estimate, att6.6.or$estimate) 
col2 <- c(att6.1.or$p.value, att6.2.or$p.value, att6.3.or$p.value, 
          att6.4.or$p.value, att6.5.or$p.value, att6.6.or$p.value)
col3 <- c(att6.1.or$conf.low, att6.2.or$conf.low, att6.3.or$conf.low, 
          att6.4.or$conf.low, att6.5.or$conf.low, att6.6.or$conf.low)
col4 <- c(att6.1.or$conf.high, att6.2.or$conf.high, att6.3.or$conf.high, 
          att6.4.or$conf.high, att6.5.or$conf.high, att6.6.or$conf.high)

table6.1.or<-cbind(col0, col1, col2, col3, col4)  
table6.1.or<-as.data.frame(table6.1.or)
table6.1.or <- table6.1.or %>%
  mutate(across(-col0, as.numeric))
table6.1.or <- table6.1.or %>% mutate(across(-col0, ~ round(., 6)))
colnames(table6.1.or)[1] <- "Model"
colnames(table6.1.or)[2] <- "Estimate"
colnames(table6.1.or)[3] <- "P-value"
colnames(table6.1.or)[4] <- "CI Lower"
colnames(table6.1.or)[5] <- "CI Upper"
print(table6.1.or)


stargazer(table6.1.or, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in Local Elections (Political Discrimination): ATT Estimates as Odds Ratios - Complete Model"
)

##########################
#### TABLE 39 (P. 38) ####
##########################

formula<-(voted2010_local ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi)


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

ndata <- mdata %>%
  select(voted2010_local, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol
 
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

model7.1 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data1, weights=weights, 
                family=quasibinomial())
summary(model7.1)

model7.2 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data2, weights=weights, 
                family=quasibinomial())
summary(model7.2)

model7.3 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data3, weights=weights, 
                family=quasibinomial())
summary(model7.3)

model7.4 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data4, weights=weights, 
                family=quasibinomial())
summary(model7.4)

model7.5 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data5, weights=weights, 
                family=quasibinomial())
summary(model7.5)

model7.6 <- glm(voted2010_local~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                data=mat.data6, weights=weights, 
                family=quasibinomial())
summary(model7.6)

att7.1.or<-avg_comparisons(model7.1,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att7.1.or)

att7.2.or<-avg_comparisons(model7.2,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att7.2.or)

att7.3.or<-avg_comparisons(model7.3,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att7.3.or)

att7.4.or<-avg_comparisons(model7.4,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att7.4.or)

att7.5.rr<-avg_comparisons(model7.5,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnratioavg",
                           transform = "exp")
print(att7.5.rr)

att7.5.or<-avg_comparisons(model7.5,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att7.5.or)

att7.6.or<-avg_comparisons(model7.6,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att7.6.or)

### Vote in Local Elections (Societal Discrimination): ATT Estimates as Odds Ratios - ~Simplified Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att7.1.or$estimate, att7.2.or$estimate, att7.3.or$estimate, 
          att7.4.or$estimate, att7.5.or$estimate, att7.6.or$estimate) 
col2 <- c(att7.1.or$p.value, att7.2.or$p.value, att7.3.or$p.value, 
          att7.4.or$p.value, att7.5.or$p.value, att7.6.or$p.value)
col3 <- c(att7.1.or$conf.low, att7.2.or$conf.low, att7.3.or$conf.low, 
          att7.4.or$conf.low, att7.5.or$conf.low, att7.6.or$conf.low)
col4 <- c(att7.1.or$conf.high, att7.2.or$conf.high, att7.3.or$conf.high, 
          att7.4.or$conf.high, att7.5.or$conf.high, att7.6.or$conf.high)

table7.1.or<-cbind(col0, col1, col2, col3, col4)  
table7.1.or<-as.data.frame(table7.1.or)
table7.1.or <- table7.1.or %>%
  mutate(across(-col0, as.numeric))
table7.1.or <- table7.1.or %>% mutate(across(-col0, ~ round(., 6)))
colnames(table7.1.or)[1] <- "Model"
colnames(table7.1.or)[2] <- "Estimate"
colnames(table7.1.or)[3] <- "P-value"
colnames(table7.1.or)[4] <- "CI Lower"
colnames(table7.1.or)[5] <- "CI Upper"
print(table7.1.or)


stargazer(table7.1.or, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in Local Elections (Societal Discrimination): ATT Estimates as Odds Ratios - Simplified Model"
)

##########################
#### TABLE 40 (P. 38) ####
##########################

ndata <- mdata %>%
  select(voted2010_local, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

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

model8.1 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data1, weights=weights, 
                family=quasibinomial())
summary(model8.1)

model8.2 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data2, weights=weights, 
                family=quasibinomial())
summary(model8.2)

model8.3 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data3, weights=weights, 
                family=quasibinomial())
summary(model8.3)

model8.4 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data4, weights=weights, 
                family=quasibinomial())
summary(model8.4)

model8.5 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data5, weights=weights, 
                family=quasibinomial())
summary(model8.5)

model8.6 <- glm(voted2010_local~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                             identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                             medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                data=mat.data6, weights=weights, 
                family=quasibinomial())
summary(model8.6)

att8.1.or<-avg_comparisons(model8.1,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att8.1.or)

att8.2.or<-avg_comparisons(model8.2,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att8.2.or)

att8.3.or<-avg_comparisons(model8.3,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att8.3.or)

att8.4.or<-avg_comparisons(model8.4,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att8.4.or)

att8.5.or<-avg_comparisons(model8.5,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att8.5.or)

att8.6.or<-avg_comparisons(model8.6,
                           variables = "poldisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, poldisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att8.6.or)

### Vote in Local Elections (Societal Discrimination): ATT Estimates as Odds Ratios - ~Simplified Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att8.1.or$estimate, att8.2.or$estimate, att8.3.or$estimate, 
          att8.4.or$estimate, att8.5.or$estimate, att8.6.or$estimate) 
col2 <- c(att8.1.or$p.value, att8.2.or$p.value, att8.3.or$p.value, 
          att8.4.or$p.value, att8.5.or$p.value, att8.6.or$p.value)
col3 <- c(att8.1.or$conf.low, att8.2.or$conf.low, att8.3.or$conf.low, 
          att8.4.or$conf.low, att8.5.or$conf.low, att8.6.or$conf.low)
col4 <- c(att8.1.or$conf.high, att8.2.or$conf.high, att8.3.or$conf.high, 
          att8.4.or$conf.high, att8.5.or$conf.high, att8.6.or$conf.high)

table8.1.or<-cbind(col0, col1, col2, col3, col4)  
table8.1.or<-as.data.frame(table8.1.or)
table8.1.or <- table8.1.or %>%
  mutate(across(-col0, as.numeric))
table8.1.or <- table8.1.or %>% mutate(across(-col0, ~ round(., 6)))
colnames(table8.1.or)[1] <- "Model"
colnames(table8.1.or)[2] <- "Estimate"
colnames(table8.1.or)[3] <- "P-value"
colnames(table8.1.or)[4] <- "CI Lower"
colnames(table8.1.or)[5] <- "CI Upper"
print(table8.1.or)


stargazer(table8.1.or, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Vote in Local Elections (Political Discrimination): ATT Estimates as Odds Ratios - Simplified Model"
)

##########################
#### TABLE 41 (P. 38) ####
##########################

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

ndata <- mdata %>%
  select(ethnic_active, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi, vote_duty, efficacy, democ_satis, trust_parliament) %>%
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

att9.1.or<-avg_comparisons(model9.1,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data1, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att9.1.or)

att9.2.or<-avg_comparisons(model9.2,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data2, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att9.2.or)

att9.3.or<-avg_comparisons(model9.3,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data3, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att9.3.or)

att9.4.or<-avg_comparisons(model9.4,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data4, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att9.4.or)

att9.5.or<-avg_comparisons(model9.5,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data5, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att9.5.or)

att9.6.or<-avg_comparisons(model9.6,
                           variables = "socdisc_b",
                           vcov = ~subclass,
                           newdata = subset(mat.data6, socdisc_b == 1),
                           wts = "weights",
                           comparison = "lnoravg",
                           transform = "exp")
print(att9.6.or)

### Ethnic based engagement (Societla Discrimination): ATT Estimates as Odds Ratios - Complete Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att9.1.or$estimate, att9.2.or$estimate, att9.3.or$estimate, 
          att9.4.or$estimate, att9.5.or$estimate, att9.6.or$estimate) 
col2 <- c(att9.1.or$p.value, att9.2.or$p.value, att9.3.or$p.value, 
          att9.4.or$p.value, att9.5.or$p.value, att9.6.or$p.value)
col3 <- c(att9.1.or$conf.low, att9.2.or$conf.low, att9.3.or$conf.low, 
          att9.4.or$conf.low, att9.5.or$conf.low, att9.6.or$conf.low)
col4 <- c(att9.1.or$conf.high, att9.2.or$conf.high, att9.3.or$conf.high, 
          att9.4.or$conf.high, att9.5.or$conf.high, att9.6.or$conf.high)

table9.1.or<-cbind(col0, col1, col2, col3, col4)  
table9.1.or<-as.data.frame(table9.1.or)
table9.1.or <- table9.1.or %>%
  mutate(across(-col0, as.numeric))
table9.1.or <- table9.1.or %>% mutate(across(-col0, ~ round(., 6)))
colnames(table9.1.or)[1] <- "Model"
colnames(table9.1.or)[2] <- "Estimate"
colnames(table9.1.or)[3] <- "P-value"
colnames(table9.1.or)[4] <- "CI Lower"
colnames(table9.1.or)[5] <- "CI Upper"
print(table9.1.or)


stargazer(table9.1.or, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Ethnic based engagement (Societal Discrimination): ATT Estimates as Odds Ratios - Complete Model"
)

##########################
#### TABLE 42 (P. 38) ####
##########################

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

att10.1.or<-avg_comparisons(model10.1,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data1, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnoravg",
                            transform = "exp")
print(att10.1.or)

att10.2.or<-avg_comparisons(model10.2,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data2, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnoravg",
                            transform = "exp")
print(att10.2.or)

att10.3.or<-avg_comparisons(model10.3,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data3, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnoravg",
                            transform = "exp")
print(att10.3.or)

att10.4.or<-avg_comparisons(model10.4,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data4, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnoravg",
                            transform = "exp")
print(att10.4.or)

att10.5.or<-avg_comparisons(model10.5,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data5, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnoravg",
                            transform = "exp")
print(att10.5.or)

att10.6.or<-avg_comparisons(model10.6,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data6, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnoravg",
                            transform = "exp")
print(att10.6.or)

### Ethnic based engagement (Societla Discrimination): ATT Estimates as Odds Ratios - Complete Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att10.1.or$estimate, att10.2.or$estimate, att10.3.or$estimate, 
          att10.4.or$estimate, att10.5.or$estimate, att10.6.or$estimate) 
col2 <- c(att10.1.or$p.value, att10.2.or$p.value, att10.3.or$p.value, 
          att10.4.or$p.value, att10.5.or$p.value, att10.6.or$p.value)
col3 <- c(att10.1.or$conf.low, att10.2.or$conf.low, att10.3.or$conf.low, 
          att10.4.or$conf.low, att10.5.or$conf.low, att10.6.or$conf.low)
col4 <- c(att10.1.or$conf.high, att10.2.or$conf.high, att10.3.or$conf.high, 
          att10.4.or$conf.high, att10.5.or$conf.high, att10.6.or$conf.high)

table10.1.or<-cbind(col0, col1, col2, col3, col4)  
table10.1.or<-as.data.frame(table10.1.or)
table10.1.or <- table10.1.or %>%
  mutate(across(-col0, as.numeric))
table10.1.or <- table10.1.or %>% mutate(across(-col0, ~ round(., 6)))
colnames(table10.1.or)[1] <- "Model"
colnames(table10.1.or)[2] <- "Estimate"
colnames(table10.1.or)[3] <- "P-value"
colnames(table10.1.or)[4] <- "CI Lower"
colnames(table10.1.or)[5] <- "CI Upper"
print(table10.1.or)


stargazer(table10.1.or, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Ethnic based engagement (Political Discrimination): ATT Estimates as Odds Ratios - Complete Model"
)

##########################
#### TABLE 43 (P. 38) ####
##########################

formula<-(ethnic_active ~ 
            socdisc + poldisc+
            relatt_oth_r + pol_interest + polknowledge + partyid +
            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi)


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

ndata <- mdata %>%
  select(ethnic_active, socdisc_b, poldisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(socdisc_b~ poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

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

model11.1 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                 data=mat.data1, weights=weights, 
                 family=quasibinomial())
summary(model11.1)

model11.2 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                 data=mat.data2, weights=weights, 
                 family=quasibinomial())
summary(model11.2)

model11.3 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                 data=mat.data3, weights=weights, 
                 family=quasibinomial())
summary(model11.3)

model11.4 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                 data=mat.data4, weights=weights, 
                 family=quasibinomial())
summary(model11.4)

model11.5 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                 data=mat.data5, weights=weights, 
                 family=quasibinomial())
summary(model11.5)

model11.6 <- glm(ethnic_active~socdisc_b*(poldisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi), 
                 data=mat.data6, weights=weights, 
                 family=quasibinomial())
summary(model11.6)

att11.1.or<-avg_comparisons(model11.1,
                            variables = "socdisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data1, socdisc_b == 1),
                            wts = "weights",
                            comparison = "lnoravg",
                            transform = "exp")
print(att11.1.or)

att11.2.or<-avg_comparisons(model11.2,
                            variables = "socdisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data2, socdisc_b == 1),
                            wts = "weights",
                            comparison = "lnoravg",
                            transform = "exp")
print(att11.2.or)

att11.3.or<-avg_comparisons(model11.3,
                            variables = "socdisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data3, socdisc_b == 1),
                            wts = "weights",
                            comparison = "lnoravg",
                            transform = "exp")
print(att11.3.or)

att11.4.or<-avg_comparisons(model11.4,
                            variables = "socdisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data4, socdisc_b == 1),
                            wts = "weights",
                            comparison = "lnoravg",
                            transform = "exp")
print(att11.4.or)

att11.5.or<-avg_comparisons(model11.5,
                            variables = "socdisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data5, socdisc_b == 1),
                            wts = "weights",
                            comparison = "lnoravg",
                            transform = "exp")
print(att11.5.or)

att11.6.or<-avg_comparisons(model11.6,
                            variables = "socdisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data6, socdisc_b == 1),
                            wts = "weights",
                            comparison = "lnoravg",
                            transform = "exp")
print(att11.6.or)


### Ethnic based engagement (Societal Discrimination): ATT Estimates as Odds Ratios - Simplified Model

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att11.1.or$estimate, att11.2.or$estimate, att11.3.or$estimate, 
          att11.4.or$estimate, att11.5.or$estimate, att11.6.or$estimate) 
col2 <- c(att11.1.or$p.value, att11.2.or$p.value, att11.3.or$p.value, 
          att11.4.or$p.value, att11.5.or$p.value, att11.6.or$p.value)
col3 <- c(att11.1.or$conf.low, att11.2.or$conf.low, att11.3.or$conf.low, 
          att11.4.or$conf.low, att11.5.or$conf.low, att11.6.or$conf.low)
col4 <- c(att11.1.or$conf.high, att11.2.or$conf.high, att11.3.or$conf.high, 
          att11.4.or$conf.high, att11.5.or$conf.high, att11.6.or$conf.high)

table11.1.or<-cbind(col0, col1, col2, col3, col4)  
table11.1.or<-as.data.frame(table11.1.or)
table11.1.or <- table11.1.or %>%
  mutate(across(-col0, as.numeric))
table11.1.or <- table11.1.or %>% mutate(across(-col0, ~ round(., 6)))
colnames(table11.1.or)[1] <- "Model"
colnames(table11.1.or)[2] <- "Estimate"
colnames(table11.1.or)[3] <- "P-value"
colnames(table11.1.or)[4] <- "CI Lower"
colnames(table11.1.or)[5] <- "CI Upper"
print(table11.1.or)


stargazer(table11.1.or, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Ethnic based engagement (Societal Discrimination): ATT Estimates as Odds Ratios - Simplified Model"
)

##########################
#### TABLE 44 (P. 39) ####
##########################

ndata <- mdata %>%
  select(ethnic_active, poldisc_b, socdisc, relatt_oth_r , pol_interest , polknowledge , partyid , identity, english, native_born, female, age, education2, highinc, medinc, misinc, black_caribbean, indian, pakistani, bangladeshi) %>%
  na.omit() %>%
  as.data.frame() # Must be a data.frame, not a tibble

m.out.fp <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "probit")
m.out.fp
summary(m.out.fp, un = FALSE)

m.out.fl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "full", distance = "glm", link = "logit")
m.out.fl

m.out.np <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "probit")

m.out.np

m.out.nl <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "nearest", distance = "glm", link = "logit")

m.out.nl

m.out.op <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "probit")

m.out.op

m.out.ol <- matchit(poldisc_b~ socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                      identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                      medinc+ misinc+ black_caribbean+ indian+ pakistani+ bangladeshi,
                    data = ndata, method = "optimal", distance = "glm", link = "logit")

m.out.ol

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

model12.1 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                 data=mat.data1, weights=weights, 
                 family=quasibinomial())
summary(model12.1)

model12.2 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                 data=mat.data2, weights=weights, 
                 family=quasibinomial())
summary(model12.2)

model12.3 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                 data=mat.data3, weights=weights, 
                 family=quasibinomial())
summary(model12.3)

model12.4 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                 data=mat.data4, weights=weights, 
                 family=quasibinomial())
summary(model12.4)

model12.5 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                 data=mat.data5, weights=weights, 
                 family=quasibinomial())
summary(model12.5)

model12.6 <- glm(ethnic_active~poldisc_b*(socdisc + relatt_oth_r + pol_interest + polknowledge + partyid +
                                            identity+ english+ native_born+ female+ age+ education2+ highinc+ 
                                            medinc+ misinc+ black_caribbean+ indian+ pakistani+bangladeshi), 
                 data=mat.data6, weights=weights, 
                 family=quasibinomial())
summary(model12.6)

att12.1.or<-avg_comparisons(model12.1,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data1, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnoravg",
                            transform = "exp")
print(att12.1.or)

att12.2.or<-avg_comparisons(model12.2,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data2, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att12.2.or)


att12.3.or<-avg_comparisons(model12.3,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data3, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att12.3.or)

att12.4.or<-avg_comparisons(model12.4,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data4, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att12.4.or)

att12.5.or<-avg_comparisons(model12.5,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data5, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att12.5.or)

att12.6.or<-avg_comparisons(model12.6,
                            variables = "poldisc_b",
                            vcov = ~subclass,
                            newdata = subset(mat.data6, poldisc_b == 1),
                            wts = "weights",
                            comparison = "lnratioavg",
                            transform = "exp")
print(att12.6.or)

col0 <- c("Full Probit", "Full Logit", 
          "Nearest Probit", "Nearest Logit", 
          "Optimal Probit", "Optimal Pobit")
col1 <- c(att12.1.or$estimate, att12.2.or$estimate, att12.3.or$estimate, 
          att12.4.or$estimate, att12.5.or$estimate, att12.6.or$estimate) 
col2 <- c(att12.1.or$p.value, att12.2.or$p.value, att12.3.or$p.value, 
          att12.4.or$p.value, att12.5.or$p.value, att12.6.or$p.value)
col3 <- c(att12.1.or$conf.low, att12.2.or$conf.low, att12.3.or$conf.low, 
          att12.4.or$conf.low, att12.5.or$conf.low, att12.6.or$conf.low)
col4 <- c(att12.1.or$conf.high, att12.2.or$conf.high, att12.3.or$conf.high, 
          att12.4.or$conf.high, att12.5.or$conf.high, att12.6.or$conf.high)

table12.1.or<-cbind(col0, col1, col2, col3, col4)  
table12.1.or<-as.data.frame(table12.1.or)
table12.1.or <- table12.1.or %>%
  mutate(across(-col0, as.numeric))
table12.1.or <- table12.1.or %>% mutate(across(-col0, ~ round(., 6)))
colnames(table12.1.or)[1] <- "Model"
colnames(table12.1.or)[2] <- "Estimate"
colnames(table12.1.or)[3] <- "P-value"
colnames(table12.1.or)[4] <- "CI Lower"
colnames(table12.1.or)[5] <- "CI Upper"
print(table12.1.or)


stargazer(table12.1.or, 
          type = "latex", 
          summary = F,
          style="APSR", 
          title = "Ethnic based engagement (Political Discrimination): ATT Estimates as Odds Ratios - Simplified Model"
)

##########################
#### TABLE 45 (P. 41) ####
##########################

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

stargazer(mod_sum16, mod_sum18, mod_sum20, 
          style="APSR", 
          covariate.labels = modellabels8, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Alternative outcomes (Specific Measures) - Support for Violend Demonstrations",
          dep.var.labels = dvlabel7
)

##########################
#### TABLE 46 (P. 42) ####
##########################

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

modellabels8<-c("Societal Discrimination", "Political Discrimination",
                "Worship Attendance","Participation in Social Networks", "Political Interest", "Political Knowledge", 
                "Party ID (Yes=1)", "Close to British ID", "English (Main Lang)", "Native Born",
                "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
                "Black Caribbean", "Indian", "Pakistani", "Bangladeshi",
                "Vote Duty", "Political Efficacy","Democratic Satisfaction","Trust Parliament", 
                "National economic future", "Use of Internet"
)

dvlabel8<-c("Non-electoral political participation")

stargazer(mod_sum17, mod_sum19, mod_sum21, 
          style="APSR", 
          covariate.labels = modellabels8, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Alternative outcomes (Specific Measures) - Non-electoral Political Participation",
          dep.var.labels = dvlabel8
)

##########################
#### TABLE 47 (P. 46) ####
##########################

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

eaa.coef <- ethnic_active_adapted$coefficients  # point estimates
vc.eaa <- vcov(ethnic_active_adapted)

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

##########################
#### TABLE 48 (P. 47) ####
##########################

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

##########################
#### FIGURE 17 (P. 47) ###
##########################

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

##########################
#### FIGURE 18 (P. 48) ###
##########################

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

##########################
#### TABLE 49 (P. 48) ####
##########################

data<-haven::read_dta("SCIP_UK_W1.dta")

stargazer(as.data.frame(data[,c("race_pol_disc_1", "race_pol_disc_2")]), 
          covariate.labels = c("Political Discrimination 1", "Political Discrimination 2"), 
          title = "Summary Statistrics for Specific Measures (Political Discrimination)",
          style = "APSR", 
          out.header = T
)

##########################
#### TABLE 50 (P. 48) ####
##########################

stargazer(as.data.frame(data[,c("race_soc_disc_1", "race_soc_disc_2")]), 
          covariate.labels = c("Societal Discrimination 1", "Societal Discrimination 2"), 
          title = "Summary Statistrics for Specific Measures (Societal Discrimination)",
          style = "APSR", 
          out.header = T
)

##########################
#### TABLE 51 (P. 49) ####
##########################
  
data<-haven::read_dta("SCIP_NL_W1.dta")

stargazer(as.data.frame(data[,c("race_pol_disc_1", "race_pol_disc_2")]), 
          covariate.labels = c("Political Discrimination 1", "Political Discrimination 2"), 
          title = "Summary Statistrics for Specific Measures (Political Discrimination)",
          style = "APSR", 
          out.header = T
)

##########################
#### TABLE 52 (P. 49) ####
##########################

stargazer(as.data.frame(data[,c("race_soc_disc_1", "race_soc_disc_2")]), 
          covariate.labels = c("Societal Discrimination 1", "Societal Discrimination 2"), 
          title = "Summary Statistrics for Specific Measures (Societal Discrimination)",
          style = "APSR", 
          out.header = T
)

##########################
#### FIGURE 19 (P. 50) ###
##########################

data<-haven::read_dta("SCIP_UK_W1.dta")

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

# Create a data frame
data1 <- data.frame(values1, frequencies1)
data2 <- data.frame(values2, frequencies2)
data3 <- data.frame(values3, frequencies3)
data4 <- data.frame(values4, frequencies4)

# Panel (b)
p1 <- ggplot(data1, aes(x = factor(values1), y = frequencies1)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(labels = c("0", "1", "2")) +
  labs(x = "Values", y = "Frequency", title = "Frequencies for Political Discrimination 1 (UK)")

p1 + geom_text(aes(label = frequencies1), vjust = -0.5, color = "black", size = 3)

# Panel (d)
p2 <- ggplot(data2, aes(x = factor(values2), y = frequencies2)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(labels = c("0", "1", "2", "3", "4", "5", "6")) +
  labs(x = "Values", y = "Frequency", title = "Frequencies for Political Discrimination 2 (UK)")

p2 + geom_text(aes(label = frequencies2), vjust = -0.5, color = "black", size = 3)

# Panel (a)
p3 <- ggplot(data3, aes(x = factor(values3), y = frequencies3)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(labels = c("0", "1", "2")) +
  labs(x = "Values", y = "Frequency", title = "Frequencies for Societal Discrimination 1 (UK)")

p3 + geom_text(aes(label = frequencies3), vjust = -0.5, color = "black", size = 3)


# Panel (b)
p4 <- ggplot(data4, aes(x = factor(values4), y = frequencies4)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(labels = c("0", "1", "2", "3", "4", "5", "6")) +
  labs(x = "Values", y = "Frequency", title = "Frequencies for Societal Discrimination 2 (UK)")

p4 + geom_text(aes(label = frequencies4), vjust = -0.5, color = "black", size = 3)

##########################
#### FIGURE 20 (P. 51) ###
##########################

data<-haven::read_dta("SCIP_NL_W1.dta")

table(data$race_pol_disc_1)
table(data$race_pol_disc_2)
table(data$race_soc_disc_1)
table(data$race_soc_disc_2)

values1 <- c(0, 1, 2)
frequencies1 <- c(3187, 136, 32)

values2 <- c(0, 1, 2, 3, 4, 6)
frequencies2 <- c(3195, 8, 44, 78, 6, 24)

values3 <- c(0, 1, 2)
frequencies3 <- c(3233, 109, 13)

values4 <- c(0, 1, 2, 3, 4, 6)
frequencies4 <- c(3236, 9, 31, 67, 4, 8)

# Create a data frame
data1 <- data.frame(values1, frequencies1)
data2 <- data.frame(values2, frequencies2)
data3 <- data.frame(values3, frequencies3)
data4 <- data.frame(values4, frequencies4)

# Panel (b)
p1 <- ggplot(data1, aes(x = factor(values1), y = frequencies1)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(labels = c("0", "1", "2")) +
  labs(x = "Values", y = "Frequency", title = "Frequencies for Political Discrimination 1 (NL)")

p1 + geom_text(aes(label = frequencies1), vjust = -0.5, color = "black", size = 3)

# Panel (d)
p2 <- ggplot(data2, aes(x = factor(values2), y = frequencies2)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(labels = c("0", "1", "2", "3", "4", "5", "6")) +
  labs(x = "Values", y = "Frequency", title = "Frequencies for Political Discrimination 2 (NL)")

p2 + geom_text(aes(label = frequencies2), vjust = -0.5, color = "black", size = 3)

# Panel (a)
p3 <- ggplot(data3, aes(x = factor(values3), y = frequencies3)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(labels = c("0", "1", "2")) +
  labs(x = "Values", y = "Frequency", title = "Frequencies for Societal Discrimination 1 (NL)")

p3 + geom_text(aes(label = frequencies3), vjust = -0.5, color = "black", size = 3)


# Panel (c)
p4 <- ggplot(data4, aes(x = factor(values4), y = frequencies4)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(labels = c("0", "1", "2", "3", "4", "5", "6")) +
  labs(x = "Values", y = "Frequency", title = "Frequencies for Societal Discrimination 2 (NL)")

p4 + geom_text(aes(label = frequencies4), vjust = -0.5, color = "black", size = 3)

##########################
#### TABLE 53 (P. 52) ####
##########################

data<-haven::read_dta("SCIP_UK_W1.dta")
stargazer(as.data.frame(data[,1:646]), type = "text")

stargazer(as.data.frame(data[,c("rel_att", "pol_int", "political_knowledge", "val_three", "lan_home", "female",
                                "age", "educ_yrs", "highinc", "medinc", "misinc", "pakistani" 
)]), 
covariate.labels = c("Worship Attendance", "Political Interest", "Political Knowledge", 
                     "Ireconcilable values", "Language at home",
                     "Female", "Age", "Education", "High Income", "Med Income", "Missing Income", "Pakistani"), 

title = "Summary Statistics for Control Variables in SCIP UK Dataset",
style = "APSR", 
out.header = T
)

##########################
#### TABLE 54 (P. 53) ####
##########################

data<-haven::read_dta("SCIP_NL_W1.dta")
stargazer(as.data.frame(data[,1:602]), type = "text")

stargazer(as.data.frame(data[,c("rel_att", "pol_int", "political_knowledge", "val_three",
                                "lan_home", "female", "age", "educ_yrs", "highinc", "medinc", "misinc",
                                "bulgarian", "moroccan", "polish", "surinamese", "turkish"
)]), 
covariate.labels = c("Worship Attendance", "Political Interest", "Political Knowledge", 
                     "Irreconcilable values", "Language at home",
                     "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
                     "Bulgarian" ,"Morrocan", "Polish", "Surinamese", "Turkish"), 

title = "Summary Statistics for Control Variables in SCIP NL Dataset",
style = "APSR", 
out.header = T
)

##########################
#### TABLE 55 (P. 54) ####
##########################

data<-haven::read_dta("SCIP_UK_W1.dta")

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

##########################
#### TABLE 56 (P. 55) ####
##########################

data<-haven::read_dta("SCIP_NL_W1.dta")

formula<-(ebe ~ broad_discrimination_i +
            rel_att + pol_int + political_knowledge + val_three +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            bulgarian + moroccan + polish + surinamese + turkish)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

ebe.broad.iii <- glm(formula, family=binomial(link=logit), data=mdata)
summary(ebe.broad.iii)

pe.3 <- ebe.broad.iii$coefficients  # point estimates
vc.3 <- vcov(ebe.broad.iii)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.3, vc.3)

# nscen = 1 cause I am interested only in borad disc. i
xhyp3 <- cfMake(formula, mdata, nscen=1, f=mean)

xhyp3 <- cfName(xhyp3, "broad_discrimination_i", scen=1) 
xhyp3 <-cfChange(xhyp3, "broad_discrimination_i", x=1, xpre=0, scen=1)

# use the same CI as in the original study
# most relaxed CI that can be used 
xhyp3
yhyp3<-logitsimfd(xhyp3, simbetas, ci=0.90)
yhyp3


formula<-(ebe ~ broad_discrimination_ii +
            rel_att + pol_int + political_knowledge + val_three +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            bulgarian + moroccan + polish + surinamese + turkish)

mdata<-extractdata(formula, data, na.rm=TRUE)

k <- length(names(mdata))
for (k in 1:k) {
  print(class(mdata[,k]))
}
for (k in 1:k) {
  mdata[,k] <- as.numeric(mdata[,k])
}

ebe.broad.iv <- glm(formula, family=binomial(link=logit), data=mdata)
summary(ebe.broad.iv)

pe.4 <- ebe.broad.iv$coefficients  # point estimates
vc.4 <- vcov(ebe.broad.iv)

### simulation
sims <- 10000
simbetas <- mvrnorm(sims, pe.4, vc.4)

# nscen = 1 cause I am interested only in borad disc. i
xhyp4 <- cfMake(formula, mdata, nscen=1, f=mean)

xhyp4 <- cfName(xhyp4, "broad_discrimination_ii", scen=1) 
xhyp4 <-cfChange(xhyp4, "broad_discrimination_ii", x=1, xpre=0, scen=1)

# use the same CI as in the original study
# most relaxed CI that can be used 
xhyp4
yhyp4<-logitsimfd(xhyp4, simbetas, ci=0.90)
yhyp4


modellabels<-c("Broad Discrimination 1", "Broad Discrimination 2",
               "Worship Attendance", "Political Interest", "Political Knowledge", 
               "Ireconcilable values", "Language at home",
               "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
               "Bulgarian" ,"Morrocan", "Polish", "Surinamese", "Turkish"
)

dvlabel<-c("Ethnic-Based Engagement")

stargazer(ebe.broad.iii, ebe.broad.iv,
          style="APSR", 
          covariate.labels = modellabels, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Ethnic Based Engagement (Broad discrimination)",
          dep.var.labels = dvlabel)


##########################
#### FIGURE 21 (P. 55) ####
##########################

## Panel (a)

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

## Panel (b) 

trace5 <- ropeladder(
  x=yhyp3$pe,
  lower=yhyp3$lower,
  upper=yhyp3$upper,
  labels=c("Broad Discrimination 1"), 
  col= c("Black"),
  pch=c(16),
  cex=2, 
  plot=1)

trace6 <- ropeladder(
  x=yhyp4$pe,
  lower=yhyp4$lower,
  upper=yhyp4$upper,
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
trace5$entryheight <- .20
trace6$entryheight <- .20

at.x1 <- seq(-.1, .1,.1)
at.x2 <- seq(-.1, .1,.1)



tc_nl_scip_2<-tile(trace5, trace6,
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
                   undertitle = list(labels="Change in Predicted Probability (Min-Max)", cex=.7, x=.1),
                   output = list(file = "nl_scip_2", type="pdf", width=12), 
                   frame=FALSE,
                   gridlines=list(type="t", col="grey65")
)

##########################
#### TABLE 57 (P. 56) ####
##########################

data<-haven::read_dta("SCIP_UK_W1.dta")

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

table(data$race_pol_disc_1)
table(data$race_soc_disc_1)

val3.spec.1 <- multinom(val_three ~ race_soc_disc_1 + race_pol_disc_1 +
                          rel_att + pol_int + political_knowledge +
                          lan_home + female + age + educ_yrs + highinc + medinc + misinc +
                          pakistani, Hess = T, data=mdata)


summary(val3.spec.1)

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

val3.spec.2 <- multinom(val_three ~ race_soc_disc_2 + race_pol_disc_2 +
                          rel_att + pol_int + political_knowledge +
                          lan_home + female + age + educ_yrs + highinc + medinc + misinc +
                          pakistani, Hess = T, data=mdata)

summary(val3.spec.2)

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

##########################
#### TABLE 58 (P. 57) ####
##########################

data<-haven::read_dta("SCIP_NL_W1.dta")

formula<-(val_three ~ race_pol_disc_1 + race_soc_disc_1 + 
            rel_att + pol_int + political_knowledge + host_cntry_importance +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            bulgarian + moroccan + polish + surinamese + turkish
          
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

val3.spec.1 <- multinom(val_three ~ race_pol_disc_1 + race_soc_disc_1 +
                          rel_att + pol_int + political_knowledge + host_cntry_importance +
                          lan_home + female + age + educ_yrs + highinc + medinc + misinc +
                          bulgarian + moroccan + polish + surinamese + turkish, Hess = T, data=mdata)

summary(val3.spec.1)

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

formula<-(val_three ~ race_pol_disc_2 + race_soc_disc_2 + 
            rel_att + pol_int + political_knowledge + host_cntry_importance +
            lan_home + female + age + educ_yrs + highinc + medinc + misinc +
            bulgarian + moroccan + polish + surinamese + turkish
          
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

val3.spec.2 <- multinom(val_three ~ race_pol_disc_2 + race_soc_disc_2 +
                          rel_att + pol_int + political_knowledge + host_cntry_importance +
                          lan_home + female + age + educ_yrs + highinc + medinc + misinc +
                          bulgarian + moroccan + polish + surinamese + turkish, Hess = T, data=mdata)

summary(val3.spec.2)

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

modellabels<-c("Societal Discrimination 1", "Political Discrimination 1",
               "Societal Discrimination 2", "Political Discrimination 2",
               "Worship Attendance", "Political Interest", "Political Knowledge", "Language at home",
               "Female", "Age", "Education", "High Income", "Med Income", "Missing Income",
               "Bulgarian" ,"Morrocan", "Polish", "Surinamese", "Turkish"
)

dvlabel<-c("Irreconcilable values")

stargazer(val3.spec.1, val3.spec.2,
          style="APSR", 
          covariate.labels = modellabels, 
          out.header=T,
          model.numbers = TRUE, 
          title = "Irreconcilable values: Specific Measures (Political and Societal Discrimination) for NL",
          dep.var.labels = dvlabel)

##########################
#### FIGURE 22 (P. 58) ###
##########################

## Panel (a): upper

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

## Panel (a): lower

## I retyped the result from the output

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

## Panel (b): upper

# Neutral relative to disagreement 
yhype5m <- c(0.01679081, -0.0376336) 
yhype5l<- c(-0.09482910, -0.1156704)
yhype5u<- c(0.1607363, 0.06411349)

# Agree relative to disagree
yhype5.2m <- c(0.19512976, -0.1442187) 
yhype5.2l<- c(0.04624222, -0.2519552)
yhype5.2u<- c(0.3231838, -0.02850566)


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
at.x2 <-seq(-.3,.4,.1)

tc_nl_scip_4<-tile(trace7,
                   trace8,
                   vertmark,
                   xaxis=list(at1 = at.x1, 
                              at2= at.x2, 
                              cex=.8), 
                   topaxis=list(at1 = at.x1, 
                                at2= at.x2, 
                                cex=.8,
                                add = rep(TRUE,2)),
                   topaxistitle = list(labels1= "Neutral vs. Disagreement", 
                                       labels2= "Agreement vs. Disagreement"),
                   undertitle = list(labels="Change in Predicted Probability (Min-Max)", cex=.7, x=1),
                   plottitle= list(label=c("Inconcievable values (Specific discrimination - racial) "), cex=1),
                   output = list(file = "nl_scip_4", width=12, type="pdf"), 
                   frame=FALSE,
                   gridlines=list(type="x", col="grey65")
)

## Panel (b): lower

## Again I retyped the result from the output

# Neutral relative to disagreement 
yhype6m <- c(0.01230713, -0.09145126) 
yhype6l<- c(-0.11658882, -0.16021293)
yhype6u<- c(0.1911408, 0.005018089)

# Agree relative to disagree
yhype6.2m <- c(0.23453024, -0.13999009) 
yhype6.2l<- c(0.04822912, -0.26510664)
yhype6.2u<- c(0.3786218, -0.003041571)

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
at.x2 <-seq(-.3,.4,.1)

tc_nl_scip_5<-tile(trace9,
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
                   output = list(file = "nl_scip_5", width=12, type="pdf"), 
                   frame=FALSE,
                   gridlines=list(type="x", col="grey65")
)

toc()
# 1024.706 sec elapsed = 17.06667 min. to execute the code.

#################
#### THE END ####
#################
