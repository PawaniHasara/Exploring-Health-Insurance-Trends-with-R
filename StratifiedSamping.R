setwd("D:/Pawani/Uni/3rd year-semester 1/This year/IS 3001-Sampling Techniques/Group Project")

install.packages("survey")
library(survey)

install.packages("readxl")
library(readxl)

install.packages("sampler")
library(sampler)

install.packages("dplyr")
library(dplyr)

Dataset <- read_excel("Dataset_Gr16.xlsx")

###For ppulation

attach(Dataset)

#Mean values

# Population mean for Age
pop_mean_age=mean(age,na.rm=TRUE)
pop_mean_age

# Population mean for bmi
pop_mean_bmi=mean(bmi,na.rm=TRUE)
pop_mean_bmi

# Population mean for charges
pop_mean_charges=mean(charges,na.rm=TRUE)
pop_mean_charges

# Mean charge for smokers
mean_charge_smoker <- Dataset %>%
  filter(smoker == "yes") %>%
  summarise(mean_charge = mean(charges))
round(mean_charge_smoker$mean_charge, 2)

# Mean charge for non-smokers
mean_charge_non_smoker <- Dataset %>%
  filter(smoker == "no") %>%
  summarise(mean_charge = mean(charges))
round(mean_charge_non_smoker$mean_charge, 2)

# Mean charge for male
mean_charge_male <- Dataset %>%
  filter(sex == "male") %>%
  summarise(mean_charge = mean(charges))
round(mean_charge_male$mean_charge, 2)

# Mean charge for female
mean_charge_female <- Dataset %>%
  filter(sex == "female") %>%
  summarise(mean_charge = mean(charges))
round(mean_charge_female$mean_charge, 2)

#Total values

# Population total for Age
pop_total_age=sum(age,na.rm=TRUE)
pop_total_age

# Population total for bmi
pop_total_bmi=sum(bmi,na.rm=TRUE)
pop_total_bmi

# Population total for charges
pop_total_charges=sum(charges,na.rm=TRUE)
pop_total_charges

# Total charge for smokers
total_charge_smoker <- Dataset %>%
  filter(smoker == "yes") %>%
  summarise(total_charge = sum(charges))
round(total_charge_smoker$total_charge, 2)

# Total charge for non smokers
total_charge_non_smoker <- Dataset %>%
  filter(smoker == "no") %>%
  summarise(total_charge = sum(charges))
round(total_charge_non_smoker$total_charge, 2)

# Total charge for male
total_charge_male <- Dataset %>%
  filter(sex == "male") %>%
  summarise(total_charge = sum(charges))
round(total_charge_male$total_charge, 2)

# Total charge for male
total_charge_female <- Dataset %>%
  filter(sex == "female") %>%
  summarise(total_charge = sum(charges))
round(total_charge_female$total_charge, 2)

#Proportion values

#Proportion for sex
sex_proportion=table(sex)/length(sex)
sex_proportion

#Proportion for smoker
smoker_proportion=table(smoker)/length(smoker)
smoker_proportion

#Proportion for region
region_proportion=table(region)/length(region)
region_proportion

detach(Dataset)

#strata sizes for Stratified sampling
strata_size=ssampcalc(Dataset,594,`region`)
strata_size

#Sample 1
set.seed(345)
strat_samp1=ssamp(Dataset,594,`region`)
strat_samp1

attach(strat_samp1)

strat_samp1$w1 <- ifelse(strat_samp1$region == "northeast", round(324/144, 2),
                          ifelse(strat_samp1$region == "northwest", round(325/144, 2),
                                 ifelse(strat_samp1$region == "southeast", round(364/162, 2),
                                        ifelse(strat_samp1$region == "southwest", round(325/144, 2), 0))))

strat_design1 <- svydesign(id = ~1, strata = ~region, weights = ~w1, data = strat_samp1) 

#sample mean for age
str_sample_mean_age=svymean(~age,strat_design1)
str_sample_mean_age

#sample mean for bmi
str_sample_mean_bmi=svymean(~bmi,strat_design1)
str_sample_mean_bmi

# sample mean for charges
str_sample_mean_charges=svymean(~charges,strat_design1)
str_sample_mean_charges

# Sample mean charge for smokers
smokers_data <- strat_samp1 %>%
  filter(smoker == "yes")
mean_charges_smokers <- mean(smokers_data$charges)
round(mean_charges_smokers, 2)

#Standard error for the mean charge for smokers
n_smokers <- nrow(smokers_data)
sd_charges_smokers <- sd(smokers_data$charges)
se_charges_smokers <- sd_charges_smokers / sqrt(n_smokers)
round(se_charges_smokers, 2)

# Sample mean charge for non smokers
non_smokers_data <- strat_samp1 %>%
  filter(smoker == "no")
mean_charges_non_smokers <- mean(non_smokers_data$charges)
round(mean_charges_non_smokers, 2)

#Standard error for the mean charge for non smokers
n_non_smokers <- nrow(non_smokers_data)
sd_charges_non_smokers <- sd(non_smokers_data$charges)
se_charges_non_smokers <- sd_charges_non_smokers / sqrt(n_non_smokers)
round(se_charges_non_smokers, 2)

# Sample mean charge for males
male_data <- strat_samp1 %>%
  filter(sex == "male")
mean_charges_male <- mean(male_data$charges)
round(mean_charges_male, 2)

#Standard error for the mean charge for male
n_male <- nrow(male_data)
sd_charges_male <- sd(male_data$charges)
se_charges_male <- sd_charges_male / sqrt(n_male)
round(se_charges_male, 2)

# Sample mean charge for females
female_data <- strat_samp1 %>%
  filter(sex == "female")
mean_charges_female <- mean(female_data$charges)
round(mean_charges_female, 2)

#Standard error for the mean charge for female
n_female <- nrow(female_data)
sd_charges_female <- sd(female_data$charges)
se_charges_female <- sd_charges_female / sqrt(n_female)
round(se_charges_female, 2)

#Total values

# sample total for Age
str_total_age=svytotal(~age,strat_design1)
str_total_age

# sample total for bmi
str_total_bmi=svytotal(~bmi,strat_design1)
str_total_bmi

# sample total for charges
str_total_charges=svytotal(~charges,strat_design1)
str_total_charges

# Sample total charge for smokers
smokers_data <- strat_samp1 %>%
  filter(smoker == "yes")
total_charges_smokers <- sum(smokers_data$charges)
round(total_charges_smokers, 2)

#Standard error for the total charge for smokers
n_smokers <- nrow(smokers_data)
sd_charges_smokers <- sd(smokers_data$charges)
se_charges_smokers <- sd_charges_smokers / sqrt(n_smokers)
round(se_charges_smokers, 2)

# Sample total charge for non smokers
non_smokers_data <- strat_samp1 %>%
  filter(smoker == "no")
total_charges_non_smokers <- sum(non_smokers_data$charges)
round(total_charges_non_smokers, 2)

#Standard error for the total charge for non smokers
n_non_smokers <- nrow(non_smokers_data)
sd_charges_non_smokers <- sd(non_smokers_data$charges)
se_charges_non_smokers <- sd_charges_non_smokers / sqrt(n_non_smokers)
round(se_charges_non_smokers, 2)

# Sample total charge for males
male_data <- strat_samp1 %>%
  filter(sex == "male")
total_charges_male <- sum(male_data$charges)
round(total_charges_male, 2)

#Standard error for the total charge for male
n_male <- nrow(male_data)
sd_charges_male <- sd(male_data$charges)
se_charges_male <- sd_charges_male / sqrt(n_male)
round(se_charges_male, 2)

# Sample total charge for females
female_data <- strat_samp1 %>%
  filter(sex == "female")
total_charges_female <- sum(female_data$charges)
round(total_charges_female, 2)

#Standard error for the total charge for female
n_female <- nrow(female_data)
sd_charges_female <- sd(female_data$charges)
se_charges_female <- sd_charges_female / sqrt(n_female)
round(se_charges_female, 2)

#Proportion values

# sample proportion for sex
str_prop_sex=svymean(~sex,strat_design1)
str_prop_sex

# sample proportion for smoker
str_prop_smoker=svymean(~smoker,strat_design1)
str_prop_smoker

detach(strat_samp1)

#Regression Estimation
plot(strat_samp1$age,strat_samp1$charges)
regression_model1 <- lm(charges ~ age, data = as.data.frame(strat_samp1))
regression_model1

attach(Dataset)

mean_charges = 4174.7 + (231.6*mean(age))
mean_charges

detach(Dataset)


#Sample 2
set.seed(521)
strat_samp2=ssamp(Dataset,594,`region`)
strat_samp2


attach(strat_samp2)

strat_samp2$w2 <- ifelse(strat_samp2$region == "northeast", round(324/144, 2),
                         ifelse(strat_samp2$region == "northwest", round(325/144, 2),
                                ifelse(strat_samp2$region == "southeast", round(364/162, 2),
                                       ifelse(strat_samp2$region == "southwest", round(325/144, 2), 0))))

strat_design2 <- svydesign(id = ~1, strata = ~region, weights = ~w2, data = strat_samp2) 

#sample mean for age
str_sample_mean_age2=svymean(~age,strat_design2)
str_sample_mean_age2

#sample mean for bmi
str_sample_mean_bmi2=svymean(~bmi,strat_design2)
str_sample_mean_bmi2

# sample mean for charges
str_sample_mean_charges2=svymean(~charges,strat_design2)
str_sample_mean_charges2

# Sample mean charge for smokers
smokers_data2 <- strat_samp2 %>%
  filter(smoker == "yes")
mean_charges_smokers2 <- mean(smokers_data2$charges)
round(mean_charges_smokers2, 2)

#Standard error for the mean charge for smokers
n_smokers2 <- nrow(smokers_data2)
sd_charges_smokers2 <- sd(smokers_data2$charges)
se_charges_smokers2 <- sd_charges_smokers2 / sqrt(n_smokers2)
round(se_charges_smokers2, 2)

# Sample mean charge for non smokers
non_smokers_data2 <- strat_samp2 %>%
  filter(smoker == "no")
mean_charges_non_smokers2 <- mean(non_smokers_data2$charges)
round(mean_charges_non_smokers2, 2)

#Standard error for the mean charge for non smokers
n_non_smokers2 <- nrow(non_smokers_data2)
sd_charges_non_smokers2 <- sd(non_smokers_data2$charges)
se_charges_non_smokers2 <- sd_charges_non_smokers2 / sqrt(n_non_smokers2)
round(se_charges_non_smokers2, 2)

# Sample mean charge for males
male_data2 <- strat_samp2 %>%
  filter(sex == "male")
mean_charges_male2 <- mean(male_data2$charges)
round(mean_charges_male2, 2)

#Standard error for the mean charge for male
n_male2 <- nrow(male_data2)
sd_charges_male2 <- sd(male_data2$charges)
se_charges_male2 <- sd_charges_male2 / sqrt(n_male2)
round(se_charges_male2, 2)

# Sample mean charge for females
female_data2 <- strat_samp2 %>%
  filter(sex == "female")
mean_charges_female2 <- mean(female_data2$charges)
round(mean_charges_female2, 2)

#Standard error for the mean charge for female
n_female2 <- nrow(female_data2)
sd_charges_female2 <- sd(female_data2$charges)
se_charges_female2 <- sd_charges_female2 / sqrt(n_female2)
round(se_charges_female2, 2)

#Total values

# sample total for Age
str_total_age2=svytotal(~age,strat_design2)
str_total_age2

# sample total for bmi
str_total_bmi2=svytotal(~bmi,strat_design2)
str_total_bmi2

# sample total for charges
str_total_charges2=svytotal(~charges,strat_design2)
str_total_charges2

# Sample total charge for smokers
smokers_data2 <- strat_samp2 %>%
  filter(smoker == "yes")
total_charges_smokers2 <- sum(smokers_data2$charges)
round(total_charges_smokers2, 2)

#Standard error for the total charge for smokers
n_smokers2 <- nrow(smokers_data2)
sd_charges_smokers2 <- sd(smokers_data2$charges)
se_charges_smokers2 <- sd_charges_smokers2 / sqrt(n_smokers2)
round(se_charges_smokers2, 2)

# Sample total charge for non smokers
non_smokers_data2 <- strat_samp2 %>%
  filter(smoker == "no")
total_charges_non_smokers2 <- sum(non_smokers_data2$charges)
round(total_charges_non_smokers2, 2)

#Standard error for the total charge for non smokers
n_non_smokers2 <- nrow(non_smokers_data2)
sd_charges_non_smokers2 <- sd(non_smokers_data2$charges)
se_charges_non_smokers2 <- sd_charges_non_smokers2 / sqrt(n_non_smokers2)
round(se_charges_non_smokers2, 2)

# Sample total charge for males
male_data2 <- strat_samp2 %>%
  filter(sex == "male")
total_charges_male2 <- sum(male_data2$charges)
round(total_charges_male2, 2)

#Standard error for the total charge for male
n_male2 <- nrow(male_data2)
sd_charges_male2 <- sd(male_data2$charges)
se_charges_male2 <- sd_charges_male2 / sqrt(n_male2)
round(se_charges_male2, 2)

# Sample total charge for females
female_data2 <- strat_samp2 %>%
  filter(sex == "female")
total_charges_female2 <- sum(female_data2$charges)
round(total_charges_female2, 2)

#Standard error for the total charge for female
n_female2 <- nrow(female_data2)
sd_charges_female2 <- sd(female_data2$charges)
se_charges_female2 <- sd_charges_female2 / sqrt(n_female2)
round(se_charges_female2, 2)

#Proportion values

# sample proportion for sex
str_prop_sex2=svymean(~sex,strat_design2)
str_prop_sex2

# sample proportion for smoker
str_prop_smoker2=svymean(~smoker,strat_design2)
str_prop_smoker2

detach(strat_samp2)

#Regression Estimation
plot(strat_samp2$age,strat_samp2$charges)
regression_model2 <- lm(charges ~ age, data = as.data.frame(strat_samp2))
regression_model2

attach(Dataset)

mean_charges = 1388.0 + (294.5*mean(age))
mean_charges

detach(Dataset)

###Comparision
Comp1 = data.frame("Population" = round(c(pop_mean_age,pop_mean_bmi,pop_mean_charges,pop_total_age,pop_total_bmi,pop_total_charges
),digits = 3),
"Sample 1" = round(c(str_sample_mean_age,str_sample_mean_bmi,str_sample_mean_charges,str_total_age,str_total_bmi,str_total_charges
),digits = 3),
"Sample 2" = round(c(str_sample_mean_age2,str_sample_mean_bmi2,str_sample_mean_charges2,str_total_age2,str_total_bmi2,str_total_charges2
),digits = 3))
rownames(Comp1) = c("Mean of age","Mean of bmi","Mean of charges",
                    "Total age","Total bmi","Total charges")
Comp1

#                   Population     Sample.1     Sample.2
#Mean of age           39.207       39.393       39.802
#Mean of bmi           30.663       30.877       30.626
#Mean of charges    13270.422    13295.548    13108.893
#Total age          52459.000    52762.630    53310.630
#Total bmi          41027.625    41356.304    41019.443
#Total charges   17755824.991 17807790.689 17557788.791

Comp2 = data.frame("Population" = round(c(sex_proportion),digits = 3),
                   "Sample 1" = round(c(str_prop_sex), digits = 3),
                   "Sample 2" = round(c(str_prop_sex2), digits = 3))

rownames(Comp2)=c("Female","Male")
Comp2

#         Population Sample.1 Sample.2
#Female      0.495    0.497    0.513
#Male        0.505    0.503    0.487

###Graphical Analysis

#Bar Chart for sex of sample 1
barplot(str_prop_sex,
        main = "Proportions of Gender in sample 1(Stratified)",
        names.arg = c("Female", "Male"),
        ylab = "Proportion")


#Bar Chart for sex of sample2
barplot(str_prop_sex2,
        main = "Proportions of Gender in sample 2(Stratified)",
        names.arg = c("Female", "Male"),
        ylab = "Proportion")

#Bar Chart for smoker of sample1
barplot(str_prop_smoker,
        main = "Proportions of Smoker in sample 1(Stratified)",
        names.arg = c("Non-Smoker","Smoker"),
        ylab = "Proportion")


#Bar Chart for smoker of sample2
barplot(str_prop_smoker2,
        main = "Proportions of Smoker in sample 2(Stratified)",
        names.arg = c("Non-Smoker","Smoker"),
        ylab = "Proportion")

#Histogram for charges of sample 1
hist(x = strat_samp1$charges,prob=T,main="Histogram of Charges in sample 1(Stratified)",xlab="Charges")

#Histogram for charges of sample 2
hist(x = strat_samp2$charges,prob=T,main="Histogram of Charges in sample 2(Stratified)",xlab="Charges")

#Box plot for charges with sex of sample 1
svyboxplot(~charges~sex, strat_design1, main="Boxplot of Charges with 
Sex in sample 1(Stratified)", col="Cyan" )

#Box plot for charges with sex of sample 2
svyboxplot(~charges~sex, strat_design2, main="Boxplot of Charges with 
Sex in sample 2(Stratified)", col="Cyan" )

#Box plot for charges with smoker of sample 1
svyboxplot(~charges~smoker, strat_design1, main="Boxplot of Charges with 
Sex in sample 1(Stratified)", col="Cyan" )

#Box plot for charges with smoker of sample 2
svyboxplot(~charges~smoker, strat_design2, main="Boxplot of Charges with 
Sex in sample 2(Stratified)", col="Cyan" )

#Scatter plot between charges and age of sample 1
plot(strat_samp1$charges,strat_samp1$age,
     main="Scatterplot of charges and age in sample 1(Stratified)",
     xlab="Charges",ylab="Age")

#Scatter plot between charges and age of sample 2
plot(strat_samp2$charges,strat_samp2$age,
     main="Scatterplot of charges and age in sample 2(Stratified)",
     xlab="Charges",ylab="Age")

#Scatter plot between charges and bmi of sample 1
plot(strat_samp1$charges,strat_samp1$bmi,
     main="Scatterplot of charges and bmi in sample 1(Stratified)",
     xlab="Charges",ylab="BMI")

#Scatter plot between charges and bmi of sample 2
plot(strat_samp2$charges,strat_samp2$bmi,
     main="Scatterplot of charges and bmi in sample 2(Stratified)",
     xlab="Charges",ylab="BMI")





