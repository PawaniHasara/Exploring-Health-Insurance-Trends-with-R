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


##sample size for SRS
srs_size=rsampcalc(nrow(Dataset),e=3,ci=95) 
srs_size

###Sample 1
set.seed(123)
srs_sample1=rsamp(Dataset,n=srs_size,rep =FALSE)
srs_sample1

attach(srs_sample1)
srs_sample_design1=svydesign(id=~1,strata=NULL,data =srs_sample1)

#Mean values

# sample mean for Age
srs_sample_mean_age=svymean(~age,srs_sample_design1)
srs_sample_mean_age

# sample mean for bmi
srs_sample_mean_bmi=svymean(~bmi,srs_sample_design1)
srs_sample_mean_bmi

# sample mean for charges
srs_sample_mean_charges=svymean(~charges,srs_sample_design1)
srs_sample_mean_charges

# Sample mean charge for smokers
smokers_data <- srs_sample1 %>%
  filter(smoker == "yes")
mean_charges_smokers <- mean(smokers_data$charges)
round(mean_charges_smokers, 2)

#Standard error for the mean charge for smokers
n_smokers <- nrow(smokers_data)
sd_charges_smokers <- sd(smokers_data$charges)
se_charges_smokers <- sd_charges_smokers / sqrt(n_smokers)
round(se_charges_smokers, 2)

# Sample mean charge for non smokers
non_smokers_data <- srs_sample1 %>%
  filter(smoker == "no")
mean_charges_non_smokers <- mean(non_smokers_data$charges)
round(mean_charges_non_smokers, 2)

#Standard error for the mean charge for non smokers
n_non_smokers <- nrow(non_smokers_data)
sd_charges_non_smokers <- sd(non_smokers_data$charges)
se_charges_non_smokers <- sd_charges_non_smokers / sqrt(n_non_smokers)
round(se_charges_non_smokers, 2)

# Sample mean charge for males
male_data <- srs_sample1 %>%
  filter(sex == "male")
mean_charges_male <- mean(male_data$charges)
round(mean_charges_male, 2)

#Standard error for the mean charge for male
n_male <- nrow(male_data)
sd_charges_male <- sd(male_data$charges)
se_charges_male <- sd_charges_male / sqrt(n_male)
round(se_charges_male, 2)

# Sample mean charge for females
female_data <- srs_sample1 %>%
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
srs_total_age=svytotal(~age,srs_sample_design1)
srs_total_age

# sample total for bmi
srs_total_bmi=svytotal(~bmi,srs_sample_design1)
srs_total_bmi

# sample total for charges
srs_total_charges=svytotal(~charges,srs_sample_design1)
srs_total_charges

# Sample total charge for smokers
smokers_data <- srs_sample1 %>%
  filter(smoker == "yes")
total_charges_smokers <- sum(smokers_data$charges)
round(total_charges_smokers, 2)

#Standard error for the total charge for smokers
n_smokers <- nrow(smokers_data)
sd_charges_smokers <- sd(smokers_data$charges)
se_charges_smokers <- sd_charges_smokers / sqrt(n_smokers)
round(se_charges_smokers, 2)

# Sample total charge for non smokers
non_smokers_data <- srs_sample1 %>%
  filter(smoker == "no")
total_charges_non_smokers <- sum(non_smokers_data$charges)
round(total_charges_non_smokers, 2)

#Standard error for the total charge for non smokers
n_non_smokers <- nrow(non_smokers_data)
sd_charges_non_smokers <- sd(non_smokers_data$charges)
se_charges_non_smokers <- sd_charges_non_smokers / sqrt(n_non_smokers)
round(se_charges_non_smokers, 2)

# Sample total charge for males
male_data <- srs_sample1 %>%
  filter(sex == "male")
total_charges_male <- sum(male_data$charges)
round(total_charges_male, 2)

#Standard error for the total charge for male
n_male <- nrow(male_data)
sd_charges_male <- sd(male_data$charges)
se_charges_male <- sd_charges_male / sqrt(n_male)
round(se_charges_male, 2)

# Sample total charge for females
female_data <- srs_sample1 %>%
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
srs_prop_sex=svymean(~sex,srs_sample_design1)
srs_prop_sex

# sample proportion for smoker
srs_prop_smoker=svymean(~smoker,srs_sample_design1)
srs_prop_smoker

# sample proportion for region
srs_prop_region=svymean(~region,srs_sample_design1)
srs_prop_region

detach(srs_sample1)

#Regression Estimation
plot(srs_sample1$age,srs_sample1$charges)
regression_model1 <- lm(charges ~ age, data = as.data.frame(srs_sample1))
regression_model1

attach(Dataset)

mean_charges = 6048.8 + (190.1*mean(age))
mean_charges

detach(Dataset)


###Sample 2
set.seed(321)
srs_sample2=rsamp(Dataset,n=srs_size,rep =FALSE)
srs_sample2

attach(srs_sample2)
srs_sample_design2=svydesign(id=~1,strata=NULL,data =srs_sample2)

#Mean values

# sample mean for Age
srs_sample_mean_age2=svymean(~age,srs_sample_design2)
srs_sample_mean_age2

# sample mean for bmi
srs_sample_mean_bmi2=svymean(~bmi,srs_sample_design2)
srs_sample_mean_bmi2

# sample mean for charges
srs_sample_mean_charges2=svymean(~charges,srs_sample_design2)
srs_sample_mean_charges2

# Sample mean charge for smokers
smokers_data2 <- srs_sample2 %>%
  filter(smoker == "yes")
mean_charges_smokers2 <- mean(smokers_data2$charges)
round(mean_charges_smokers2, 2)

#Standard error for the mean charge for smokers
n_smokers2 <- nrow(smokers_data2)
sd_charges_smokers2 <- sd(smokers_data2$charges)
se_charges_smokers2 <- sd_charges_smokers2 / sqrt(n_smokers2)
round(se_charges_smokers2, 2)

# Sample mean charge for non smokers
non_smokers_data2 <- srs_sample2 %>%
  filter(smoker == "no")
mean_charges_non_smokers2 <- mean(non_smokers_data2$charges)
round(mean_charges_non_smokers2, 2)

#Standard error for the mean charge for non smokers
n_non_smokers2 <- nrow(non_smokers_data2)
sd_charges_non_smokers2 <- sd(non_smokers_data2$charges)
se_charges_non_smokers2 <- sd_charges_non_smokers2 / sqrt(n_non_smokers2)
round(se_charges_non_smokers2, 2)

# Sample mean charge for males
male_data2 <- srs_sample2 %>%
  filter(sex == "male")
mean_charges_male2 <- mean(male_data2$charges)
round(mean_charges_male2, 2)

#Standard error for the mean charge for male
n_male2 <- nrow(male_data2)
sd_charges_male2 <- sd(male_data2$charges)
se_charges_male2 <- sd_charges_male2 / sqrt(n_male2)
round(se_charges_male2, 2)

# Sample mean charge for females
female_data2 <- srs_sample2 %>%
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
srs_total_age2=svytotal(~age,srs_sample_design2)
srs_total_age2

# sample total for bmi
srs_total_bmi2=svytotal(~bmi,srs_sample_design2)
srs_total_bmi2

# sample total for charges
srs_total_charges2=svytotal(~charges,srs_sample_design2)
srs_total_charges2

# Sample total charge for smokers
smokers_data2 <- srs_sample2 %>%
  filter(smoker == "yes")
total_charges_smokers2 <- sum(smokers_data2$charges)
round(total_charges_smokers2, 2)

#Standard error for the total charge for smokers
n_smokers2 <- nrow(smokers_data2)
sd_charges_smokers2 <- sd(smokers_data2$charges)
se_charges_smokers2 <- sd_charges_smokers2 / sqrt(n_smokers2)
round(se_charges_smokers2, 2)

# Sample total charge for non smokers
non_smokers_data2 <- srs_sample2 %>%
  filter(smoker == "no")
total_charges_non_smokers2 <- sum(non_smokers_data2$charges)
round(total_charges_non_smokers2, 2)

#Standard error for the total charge for non smokers
n_non_smokers2 <- nrow(non_smokers_data2)
sd_charges_non_smokers2 <- sd(non_smokers_data2$charges)
se_charges_non_smokers2 <- sd_charges_non_smokers2 / sqrt(n_non_smokers2)
round(se_charges_non_smokers2, 2)

# Sample total charge for males
male_data2 <- srs_sample2 %>%
  filter(sex == "male")
total_charges_male2 <- sum(male_data2$charges)
round(total_charges_male2, 2)

#Standard error for the total charge for male
n_male2 <- nrow(male_data2)
sd_charges_male2 <- sd(male_data2$charges)
se_charges_male2 <- sd_charges_male2 / sqrt(n_male2)
round(se_charges_male2, 2)

# Sample total charge for females
female_data2 <- srs_sample2 %>%
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
srs_prop_sex2=svymean(~sex,srs_sample_design2)
srs_prop_sex2

# sample proportion for smoker
srs_prop_smoker2=svymean(~smoker,srs_sample_design2)
srs_prop_smoker2

# sample proportion for region
srs_prop_region2=svymean(~region,srs_sample_design2)
srs_prop_region2

detach(srs_sample2)

##Regression Estimation
plot(srs_sample2$age,srs_sample2$charges)
regression_model2 <- lm(charges ~ age, data = as.data.frame(srs_sample2))
regression_model2

attach(Dataset)

mean_charges2 = 1439.4 + (303.2*mean(age))
mean_charges2

detach(Dataset)

###Comparision
Comp1 = data.frame("Population" = round(c(pop_mean_age,pop_mean_bmi,pop_mean_charges,pop_total_age,pop_total_bmi,pop_total_charges
),digits = 3),
                  "Sample 1" = round(c(srs_sample_mean_age,srs_sample_mean_bmi,srs_sample_mean_charges,srs_total_age,srs_total_bmi,srs_total_charges
),digits = 3),
                  "Sample 2" = round(c(srs_sample_mean_age2,srs_sample_mean_bmi2,srs_sample_mean_charges2,srs_total_age2,srs_total_bmi2,srs_total_charges2
),digits = 3))
rownames(Comp1) = c("Mean of age","Mean of bmi","Mean of charges",
                   "Total age","Total bmi","Total charges")
Comp1

#                   Population    Sample.1    Sample.2
#Mean of age           39.207      39.244      39.572
#Mean of bmi           30.663      30.915      30.593
#Mean of charges    13270.422   13508.835   13437.963
#Total age          52459.000   23311.000   23506.000
#Total bmi          41027.625   18363.590   18172.385
#Total charges   17755824.991 8024247.928 7982149.830

Comp2 = data.frame("Population" = round(c(sex_proportion),digits = 3),
                   "Sample 1" = round(c(srs_prop_sex), digits = 3),
                   "Sample 2" = round(c(srs_prop_sex2), digits = 3))

rownames(Comp2)=c("Female","Male")
Comp2

#         Population Sample.1 Sample.2
#Female       0.495     0.48    0.475
#Male         0.505     0.52    0.525

###Graphical Analysis

#Bar Chart for sex of population
barplot(sex_proportion,main="Proportions of Gender in population",
        names.arg =c("Female","Male"),
        ylab = "Proportion" )


#Bar Chart for sex of sample1
barplot(srs_prop_sex,
        main = "Proportions of Gender in sample 1",
        names.arg = c("Female", "Male"),
        ylab = "Proportion")


#Bar Chart for sex of sample2
barplot(srs_prop_sex2,
        main = "Proportions of Gender in sample 2",
        names.arg = c("Female", "Male"),
        ylab = "Proportion")

#Bar Chart for smoker of population
barplot(smoker_proportion,main="Proportions of Smoker in population",
        names.arg =c("Non-Smoker","Smoker"),
        ylab = "Proportion" )


#Bar Chart for smoker of sample1
barplot(srs_prop_smoker,
        main = "Proportions of Smoker in sample 1",
        names.arg = c("Non-Smoker","Smoker"),
        ylab = "Proportion")


#Bar Chart for smoker of sample2
barplot(srs_prop_smoker2,
        main = "Proportions of Smoker in sample 2",
        names.arg = c("Non-Smoker","Smoker"),
        ylab = "Proportion")

#Bar Chart for region of population
barplot(region_proportion,main="Proportions of Region in population",
        names.arg =c("Northeast","Northwest","Southeast","Southwest"),
        ylab = "Proportion" )


#Bar Chart for region of sample1
barplot(srs_prop_region,
        main = "Proportions of Region in sample 1",
        names.arg = c("Northeast","Northwest","Southeast","Southwest"),
        ylab = "Proportion")

#Bar Chart for region of sample2
barplot(srs_prop_region2,
        main = "Proportions of Region in sample 2",
        names.arg = c("Northeast","Northwest","Southeast","Southwest"),
        ylab = "Proportion")

#Histogram for charges of population
hist(x = Dataset$charges,prob=T,main="Histogram of Charges in population",xlab="Charges")

#Histogram for charges of sample 1
hist(x = srs_sample1$charges,prob=T,main="Histogram of Charges in sample 1",xlab="Charges")

#Histogram for charges of sample 2
hist(x = srs_sample2$charges,prob=T,main="Histogram of Charges in sample 2",xlab="Charges")

#Box plot for charges with sex of population
boxplot(charges ~ sex, data = Dataset, main = "Boxplot of Charges with 
Sex in population", col="Cyan" )

#Box plot for charges with sex of sample 1
svyboxplot(~charges~sex, srs_sample_design1, main="Boxplot of Charges with 
Sex in sample 1", col="Cyan" )

#Box plot for charges with sex of sample 2
svyboxplot(~charges~sex, srs_sample_design2, main="Boxplot of Charges with 
Sex in sample 2", col="Cyan" )

#Box plot for charges with smoker of population
boxplot(charges ~ smoker, data = Dataset, main = "Boxplot of Charges with 
Smoker in population", col="Cyan" )

#Box plot for charges with smoker of sample 1
svyboxplot(~charges~smoker, srs_sample_design1, main="Boxplot of Charges with 
Sex in sample 1", col="Cyan" )

#Box plot for charges with smoker of sample 2
svyboxplot(~charges~smoker, srs_sample_design2, main="Boxplot of Charges with 
Sex in sample 2", col="Cyan" )

#Box plot for charges with region of population
boxplot(charges ~ region, data = Dataset, main = "Boxplot of Charges with 
Smoker in population", col="Cyan" )

#Box plot for charges with region of sample 1
svyboxplot(~charges~region, srs_sample_design1, main="Boxplot of Charges with 
Sex in sample 1", col="Cyan" )

#Box plot for charges with region of sample 2
svyboxplot(~charges~region, srs_sample_design2, main="Boxplot of Charges with 
Sex in sample 2", col="Cyan" )

#Scatter plot between charges and age of population
plot(Dataset$charges,Dataset$age,
     main="Scatterplot of charges and age in population",
     xlab="Charges",ylab="Age")

#Scatter plot between charges and age of sample 1
plot(srs_sample1$charges,srs_sample1$age,
     main="Scatterplot of charges and age in sample 1",
     xlab="Charges",ylab="Age")

#Scatter plot between charges and age of sample 2
plot(srs_sample2$charges,srs_sample2$age,
     main="Scatterplot of charges and age in sample 2",
     xlab="Charges",ylab="Age")

#Scatter plot between charges and bmi of population
plot(Dataset$charges,Dataset$bmi,
     main="Scatterplot of charges and bmi in population",
     xlab="Charges",ylab="BMI")

#Scatter plot between charges and bmi of sample 1
plot(srs_sample1$charges,srs_sample1$bmi,
     main="Scatterplot of charges and bmi in sample 1",
     xlab="Charges",ylab="BMI")

#Scatter plot between charges and bmi of sample 2
plot(srs_sample2$charges,srs_sample2$bmi,
     main="Scatterplot of charges and bmi in sample 2",
     xlab="Charges",ylab="BMI")

