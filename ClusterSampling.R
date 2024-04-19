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

###For population

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

#Cluster variable = age
# Define age groups
age_breaks <- c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 64)
age_labels <- paste("Group", 1:10)
Dataset$AgeGroup <- cut(Dataset$age, breaks = age_breaks, labels = age_labels, right = TRUE)

# Display age groups with age ranges
age_group_ranges <- paste(age_labels, "Ages", age_breaks, "-", c(tail(age_breaks, n = -1), "64"))
age_group_ranges

#"Group 1 => Ages 15 - 20" , "Group 2 => Ages 20 - 25" , "Group 3  =>Ages 25 - 30" , "Group 4 => Ages 30 - 35" , 
#"Group 5 => Ages 35 - 40" , "Group 6 => Ages 40 - 45" , "Group 7 => Ages 45 - 50" , "Group 8 => Ages 50 - 55" ,
#"Group 9 => Ages 55 - 60" , "Group 10 => Ages 60 - 64"

#Number of clusters in the population
N=length(unique(Dataset$AgeGroup))
N

#N = 10
#Rule of thumb is n = sqrt(M/2) ; M=population size / Number of clusters in the population(N)
n=round(sqrt((1338/10)/2))
n

#Selecting the First Cluster Sample
#Selecting the clusters using SRS
set.seed(1234)
clusters1 = sample(unique(Dataset$AgeGroup),size = n,replace = F)
clusters1

#Variable to save data after selecing clusters
Cluster1 = data.frame()

#variable to save sample sizes
m=numeric(n)

#Variable to save population size of clusters
ClusterSize = numeric(n)

e <- 3
ci <- 95 

for (i in 1:n){
  #Dividing the dataset into clusters
  dat <- subset(Dataset, AgeGroup == clusters1[i])
  ClusterSize[i] = nrow(dat)
  #Selecting sample sizes for each cluster
  m[i] = rsampcalc(N = nrow(dat),e = e,ci = ci)
  #selecting a sample from each cluster and saving it
  Cluster1=rbind(Cluster1,rsamp(df = dat,n = m[i],rep = F))
}

ClusterDetails <- data.frame(AgeGroup = clusters1, SampleSize = m, PopulationSize = ClusterSize)
ClusterDetails

#Calculating sample weights
pw <- numeric(0)

for (i in 1:n) {
  cluster_age_group <- ClusterDetails$AgeGroup[i]
  cluster_sample_size <- ClusterDetails$SampleSize[i]
  cluster_population_size <- ClusterDetails$PopulationSize[i]
  
  weight <- (N * cluster_population_size) / (n * cluster_sample_size)
  
  pw <- c(pw, rep(weight, cluster_sample_size))
}

# Add the sample weights to Cluster1
Cluster1$pw <- pw

# Create a survey design
Cluster_Design <- svydesign(ids = ~AgeGroup, weights = ~pw, data = Cluster1)

#sample mean for bmi
Cluster_mean_bmi = svymean(~bmi,design = Cluster_Design)
Cluster_mean_bmi

#sample mean for charges
Cluster_mean_charges = svymean(~charges,design = Cluster_Design)
Cluster_mean_charges

# Sample mean charges for smokers and non-smokers
Cluster_means_charges_smoker <- svyby(~charges, by = ~smoker, design = Cluster_Design, FUN = svymean)
Cluster_means_charges_smoker

# Sample mean charge for males and females
Cluster_means_charges_sex <- svyby(~charges, by = ~sex, design = Cluster_Design, FUN = svymean)
Cluster_means_charges_sex

#Total values

# sample total for bmi
Cluster_total_bmi=svytotal(~bmi,design = Cluster_Design)
Cluster_total_bmi

# sample total for charges
Cluster_total_charges=svytotal(~charges,design = Cluster_Design)
Cluster_total_charges

# Sample total charges for smokers and non-smokers
Cluster_total_charges_smoker <- svyby(~charges, by = ~smoker, design = Cluster_Design, FUN = svytotal)
Cluster_total_charges_smoker

# Sample mean charge for males and females
Cluster_total_charges_sex <- svyby(~charges, by = ~sex, design = Cluster_Design, FUN = svytotal)
Cluster_total_charges_sex

#Proportion values

# sample proportion for sex
Cluster_prop_sex=svymean(~sex,design = Cluster_Design)
Cluster_prop_sex

# sample proportion for smoker
Cluster_prop_smoker=svymean(~smoker,design = Cluster_Design)
Cluster_prop_smoker

# sample proportion for region
Cluster_prop_region=svymean(~region,design = Cluster_Design)
Cluster_prop_region

#Regression Estimation
plot(Cluster1$bmi,Cluster1$charges)
regression_model1 <- lm(charges ~ bmi, data = Cluster1)
regression_model1

attach(Dataset)

mean_charges = 399.4 + (419.4*mean(bmi))
mean_charges

detach(Dataset)

#Ratio Estimation
r=svyratio(~ charges, ~ bmi,Cluster_Design)
r
predict(r,mean(Dataset$bmi))


#Selecting the Second Cluster Sample
#Selecting the clusters using SRS
set.seed(879)
clusters2 = sample(unique(Dataset$AgeGroup),size = n,replace = F)
clusters2

#Variable to save data after selecing clusters
Cluster2 = data.frame()

#variable to save sample sizes
m2=numeric(n)

#Variable to save population size of clusters
ClusterSize2 = numeric(n)

e <- 3
ci <- 95 

for (i in 1:n){
  #Dividing the dataset into clusters
  dat <- subset(Dataset, AgeGroup == clusters2[i])
  ClusterSize2[i] = nrow(dat)
  #Selecting sample sizes for each cluster
  m2[i] = rsampcalc(N = nrow(dat),e = e,ci = ci)
  #selecting a sample from each cluster and saving it
  Cluster2=rbind(Cluster2,rsamp(df = dat,n = m2[i],rep = F))
}

ClusterDetails2 <- data.frame(AgeGroup = clusters2, SampleSize = m2, PopulationSize = ClusterSize2)
ClusterDetails2

#Calculating sample weights
pw <- numeric(0)

for (i in 1:n) {
  cluster_age_group2 <- ClusterDetails2$AgeGroup[i]
  cluster_sample_size2 <- ClusterDetails2$SampleSize[i]
  cluster_population_size2 <- ClusterDetails2$PopulationSize[i]
  
  weight <- (N * cluster_population_size2) / (n * cluster_sample_size2)
  
  pw <- c(pw, rep(weight, cluster_sample_size2))
}

# Add the sample weights to Cluster2
Cluster2$pw <- pw

# Create a survey design
Cluster_Design2 <- svydesign(ids = ~AgeGroup, weights = ~pw, data = Cluster2)

#sample mean for bmi
Cluster_mean_bmi2 = svymean(~bmi,design = Cluster_Design2)
Cluster_mean_bmi2

#sample mean for charges
Cluster_mean_charges2 = svymean(~charges,design = Cluster_Design2)
Cluster_mean_charges2

# Sample mean charges for smokers and non-smokers
Cluster_means_charges_smoker2 <- svyby(~charges, by = ~smoker, design = Cluster_Design2, FUN = svymean)
Cluster_means_charges_smoker2

# Sample mean charge for males and females
Cluster_means_charges_sex2 <- svyby(~charges, by = ~sex, design = Cluster_Design2, FUN = svymean)
Cluster_means_charges_sex2

#Total values

# sample total for bmi
Cluster_total_bmi2=svytotal(~bmi,design = Cluster_Design2)
Cluster_total_bmi2

# sample total for charges
Cluster_total_charges2=svytotal(~charges,design = Cluster_Design2)
Cluster_total_charges2

# Sample total charges for smokers and non-smokers
Cluster_total_charges_smoker2 <- svyby(~charges, by = ~smoker, design = Cluster_Design2, FUN = svytotal)
Cluster_total_charges_smoker2

# Sample mean charge for males and females
Cluster_total_charges_sex2 <- svyby(~charges, by = ~sex, design = Cluster_Design2, FUN = svytotal)
Cluster_total_charges_sex2

#Proportion values

# sample proportion for sex
Cluster_prop_sex2=svymean(~sex,design = Cluster_Design2)
Cluster_prop_sex2

# sample proportion for smoker
Cluster_prop_smoker2=svymean(~smoker,design = Cluster_Design2)
Cluster_prop_smoker2

# sample proportion for region
Cluster_prop_region2=svymean(~region,design = Cluster_Design2)
Cluster_prop_region2

#Regression Estimation
plot(Cluster2$bmi,Cluster2$charges)
regression_model2 <- lm(charges ~ bmi, data = Cluster2)
regression_model2

attach(Dataset)

mean_charges = 532.4 + (391.8*mean(bmi))
mean_charges

detach(Dataset)

#Ratio Estimation
r=svyratio(~ charges, ~ bmi,Cluster_Design2)
r
predict(r,mean(Dataset$bmi))

###Comparision
Comp1 = data.frame("Population" = round(c(pop_mean_bmi,pop_mean_charges,pop_total_bmi,pop_total_charges
),digits = 3),
"Sample 1" = round(c(Cluster_mean_bmi,Cluster_mean_charges,Cluster_total_bmi,Cluster_total_charges
),digits = 3),
"Sample 2" = round(c(Cluster_mean_bmi2,Cluster_mean_charges2,Cluster_total_bmi2,Cluster_total_charges2
),digits = 3))
rownames(Comp1) = c("Mean of bmi","Mean of charges",
                    "Total bmi","Total charges")
Comp1

#                  Population     Sample.1     Sample.2
#Mean of bmi           30.663       30.528       30.554
#Mean of charges    13270.422    13162.113    12461.690
#Total bmi          41027.625    40754.300    40521.721
#Total charges   17755824.991 17571420.486 16527316.795

Comp2 = data.frame("Population" = round(c(sex_proportion),digits = 3),
                   "Sample 1" = round(c(Cluster_prop_sex), digits = 3),
                   "Sample 2" = round(c(Cluster_prop_sex2), digits = 3))

rownames(Comp2)=c("Female","Male")
Comp2

#         Population Sample.1 Sample.2
#Female       0.495     0.49    0.487
#Male         0.505     0.51    0.513

###Graphical Analysis

#Bar Chart for sex of sample1
barplot(Cluster_prop_sex,
        main = "Proportions of Gender in sample 1(Cluster)",
        names.arg = c("Female", "Male"),
        ylab = "Proportion")


#Bar Chart for sex of sample2
barplot(Cluster_prop_sex2,
        main = "Proportions of Gender in sample 2(Cluster)",
        names.arg = c("Female", "Male"),
        ylab = "Proportion")

#Bar Chart for smoker of sample1
barplot(Cluster_prop_smoker,
        main = "Proportions of Smoker in sample 1(Cluster)",
        names.arg = c("Non-Smoker","Smoker"),
        ylab = "Proportion")

#Bar Chart for smoker of sample2
barplot(Cluster_prop_smoker2,
        main = "Proportions of Smoker in sample 2(Cluster)",
        names.arg = c("Non-Smoker","Smoker"),
        ylab = "Proportion")


#Bar Chart for region of sample1
barplot(Cluster_prop_region,
        main = "Proportions of Region in sample 1(Cluster)",
        names.arg = c("Northeast","Northwest","Southeast","Southwest"),
        ylab = "Proportion")

#Bar Chart for region of sample2
barplot(Cluster_prop_region2,
        main = "Proportions of Region in sample 2(Cluster)",
        names.arg = c("Northeast","Northwest","Southeast","Southwest"),
        ylab = "Proportion")

#Histogram for charges of sample 1
hist(x = Cluster1$charges,prob=T,main="Histogram of Charges in sample 1(Cluster)",xlab="Charges")

#Histogram for charges of sample 2
hist(x = Cluster2$charges,prob=T,main="Histogram of Charges in sample 2(Cluster)",xlab="Charges")

#Box plot for charges with sex of sample 1
svyboxplot(~charges~sex, Cluster_Design, main="Boxplot of Charges with 
Sex in sample 1(Cluster)", col="Cyan" )

#Box plot for charges with sex of sample 2
svyboxplot(~charges~sex, Cluster_Design2, main="Boxplot of Charges with 
Sex in sample 2(Cluster)", col="Cyan" )

#Box plot for charges with smoker of sample 1
svyboxplot(~charges~smoker, Cluster_Design, main="Boxplot of Charges with 
Sex in sample 1(Cluster)", col="Cyan" )

#Box plot for charges with smoker of sample 2
svyboxplot(~charges~smoker, Cluster_Design2, main="Boxplot of Charges with 
Sex in sample 2(Cluster)", col="Cyan" )

#Box plot for charges with region of sample 1
svyboxplot(~charges~region, Cluster_Design, main="Boxplot of Charges with 
Sex in sample 1(Cluster)", col="Cyan" )

#Box plot for charges with region of sample 2
svyboxplot(~charges~region, Cluster_Design2, main="Boxplot of Charges with 
Sex in sample 2(Cluster)", col="Cyan" )

#Scatter plot between charges and bmi of sample 1
plot(Cluster1$charges,Cluster1$bmi,
     main="Scatterplot of charges and bmi in sample 1(Cluster)",
     xlab="Charges",ylab="BMI")

#Scatter plot between charges and bmi of sample 2
plot(Cluster2$charges,Cluster2$bmi,
     main="Scatterplot of charges and bmi in sample 2(Cluster)",
     xlab="Charges",ylab="BMI")






