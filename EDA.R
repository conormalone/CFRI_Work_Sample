library(readxl)
library(tidyverse)
data <- read_excel("Research Analyst dataset.xlsx", sheet = "Data")
#clean up variables
colnames(data) <- c("Patient_ID", "Date_of_Birth", "Gender", "Allele_1_Mutation", "Allele_2_Mutation", "Date_of_Encounter", "FEV1pp", "BMI_z")
#factors
data$Patient_ID <- as.factor(data$Patient_ID)
data$Gender <- as.factor(data$Gender)
data$Allele_1_Mutation <- as.factor(data$Allele_1_Mutation)
data$Allele_2_Mutation <- as.factor(data$Allele_2_Mutation)

#create Genotype variable
If both Alleles are [delta]F508; c.1521_1523delCTT; p.Phe508del, then patient genotype is "Homozygous F508del"
If one Allele is [delta]F508; c.1521_1523delCTT; p.Phe508del, then patient genotype is "Heterozygous F508del"
If neither Allele is [delta]F508; c.1521_1523delCTT; p.Phe508del, then patient is "Other Genotype"

data <- data %>% mutate(Genotype = as.factor(case_when(
  Allele_1_Mutation == "[delta]F508; c.1521_1523delCTT; p.Phe508del" & Allele_2_Mutation == "[delta]F508; c.1521_1523delCTT; p.Phe508del"~ "Homozygous F508del",
  Allele_1_Mutation == "[delta]F508; c.1521_1523delCTT; p.Phe508del"| Allele_2_Mutation == "[delta]F508; c.1521_1523delCTT; p.Phe508del"  ~ "Heterozygous F508del",
  TRUE ~ "Other Genotype"))) 










#numeric
data$FEV1pp <-as.numeric(data$FEV1pp)
data$BMI_z <- as.numeric(data$BMI_z)
#dates
library(lubridate)
data$Date_of_Birth <- ymd(data$Date_of_Birth)
data$Date_of_Encounter <- ymd(data$Date_of_Encounter)
data$Age_at_Encounter <- as.integer(floor((data$Date_of_Encounter- data$Date_of_Birth)/365))
data$Age <- as.integer(floor((ymd("2020-12-31")- data$Date_of_Birth)/365))
#Question 1
library(tableone)
CreateTableOne(vars = c("Age", "Genotype"), strata = "Gender", data=data)


#question 2 summarise

summary(data$FEV1pp)
FEV <- data[complete.cases(data$FEV1pp), ] 
plot(density(FEV$FEV1pp))
ggplot(aes(Age,FEV1pp,group = Age), data = FEV)+geom_boxplot()

BMI <- data[complete.cases(data$BMI_z), ] 

plot(density(BMI$BMI_z))
ggplot(aes(Age,BMI_z, group = Age),data = BMI)+geom_boxplot()



#question 3 lung function over time genotype
plot(data$Age_at_Encounter, data$FEV1pp)
ggplot(aes(Age_at_Encounter, FEV1pp, group = Age_at_Encounter), data = data)+geom_boxplot() +facet_wrap(~ Genotype) +geom_smooth(method="lm") 
