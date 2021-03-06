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
data$Age_at_Encounter_Cont <- (data$Date_of_Encounter- data$Date_of_Birth)/365
data$Age <- as.integer(floor((ymd("2020-12-31")- data$Date_of_Birth)/365))
#Question 1
library(tableone)
CreateTableOne(vars = c("Age", "Genotype"), strata = "Gender", data=data)


#question 2 summarise
library(papeR)
papeR::summarize(data)
summary(data$FEV1pp)
summary(FEV$FEV1pp)
FEV <- data[complete.cases(data$FEV1pp), ] 
plot(density(FEV$FEV1pp))
abline(v=mean(FEV$FEV1pp), col ="red")
ggplot(aes(FEV1pp), data = FEV)+geom_density()

BMI <- data[complete.cases(data$BMI_z), ] 

hist(BMI$BMI_z)

data %>% select(Patient_ID, Gender, Age, Genotype) %>% group_by(Patient_ID) %>% unique


library(viridis)
#question 3 lung function over time genotype
plot(data$Age_at_Encounter, data$FEV1pp)
ggplot(aes(Age_at_Encounter, FEV1pp, group = Age_at_Encounter), data = data)+geom_boxplot() +facet_wrap(~ Genotype) +geom_smooth(method="lm")
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(aes(Age_at_Encounter_Cont, FEV1pp, color = Genotype), data = data)+geom_point()+geom_smooth(method="lm")+xlim(c(4,30)) +
  scale_colour_viridis(discrete = TRUE)


table(data$Genotype, data$FEV1pp)
