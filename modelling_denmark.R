library(tidyverse)
library(gridExtra)
library(scales)
library(lubridate)
library(ggplot2)
library(zoo)
library(lme4)
library(lmerTest) 
library(MASS)


owid <- read_csv('data/owid-covid-data.csv')

denmark = subset(owid,location == "Denmark")
df = denmark[, colSums(is.na(denmark)) != nrow(denmark)]

keep <- c("date","new_cases","new_cases_smoothed","new_deaths","new_deaths_smoothed",
          "reproduction_rate","icu_patients","hosp_patients",                             
          "new_tests"                       ,      "total_tests_per_thousand"       ,           
          "new_tests_per_thousand"    ,                 "new_tests_smoothed"        ,                 "new_tests_smoothed_per_thousand"      ,      "positive_rate"              ,               
          "tests_per_case"      ,                       "tests_units"                  ,              "total_vaccinations"             ,            "people_vaccinated"      ,                   
          "people_fully_vaccinated"       ,             "total_boosters"               ,              "new_vaccinations"     ,                      "new_vaccinations_smoothed"     ,            
          "total_vaccinations_per_hundred"       ,      "people_vaccinated_per_hundred"       ,       "people_fully_vaccinated_per_hundred"      ,  "total_boosters_per_hundred"      ,          
          "new_vaccinations_smoothed_per_million"  ,    "new_people_vaccinated_smoothed"    ,         "new_people_vaccinated_smoothed_per_hundred", "stringency_index", "population"   ,                             
          "population_density"                         ,                  "aged_65_older"              ,               
          "aged_70_older"             ,             "cardiovasc_death_rate"          ,           
          "diabetes_prevalence" , "hospital_beds_per_thousand" )

keep_1 <- c('new_deaths_smoothed', 'new_cases_smoothed', 'hosp_patients', ' stringency_index' , 'new_people_vaccinated_smoothed_per_hundred', 'reproduction_rate', 'date', 'icu_patients', 'hospital_beds_per_thousand')

df = subset(df, select= keep)

omicron_phase <- df %>% filter(date > "2021-11-01")
summary(df)

fit1 <- lm(new_deaths_smoothed ~ new_cases_smoothed+hosp_patients + stringency_index  + new_people_vaccinated_smoothed_per_hundred + reproduction_rate , omicron_phase)
anova(fit1)
summary(fit1)

fit2 <- lm(hosp_patients ~  aged_65_older + new_people_vaccinated_smoothed_per_hundred + reproduction_rate + diabetes_prevalence + cardiovasc_death_rate, df)
anova(fit2)
summary(fit2)

par(mfrow = c(2, 2))
plot(fit1)

b <- boxcox(fit1)
lambda <- b$x[which.max(b$y)]

fit3 <- lmer((new_deaths_smoothed)^0.8 ~ new_cases_smoothed+hosp_patients + stringency_index + (1|aged_65_older) + new_people_vaccinated_smoothed_per_hundred + reproduction_rate + diabetes_prevalence + cardiovasc_death_rate , omicron_phase)
anova(fit3)
summary(fit3)

pairs(omicron_phase[,c(3,4,5,7)])

p<-ggplot(data=denmark, aes(x=date, y=hosp_patients)) +
  geom_bar(stat="identity")
p

p<-ggplot(data=denmark, aes(x=date, y=stringency_index)) +
  geom_bar(stat="identity")
p
