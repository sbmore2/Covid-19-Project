library(tidyverse)
library(gridExtra)
library(scales)
library(lubridate)
library(ggplot2)
library(zoo)
library(lme4)
library(lmerTest) 
library(MASS)
library(reshape2)
library(plotly)

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

keep_1 <- c('new_deaths_smoothed', 'new_cases_smoothed', 'hosp_patients' , 'new_people_vaccinated_smoothed_per_hundred', 'reproduction_rate', 'date', 'icu_patients','stringency_index', 'hospital_beds_per_thousand', 'population_density')

df = subset(df, select= keep_1)

omicron_phase <- df %>% filter(date > "2021-12-13")
summary(omicron_phase)

df_complete = df_complete <- omicron_phase[complete.cases(omicron_phase), ]
summary(df_complete)

#convert stringency index to categories 

df_complete <- df_complete %>% 
  mutate(restriction = as.factor(case_when(stringency_index <= 20 ~ 0,(stringency_index > 20 & stringency_index <= 40) ~ 1, (stringency_index > 40 & stringency_index <= 60) ~ 2,(stringency_index > 60 & stringency_index <= 80) ~ 3,(stringency_index > 80 & stringency_index <= 100) ~ 4 )))

#month
df_complete <- df_complete %>% 
  mutate(month = as.factor(month(ymd(df_complete$date))))

#week 
df_complete <- df_complete %>% 
  mutate(week = as.factor(isoweek(ymd(df_complete$date))))


pairs(df_complete[,c(2,3,4,5,6)])
fit_3 <- lm(new_deaths_smoothed ~ new_cases_smoothed+hosp_patients + restriction  + new_people_vaccinated_smoothed_per_hundred + reproduction_rate + icu_patients, df_complete)
anova(fit_3)
summary(fit_3)

par(mfrow = c(2, 2))
plot(fit_3)

b <- boxcox(fit_3)
lambda <- b$x[which.max(b$y)]

#model with normal qq plot 

fit_4 <- lmer(new_deaths_smoothed ~  hosp_patients + restriction  + (1|month), df_complete)
anova(fit_4)
summary(fit_4)
par(mfrow = c(2, 2))
plot(fit_4)
qqnorm(resid(fit_4)) 
qqline(resid(fit_4))


fit_5 <- lm(new_deaths_smoothed ~  hosp_patients + restriction  + week + new_cases_smoothed, df_complete)
anova(fit_5)
summary(fit_5)
par(mfrow = c(2, 2))
plot(fit_5)
qqnorm(resid(fit_5)) 
qqline(resid(fit_5))



library("olsrr")
ols_test_breusch_pagan(fit_5, rhs = TRUE, multiple = TRUE)

b <- boxcox(fit_4)
lambda <- b$x[which.max(b$y)]

fit_6 <- lm(new_deaths_smoothed ~  hosp_patients + restriction  + month  + new_cases_smoothed, df_complete)
anova(fit_6)
summary(fit_6)
par(mfrow = c(2, 2))
plot(fit_6)
qqnorm(resid(fit_6)) 
qqline(resid(fit_6))

fit_7 <- lmer(new_deaths_smoothed ~  hosp_patients + restriction  + (1|reproduction_rate) + (1|week) + new_cases_smoothed, df_complete)
anova(fit_7)
summary(fit_7)
par(mfrow = c(2, 2))
plot(fit_7)
qqnorm(resid(fit_7)) 
qqline(resid(fit_7))

fit_8 <- lmer(new_deaths_smoothed ~  hosp_patients + restriction + (1|week), df_complete)
anova(fit_8)
summary(fit_8)
par(mfrow = c(2, 2))
plot(fit_8)
qqnorm(resid(fit_8)) 
qqline(resid(fit_8))

fit_9 <- lmer(new_deaths_smoothed ~  hosp_patients + restriction + (1|month) + (1|reproduction_rate), df_complete)
anova(fit_9)
summary(fit_9)
par(mfrow = c(2, 2))
plot(fit_9)
qqnorm(resid(fit_9)) 
qqline(resid(fit_9))

fit_10 <- lmer(new_deaths_smoothed ~  hosp_patients + restriction + (1|week) + (1|reproduction_rate), df_complete)
anova(fit_10)
summary(fit_10)
par(mfrow = c(2, 2))
plot(fit_10)
qqnorm(resid(fit_10)) 
qqline(resid(fit_10))
