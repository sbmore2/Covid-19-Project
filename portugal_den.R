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

europe = subset(owid,location == c("Denmark", "Portugal"))



euro_omi <- europe %>% filter(date > "2021-12-13")
summary(euro_omi)

keep_euro <- c("location", "date","new_cases_smoothed","new_deaths_smoothed","new_deaths_per_million","new_cases_per_million",
          "reproduction_rate","icu_patients","hosp_patients",                                   "people_fully_vaccinated_per_hundred"      ,  "total_boosters_per_hundred"      ,          
          "new_vaccinations_smoothed_per_million"  ,    "new_people_vaccinated_smoothed"    ,         "new_people_vaccinated_smoothed_per_hundred", "stringency_index", "population"   ,                             
          "population_density"                         ,                  "aged_65_older"              ,               
          "aged_70_older"             ,             "cardiovasc_death_rate"          ,           
          "diabetes_prevalence" , "hospital_beds_per_thousand" )

df_euro = subset(euro_omi, select= keep_euro)

summary(df_euro)

#remove all na
df_euro = df_euro <- df_euro[complete.cases(df_euro), ]
summary(df_euro)
df_euro$location <- as.factor(df_euro$location)


fit_1 <- lmer(new_deaths_per_million ~   hosp_patients + stringency_index  + (1|reproduction_rate) + (1|location)  + (1|population_density)  ,df_euro)
anova(fit_1)
summary(fit_1)

par(mfrow = c(2, 2))
plot(fit_1)
qqnorm(resid(fit_1)) 
qqline(resid(fit_1))

fit_2 <- lm(new_deaths_per_million ~   hosp_patients + stringency_index    + population_density + new_cases_per_million  ,df_euro)
anova(fit_2)
summary(fit_2)

par(mfrow = c(2, 2))
plot(fit_2)
qqnorm(resid(fit_1)) 
qqline(resid(fit_1))

#correlation
heat <- cor(df_euro[sapply(df_euro,is.numeric)])
heat1 <- melt(heat)
pp <- ggplot(heat1, aes(x = Var1,
                        y = Var2,
                        fill = value))+ geom_tile() + theme(axis.text.x  = element_text(angle=30, vjust=0.5, size=7)) + labs(y="", x="", title="") 
ggplotly(pp)
