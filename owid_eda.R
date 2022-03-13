library(tidyverse)
library(gridExtra)
library(scales)
library(lubridate)
library(ggplot2)
library(zoo)

owid <- read_csv('data/owid-covid-data.csv')
compare = subset(owid,location == c("Denmark", "United States","Portugal"))

denmark = subset(owid,location == "Denmark")

summary(denmark)
compare_df = compare[, colSums(is.na(compare)) != nrow(compare)]

keep <- c("date","new_cases","new_cases_smoothed","total_deaths","new_deaths","new_deaths_smoothed",
          "new_cases_smoothed_per_million","total_deaths_per_million",
          "new_deaths_smoothed_per_million", "reproduction_rate","icu_patients","hosp_patients",                             
          "hosp_patients_per_million", "weekly_icu_admissions" ,"weekly_icu_admissions_per_million","weekly_hosp_admissions" ,
          "weekly_hosp_admissions_per_million" ,        "new_tests"                       ,      "total_tests_per_thousand"       ,           
          "new_tests_per_thousand"    ,                 "new_tests_smoothed"        ,                 "new_tests_smoothed_per_thousand"      ,      "positive_rate"              ,               
          "tests_per_case"      ,                       "tests_units"                  ,              "total_vaccinations"             ,            "people_vaccinated"      ,                   
          "people_fully_vaccinated"       ,             "total_boosters"               ,              "new_vaccinations"     ,                      "new_vaccinations_smoothed"     ,            
          "total_vaccinations_per_hundred"       ,      "people_vaccinated_per_hundred"       ,       "people_fully_vaccinated_per_hundred"      ,  "total_boosters_per_hundred"      ,          
          "new_vaccinations_smoothed_per_million"  ,    "new_people_vaccinated_smoothed"    ,         "new_people_vaccinated_smoothed_per_hundred", "stringency_index", "population"   ,                             
          "population_density"              ,           "median_age"               ,                  "aged_65_older"              ,               
          "aged_70_older"             ,             "cardiovasc_death_rate"          ,           
          "diabetes_prevalence" , "hospital_beds_per_thousand" )
df = subset(denmark, select= keep)

#missing data handling
df = df[, colSums(is.na(df)) != nrow(df)]
df = df[!is.na(df$new_cases_smoothed_per_million),]
df = df[!is.na(df$new_deaths_smoothed_per_million),]
summary(df)

df$data = as.Date(df$date)

pop <- owid %>%
  filter(continent== "Europe") %>% 
  group_by(location) %>% summarise(population_density = max(population_density)) %>% arrange(desc(population_density))



denmark$weekly_hosp_admissions


#line data of overall cases per million and over all deaths
denmark_case <- ggplot(df, aes(x=date,y=new_cases_smoothed_per_million)) + geom_line(col=2) +
  scale_x_date(date_labels = "%y %b",date_breaks = "1 month")+
  scale_y_continuous(labels = scales::comma) + theme_classic() + labs(x = "", y= "", color = "")
case <- list(
  text = "New Cases Per million in Denmark",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)
den_case_ly <- ggplotly(denmark_case) %>% layout(annotations = case)

den_deaths <- ggplot(df, aes(x=date,y=new_deaths_smoothed_per_million)) + geom_line(col=2) +
  scale_x_date(date_labels = "%y %b",date_breaks = "1 month") +
  scale_y_continuous(labels = scales::comma) + theme_classic() + labs(x = "", y= "", color = "")
death <- list(
  text = "New Deaths",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)
den_deaths_ly <- ggplotly(den_death) %>% layout(annotations = death)

den_hosp <- ggplot(df, aes(x=date,y=hosp_patients_per_million)) + geom_line(col=2) +
  scale_x_date(date_labels = "%y %b",date_breaks = "1 month")+
  scale_y_continuous(labels = scales::comma) + theme_classic() + labs(x = "", y= "", color = "")
hosp <- list(
  text = "Hospitalizations Per 1M",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)
den_hosp_ly <- ggplotly(den_hosp) %>% layout(annotations = hosp)

den_vac <- ggplot(df, aes(x=date,y=people_fully_vaccinated_per_hundred)) + geom_line(col=2) +
  scale_x_date(date_labels = "%y %b",date_breaks = "1 month") +
  scale_y_continuous(labels = scales::comma) + theme_classic() + labs(x = "", y= "", color = "")
vac <- list(
  text = "New Deaths",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)
den_vac_ly <- ggplotly(den_vac) %>% layout(annotations = vac)


subplot(den_case_ly,den_deaths_ly,den_hosp_ly,den_vac_ly,nrows=4,margin = 0.07, titleX = TRUE, titleY = TRUE) %>% layout(showlegend = FALSE)


