library(tidyverse)
library(gridExtra)
library(scales)
library(lubridate)
library(ggplot2)
library(zoo)
library(plotly)

vaccine <- read_csv('data/vaccination-data.csv')
covid <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")


covid <- covid %>% 
  filter(WHO_region != "Other") %>% 
  mutate(WHO_region = fct_recode(WHO_region,                            "Eastern Mediterranean"="EMRO","Europe" = "EURO","Africa" = "AFRO","Western Pacific" = "WPRO","Americas"="AMRO","South-East Asia" = "SEARO"))

range(covid$Date_reported)
length(unique(covid$Country))

owid <- read_csv('data/owid-covid-data.csv')
denmark = subset(owid,location == "Denmark")


covid %>% group_by(WHO_region) %>% 
  summarise(New_cases = sum(New_cases)) 

#interactive
world_case <- ggplot(covid, aes(x=Date_reported,y=New_cases)) + geom_line(col=2) +
  scale_y_continuous(labels = scales::comma) + theme_classic() + labs(x = "", y= "", color = "")
case_a <- list(
  text = "New cases in the world",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)
world_case_ly <- ggplotly(world_case) %>% layout(annotations = case_a)

#covid cases and death data per day 
euro_covid = subset(covid, WHO_region == 'Europe')
summary(euro_covid)
fig.spaghetti.1 <- covid %>% 
  filter(Date_reported>= "2020-01-03", Date_reported<= "2021-01-28", Country_code=="IT") %>% 
  mutate(Date=as.Date(Date_reported)) %>%
  ggplot(aes(x=Date,y=New_cases,by=Country)) +
  geom_line(aes(color=Country)) +
  theme(legend.position ='none')
fig.spaghetti.1   

#functions for plotting 
plot_new_case_pr_day = function(place_abbr){
  data_for_country = subset(covid, Country_code == place_abbr)
  
  newd = aggregate(New_cases~Date_reported, data = data_for_country, sum)
  
  
  #newd$date = as.yearmon(paste(newd$yr, newd$mo), "%Y %m")
  #newd$date = as.Date(newd$date)
  
  ggplot(data_for_country)+
    geom_bar(aes(x=Date_reported, y = New_cases), stat = "identity")+xlab("Date") +
    ylab("Number of New Cases")+ggtitle(data_for_country$Country)+ theme_bw()
  
}

plot_new_case_pr_day('DK')
#monthly new cases

library(gridExtra)
monthly_new_cases = function(place_abbr){
  
  data_for_country = subset(covid, Country_code == place_abbr)
  
  #split into month and year
  data_for_country$mo <- strftime(data_for_country$Date_reported, "%m")
  data_for_country$yr <- strftime(data_for_country$Date_reported, "%Y")
  
  #aggregate by month
  new.case = aggregate(New_cases~mo +yr, data = data_for_country, sum)
  new.case$date = as.yearmon(paste(new.case$yr, new.case$mo), "%Y %m")
  new.case$date = as.Date(new.case$date)
  
  ggplot(new.case)+
    geom_bar(aes(x=date, y = New_cases), stat = "identity")+xlab("Date") +
    ylab("Number of New Cases")+ggtitle(paste0("Monthly Number of New Cases for ", data_for_country$Country))+
    theme(plot.title = element_text(hjust = 0.5))+theme_bw()+
    scale_x_date(date_labels = "%y %b",date_breaks = "1 month")+
    #scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
    theme(axis.text.x = element_text(angle = 90))
}
monthly_new_cases('IT')

monthly_num_dead = function(place_abbr){
  
  data_for_country = subset(covid, Country_code == place_abbr)
  
  #split into month and year
  data_for_country$mo <- strftime(data_for_country$Date_reported, "%m")
  data_for_country$yr <- strftime(data_for_country$Date_reported, "%Y")
  
  #aggregate by month
  newd.dead = aggregate(New_deaths~mo +yr, data = data_for_country, sum)
  newd.dead$date = as.yearmon(paste(newd.dead$yr, newd.dead$mo), "%Y %m")
  newd.dead$date = as.Date(newd.dead$date)
  
  ggplot(newd.dead)+
    geom_bar(aes(x=date, y = New_deaths), stat = "identity")+xlab("Date") +
    ylab("Mortalities")+ggtitle(paste0("Monthly Mortalities for ", data_for_country$Country))+
    theme(plot.title = element_text(hjust = 0.5))+theme_bw()+
    scale_x_date(date_labels = "%y %b",date_breaks = "1 month")+
    #scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
    theme(axis.text.x = element_text(angle = 90))
}

monthly_num_dead('IT')

cases_mortality_plot = function(place_abbr){
  
  data_for_country = subset(covid, Country_code == place_abbr)
  
  #split into month and year
  data_for_country$mo <- strftime(data_for_country$Date_reported, "%m")
  data_for_country$yr <- strftime(data_for_country$Date_reported, "%Y")
  
  #aggregate by month
  new.case = aggregate(New_cases~mo +yr, data = data_for_country, sum)
  new.case$date = as.yearmon(paste(new.case$yr, new.case$mo), "%Y %m")
  new.case$date = as.Date(new.case$date)
  
  g1 = ggplot(new.case)+
    geom_bar(aes(x=date, y = New_cases), stat = "identity")+xlab("Date") +
    ylab("Number of New Cases")+ggtitle(paste0("Monthly Number of New Cases for ", data_for_country$Country))+
    theme(plot.title = element_text(hjust = 0.5))+theme_bw()+
    scale_x_date(date_labels = "%y %b",date_breaks = "1 month")+
    #scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
    theme(axis.text.x = element_text(angle = 90))
  
  #aggregate by month
  newd.dead = aggregate(New_deaths~mo +yr, data = data_for_country, sum)
  newd.dead$date = as.yearmon(paste(newd.dead$yr, newd.dead$mo), "%Y %m")
  newd.dead$date = as.Date(newd.dead$date)
  
  g2 = ggplot(newd.dead)+
    geom_bar(aes(x=date, y = New_deaths), stat = "identity")+xlab("Date") +
    ylab("Mortalities")+ggtitle(paste0("Monthly Mortalities for ", data_for_country$Country))+
    theme(plot.title = element_text(hjust = 0.5))+theme_bw()+
    scale_x_date(date_labels = "%y %b",date_breaks = "1 month")+
    #scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
    theme(axis.text.x = element_text(angle = 90))
  
  a = grid.arrange(g1,g2, nrow = 2)
  return(a)
}
cases_mortality_plot('DK')

covid$Country[covid$Country == 'United States of America'] <- 'USA'
covid$Country[covid$Country == 'Bolivia (Plurinational State of)'] <- 'Bolivia'

library(maps)
world <- map_data("world");
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) 


covid.today<- covid %>% 
  filter(Date_reported == "2021-2-18") %>% 
  mutate(region=Country)


covid.today.world<- inner_join(world, covid.today, by = "region")

fig.map  <- ggplot() +
  geom_polygon(data = covid.today.world, aes(x=long, y = lat, group = group,fill=New_deaths)) + 
  coord_fixed(1.3) + ggtitle("New Confirmed Deaths as of 18th February 2022")
fig.map
