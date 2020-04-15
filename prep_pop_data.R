## Script using COVID19 (package available from https://github.com/emanuele-guidotti/COVID19) package
## Copyright (C) 2020 
## Authors Krzysztof Bartoszek, Emanuele Guidotti, Stefano Iacus, Marcin Okroj
##
##  This program is free software: you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation, either version 3 of the License, or
##    (at your option) any later version.
##
##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.

##    You should have received a copy of the GNU General Public License
## along with this program. If not, see <https://www.gnu.org/licenses/>.

## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this package or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .

## read in population deaths data
x <- read.csv("comuni_settimana.csv") ## csv obtained from https://www.istat.it/it/archivio/240401

# age
x$age <- as.character(x$CLASSE_DI_ETA)
id1 <- startsWith(x$age, "0")
id2 <- startsWith(x$age, "1")
x$age[id1] <- "0-14"
x$age[id2] <- "15-64"
x$age[!id1 & !id2] <- "65+"

# grou by region, week and age. Trasform to daily numbers
x1 <- x %>% group_by(NOME_REGIONE, SETTIMANA, age) %>%
  summarise(
    "MASCHI_2015" = sum(MASCHI_2015)/7,
    "MASCHI_2016" = sum(MASCHI_2016)/7,
    "MASCHI_2017" = sum(MASCHI_2017)/7,
    "MASCHI_2018" = sum(MASCHI_2018)/7,
    "MASCHI_2019" = sum(MASCHI_2019)/7,
    "MASCHI_2020" = sum(MASCHI_2020)/7,
    
    "FEMMINE_2015" = sum(FEMMINE_2015)/7,
    "FEMMINE_2016" = sum(FEMMINE_2016)/7,
    "FEMMINE_2017" = sum(FEMMINE_2017)/7,
    "FEMMINE_2018" = sum(FEMMINE_2018)/7,
    "FEMMINE_2019" = sum(FEMMINE_2019)/7,
    "FEMMINE_2020" = sum(FEMMINE_2020)/7,
    
    "TOTALE_2015" = sum(TOTALE_2015)/7,
    "TOTALE_2016" = sum(TOTALE_2016)/7,
    "TOTALE_2017" = sum(TOTALE_2017)/7,
    "TOTALE_2018" = sum(TOTALE_2018)/7,
    "TOTALE_2019" = sum(TOTALE_2019)/7,
    "TOTALE_2020" = sum(TOTALE_2020)/7
  ) %>%
  ungroup()

# clean state and date
x1$state <- as.character(x1$NOME_REGIONE)
x1$date <- as.Date(sapply(x1$SETTIMANA, function(d){
  str_split(as.character(d), pattern = '-')[[1]][2]
}), format = "%d/%m")


######### merge with COVID data #########
require(COVID19)
it_tmp <- covid19("ITA", 2) %>%
  dplyr::group_by(country, state, city) %>%
  dplyr::mutate(confirmed_new = c(confirmed[1], pmax(0,diff(confirmed))),
                tests_new     = c(tests[1],     pmax(0,diff(tests))),
                deaths_new    = c(deaths[1],    pmax(0,diff(deaths))))


x2 <- x1 %>% 
  group_by(state, date) %>%
  summarise(pop_deaths_2020 = sum(TOTALE_2020),
            pop_deaths = sum(TOTALE_2015, 
                             TOTALE_2016, 
                             TOTALE_2017, 
                             TOTALE_2018, 
                             TOTALE_2019)/5)


x2$week <- as.integer((x2$date - as.Date(pop_date_till))/7)
it_tmp$week <- as.integer((it_tmp$date - as.Date(pop_date_till))/7)

x3 <- x2
x3$date <- NULL
it2 <- as_tibble(merge(x = it_tmp, y = x3, all.x = TRUE, by = c("state","week")))

# fill pop_deaths in the period 21 March - today (data not available from ISTAT)
# it2 <- it2 %>% fill(pop_deaths, pop_deaths_2020)
it2 <- it2 %>% fill(pop_deaths)


# weekly deaths/(pop_death_2020-pop_death)
it3 <- it2 %>%
  group_by(country, state, country, week) %>% 
  summarise(date = max(date),
            excess_death = sum(pop_deaths_2020-pop_deaths,na.rm=TRUE),
            excess_death_frac = sum(deaths_new,na.rm=TRUE)/sum(pop_deaths_2020-pop_deaths,na.rm=TRUE),
            death_2020_ratio = sum(pop_deaths_2020,na.rm=TRUE)/sum(pop_deaths,na.rm=TRUE),
            weekly_death = sum(deaths_new,na.rm=TRUE), ## COVID positive deaths in each week
            weekly_death_frac = sum(deaths_new,na.rm=TRUE)/sum(pop_deaths_2020,na.rm=TRUE), ## fracion of COVID positive deaths in week
            weekly_death_frac_pastavg = sum(deaths_new,na.rm=TRUE)/sum(pop_deaths,na.rm=TRUE), ## fracion of COVID positive deaths in week
            deaths= max(deaths,na.rm=TRUE), ## cumulative number of COVID positive deaths at end of week
            pop_deaths_2020=sum(pop_deaths_2020,na.rm=TRUE), ##total number of deaths in week
            pop_deaths=sum(pop_deaths,na.rm=TRUE) ##total number of deaths in week
            )
it3<- it3 %>% 
        mutate(cumul_total_pop_deaths=NA,cumul_total_pop_deaths_2020=NA)

for (i in 1:nrow(it3)){
## get cumulative number of deaths till end of week in each region
    it3_state<-it3[which(it3$state==it3$state[i]),]
    it3$cumul_total_pop_deaths_2020[i]<-sum(it3_state$pop_deaths_2020[which(it3_state$date<=it3$date[i])])
    it3$cumul_total_pop_deaths[i]<-sum(it3_state$pop_deaths[which(it3_state$date<=it3$date[i])])
}

it3<- it3 %>% 
        mutate(cumul_death_frac = deaths/cumul_total_pop_deaths_2020,cumul_death_frac_pastavg = deaths/cumul_total_pop_deaths)
it2<- it2 %>% 
        mutate(excess_death=NA,excess_death_frac=NA,weekly_death=NA,weekly_death_pop=NA,weekly_death_pop_pastavg=NA,weekly_death_frac=NA,weekly_death_frac_pastavg=NA,cumul_total_pop_deaths=NA,cumul_total_pop_deaths_2020=NA,cumul_death_frac=NA,cumul_death_frac_pastavg=NA,deaths_cumul=NA,death_2020_ratio=NA)
        
for (i in 1:nrow(it2)){
## copy all the data to it2, with a flat value for each week
    it3_state<-it3[which(it3$state==it2$state[i]),]
    week_end_date<-min(it3_state$date[which(it3_state$date>=it2$date[i])])
    i_week<-which(it3_state$date==week_end_date)
    it2[i,"excess_death"]<-it3_state[i_week,"excess_death"]
    it2[i,"excess_death_frac"]<-it3_state[i_week,"excess_death_frac"]
    it2[i,"weekly_death"]<-it3_state[i_week,"weekly_death"]
    it2[i,"weekly_death_frac"]<-it3_state[i_week,"weekly_death_frac"]
    it2[i,"weekly_death_frac_pastavg"]<-it3_state[i_week,"weekly_death_frac_pastavg"]
    it2[i,"weekly_death_pop"]<-it3_state[i_week,"pop_deaths_2020"]
    it2[i,"weekly_death_pop_pastavg"]<-it3_state[i_week,"pop_deaths"]
    it2[i,"cumul_total_pop_deaths_2020"]<-it3_state[i_week,"cumul_total_pop_deaths_2020"]
    it2[i,"cumul_death_frac"]<-it3_state[i_week,"cumul_death_frac"]
    it2[i,"cumul_total_pop_deaths"]<-it3_state[i_week,"cumul_total_pop_deaths"]
    it2[i,"cumul_death_frac_pastavg"]<-it3_state[i_week,"cumul_death_frac_pastavg"]
    it2[i,"deaths_cumul"]<-it3_state[i_week,"deaths"]
    it2[i,"death_2020_ratio"]<-it3_state[i_week,"death_2020_ratio"]
}





