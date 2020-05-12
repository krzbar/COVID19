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
x <- read.csv("comuni_giornaliero.csv", na.strings = 'n.d.') ## csv obtained from https://www.istat.it/it/archivio/240401

# age
x$age <- "65+"
x$age[x$CL_ETA <= 13] <- "15-64"
x$age[x$CL_ETA <= 3] <- "0-14"

# grou by region, week and age. Trasform to daily numbers
x1 <- x %>% group_by(NOME_REGIONE, GE, age) %>%
  summarise(
    "MASCHI_2015" = sum(M_15, na.rm = TRUE),
    "MASCHI_2016" = sum(M_16, na.rm = TRUE),
    "MASCHI_2017" = sum(M_17, na.rm = TRUE),
    "MASCHI_2018" = sum(M_18, na.rm = TRUE),
    "MASCHI_2019" = sum(M_19, na.rm = TRUE),
    "MASCHI_2020" = sum(M_20, na.rm = TRUE),
    
    "FEMMINE_2015" = sum(F_15, na.rm = TRUE),
    "FEMMINE_2016" = sum(F_16, na.rm = TRUE),
    "FEMMINE_2017" = sum(F_17, na.rm = TRUE),
    "FEMMINE_2018" = sum(F_18, na.rm = TRUE),
    "FEMMINE_2019" = sum(F_19, na.rm = TRUE),
    "FEMMINE_2020" = sum(F_20, na.rm = TRUE),
    
    "TOTALE_2015" = sum(T_15, na.rm = TRUE),
    "TOTALE_2016" = sum(T_16, na.rm = TRUE),
    "TOTALE_2017" = sum(T_17, na.rm = TRUE),
    "TOTALE_2018" = sum(T_18, na.rm = TRUE),
    "TOTALE_2019" = sum(T_19, na.rm = TRUE),
    "TOTALE_2020" = sum(T_20, na.rm = TRUE)
  ) %>%
  ungroup()

# clean state and date
x1$administrative_area_level_2 <- as.character(x1$NOME_REGIONE)
x1$date <- as.Date(sapply(x1$GE, function(d){
  paste0('0',d)
}), format = "%m%d")


######### merge with COVID data #########
require(COVID19)
it_tmp <- covid19("ITA", 2, vintage = TRUE, start = '2020-02-24', end = '2020-04-20') %>%
  dplyr::group_by(administrative_area_level_1, administrative_area_level_2, administrative_area_level_3) %>%
  dplyr::mutate(confirmed_new = c(confirmed[1], pmax(0,diff(confirmed))),
                tests_new     = c(tests[1],     pmax(0,diff(tests))),
                deaths_new    = c(deaths[1],    pmax(0,diff(deaths))))


x2 <- x1 %>% 
  group_by(administrative_area_level_2, date) %>%
  summarise(pop_deaths_2020 = sum(TOTALE_2020),
            pop_deaths = sum(TOTALE_2015, 
                             TOTALE_2016, 
                             TOTALE_2017, 
                             TOTALE_2018, 
                             TOTALE_2019)/5)

## correct names of regions to be consistent between the data sets
x2$administrative_area_level_2[which(x2$administrative_area_level_2=="Friuli-Venezia Giulia")]<-"Friuli Venezia Giulia"
x2$administrative_area_level_2[which(x2$administrative_area_level_2=="Valle d'Aosta/Vallée d'Aoste")]<-"Valle d'Aosta"

x3 <- x2
it2 <- as_tibble(merge(x = it_tmp, y = x3, all.x = TRUE, by = c("administrative_area_level_2","date")))

# fill pop_deaths in the period 5 April - today (data not available from ISTAT)
# it2 <- it2 %>% fill(pop_deaths, pop_deaths_2020)
it2 <- it2 %>% 
  group_by(id) %>%
  arrange(id, date) %>%
  fill(pop_deaths)


# weekly deaths/(pop_death_2020-pop_death)
it3 <- it2 %>%
  group_by(administrative_area_level_1, administrative_area_level_2, administrative_area_level_3, date) %>% 
  summarise(
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

## P. A. Bolzano and P. A. Trento are joined in ISTAT's population deaths as one region "Trentino-Alto Adige/Sudtirol"
it3$pop_deaths_2020[which(it3$administrative_area_level_2=="P.A. Bolzano")]<-NA
it3$pop_deaths_2020[which(it3$administrative_area_level_2=="P.A. Trento")]<-NA
it3$excess_death_frac[which(it3$administrative_area_level_2=="P.A. Bolzano")]<-NA
it3$excess_death_frac[which(it3$administrative_area_level_2=="P.A. Trento")]<-NA
it3$excess_death[which(it3$administrative_area_level_2=="P.A. Bolzano")]<-NA
it3$excess_death[which(it3$administrative_area_level_2=="P.A. Trento")]<-NA
it3$death_2020_ratio[which(it3$administrative_area_level_2=="P.A. Bolzano")]<-NA
it3$death_2020_ratio[which(it3$administrative_area_level_2=="P.A. Trento")]<-NA
it3$weekly_death_frac[which(it3$administrative_area_level_2=="P.A. Bolzano")]<-NA
it3$weekly_death_frac[which(it3$administrative_area_level_2=="P.A. Trento")]<-NA
it3$weekly_death_frac_pastavg[which(it3$administrative_area_level_2=="P.A. Bolzano")]<-NA
it3$weekly_death_frac_pastavg[which(it3$administrative_area_level_2=="P.A. Trento")]<-NA



it3<- it3 %>% 
        mutate(cumul_total_pop_deaths=NA,cumul_total_pop_deaths_2020=NA)

for (i in 1:nrow(it3)){
## get cumulative number of deaths till end of week in each region
    it3_state<-it3[which(it3$administrative_area_level_2==it3$administrative_area_level_2[i]),]
    # it3$cumul_total_pop_deaths_2020[i]<-sum(it3_state$pop_deaths_2020[which(it3_state$date<=it3$date[i])])
    # it3$cumul_total_pop_deaths[i]<-sum(it3_state$pop_deaths[which(it3_state$date<=it3$date[i])])
    time_first_death <- min(it3_state$date[it3_state$deaths>0])
    it3$cumul_total_pop_deaths_2020[i]<-sum(it3_state$pop_deaths_2020[which(it3_state$date>=time_first_death & it3_state$date<=it3$date[i])],na.rm=TRUE)
    it3$cumul_total_pop_deaths[i]<-sum(it3_state$pop_deaths[which(it3_state$date>=time_first_death & it3_state$date<=it3$date[i])],na.rm=TRUE)
}

## P. A. Bolzano and P. A. Trento are joined in ISTAT's population deaths as one region "Trentino-Alto Adige/Sudtirol"
it3$cumul_total_pop_deaths_2020[which(it3$administrative_area_level_2=="P.A. Bolzano")]<-NA
it3$cumul_total_pop_deaths_2020[which(it3$administrative_area_level_2=="P.A. Trento")]<-NA
it3$cumul_total_pop_deaths[which(it3$administrative_area_level_2=="P.A. Bolzano")]<-NA
it3$cumul_total_pop_deaths[which(it3$administrative_area_level_2=="P.A. Trento")]<-NA

it3<- it3 %>% 
        mutate(cumul_death_frac = deaths/cumul_total_pop_deaths_2020,cumul_death_frac_pastavg = deaths/cumul_total_pop_deaths)

for(i in c('excess_death','excess_death_frac','weekly_death','weekly_death_pop','weekly_death_pop_pastavg','weekly_death_frac','weekly_death_frac_pastavg','cumul_total_pop_deaths','cumul_total_pop_deaths_2020','cumul_death_frac','cumul_death_frac_pastavg','deaths_cumul','death_2020_ratio')){
  it2[[i]] <- as.numeric(NA)
}

for (i in 1:nrow(it2)){
## copy all the data to it2, with a flat value for each week
    it3_state<-it3[which(it3$administrative_area_level_2==it2$administrative_area_level_2[i]),]
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





