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
require(tidyverse)
require(hrbrthemes)
library(ggpubr)
library(gridExtra)
library(ggthemes)

## read in population deaths data
x <- read.csv("comuni_giornaliero.csv", na.strings = 'n.d.',encoding="latin1") ## csv obtained from https://www.istat.it/it/archivio/240401

# age
x$age <- "65+"
x$age[x$CL_ETA <= 13] <- "15-64"
x$age[x$CL_ETA <= 3] <- "0-14"

## KB
x$date <- as.Date(sapply(x$GE, function(d){
  paste0('0',d)
}), format = "%m%d")
x$week <- as.integer((x$date - as.Date(pop_date_till))/7)
## =============================   


# group by region, day and age. Trasform to daily numbers
# KB group by region, week and age. Trasform to weekly sum
#x1 <- x %>% group_by(NOME_REGIONE, GE, age) %>%
x1 <- x %>% group_by(NOME_REGIONE, week, age) %>%
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
#x1$date <- as.Date(sapply(x1$GE, function(d){
#  paste0('0',d)
#}), format = "%m%d")


## correct names of regions to be consistent between the data sets
x1$administrative_area_level_2[which(x1$administrative_area_level_2=="Friuli-Venezia Giulia")]<-"Friuli Venezia Giulia"
x1$administrative_area_level_2[which(x1$administrative_area_level_2=="Valle d'Aosta/Vallée d'Aoste")]<-"Valle d'Aosta"
x1$administrative_area_level_2[which(x1$administrative_area_level_2=="Trentino-Alto Adige/Südtirol")]<-"P. A. Bolzano/P. A. Trento"

# fill
x1 <- x1 %>% 
  dplyr::na_if(0) %>% 
  dplyr::group_by(administrative_area_level_2, age) %>%
#  dplyr::arrange(date) %>%
  dplyr::arrange(week) %>% ## KB
  tidyr::fill(-c(administrative_area_level_2, age))


#x3 <- x1
#x3$date <- NULL
#it2 <- as_tibble(merge(x = it_tmp, y = x3, all.x = TRUE, by = c("administrative_area_level_2","week")))

# fill pop_deaths in the period 21 March - today (data not available from ISTAT)
# it2 <- it2 %>% fill(pop_deaths, pop_deaths_2020)
#it2 <- it2 %>% fill(pop_deaths)



# function to do and save plots
f <- function(x, g){

    name_g<-make.names(g)
    name_g<-gsub("\\.","_",name_g)  
    name_g<-gsub("é","e",name_g)  
    name_g<-gsub("ü","u",name_g)  

  d <- sprintf("ITraw_mortality")##, make.names(g))
  if(!dir.exists(d))
    dir.create(d)
  

 # if(!dir.exists(d))
#    dir.create(d)
  
  x$week<-x$week+abs(min(x$week))+1
  gTot<-ggplot(data = x, aes(x = week)) +
          geom_line(aes(y = TOTALE_2015, group = age, color = age), linetype = "longdash") +
          geom_line(aes(y = TOTALE_2016, group = age, color = age), linetype = "dotdash") +
          geom_line(aes(y = TOTALE_2017, group = age, color = age), linetype = "dotted") +
          geom_line(aes(y = TOTALE_2018, group = age, color = age), linetype = "dashed") +
          geom_line(aes(y = TOTALE_2019, group = age, color = age), linetype = "twodash") +
          geom_line(aes(y = TOTALE_2020, group = age, color = age),size=1.5) +
          ggtitle(paste(g, "TOTAL")) +
          theme_ipsum(base_family="sans") +
          xlab("") + ylab("Number of deaths per week") +
          labs(color = "Age (years)")
  
 gMen<-ggplot(data = x, aes(x = week)) +
          geom_line(aes(y = MASCHI_2015, group = age, color = age), linetype = "longdash") +
          geom_line(aes(y = MASCHI_2016, group = age, color = age), linetype = "dotdash") +
          geom_line(aes(y = MASCHI_2017, group = age, color = age), linetype = "dotted") +
          geom_line(aes(y = MASCHI_2018, group = age, color = age), linetype = "dashed") +
          geom_line(aes(y = MASCHI_2019, group = age, color = age), linetype = "twodash") +
          geom_line(aes(y = MASCHI_2020, group = age, color = age),size=1.5) +
          ggtitle(paste(g, "MEN")) +
          theme_ipsum(base_family="sans") +
          xlab("") + ylab("Number of deaths per week") +
          labs(color = "Age (years)")
  
 gWomen<-ggplot(data = x, aes(x = week)) +
          geom_line(aes(y = FEMMINE_2015, group = age, color = age), linetype = "longdash") +
          geom_line(aes(y = FEMMINE_2016, group = age, color = age), linetype = "dotdash") +
          geom_line(aes(y = FEMMINE_2017, group = age, color = age), linetype = "dotted") +
          geom_line(aes(y = FEMMINE_2018, group = age, color = age), linetype = "dashed") +
          geom_line(aes(y = FEMMINE_2019, group = age, color = age), linetype = "twodash") +
          geom_line(aes(y = FEMMINE_2020, group = age, color = age),size=1.5) +
          ggtitle(paste(g, "WOMEN")) +
          theme_ipsum(base_family="sans") +
          xlab("") + ylab("Number of deaths per week") +
          labs(color = "Age (years)")

    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)
    }
      mylegend <- g_legend(gTot)

  pdf(paste(d, "/",name_g,"_rawmortality.pdf", sep = ""), width = 20)
    grid.arrange(arrangeGrob(gTot + theme(legend.position="none"),
                                 gWomen + theme(legend.position="none"),
                             gMen + theme(legend.position="none"),
                             nrow=1),
#                 mylegend, nrow=2, heights=c(10, 2))
                   nrow=1, heights=c(10)) 
  dev.off()
  
}

  
# do and save plots
g <- x1 %>% group_by(administrative_area_level_2) %>%
  group_map(f)


it_all <- x1 %>% 
#          group_by(date,age) %>% 
          group_by(week,age) %>%  ##KB
          summarise(                    
            FEMMINE_2015=sum(FEMMINE_2015),
            FEMMINE_2016=sum(FEMMINE_2016),
            FEMMINE_2017=sum(FEMMINE_2017),
            FEMMINE_2018=sum(FEMMINE_2018),
            FEMMINE_2019=sum(FEMMINE_2019),
            FEMMINE_2020=sum(FEMMINE_2020),
	    MASCHI_2015=sum(MASCHI_2015),
	    MASCHI_2016=sum(MASCHI_2016),
	    MASCHI_2017=sum(MASCHI_2017),
	    MASCHI_2018=sum(MASCHI_2018),
	    MASCHI_2019=sum(MASCHI_2019),
	    MASCHI_2020=sum(MASCHI_2020),
	    TOTALE_2015=sum(TOTALE_2015),
	    TOTALE_2016=sum(TOTALE_2016),
	    TOTALE_2017=sum(TOTALE_2017),
	    TOTALE_2018=sum(TOTALE_2018),
	    TOTALE_2019=sum(TOTALE_2019),
	    TOTALE_2020=sum(TOTALE_2020)          
          ) 
#%>%
#          right_join(it_all)

f(it_all,"Italy")
