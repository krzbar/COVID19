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

c_ending<-""
if (b_regions){c_sink_start<-"MSE_Regions";c_ending<-paste0(c_ending,"_regions")}else{c_sink_start<-"MSE_Italy";c_ending<-paste0(c_ending,"_all")}
if (!b_donumtest){
    if (b_dolog){c_ending<-paste0(c_ending,"_log")
    }else{c_ending<-paste0(c_ending,"_real")}
    if (b_dodaily){c_ending<-paste0(c_ending,"_")
    }else{c_ending<-paste0(c_ending,"_no")}
    c_ending<-paste0(c_ending,"dailytest")
}else{
    if (b_dolog){c_ending<-paste0(c_ending,"_log")
    }else{c_ending<-paste0(c_ending,"_real")}
    c_ending<-paste0(c_ending,"_numtest")
}
if (b_onlycumul){
    c_ending<-paste0(c_ending,"_onlycumul")
}
dp <- covid19("DPC", vintage = TRUE, end = data_study_date_end)
dp <- subset(dp, confirmed>0)
if (b_dolog){
    dp$confirmed_scaled <- log(dp$confirmed/(dp$population))
}else{
    dp$confirmed_scaled <- (dp$confirmed/(dp$population))
}

if (b_regions){
    it <- covid19("ITA", 2, vintage = TRUE, start = data_study_date_start, end = data_study_date_end)
}else{it <- covid19("ITA", 1,  vintage = TRUE, start = data_study_date_start, end = data_study_date_end)}

it <- it %>%
  dplyr::group_by(administrative_area_level_1, administrative_area_level_2, administrative_area_level_3) %>%
  dplyr::mutate(confirmed_new = c(confirmed[1], pmax(0,diff(confirmed))),
                tests_new     = c(tests[1],     pmax(0,diff(tests))),
                deaths_new    = c(deaths[1],    pmax(0,diff(deaths))))

if (b_scale_2020deaths){
## connect with whole population death data
    source("prep_pop_data.R")
    if (b_regions){
    ## for each region
	it<-it2
    }else{
	## for whole of Italy
	## do the same thing that is in prep_pop_data.R (but there it is for region)
	
	it <- it2 %>% 
	  group_by(administrative_area_level_1, date) %>%
	  summarise(pop_deaths = sum(pop_deaths,na.rm=TRUE),
	            pop_deaths_2020 = sum(pop_deaths_2020,na.rm=TRUE),
	            excess_death=sum(excess_death,na.rm=TRUE),
	            weekly_death=sum(weekly_death,na.rm=TRUE),
    	            weekly_death_pop=sum(weekly_death_pop,na.rm=TRUE),
    	            weekly_death_pop_pastavg=sum(weekly_death_pop_pastavg,na.rm=TRUE),
    	            cumul_total_pop_deaths=sum(cumul_total_pop_deaths,na.rm=TRUE),
	            cumul_total_pop_deaths_2020=sum(cumul_total_pop_deaths_2020,na.rm=TRUE)	            
	            ) %>%
	  right_join(it)

	it <- it %>%
		mutate(excess_death_frac=weekly_death/excess_death,weekly_death_frac=weekly_death/weekly_death_pop,weekly_death_frac_pastavg=weekly_death/weekly_death_pop_pastavg,cumul_death_frac=deaths/cumul_total_pop_deaths_2020,cumul_death_frac_pastavg=deaths/cumul_total_pop_deaths,death_2020_ratio=pop_deaths_2020/pop_deaths)    
    }
    
    if (!is.na(pop_2020_end_date)){
	it$death_2020_ratio[which(it$date > pop_2020_end_date)]<-NA
	it$pop_deaths_2020[which(it$date > pop_2020_end_date)]<-NA
	it$excess_death[which(it$date > pop_2020_end_date)]<-NA
	it$excess_death_frac[which(it$date > pop_2020_end_date)]<-NA
	#it$weekly_death[which(it$date > pop_2020_end_date)]<-NA
	it$weekly_death_frac[which(it$date > pop_2020_end_date)]<-NA	
	it$weekly_death_pop[which(it$date > pop_2020_end_date)]<-NA
	it$cumul_total_pop_deaths_2020[which(it$date > pop_2020_end_date)]<-NA
	it$cumul_death_frac[which(it$date > pop_2020_end_date)]<-NA
    }
    
    it <- it %>% 
	    mutate(deaths_div_by=pop_deaths_2020,deaths_div_by_cumul=cumul_total_pop_deaths_2020)
    
    if (b_dolog){
        it$excess_death_frac <- log(it$excess_death_frac)
        it$weekly_death_frac <- log(it$weekly_death_frac)
        it$weekly_death_frac_pastavg <- log(it$weekly_death_frac_pastavg)
        it$cumul_death_frac <- log(it$cumul_death_frac)
        it$cumul_death_frac_pastavg <- log(it$cumul_death_frac_pastavg)        
        it$death_2020_ratio <- log(it$death_2020_ratio)
    }
}else{
  
  if(b_regions){
    istat <- read.csv("ITA.csv")
    it <- merge(it, istat[,c('id','pop_death_rate')], by.x = 'administrative_area_level_2', by.y = 'id', all.x = TRUE)
  }else {it$pop_death_rate <- 0.0107}
  
    it <- it %>% 
	mutate(deaths_div_by=(pop_death_rate*population/365),deaths_div_by_cumul=(1:length(deaths)*pop_death_rate*population/365),deaths_cumul=deaths)
}


{
    if (!b_donumtest){
	if (b_dolog){
	    if (b_dodaily){
		it <- it %>% 
		    mutate(confirmed_scaled     = log(confirmed/tests),
        		confirmed_new_scaled = log(confirmed_new/tests_new),
        		deaths_scaled        = log(deaths_new/deaths_div_by))
    	    }else{
		it <- it %>% 
		    mutate(confirmed_scaled     = log(confirmed/tests),
        		deaths_scaled        = log(deaths_new/deaths_div_by))        		
    	    }
	}else{
	    if (b_dodaily){
		it <- it %>% 
		     mutate(confirmed_scaled     = (confirmed/tests),
    			confirmed_new_scaled = (confirmed_new/tests_new),
    			deaths_scaled        = (deaths_new/deaths_div_by))
    	    }else{
    		it <- it %>% 
		     mutate(confirmed_scaled     = (confirmed/tests),
    			deaths_scaled        = (deaths_new/deaths_div_by))
    	    }
    }
    }else{
	if (b_dolog){
	    it <- it %>% 
		mutate(confirmed_new_gr = log(confirmed_new),
    		    tests_new_gr = log(tests_new),
    		    diff_confirmed_new= log(confirmed_new)-log(tests_new),
    		    diff_confirmed_cumul= log(confirmed)-log(tests)
        	)
	}else{
	    it <- it %>% 
		mutate(confirmed_new_gr = (confirmed_new),
    		    tests_new_gr = (tests_new)	
        	)
	}
    }
}

it <- it %>% 
  arrange(date) %>%
  group_by(administrative_area_level_1, administrative_area_level_2, administrative_area_level_3) 

dp <- dp %>%
  arrange(date)
