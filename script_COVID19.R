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
dp <- covid19("DPC", vintage = TRUE, end = '2020-04-20')
dp <- subset(dp, confirmed>0)
if (b_dolog){
    dp$confirmed_scaled <- log(dp$confirmed/(dp$pop))
}else{
    dp$confirmed_scaled <- (dp$confirmed/(dp$pop))
}

if (b_regions){
    it <- covid19("ITA", 2, vintage = TRUE, start = '2020-02-24', end = '2020-04-20')
}else{it <- covid19("ITA", 1, vintage = TRUE, start = '2020-02-24', end = '2020-04-20')}

it <- it %>%
  dplyr::group_by(country, state, city) %>%
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
	  group_by(country, date) %>%
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
        it$weekly_death_frac_pastavg <- log(it$weekly_death_frac)
        it$cumul_death_frac <- log(it$cumul_death_frac)
        it$cumul_death_frac_pastavg <- log(it$cumul_death_frac_pastavg)        
        it$death_2020_ratio <- log(it$death_2020_ratio)
    }
}else{
    it <- it %>% 
	mutate(deaths_div_by=(pop_death_rate*pop/365),deaths_div_by_cumul=(1:length(deaths)*pop_death_rate*pop/365),deaths_cumul=deaths)
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
  group_by(country, state, city) 

dp <- dp %>%
  arrange(date)

f1 <- function(data, key, b_dodaily, b_donumtest, b_dolog,b_onlycumul,b_do_lm_numtest){
    res<-ggplot(data = data) 
    vMSE<-c()
    if (! b_donumtest){
	res<-res+geom_line(aes(x = date, y = confirmed_scaled, color = '(IT) Confirmed/Tests')) 
	vMSE<-c(vMSE,confirmed_scaled_all=sqrt(mean((data$confirmed_scaled-dp$confirmed_scaled[1:nrow(data)])^2,na.rm=TRUE)))
	vMSE<-c(vMSE,confirmed_scaled_dp=sqrt(mean((data$confirmed_scaled[1:min(nrow(data),length(dp$confirmed_scaled))]-dp$confirmed_scaled[1:min(nrow(data),length(dp$confirmed_scaled))])^2,na.rm=TRUE)))
	    if ((!b_onlycumul)&&!(b_scale_2020deaths)){
		res<-res+geom_line(aes(x = date, y = deaths_scaled, color = "(IT) Deaths Scaled"))
		vMSE<-c(vMSE,deaths_scaled=sqrt(mean((data$deaths_scaled-dp$confirmed_scaled[1:nrow(data)])^2,na.rm=TRUE)))
	    	vMSE<-c(vMSE,deaths_scaled_dp=sqrt(mean((data$deaths_scaled[1:min(nrow(data),length(dp$confirmed_scaled))]-dp$confirmed_scaled[1:min(nrow(data),length(dp$confirmed_scaled))])^2,na.rm=TRUE)))
	    }
	    if (b_dodaily){
		res<-res+geom_line(aes(x = date, y = confirmed_new_scaled, color = '(IT) Daily Confirmed/Tests'))
		vMSE<-c(vMSE,confirmed_new_scaled=sqrt(mean((data$confirmed_new_scaled-dp$confirmed_scaled[1:nrow(data)])^2,na.rm=TRUE)))
	    	vMSE<-c(vMSE,confirmed_new_scaled_dp=sqrt(mean((data$confirmed_new_scaled[1:min(nrow(data),length(dp$confirmed_scaled))]-dp$confirmed_scaled[1:min(nrow(data),length(dp$confirmed_scaled))])^2,na.rm=TRUE)))
	    }
	xdp <- dp[1:nrow(data),]
	xdp$date <- data$date[1:nrow(data)]
	res<-res+geom_line(data = xdp, size = 2, aes(x = date, y = confirmed_scaled, color = '(DP) Confirmed Scaled')) 
    }else{
    	    res<-res+geom_line(aes(x = date, y = confirmed_new_gr, color = "(IT) Confirmed")) +
		geom_line(aes(x = date, y = tests_new_gr, color = '(IT) Tests')) 
	    if (b_dolog){
		res<-res+geom_line(aes(x = date, y = diff_confirmed_new, color = '(IT) Confirmed - Tests'))
		res<-res+geom_line(aes(x = date, y = diff_confirmed_cumul, color = '(IT) Confirmed - Tests cumulative'))
		if (b_do_lm_numtest){
		    c_reg<-strsplit(data$id[1],", ")[[1]][2]
		    if (is.na(c_reg)){c_reg<-"Italy"}
		    dfdata_cutoff<-read.csv("NumTest_dates.csv",header=TRUE,sep=";")
		    i_reg<-which(dfdata_cutoff$region==c_reg)
		    sink("LM_difflogcofirmestested_slopeontime.txt",append=TRUE)
		    print(c_reg)
		    if (!is.na(dfdata_cutoff[i_reg,"date_cutoff"])){
			data_for_lm<-data
			if (!is.na(dfdata_cutoff[i_reg,"remove_datefrom"])){
			    data_for_lm$diff_confirmed_new[intersect(which(data_for_lm$date>=dfdata_cutoff[i_reg,"remove_datefrom"]),which(data_for_lm$date<=dfdata_cutoff[i_reg,"remove_dateto"]))]<-NA
			    ##data_for_lm$diff_confirmed_cumul[intersect(which(data_for_lm$date>=dfdata_cutoff[i_reg,"remove_datefrom"]),which(data_for_lm$date<=dfdata_cutoff[i_reg,"remove_dateto"]))]<-NA
			}
			data_for_lm<-data_for_lm[which(data_for_lm$date>=dfdata_cutoff[i_reg,"date_cutoff"]),]
			if (!is.na(dfdata_cutoff[i_reg,"end_date"])){
			    data_for_lm<-data_for_lm[which(data_for_lm$date<=dfdata_cutoff[i_reg,"end_date"]),]
			}
			data_for_lm$diff_confirmed_new[c(which(is.infinite(data_for_lm$diff_confirmed_new),is.nan(data_for_lm$diff_confirmed_new)))]<-NA
			data_for_lm$diff_confirmed_cumul[c(which(is.infinite(data_for_lm$diff_confirmed_cumul),is.nan(data_for_lm$diff_confirmed_cumul)))]<-NA
			data_for_lm$t <- 1:nrow(data_for_lm)
			mod_new <- lm(diff_confirmed_new ~ t, data = data_for_lm,na.action="na.exclude")
			ci_new <- predict(mod_new, newdata = data_for_lm, interval = 'prediction')
			mod_cumul <- lm(diff_confirmed_cumul ~ t, data = data_for_lm,na.action="na.exclude")
			ci_cumul <- predict(mod_new, newdata = data_for_lm, interval = 'prediction')
			data$ci_lwr_new<-NA
			data$ci_upr_new<-NA			
			data$pred_diff_new<-NA			
			data$ci_lwr_cumul<-NA
			data$ci_upr_cumul<-NA			
			data$pred_diff_cumul<-NA			
			v_dates_for_lm_indices<-which(data$date>=dfdata_cutoff[i_reg,"date_cutoff"])
			if (!is.na(dfdata_cutoff[i_reg,"end_date"])){
			    v_dates_for_lm_indices<-intersect(v_dates_for_lm_indices,which(data_for_lm$date<=dfdata_cutoff[i_reg,"end_date"]))
			}
			data$pred_diff_new[v_dates_for_lm_indices]<-ci_new[,1]
			data$ci_lwr_new[v_dates_for_lm_indices]<-ci_new[,2]
			data$ci_upr_new[v_dates_for_lm_indices]<-ci_new[,3]
			data$pred_diff_cumul[v_dates_for_lm_indices]<-ci_cumul[,1]
			data$ci_lwr_cumul[v_dates_for_lm_indices]<-ci_cumul[,2]
			data$ci_upr_cumul[v_dates_for_lm_indices]<-ci_cumul[,3]

			#res<-res+  geom_line(aes(y = ci[,1],)) +   geom_ribbon(aes(ymin = ci[,2], ymax = ci[,3]), alpha = 0.4) 
			#res<-res+   geom_ribbon(aes(x=date,ymin = data$ci_lwr, ymax = data$ci_upr), alpha = 0.4) 
			if (is.na(dfdata_cutoff[i_reg,"end_date"])){
			    #res<-res+   geom_ribbon(data = data %>% filter(date >= as.Date(dfdata_cutoff[i_reg,"date_cutoff"])), aes(x=date,ymin = ci_lwr_new, ymax = ci_upr_new), alpha = 0.4) 
			    res<-res+   geom_ribbon(data = data %>% filter(date >= as.Date(dfdata_cutoff[i_reg,"date_cutoff"])), aes(x=date,ymin = ci_lwr_cumul, ymax = ci_upr_cumul), alpha = 0.4) 
			    
			    #res<-res+ geom_line(data = data %>% filter(date >= as.Date(dfdata_cutoff[i_reg,"date_cutoff"])),aes(x=date,y = pred_diff_new))
			    res<-res+ geom_line(data = data %>% filter(date >= as.Date(dfdata_cutoff[i_reg,"date_cutoff"])),aes(x=date,y = pred_diff_cumul))
			}else{
			    res<-res+   geom_ribbon(data = data %>% filter(date >= as.Date(dfdata_cutoff[i_reg,"date_cutoff"])) %>% filter(date <= as.Date(dfdata_cutoff[i_reg,"end_date"])), aes(x=date,ymin = ci_lwr_new, ymax = ci_upr_new), alpha = 0.4) 
			    res<-res+   geom_ribbon(data = data %>% filter(date >= as.Date(dfdata_cutoff[i_reg,"date_cutoff"])) %>% filter(date <= as.Date(dfdata_cutoff[i_reg,"end_date"])), aes(x=date,ymin = ci_lwr_cumul, ymax = ci_upr_cumul), alpha = 0.4) 
			    
			    res<-res+ geom_line(data = data %>% filter(date >= as.Date(dfdata_cutoff[i_reg,"date_cutoff"])) %>% filter(date <= as.Date(dfdata_cutoff[i_reg,"end_date"])),aes(x=date,y = pred_diff_new))
			    res<-res+ geom_line(data = data %>% filter(date >= as.Date(dfdata_cutoff[i_reg,"date_cutoff"])) %>% filter(date <= as.Date(dfdata_cutoff[i_reg,"end_date"])),aes(x=date,y = pred_diff_cumul))
			}
			summ_lm_new<-summary(mod_new)
			summ_lm_cumul<-summary(mod_cumul)
			ci_slope_new<-c(summ_lm_new$coefficients[2,1]-qnorm(0.975)*summ_lm_new$coefficients[2,2],summ_lm_new$coefficients[2,1]+qnorm(0.975)*summ_lm_new$coefficients[2,2])
			print(paste0("Estimated slope and its 95% confidence interval (new): ",format(round(summ_lm_new$coefficients[2,1], 3), nsmall = 3)," (",format(round(ci_slope_new[1], 3), nsmall = 3),",",format(round(ci_slope_new[2], 3), nsmall = 3),")"))
			print(paste0("Half-life in days and its 95% confidence interval (new): ",format(round((-1)*log(2)/summ_lm_new$coefficients[2,1], 3), nsmall = 3)," (",format(round((-1)*log(2)/ci_slope_new[1], 3), nsmall = 3),",",format((-1)*round(log(2)/ci_slope_new[2], 3), nsmall = 3),")"))

			ci_slope_cumul<-c(summ_lm_cumul$coefficients[2,1]-qnorm(0.975)*summ_lm_cumul$coefficients[2,2],summ_lm_cumul$coefficients[2,1]+qnorm(0.975)*summ_lm_cumul$coefficients[2,2])
			print(paste0("Estimated slope and its 95% confidence interval (cumulative): ",format(round(summ_lm_cumul$coefficients[2,1], 3), nsmall = 3)," (",format(round(ci_slope_cumul[1], 3), nsmall = 3),",",format(round(ci_slope_cumul[2], 3), nsmall = 3),")"))
			print(paste0("Doubling time in days and its 95% confidence interval (cumulative): ",format(round(log(2)/summ_lm_cumul$coefficients[2,1], 3), nsmall = 3)," (",format(round(log(2)/ci_slope_cumul[2], 3), nsmall = 3),",",format(round(log(2)/ci_slope_cumul[1], 3), nsmall = 3),")"))
		    }else{
			print(c(NA,NA))
		    }
		    print("=======================================")
		    sink()
		}
	    }
    }    
    if (b_scale_2020deaths){
        if (b_plot_COVIDexcessfrac){
            res<-res+geom_line(aes(x = date, y = excess_death_frac, color = "(IT) COVID frac excess deaths")) 	
    	}
        res<-res+geom_line(aes(x = date, y = weekly_death_frac, color = "(IT) COVID frac weekly deaths")) 	
        res<-res+geom_line(aes(x = date, y = weekly_death_frac_pastavg, color = "(IT) COVID frac weekly deaths wrt past")) 	
        res<-res+geom_line(aes(x = date, y = cumul_death_frac, color = "(IT) COVID frac cumul deaths")) 	
        res<-res+geom_line(aes(x = date, y = cumul_death_frac_pastavg, color = "(IT) COVID frac cumul deaths wrt past")) 	
        res<-res+geom_line(aes(x = date, y = death_2020_ratio, color = "(IT) deaths in 2020 wrt past")) 	
    }
    title <- ifelse(is.na(key$state), key$country, key$state)
    if(b_dolog) title <- paste(title,"(log)")
    res <- res + ggtitle(title) + ylab("") + xlab("") + theme(legend.title = element_blank())
    res
    list(grplot=res,MSE=vMSE, title = title)
}

g <- group_map(it, f1, b_dodaily, b_donumtest, b_dolog,b_onlycumul,b_do_lm_numtest)

g
 dirname <- paste0(dir_prefix,c_ending)
 dir.create(dirname)
 sink(file=sprintf("%s/%s%s.txt",dirname,c_sink_start,c_ending))
 for(i in 1:length(g)){
   name <- group_keys(it)[i,2][[1]]
   cat(name);cat("\n")
   print(g[[i]]$MSE)
   cat("=============================================================");   cat("\n")
   pdf(file = sprintf("%s/%s%s.pdf",dirname,name,c_ending), width = 12)
   plot(g[[i]]$grplot)
   dev.off()
 }
 sink()

# store all plots
gg[[length(gg)+1]] <- g
 
