## Authors Krzysztof Bartoszek, Emanuele Guidotti, Stefano Iacus, Marcin Okroj
## License GNU GPL v3.0
## This software comes AS IS in the hope that it will be useful WITHOUT ANY WARRANTY, 
## NOT even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## Please understand that there may still be bugs and errors. Use it at your own risk. 
## We take no responsibility for any errors or omissions in this package or for any misfortune 
## that may befall you or others as a result of its use. Please send comments and report 
## bugs to Krzysztof Bartoszek at krzbar@protonmail.ch .

require(dplyr)
require(ggplot2)
require(COVID19) ##https://github.com/emanuele-guidotti/COVID19

b_onlycumul<-FALSE

b_regions<-TRUE
b_dolog<-TRUE

b_dodaily<-TRUE
b_donumtest<-FALSE

b_visualize_Italy<-FALSE
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
dp <- diamond() 
dp <- subset(dp, confirmed>0)
if (b_dolog){
    dp$confirmed_scaled <- log(dp$confirmed/(dp$pop))
}else{
    dp$confirmed_scaled <- (dp$confirmed/(dp$pop))
}

if (b_regions){
    it <- italy("state")
}else{it <- italy("country")}
if (b_onlycumul){it <- subset(it, confirmed>0)}else{it <- subset(it, deaths>0)}

{
    if (!b_donumtest){
	if (b_dolog){
	    if (b_dodaily){
		it <- it %>% 
		    mutate(confirmed_scaled     = log(confirmed/tests),
        		confirmed_new_scaled = log(confirmed_new/tests_new),
        		deaths_scaled        = log(deaths/(1:length(deaths)*pop_death_rate*pop/365)))
    	    }else{
		it <- it %>% 
		    mutate(confirmed_scaled     = log(confirmed/tests),
        		deaths_scaled        = log(deaths/(1:length(deaths)*pop_death_rate*pop/365))
        		)
    	    }
	}else{
	    if (b_dodaily){
		it <- it %>% 
		     mutate(confirmed_scaled     = (confirmed/tests),
    			confirmed_new_scaled = (confirmed_new/tests_new),
    			deaths_scaled        = (deaths/(1:length(deaths)*pop_death_rate*pop/365)))
    	    }else{
    		it <- it %>% 
		     mutate(confirmed_scaled     = (confirmed/tests),
    			deaths_scaled        = (deaths/(1:length(deaths)*pop_death_rate*pop/365)))
    	    }
    }
    }else{
	if (b_dolog){
	    it <- it %>% 
		mutate(confirmed_new_gr = log(confirmed_new),
    		    tests_new_gr = log(tests_new),
    		    diff_confirmed_deaths= log(tests_new)-log(confirmed_new)
        	)
	}else{
	    it <- it %>% 
		mutate(confirmed_new_gr = (confirmed_new),
    		    tests_new_gr = (tests_new)	
        	)
	}
    }
}
f1 <- function(data, key, b_dodaily, b_donumtest, b_dolog,b_onlycumul){
    res<-ggplot(data = data) 
    vMSE<-c()
    if (! b_donumtest){
	res<-res+geom_line(aes(x = date, y = confirmed_scaled, color = '(IT) Confirmed/Tests')) 
	vMSE<-c(vMSE,confirmed_scaled_all=sqrt(mean((data$confirmed_scaled-dp$confirmed_scaled[1:nrow(data)])^2,na.rm=TRUE)))
	vMSE<-c(vMSE,confirmed_scaled_dp=sqrt(mean((data$confirmed_scaled[1:min(nrow(data),length(dp$confirmed_scaled))]-dp$confirmed_scaled[1:min(nrow(data),length(dp$confirmed_scaled))])^2,na.rm=TRUE)))
	    if (!b_onlycumul){
		res<-res+geom_line(aes(x = date, y = deaths_scaled, color = "(IT) Deaths Scaled"))
		vMSE<-c(vMSE,deaths_scaled=sqrt(mean((data$deaths_scaled-dp$confirmed_scaled[1:nrow(data)])^2,na.rm=TRUE)))
	    	vMSE<-c(vMSE,deaths_scaled_dp=sqrt(mean((data$deaths_scaled[1:min(nrow(data),length(dp$confirmed_scaled))]-dp$confirmed_scaled[1:min(nrow(data),length(dp$confirmed_scaled))])^2,na.rm=TRUE)))
	    }
	    if (b_dodaily){
		res<-res+geom_line(aes(x = date, y = confirmed_new_scaled, color = '(IT) Daily Confirmed/Tests'))
		vMSE<-c(vMSE,confirmed_new_scaled=sqrt(mean((data$confirmed_new_scaled-dp$confirmed_scaled[1:nrow(data)])^2,na.rm=TRUE)))
	    	vMSE<-c(vMSE,confirmed_new_scaled_dp=sqrt(mean((data$confirmed_new_scaled[1:min(nrow(data),length(dp$confirmed_scaled))]-dp$confirmed_scaled[1:min(nrow(data),length(dp$confirmed_scaled))])^2,na.rm=TRUE)))
	    }
	res<-res+geom_line(aes(x = date, y = dp$confirmed_scaled[1:nrow(data)], color = '(DP) Confirmed Scaled')) 
    }else{
    	    res<-res+geom_line(aes(x = date, y = confirmed_new_gr, color = "(IT) Confirmed")) +
		geom_line(aes(x = date, y = tests_new_gr, color = '(IT) Tests')) 
	    if (b_dolog){res<-res+geom_line(aes(x = date, y = diff_confirmed_deaths, color = '(IT) Confirmed - Tests'))}
    }    
    res<- res+ labs(title = key, ylab = "value")
    res
    list(grplot=res,MSE=vMSE)
}

g <- group_map(it, f1, b_dodaily, b_donumtest, b_dolog,b_onlycumul)
g

 dirname <- paste0("it",c_ending)
 dir.create(dirname)
 sink(file=sprintf("%s/%s%s.txt",dirname,c_sink_start,c_ending))
 for(i in 1:length(g)){
   name <- strsplit(as.character(group_keys(it)[i,1]), split = "|", fixed = T)[[1]][2]
   cat(name);cat("\n")
   print(g[[i]]$MSE)
   cat("=============================================================");   cat("\n")
   pdf(file = sprintf("%s/%s%s.pdf",dirname,name,c_ending), width = 12)
   plot(g[[i]]$grplot)
   dev.off()
 }
 sink()


if (b_visualize_Italy){
# data visualization
 geomap(it, 
        map          = "italy", 
        value        = "deaths_scaled",
        caption      = "Data source: Ministero della Salute, Dipartimento della Protezione Civile, Italia",
        legend.title = "Log Scaled Deaths",
        filename     = "scaled-deaths.gif",
        end_pause    = 30)
}
