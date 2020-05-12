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

require(dplyr)
require(ggplot2)
require(tidyverse)
require(hrbrthemes)
require(lubridate)
require(COVID19) 


data_study_date_start<-"2020-02-24"
data_study_date_end<-"2020-04-20"


data_study_date_start<-"2020-02-24" ## date from which we start downloading the data via COVID
data_study_date_end<-"2020-04-20" ## date until which we download the data via COVID

it_object_RData_file_toload_prefix<-NA ## set to NA if you want to rerun the analysis completely including the downloading of the data via COVID, otherwise it is the prefix of the .RData file where the data prepared for plotting by the scripts is stored
it_object_RData_file_tosave_prefix<-"it_object_2020422" ## prefix of files to save data prepared for plotting.
pop_2020_end_date<-"2020-04-04" ## when do the deceased data in the file "comuni_settimana.csv" end

if (!is.na(data_study_date_end)){
    pop_date_till<-as.Date(data_study_date_end)
}else{
    pop_date_till<-today()
}
dir_prefix<-"it" ## this is the prefix of the directory that will be great where the output graphs will be saved in
gg <- list() ## store plots

b_visualize_Italy<-FALSE
b_scale_2020deaths_tmp<-TRUE   ## do we want to do scaling with the 2020 weekly deaths
b_plot_COVIDexcessfrac_tmp<-TRUE ## do we plot the excess death information
b_plot_daily_tests<-FALSE ## at the moment this does not have an effect on the file naming, so as not to change the LaTeX
b_do_lm_numtest<-TRUE

## List of various plot setups
lsetups<-list(
## Visualization of positive fraction of tests
list(index_setup=1,b_onlycumul=FALSE,b_regions=TRUE,b_dolog=TRUE,b_dodaily=FALSE,b_donumtest=TRUE,b_scale_2020deaths=FALSE,b_plot_COVIDexcessfrac=FALSE,b_do_lm_numtest=b_do_lm_numtest),
list(index_setup=2,b_onlycumul=FALSE,b_regions=FALSE,b_dolog=TRUE,b_dodaily=FALSE,b_donumtest=TRUE,b_scale_2020deaths=FALSE,b_plot_COVIDexcessfrac=FALSE,b_do_lm_numtest=b_do_lm_numtest),
list(index_setup=3,b_onlycumul=FALSE,b_regions=TRUE,b_dolog=FALSE,b_dodaily=FALSE,b_donumtest=TRUE,b_scale_2020deaths=FALSE,b_plot_COVIDexcessfrac=FALSE,b_do_lm_numtest=FALSE),
list(index_setup=4,b_onlycumul=FALSE,b_regions=FALSE,b_dolog=FALSE,b_dodaily=FALSE,b_donumtest=TRUE,b_scale_2020deaths=FALSE,b_plot_COVIDexcessfrac=FALSE,b_do_lm_numtest=FALSE),

## Visualization of scaled deaths and cumulative fraction of positive tests
list(index_setup=5,b_onlycumul=FALSE,b_regions=TRUE,b_dolog=TRUE,b_dodaily=b_plot_daily_tests,b_donumtest=FALSE,b_scale_2020deaths=b_scale_2020deaths_tmp,b_plot_COVIDexcessfrac=b_plot_COVIDexcessfrac_tmp,b_do_lm_numtest=FALSE),
list(index_setup=6,b_onlycumul=FALSE,b_regions=FALSE,b_dolog=TRUE,b_dodaily=b_plot_daily_tests,b_donumtest=FALSE,b_scale_2020deaths=b_scale_2020deaths_tmp,b_plot_COVIDexcessfrac=b_plot_COVIDexcessfrac_tmp,b_do_lm_numtest=FALSE),
list(index_setup=7,b_onlycumul=FALSE,b_regions=TRUE,b_dolog=FALSE,b_dodaily=b_plot_daily_tests,b_donumtest=FALSE,b_scale_2020deaths=b_scale_2020deaths_tmp,b_plot_COVIDexcessfrac=b_plot_COVIDexcessfrac_tmp,b_do_lm_numtest=FALSE),
list(index_setup=8,b_onlycumul=FALSE,b_regions=FALSE,b_dolog=FALSE,b_dodaily=b_plot_daily_tests,b_donumtest=FALSE,b_scale_2020deaths=b_scale_2020deaths_tmp,b_plot_COVIDexcessfrac=b_plot_COVIDexcessfrac_tmp,b_do_lm_numtest=FALSE),

## Visualization of  only cumulative
list(index_setup=9,b_onlycumul=TRUE,b_regions=TRUE,b_dolog=TRUE,b_dodaily=FALSE,b_donumtest=FALSE,b_scale_2020deaths=TRUE,b_plot_COVIDexcessfrac=FALSE,b_do_lm_numtest=FALSE),
list(index_setup=10,b_onlycumul=TRUE,b_regions=FALSE,b_dolog=TRUE,b_dodaily=FALSE,b_donumtest=FALSE,b_scale_2020deaths=TRUE,b_plot_COVIDexcessfrac=FALSE,b_do_lm_numtest=FALSE),
list(index_setup=11,b_onlycumul=TRUE,b_regions=TRUE,b_dolog=FALSE,b_dodaily=FALSE,b_donumtest=FALSE,b_scale_2020deaths=TRUE,b_plot_COVIDexcessfrac=FALSE,b_do_lm_numtest=FALSE),
list(index_setup=12,b_onlycumul=TRUE,b_regions=FALSE,b_dolog=FALSE,b_dodaily=FALSE,b_donumtest=FALSE,b_scale_2020deaths=TRUE,b_plot_COVIDexcessfrac=FALSE,b_do_lm_numtest=FALSE)
)

if (!is.na(it_object_RData_file_tosave_prefix)){
    it_objects<-vector("list",length(lsetups))
}
for (setup in lsetups){
## Control variables what to plot
    b_onlycumul<-setup$b_onlycumul ## do we only plot the cumulative tested curve
    b_regions<-setup$b_regions ## regions or whole of Italy
    b_dolog<-setup$b_dolog ## log or real scale
    b_dodaily<-setup$b_dodaily ## do we plot the daily positive tested fraction, very noisy
    b_donumtest<-setup$b_donumtest ## plot of the number of postive tests versus number of tests
    b_scale_2020deaths<-setup$b_scale_2020deaths
    b_plot_COVIDexcessfrac<-setup$b_plot_COVIDexcessfrac
    b_do_lm_numtest<-setup$b_do_lm_numtest
    
    if (is.na(it_object_RData_file_toload_prefix)){
	source("prep_it_object.R")
    }else{
	load(paste0(it_object_RData_file_toload_prefix,"_setup_",setup$index_setup,".RData"))
    }
    if (!is.na(it_object_RData_file_tosave_prefix)){
	save.image(file=paste0(it_object_RData_file_tosave_prefix,"_setup_",setup$index_setup,".RData"))
    }
    source("create_plots.R")  
}

source("do_plots.R")
source("ITAgeMortality.R")
