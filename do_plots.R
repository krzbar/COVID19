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

library(ggpubr)
library(gridExtra)
library(ggthemes)

theme_set(theme_gray(base_size = 20))

######################## plot function

doplots <- function(i1, i2, dir, manual_scale = FALSE){

  if(!dir.exists(dir)) 
    dir.create(dir)
  
  for(i in 1:length(gg[[i1]])){
    
    mapcol <- c(
      "(IT) Confirmed" = "red", 
      '(IT) Tests' = "blue",
      '(IT) Confirmed - Tests' = "darkgreen", 
      '(IT) Confirmed - Tests cumulative' = "green")
    
    g1 <- gg[[i1]][[i]]$grplot + theme(legend.position="bottom")
    g2 <- gg[[i2]][[i]]$grplot 
    
    # g1 <- g1 + theme_cleveland()
    # g2 <- g2 + theme_cleveland()
    
    if(manual_scale){
      g1 <- g1 + scale_color_manual(values = mapcol)
      g2 <- g2 + scale_color_manual(values = mapcol)
    }
    
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)
    }
    
    mylegend <- g_legend(g1)
    
    if (is.element("filename",names(gg[[i2]][[i]]))){
	file <- gg[[i2]][[i]]$filename
    }else{
	file <- make.names(gg[[i2]][[i]]$title)
    }
    if (is.element("dirname",names(gg[[i2]][[i]]))){
	dir <- gg[[i2]][[i]]$dirname
    }
    
    pdf(file = sprintf("%s/%s.pdf", dir, file), width = 15)
    
    grid.arrange(arrangeGrob(g2 + theme(legend.position="none"),
                             g1 + theme(legend.position="none"),
                             nrow=1),
                 mylegend, nrow=2, heights=c(10, 1))
    
    dev.off()
    
  }
  
}

doplots(1, 3, 'g1', manual_scale = TRUE)
doplots(5, 7, 'g2')
doplots(9, 11, 'g3')

doplots(2, 4, 'g4', manual_scale = TRUE)
doplots(6, 8, 'g5')
doplots(10, 12, 'g6')



############################## ALL

x <- covid19('ITA', 2, vintage = TRUE, start = data_study_date_start, end = data_study_date_end)

x$frac_confirmed <- x$confirmed/x$tests
x$frac_confirmed[is.infinite(x$frac_confirmed)] <- NA

x$frac_deaths    <- x$deaths/x$confirmed
x$frac_deaths[is.infinite(x$frac_deaths)] <- NA

g1 <- ggplot(x, aes(x = date)) + 
  geom_line(aes(y = frac_confirmed, group = id, color = administrative_area_level_2)) +
  ggtitle("Confirmed / Tested") +
  ylab("") + xlab("") + 
  theme(legend.title = element_blank())

g2 <- ggplot(x, aes(x = date)) + 
  geom_line(aes(y = frac_deaths, group = id, color = administrative_area_level_2)) +
  ggtitle("Deaths / Confirmed") +
  ylab("") + xlab("") 
  theme(legend.title = element_blank())

if(!dir.exists('g0')) dir.create('g0')
ggarrange(g1, g2, ncol = 2, common.legend = TRUE, legend = "bottom") %>%
  ggexport(filename = 'g0/all.pdf', width = 15)


