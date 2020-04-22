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
    
    file <- make.names(gg[[i2]][[i]]$title)
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

x <- covid19('ITA', 2, vintage = TRUE, start = '2020-02-24', end = '2020-04-20')

x$frac_confirmed <- x$confirmed/x$tests
x$frac_confirmed[is.infinite(x$frac_confirmed)] <- NA

x$frac_deaths    <- x$deaths/x$confirmed
x$frac_deaths[is.infinite(x$frac_deaths)] <- NA

g1 <- ggplot(x, aes(x = date)) + 
  geom_line(aes(y = frac_confirmed, group = id, color = state)) +
  ggtitle("Confirmed / Tested") +
  ylab("") + xlab("") + 
  theme(legend.title = element_blank())

g2 <- ggplot(x, aes(x = date)) + 
  geom_line(aes(y = frac_deaths, group = id, color = state)) +
  ggtitle("Deaths / Confirmed") +
  ylab("") + xlab("") 
  theme(legend.title = element_blank())

if(!dir.exists('g0')) dir.create('g0')
ggarrange(g1, g2, ncol = 2, common.legend = TRUE, legend = "bottom") %>%
  ggexport(filename = 'g0/all.pdf', width = 15)


