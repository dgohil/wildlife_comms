


##########################################################################
# wildlife sightings trends

library(grid)

spp <- read.csv("keysp10_14.csv", header=T)
s1 <- unique(spp$sp)


# regular sightings
# sightings displayed as 100's of sightings to make the y axis more clear

windows(6,4,record=T)

for(i in 1:length(s1)){
  ssp1 <- spp[spp$sp==s1[i],]
  ssp1$sum100 <- round(ssp1$sum/100,0)
  #ssp1$stan <- ssp1$sum
  #ssp1$stan <- (ssp1$sum-mean(ssp1$sum))/sd(ssp1$sum)
  #ssp1$stan <- (ssp1$sum-min(ssp1$sum))/(max(ssp1$sum)-min(ssp1$sum))
  
  my_grob = grobTree(textGrob(label=ssp1$sp, x=0.8,  y=0.9, hjust=0,
                              gp=gpar(col="grey30", fontsize=12, fontface="bold")))
  
  p11 <- ggplot(ssp1,aes(x = yr, y = sum100)) + 
    geom_bar(aes(x=yr, y=sum100),fill=nrt_red,stat="identity",bg="white",width=.6) +
    #geom_text(aes(x=year, y=stan, label=(paste(perc, "%")),parse = TRUE, vjust=-1),size=4) +
    #ylab("Sightings (100's)") +
    annotation_custom(my_grob) +
    theme_bw() + 
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "grey",lineend="butt",size=2)) +
    theme(axis.title.x = element_blank()) +   # Remove x-axis label
    theme(axis.title.y = element_blank()) +   # Remove x-axis label
    theme(axis.text=element_text(family="", size=10, face="plain")) +
    #scale_y_continuous(limits=c(0,100),label=function(x){return(paste( x, "%"))})+
    stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 2,se=F,colour=nrt_blue)
  print(p11)
  ggsave(paste("C:\\Worden\\Dropbox (Personal)\\State_of_conservancies\\soc_final_figures_April27\\wildlife_trends2\\",s1[i],"_27Apr2015.eps"))
  
}

