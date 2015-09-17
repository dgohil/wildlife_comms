
######################################################################
#   Mapping wildlife densities with ggplot2
#
#   last updated by Deepali Gohil: 17 September 2015
#
######################################################################

library(maptools)
library(ggplot2)
library(sp)
library(rgdal)
library(MASS)
library(grid)
library(plyr)
library(XLConnect)
library(data.table)

setwd("~/Documents/NRT/W-Comms/mapping/wildlife_comms")

s1 <- readShapePoly("data/nrt_centre2014.shp") 
str(s1)

s1a <- s1[s1$Name=="Sera"| s1$Name=="Melako"|s1$Name=="Namunyak"| s1$Name=="Meibae"|s1$Name=="West Gate"| s1$Name=="Kalama"|s1$Name=="Mpus Kutuk"| s1$Name=="Naibunga"|s1$Name=="Lekuruki"| s1$Name=="Ilngwesi",]
dum2 <- fortify(s1a,region="Name")

x1 <- read.csv("data/clean_wcomms_2015_06_11.csv")
#str(x1)
x1 <- data.table(x1)
setkey(x1, ccy)
ccy <- sort(unique(x1$ccy))
ccy
yr <- sort(unique(x1$year))
yr
x1_1 <- subset(x1, ccy=)
x2 <- data.frame(x1$UTMX,x1$UTMY,x1$NumIndiv,x1$spp,x1$year, x1$ccy)

names(x2) <- c("X","Y","Z","Species","Year", "Conservancy")

x2$error <- 0
x2$error[is.na(x2$X)==T] <- 1
x2$error[is.na(x2$Y)==T] <- 1
x2$error[x2$X==0] <- 1
x2$error[x2$Y==0] <- 1
length(which(is.na(x2$X)==T |is.na(x2$Y)==T ))
x2 <- x2[x2$error!=1,]

x2a <- x2
coordinates(x2a) <- ~X+Y
proj4string(s1a) <- CRS("+proj=utm +zone=37 ellps=WGS84") 
proj4string(x2a) <- CRS("+proj=utm +zone=37 ellps=WGS84") 

x2b <- over(x2a,s1a)
x2b1 <- data.frame(x2a,x2b$Name)
x2b2 <- x2b1[is.na(x2b1$x2b.Name)==F,]


x3 <- fortify(x2b2)
str(x3)
x4 <- x3
coordinates(x4) <- ~X+Y
proj4string(x4) <- CRS("+proj=utm +zone=37 ellps=WGS84") 

c1 <- vector()
c2 <- vector()
for(i in 1:length(s1a$Name)){
c1[i] <- s1a@polygons[[i]]@labpt[1]
c2[i] <- s1a@polygons[[i]]@labpt[2]
}

cent <- data.frame(c1,c2,s1a$Name)

library(rgeos)

distm <- 1000
x1km <- gBuffer(x4, width=1*distm, byid=T)

x1$Species <- as.factor(x1$Species)
x1$Species <- revalue(x1$Species, c("Bat eared fox" = "Bat Eared Fox",
                            "Bufalo" = "Buffalo",
                            "bush back" = "Bushbuck",
                            "Bush Back" = "Bushbuck",
                            "bush bick" = "Bushbuck",
                            "Bush Buck" = "Bushbuck",
                            "Bushback" ="Bushbuck",
                            "caracal" = "Caracal",
                            "Cheatah" = "Cheetah",
                            "cheetah" = "Cheetah",
                            "clipspringer" = "Klipspringer",
                            "Columbus monkey" = "Colobus Monkey",
                            "common zebra" = "Common Zebra",
                            "Common zebra" = "Common Zebra",
                            "crocodile" = "Crocodile",
                            "Depress Monkey" = "De Brazza Monkey",
                            "eland" = "Eland",
                            "elephant" = "Elephant",
                            "Generuk" = "Gerenuk",
                            "gerenuk" = "Gerenuk",
                            "Grant gazelle" = "Grant's Gazelle",
                            "Grant Gazelle" = "Grant's Gazelle",
                            "Grant's gazelle" = "Grant's Gazelle",
                            "greater kudu" = "Greater Kudu",
                            "Greater kudu" = "Greater Kudu",
                            "Greater  Kudu" = "Greater Kudu",
                            "Grevy zebra" = "Grevy's Zebra",
                            "Grevy Zebra" = "Grevy's Zebra",
                            "Hippo" = "Hippopotamus",
                            "Hippotamus" = "Hippopotamus",
                            "Hyaena" = "Spotted Hyena",
                            "Hyena" = "Spotted Hyena",
                            "jackal" = "Jackal",
                            "klipsringer" = "Klipspringer",
                            "leopard" = "Leopard",
                            "lesser kudu" = "Lesser Kudu",
                            "Lesser kudu" = "Lesser Kudu",
                            "lion" = "Lion",
                            "LIon" = "Lion",
                            "oryx" = "Oryx",
                            "plain zebra" = "Plains Zebra",
                            "Plain zebra" = "Plains Zebra",
                            "Plain Zebra" = "Plains Zebra",
                            "potted Hyena" = "Spotted Hyena",
                            "snake" = "Snake",
                            "Spotted Hyaena" = "Spotted Hyena",
                            "Spotted hyaena" = "Spotted Hyena",
                            "Striped Hyaena" = "Striped Hyena",
                            "striped hyeana" = "Striped Hyena",
                            "Stripped Hyaena" = "Striped Hyena",
                            "Stripped Hyena" = "Striped Hyena",
                            "stripped haena" = "Striped Hyena",
                            "stripped hyaena" = "Striped Hyena",
                            "stripped hyaena" = "Striped Hyena",
                            "Waterbuck " = "Waterbuck",
                            "Thompsons Gazelle" = "Thomson's Gazelle",
                            "Thomson Gazelle" = "Thomson's Gazelle",
                            "warthog" = "Warthog",
                            "water buck" = "Waterbuck",
                            "Water Buck" = "Waterbuck",
                            "waterbuck" = "Waterbuck",
                            "Waterbuck" = "Waterbuck",
                            "Wildog" = "Wild Dog"))

spp <- sort(unique(x1$Species))
spp

spp1 <- spp[c(11,12,14,18,25,26,38)]
spp1
n1 <- c(237,204)
n1



for ( i in 1: length(spp1)){
  x5 <- x3[x3$Species==spp1[i],]
  x5$z1 <- x5$Z/max(x5$Z)
  dens <- kde2d(x5$X,x5$Y)
  brks <- seq(min(dens$z),max(dens$z),length.out=3)
  brks1 <- round(brks/max(brks),2)
  
p1 <- ggplot() +
  geom_polygon(data=dum2, aes(x =long, y =lat,group = group),color="grey40",fill="grey90") +
  scale_x_continuous(limits=c(220000,457000)) + 
  scale_y_continuous(limits=c(25000,229000)) +
  coord_equal() +
  geom_point(data=x5,aes(x=X,y=Y,z=Z),color="red",shape=20,cex=1) +
  stat_density2d(data=x5, aes(x=X,y=Y,z=z1,fill=..level..,alpha=..level..),
                 size=2,bins=10,geom="polygon",n=100)  +
  scale_fill_gradient(low = "yellow", high = "red",breaks=brks,labels=c("","","")) +
  geom_text(data=cent,aes(x=c1,y=c2,label=s1a.Name),color="black",size=2) +
  geom_text(aes(x=280000,y=200000,label=spp1[i]),color="black",size=5) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(legend.position="none") +
  geom_path(data=dum2, aes(x =long, y =lat,group = group),color="black") +
  guides(alpha=FALSE)
print(p1)
ggsave(filename=paste("Species Sightings 2010-2014 ",i,".tiff",sep=""), plot=p1)

}


# for the last plot - add legend

i=7
x5 <- x3[x3$Species==spp1[i],]
x5$z1 <- x5$Z/max(x5$Z)
dens <- kde2d(x5$X,x5$Y)
brks <- seq(min(dens$z),max(dens$z),length.out=3)
brks1 <- round(brks/max(brks),2)

p1 <- ggplot() +
  geom_polygon(data=dum2, aes(x =long, y =lat,group = group),color="grey40",fill="grey90") +
  scale_x_continuous(limits=c(220000,457000)) + 
  scale_y_continuous(limits=c(25000,229000)) +
  coord_equal() +
  geom_point(data=x5,aes(x=X,y=Y,z=Z),color="red",shape=20,cex=1) +
  stat_density2d(data=x5, aes(x=X,y=Y,z=z1,fill=..level..,alpha=..level..),
                 size=2,bins=10,geom="polygon",n=100)  +
  scale_fill_gradient(low = "yellow", high = "red",breaks=brks,labels=c("","","")) +
  geom_text(data=cent,aes(x=c1,y=c2,label=s1a.Name),color="black",size=2) +
  geom_text(aes(x=280000,y=200000,label=spp1[i]),color="black",size=5) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(legend.justification=c(1,0), legend.position=c(.85,0),
        legend.title=element_blank()) +
  theme(legend.key.size = unit(0.5,"cm"))+
  geom_path(data=dum2, aes(x =long, y =lat,group = group),color="black") +
  guides(alpha=FALSE)
print(p1)
ggsave(filename=paste("Species Sightings 2010-2014 ",i,".tiff",sep=""), plot=p1)

