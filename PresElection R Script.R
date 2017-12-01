#Data on the Rocks
#Global Warming


setwd("C:/Users/sbuja/Documents/Data on the Rocks/Presidential Elections")

#REQUIRED PACKAGES
library(xlsx) #package to import xls files directly
library(pastecs) #summary stats function stat.desc
library(ggplot2)
library(scales) #for percent axis
library(plyr) #data wrangling
library(dplyr) #data wrangling 2
library(tidyr) #data wrangling 3
library(lubridate) #data manipulation with date

#CUSTOM FUNCTIONS
Sp.Desc <- function(data)
{
  data.num <- data.frame(Dummy=rep(NA,dim(data)[1])) #Make a dummy data.frame
  data.cat <- data.frame(Dummy=rep(NA,dim(data)[1]))
  #separate categorical from numerical data
  for(i in 1:dim(data)[2]){
    if(!is.na(stat.desc(data[i])["mean",])){#if R can compute a mean then add to data.num
      data.num <- cbind(data.num, data[i])
    }
    else{
      data.cat <- cbind(data.cat, data[i])
    }
  }
  #Delete dummy variable
  data.num$Dummy <- NULL
  data.cat$Dummy <- NULL
  
  #Print Numerical results
  if(dim(data.num)[2]>0) {
    print(t(stat.desc(data.num))[,-c(2,3,6,7,11,14)])
    cat(noquote(""), sep="\n\n")
  }
  
  #Print categorical results
  if(dim(data.cat)[2]>0) {
    for(j in 1:dim(data.cat)[2]){
      cat(noquote(names(data.cat[j])))
      print(table(data.cat[j]))
    }
  }
}

DoR.Theme <- function(axis.text.size=16, axis.title.size=16, title.size=20, legend.position="none")
{
  theme(panel.grid.major = element_line(colour="grey90"), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x=element_line(colour="black"), axis.line.y=element_line(colour="black"),
        axis.title.x=element_text(colour = "black", size=axis.title.size), axis.title.y=element_text(colour = "black", size=axis.title.size),
        axis.text.x=element_text(colour = "black", size=axis.text.size), axis.text.y=element_text(colour = "black", size=axis.text.size),
        plot.title=element_text(colour = "black",size=title.size, face="bold", hjust=.5),
        axis.ticks=element_line(colour="black"), legend.position = legend.position, legend.key=element_blank())
}



#DATA IMPORT----
Pres <- read.xlsx("C:/Users/sbuja/Documents/Data on the Rocks/Presidential Elections/PresElection Data.xlsx", sheetName="Sheet1")
Sp.Desc(Pres)

Pres2 <- Pres
Pres2$Year <- Pres2$Year + 3.99

Pres3 <- rbind(Pres, Pres2)

Pres3 <- Pres3[order(Pres3$Year),] 

#calculate winning share
Pres$P.Win <- ifelse(Pres$Won=="R",Pres$p.Rep,Pres$p.Dem)
stat.desc(Pres$P.Win)
stat.desc(Pres$p.NoVote)

#GRAPHING DATA----
colours <- c("#d3d3d3", "#edef56", "#e22424", "#1a2ccc") #NoVote = grey, Third Party = Yellow, Republican = Red, Democrat = Blue

gap <- 0.002

Pres.Plot <- ggplot(Pres3, aes(x=Year)) +
  #ribbons of the vote shares
  geom_ribbon(aes(ymin=No.Min, ymax=No.Max-gap), fill=colours[1]) +
  geom_ribbon(aes(ymin=Third.Min+gap, ymax=Third.Max-gap), fill=colours[2]) +
  geom_ribbon(aes(ymin=Rep.Min+gap, ymax=Rep.Max-gap), fill=colours[3]) +
  geom_ribbon(aes(ymin=Dem.Min+gap, ymax=Dem.Max-gap), fill=colours[4]) +
  #white lines to break up the elections
  geom_vline(xintercept=seq(1916,2016,4), colour="white") + 
  #average winning VAP
  #geom_hline(yintercept = mean(Pres$P.Win), colour="black", size=2) +
  #annotate("text", label="Average Winning Percentage", 
  #         x=1921, y=mean(Pres$P.Win)-0.02, colour="black", size=6, fontface="bold", hjust=0, vjust=1) + 
  #legend construction
  annotate("rect", fill="white", xmin=1989.75, xmax=2014, ymin=0.05, ymax=0.22) + 
  annotate("text", label="Didn't Vote", x=1992.5, y=0.06, colour="#adadad", size=6, fontface="bold", hjust=0, vjust=0) + 
  annotate("text", label="Third Party", x=1992.5, y=0.10, colour=colours[2], size=6, fontface="bold", hjust=0, vjust=0) + 
  annotate("text", label="Republican", x=1992.5, y=0.14, colour=colours[3], size=6, fontface="bold", hjust=0, vjust=0) + 
  annotate("text", label="Democratic", x=1992.5, y=0.18, colour=colours[4], size=6, fontface="bold", hjust=0, vjust=0) + 
  #astrices for non-pop vote winners
  #annotate("text", label="*", x=2018, y=0.9, colour="white", size=6, fontface="bold", hjust=0.5, vjust=0) + 
  #annotate("text", label="*", x=2002, y=0.9, colour="white", size=6, fontface="bold", hjust=0.5, vjust=0) + 
  #plot details
  scale_x_continuous("Year", limits=c(1916,2020), breaks=c(seq(1920,2016,20),2016), expand = c(0,0)) +
  scale_y_continuous("Voting Age Population", limits=c(0,1), breaks=seq(0,1,0.2), expand = c(0,0), labels=percent) +
  ggtitle("100 Years of Presidential Elections") +
  DoR.Theme()
Pres.Plot

ggsave(Pres.Plot, filename="Pres.Plot.png", width = 8, height=7, dpi=500)


Pres.Plot.Long <- ggplot(Pres3, aes(x=Year)) +
  #ribbons of the vote shares
  geom_ribbon(aes(ymin=No.Min, ymax=No.Max-gap), fill=colours[1]) +
  geom_ribbon(aes(ymin=Third.Min+gap, ymax=Third.Max-gap), fill=colours[2]) +
  geom_ribbon(aes(ymin=Rep.Min+gap, ymax=Rep.Max-gap), fill=colours[3]) +
  geom_ribbon(aes(ymin=Dem.Min+gap, ymax=Dem.Max-gap), fill=colours[4]) +
  #white lines to break up the elections
  geom_vline(xintercept=seq(1916,2016,4), colour="white") + 
  #average winning VAP
  geom_hline(yintercept = mean(Pres$P.Win), colour="black", size=2) +
  annotate("text", label="Average Winning Percentage", 
           x=1921, y=mean(Pres$P.Win)-0.02, colour="black", size=6, fontface="bold", hjust=0, vjust=1) + 
  #legend construction
  annotate("rect", fill="white", xmin=1989.75, xmax=2014, ymin=0.05, ymax=0.22) + 
  annotate("text", label="Didn't Vote", x=1992.5, y=0.06, colour="#adadad", size=6, fontface="bold", hjust=0, vjust=0) + 
  annotate("text", label="Third Party", x=1992.5, y=0.10, colour=colours[2], size=6, fontface="bold", hjust=0, vjust=0) + 
  annotate("text", label="Republican", x=1992.5, y=0.14, colour=colours[3], size=6, fontface="bold", hjust=0, vjust=0) + 
  annotate("text", label="Democratic", x=1992.5, y=0.18, colour=colours[4], size=6, fontface="bold", hjust=0, vjust=0) + 
  #astrices for non-pop vote winners
  annotate("text", label="*", x=2018, y=0.9, colour="white", size=6, fontface="bold", hjust=0.5, vjust=0) + 
  annotate("text", label="*", x=2002, y=0.9, colour="white", size=6, fontface="bold", hjust=0.5, vjust=0) + 
  #plot details
  scale_x_continuous("Year", limits=c(1916,2020), breaks=c(seq(1920,2016,20),2016), expand = c(0,0)) +
  scale_y_continuous("Voting Age Population", limits=c(0,1), breaks=seq(0,1,0.2), expand = c(0,0), labels=percent) +
  #ggtitle("The Last 100 Years of Presidential Elections") +
  DoR.Theme()
Pres.Plot.Long

ggsave(Pres.Plot.Long, filename="Pres.Plot.Long.png", width = 12, height=5, dpi=500)




