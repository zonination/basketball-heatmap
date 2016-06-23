# Set working directory, load dataset, load libraries
setwd("~/Dropbox/R/Elo Project")
games <- read.csv("games2016.csv", stringsAsFactors=F)
games <- rbind(games,read.csv("games2015.csv", stringsAsFactors=F))
games <- rbind(games,read.csv("games2014.csv", stringsAsFactors=F))
games <- rbind(games,read.csv("games2013.csv", stringsAsFactors=F))
games <- rbind(games,read.csv("games2012.csv", stringsAsFactors=F))
library(ggplot2)
library(reshape2)
library(scales)
library(stringr)

# Declare our variables
games$v.name<-word(games$v.name,-1)
games$h.name<-word(games$h.name,-1)
n<-length(unique(c(games$v.name,games$h.name)))
score<-as.data.frame(matrix(0,ncol=n,nrow=n))
total<-as.data.frame(matrix(0,ncol=n,nrow=n))
names(score)<-unique(c(games$v.name,games$h.name))
names(total)<-unique(c(games$v.name,games$h.name))
row.names(score)<-names(score)
row.names(total)<-names(total)

# Main Loop
for(n in 1:nrow(games)){
  # Tally up the wins for each team
  if(games$v.score[n]>games$h.score[n]){
    eval(parse(text=paste(
      "score[\"",games$v.name[n],"\",\"",games$h.name[n],"\"]<-",
      "score[\"",games$v.name[n],"\",\"",games$h.name[n],"\"]+1",
      sep="")))
  } else {
    eval(parse(text=paste(
      "score[\"",games$h.name[n],"\",\"",games$v.name[n],"\"]<-",
      "score[\"",games$h.name[n],"\",\"",games$v.name[n],"\"]+1",
      sep="")))
  }
  # Tally up the total number of games, so we can get a % going
  eval(parse(text=paste(
    "total[\"",games$v.name[n],"\",\"",games$h.name[n],"\"]<-",
    "total[\"",games$v.name[n],"\",\"",games$h.name[n],"\"]+1",
    sep="")))
  eval(parse(text=paste(
    "total[\"",games$h.name[n],"\",\"",games$v.name[n],"\"]<-",
    "total[\"",games$h.name[n],"\",\"",games$v.name[n],"\"]+1",
    sep="")))
}

# Let's normalize to %, then compile all data into a formatted list
total[total==0]<-NA
df<-score/(total)
df$challenger<-row.names(score)
df<-melt(df,id="challenger")
total$challenger<-row.names(total)
df$ngames<-melt(total,id="challenger")$value
# Get our set ordered out
df$challenger<-factor(df$challenger)
df$variable <- factor(df$variable, levels =
                        levels(df$variable)[order(levels(df$variable))])
df$challenger <- factor(df$challenger, levels =
                        levels(df$challenger)[order(levels(df$challenger),decreasing=T)])
df<-subset(df,!is.na(df$value))

# Plot our heatmap
ggplot(df,aes(y=challenger,x=variable),na.rm=T)+
  geom_tile(aes(fill=value),color="white")+
  geom_text(aes(label=percent(round(value,2))),size=2.5)+
  scale_fill_distiller("Win Rate",palette="BuGn",direction=1,labels=scales::percent)+
  scale_alpha(guide="none")+
  theme_bw()+
  ggtitle("Game Win Rate since 2012")+
  ylab("Challenger")+
  xlab("Defender")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=.5))