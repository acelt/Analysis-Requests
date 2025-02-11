#import data
data<- read.csv("X:/Botany/AIM/1. Taos/Data Analysis/T&NT.csv")
attach(data)
names(data)
print(data)

#create subsets for each indicator
##Bare Soil
bg<-subset(data,Indicator=="Bare Soil")
##Perennial Grass
pg<-subset(data,Indicator=="Graminoid (Perennial)")

#convert values to percentages
bg$percent<-bg$AnyHitAvg*100
pg$percent<-pg$AnyHitAvg*100

#Extract ecological sites
loamy.bg<-subset(bg,bg$Ecol_Site=="R036XB006NM")
malpais.bg<-subset(bg,bg$Ecol_Site=="R036XB007NM")
mtn.bg<-subset(bg,bg$Ecol_Site=="R048AY005NM")

loamy.pg<-subset(pg,pg$Ecol_Site=="R036XB006NM")
malpais.pg<-subset(pg,pg$Ecol_Site=="R036XB007NM")
mtn.pg<-subset(pg,pg$Ecol_Site=="R048AY005NM")

#plot treated vs untreated
qplot(loamy.bg$site,loamy.bg$percent,geom="boxplot")
qplot(malpais.bg$site,malpais.bg$percent,geom="boxplot")
qplot(mtn.bg$site,mtn.bg$percent,geom="boxplot")

qplot(loamy.pg$site,loamy.pg$percent,geom="boxplot")
qplot(malpais.pg$site,malpais.pg$percent,geom="boxplot")
qplot(mtn.pg$site,mtn.pg$percent,geom="boxplot")


#add color and labels and y axis limits
qplot(loamy.bg$site,loamy.bg$percent,geom="boxplot",xlab=NULL,ylab="Bare Soil (%)",main="Loamy Ecological Site",fill=factor(loamy.bg$site)) +
        coord_cartesian(ylim = c(0, 70))
qplot(malpais.bg$site,malpais.bg$percent,geom="boxplot",xlab=NULL,ylab="Bare Soil (%)",main = "Malpais Ecological Site",fill=factor(malpais.bg$site)) +
  coord_cartesian(ylim = c(0, 70))
qplot(mtn.bg$site,mtn.bg$percent,geom="boxplot",xlab=NULL,ylab="Bare Soil (%)",main = "Mountain Malpais Ecological Site", fill=factor(mtn.bg$site)) +
  coord_cartesian(ylim = c(0, 70))

qplot(loamy.pg$site,loamy.pg$percent,geom="boxplot",xlab=NULL,ylab="Perennial Grass Cover (%)",main="Loamy Ecological Site",fill=factor(loamy.pg$site)) +
  coord_cartesian(ylim = c(0, 70))
qplot(malpais.pg$site,malpais.pg$percent,geom="boxplot",xlab=NULL,ylab="Perennial Grass Cover (%)",main = "Malpais Ecological Site",fill=factor(malpais.pg$site)) +
  coord_cartesian(ylim = c(0, 70))
qplot(mtn.pg$site,mtn.pg$percent,geom="boxplot",xlab=NULL,ylab="Perennial Grass Cover (%)",main = "Mountain Malpais Ecological Site", fill=factor(mtn.pg$site)) +
  coord_cartesian(ylim = c(0, 70))

#stats
test1<-t.test(subset(mtn.bg$AnyHitAvg,mtn.bg$site=="Treated"),subset(mtn.bg$AnyHitAvg,mtn.bg$site=="Untreated"))
test1

