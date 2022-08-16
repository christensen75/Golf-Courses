library(geosphere)
w=function(lat=0,long=0,i){
w1=6371*acos(sin(d$Lat[i]*pi/180)*sin(lat*pi/180)+cos(d$Lat[i]*pi/180)*cos(lat*pi/180)*cos(abs(d$Long[i]-long)*pi/180))
w1[is.na(w1)]==0
w1}

setwd("C:/Users/chip/OneDrive/Documents/golf")
d=read.csv("ergebnis.csv")[,c(4,5,1)]
a=read.csv("golf.csv"); names(a)=c("Latitude","Longitude")

d$Value=1

r=50
for(i in 1:nrow(d)){
km=w(a$Lat,a$Long,i)
d$Value[i]=sum(km<r)
}

d=d[order(-d$Value),]
xd=paste(10*round(d$Lat/10),10*round(d$Long/10))
d=d[!duplicated(xd),]
d=d[1:54,]

d$Color="#fce303"
a$Value=1
a$Color="#04b004"
d=rbind(d,a)

h=read.table("htmltemplate.txt",sep="~",stringsAsFactor=F,quote="")[,1]
xxColor=paste(paste0("\"",d$Color,"\""),collapse=",")
xxValue=paste(ceiling(200*d$Value/max(d$Value)),collapse=",")
xxLongitude=paste(d$Longitude,collapse=",")
xxLatitude=paste(d$Latitude,collapse=",")
h=gsub("xxColor",xxColor,h)
h=gsub("xxValue",xxValue,h)
h=gsub("xxLatitude",xxLatitude,h)
h=gsub("xxLongitude",xxLongitude,h)


fileConn=file("golf50.html")
writeLines(h, fileConn)
close(fileConn)

