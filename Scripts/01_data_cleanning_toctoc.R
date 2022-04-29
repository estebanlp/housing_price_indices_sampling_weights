# Data clanning TocToc


library(data.table)
library(chilemapas)
library(foreign)
library(ggplot2)

pathD <- "C:/Users/smw634/Dropbox/Documents/006 Library/01 Chile/TocToc/BDPublicacionesTOCTOC/"
files<-dir(pathD)[grepl("data",dir(pathD))]

#-----------------------------
#reading & merging the data
#aa<-read.table(file = paste0(pathD,files[i]),sep = ",",fileEncoding = "UTF-16",encoding = "Latin-1",skip = 1)

hdata<-NULL
for(i in 1:3){
  dat<-fread(paste0(pathD,files[i]),select = c(1,2,4,5,6,7,9:13,16:17,19:22,36))
  dat[,fechaPublicacion:=as.character(fechaPublicacion)]
  hdata<-rbind(hdata,dat)
}

for(i in 4:5){
  dat<-fread(paste0(pathD,files[i]),select = c(1,2,4,5,6,7,9:13,15:20,33))
  dat[,fechaPublicacion:=as.character(fechaPublicacion)]
  hdata<-rbind(hdata,dat)
}
rm(dat)

# total datos 836833

#dat<-fread(paste0(pathD,"RestoDataantesde2017.txt"))

#cdata<-fread(paste0(pathD,"comunapareo.csv"))
#cdata[,IdComunaPareo:=as.character(IdComunaPareo)]
idcom<-fread(paste0(pathD,"IdComuna.csv"))
idcom[,IdComuna:=as.character(IdComuna)]

#hdata<-merge(hdata,cdata,by.x="idComuna",by.y="IdComunaPareo",all=T,sort = F)
hdata<-merge(hdata,idcom,by.x="idComuna",by.y="IdComuna",all=T,sort = F)

#option 1: use bbox de chilemapas at comuna level
hdata[,latitud2:=as.numeric(latitud)]
hdata[,longitud2:=as.numeric(longitud)]
#ggplot(mapa_comunas[mapa_comunas$codigo_comuna=="13101",])+geom_sf(aes(geometry=geometry))
#View(codigos_territoriales)

for(i in 1:dim(idcom)[1]){
  border<-st_bbox(st_sf(mapa_comunas[mapa_comunas$codigo_comuna==idcom$cod_terr[i],]))
  hdata[idComuna==i & longitud2%between%border[c(1,3)] & latitud2%between%border[c(2,4)] ,coord_OK:=1]
}

#cleaning
hdata[,.N,by=.(coord_OK)]
hdata <- na.omit (hdata, "coord_OK") 
hdata[,fechaPublicacion:=as.Date(fechaPublicacion)]
hdata[,mesPublicacion:=month(fechaPublicacion)]
hdata[,yearPublicacion:=year(fechaPublicacion)]
hdata[,idTipoPropiedad:=as.numeric(idTipoPropiedad)]
hdata[,dormitorios:=as.numeric(dormitorios)]
hdata[,banos:=as.numeric(banos)]
hdata <- hdata[dormitorios%between%c(1,10),]
hdata <- hdata[banos%between%c(1,10),]
hdata[,totalImagenes:=as.numeric(totalImagenes)]
#hdata <- hdata[totalImagenes%between%c(1,30),]



# plots to show how dirty the data is

plot(density(hdata[IdRegion==13 & yearPublicacion==2017 & mesPublicacion==8 & precioUF%between%quantile(precioUF,probs = c(0.1,0.90),na.rm = T),]$precioUF,na.rm = T),main = "")
lines(density(hdata[IdRegion==13 & yearPublicacion==2017 & mesPublicacion==7 & precioUF%between%quantile(precioUF,probs = c(0.1,0.90),na.rm = T),]$precioUF,na.rm = T),col="Red")
lines(density(hdata[IdRegion==13 & yearPublicacion==2017 & mesPublicacion==9 & precioUF%between%quantile(precioUF,probs = c(0.1,0.90),na.rm = T),]$precioUF,na.rm = T),col="Blue")
lines(density(hdata[IdRegion==13 & yearPublicacion==2017 & mesPublicacion==10 & precioUF%between%quantile(precioUF,probs = c(0.1,0.90),na.rm = T),]$precioUF,na.rm = T),col="Green")
lines(density(hdata[IdRegion==13 & yearPublicacion==2017 & mesPublicacion==11 & precioUF%between%quantile(precioUF,probs = c(0.1,0.90),na.rm = T),]$precioUF,na.rm = T),col="purple")
lines(density(hdata[IdRegion==13 & yearPublicacion==2017 & mesPublicacion==12 & precioUF%between%quantile(precioUF,probs = c(0.1,0.90),na.rm = T),]$precioUF,na.rm = T),col="brown")

library(ggplot2)
ggplot(hdata[IdRegion==13 & yearPublicacion==2017 & precioUF%between%quantile(precioUF,probs = c(0.1,0.90),na.rm = T),],aes(precioUF,fill=factor(mesPublicacion)))+geom_density()+facet_wrap(~mesPublicacion)+theme(legend.position = "none")


hdata[,sum(precioUF<=0)]
hdata<-hdata[precioUF>0,]


#trying to recover sqft data

hdata[,superficieConstruida:=as.numeric(superficieConstruida)]
hdata[,superficieUtil:=as.numeric(superficieUtil)]

ggplot(data = hdata[yearPublicacion==2017& mesPublicacion==7 & superficieUtil<1000 & superficieConstruida<1000,])+
  geom_point(aes(x=as.numeric(superficieUtil), y=as.numeric(superficieConstruida)))

#aim: populate zeros in superficieConstruida using the superficieUtil variable
hdata[superficieConstruida==0 & superficieUtil==0,.N] # number of zeros all together 16566
hdata[superficieConstruida==0,.N] # number of zeros only in superficieConstruida 259743
hdata[superficieConstruida==0&superficieUtil>0,.N] # cases with zero in superficieConstruida, but positive values in superficie Util 243136

hdata[,sqft:=superficieConstruida]
hdata[superficieConstruida==0 & superficieUtil>0,sqft:=superficieUtil]
hdata[,summary(sqft)]
View(hdata[!sqft%between%c(0,1000),])
hdata<-hdata[sqft%between%c(0,1000),] # drops 1703 posts (really big and luxury houses)

summary(hdata$sqft)
ggplot(hdata[IdRegion==13 & yearPublicacion==2017 ,],aes(sqft,fill=factor(mesPublicacion)))+
  geom_density()+
  facet_wrap(~mesPublicacion)+theme(legend.position = "none")



hdata[,idTipoOperacion:=as.numeric(idTipoOperacion)]
hdata[idTipoOperacion%in%c(1,7,8,9),sale:=1]
hdata[idTipoOperacion%in%c(2,4,10,11,16,20),sale:=0]#rent

hdata<-hdata[idTipoPropiedad%in%c(1,2) & !is.na(sale),]

#dropping the tails of the data by sale 

sale_q5<-hdata[sale==1,quantile(precioUF,probs=0.05)]
rent_q5<-hdata[sale==0,quantile(precioUF,probs=0.05)]
sale_q95<-hdata[sale==1,quantile(precioUF,probs=0.95)]
rent_q95<-hdata[sale==0,quantile(precioUF,probs=0.95)]

hdata[sale==1 & precioUF%between%c(sale_q5,sale_q95),keep:=1] 
hdata[sale==0 & precioUF%between%c(rent_q5,rent_q95),keep:=1] 

hdata[sale==1,max(precioUF),by=.(keep)]
hdata[sale==0,max(precioUF),by=.(keep)]

hdata[,.N,by=.(keep)] # drops 68892, keeps 624839

ggplot(data=hdata[keep==1,])+
  geom_density(aes(precioUF))+
  facet_wrap(sale~idTipoPropiedad,scales = "free")


hdata<-hdata[keep==1,] # drops 41066 - q95== 18900UFs!

hdata[,keep:=NULL]

fwrite(hdata, file = "01 Data/base_toctoc.csv.gz")
