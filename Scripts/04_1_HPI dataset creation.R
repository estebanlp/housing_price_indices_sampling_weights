# Data creation for HPIs
library(data.table)

hdata<-fread(file = "01 Data/base_toctoc.csv.gz")


# Merge toctoc data with sampling weight type 1 and 2
library(data.table)

#Sampling weights
houses <- fread("01 Data/01_Comuna/Houses_controlDB.csv")
apts <- fread("01 Data/01_Comuna/Apts_controlDB.csv")

ha <- list(houses,apts)
houses_apts <- rbindlist(ha)

hdata <- merge(houses_apts, hdata, by = "id", all = TRUE)
fwrite(hdata, file = "01 Data/base_toctoc_exp.csv.gz")
#write.csv(toctoc_exp, file = "01 Data/01_Comuna/base_toctoc_exp.csv")


# selecting municipalities that have enough data

base_comuna1<-hdata[,.(Num_obs=.N,Prom_PrecioUF=mean(precioUF,na.rm=T),Mediana_PrecioUF=median(precioUF,na.rm=T)),by=.(yearPublicacion,mesPublicacion,Descripcion,idComuna,cod_terr,idTipoPropiedad,sale)]


base_comuna1<-dcast.data.table(base_comuna1[!is.na(sale) & Num_obs>=30,],yearPublicacion + mesPublicacion + Descripcion +idComuna +cod_terr ~ idTipoPropiedad + sale,value.var = "Prom_PrecioUF")

write.csv(base_comuna1, file = "01 Data/base_comuna1.csv")

#keeping only comunas con al menos 50 datos al mes en Julio 2017 
keep50<-hdata[,.N,by=.(yearPublicacion,mesPublicacion,idComuna,Descripcion)]
keep50<-keep50[!is.na(yearPublicacion),]
keep50[,g50:=as.numeric(N>=50)]
keep50[,g100:=as.numeric(N>=100)]

ToKeep<-keep50[,sum(g100),by=.(yearPublicacion,idComuna,Descripcion)][yearPublicacion==2017 & V1==6,unique(idComuna)]


#----- Monthly Index: Great Santiago ----
GS_comunas<-c(321,314,317,318,313,323,326,311,336,333,328,325,338,319,322,327,342,335,324,320,341,330,340,331,316,329,339,334,315,312,332,337)

dat1<-hdata[(yearPublicacion==2017 & mesPublicacion==7) & idComuna%in%GS_comunas,]
dat1[,treated:=0]
for(y in 2017:2020){
  for(m in 1:12){
    if(y==2017 & m<=6){m<-8}
    print(paste("I am in en el año",y,"con el mes",m))
    dat2<-hdata[(yearPublicacion==y & mesPublicacion==m) & idComuna%in%GS_comunas,]    
    dat2[,treated:=1]
    hhdata<-rbind(dat1,dat2)
    fwrite(hhdata,file = paste0("01 Data/02_GS/GS","_",y,"_",m,".csv"))
  }
}



#----- Monthly Index: Macrozones ----

hdata[idComuna%in%c(337,340,313,314,312,311),macrozona:=1]# Oriente: Providencia, Ñuñoa, Las Condes, La Reina, Vitacura, Lo Barnechea.

hdata[idComuna%in%c(339,325,324,335),macrozona:=2]# Centro: Santiago, Recoleta, Independencia, San Miguel

hdata[idComuna%in%c(341,315,342,316,295,330,331,334,333,308,296),macrozona:=3]#Suroriente: San Joaquín, Peñalolén, Macul, La Florida, Puente Alto, La Cisterna, San Ramon, La Granja, La Pintana, El Bosque, San Bernardo, Pirque.

hdata[idComuna%in%c(327,326,322,323,292,309,293,338,336,318,317,319,320,321,329,328),macrozona:=4]# Norponiente: Huechuraba, Conchalí, Renca, Quilicura, Colina, Buin, Lampa, Estacion Central, Quinta Normal, Cerro Navia, Pudahuel, Lo Prado, Maipú, Cerrillos,PAC, Lo Espejo


# monthly
for(i in 1:4){
  dat1<-hdata[macrozona==i & yearPublicacion==2017 & mesPublicacion==7,]
  dat1[,treated:=0]
  for(y in 2017:2020){
    for(m in 1:12){
      if(y==2017 & m<=7){m<-8}
      print(paste("I am in",i,"en el año",y,"con el mes",m))
      dat2<-hdata[macrozona==i & yearPublicacion==y & mesPublicacion==m,]    
      dat2[,treated:=1]
      hhdata<-rbind(dat1,dat2)
      fwrite(hhdata,file = paste0("01 Data/03_Groups/",i,"_",y,"_",m,".csv"))
    }
  }
}

#----- Monthly Index: Municipality level ----

#loop para crear bases mensuales de treated y control
for(i in 1:length(ToKeep)){
  dat1<-hdata[idComuna==ToKeep[i] & yearPublicacion==2017 & mesPublicacion==7,]
  dat1[,treated:=0]
  for(y in 2017:2020){
    for(m in 1:12){
      if(y==2017 & m<=6){m<-8}
      print(paste("I am in",ToKeep[i],"en el año",y,"con el mes",m))
      dat2<-hdata[idComuna==ToKeep[i] & yearPublicacion==y & mesPublicacion==m,]    
      dat2[,treated:=1]
      hhdata<-rbind(dat1,dat2)
      fwrite(hhdata,file = paste0("01 Data/01_Comuna/Monthly/",i,"_",y,"_",m,".csv"))
    }
  }
}


