#########################################################
#  Part 3.1: Sampling weights - Frequency based
#########################################################



# spatial overlay listing data on census blocks
library(data.table)
library(chilemapas)
library(sf)
library(sp)
library(dplyr)
library(nngeo)

hh<-fread(file = "01 Data/base_toctoc.csv.gz",stringsAsFactors = F)
hh<-hh[yearPublicacion==2017 & mesPublicacion==7,]
# Quarterly
#hh <- hh[yearPublicacion==2017 & mesPublicacion==7, Tr:=1]
#hh <- hh[yearPublicacion==2017 & mesPublicacion==8, Tr:=1]
#hh <- hh[yearPublicacion==2017 & mesPublicacion==9, Tr:=1]
#hh <- hh[Tr==1,][,Tr:=NULL]

coms<-codigos_territoriales[codigos_territoriales$codigo_provincia %in% 131 | codigos_territoriales$codigo_comuna %in% c(13201,13401),]$codigo_comuna


viviendas_vacantes_rm<-fread(file='01 Data/viviendas_vacantes_rm.csv')

mapa_zonas_rm_casa<- mapa_zonas %>%
  filter(codigo_provincia %in% 131 | codigo_comuna %in% c(13201,13401))%>%
  left_join(viviendas_vacantes_rm %>% filter(p01==1,vacante==1) ,by="geocodigo")

mapa_zonas_rm_dpto<- mapa_zonas %>%
  filter(codigo_provincia %in% 131 | codigo_comuna %in% c(13201,13401))%>%
  left_join(viviendas_vacantes_rm %>% filter(p01==2,vacante==1) ,by="geocodigo")



#loop for houses
HH<-NULL
for(i in coms){
  print(paste("In comuna...", i))
  x<-st_sf(mapa_zonas_rm_casa[mapa_zonas_rm_casa$codigo_comuna==i,])  
  y<-hh[cod_terr==i & idTipoPropiedad==1,.(latitud2,longitud2,id)]  
  if(dim(y)[1]==0){next}else{
    y<-as(SpatialPointsDataFrame(coords = y[,.(longitud2,latitud2)],data = y,proj4string = CRS(projargs = '+proj=longlat +ellps=GRS80 +no_defs')),'sf')
    z<-st_join(x,y,join=st_intersects)
    z<- z%>% 
      group_by(geocodigo)%>%
      mutate(cuenta_emp=n())%>%
      mutate(expansor=(cuenta/cuenta_emp))  
    #z<-z%>%
    # mutate(cuenta_exp=n()*prop)
    z<-data.table(z)
    total_vacantes_comuna<-z[!duplicated(geocodigo),sum(cuenta,na.rm = T)]
    #total_listings_comuna<-z[!duplicated(geocodigo),sum(cuenta_emp,na.rm = T)]
    total_listings_comuna<-z[!is.na(latitud2),.N]
    z[,theta:=(total_listings_comuna*(cuenta/total_vacantes_comuna))]
    z[,cuenta_emp2:=cuenta_emp][is.na(latitud2),cuenta_emp2:=0]
    z[,lambda:=((cuenta_emp2+1)/(theta+1))]
    z[,expansor2:=expansor]
    nas<-z[is.na(z$id),]$geocodigo
    if(length(nas)>0){
      for(j in nas){
        ind<-st_nn(st_sf(z[geocodigo%in%j,]),st_sf(z[!geocodigo%in%nas,]),k=1)
        idd<-z[!geocodigo%in%nas,][ind[[1]]]$id
        toadd<-z[geocodigo%in%j,]$expansor
        z[id==idd,expansor2:=expansor+toadd]
      }
    }
    z<-z[!is.na(id),.(id,geocodigo,codigo_comuna,p01,cuenta,prop,cuenta_emp,expansor,expansor2,theta,lambda)]
    HH<-rbind(HH,z)
  }
}

fwrite(HH,file = "01 Data/01_Comuna/Houses_controlDB.csv")
#fwrite(HH,file = "01 Data/01_Comuna/Houses_controlDB_Q.csv")


#loop for appartments
AA<-NULL
for(i in coms){
  print(paste("In comuna...", i))
  x<-st_sf(mapa_zonas_rm_dpto[mapa_zonas_rm_dpto$codigo_comuna==i,])  
  y<-hh[cod_terr==i & idTipoPropiedad==2,.(latitud2,longitud2,id)]  
  if(dim(y)[1]==0){next}else{
    y<-as(SpatialPointsDataFrame(coords = y[,.(longitud2,latitud2)],data = y,proj4string = CRS(projargs = '+proj=longlat +ellps=GRS80 +no_defs')),'sf')
    z<-st_join(x,y,join=st_intersects)
    z<- z%>% 
      group_by(geocodigo)%>%
      mutate(cuenta_emp=n())%>%
      mutate(expansor=(cuenta/cuenta_emp))  
    #z<-z%>%
    # mutate(cuenta_exp=n()*prop)
    z<-data.table(z)
    total_vacantes_comuna<-z[!duplicated(geocodigo),sum(cuenta,na.rm = T)]
    total_listings_comuna<-z[!duplicated(geocodigo),sum(cuenta_emp,na.rm = T)]
    z[,theta:=(total_listings_comuna*(cuenta/total_vacantes_comuna))]
    z[,lambda:=((cuenta_emp+1)/(theta+1))]
    z[,expansor2:=expansor]#includes census tracts with vacancies but no posting data to the nearest posting data
    nas<-z[is.na(z$id),]$geocodigo
    if(length(nas)>0){
      for(j in nas){
        ind<-st_nn(st_sf(z[geocodigo%in%j,]),st_sf(z[!geocodigo%in%nas,]),k=1)
        idd<-z[!geocodigo%in%nas,][ind[[1]]]$id
        toadd<-z[geocodigo%in%j,]$expansor
        z[id==idd,expansor2:=expansor+toadd]
      }
    }
    z<-z[!is.na(id),.(id,geocodigo,codigo_comuna,p01,cuenta,prop,cuenta_emp,expansor,expansor2,theta,lambda)]
    AA<-rbind(AA,z)
  }
}

fwrite(AA,file = "01 Data/01_Comuna/Apts_controlDB.csv")
#fwrite(AA,file = "01 Data/01_Comuna/Apts_controlDB_Q.csv")
