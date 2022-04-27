# Figures

#---- Figure 1 ----
library(data.table)
library(ggplot2)
library(ggsf)
library(chilemapas)
library(dplyr)
library(tmap)

viviendas_vacantes_rm<-fread(file='01 Data/viviendas_vacantes_rm.csv')
coms<-codigos_territoriales[codigos_territoriales$codigo_provincia %in% 131 | codigos_territoriales$codigo_comuna %in% c(13201,13401),]$codigo_comuna

hh<-fread(file = "01 Data/base_toctoc.csv.gz",stringsAsFactors = F)
hh<-hdata[(yearPublicacion==2017 & mesPublicacion==7),]
hh[,codigo_provincia:=substr(cod_terr,1,3)]
hh<-hh[codigo_provincia %in% 131 | cod_terr %in% c(13201,13401),.(Total_Postings=.N),by=.(cod_terr)]
hh[,codigo_comuna:=as.numeric(cod_terr)] 
hh[,cod_terr:=NULL]

viviendas_vacantes_rm[,codigo_provincia:=substr(comuna,1,3)]
names(viviendas_vacantes_rm)[1]<-"codigo_comuna"
Total_viviendas<-viviendas_vacantes_rm[codigo_provincia %in% 131 | codigo_comuna %in% c(13201,13401),.(Total_Viviendas=sum(cuenta)),by=.(codigo_comuna)]
Total_casas<-viviendas_vacantes_rm[(codigo_provincia %in% 131 | codigo_comuna %in% c(13201,13401)) & p01==1,.(Total_casas=sum(cuenta)),by=.(codigo_comuna)]
Total_dptos<-viviendas_vacantes_rm[(codigo_provincia %in% 131 | codigo_comuna %in% c(13201,13401)) & p01==2,.(Total_dptos=sum(cuenta)),by=.(codigo_comuna)]
Total_vacantes<-viviendas_vacantes_rm[(codigo_provincia %in% 131 | codigo_comuna %in% c(13201,13401)) & vacante==1,.(Total_vacantes=sum(cuenta)),by=.(codigo_comuna)]

Viviendas_RM<-merge(Total_viviendas,Total_casas, by="codigo_comuna")
Viviendas_RM<-merge(Viviendas_RM,Total_dptos, by="codigo_comuna")
Viviendas_RM<-merge(Viviendas_RM,Total_vacantes, by="codigo_comuna")

Viviendas_RM<-merge(Viviendas_RM,hh,by="codigo_comuna",all.x = T)
Viviendas_RM[,Coverage:=Total_Postings/Total_vacantes]
Viviendas_RM[,Vacancies:=Total_vacantes/Total_Viviendas]
Viviendas_RM[,codigo_comuna:=as.character(codigo_comuna)]

mapa_comunas_rm <- mapa_comunas %>%
  filter(codigo_provincia %in% 131 | codigo_comuna %in% c(13201,13401))%>%
  left_join(Viviendas_RM ,by="codigo_comuna")


mapa_comunas_rm<-st_sf(mapa_comunas_rm)

Fig1<-tm_shape(mapa_comunas_rm)+
  tm_polygons(col="Total_Viviendas",
              style='jenks',
              palette="Purples",
              title="Total Dwellings",
              legend.hist=TRUE)+
  tm_bubbles(size = "Vacancies", alpha = 0.7,
             col= "Coverage",
             palette = "Reds",
             n=3,
             title.col="TocToc Posting Coverage",
             title.size = "Census Vacancies") + 
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "right",
            frame = FALSE,
            legend.hist.width = 5)+
  tm_compass()+
  tm_scale_bar()+
  tm_credits(text = "Census vacancies for 04/2017. TocToc posting coverage for 07/2017. Both numbers are percentages.",
             position = c("right","bottom"))

tmap_save(Fig1,"04 Results/Figure1.png")

 # ggplot(data = Viviendas_RM)+
    #geom_point(aes(x=Vacancies,y=Coverage))+
    #geom_text(aes(x=Vacancies,y=Coverage,label=codigo_comuna))    

# higher coverage municipalities (>30%) == c(13115,13132,13114,13113)

#---- Figure 2 ----
  
#---- Figure 4 ----
#Listing intensity changes
library(data.table)
library(ggplot2)
library(chilemapas)
library(dplyr)

hdata<-fread("01 Data/base_toctoc.csv.gz",stringsAsFactors = F)
hdata[,codigo_provincia:=substr(cod_terr,1,3)]
hdata<-hdata[codigo_provincia %in% 131 | cod_terr %in% c(13201,13401),]

mapa_comunas_rm <- mapa_comunas %>%
  filter(codigo_provincia %in% 131 | codigo_comuna %in% c(13201,13401))
mapa_comunas_rm<-st_sf(mapa_comunas_rm)

hdata[dormitorios%in%c(1,2),`Bedrooms`:="1-2 bedrooms"]
hdata[dormitorios%in%c(3,4),`Bedrooms`:="3-4 bedrooms"]
hdata[dormitorios>4,`Bedrooms`:="4< bedrooms"]
hdata[,`Sale Price in\nChilean UFs`:=precioUF]

ggplot(hdata[((mesPublicacion==8 & yearPublicacion==2017) |(mesPublicacion==12 & yearPublicacion==2019)) & sale==1,])+
  geom_sf(data = mapa_comunas_rm,lwd=0.5)+
  geom_point(aes(y=latitud,x=longitud,colour=`Sale Price in\nChilean UFs`),size=0.25)+
  geom_density_2d(aes(y=latitud,x=longitud),color="red",bins=7)+
  scale_color_continuous(type = "viridis")+
  facet_grid(interaction(mesPublicacion,yearPublicacion,sep = "/")~Bedrooms)
ggsave(filename = "04 Results/Figure2.png")


#---- Figure 5 ----
library(data.table)
library(ggplot2)

hh<-fread(file = "01 Data/base_toctoc.csv.gz",stringsAsFactors = F)

#Sampling weights
houses <- fread("01 Data/01_Comuna/Houses_controlDB.csv")
apts <- fread("01 Data/01_Comuna/Apts_controlDB.csv")

hh_sw <- rbind(houses,apts)
hh_sw <- merge(hh_sw, hh, by = "id", all = TRUE)

f5<-hh_sw[!is.na(geocodigo),.(`Listing data \n(weigthed)`=sum(expansor,na.rm=T),`Listing data \n(unweigthed)`=.N,`Census data`=max(cuenta)),by=.(geocodigo)]

f5<-melt(f5,id.vars = "geocodigo")
A<-f5[variable%in%c("Listing data \n(unweigthed)","Census data"),]
A[,status:="Before sampling weights"]
B<-f5[variable%in%c("Listing data \n(weigthed)","Census data"),]
B[,status:="After sampling weights"]
f5<-rbind(B,A)


ggplot(data = f5[value<=300,], aes(x=value,group=variable,color=variable))+
  geom_density()+
  labs(x="Number of vacancies per census tract", y="Density")+
  theme_minimal()+
  #theme(legend.position = 'none')+
  facet_wrap(~reorder(x=status,status),scales = 'free_y')
ggsave(filename = "04 Results/Figure5.png")  
