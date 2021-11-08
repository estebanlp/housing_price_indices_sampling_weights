rm(list=ls())
library(censo2017)
library(chilemapas)
library(dbplyr)
library(ggplot2)
library(tidyverse)

censo_descargar()

viviendas_vacantes_rm<- tbl(censo_conectar(), "zonas") %>%
  mutate(
      region = substr(as.character(geocodigo),1,2),
      comuna = substr(as.character(geocodigo),1,5)
  ) %>%
  filter(region=="13")%>%
  select(comuna,geocodigo,zonaloc_ref_id)%>%
  inner_join(select(tbl(censo_conectar(),"viviendas"),zonaloc_ref_id,vivienda_ref_id,p01,p02),by='zonaloc_ref_id')%>%
  collect()

viviendas_vacantes_rm<- viviendas_vacantes_rm%>%
  filter(p01%in%c(1,2))%>%
  mutate(vacante = as.numeric(p02==3))%>%
  group_by(comuna,geocodigo,p01,vacante)%>%
  summarise(cuenta=n())%>%
  group_by(geocodigo,p01)%>%
  mutate(prop = cuenta /sum(cuenta))

censo_desconectar_base()


fwrite(viviendas_vacantes_rm,file='01 Data/viviendas_vacantes_rm.csv')

viviendas_vacantes_rm <- fread('01 Data/viviendas_vacantes_rm.csv')
viviendas_vacantes_rm$geocodigo <- as.character(viviendas_vacantes_rm$geocodigo)

mapa_comunas_rm <- mapa_comunas %>%
  filter(codigo_provincia %in% 131 | codigo_comuna %in% c(13201,13401))

mapa_zonas_rm_casa<- mapa_zonas %>%
  filter(codigo_provincia %in% 131 | codigo_comuna %in% c(13201,13401))%>%
  left_join(viviendas_vacantes_rm %>% filter(p01==1,vacante==1) ,by="geocodigo")

mapa_zonas_rm_dpto<- mapa_zonas %>%
  filter(codigo_provincia %in% 131 | codigo_comuna %in% c(13201,13401))%>%
  left_join(viviendas_vacantes_rm %>% filter(p01==2,vacante==1) ,by="geocodigo")

g_casa<-ggplot() +
  geom_sf(data=mapa_zonas_rm_casa,
          aes(fill=prop,geometry=geometry),lwd=0.1)

g_dpto<-ggplot() +
  geom_sf(data=mapa_zonas_rm_dpto,
          aes(fill=prop,geometry=geometry),lwd=0.1)
