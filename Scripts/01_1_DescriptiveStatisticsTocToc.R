# descriptive statistics toctoc
library(data.table)

hdata<-fread("01 Data/base_toctoc.csv.gz",stringsAsFactors = F)


#Table 1. Descriptive statistics

names(hdata)

vars_1<-c("precioUF","sqft","dormitorios","banos","totalImagenes","mesPublicacion","yearPublicacion","latitud2","longitud2")


hdata[sale==1 & idTipoPropiedad==1,.(Min=lapply(.SD,min,na.rm=T),
                                     p25=lapply(.SD,quantile,probs=0.25,na.rm=T),
                                     Median=lapply(.SD,quantile,probs=0.5,na.rm=T),
                                     p75=lapply(.SD,quantile,probs=0.75,na.rm=T),
                                     Max=lapply(.SD,max,na.rm=T),`Obs`=.N),
      .SDcols=vars_1]


A<-hdata[sale==1 & idTipoPropiedad==1,.(Min=lapply(.SD,min,na.rm=T),
                   p25=lapply(.SD,quantile,probs=0.25,na.rm=T),
                   Median=lapply(.SD,quantile,probs=0.5,na.rm=T),
                   p75=lapply(.SD,quantile,probs=0.75,na.rm=T),
                   Max=lapply(.SD,max,na.rm=T),`Obs`=.N),
         .SDcols=vars_1]# sales & houses

A2<-hdata[sale==1 & idTipoPropiedad==2,.(Min=lapply(.SD,min,na.rm=T),
                                        p25=lapply(.SD,quantile,probs=0.25,na.rm=T),
                                        Median=lapply(.SD,quantile,probs=0.5,na.rm=T),
                                        p75=lapply(.SD,quantile,probs=0.75,na.rm=T),
                                        Max=lapply(.SD,max,na.rm=T),`Obs`=.N),
         .SDcols=vars_1]# sales & apartments

B<-hdata[sale==0 & idTipoPropiedad==1,.(Min=lapply(.SD,min,na.rm=T),
                   p25=lapply(.SD,quantile,probs=0.25,na.rm=T),
                   Median=lapply(.SD,quantile,probs=0.5,na.rm=T),
                   p75=lapply(.SD,quantile,probs=0.75,na.rm=T),
                   Max=lapply(.SD,max,na.rm=T),`Obs`=.N),
         .SDcols=vars_1]# rent & houses

B2<-hdata[sale==0 & idTipoPropiedad==2,.(Min=lapply(.SD,min,na.rm=T),
                                        p25=lapply(.SD,quantile,probs=0.25,na.rm=T),
                                        Median=lapply(.SD,quantile,probs=0.5,na.rm=T),
                                        p75=lapply(.SD,quantile,probs=0.75,na.rm=T),
                                        Max=lapply(.SD,max,na.rm=T),`Obs`=.N),
         .SDcols=vars_1]# rent & apartments


DescStatsTable<-rbind(cbind(vars_1,A),cbind(vars_1,A2),cbind(vars_1,B),cbind(vars_1,B2))
fwrite(file = "04 Results/DescStatsTocToc.csv",x = DescStatsTable)
rm(list=c("A","A2","B","B2","DescStatsTable"))