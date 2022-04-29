### Results compilation

library(ggplot2)
library(data.table)

# Great Santiago -Sales
# No SW
aa<-fread("04 Results/02_GS/Sale/1_Med_Ind.txt",header = F,drop =c("V3"))
aa<-aa[V2!="c1",]
aa[,value:=as.numeric(V2)][,V2:=NULL]
aa[,c("year","month"):=data.table(t(aa[,strsplit(x = V1,split = " ",fixed = T)]))][,V1:=NULL]
aa[,c("year","month"):=lapply(.SD,FUN = as.numeric),.SDcols=c("year","month")]
aa<-rbind(data.table(value=1,year=2017,month=7),aa)
aa[,serie:="Median Index"]
setcolorder(aa,c("serie","year","month","value"))


bb<-fread("04 Results/02_GS/Sale/1_HedPrice2_Sd2.txt",header = F,drop = "V4")
bb<-bb[V2!="HPI",]
bb[,value:=as.numeric(V2)][,V2:=NULL]
bb[,sd:=as.numeric(V3)][,V3:=NULL]
bb[,c("year","month"):=data.table(t(bb[,strsplit(x = V1,split = " ",fixed = T)]))][,V1:=NULL]
bb[,c("year","month"):=lapply(.SD,FUN = as.numeric),.SDcols=c("year","month")]
bb<-rbind(data.table(value=1,sd=0,year=2017,month=7),bb)
bb[,serie:="HPI - without SW"]
setcolorder(bb,c("serie","year","month","value","sd"))

# With SW 1
bb2<-fread("04 Results/02_GS/Sale_exp_1/1_HedPrice2_Sd2.txt",header = F,drop = "V4")
bb2<-bb2[V2!="HPI",]
bb2[,value:=as.numeric(V2)][,V2:=NULL]
bb2[,sd:=as.numeric(V3)][,V3:=NULL]
bb2[,c("year","month"):=data.table(t(bb2[,strsplit(x = V1,split = " ",fixed = T)]))][,V1:=NULL]
bb2[,c("year","month"):=lapply(.SD,FUN = as.numeric),.SDcols=c("year","month")]
bb2<-rbind(data.table(value=1,sd=0,year=2017,month=7),bb2)
bb2[,serie:="HPI - SW1"]
setcolorder(bb2,c("serie","year","month","value","sd"))

cc<-rbind(aa,bb,bb2,fill=T)
cc[,Date:=as.Date(paste0(year,"-",month,"-01"))]
cc[,sale:="Sale"]


# Great Santiago -Rent
# No SW
aa_r<-fread("04 Results/02_GS/Rent/1_Med_Ind.txt",header = F,drop =c("V3"))
aa_r<-aa_r[V2!="c1",]
aa_r[,value:=as.numeric(V2)][,V2:=NULL]
aa_r[,c("year","month"):=data.table(t(aa_r[,strsplit(x = V1,split = " ",fixed = T)]))][,V1:=NULL]
aa_r[,c("year","month"):=lapply(.SD,FUN = as.numeric),.SDcols=c("year","month")]
aa_r<-rbind(data.table(value=1,year=2017,month=7),aa_r)
aa_r[,serie:="Median Index"]
setcolorder(aa_r,c("serie","year","month","value"))


bb_r<-fread("04 Results/02_GS/Rent/1_HedPrice2_Sd2.txt",header = F,drop = "V4")
bb_r<-bb_r[V2!="HPI",]
bb_r[,value:=as.numeric(V2)][,V2:=NULL]
bb_r[,sd:=as.numeric(V3)][,V3:=NULL]
bb_r[,c("year","month"):=data.table(t(bb[,strsplit(x = V1,split = " ",fixed = T)]))][,V1:=NULL]
bb_r[,c("year","month"):=lapply(.SD,FUN = as.numeric),.SDcols=c("year","month")]
bb_r<-rbind(data.table(value=1,sd=0,year=2017,month=7),bb_r)
bb_r[,serie:="HPI - without SW"]
setcolorder(bb_r,c("serie","year","month","value","sd"))

# With SW 1
bb2_r<-fread("04 Results/02_GS/Rent_exp_1/1_HedPrice2_Sd2.txt",header = F,drop = "V4")
bb2_r<-bb2_r[V2!="HPI",]
bb2_r[,value:=as.numeric(V2)][,V2:=NULL]
bb2_r[,sd:=as.numeric(V3)][,V3:=NULL]
bb2_r[,c("year","month"):=data.table(t(bb2_r[,strsplit(x = V1,split = " ",fixed = T)]))][,V1:=NULL]
bb2_r[,c("year","month"):=lapply(.SD,FUN = as.numeric),.SDcols=c("year","month")]
bb2_r<-rbind(data.table(value=1,sd=0,year=2017,month=7),bb2_r)
bb2_r[,serie:="HPI - SW1"]
setcolorder(bb2_r,c("serie","year","month","value","sd"))

cc_r<-rbind(aa_r,bb_r,bb2_r,fill=T)
cc_r[,Date:=as.Date(paste0(year,"-",month,"-01"))]
cc_r[,sale:="Rent"]
CC<-rbind(cc,cc_r)




ggplot(data=CC[serie%in%c("HPI - without SW","Median Index")],aes(x=Date,y=value))+
  geom_line(aes(col=serie))+
  scale_color_manual(values=c(`Median Index`="grey50",`HPI - without SW`="#A00000"))+
  geom_ribbon(aes(ymin=value-(1.96*sd),ymax=value+(1.96*sd),fill=serie),alpha=0.5)+
  scale_fill_manual(values = c(`Median Index`="000000",`HPI - without SW`="#A00000"))+
  scale_x_date(date_breaks='months',date_labels = "%Y - %b",minor_breaks = NULL,limits = c(as.Date("2017-07-01"),as.Date("2020-11-01")))+
  geom_hline(aes(yintercept=1),col='red')+
  theme_classic()+
  theme(axis.text = element_text(angle = 90))+
  facet_wrap(~sale,scales = "free_y",ncol = 1)
ggsave(filename = "04 Results/Figure3.png")


ggplot(data=CC[serie%in%c("HPI - without SW","HPI - SW1")],aes(x=Date))+
  geom_line(aes(y=value,group=serie,col=serie))+
  geom_ribbon(aes(ymin=value-(1.96*sd),ymax=value+(1.96*sd),fill=serie),alpha=0.5)+
  #geom_line(data=cc[serie%in%c("Median Index")],aes(x=Date,y=value,colour="Median Index"),col='black')+
  scale_x_date(date_breaks='months',date_labels = "%Y - %b",minor_breaks = NULL,limits = c(as.Date("2017-07-01"),as.Date("2020-11-01")))+
  geom_hline(aes(yintercept=1),col='red')+
  theme_classic()+
  theme(axis.text = element_text(angle = 90))+
  facet_wrap(~sale,scales = "free_y",ncol = 1)
ggsave(filename = "04 Results/Figure4.png")

dif_1<-dcast.data.table(CC[serie%in%c("HPI - without SW","HPI - SW1")],formula = year + month + sale ~ serie)
dif_1[,diff:=`HPI - SW1`- `HPI - without SW`]
dif_1[diff>0,`Delta type`:="Under estimation"]
dif_1[diff<0,`Delta type`:="Over estimation"]

ggplot(dif_1[!is.na(`Delta type`),.(`Average delta`=mean(abs(diff))),by=.(sale,`Delta type`)])+
  geom_col(aes(y=sale,x=`Average delta`,fill=`Delta type`),position = 'dodge')+
  theme_classic()+
  theme(legend.title = element_blank())
ggsave(filename = "04 Results/Figure5.png")

ggplot(dif_1[!is.na(`Delta type`),.(`Total delta`=sum(abs(diff))),by=.(sale,`Delta type`)])+
  geom_col(aes(y=sale,x=`Total delta`,fill=`Delta type`),position = 'dodge')+
  theme_classic()+
  theme(legend.title = element_blank())



