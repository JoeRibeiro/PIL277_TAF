
#######################################################################################################################
############################## Generate plots shown in working group presentation. ####################################
############################## This is a standalone file and is separate to the advice ################################
#######################################################################################################################

library(rstudioapi) # This has been written as an R studio project using RStudio Version 1.4.1717 
library(icesTAF)
library(icesTAF)
library(plyr)
library(ggplot2)
library(reshape)


try(setwd(dirname(getActiveDocumentContext()$path)))
wdir = getwd(); print(wdir)

Catches=read.taf(paste0(wdir,"/bootstrap/data/Catches.csv"))

### metiers

Catches$metier2<-ifelse(Catches$Fleet=="PS_SPF_0_0_0","PS_SPF_0_0_0","Other")
Catches$metier2<-ifelse(Catches$Fleet=="OTM_SPF_32-69_0_0_all","OTM_SPF_32-69_0_0_all",Catches$metier2)
Catches$metier2<-ifelse(Catches$Fleet=="GNS_DEF_all_0_0_all","GNS_DEF_all_0_0_all",Catches$metier2)
Catches$metier2<-ifelse(Catches$Fleet=="PTM_SPF_16-31_0_0_all","PTM_SPF_16-31_0_0_all",Catches$metier2)
Catches$metier2<-ifelse(Catches$Fleet=="OTM_SPF_16-31_0_0","OTM_SPF_16-31_0_0",Catches$metier2)
Catches$metier2<-ifelse(Catches$Fleet=="PTM_SPF_32-69_0_0_all","PTM_SPF_32-69_0_0_all",Catches$metier2)
Catches$metier2<-ifelse(Catches$Fleet=="OTB_SPF_32-69_0_0_all","OTB_SPF_32-69_0_0_all",Catches$metier2)
Catches$metier2<-ifelse(Catches$Fleet=="SSC_DEF_70-99_0_0_all","SSC_DEF_70-99_0_0_all",Catches$metier2)

Catches$metier2<-factor(Catches$metier2, levels=c("GNS_DEF_all_0_0_all","OTB_SPF_32-69_0_0_all","OTM_SPF_16-31_0_0","OTM_SPF_32-69_0_0_all","PS_SPF_0_0_0","PTM_SPF_16-31_0_0_all","PTM_SPF_32-69_0_0_all","SSC_DEF_70-99_0_0_all","Other"))



### We use data from 2002 to compare data among countries, as Scotland did not report data from previous years
Catch2002<-subset(Catches, Year>2001)

mkdir("output")

inpDir <- ("data")
outDir <- ("output")

finalyear<-max(Catches$Year)


### Generate summary catches table for last 2 years
yr0_yr1 = c(finalyear,finalyear-1)
summarycatches = Catch2002[Catch2002$Year %in% yr0_yr1,]
summarycatches = summarycatches[,c("Caton","Year","Country","CatchCategory")]
summarycatches$Caton = summarycatches$Caton/1000 
summarycatches$Caton = round(summarycatches$Caton)
summarycatches = tidyr::pivot_wider(summarycatches,names_from = CatchCategory, values_from = Caton, values_fn = ~sum(.x, na.rm = TRUE))
a = data.frame(summarycatches[summarycatches$Year==yr0_yr1[1],][,c("Country", "BMS landing", "Discards", "Landings")])
b = data.frame(summarycatches[summarycatches$Year==yr0_yr1[2],][,c("Country", "BMS landing", "Discards", "Landings")])
row.names(a) = a$Country
row.names(b) = b$Country
a = a[,c("BMS.landing", "Discards", "Landings")]
b = b[,c("BMS.landing", "Discards", "Landings")]
adup = a; bdup = b
adup[is.na(adup)]=0
bdup[is.na(bdup)]=0
a$Total = adup$BMS.landing + adup$Discards + adup$Landings
b$Total = bdup$BMS.landing + bdup$Discards + bdup$Landings

write.csv(a,paste0(outDir,'/catchsummary',yr0_yr1[1],'.csv'))
write.csv(b,paste0(outDir,'/catchsummary',yr0_yr1[2],'.csv'))


############################################################
#################### 2. Exploratory analysis
############################################################

############# Catch data by category

#Discards reported in 2005 by the UK were very high and not representative of the fishery. It was when the sampling programme for discards started and they sampled vessels not targeting sardine.I'll delete this value


Catch2002b<-subset(Catch2002, !(Year==2005&Country=="UK (England)"&CatchCategory=="Discards"&Caton==62327359
))

CatchCat<- ddply(Catch2002b,.(Year, CatchCategory),summarize,Catch=sum(Caton)/1000) 

CatchCat_advicesheet<-reshape(CatchCat,direction="wide",idvar="Year", timevar="CatchCategory")

plot1<-ggplot(CatchCat, aes(Year,Catch, fill=CatchCategory))+
  geom_col()+ylab("Catch (t)")+xlab("")+scale_x_continuous(breaks=seq(2002,finalyear,5))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),axis.text.y=element_text(size=13),legend.text = element_text(size=13),legend.title=element_text(size=13),strip.text.x = element_text(size=13),axis.title = element_text(size=13), legend.key.size = unit(1,'cm'),legend.position="bottom")+guides(fill=guide_legend(ncol=4, title=""))+labs(fill="Catch category")

png(paste(outDir,'CatchbyCat.png', sep="/"), width=808, height=538)
plot1
dev.off()

############ Landings

Lan2002<-subset(Catch2002, CatchCategory=="Landings"|CatchCategory=="BMS landing")

Lan2002$CatchCategory<-factor(Lan2002$CatchCategory)
summary(Lan2002)

landSeason<- ddply(Lan2002,.(Year, Season),summarize,Catch=sum(Caton)/1000) 


landCountry<- ddply(Lan2002,.(Country,Year),summarize,OfficialLand=sum(OfficialLandings[!is.na(OfficialLandings)]/1000), Caton=sum(Caton[!is.na(Caton)])/1000) 

landCountry<- ddply(Lan2002,.(Year, Country),summarize,Catch=sum(Caton[!is.na(Caton)])/1000) 
library (reshape)
table1<-cast(landCountry, Year~Country)


plot2<-ggplot(landCountry, aes(Year,Catch, fill=Country))+
  geom_col()+ylab("Landings (t)")+xlab("") +scale_x_continuous(breaks=seq(2002,finalyear,5))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),axis.text.y=element_text(size=13),legend.text = element_text(size=13),legend.title=element_text(size=13),axis.title = element_text(size=13), legend.key.size = unit(1,'cm'), legend.position="right")+guides(fill=guide_legend(ncol=1, title=""))+scale_fill_manual(values=c("#F8766D","#D39200","#93AA00","#00BA38","#00C19F","#00B9E3","#619CFF","#DB72FB","#FF61C3","#FF4533"))

png(paste(outDir,'CatchbyCountry.png', sep="/"), width=870, height=538)
plot2
dev.off()




###################### ICES Division

landDiv<- ddply(Lan2002,.(Year,Area, Country),summarize,Landings=sum(Caton)/1000) 


plot3<-ggplot(landDiv, aes(Year,Landings, fill=Area))+
  geom_col()+ylab("Landings (t)")+facet_wrap(.~Country)+
  xlab("")+scale_x_continuous(breaks=seq(2002,finalyear,5))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),axis.text.y=element_text(size=13),legend.text = element_text(size=13),legend.title=element_text(size=13),axis.title = element_text(size=13), legend.key.size = unit(1,'cm'), legend.position="bottom")+guides(fill=guide_legend(ncol=5, title=""))+
scale_fill_manual(values=c("#F8766D","#D39200","#93AA00","#00BA38","#00C19F","#3288bd","#619CFF","#DB72FB","#FF61C3","#FF4533","#A50026"))

png(paste(outDir,'CatchbyCoun&div.png', sep="/"), width=870, height=538,pointsize = 13)
plot3
dev.off()


landDiv<- ddply(Lan2002,.(Year,Area),summarize,Landings=sum(Caton)/1000) 
Land_div2<-reshape(landDiv,direction="wide",idvar="Year", timevar="Area")


plot4<-ggplot(landDiv, aes(Year,Landings, fill=Area))+
  geom_col()+ylab("Landings (t)")+
  xlab("")+scale_x_continuous(breaks=seq(2002,finalyear,5))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),axis.text.y=element_text(size=13),legend.text = element_text(size=13),legend.title=element_text(size=13),axis.title = element_text(size=13), legend.key.size = unit(1,'cm'), legend.position="bottom")+guides(fill=guide_legend(ncol=5, title=""))+
  scale_fill_manual(values=c("#F8766D","#D39200","#93AA00","#00BA38","#00C19F","#3288bd","#619CFF","#DB72FB","#FF61C3","#FF4533","#A50026"))
png(paste(outDir,'CatchbyDiv.png', sep="/"), width=870, height=538,pointsize = 13)
plot4
dev.off()

########### METIERS


metier<- ddply(Lan2002,.(Year,metier2),summarize,Landings=sum(Caton)/1000) 

plot5<-ggplot(metier, aes(Year,Landings, fill=metier2))+
  geom_col()+ylab("Landings (t)")+xlab("")+labs(fill="Fleet")+scale_x_continuous(breaks=seq(2002,finalyear,5))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),axis.text.y=element_text(size=13),legend.text = element_text(size=13),legend.title=element_text(size=13),axis.title = element_text(size=13), legend.key.size = unit(1,'cm'), legend.position="bottom")+guides(fill=guide_legend(ncol=3, title=""))+
  scale_fill_manual(values=c("#F8766D","#D39200","#93AA00","#00BA38","#00C19F","#3288bd","#619CFF","#DB72FB","#FF61C3"))

png(paste(outDir,'CatchbyFleet.png', sep="/"), width=870, height=538,pointsize = 13)
plot5
dev.off()



### Landings by quarter

LandQuarter <- ddply(Lan2002,.(Year,Season, Country),summarize,Landings=sum(Caton)/1000) 

ggplot(LandQuarter, aes(Year,Landings, group=factor(Season), fill=factor(Season)))+
geom_col()+ylab("Landings (t)")+facet_wrap(.~Country,scales="free_y")+xlab("Year")+labs(fill="Quarter")+scale_x_continuous(breaks=seq(1987,2019,4))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5))



LandQuarter <- ddply(Lan2002,.(Year,Season),summarize,Landings=sum(Caton)/1000) 

plot6<-ggplot(LandQuarter, aes(Year,Landings, group=factor(Season), col=factor(Season)))+
  geom_line (size=1.3)+ylab("Landings (t)")+xlab("")+labs(col="Quarter")+scale_x_continuous(breaks=seq(2002,finalyear,5))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),axis.text.y=element_text(size=13),legend.text = element_text(size=13),legend.title=element_text(size=13),axis.title = element_text(size=13), legend.key.size = unit(1,'cm'), legend.position="right")+guides(fill=guide_legend(ncol=4, title="Quarter"))

png(paste(outDir,'CatchbyQ.png', sep="/"), width=870, height=538,pointsize = 13)
plot6

dev.off()
















if(T){# To rerun length data plots from last year (2021 assessment)
  ##### Modified from: Rosana Ourens
  ##### 19.11.2021
  
  library(plyr)
  library(ggplot2)
  
  ############################################### get the size data
  
  # Load pre-aggregated, anonymised data. Anonymising script is saved to C:\Users\JR13\OneDrive - CEFAS\My onedrive documents\TAF_non_anonymised\anonymising_script.R    - the script removes vessel names and identifying info (Joseph Ribeiro, CEFAS).
  LengthDat <- read.csv(paste0(wdir,"/bootstrap/initial/data/anonymous_PIL_db_agg1718192021.csv"), header=T)
  
  #Delete data 2021 because there is only info from January:
  LengthDat<-subset(LengthDat, length_cm>0&year<2021)
  
  
  ########## Size distribution provided by the producers
  
  LengthProd<-subset(LengthDat, source=="processor"&length_cm>0)
  LengthProd$Source_name<-factor(LengthProd$Source_name)
  
  Size_data<-ddply(LengthProd,.(length_cm, year, Source_nameAn), summarize, CANUM=sum(n))
  
  # Slide 8
  producers<-ggplot(Size_data, aes(length_cm,CANUM, fill=Source_nameAn)) + geom_density(stat="identity", alpha=0.8) + 
   facet_grid(year~., scales="free_y")+
    ylab("N")+xlab("Length (cm)")+labs(fill="", col="")#+
    #geom_vline(data=ddply(Size_data,.(year, Source_nameAn), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM)), aes(xintercept=meansize, col=Source_nameAn),linetype=2, size=1.2) 
  
  png(paste(outDir,'ProducersDat_2021.png', sep="/"), width=687, height=644)
  producers
  dev.off()
  
  
  
  ########## Size distribution provided by  fishers
  
  LengthFish<-subset(LengthDat, source=="fisher"&length_cm>0)
  
  Size_data<-ddply(LengthFish,.(length_cm,year,Source_nameAn), summarize, CANUM=sum(n))
  
  # slide 8
  fishers<-ggplot(Size_data, aes(length_cm,CANUM, fill=Source_nameAn)) + geom_density(stat="identity", alpha=0.8) + 
    facet_grid(year~., scales="free_y")+
    ylab("N")+xlab("Length (cm)")+labs(fill="", col="")#+
   # geom_vline(data=ddply(Size_data,.(year, Source_nameAn), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM)), aes(xintercept=meansize, col=Source_nameAn),linetype=2, size=1.2) 
  png(paste(outDir,'FishersDat_2021.png', sep="/"), width=687, height=644)
  fishers
  dev.off()
  
  
  
  
  ################################ Size distribution of the samples by year and source
  
  ## Delete year 2017 and processor 2 because data is too spiky
  
  LengthClean<-subset(LengthDat, year>2017& LengthDat$Source_nameAn!="Processor 2")
  
  Size_data<-ddply(LengthClean,.(length_cm, year, source), summarize, CANUM=sum(n))
  meansize<-ddply(Size_data,.(year, source), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM))
  
  
  ggplot(Size_data, aes(length_cm,CANUM, fill=source)) + geom_density(stat="identity", alpha=0.8, position="stack") + 
    facet_grid(year~., scales="free_y")+
    ylab("N")+xlab("Length (cm)")+
    geom_vline(data=ddply(Size_data,.(year, source), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM)), aes(xintercept=meansize, col=source),linetype=2, size=1.2) 
  
  # Slide 9
  aggregDat<-ggplot(Size_data, aes(length_cm,CANUM, fill=source)) + geom_density(stat="identity", alpha=0.8) + 
    facet_grid(source~year, scales="free_y")+
    ylab("N")+xlab("Length (cm)")+
    geom_vline(data=ddply(Size_data,.(year, source), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM)), aes(xintercept=meansize, col=source),linetype=2, size=1.2) 
  
  
  png(paste(outDir,'aggregLength_2021.png', sep="/"), width=749, height=529)
  aggregDat
  dev.off()
  
  ## pooled data by year
  Size_data<-ddply(LengthClean,.(length_cm, year), summarize, CANUM=sum(n))
  meansize<-ddply(Size_data,.(year), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM))
  
  # slide 10
  aggregDat2<-ggplot(Size_data, aes(length_cm,CANUM, fill=factor(year))) + geom_density(stat="identity", alpha=0.8) + 
    facet_grid(year~., scales="free_y")+
    ylab("N")+xlab("Length (cm)")+
    geom_vline(data=ddply(Size_data,.(year), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM)), aes(xintercept=meansize, col=factor(year)) )
  
  png(paste(outDir,'combinedLength_2021.png', sep="/"), width=749, height=529)
  aggregDat2
  dev.off()
}






# Tweaking the above for the latest data from Silvia Rodriguez-Climent (CEFAS), received Sept 2022. There is an issue with the data this year:
# "There are 2 main files: 1 for the processors and 1 for the fishers (metadata from both files attached for better understanding of the nomenclature used).
# While preparing the data for the fishers, I realised that right now when we have more than one haul a day, we cannot associated with the recorded length measurements, because the fishers do not complete the “number of haul” field in the logbooks: we are working to fix this in the future, but for now you can only see data for the fishers when only one haul was carried out on the day (otherwise correspondence with the measurements was not possible).
# 
# I've also created a document on how the sampling is done that hopefully will help to understand all the process. The GitHub is not fully functional yet, but It will be soon.
# I've only prepare this data for last year season: 2020-2021, but potentially I can retrospectively prepare it for the other years, so let me know if this is what you are looking for and I can work from there!
# 
# Any questions please let me know, 
# Kind regards, 
# Silvia"

library(plyr)
library(ggplot2)
library(Matrix.utils)


############################################### get the size data
### I suspect this is how the data were aggregated as the first few plots come out the same, but there are missing data in this non aggregated file - the 'fishers' source is missing and it aggregates to fewer rows.

# Load data
LengthDat <- read.csv(paste0(wdir,"/bootstrap/initial/data/anonymous_selfsampling_ICES2022.csv"), header=T)



# #Delete data 2022 because there is only info from January and February:
LengthDat<-subset(LengthDat, length_cm>0&year<2022)
LengthDat$Source_name<-LengthDat$Source_nameAn
LengthDat$Source_name<-factor(LengthDat$Source_name)


  
########## Size distribution provided by the producers

LengthProd<-subset(LengthDat, source=="processor"&length_cm>0)
LengthProd$Source_name<-factor(LengthProd$Source_name)

Size_data<-ddply(LengthProd,.(length_cm, year, Source_name), summarize, CANUM=sum(n))

# Slide 8
producers<-ggplot(Size_data, aes(length_cm,CANUM, fill=Source_name)) + geom_density(stat="identity", alpha=0.8) + 
 facet_grid(year~., scales="free_y")+
  ylab("N")+xlab("Length (cm)")+labs(fill="", col="")#+
  #geom_vline(data=ddply(Size_data,.(year, Source_nameAn), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM)), aes(xintercept=meansize, col=Source_nameAn),linetype=2, size=1.2) 

png(paste(outDir,'ProducersDat.png', sep="/"), width=687, height=644)
producers
dev.off()



########## Size distribution provided by fishers

LengthFish<-subset(LengthDat, source=="fishers"&length_cm>0)

Size_data<-ddply(LengthFish,.(length_cm,year,Source_name), summarize, CANUM=sum(n))

# slide 8
fishers<-ggplot(Size_data, aes(length_cm,CANUM, fill=Source_name)) + geom_density(stat="identity", alpha=0.8) + 
  facet_grid(year~., scales="free_y")+
  ylab("N")+xlab("Length (cm)")+labs(fill="", col="")#+
 # geom_vline(data=ddply(Size_data,.(year, Source_nameAn), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM)), aes(xintercept=meansize, col=Source_nameAn),linetype=2, size=1.2) 
png(paste(outDir,'FishersDat.png', sep="/"), width=687, height=644)
fishers
dev.off()


################################ Size distribution of the samples by year and source


Size_data<-ddply(LengthDat,.(length_cm, year, source), summarize, CANUM=sum(n))
meansize<-ddply(Size_data,.(year, source), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM))


ggplot(Size_data, aes(length_cm,CANUM, fill=source)) + geom_density(stat="identity", alpha=0.8, position="stack") + 
  facet_grid(year~., scales="free_y")+
  ylab("N")+xlab("Length (cm)")+
  geom_vline(data=ddply(Size_data,.(year, source), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM)), aes(xintercept=meansize, col=source),linetype=2, size=1.2) 

# Slide 9
aggregDat<-ggplot(Size_data, aes(length_cm,CANUM, fill=source)) + geom_density(stat="identity", alpha=0.8) + 
  facet_grid(source~year, scales="free_y")+
  ylab("N")+xlab("Length (cm)")+
  geom_vline(data=ddply(Size_data,.(year, source), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM)), aes(xintercept=meansize, col=source),linetype=2, size=1.2) 


png(paste(outDir,'aggregLength.png', sep="/"), width=749, height=529)
aggregDat
dev.off()

## pooled data by year
Size_data<-ddply(LengthDat,.(length_cm, year), summarize, CANUM=sum(n))
meansize<-ddply(Size_data,.(year), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM))

# slide 10
aggregDat2<-ggplot(Size_data, aes(length_cm,CANUM, fill=factor(year))) + geom_density(stat="identity", alpha=0.8) + 
  facet_grid(year~., scales="free_y")+
  ylab("N")+xlab("Length (cm)")+
  geom_vline(data=ddply(Size_data,.(year), summarize, meansize=sum(CANUM*length_cm)/sum(CANUM)), aes(xintercept=meansize, col=factor(year)) )

png(paste(outDir,'combinedLength.png', sep="/"), width=749, height=529)
aggregDat2
dev.off()


