
#######################################################################################################################################
############################## This code is being retained for reference in case it is used again. ####################################
############################## Replaces french records in Intercatch with an update                ####################################
#######################################################################################################################################

library(rstudioapi) # This has been written as an R studio project using RStudio Version 1.4.1717 
library(icesTAF)
library(icesTAF)
library(plyr)
library(ggplot2)
library(reshape)


try(setwd(dirname(getActiveDocumentContext()$path)))
wdir = getwd(); print(wdir)

Catchdat = read.taf(paste0(wdir,"/bootstrap/data/Intercatch.csv"))


####################################
################ 1. Tuning the data
#####################################
summary(Catchdat)
Catchdat<-subset(Catchdat, Caton>0)
Catchdat$Country<-as.factor(Catchdat$Country)
Catchdat$CatchCategory<-as.factor(Catchdat$CatchCategory)



################# FRANCE updated their landings for the benchmark but the reviewed landings were not uploaded to Intercatch yet (check data for WG2022).We add them manually for the assessment this year:

########### We need to raplace the data from France in intercatch with the data from the ascension folder. The data in intercatch was only for three years and it did not include information by rectangle
Catchdat<-subset(Catchdat,!(Country=="France"&CatchCategory=="Landings"&Year>1999&Year<2020))

France_data_final<-read.csv(paste0(wdir,"/bootstrap/data/review landings_France.csv"))
Catchdat<-rbind(Catchdat,France_data_final)

write.taf(Catchdat,paste0(wdir,"/bootstrap/initial/data/Catchdata.csv"),quote=TRUE)

# ################## DENMARK
# 
# ############## Denmark sent an update of their landings by gear for the benchmark. 
# ### New data need to be formatted and replace the information in 'landings.csv'
# 
# 
# 
# files=list.files(path = "C:/Users/ro03/Documents/Sardine/WKWEST/Intercatch/Input/InterCatch_DNK", pattern = "*.csv", full.names = T)
# dataDNK = sapply(files, function(f) {
#   
#   database<- read.csv(f,header=F)},simplify=FALSE,USE.NAMES=F) %>% 
#   bind_rows(.id = "id")
# 
# landDNK<-subset(dataDNK,V1=="SI")
# landDNK$Country<-"Denmark"
# landDNK$CATONRaisedOrImported<-"Imported_Data"
# landDNK$Usage<-NA
# landDNK$Effort<-NA ## No effort data provided
# landDNK$UnitEffort<-NA
# 
# landDNK<-landDNK[,c(4,35,9,13,36,5,6,7,14,15,20,21,37,38,39)]
# landDNK<-rename(landDNK,Year=V3,Area=V8,CatchCategory=V12,SeasonType=V4,Season=V5,Fleet=V6,ReportingCategory=V13,MisreportedArea=V14,Caton=V19,OfficialLandings=V20)
# landDNK$CatchCategory<-"Landings"#only landings reported
# landDNK$ReportingCategory<-"Reported"
# landDNK$Caton<-landDNK$Caton*1000 # tranform in kg
# landDNK$OfficialLandings<-landDNK$OfficialLandings*1000
# 
# ## The landings are the same as before. they only changed the gear. 
# 
# landDNK$Area<-revalue(landDNK$Area,c("27.7.d "="27.7.d","27.7.e "="27.7.e", "27.7.h "="27.7.h", "27.7.f "="27.7.f","27.7.j "="27.7.j"))
# landDNK$Fleet<-revalue(landDNK$Fleet,c("OTM_SPF_32-69_0_0_all "="OTM_SPF_32-69_0_0_all"))
# 
# 
# Catchdat<-subset(Catchdat,!(Country=="Denmark"&Year<2020))
# Catchdat<-rbind(Catchdat,landDNK)
# 
# write.taf(Catchdat,paste0(wdir,"/bootstrap/data/Catches.csv"),quote=TRUE)
