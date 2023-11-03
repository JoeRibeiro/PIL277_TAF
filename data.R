renv::snapshot()

## Preprocess data, write TAF data tables
# There are many data objects defined in this script that are plotted in output.R, but they should instead be saved to intermediate files

library(rstudioapi) # This has been written as an R studio project using RStudio Version 1.4.1717 
library(icesTAF)
library(icesTAF)
library(plyr)
library(ggplot2)
library(reshape)

try(setwd(dirname(getActiveDocumentContext()$path)))
wdir = getwd(); print(wdir)


# Get the last year in the biomass timeseries. This will be the current year for the peltic survey just done. 
thisyear = max(read.csv("bootstrap/data/Index_bootstrap.csv", fileEncoding = 'UTF-8-BOM', stringsAsFactors=FALSE, na.strings=".")$year)
finalyear = thisyear - 1 # The last year with complete landings data will be the previous year
cat(thisyear,file=paste0(wdir,"/bootstrap/data/thisyear.txt"),sep="\n") # Where possible, define everything relative to this.

# Begin data wrangling. Combine Catch
Catches=read.taf(paste0(wdir,"/bootstrap/data/Catches.csv"))
Catches2022=read.taf(paste0(wdir,"/bootstrap/data/intercatch_23_StockOverview.csv"))
Catches2022=dplyr::rename(Catches2022,SeasonType = Season.type)
Catches2022$Stock <- NULL
Catches2022$Caton = Catches2022$Catch..kg*1000
Catches2022$Catch..kg <- NULL
Catches2022=dplyr::rename(Catches2022,  Fleet =  Fleets   )
Catches2022=dplyr::rename(Catches2022,   ReportingCategory = Report.cat.   )
Catches2022=dplyr::rename(Catches2022, CatchCategory  = Catch.Cat.   )
Catches2022=dplyr::rename(Catches2022, CATONRaisedOrImported  = Discards.Imported.Or.Raised  )
Catches=plyr::rbind.fill(Catches,Catches2022)

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


write.taf(Catches,paste0(wdir,"/bootstrap/data/Catches_metier.csv"),quote=TRUE)

mkdir("output")
inpDir <- ("data")
outDir <- ("output")



############################################################
#################### 2. Exploratory analysis
############################################################

############# Catch data by category

#Discards reported in 2005 by the UK were very high and not representative of the fishery. It was when the sampling programme for discards started and they sampled vessels not targeting sardine.I'll delete this value


Catch2002b<-subset(Catch2002, !(Year==2005&Country=="UK (England)"&CatchCategory=="Discards"&Caton==62327359
))

CatchCat<- ddply(Catch2002b,.(Year, CatchCategory),summarize,Catch=sum(Caton)/1000) 

CatchCat_advicesheet<-reshape(CatchCat,direction="wide",idvar="Year", timevar="CatchCategory")

if(!"Biomass index (total area)" %in% colnames(CatchCat_advicesheet)){CatchCat_advicesheet$`Biomass index (total area)` = 0}
if(!"High" %in% colnames(CatchCat_advicesheet)){CatchCat_advicesheet$High = 0}
if(!"Low" %in% colnames(CatchCat_advicesheet)){CatchCat_advicesheet$Low = 0}

write.taf(CatchCat_advicesheet,"bootstrap/data/Catch_advice sheet.csv")

############ Landings

Lan2002<-subset(Catch2002, CatchCategory=="Landings"|CatchCategory=="BMS landing")

Lan2002$CatchCategory<-factor(Lan2002$CatchCategory)

landSeason<- ddply(Lan2002,.(Year, Season),summarize,Catch=sum(Caton)/1000) 



landCountry<- ddply(Lan2002,.(Country,Year),summarize,OfficialLand=sum(OfficialLandings[!is.na(OfficialLandings)]/1000), Caton=sum(Caton[!is.na(Caton)])/1000) 

landCountry<- ddply(Lan2002,.(Year, Country),summarize,Catch=sum(Caton[!is.na(Caton)])/1000) 
table1<-cast(landCountry, Year~Country)
if(!"Spain" %in% colnames(table1)){table1$Spain = 0}









###################### ICES Division

landDivA<- ddply(Lan2002,.(Year,Area, Country),summarize,Landings=sum(Caton)/1000) 




landDivb<- ddply(Lan2002,.(Year,Area),summarize,Landings=sum(Caton)/1000) 
Land_div2<-reshape(landDivb,direction="wide",idvar="Year", timevar="Area")


########### METIERS


metier<- ddply(Lan2002,.(Year,metier2),summarize,Landings=sum(Caton)/1000) 



### Landings by quarter

LandQuarter1 <- ddply(Lan2002,.(Year,Season, Country),summarize,Landings=sum(Caton)/1000) 


LandQuarter2 <- ddply(Lan2002,.(Year,Season),summarize,Landings=sum(Caton)/1000) 


# Rename columns for the next section (Spict)
landSeason<-dplyr::rename(landSeason,Quarter=Season,landings=Catch)

mkdir('data')

print("data stuff done, now try and save it all as rdata file...")
save.image(paste0(wdir,"/data/processed_data_various.RData"))
print("This line should show if data.r ran succesfully")






# section here to create table 6 from "Catches"
latest_by_metier=metier[metier$Year==finalyear,]
oth_class = latest_by_metier$Landings[latest_by_metier$metier2 %in%c("Other","GNS_DEF_all_0_0_all")]
pel_trawl_class = latest_by_metier$Landings[latest_by_metier$metier2 %in% c("OTM_SPF_16-31_0_0","OTM_SPF_32-69_0_0_all","	
OTB_SPF_32-69_0_0_all","PTM_SPF_16-31_0_0_all")]
ps_class = latest_by_metier$Landings[latest_by_metier$metier2 %in% c("PS_SPF_0_0_0","SSC_DEF_70-99_0_0_all")]

# As far as I can tell, this must have been how they were defined
landingssum = sum(latest_by_metier$Landings)
catchsum = landingssum
purse_seine_perc = round((sum(ps_class)/landingssum)*100)
pelagic_trawl_perc = round((sum(pel_trawl_class)/landingssum)*100)
other_perc = round((sum(oth_class)/landingssum)*100)
#if(sum(other_perc,purse_seine_perc,pel_trawl_class)!=100){other_perc=other_perc+1}

save(list=c("landingssum","catchsum","purse_seine_perc","pelagic_trawl_perc","other_perc"), file='data/catches_by_category_table6.rdata')






# section here aims to create table 7. 
# denmark aligns most years regardless of whether Catons or OfficialLandings is used. But 2012 does not match.
# lithuania aligns apart from 2006
# belgium aligns apart from 2018
# uk aligns relatively well but not exactly...

# Since I can't get the numbers to align, I load the old table as a csv and append a new row

# Aggregate landings
library(tidyr)
Lan2002<-subset(Catch2002, CatchCategory=="Landings"|CatchCategory=="BMS landing")
Lan2002$CatchCategory<-factor(Lan2002$CatchCategory)
landCountry<- ddply(Lan2002,.(Year, Country),summarize,Catch=sum(OfficialLandings[!is.na(OfficialLandings)])/1000) 
t7 <- landCountry %>%  pivot_wider(names_from = Country, values_from = Catch)

# Add spain if absent
if(!"Spain" %in% colnames(t7)){t7$Spain = 0}

# Add Eng and scotland together
t7["United.Kingdom"] = t7["UK (England)"] +  t7["UK(Scotland)"]
t7["UK (England)"] <- NULL
t7["UK(Scotland)"] <- NULL

# Should be dataframe not tibble
t7 = as.data.frame(t7)

# Order columns to be like old table     
official_landings_7 = read.csv("bootstrap/data/Catches_reported.csv", fileEncoding = 'UTF-8-BOM',colClasses="character", stringsAsFactors=FALSE, na.strings=".")
t7 = t7[colnames(official_landings_7)]

# NA to 0
t7[is.na(t7)] = 0

# Add new catch rows on to the old table. This will go into the report
t7=rbind(official_landings_7,t7[t7$Year>2019,])
write.taf(t7,"bootstrap/data/Catches_table7.csv")

