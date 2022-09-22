
#############################################################################################
############################# 0: Define basis for the advice ################################
#############################################################################################


try(setwd(dirname(getActiveDocumentContext()$path)))
wdir = getwd(); print(wdir)

outDir="output"
mkdir(outDir)

load("data/processed_data_various.RData")

# These are outputs from previously constructed objects
library(rstudioapi) # This has been written as an R studio project using RStudio Version 1.4.1717 
library(ggplot2)
library(spict)
library(xlsx)



# Historic and "Official landings" tables from previous advice
historic_advice_5a = read.csv("bootstrap/data/Advice_history_a.csv", fileEncoding = 'UTF-8-BOM',colClasses="character", stringsAsFactors=FALSE, na.strings=".",check.names=FALSE)
historic_advice_5b = read.csv("bootstrap/data/Advice_history_b.csv", fileEncoding = 'UTF-8-BOM',colClasses="character", stringsAsFactors=FALSE, na.strings=".")
official_landings_7 = read.csv("bootstrap/data/Catches_table7.csv", fileEncoding = 'UTF-8-BOM',colClasses="character", stringsAsFactors=FALSE, na.strings=".")
catchadvicesheet = read.csv("bootstrap/data/Catch_advice sheet.csv")



# We can at least construct half of table 8
# Attempting to recreate high and low columns
biomass_land_disc_8_in = read.csv("bootstrap/data/Index_bootstrap.csv", fileEncoding = 'UTF-8-BOM', stringsAsFactors=FALSE, na.strings=".")
biomass_land_disc_8_in$Year = biomass_land_disc_8_in$year; biomass_land_disc_8_in$year <- NULL
biomass_land_disc_8_in$Biomass.index..total.area. = biomass_land_disc_8_in$Biomass_sum_mean; biomass_land_disc_8_in$Biomass_sum_mean <- NULL
biomass_land_disc_8_in$High = biomass_land_disc_8_in$Biomass.index..total.area. + (biomass_land_disc_8_in$Biomass_sum_sd * 2)
biomass_land_disc_8_in$Low = biomass_land_disc_8_in$Biomass.index..total.area. - (biomass_land_disc_8_in$Biomass_sum_sd * 2)
biomass_land_disc_8_in = biomass_land_disc_8_in[,c("Year","Biomass.index..total.area.","High","Low")]

tomerge=data.frame('Landings'=round(as.numeric(catchadvicesheet[,'Catch.Landings'])),'Discards'=round(as.numeric(catchadvicesheet[,'Catch.Discards'])),'BMS.landing'=round(as.numeric(catchadvicesheet[,'Catch.BMS.landing'])),'Year'=catchadvicesheet[,'Year'])
biomass_land_disc_8 = merge(biomass_land_disc_8_in,tomerge,by='Year',all=T)
biomass_land_disc_8 = round(biomass_land_disc_8)
biomass_land_disc_8[is.na(biomass_land_disc_8)] <- "" # else it prints as NA in the report

# Assessment year
EndYear = read.table(paste0(wdir,"/bootstrap/data/thisyear.txt"))[[1]]


# Mean catches over 2y
catchadvicesheet$catch_summed = catchadvicesheet$Catch.Landings + catchadvicesheet$Catch.BMS.landing
prev2years=catchadvicesheet[catchadvicesheet$Year %in% c(EndYear-2, EndYear-1),]
mean_2prevyears=round(sum(prev2years$catch_summed)/2)



# Catch and Biomass index data for different ranges of time, all of which are used further on
added_landings = as.double(biomass_land_disc_8$Landings) + as.double(biomass_land_disc_8$BMS.landing)
Valid_rows_biom=is.finite(as.double(biomass_land_disc_8$Biomass.index..total.area.))
Valid_rows_catch_biom=is.finite(as.double(biomass_land_disc_8$Biomass.index..total.area.)) & is.finite(as.double(biomass_land_disc_8$Landings))
Iseries_full = as.double(biomass_land_disc_8[Valid_rows_biom,]$Biomass.index..total.area.)
Cseries_full = added_landings[Valid_rows_biom]
Ihist = Iseries_full[1:length(Iseries_full)-1] # Ihist is the available historical series of the abundance index, but not the last available year. 
Iseries_overlap = as.double(biomass_land_disc_8[Valid_rows_catch_biom,]$Biomass.index..total.area.)
Cseries_overlap = added_landings[Valid_rows_catch_biom]

# Define the basis for advice
historic_average_index = c(0,0) # We are averaging the year before with the year before that. Starting with these two zeros align the historic averages with biomass_land_disc_8[Valid_rows_biom,]$Year
for(rw in 2:length(Iseries_full)){historic_average_index = append(historic_average_index,(Iseries_full[rw-1]+Iseries_full[rw-2])/2)} # average the year before with the year before that
historic_average_catch = c(0,0) # We are averaging the year before with the year before that. Starting with these two zeros align the historic averages with biomass_land_disc_8[Valid_rows_biom,]$Year
for(rw in 2:length(Cseries_full)){historic_average_catch = append(historic_average_catch,(Cseries_full[rw-1]+Cseries_full[rw-2])/2)} # average the year before with the year before that

index_1o2 = Iseries_full/historic_average_index
advised_catch = index_1o2*historic_average_catch

modeladvisedcatch = round(advised_catch[length(advised_catch)])
advisedcatch = modeladvisedcatch
index_latest = Iseries_full[length(Iseries_full)]
index_previous = round((Iseries_full[length(Iseries_full)-1]+Iseries_full[length(Iseries_full)-2])/2)
discard_rate='Negligible' # In time, this could change to something that is calculated
advice_change='Not applicable' # In time, this could change to something that is calculated. Unsure why this is not applicable.

# Calculate some index related things
if(index_latest>index_previous){change = 'increase'} 
if(index_latest<index_previous){change = 'decrease'} 
index_ratio_sign = round(100*(1-(index_latest/index_previous)))
index_ratio = abs(index_ratio_sign)
index_ratio_inv = (100-index_ratio)/100

# Basis for advice - index cap?
uncertaintycap = 'Not applied'
uncertaintycapval = ''
if(index_ratio>80){uncertaintycapval = '0.8'; uncertaintycap = 'Applied'}
if(index_ratio<20){uncertaintycapval = '0.2'; uncertaintycap = 'Applied'}

# Basis for advice - biomass safeguard?
biomass_safeguard_val = ''
biomass_safeguard = 'Not applicable'
if(index_latest<biomass_safeguard_val){advisedcatch = modeladvisedcatch *(index_latest/biomass_safeguard_val)
                                       biomass_safeguard = 'Applied'
                                       biomass_safeguard_val = round(exp(mean(log(Ihist))) * exp(-1.645*sd(log(Ihist))))}



# Get full stops out of title names
names(historic_advice_5a) <- gsub(x = names(historic_advice_5a), pattern = "\\.", replacement = " ")
names(historic_advice_5b) <- gsub(x = names(historic_advice_5b), pattern = "\\.", replacement = " ")
names(official_landings_7) <- gsub(x = names(official_landings_7), pattern = "\\.", replacement = " ")
names(biomass_land_disc_8) <- gsub(x = names(biomass_land_disc_8), pattern = "\\.", replacement = " ")
#names() <- gsub(x = names(), pattern = "\\.", replacement = " ")



# Save advice info to R object
save.image(file='output/AdviceInfo.rdata')



#############################################################################################
############################# 1: Plotting underlying landings and catches ###################
#############################################################################################

#pander(CatchCat_advicesheet)
print("Table 7: Sardine in Subarea 7. Assessment summary. The high and low columns represent the 95% confidence intervals of the biomass index. All values are in tonnes.")

write.csv(CatchCat_advicesheet,paste0(wdir,"/output/Catch_advice sheet.csv"))

CatchCat$CatchT=CatchCat$Catch/1000
plot1<-ggplot(CatchCat, aes(Year,CatchT, fill=CatchCategory))+
geom_col(width = 0.4)+ylab("Catches in 1000 t")+xlab("")+scale_x_continuous(breaks=seq(2002,finalyear,5))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),axis.text.y=element_text(size=13),legend.text = element_text(size=13),legend.title=element_text(size=13),strip.text.x = element_text(size=13),axis.title = element_text(size=13), legend.key.size = unit(0.25,'cm'),legend.position="bottom")+guides(fill=guide_legend(ncol=4, title=""))+labs(fill="Catch category")+ 
scale_fill_manual("legend", values = c( "Landings" = "navyblue", "Discards"  = "lightblue",    "BMS landing" = "lightgreen"))  + theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
theme(  panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line( size=.05, color="lightgrey" ) ,
        panel.grid.minor.y = element_line( size=.05, color="lightgrey" ) ,
        axis.text.x = element_text(angle = 0)
)
ggsave(plot1,file=paste(wdir,'/output/CatchbyCat.png', sep="/"), width=6, height=4)

write.xlsx(landSeason, paste(wdir,'/output/Land_Quarter.xlsx', sep="/"))









# panderOptions("table.split.table",Inf)
# panderOptions("keep.line.breaks",T) # Without this, the template is automatically overwritten it seems
# panderOptions("list.style", 40)
# pander(table1)
print("Table 6: Sardine in Subarea 7. History of reported landings; values are presented for each country participating in the fishery. All weights are in tonnes*.")


write.xlsx(table1, paste(wdir,"/output/Land_country.xlsx", sep="/"))


plot2<-ggplot(landCountry, aes(Year,Catch, fill=Country))+
  geom_col()+ylab("Landings (t)")+xlab("") +scale_x_continuous(breaks=seq(2002,finalyear,5))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),axis.text.y=element_text(size=13),legend.text = element_text(size=13),legend.title=element_text(size=13),axis.title = element_text(size=13), legend.key.size = unit(1,'cm'), legend.position="right")+guides(fill=guide_legend(ncol=1, title=""))+scale_fill_manual(values=c("#F8766D","#D39200","#93AA00","#00BA38","#00C19F","#00B9E3","#619CFF","#DB72FB","#FF61C3","#FF4533"))

ggsave(plot2,file=paste(wdir,'/output/CatchbyCountry.png', sep="/"), width=6, height=4)






plot3<-ggplot(landDivA, aes(Year,Landings, fill=Area))+
  geom_col()+ylab("Landings (t)")+facet_wrap(.~Country)+
  xlab("")+scale_x_continuous(breaks=seq(2002,finalyear,5))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),axis.text.y=element_text(size=13),legend.text = element_text(size=13),legend.title=element_text(size=13),axis.title = element_text(size=13), legend.key.size = unit(1,'cm'), legend.position="bottom")+guides(fill=guide_legend(ncol=5, title=""))+
scale_fill_manual(values=c("#F8766D","#D39200","#93AA00","#00BA38","#00C19F","#3288bd","#619CFF","#DB72FB","#FF61C3","#FF4533","#A50026"))

ggsave(plot3,file=paste(wdir,'/output/CatchbyCoun&div.png', sep="/"), width=6, height=4)


write.xlsx(Land_div2, paste(outDir,'Land_division.xlsx', sep="/"))






plot4<-ggplot(landDivb, aes(Year,Landings, fill=Area))+
  geom_col()+ylab("Landings (t)")+
  xlab("")+scale_x_continuous(breaks=seq(2002,finalyear,5))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),axis.text.y=element_text(size=13),legend.text = element_text(size=13),legend.title=element_text(size=13),axis.title = element_text(size=13), legend.key.size = unit(1,'cm'), legend.position="bottom")+guides(fill=guide_legend(ncol=5, title=""))+
  scale_fill_manual(values=c("#F8766D","#D39200","#93AA00","#00BA38","#00C19F","#3288bd","#619CFF","#DB72FB","#FF61C3","#FF4533","#A50026"))
ggsave(plot4,file=paste(wdir,'/output/CatchbyDiv.png', sep="/"), width=6, height=4)





plot5<-ggplot(metier, aes(Year,Landings, fill=metier2))+
  geom_col()+ylab("Landings (t)")+xlab("")+labs(fill="Fleet")+scale_x_continuous(breaks=seq(2002,finalyear,5))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),axis.text.y=element_text(size=13),legend.text = element_text(size=13),legend.title=element_text(size=13),axis.title = element_text(size=13), legend.key.size = unit(1,'cm'), legend.position="bottom")+guides(fill=guide_legend(ncol=3, title=""))+
  scale_fill_manual(values=c("#F8766D","#D39200","#93AA00","#00BA38","#00C19F","#3288bd","#619CFF","#DB72FB","#FF61C3"))

ggsave(plot5,file=paste(wdir,'/output/CatchbyFleet.png', sep="/"), width=6, height=4)




plot6 <- ggplot(LandQuarter1, aes(Year,Landings, group=factor(Season), fill=factor(Season)))+
geom_col()+ylab("Landings (t)")+facet_wrap(.~Country,scales="free_y")+xlab("Year")+labs(fill="Quarter")+scale_x_continuous(breaks=seq(1987,2019,4))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5))


plot7<-ggplot(LandQuarter2, aes(Year,Landings, group=factor(Season), col=factor(Season)))+
  geom_line (size=1.3)+ylab("Landings (t)")+xlab("")+labs(col="Quarter")+scale_x_continuous(breaks=seq(2002,finalyear,5))+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=0.5, size=13),axis.text.y=element_text(size=13),legend.text = element_text(size=13),legend.title=element_text(size=13),axis.title = element_text(size=13), legend.key.size = unit(1,'cm'), legend.position="right")+guides(fill=guide_legend(ncol=4, title="Quarter"))


ggsave(plot7,file=paste(wdir,'/output/CatchbyQ.png', sep="/"), width=6, height=4)






#############################################################################################
############################# 2: Plotting biomass index #####################################
#############################################################################################
biomass_data = biomass_land_disc_8_in
biomass_data=biomass_data[is.finite(biomass_data$Biomass.index..total.area.),]
biomass_data$Biomass = biomass_data$Biomass.index..total.area./1000
biomass_data$Low = biomass_data$Low/1000
biomass_data$High = biomass_data$High/1000

bimplt <-   ggplot(biomass_data, aes(x=Year, y=Biomass)) + 
    geom_line(aes(y = Biomass),color="darkgreen",size=1) +
    geom_segment(aes(x = EndYear-0.5, y=index_latest/1000, xend = EndYear+0.5, yend = index_latest/1000),color="orange",size=1) +
    geom_segment(aes(x = EndYear-2.5, y=index_previous/1000, xend = EndYear-0.5, yend = index_previous/1000),color="orange",size=1) +
    geom_ribbon(aes(ymin=Low,ymax=High, linetype = '95% confidence interval'), fill="darkseagreen4", size=0, alpha=0.5)+ ylab("Biomass index in 1000 t")+
    scale_y_continuous(expand = c(0, 0), limits=c(0,550)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  theme(  panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line( size=.05, color="lightgrey" ) ,
        panel.grid.minor.y = element_blank() ,
        legend.title = element_blank() ,
        axis.text.x = element_text(angle=0, size=13),
        axis.text.y=element_text(size=13),
        legend.text = element_text(size=13),
        strip.text.x = element_text(size=13),
        axis.title = element_text(size=13), 
        legend.key.size = unit(0.25,'cm'),
        legend.position="bottom"
)

ggsave(bimplt,file=paste(wdir,'/output/biomass_timseries.png', sep="/"), width=6, height=4)



#############################################################################################
###################################### 3: Plot model outputs ################################
#############################################################################################
load(file='model/fittedmodel.rdata')

plot(1:1); dev.new() # Make a window 

# individual plots
png(paste(outDir,'spict_biomass.png', sep="/"), width=606, height=404,pointsize = 13)
plotspict.biomass(fit)
dev.off()

png(paste(outDir,'spict_bbmsy.png', sep="/"), width=606, height=404,pointsize = 13)
plotspict.bbmsy(fit)
dev.off()

png(paste(outDir,'spict_f.png', sep="/"), width=606, height=404,pointsize = 13)
plotspict.f(fit)
dev.off()

png(paste(outDir,'spict_ffmsy.png', sep="/"), width=606, height=404,pointsize = 13)
plotspict.ffmsy(fit)
dev.off()

png(paste(outDir,'spict_catch.png', sep="/"), width=606, height=404,pointsize = 13)
plotspict.catch(fit)
dev.off()

png(paste(outDir,'spict_fb.png', sep="/"), width=606, height=404,pointsize = 13)
plotspict.fb(fit, ylim=c(0, 1.3), xlim=c(0, 300))
dev.off()


########### Forecast

#inp$maninterval<-c(2021,2022)
#inp$maneval <- 2021
rep <- fit.spict(inp)
sumspict.predictions(rep)

png(paste(outDir,'spict_forecast.png', sep="/"), width=870, height=538,pointsize = 13)
plot2(rep)
dev.off()



#extract values
#get.par("logFFmsy",rep, exp=T)
#get.par("logBBmsy",fit, exp=T)



FFMSY<-as.data.frame(get.par("logFFmsy",fit,exp=T))
FFMSY<-tibble::rownames_to_column(FFMSY, "Year")
ffmsy<-write.csv(FFMSY,paste0(outDir,"/ffmsy.csv"))

png(paste(outDir,'spict_parameters.png', sep="/"), width=870, height=538,pointsize = 13)
plotspict.priors(fit)
dev.off()




