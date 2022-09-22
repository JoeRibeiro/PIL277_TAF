# Clean environment, and define some r objects for writing the markdown
rm(list = ls())

library(icesTAF)

mkdir("report")

try(setwd(dirname(getActiveDocumentContext()$path)))
wdir = getwd(); print(wdir)

load("output/AdviceInfo.rdata")
load('data/catches_by_category_table6.rdata') # For table 6

# Build table 1, take the values, convert to strings and put units beside them etc
basis <- data.frame(name = c(paste0("Index A (",as.character(EndYear),")"),paste0("Index B (",as.character(EndYear-2),"–",as.character(EndYear-1),")"),"Index ratio (A/B)","Biomass safeguard (Istat)","Uncertainty cap",paste0("Mean catches (",as.character(EndYear-2),"–",as.character(EndYear-1),")"),"Discard rate",paste0("Catch advice ",as.character(EndYear+1)," ***"),"% advice change ^"), value = "")
basis$value[basis$name==paste0("Index A (",as.character(EndYear),")")] = paste0(index_latest," tonnes")
basis$value[basis$name==paste0("Index B (",as.character(EndYear-2),"–",as.character(EndYear-1),")")] = paste0(index_previous," tonnes")
basis$value[basis$name=="Index ratio (A/B)"] = paste0(index_ratio_inv)
basis$value[basis$name=="Biomass safeguard (Istat)"] = paste0(biomass_safeguard," ",biomass_safeguard_val)
basis$value[basis$name=="Uncertainty cap"] = paste0(uncertaintycap," ",uncertaintycapval)
basis$value[basis$name==paste0("Mean catches (",as.character(EndYear-2),"–",as.character(EndYear-1),")")] = paste0(mean_2prevyears," tonnes")
basis$value[basis$name=="Discard rate"] = paste0(discard_rate)
basis$value[basis$name==paste0("Catch advice ",as.character(EndYear+1)," ***")] = paste0(advisedcatch," tonnes")
basis$value[basis$name=="% advice change ^"] = paste0(advice_change)
# the next lines with the hspace stuff is an attempt to set column width wider. It is a hacky approach as changing column widths is not currently supported in pandoc, or is too cryptic for me
basis$value=paste0("\\hspace{120 mm} ",basis$value); basis$name=paste0(basis$name,"\\hspace{120 mm} ")
names(basis)<-NULL


# Build table 2, which is descriptive
basis_descriptive = data.frame(name = c("Advice basis\\hspace{120 mm} ","Management plan\\hspace{120 mm} "), value = c("\\hspace{120 mm} Precautionary approach.","\\hspace{120 mm} ICES is not aware of any agreed precautionary management plan for sardine in this area."))
names(basis_descriptive)<-NULL

# define istat
istat = round( exp(mean(log(Ihist))) * (2.71828^(-1.645*sd(log(Ihist)))) )

save.image("report/report_datatables.Rdata")

# Knit these tables into a report word file
rmarkdown::render("report.rmd", "word_document",output_file = paste0("pil.27.7_advice",format(Sys.time(), "%a%b%d_%Y_%s"),".docx"))