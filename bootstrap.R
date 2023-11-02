# Script information ------------------------------------------------------
# Title: pil.27.7
# Authors: Joseph Ribeiro, Rosana Ourens-Chans
# Date: 2022/05/26
# Purpose: This script should set up your working folder structure,
#          document the origins of any data going in to the analysis (via draft.data function), and
#          bring in the correct versions of packages and any software you need
# Use: I recommend renv for handling your packages. Alternatively, you could load the renv.lock file and manually install everything listed here
# Working directory: root (applies to all the TAF .r files)


# Load libraries ----------------------------------------------------------
stopifnot("renv package must be installed. This package will construct an environment for the project"=class(try(find.package("renv"), silent = TRUE)) != "try-error")

# if(renv::status()[["synchronized"]]==F){
#   print("Attention! The required libraries need to be installed. Please accept installation to set up reproducible environment. renv will install these packages in a new isolated environment specifically for this stock assessment.")
#   renv::activate() # Check the environment is right and if not install the right versions of packages.
#   renv::restore() # Check the environment is right and if not install the right versions of packages.
#   print("Please check for any error installation messages. If successful, rerun bootstrap.")
# }else{
  
  # Load libraries needed in bootstrap.r
  library(rstudioapi) # This has been written as an R studio project using RStudio Version 1.4.1717 on windows 10 and R version 4.1.2
  library(icesTAF)
  library(readxl) # One of the data inputs is consistently provided as an excel spreadsheet
  
  # Set the working directory (if sourced in rstudio) ---------------------------------------------
  try(setwd(dirname(getActiveDocumentContext()$path)))
  wdir = getwd(); print(wdir)
  
  # Create repository and R project -----------------------------------------
  
  # Clone the empty github repository created by ICES secretariat
  # The cloned repository is in C:/use/GitHub/ices-taf/2022_ane.27.9a_west_assessment
  # Create an R project in that folder
  
  
  # Make the skeleton -------------------------------------------------------
  
  # create initial directories and R scripts for a new TAF analysis
  # 2022_ane.27.9a_west_assessment    
  # ¦--bootstrap   
  # ¦   °--initial 
  # ¦       °--data
  # ¦--data.R      
  # ¦--model.R     
  # ¦--output.R    
  # °--report.R    
  
  taf.skeleton()
  
  # Upload initial data -----------------------------------------------------
  
  # Include data in the folder: \bootstrap/initial/data 
  # It can be done by-hand or copying it from another path using the following command:
  # file.copy(from=paste0(wdir,"/bootstrap/data/realdata_87-19_nov2019_juvena_corrected.txt"),
  #            to=paste0(wdir,"/bootstrap/initial/data/realdata_87-19_nov2019_juvena_corrected.txt"), overwrite=T)
  mkdir('output')
  mkdir('data')

  # Load each data file. Storing data on github is not recommended, but it should mean the data is all accessible. If on ICES sharepoint, only WG members can run the assessment
  Catches_uncorrected=read.taf(paste0(wdir,'/bootstrap/initial/data/LandingsOnly.csv')) # Ideally, find the actual origin of these data and save to a database. Data files should not be stored in github.
  Catches_reviewed_fr=read.taf(paste0(wdir,'/bootstrap/initial/data/review landings_France.csv')) # Ideally, find the actual origin of these data and save to a database. Data files should not be stored in github.
  Catches=read.taf(paste0(wdir,'/bootstrap/initial/data/Catchdata.csv')) # Ideally, find the actual origin of these data and save to a database. Data files should not be stored in github.
  Catches_reported=read.csv(paste0(wdir,'/bootstrap/initial/data/official_landings_7.csv'), fileEncoding = 'UTF-8-BOM',colClasses="character", stringsAsFactors=FALSE, na.strings=".") # read.taf doesn't work with this file but read.csv does
  Index_core=readxl::read_excel(paste0(wdir,'/bootstrap/initial/data/Peltic_timeseries.xlsx'),range="A2:B11")
  Index_total_area_bootstrap=read.csv(paste0(wdir,'/bootstrap/initial/data/PIL_bootstrap_results_totarea_2017-2021.csv'), fileEncoding = 'UTF-8-BOM',colClasses="character", stringsAsFactors=FALSE, na.strings=".") # read.taf doesn't work with this file but read.csv does
  Advice_history_a=read.csv(paste0(wdir,'/bootstrap/initial/data/historic_advice_5a.csv'), fileEncoding = 'UTF-8-BOM',colClasses="character", stringsAsFactors=FALSE, na.strings=".") # read.taf doesn't work with this file but read.csv does
  Advice_history_b=read.csv(paste0(wdir,'/bootstrap/initial/data/historic_advice_5b.csv'), fileEncoding = 'UTF-8-BOM',colClasses="character", stringsAsFactors=FALSE, na.strings=".") # read.taf doesn't work with this file but read.csv does

  # Write each data file 
  write.taf(Catches_uncorrected,paste0(wdir,"/bootstrap/data/Intercatch.csv"),quote=TRUE)
  write.taf(Catches_reviewed_fr,paste0(wdir,"/bootstrap/data/review landings_France.csv"),quote=TRUE)
  write.taf(Catches,paste0(wdir,"/bootstrap/data/Catches.csv"),quote=TRUE)
  write.taf(Catches_reported,paste0(wdir,"/bootstrap/data/Catches_reported.csv"),quote=TRUE)
  write.taf(Index_core,paste0(wdir,"/bootstrap/data/Index_core.csv"),quote=TRUE)
  write.taf(Index_total_area_bootstrap,paste0(wdir,"/bootstrap/data/Index_bootstrap.csv"),quote=TRUE)
  write.taf(Advice_history_a,paste0(wdir,"/bootstrap/data/Advice_history_a.csv"),quote=TRUE)
  write.taf(Advice_history_b,paste0(wdir,"/bootstrap/data/Advice_history_b.csv"),quote=TRUE)

  
  # Document data and create the data.bib file ------------------------------
  
  # use function draft.data() to create the data.bib file
  ?draft.data
  
  # data for stock assessment (unique file)
  
  draft.data(originator="WGHANSA", 
             year=2022, 
             title="Landings data for pil.27.7", 
             period="1987-2020",
             source="Intercatch download. This is inaccurate and needs updating with the latest French data. There is a corrected alternative to this file.",
             data.files=c("Intercatch.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="bootstrap/Intercatch.bib",
             append=F)

    
  draft.data(originator="WGHANSA", 
             year=2022, 
             title="Landings data for pil.27.7", 
             period="1987-2020",
             source="Provided by FR during WGHANSA 2021 - a correction to apply to the intercatch data",
             data.files=c("review landings_France.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="bootstrap/review landings_France.bib",
             append=F)

  
  draft.data(originator="WGHANSA", 
             year=2022, 
             title="Landings data for pil.27.7", 
             period="1987-2020",
             source="Corrected intercatch download with the latest French and Danish data",
             data.files=c("Catches.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="bootstrap/Catches.bib",
             append=F)

  
  draft.data(originator="WGHANSA", 
             year=2022, 
             title="Landings data for pil.27.7", 
             period="1987-2020",
             source="Lifted from old report table. Origin / derivation of this table is not clear to me as it does not align with Catchdata.csv, which is the catch data used to run the spict model",
             data.files=c("Catches_reported.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="bootstrap/Catches_reported.bib",
             append=F)

  
  draft.data(originator="WGHANSA", 
         year=2022, 
         title="Biomass index data for pil.27.7 (all areas)", 
         period="1987-2020",
         source="Bootstrap assessment of the biomass index for the entire region, provided by members of the Cefas team SCI-AFST-PIR, likely to be Fabio Campanella. This the biomass index indicative of the fullest area surveyed",
         data.files=c("Index_bootstrap.csv"),
         data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
         file="bootstrap/Index_bootstrap.bib",
         append=F)
  
  
  draft.data(originator="WGHANSA", 
             year=2022, 
             title="Biomass index data for pil.27.7 (core)", 
             period="2002-2021",
             source="Biomass index for the core region, lifted from an excel spreadsheet called Peltic_timeseries.xlsx, which is a survey output provided by members of the Cefas team SCI-AFST-PIR, likely to be Fabio Campanella. It comes from a single run, whereas the bootstrap comes from multiple runs. This is used because spict needs longer timeseries (core area is longer timeseries)",
             data.files=c("Index_core.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="bootstrap/Index_core.bib",
             append=F)
  

  draft.data(originator="WGHANSA", 
             year=2022, 
             title="Advice history for pil.27.7, time period a", 
             period="2002-2021",
             source="Lifted from previous advice. For this time period (prior to 2017) sardine in this area was assessed as a single stock combining Subarea 7",
             data.files=c("Advice_history_a.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="bootstrap/Advice_history_a.bib",
             append=F)
  
  
  draft.data(originator="WGHANSA", 
             year=2022, 
             title="Advice history for pil.27.7, time period b", 
             period="2002-2021",
             source="Lifted from previous advice. For this time period (from 2017), Sardine was assessed as separate stocks in divisions 8.a–b and 8.d, and Subarea 7",
             data.files=c("Advice_history_b.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="bootstrap/Advice_history_b.bib",
             append=F)
  
  

  # NOT used for advice 
  draft.data(originator="WGHANSA", 
             year=2022, 
             title="UK Length data in non-aggregated format", 
             period="2002-2021",
             source="Data file used for generating figures shown in WGHANSA presentation, provided by Silvia Rodriguez-Climent, Cefas. This seems to be missing the data from fisher co-sampling",
             data.files=c("PIL_db_nonagg1718192021.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="bootstrap/PIL_db_nonagg1718192021.bib",
             append=F)
  
    draft.data(originator="WGHANSA", 
             year=2022, 
             title="UK Length data in aggregated format", 
             period="2002-2021",
             source="Data file used for generating figures shown in WGHANSA presentation, provided by Silvia Rodriguez-Climent, Cefas",
             data.files=c("PIL_db_agg1718192021.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="bootstrap/PIL_db_agg1718192021.bib",
             append=F)
  

  
  # Upload software ---------------------------------------------------------
  # Document software and create the software.bib file ----------------------
  # no software to upload. We only use R libraries as loaded by renv

  # Process the data.bib and software.bib metafiles -------------------------
  
  # the function taf.bootstrap() processes:
  #   the data.bib file and creates/copies all the data files into the folder "bootstrap/data"
  #   and 
  #   the software.bib file and creates/copies the software into the folder bootstrap/software
  ?taf.bootstrap
  
  # apply taf.bootstrap. Processes the data.bib file. Creates a folder called data. CM showed us that If it can't find a file, use taf.data.path("data.csv") and then run the function & it should work
  taf.bootstrap(clean=F)
  unlink('bootstrap/data/reportTemplate.docx') # We don't use the default report template, we use a modified one
  
  # Session info ------------------------------------------------------------
  
  sessionInfo()
  
  # End of script -----------------------------------------------------------
#}  


