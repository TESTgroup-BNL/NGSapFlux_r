##' Function to load and format raw CR1000 data
##' 
##' @name load_data
##' @description  function to load raw CR1000 data. 
##' Set up to run for each CR1000 box's data in a separate directory. 
##' Can be changed to process all boxes from one directory preferred
##' 
##' @param filepath The file path
##' @param Origin_DateTime e.g. 2000-01-01 01:00:00
##' @param End_DateTime e.g. 2030-01-01 01:00:00
##' 
##' @return A dataframe in standard NGSapFlux_r format. 
##' These data are not yet processed or QAQC'ed
##' 
##' @author Kenneth Davidson
##' @author Jeremiah Anderson
##' @author Shawn Serbin
##' 
##' @export
load_data <- function(filepath, Origin_DateTime="2000-01-01 01:00:00", 
                      End_DateTime="2030-01-01 01:00:00") {
  ls_files <- list.files(path=file.path(filepath), full.names = T)
  Header <- read.csv(file=ls_files[1], header = F, skip=1, nrows = 1, as.is = T)
  # hard-coded skip below -could cause problems in the long run!!
  unprocData <- do.call(plyr::rbind.fill, apply(X = as.matrix(ls_files),
                                                FUN = read.csv, MARGIN = 1, 
                                                skip=4, header=F))
  colnames(unprocData) <- Header
  # stock names for sapflux data frames
  # !!! FOR NOW DROPPING EXTRA COLUMN OF DATA !!!
  #! DOING THIS TO MAKE SURE THE ASSIGNED NAMES MATCH NUM OF COLs
  unprocData <- unprocData[,-dim(unprocData)[2]]
  names(unprocData) <- c("TIMESTAMP", "RECORD", "TREE1_TH1", "TREE2_TH1", 
                         "TREE3_TH1", "TREE4_TH1", "TREE5_TH1",
                         "TREE1_TH2", "TREE2_TH2", "TREE3_TH2", "TREE4_TH2", 
                         "TREE5_TH2", "TREE1_TH3", "TREE2_TH3", 
                         "TREE3_TH3", "TREE4_TH3", "TREE5_TH3", "Volt", "HT_START")
  
  to_convert <- grep("TREE", names(unprocData))
  unprocData$TIMESTAMP <- lubridate::ymd_hms(unprocData$TIMESTAMP, tz="UTC")
  unprocData <- unprocData[unprocData$TIMESTAMP >= as.POSIXct(Origin_DateTime,
                                                              tz="UTC") 
                           & unprocData$TIMESTAMP <= as.POSIXct(End_DateTime,
                                                                tz="UTC"),]
  unprocData[to_convert] <- lapply(unprocData[to_convert], as.numeric)
  #!! remove bad thermister data. expected that raw data < 0.9
  #unprocData[to_convert][unprocData[to_convert] >=0.9] <- NA
  #!!
  unprocData[to_convert] <- lapply(unprocData[to_convert], 
                                   ngsapflux::convert_temp)
  unprocData$Date <- lubridate::date(unprocData$TIMESTAMP)
  unprocData <- unprocData[!is.na(unprocData$Date),]
  unprocData$Min <- lubridate::minute(unprocData$TIMESTAMP)
  unprocData$Time <- hms::as_hms(unprocData$TIMESTAMP)
  # !!! these appear to be hard-coded assumptions that we should be confident about
  unprocData$Baseline <- unprocData$Min %in% c(0,1,30,31)
  unprocData$Heat <- unprocData$Min %in% c(3,4,33,34)
  # !!!
  unprocData$Pulse <- as.factor(paste0(lubridate::hour(unprocData$Time),"_",
                                       lubridate::minute(lubridate::round_date(
                                         unprocData$TIMESTAMP, "30 minutes"))))
  levels(unprocData$Pulse) <- c('P01','P02','P03','P04','P21','P22','P23','P24',
                                'P25','P26','P27','P28','P29','P30','P31','P32',
                                'P33','P34','P35','P36','P37','P38','P39','P40',
                                'P05','P06','P41','P42','P43','P44','P45','P46',
                                'P47','P48','P07','P08','P09','P10','P11','P12',
                                'P13','P14','P15','P16','P17','P18','P19','P20')
  unprocData$Pulse <- as.character(unprocData$Pulse)
  X <- aggregate(TIMESTAMP~Date,unprocData,length)
  names(X)[2] <- "prop" # !! THIS SHOULD HAVE A MORE DESCRIPTIVE NAME!!
  unprocData <- merge(unprocData,X, by="Date")
  # !! it seems weird to just randomly do a data QA/QC subset here without 
  # making it clear and days with low data are dropped! 
  unprocData <- unprocData[unprocData$prop>=1000,]
  
  return(unprocData)
  
}
