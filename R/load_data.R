# Set up to run for each CR1000 box's data in a separate directory. Can be changed to process all boxes from one directory preferred


#!!!! TODO - change from BoxA to something more generic
#!!!! TODO - clean up function formatting
##' The initial function to load raw CR1000 data
##' Set up to run for each CR1000 box's data in a separate directory. 
##' Can be changed to process all boxes from one directory preferred
##' 
##' @name load_data
##' @description  function to load raw CR1000 data
##' 
##' @param filepath The file path
##' @param Origin_DateTime e.g. 2000-01-01 01:00:00
##' @param End_DateTime e.g. 2030-01-01 01:00:00
##' 
##' @return BoxA
##' 
##' @author Kenneth Davidson
##' @export
load_Data <- function(filepath, Origin_DateTime="2000-01-01 01:00:00", End_DateTime="2030-01-01 01:00:00"){
  ls_files <- list.files(path=filepath, full.names = T) 
  Header <- read.csv(file=ls_files[1], header = F, skip=1,nrows = 1, as.is = T)
  BoxA=do.call("rbind.fill", apply(X = as.matrix(ls_files),FUN = read.csv,MARGIN = 1, skip=4,header=F))
  colnames(BoxA) <- Header
  names(BoxA) <- c("TIMESTAMP", "RECORD", "TREE1_TH1", "TREE2_TH1", "TREE3_TH1", "TREE4_TH1", "TREE5_TH1",
                   "TREE1_TH2", "TREE2_TH2", "TREE3_TH2", "TREE4_TH2", "TREE5_TH2", "TREE1_TH3", "TREE2_TH3", 
                   "TREE3_TH3", "TREE4_TH3", "TREE5_TH3", "HT_START") # stock names for sapflux data frames
  to_convert <- grep("TREE", names(BoxA))
  BoxA$TIMESTAMP <- ymd_hms(BoxA$TIMESTAMP, tz="UTC")
  BoxA <- BoxA[BoxA$TIMESTAMP >=as.POSIXct(Origin_DateTime,tz="UTC"),]
  BoxA <- BoxA[BoxA$TIMESTAMP <as.POSIXct(End_DateTime,tz="UTC"),]
  #BoxA <- BoxA[complete.cases(BoxA), ]
  BoxA[to_convert] <- lapply(BoxA[to_convert], as.numeric)
  BoxA[to_convert] <- lapply(BoxA[to_convert], convert_temp)
  BoxA$Date <- date(BoxA$TIMESTAMP)
  BoxA <- BoxA[!is.na(BoxA$Date),]
  Full_Time_Range <- data.frame(TIMESTAMP=as.POSIXct(paste0(rep(unique(BoxA$Date), each=68400)," ", format(seq(as.POSIXct("00:00:01", format = "%T"), as.POSIXct("23:59:59", format = "%T"), by = "1 sec"), "%H:%M:%S")),tz="UTC"))
  Full_Time_Range$min <- minute(Full_Time_Range$TIMESTAMP)
  Full_Time_Range <- Full_Time_Range[Full_Time_Range$min %in% c(0:14,30:44),]
  `%notin%` <- Negate(`%in%`)
  New_Dates <- data.frame(TIMESTAMP=Full_Time_Range[which(Full_Time_Range$TIMESTAMP %notin% BoxA$TIMESTAMP),"TIMESTAMP"])
  BoxA <- merge(BoxA,New_Dates, by="TIMESTAMP", all.x = T, all.y = T)
  BoxA$Date <- date(BoxA$TIMESTAMP)
  BoxA$Min <- minute(BoxA$TIMESTAMP)
  BoxA$Time <- as_hms(BoxA$TIMESTAMP)
  BoxA$Baseline <- BoxA$Min %in% c(0,1,2,30,31,32)
  BoxA$Heat <- BoxA$Min %in% c(3,4,33,34)
  BoxA$Pulse <- as.factor(paste0(hour(BoxA$Time),"_",minute(round_date(BoxA$TIMESTAMP, "30 minutes"))))
  levels(BoxA$Pulse) <- c('P01','P02','P03','P04','P21','P22','P23','P24','P25','P26','P27','P28','P29','P30','P31','P32','P33','P34','P35','P36','P37','P38',
                          'P39','P40','P05','P06','P41','P42','P43','P44','P45','P46','P47','P48','P07','P08','P09','P10','P11','P12','P13','P14','P15','P16','P17','P18','P19','P20')
  return(BoxA)
  
}