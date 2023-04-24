#' Baseline Correction.
##' 
##' @name baseline
##' @description Aligns sap velocity data with met data. Locates periods of 0 VPD, PAR, and 
##' rainfall and uses those periods to assume zero flow. Applies correction to sap velocity data to account for 
##' misalignment of probes/low flow drift.
##' 
##' 
##' @param df Data frame containing velocity data as produced by solve_hrm()
##' @param baseline_length Optional minimum length for a baseline period to be considered (default 4 hours)
##' @param met_data Data frame containing met data at half hourly intervals (e.g., 10:00:00, 10:30:00, 11:00:00). 
##' Required columns are Timestamp (format YYYY-MM-DD HH:MM:SS, in either UTC or local time), Tair (ºC),
##' PAR (W m-2 or µmol m-2 s-1), RH (%), and optionally VPDair (kPa)
##' @param tz local timezone for the data
##' 
##' 
##' @return Data frame containing original and corrected velocity data, matched with met data
##' @author Kenneth Davidson
##' @export
##' 

baseline <- function(df,baseline_length=4,met_data, tz="UTC"){

  met_data$Timestamp <- as.POSIXct(lubridate::ymd_hms(met_data$Timestamp, tz="UTC"))
  df$Timestamp <- as.POSIXct(lubridate::ymd_hm(paste0(df$Date," ",df$Time)))
  df_merge <-merge(met_data, df, by="Timestamp", all.y=T, all.x=F)
  df_merge$Timestamp <- with_tz(df_merge$Timestamp, tz=tz)
  df_merge$Date <- lubridate::date(df_merge$Timestamp)
  df_merge$Time <- (substr(df_merge$Timestamp,11,19))  
  df_merge$VPDair <- plantecophys::RHtoVPD(df_merge$RH,df_merge$Tair)
  df_merge$check <- df_merge$VPDair<=0.1 & df_merge$PAR<=1
  df_merge[is.na(df_merge$check),"check"] <- FALSE
  
  df_merge$check <- runner::runner(df_merge$check, f=sum,k=(baseline_length*2))
  Baseline <- df_merge[df_merge$check>=(baseline_length*2)-2,c("Timestamp","TREE1","TREE2","TREE3","TREE4","TREE5")]
  names(Baseline)[2:6] <- c("Base_1","Base_2","Base_3","Base_4","Base_5")
  df_merge <- merge(df_merge,Baseline, by="Timestamp",all.x=T)
  
  
  df_merge$Base_1 <- find_nearest(df_merge$Base_1)
  df_merge$Base_2 <- find_nearest(df_merge$Base_2)
  df_merge$Base_3 <- find_nearest(df_merge$Base_3)
  df_merge$Base_4 <- find_nearest(df_merge$Base_4)
  df_merge$Base_5 <- find_nearest(df_merge$Base_5)
  df_merge$TREE1_Corected <- df_merge$TREE1 - df_merge$Base_1
  df_merge$TREE2_Corected <- df_merge$TREE2 - df_merge$Base_2
  df_merge$TREE3_Corected <- df_merge$TREE3 - df_merge$Base_3
  df_merge$TREE4_Corected <- df_merge$TREE4 - df_merge$Base_4
  df_merge$TREE5_Corected <- df_merge$TREE5 - df_merge$Base_5
  df_merge$Base_1 <- NULL
  df_merge$Base_2 <- NULL
  df_merge$Base_3 <- NULL
  df_merge$Base_4 <- NULL
  df_merge$Base_5 <- NULL
  df_merge$check <- NULL
  
  return(df_merge)
  
}