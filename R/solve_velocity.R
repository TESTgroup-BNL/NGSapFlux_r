#Solves the HRM equations for sensors 1 and 2. K can either as a vector or assumed
#' Title
#' 
##' @name solve_hrm
##' @description Solves the HRM equations for sensors 1 and 2. K can either as a vector or assumed
##'
##' @param df 
##' @param Dates The dates / date range to process. If omitted all data in df will be processed.
##' @param ktherm Wood thermal diffusivity. Supplied as a vector of five values corresponding to Trees 1-5.
##' @param tavg_start Beginning of averaging window, equivalent to seconds since start observations. Nominally the heat pulse will occur at/around 180. Supplied as a vector of five values corresponding to Trees 1-5.
##' @param tavg_end End of averaging window, equivalent to seconds since start observations. Nominally the observations will run for 900 seconds. Supplied as a vector of five values corresponding to Trees 1-5.
##' @param sensor_2_dist Distance in cm for sensor 2 from heater. Negative values are equivalent a position upstream of the heater.
##' @param sensor_1_dist Distance in cm for sensor 1 from heater. Negative values are equivalent a position upstream of the heater.
##'
##' @return Data frame with HRM processed data to half hour
##' 
##' @author Kenneth Davidson
##' @export
##'
solve_hrm <- function(df,
                      Dates=unique(df$Date),
                      ktherm=rep(0.0025,5),
                      tavg_start=rep(200,5),
                      tavg_end=rep(900,5),
                      sensor_2_dist=rep(0.2,5),
                      sensor_1_dist=rep(-0.2,5)){
  
  HRM_df <- data.frame(Date= rep(as.character(Dates), each=48), 
                    Pulse= rep(Pulses,length(Dates)),
                    Time= rep(Times, length(Dates)),
                    TREE1=NA,TREE2=NA,TREE3=NA,TREE4=NA,TREE5=NA)
  
  for (i in as.character(Dates)){
    for (k in Pulses){
      HRM_df[HRM_df$Date==i&HRM_df$Pulse==k,"TREE1"] <- mean(2*(ktherm[1]/(sensor_2_dist[1]-sensor_1_dist[1]))*
                                                      log(df[df$Date==i&df$Pulse==k,"TREE1_TH2"][tavg_start[1]:tavg_end[1]]/
                                                            df[df$Date==i&df$Pulse==k,"TREE1_TH1"][tavg_start[1]:tavg_end[1]]),na.rm=TRUE)*3600
      HRM_df[HRM_df$Date==i&HRM_df$Pulse==k,"TREE2"] <- mean(2*(ktherm[2]/(sensor_2_dist[2]-sensor_1_dist[2]))*
                                                      log(df[df$Date==i&df$Pulse==k,"TREE2_TH2"][tavg_start[2]:tavg_end[2]]/
                                                            df[df$Date==i&df$Pulse==k,"TREE2_TH1"][tavg_start[2]:tavg_end[2]]),na.rm=TRUE)*3600
      HRM_df[HRM_df$Date==i&HRM_df$Pulse==k,"TREE3"] <- mean(2*(ktherm[3]/(sensor_2_dist[3]-sensor_1_dist[3]))*
                                                      log(df[df$Date==i&df$Pulse==k,"TREE3_TH2"][tavg_start[3]:tavg_end[3]]/
                                                            df[df$Date==i&df$Pulse==k,"TREE3_TH1"][tavg_start[3]:tavg_end[3]]),na.rm=TRUE)*3600
      HRM_df[HRM_df$Date==i&HRM_df$Pulse==k,"TREE4"] <- mean(2*(ktherm[4]/(sensor_2_dist[4]-sensor_1_dist[4]))*
                                                      log(df[df$Date==i&df$Pulse==k,"TREE4_TH2"][tavg_start[4]:tavg_end[4]]/
                                                            df[df$Date==i&df$Pulse==k,"TREE4_TH1"][tavg_start[4]:tavg_end[4]]),na.rm=TRUE)*3600
      HRM_df[HRM_df$Date==i&HRM_df$Pulse==k,"TREE5"] <- mean(2*(ktherm[5]/(sensor_2_dist[5]-sensor_1_dist[5]))*
                                                      log(df[df$Date==i&df$Pulse==k,"TREE5_TH2"][tavg_start[5]:tavg_end[5]]/
                                                            df[df$Date==i&df$Pulse==k,"TREE5_TH1"][tavg_start[5]:tavg_end[5]]),na.rm=TRUE)*3600
    }
  }
  
  return(HRM_df)
} 