##' Coarse QAQC processing.
##' 
##' @name sanity_check_coarse
##' @description Course QAQC processing. Breaks each pulse interval into three 
##' periods, pre-pulse(00:00-02:00), heat (02:01-03:00), and post-pulse 
##' (02:01-15:00). Identifies and flags outliers as temperature readings that 
##' are greater than a given amount above or below the mode temperature reading 
##' during that period. If the number of outliers are exceed a user specified 
##' minimum threshold, the whole period is flagged as bad. Data returned as TRUE 
##' indicates an error.
##' 
##' 
##' @param df The input data frame
##' @param Dates The dates / date range to process
##' @param Pre_Threshold_Temp Difference in temp from mode (during the pre-pulse 
##' period) greater than which a point will be considered an outlier  
##' @param Pre_Threshold_Num Minimum number of outliers during the pre-pulse 
##' period before data are flagged as bad
##' @param Post_Threshold_Temp Difference in temp from mode 
##' (during the post-pulse period) greater than which a point will be 
##' considered an outlier  
##' @param Post_Threshold_Num Minimum number of outliers during the 
##' post-pulse period before data are flagged as bad
##' @param HT_Temp Temperature threshold for detecting heater pulse
##' 
##' @return A list composed of three QAQC data frames
##' @author Kenneth Davidson
##' @export
##' 
sanity_check_coarse <- function(df,Dates=unique(df$Date),Pre_Threshold_Temp=0.5,
                                Pre_Threshold_Num=20, Post_Threshold_Temp=5, 
                                Post_Threshold_Num=10, HT_Temp=0.5){
  # where is pulses coming from?
  QC_Pre <- data.frame(Date = rep(as.character(Dates), each=48), 
                       Pulse = rep(Pulses, times=length(Dates)), Tree_Blank)
  QC_Post <- data.frame(Date = rep(as.character(Dates), each=48), 
                        Pulse = rep(Pulses, times=length(Dates)), Tree_Blank)
  QC_HT <- data.frame(Date = rep(as.character(Dates), each=48), 
                      Pulse = rep(Pulses, times=length(Dates)), Heat_Blank)
  
  for (j in as.character(Dates)){
    for (k in Pulses){
      MODE_Pre <- sapply(df[df$Date==j &df$Pulse==k&df$Baseline==T,
                            colnames(Tree_Blank)],FUN=getmode)
      MODE_Post <- sapply(df[df$Date==j &df$Pulse==k&df$Baseline==F,
                             colnames(Tree_Blank)],FUN=getmode)
      MODE_HT <- sapply(df[df$Date==j &df$Pulse==k&df$Heat==T,6:10],FUN=max)
      if(is.na(MODE_Pre)[1]){
        QC_Pre[QC_Pre$Date==j &QC_Pre$Pulse==k,colnames(Tree_Blank)] <- TRUE
        QC_Post[QC_Post$Date==j &QC_Post$Pulse==k,colnames(Tree_Blank)]  <- TRUE
        QC_HT[QC_HT$Date==j &QC_HT$Pulse==k,colnames(Heat_Blank)] <- TRUE
      }else{
        QC_Pre_x <- df[df$Date==j &df$Pulse==k&df$Baseline==T,colnames(Tree_Blank)]>= matrix(rep(t(data.frame(MODE_Pre+Pre_Threshold_Temp)),nrow(df[df$Date==j &df$Pulse==k&df$Baseline==T,])), nrow=nrow(df[df$Date==j &df$Pulse==k&df$Baseline==T,]),byrow = T)|
          df[df$Date==j &df$Pulse==k&df$Baseline==T,colnames(Tree_Blank)]<=matrix(rep(t(data.frame(MODE_Pre-Pre_Threshold_Temp)),nrow(df[df$Date==j &df$Pulse==k&df$Baseline==T,])), nrow=nrow(df[df$Date==j &df$Pulse==k&df$Baseline==T,]),byrow = T)

        QC_Pre[QC_Pre$Date==j &QC_Pre$Pulse==k,colnames(Tree_Blank)]  <- 
          colSums(QC_Pre_x, na.rm = TRUE)>=Pre_Threshold_Num
        
        QC_Post_x<- df[df$Date==j &df$Pulse==k&df$Baseline==F,colnames(Tree_Blank)]>=matrix(data= rep(MODE_Post+Post_Threshold_Temp,nrow(df[df$Date==j &df$Pulse==k&df$Baseline==F,])),ncol=length(Tree_Blank), byrow=T)|df[df$Date==j &df$Pulse==k&df$Baseline==F,3:17]<=matrix(data= rep(MODE_Post-Post_Threshold_Temp,nrow(df[df$Date==j &df$Pulse==k&df$Baseline==F,])),ncol=length(Tree_Blank), byrow=T)
        
        nrow(df[df$Date==j &df$Pulse==k&df$Baseline==F,])
        
        QC_Post[QC_Post$Date==j &QC_Post$Pulse==k,colnames(Tree_Blank)]  <- colSums(QC_Post_x, na.rm = TRUE)>=Post_Threshold_Num
        
        QC_HT[QC_HT$Date==j &QC_HT$Pulse==k,colnames(Heat_Blank)] <-  as.logical(MODE_HT-MODE_Pre[6:10]<=HT_Temp)
      }
    }}
  QAQC <- list(QC_Pre,QC_Post,QC_HT)
  return(QAQC)
}


##' Fine QAQC processing
##' 
##' @name sanity_check_fine
##' @description Fine QAQC processing. Breaks each pulse interval into two 
##' periods, pre-pulse(00:00-02:00), and post post-pulse (02:01-15:00).
##' Identifies and flags outliers as temperature readings that exceed a 
##' user specified upper and lower threshold temperature reading during that 
##' period. Output is a list containing data frame with outliers removed, and 
##' two data frames corresponding to the upper and lower temperature thresholds 
##' for each data point.
##' 
##' 
##' 
##' @param df The input data frame
##' @param Dates The dates / date range to process 
##' @param Pre_Threshold_Temp_Upper Difference in mode temp (during the pre-pulse 
##' period) greater than which a point will be considered an outlier
##' @param Pre_Threshold_Temp_Lower Difference in mode temp (during the pre-pulse 
##' period) less than which a point will be considered an outlier
##' @param Post_Threshold_Temp_Upper Difference in mode temp (during the post-pulse 
##' period) greater than which a point will be considered an outlier
##' @param Post_Threshold_Temp_Lower Difference in mode temp (during the post-pulse 
##' period) less than which a point will be considered an outlier
##' @param Heat_Threshold_Temp Value which when added to mode of the
##' pre-pulse period becomes the threshold over which the max temp observed during heat pulse  must pass for the heater to be considered working
##' 
##' @return A list containing data frame with outlines removed (replaced by NA), 
##' and two data frames corresponding to the upper and lower temperature 
##' thresholds for each data point.
##' 
##' @author Kenneth Davidson
##' @export
##' 
sanity_check_fine <- function(df, Dates=unique(df$Date), 
                              Pre_Threshold_Temp_Upper=0.2, 
                              Pre_Threshold_Temp_Lower=0.2,
                              Post_Threshold_Temp_Upper=5, 
                              Post_Threshold_Temp_Lower=0.2,
                              Heat_Threshold_Temp=0.5) {
  
 df_mode <- aggregate(cbind(TREE1_TH1,TREE2_TH1,TREE3_TH1,TREE4_TH1,TREE5_TH1,
                            TREE1_TH2,TREE2_TH2,TREE3_TH2,TREE4_TH2,TREE5_TH2,
                            TREE1_TH3,TREE2_TH3,TREE3_TH3,TREE4_TH3,
                            TREE5_TH3)~Date+Pulse+Baseline,df,ngsapflux::getmode)
 
 df_Heat <- aggregate(cbind(TREE1_TH2,TREE2_TH2,TREE3_TH2,TREE4_TH2,TREE5_TH2)~Date+Pulse,df[df$Heat==T,],max)
 df_Heat_Base <- df_mode[df_mode$Baseline==T,c(1,2,9:14)]
 # Tree_Blank comes from the data.R data definitions 
 colnames(df_mode)[4:18] <- paste0(colnames(Tree_Blank),".mode")
 colnames(df_Heat)[3:7] <- paste0(colnames(Heat_Blank))
 colnames(df_Heat_Base)[3:7] <- paste0(colnames(Heat_Blank))
 
 df_comp <- merge(df,df_mode, by=c("Date","Pulse","Baseline"), all.x = T)


 df_comp_pre <- df_comp[df_comp$Baseline==T,]
 df_comp_post <- df_comp[df_comp$Baseline==F,]
 
 
 Upper_Pre <- data.frame(df_comp_pre[,c("Date","TIMESTAMP","Baseline","Pulse")],
                         df_comp_pre[,paste0(colnames(Tree_Blank),".mode")]+
                           Pre_Threshold_Temp_Upper)
 Lower_Pre <- data.frame(df_comp_pre[,c("Date","TIMESTAMP","Baseline","Pulse")],
                         df_comp_pre[,paste0(colnames(Tree_Blank),".mode")]-
                           Pre_Threshold_Temp_Lower)
 Upper_Post <- data.frame(df_comp_post[,c("Date","TIMESTAMP","Baseline","Pulse")],
                          df_comp_post[,paste0(colnames(Tree_Blank),".mode")]+
                            Post_Threshold_Temp_Upper)
 Lower_Post <- data.frame(df_comp_post[,c("Date","TIMESTAMP","Baseline","Pulse")],
                          df_comp_post[,paste0(colnames(Tree_Blank),".mode")]-
                            Post_Threshold_Temp_Lower)
 
 QC_HT <- data.frame(Date = rep(as.character(Dates), each=48), 
                     Pulse = rep(Pulses, times=length(Dates)), Heat_Blank)

  QAQC_Upper <- rbind(Upper_Pre,Upper_Post)
  QAQC_Upper<- QAQC_Upper[order(QAQC_Upper$TIMESTAMP),]
  QAQC_Lower <- rbind(Lower_Pre,Lower_Post)
  QAQC_Lower<- QAQC_Lower[order(QAQC_Lower$TIMESTAMP),]
  
  colnames(QAQC_Upper)[5:19] <- colnames(Tree_Blank)
  colnames(QAQC_Lower)[5:19] <- colnames(Tree_Blank)
  QAQC_data <- df
  
  for(i in colnames(Tree_Blank)){
  QAQC_data[,i] <- replace(QAQC_data[,i], QAQC_data[,i]>= QAQC_Upper[,i],
                           values = NA)
  QAQC_data[,i] <- replace(QAQC_data[,i], QAQC_data[,i]<= QAQC_Lower[,i],
                           values = NA)
  QAQC_data[,i] <- replace(QAQC_data[,i], is.na(QAQC_Upper[,i]),values = NA)
  QAQC_data[,i] <- replace(QAQC_data[,i], is.na(QAQC_Lower[,i]),values = NA)
  QAQC_data[,i] <- replace(QAQC_data[,i], QAQC_data[,i]<=0,values = NA)
  }
  
  for(i in colnames(Heat_Blank)){
    QC_HT[,i] <- replace(QC_HT[,i], df_Heat[,i]>= df_Heat_Base[,i]+Heat_Threshold_Temp,
                             values = "Good")
    
    QC_HT[,i] <- replace(QC_HT[,i], df_Heat[,i]<= df_Heat_Base[,i]+Heat_Threshold_Temp,
                         values = "Bad")
  }
  
  
  QAQC_data <- merge(QAQC_data,QC_HT, by=c("Date","Pulse"), all.x = T)  
  
  QAQC_data[QAQC_data$HT1=="Bad",c("TREE1_TH1","TREE1_TH2","TREE1_TH3")] <- NA  
  QAQC_data[QAQC_data$HT2=="Bad",c("TREE2_TH1","TREE2_TH2","TREE2_TH3")] <- NA  
  QAQC_data[QAQC_data$HT3=="Bad",c("TREE3_TH1","TREE3_TH2","TREE3_TH3")] <- NA  
  QAQC_data[QAQC_data$HT4=="Bad",c("TREE4_TH1","TREE4_TH2","TREE4_TH3")] <- NA  
  QAQC_data[QAQC_data$HT5=="Bad",c("TREE5_TH1","TREE5_TH2","TREE5_TH3")] <- NA  
  
  
  QAQC_data_l <- list(QAQC_data,QAQC_Upper,QAQC_Lower)
  return(QAQC_data_l)
}
### EOF