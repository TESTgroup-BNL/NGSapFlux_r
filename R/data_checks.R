# Course QAQC processing. Identifies outliers and flags if data are messy by time period (e.g., 12:00:00-12:01:59) based on number of errant observations. Also detects if heater fired. Data returned as TRUE indicates an error 
# This function is VERY SLOW, can be run in parallel to speed up
##' Course QAQC processing.
##' 
##' @name sanity_check_course
##' @description Course QAQC processing. Identifies outliers and flags if data 
##' are messy by time period (e.g., 12:00:00-12:01:59) based on number of errant 
##' observations. Also detects if heater fired. Data returned as TRUE indicates 
##' an error. NOTE: This function is VERY SLOW, can be run in parallel to 
##' speed up 
##' 
##' @param df The input data frame
##' @param Dates The dates / date range to process
##' @param Pre_Threshold_Temp PLACEHOLDER
##' @param Pre_Threshold_Num PLACEHOLDER
##' @param Post_Threshold_Temp PLACEHOLDER
##' @param Post_Threshold_Num PLACEHOLDER
##' @param HT_Temp PLACEHOLDER
##' 
##' @return QAQC list
##' @author Kenneth Davidson
##' @export
##' 
sanity_check_course <- function(df,Dates=unique(df$Date),Pre_Threshold_Temp=0.5,
                                Pre_Threshold_Num=20, Post_Threshold_Temp=5, 
                                Post_Threshold_Num=10, HT_Temp=0.5){
  
  #!! TODO: should really develop file classes that we use instead of hard coding
  QC_Pre <- data.frame(Date= rep(as.character(Dates), each=48), 
                       Pulse= rep(c('P01','P02','P03','P04','P05','P06','P07','P08','P09','P10','P11','P12','P13','P14','P15','P16','P17','P18','P19','P20','P21','P22','P23','P24',
                                    'P25','P26','P27','P28','P29','P30','P31','P32','P33','P34','P35','P36','P37','P38','P39','P40','P41','P42','P43','P44','P45','P46','P47','P48'), times=length(Dates)),
                       TREE1_TH1=NA,TREE2_TH1=NA,TREE3_TH1=NA,TREE4_TH1=NA,TREE5_TH1=NA,TREE1_TH2=NA,TREE2_TH2=NA,TREE3_TH2=NA,TREE4_TH2=NA,TREE5_TH2=NA,TREE1_TH3=NA,TREE2_TH3=NA,TREE3_TH3=NA,TREE4_TH3=NA,TREE5_TH3=NA)
  QC_Post <- data.frame(Date= rep(as.character(Dates), each=48), 
                        Pulse= rep(c('P01','P02','P03','P04','P05','P06','P07','P08','P09','P10','P11','P12','P13','P14','P15','P16','P17','P18','P19','P20','P21','P22','P23','P24',
                                     'P25','P26','P27','P28','P29','P30','P31','P32','P33','P34','P35','P36','P37','P38','P39','P40','P41','P42','P43','P44','P45','P46','P47','P48'), times=length(Dates)),
                        TREE1_TH1=NA,TREE2_TH1=NA,TREE3_TH1=NA,TREE4_TH1=NA,TREE5_TH1=NA,TREE1_TH2=NA,TREE2_TH2=NA,TREE3_TH2=NA,TREE4_TH2=NA,TREE5_TH2=NA,TREE1_TH3=NA,TREE2_TH3=NA,TREE3_TH3=NA,TREE4_TH3=NA,TREE5_TH3=NA)
  QC_HT <- data.frame(Date= rep(as.character(Dates), each=48), 
                      Pulse= rep(c('P01','P02','P03','P04','P05','P06','P07','P08','P09','P10','P11','P12','P13','P14','P15','P16','P17','P18','P19','P20','P21','P22','P23','P24',
                                   'P25','P26','P27','P28','P29','P30','P31','P32','P33','P34','P35','P36','P37','P38','P39','P40','P41','P42','P43','P44','P45','P46','P47','P48'), times=length(Dates)),
                      HT1=NA,HT2=NA,HT3=NA,HT4=NA,HT5=NA)
  
  for (j in as.character(Dates)){
    for (k in c('P01','P02','P03','P04','P05','P06','P07','P08','P09','P10','P11','P12','P13','P14','P15','P16','P17','P18','P19','P20','P21','P22','P23','P24',
                'P25','P26','P27','P28','P29','P30','P31','P32','P33','P34','P35','P36','P37','P38','P39','P40','P41','P42','P43','P44','P45','P46','P47','P48')){
      MODE_Pre <- sapply(df[df$Date==j &df$Pulse==k&df$Baseline==T,3:17],FUN=getmode)
      MODE_Post <- sapply(df[df$Date==j &df$Pulse==k&df$Baseline==F,3:17],FUN=getmode)
      MODE_HT <- sapply(df[df$Date==j &df$Pulse==k&df$Heat==T,8:12],FUN=max)
      if(is.na(MODE_Pre)[1]){
        QC_Pre[QC_Pre$Date==j &QC_Pre$Pulse==k,3:17] <- TRUE
        QC_Post[QC_Post$Date==j &QC_Post$Pulse==k,3:17]  <- TRUE
        QC_HT[QC_HT$Date==j &QC_HT$Pulse==k,3:7] <- TRUE
      }else{
        QC_Pre_x <- df[df$Date==j &df$Pulse==k&df$Baseline==T,3:17]>= matrix(rep(t(data.frame(MODE_Pre+Pre_Threshold_Temp)),180), nrow=180,byrow = T)|df[df$Date==j &df$Pulse==k&df$Baseline==T,3:17]<=matrix(rep(t(data.frame(MODE_Pre-Pre_Threshold_Temp)),180), nrow=180,byrow = T)
        QC_Pre[QC_Pre$Date==j &QC_Pre$Pulse==k,3:17]  <- colSums(QC_Pre_x, na.rm = TRUE)>=Pre_Threshold_Num
        QC_Post_x<- df[df$Date==j &df$Pulse==k&df$Baseline==F,3:17]>=matrix(data= rep(MODE_Post+Post_Threshold_Temp,720),ncol=15, byrow=T)|df[df$Date==j &df$Pulse==k&df$Baseline==F,3:17]<=matrix(data= rep(MODE_Post-Post_Threshold_Temp,720),ncol=15, byrow=T)
        QC_Post[QC_Post$Date==j &QC_Post$Pulse==k,3:17]  <- colSums(QC_Post_x, na.rm = TRUE)>=Post_Threshold_Num
        QC_HT[QC_HT$Date==j &QC_HT$Pulse==k,3:7] <-  as.logical(MODE_HT-MODE_Pre[6:10]<=HT_Temp)
      }
    }}
  QAQC <- list(QC_Pre,QC_Post,QC_HT)
  return(QAQC)
}


#Fine QAQC processing. Identifies outliers and flags if data are messy by individual point. Data returned as TRUE indicates an error 
# This function is VERY SLOW, can be run in parallel to speed up
##' Fine QAQC processing
##' 
##' @name sanity_check_fine
##' @description Fine QAQC processing. Identifies outliers and flags if data are 
##' messy by individual point. Data returned as TRUE indicates an error.
##' This function is VERY SLOW, can be run in parallel to speed up 
##' 
##' 
##' @param df The input data frame
##' @param Dates The dates / date range to process 
##' @param Pre_Threshold_Temp PLACEHOLDER
##' @param Post_Threshold_Temp PLACEHOLDER
##' 
##' @return QAQC list
##' @author Kenneth Davidson
##' @export
##' 
sanity_check_fine <- function(df,Dates=unique(df$Date),Pre_Threshold_Temp=0.5, 
                              Post_Threshold_Temp=5){
  QC_Pre <- data.frame(Date= rep(as.character(Dates), each=48*180), 
                       Pulse= rep(rep(c('P01','P02','P03','P04','P05','P06','P07','P08','P09','P10','P11','P12','P13','P14','P15','P16','P17','P18','P19','P20','P21','P22','P23','P24',
                                        'P25','P26','P27','P28','P29','P30','P31','P32','P33','P34','P35','P36','P37','P38','P39','P40','P41','P42','P43','P44','P45','P46','P47','P48'),each=180), times=length(Dates)),
                       TREE1_TH1=NA,TREE2_TH1=NA,TREE3_TH1=NA,TREE4_TH1=NA,TREE5_TH1=NA,TREE1_TH2=NA,TREE2_TH2=NA,TREE3_TH2=NA,TREE4_TH2=NA,TREE5_TH2=NA,TREE1_TH3=NA,TREE2_TH3=NA,TREE3_TH3=NA,TREE4_TH3=NA,TREE5_TH3=NA)
  QC_Post <- data.frame(Date= rep(as.character(Dates), each=48*720), 
                        Pulse= rep(rep(c('P01','P02','P03','P04','P05','P06','P07','P08','P09','P10','P11','P12','P13','P14','P15','P16','P17','P18','P19','P20','P21','P22','P23','P24',
                                         'P25','P26','P27','P28','P29','P30','P31','P32','P33','P34','P35','P36','P37','P38','P39','P40','P41','P42','P43','P44','P45','P46','P47','P48'),each=720), times=length(Dates)),
                        TREE1_TH1=NA,TREE2_TH1=NA,TREE3_TH1=NA,TREE4_TH1=NA,TREE5_TH1=NA,TREE1_TH2=NA,TREE2_TH2=NA,TREE3_TH2=NA,TREE4_TH2=NA,TREE5_TH2=NA,TREE1_TH3=NA,TREE2_TH3=NA,TREE3_TH3=NA,TREE4_TH3=NA,TREE5_TH3=NA)
  
  for (j in as.character(Dates)){
    for (k in c('P01','P02','P03','P04','P05','P06','P07','P08','P09','P10','P11','P12','P13','P14','P15','P16','P17','P18','P19','P20','P21','P22','P23','P24',
                'P25','P26','P27','P28','P29','P30','P31','P32','P33','P34','P35','P36','P37','P38','P39','P40','P41','P42','P43','P44','P45','P46','P47','P48')){
      MODE_Pre <- sapply(df[df$Date==j &df$Pulse==k&df$Baseline==T,3:17],FUN=getmode)
      MODE_Post <- sapply(df[df$Date==j &df$Pulse==k&df$Baseline==F,3:17],FUN=getmode)
      if(is.na(MODE_Pre)[1]){
        QC_Pre[QC_Pre$Date==j &QC_Pre$Pulse==k,3:17] <- TRUE
        QC_Post[QC_Post$Date==j &QC_Post$Pulse==k,3:17]  <- TRUE
        
      }else{
        QC_Pre[QC_Pre$Date==j &QC_Pre$Pulse==k,3:17] <- df[df$Date==j &df$Pulse==k&df$Baseline==T,3:17]>= matrix(rep(t(data.frame(MODE_Pre+Pre_Threshold_Temp)),length(df[df$Date==j &df$Pulse==k&df$Baseline==T,3])), ncol=15,byrow = T)|df[df$Date==j &df$Pulse==k&df$Baseline==T,3:17]<=matrix(rep(t(data.frame(MODE_Pre-Pre_Threshold_Temp)),length(df[df$Date==j &df$Pulse==k&df$Baseline==T,3])), ncol=15,byrow = T)
        QC_Post[QC_Post$Date==j &QC_Post$Pulse==k,3:17]  <- df[df$Date==j &df$Pulse==k&df$Baseline==F,3:17]>=matrix(data= rep(MODE_Post+Post_Threshold_Temp,length(df[df$Date==j &df$Pulse==k&df$Baseline==F,3])),ncol=15, byrow=T)|df[df$Date==j &df$Pulse==k&df$Baseline==F,3:17]<=matrix(data= rep(MODE_Post-Post_Threshold_Temp,length(df[df$Date==j &df$Pulse==k&df$Baseline==F,3])),ncol=15, byrow=T)
      }
    }}
  QAQC <- list(QC_Pre,QC_Post)
  return(QAQC)
}

# Replaces values that were flagged in the fine QAQC with NA
##' Replaces values that were flagged in the fine QAQC with NA
##' @name qaqc_remove
##' @description Replaces values that were flagged in the fine QAQC with NA
##' 
##' @param df The input data frame
##' @param Pre_QAQC_df PLACEHOLDER
##' @param Post_QAQC_df PLACEHOLDER
##' 
##' @return qaqc_output_df The final dataframe with NA to indicate bad data
##' @author Kenneth Davidson
##' @export
##' 
qaqc_remove <- function(df,Pre_QAQC_df,Post_QAQC_df){
  Z <- (sapply(Pre_QAQC_df[3:17],which))
  X <- as.list(as.data.frame(sapply(Post_QAQC_df[3:17],which)))
  df_1 <- df[df$Baseline==T,]
  df_2 <- df[df$Baseline==F,]
  for (i in colnames(df_1[,3:17])){
    df_1[,i][Z[[i]]] <- NA
  }
  
  for (i in colnames(df_2[,3:17])){
    df_2[,i][X[[i]]] <- NA
  }
  qaqc_output_df <- rbind(df_1,df_2)
  return(qaqc_output_df)
}