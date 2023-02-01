# Finds the baseline temp before the heat pulse, the average increase in temperature from the baseline for a given period to time (30-180 sec post heat by default), the time of peak heating, and the time at which the heat returns to baseline
#This function requires removal of outlier points in the preheat phase. Otherwise the spline will not work
#This function requires that all

Heat_Dis_Spline <- function(df,Dates=unique(df$Date),Time_Post_Heat=c(30:180)){
  Baseline <- data.frame(Date= rep(as.character(Dates), each=48), 
                         Pulse= rep(c('P01','P02','P03','P04','P05','P06','P07','P08','P09','P10','P11','P12','P13','P14','P15','P16','P17','P18','P19','P20','P21','P22','P23','P24',
                                      'P25','P26','P27','P28','P29','P30','P31','P32','P33','P34','P35','P36','P37','P38','P39','P40','P41','P42','P43','P44','P45','P46','P47','P48'), times=length(Dates)),
                         TREE1_TH1=NA,TREE2_TH1=NA,TREE3_TH1=NA,TREE4_TH1=NA,TREE5_TH1=NA,TREE1_TH2=NA,TREE2_TH2=NA,TREE3_TH2=NA,TREE4_TH2=NA,TREE5_TH2=NA,TREE1_TH3=NA,TREE2_TH3=NA,TREE3_TH3=NA,TREE4_TH3=NA,TREE5_TH3=NA)
  Heat_time <- data.frame(Date= rep(as.character(Dates), each=48), 
                          Pulse= rep(c('P01','P02','P03','P04','P05','P06','P07','P08','P09','P10','P11','P12','P13','P14','P15','P16','P17','P18','P19','P20','P21','P22','P23','P24',
                                       'P25','P26','P27','P28','P29','P30','P31','P32','P33','P34','P35','P36','P37','P38','P39','P40','P41','P42','P43','P44','P45','P46','P47','P48'), times=length(Dates)),
                          TREE1_TH1=NA,TREE2_TH1=NA,TREE3_TH1=NA,TREE4_TH1=NA,TREE5_TH1=NA,TREE1_TH2=NA,TREE2_TH2=NA,TREE3_TH2=NA,TREE4_TH2=NA,TREE5_TH2=NA,TREE1_TH3=NA,TREE2_TH3=NA,TREE3_TH3=NA,TREE4_TH3=NA,TREE5_TH3=NA)
  Heat_Increase <- data.frame(Date= rep(as.character(Dates), each=48), 
                              Pulse= rep(c('P01','P02','P03','P04','P05','P06','P07','P08','P09','P10','P11','P12','P13','P14','P15','P16','P17','P18','P19','P20','P21','P22','P23','P24',
                                           'P25','P26','P27','P28','P29','P30','P31','P32','P33','P34','P35','P36','P37','P38','P39','P40','P41','P42','P43','P44','P45','P46','P47','P48'), times=length(Dates)),
                              TREE1_TH1=NA,TREE2_TH1=NA,TREE3_TH1=NA,TREE4_TH1=NA,TREE5_TH1=NA,TREE1_TH2=NA,TREE2_TH2=NA,TREE3_TH2=NA,TREE4_TH2=NA,TREE5_TH2=NA,TREE1_TH3=NA,TREE2_TH3=NA,TREE3_TH3=NA,TREE4_TH3=NA,TREE5_TH3=NA)
  Cool_time <- data.frame(Date= rep(as.character(Dates), each=48), 
                          Pulse= rep(c('P01','P02','P03','P04','P05','P06','P07','P08','P09','P10','P11','P12','P13','P14','P15','P16','P17','P18','P19','P20','P21','P22','P23','P24',
                                       'P25','P26','P27','P28','P29','P30','P31','P32','P33','P34','P35','P36','P37','P38','P39','P40','P41','P42','P43','P44','P45','P46','P47','P48'), times=length(Dates)),
                          TREE1_TH1=NA,TREE2_TH1=NA,TREE3_TH1=NA,TREE4_TH1=NA,TREE5_TH1=NA,TREE1_TH2=NA,TREE2_TH2=NA,TREE3_TH2=NA,TREE4_TH2=NA,TREE5_TH2=NA,TREE1_TH3=NA,TREE2_TH3=NA,TREE3_TH3=NA,TREE4_TH3=NA,TREE5_TH3=NA)
  
  for (j in as_date(Dates)){
    Z <- df[df$TIMESTAMP>=ymd_hms(paste0(as_date(j)-1," 12:00:00"),tz="UTC")& df$TIMESTAMP<=ymd_hms(paste0(as_date(j)+1," 11:59:59"),tz="UTC")&df$Baseline==T, ]
    Z <- Z[complete.cases(Z[,1:17]),]
    Z$TIMESTAMP <- as.numeric(Z$TIMESTAMP)
    X <- df[df$Date== as_date(j) & df$Baseline==F&df$Min %in% c(14,44), ]
    X$Sec <- second(X$TIMESTAMP)
    X <- as.numeric(X[X$Sec==0, "TIMESTAMP"])
    Spline_Fit <- function(Y_vals,X_vals=Z$TIMESTAMP, X_Vals_New=X){
      Z1 <- smooth.spline(X_vals,Y_vals)
      Z2 <- predict(Z1, X_Vals_New)
      return(Z2$y)
    }
    
    Baseline[Baseline$Date==as_date(j),3:17] <- sapply(Z[,3:17], Spline_Fit)
    
    for (k in c('P01','P02','P03','P04','P05','P06','P07','P08','P09','P10','P11','P12','P13','P14','P15','P16','P17','P18','P19','P20','P21','P22','P23','P24',
                'P25','P26','P27','P28','P29','P30','P31','P32','P33','P34','P35','P36','P37','P38','P39','P40','P41','P42','P43','P44','P45','P46','P47','P48')){
      Z <- df[df$Date==as_date(j)& df$Pulse==k&df$Baseline==F,3:17][Time_Post_Heat,]
      X <- matrix(data=unlist(rep(Baseline[Baseline$Date==as_date(j)&Baseline$Pulse==k,3:17], length(Time_Post_Heat))),ncol=15,byrow = T)
      Heat_Increase[Heat_Increase$Date==as_date(j)&Heat_Increase$Pulse==k, 3:17] <- colMeans(Z- X)
      
      X <- sapply(df[df$Date==as_date(j)& df$Pulse==k&df$Baseline==F,3:17],which.max)
      if(identical(X[[1]], integer(0))){
        Heat_time[Heat_time$Date==as_date(j)&Heat_time$Pulse==k, 3:17] <- NA
      }else{
        Heat_time[Heat_time$Date==as_date(j)&Heat_time$Pulse==k, 3:17] <- as.character(df[df$Date==as_date(j)& df$Pulse==k&df$Baseline==F,"TIMESTAMP"][X])
      }
      
      Y1 <- c()
      for (i in 3:17){ 
        Y <- which(df[df$Date==as_date(j)& df$Pulse==k&df$Baseline==F,i]>=Baseline[Baseline$Date==as_date(j)&Baseline$Pulse==k,i])
        if(identical(Y, integer(0))){
          Y <- 720
        }else{
          Y <-Y[1]}
        Y1 <- c(Y1,Y)}
      
      Cool_time[Cool_time$Date==as_date(j)&Cool_time$Pulse==k, 3:17] <- as.character(df[df$Date==as_date(j)& df$Pulse==k&df$Baseline==F,"TIMESTAMP"][Y1])
    }
  }
  
  Heat_Dis_Data <- list(Baseline,Heat_Increase,Heat_time,Cool_time)
  return(Heat_Dis_Data)
} 