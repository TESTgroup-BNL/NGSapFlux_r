##' Function to converts from resistance to temp in deg C
##' 
##' @name convert_temp
##' @description Function to converts from resistance to temp in deg C
##' 
##' @return Temp_C the measurement temperature in degC
##' 
##' @author Jeremiah Anderson
##' @export
convert_temp <- function(x){
  Temp_C <- 97.1383 - 236.7707*x + 262.5395*x^2 - 155.3591*x^3
  return(Temp_C)
} 

##' A helper function to find mode value to establish a pre-heat 
##' baseline temp for QAQC and data processing
##' 
##' @name getmode
##' @description A helper function to find mode value to establish pre and 
##' post heat baseline temps for QAQC and data processing
##' 
##' @author Kenneth Davidson
##' @export 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
} 

##' A helper function for Heat_Dis_Spline
##' @name spline_fit
##' @description A helper function to fit a spline to a moving window of data. 
##' Used estimate baseline temp of sap post heating
##' for detection of full pulse duration. 
##' 
##' @author Kenneth Davidson
##' @export 
spline_fit <- function(Y_vals,X_vals=Z$TIMESTAMP, X_Vals_New=X){
  Z1 <- smooth.spline(X_vals,Y_vals)
  Z2 <- predict(Z1, X_Vals_New)
  return(Z2$y)
}

# DO WE STILL NEED THIS FUNCTION??
# Replaces values that were flagged in the fine QAQC with NA
QAQC_Remove <- function(df,Pre_QAQC_df,Post_QAQC_df){
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
  df_3 <- rbind(df_1,df_2)
  return(df_3)
}

##' A helper function for baseline
##' @name find_nearest
##' @description A helper function which finds the nearest non-NA value and applies it 
##' 
##' @author https://stackoverflow.com/a/10081991
##' @export 

find_nearest <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}

