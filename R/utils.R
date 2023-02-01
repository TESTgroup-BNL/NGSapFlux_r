##' Function to converts from resistance to temp in deg C
##' 
##' @name convert_temp
##' @description Function to converts from resistance to temp in deg C
##' 
##' @return Temp_C the measurement temperature in degC
##' 
##' @author Kenneth Davidson
##' @export
convert_temp <- function(x){
  Temp_C <- 97.1383 - 236.7707*x + 262.5395*x^2 - 155.3591*x^3
  return(Temp_C)
} 

##' A helper function to find mode value to establish a pre-heat 
##' baseline temp for QAQC and data processing
##' 
##' @name getmode
##' @description A helper function to find mode value to establish a pre-heat 
##' baseline temp for QAQC and data processi
##' 
##' @author Kenneth Davidson
##' @export 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
} 

##' A helper function for Heat_Dis_Spline
##' @name Spline_Fit
##' 
##' @author Kenneth Davidson
##' @export 
Spline_Fit <- function(Y_vals,X_vals=Z$TIMESTAMP, X_Vals_New=X){
  Z1 <- smooth.spline(X_vals,Y_vals)
  Z2 <- predict(Z1, X_Vals_New)
  return(Z2$y)
  
}




