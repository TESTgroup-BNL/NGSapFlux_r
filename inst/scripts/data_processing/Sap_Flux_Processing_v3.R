# Load packages
list.of.packages <- c("dplyr","ggplot2","lubridate","plyr","hms","stats","here")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=TRUE)
# load
invisible(lapply(list.of.packages, library, character.only = TRUE))

library(ngsapflux)

# clean up first
rm(list=ls())

# set paths - make sure you open the script from within the git rep structure
here::here()

# input path
Path_A <- file.path(here::here(),"inst/NGSapFlux/example_data/Alabama_NEON/3-1")

# Create example output
#output_dir <- "tempdir"
#output_dir <- file.path(here::here(),"inst/NGSapFlux/example_data/example_output")
output_dir <- file.path("~/Data/Dropbox/Talks_and_Posters/DOE/ESSPI_2023/SSerbin_Poster/Figures/SapFlux")
if (output_dir=="tempdir") {
  outdir <- tempdir()
} else {
  if (! file.exists(output_dir)) dir.create(output_dir,recursive=TRUE)
  outdir <- file.path(path.expand(output_dir))
}
setwd(outdir) # set working directory
getwd()  # check wd


# I would stick to a week or less of data. Some of the functions slow way down when given a large date range.
#date1 <- "2022-09-05 00:00:00"
#date2 <- "2022-09-10 23:59:59"

date1 <- "2022-09-06 00:00:00"
date2 <- "2022-09-06 23:59:59"
inputdata <- ngsapflux::load_data(Path_A, Origin_DateTime = date1, 
                                  End_DateTime = date2)

# filepath <- Path_A
# Origin_DateTime="2022-09-05 00:00:00"
# End_DateTime = "2022-09-10 23:59:59"
# unprocData

summary(inputdata)

unique(inputdata$Date)


# interim QAQC functions
#The threshold temps can be modified for more or less precision in QAQC 
Data_QAQC <-sanity_check_fine(inputdata,Dates=unique(inputdata$Date),
                              0.02,0.02,5,1)
#Data_QA is a list of three data frames. 
#The object is the data with bad values as NA. The second two items are the 
#data frame with values above and below the threshold makred as NA
Data_HRM <- solve_hrm(Data_QAQC[[1]]) 

# Plot the raw data
#!! make this dynamic so you can change treenum and alter plot
#treenum = "TREE1"
for (i in seq_along(1:5)) {
  print(paste0("TREE",i))
  treenum <- paste0("TREE",i)
  rawdataplot <- ggplot(inputdata, aes(Time, .data[[paste0(treenum,"_TH1")]])) + 
    geom_point(aes(color="bottom")) + 
    geom_point(aes(Time, .data[[paste0(treenum,"_TH2")]], color="middle")) + 
    geom_point(aes(Time, .data[[paste0(treenum,"_TH3")]], color="top")) + 
    ggtitle(treenum)+
    scale_color_manual(name = "sensor",values = c( "top" = "blue", 
                                                   "middle" = "red", 
                                                   "bottom" = "black"),
                       labels = c("top", "middle", "bottom")) + 
    facet_wrap(~Date, scales = "free_x") + theme(legend.position ="bottom") + 
    ylab(expression("Temperature ("*~degree*C*")"))
    
  ggsave(filename = file.path(outdir,paste0(lubridate::as_date(date1),"_",
                                            lubridate::as_date(date2),"_",
                                            treenum,
                                            "_raw_data.png")), 
         plot = rawdataplot, 
         device="png", width = 35, 
         height = 14, units = "cm",
         dpi = 300)
  
  # remove previous plot
  rm(rawdataplot)
}

# OR
# ggplot(inputdata, aes(Time, get(paste0(treenum,"_TH1"))))

# Plot the QAQC data
for (i in seq_along(1:5)) {
  print(paste0("TREE",i))
  treenum <- paste0("TREE",i)
  qaqc_data_plot <- ggplot(Data_QAQC[[1]], aes(Time, 
                                               .data[[paste0(treenum,"_TH1")]])) + 
    geom_point(aes(color="bottom")) + 
    geom_point(aes(Time, .data[[paste0(treenum,"_TH2")]], color="middle")) + 
    geom_point(aes(Time, .data[[paste0(treenum,"_TH3")]], color="top")) + 
    ggtitle(treenum)+
    scale_color_manual(name = "sensor",values = c( "top" = "blue", 
                                                   "middle" = "red", 
                                                   "bottom" = "black"),
                       labels = c("top", "middle", "bottom")) + 
    facet_wrap(~Date,scales = "free_x")+theme(legend.position ="bottom") + 
    ylab(expression("Temperature ("*~degree*C*")"))
  
  ggsave(filename = file.path(outdir,paste0(lubridate::as_date(date1),"_",
                                            lubridate::as_date(date2),"_",
                                            treenum,
                                            "_qaqc_data.png")), 
         plot = qaqc_data_plot, 
         device="png", width = 35, 
         height = 14, units = "cm",
         dpi = 300)
  rm(rawdataplot)
}


# Plot the HRM data (sap velocity in cm/hr)
for (i in seq_along(1:5)) {
  print(paste0("TREE",i))
  treenum <- paste0("TREE",i)
  hrmplot <- ggplot(Data_HRM, aes(Time,.data[[paste0(treenum)]])) + 
    geom_point()+facet_wrap(~Date) +
    ylab("Sap Velocity cm/hr")+ggtitle("Tree 1") + 
    scale_x_discrete(breaks=c("03:00","06:00","09:00","12:00","15:00","18:00",
                              "21:00"),labels=c("03:00","6:00","09:00","12:00",
                                                "15:00","18:00","21:00"))
  ggsave(filename = file.path(outdir,paste0(lubridate::as_date(date1),"_",
                                            lubridate::as_date(date2),"_",
                                            treenum,
                                            "_HRM_sapvelocity.png")), 
         plot = hrmplot, 
         device="png", width = 35, 
         height = 14, units = "cm",
         dpi = 300)
  
  rm(hrmplot)
  
}
