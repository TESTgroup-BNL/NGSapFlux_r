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

BoxA <- ngsapflux::load_data(Path_A, Origin_DateTime="2022-09-07 00:00:00", End_DateTime = "2022-09-07 23:59:59")
BoxA <- ngsapflux::load_data(Path_A, Origin_DateTime="2022-09-08 00:00:00", End_DateTime = "2022-09-08 23:59:59")
BoxA <- ngsapflux::load_data(Path_A, Origin_DateTime="2022-09-09 00:00:00", End_DateTime = "2022-09-09 23:59:59")
BoxA <- ngsapflux::load_data(Path_A, Origin_DateTime="2022-09-10 00:00:00", End_DateTime = "2022-09-10 23:59:59")


BoxA <- ngsapflux::load_data(Path_A, Origin_DateTime="2022-09-05 00:00:00", End_DateTime = "2022-09-10 23:59:59")

# filepath <- Path_A
# Origin_DateTime="2022-09-05 00:00:00"
# End_DateTime = "2022-09-10 23:59:59"
# unprocData

summary(BoxA)

unique(BoxA$Date)

ggplot(BoxA, aes(Time, TREE1_TH1)) + geom_point(aes(color="bottom"))+geom_point(aes(Time, TREE1_TH2,color="middle"))+geom_point(aes(Time, TREE1_TH3,color="top"))+ggtitle("Tree 1")+
  scale_color_manual(name = "sensor",values = c( "top" = "blue", "middle" = "red", "bottom" = "black"),
                     labels = c("top", "middle", "bottom"))


QAQC_coarse <- sanity_check_coarse(BoxA)
names(QAQC_coarse)
QC_Pre <- QAQC_coarse[[1]]
QC_Post <- QAQC_coarse[[2]]
QC_HT <- QAQC_coarse[[3]]

QAQC_fine <- sanity_check_fine(BoxA)
QC_Pre_Fine <- QAQC_fine[[1]]
QC_Post_Fine <- QAQC_fine[[2]]
QC_HT_Fine <- QAQC_fine[[3]]

BoxA_QAQC <- QAQC_Remove(BoxA,QC_Pre_Fine[3:17],QC_Post_Fine)

ggplot(QC_Pre_Fine, aes(Time, TREE1_TH1)) + geom_point(aes(color="bottom"))+geom_point(aes(Time, TREE1_TH2,color="middle"))+geom_point(aes(Time, TREE1_TH3,color="top"))+ggtitle("Tree 1")+
  scale_color_manual(name = "sensor",values = c( "top" = "blue", "middle" = "red", "bottom" = "black"),
                     labels = c("top", "middle", "bottom"))

ggplot(QC_Post_Fine, aes(TIMESTAMP, TREE1_TH1)) + geom_point(aes(color="bottom"))+geom_point(aes(TIMESTAMP, TREE1_TH2,color="middle"))+geom_point(aes(TIMESTAMP, TREE1_TH3,color="top"))+ggtitle("Tree 1")+
  scale_color_manual(name = "sensor",values = c( "top" = "blue", "middle" = "red", "bottom" = "black"),
                     labels = c("top", "middle", "bottom"))

ggplot(QC_HT_Fine, aes(TIMESTAMP, TREE1_TH1)) + geom_point(aes(color="bottom"))+geom_point(aes(TIMESTAMP, TREE1_TH2,color="middle"))+geom_point(aes(TIMESTAMP, TREE1_TH3,color="top"))+ggtitle("Tree 1")+
  scale_color_manual(name = "sensor",values = c( "top" = "blue", "middle" = "red", "bottom" = "black"),
                     labels = c("top", "middle", "bottom"))

C <- heat_dis_spline(QC_Pre_Fine)
Baseline <- C[[1]]
Heat_Increase_spline <- C[[2]]
Heat_Pulse <- C[[3]]
Cool_Time <- C[[4]]

