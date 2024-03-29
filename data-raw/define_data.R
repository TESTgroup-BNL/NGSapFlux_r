getwd()
devtools::load_all()

# Prep data "classes" for processing steps

# TODO: We should really define data classes instead
# Also using "Pulses" and "Times" is risky as we could also 
# accidentally re-define those in processing scripts or functions and those
# would overwrite these and lead to possibly unexpected behavior
Pulses <- c('P01','P02','P03','P04','P05','P06','P07','P08','P09','P10','P11',
            'P12','P13','P14','P15','P16','P17','P18','P19','P20','P21','P22',
            'P23','P24','P25','P26','P27','P28','P29','P30','P31','P32','P33',
            'P34','P35','P36','P37','P38','P39','P40','P41','P42','P43','P44',
            'P45','P46','P47','P48')
Times <- c("00:00","00:30","01:00","01:30","02:00","02:30","03:00","03:30",
           "04:00","04:30","05:00","05:30","06:00","06:30","07:00","07:30",
           "08:00","08:30","09:00","09:30","10:00","10:30","11:00","11:30",
           "12:00","12:30","13:00","13:30","14:00","14:30","15:00","15:30",
           "16:00","16:30","17:00","17:30","18:00","18:30","19:00","19:30",
           "20:00","20:30","21:00","21:30","22:00","22:30","23:00","23:30")
Tree_Blank <- data.frame(TREE1_TH1=NA,TREE2_TH1=NA,TREE3_TH1=NA,TREE4_TH1=NA,
                         TREE5_TH1=NA,TREE1_TH2=NA,TREE2_TH2=NA,TREE3_TH2=NA,
                         TREE4_TH2=NA,TREE5_TH2=NA,TREE1_TH3=NA,TREE2_TH3=NA,
                         TREE3_TH3=NA,TREE4_TH3=NA,TREE5_TH3=NA)
Heat_Blank <- data.frame(HT1=NA,HT2=NA,HT3=NA,HT4=NA,HT5=NA)

# save the datasets
save(Pulses,Times,Tree_Blank,Heat_Blank, file = "define_data.RData")
usethis::use_data(Pulses,Times,Tree_Blank,Heat_Blank, 
                  overwrite = TRUE, internal = FALSE)

