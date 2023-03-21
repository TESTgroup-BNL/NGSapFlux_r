library(ggplot2)

Filepath <- "~/Google Drive/TESTgroup/Projects/BRF-SBU/SapFlux/Data/BoxA" #Oblivious this is specific to your file structure 

Data <- load_data(Filepath, Origin_DateTime = "2022-10-02", End_DateTime = "2022-10-05") # I would stick to a week or less of data. Some of the functions slow way down when given a large date range.
Data_QA <-sanity_check_fine(Data,Dates=unique(Data$Date),0.02,0.02,5,1) #The threshold temps can be modified for more or less precision in QAQC 
Data_HRM <- solve_hrm(Data_QA[[1]]) #Data_QA is a list of three data frames. The object is the data with bad values as NA. The second two items are the data frame with values above and below the threshold makred as NA


ggplot(Data, aes(Time, TREE1_TH1))+ geom_point(aes(color="bottom"))+geom_point(aes(Time, TREE1_TH2,color="middle"))+geom_point(aes(Time, TREE1_TH3,color="top"))+ggtitle("Tree 1")+
  scale_color_manual(name = "sensor",values = c( "top" = "blue", "middle" = "red", "bottom" = "black"),
                     labels = c("top", "middle", "bottom"))+facet_wrap(~Date, scales = "free_x")+theme(legend.position ="none")

ggplot(Data_QA[[1]], aes(Time, TREE1_TH1))+ geom_point(aes(color="bottom"))+geom_point(aes(Time, TREE1_TH2,color="middle"))+geom_point(aes(Time, TREE1_TH3,color="top"))+ggtitle("Tree 1")+
  scale_color_manual(name = "sensor",values = c( "top" = "blue", "middle" = "red", "bottom" = "black"),
                     labels = c("top", "middle", "bottom"))+facet_wrap(~Date,scales = "free_x")+theme(legend.position ="none")


ggplot(Data_HRM, aes(Time,TREE1))+geom_point()+facet_wrap(~Date)+ylab("Sap Velocity cm/hr")+ggtitle("Tree 1")+scale_x_discrete(breaks=c("03:00","06:00","09:00","12:00","15:00","18:00","21:00"),labels=c("03:00","6:00","09:00","12:00","15:00","18:00","21:00"))
ggplot(Data_HRM, aes(Time,TREE2))+geom_point()+facet_wrap(~Date)+ylab("Sap Velocity cm/hr")+ggtitle("Tree 2")+scale_x_discrete(breaks=c("03:00","06:00","09:00","12:00","15:00","18:00","21:00"),labels=c("03:00","6:00","09:00","12:00","15:00","18:00","21:00"))
ggplot(Data_HRM, aes(Time,TREE3))+geom_point()+facet_wrap(~Date)+ylab("Sap Velocity cm/hr")+ggtitle("Tree 3")+scale_x_discrete(breaks=c("03:00","06:00","09:00","12:00","15:00","18:00","21:00"),labels=c("03:00","6:00","09:00","12:00","15:00","18:00","21:00"))
ggplot(Data_HRM, aes(Time,TREE4))+geom_point()+facet_wrap(~Date)+ylab("Sap Velocity cm/hr")+ggtitle("Tree 4")+scale_x_discrete(breaks=c("03:00","06:00","09:00","12:00","15:00","18:00","21:00"),labels=c("03:00","6:00","09:00","12:00","15:00","18:00","21:00"))
ggplot(Data_HRM, aes(Time,TREE5))+geom_point()+facet_wrap(~Date)+ylab("Sap Velocity cm/hr")+ggtitle("Tree 5")+scale_x_discrete(breaks=c("03:00","06:00","09:00","12:00","15:00","18:00","21:00"),labels=c("03:00","6:00","09:00","12:00","15:00","18:00","21:00"))
