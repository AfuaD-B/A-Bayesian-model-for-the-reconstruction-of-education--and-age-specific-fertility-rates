#In this file I clean all the data that has been collected
library(readxl)
library(tidyverse)
library(zoo)
library(glm2)
library(ggplot2)

#First start with TFR for one year intervals
DHS_tfr<-read_excel("K:/project/BayesEdu/Fertility/Afua/TFR2 Datasets DHS/DHS datasets all countries.xlsx",sheet="TFR")
DHS_tfr<-DHS_tfr[!(DHS_tfr$Country=="Senegal" & DHS_tfr$`Survey Year`%in%c(2006,2008)),]
DHS_tfr<-DHS_tfr[!(is.na(DHS_tfr$`Survey Year`)),]

DHS_tfr$`Survey Year`<-as.factor(DHS_tfr$`Survey Year`)
###Plot the original datasets to know which values to delete

ggplot()+
  geom_line(DHS_tfr,
            mapping=aes(x=Year ,y=TFR,colour=`Survey Year`,group=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="TFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))


##Based on the plots, the following edits are made

#Delete any values less than 0.5 and anything above 20

DHS_tfr2<-DHS_tfr[!(DHS_tfr$TFR<0.5),]
DHS_tfr2<-DHS_tfr2[!(DHS_tfr2$TFR>20),]

#Delete data for Survey years which results do not make sense

DHS_tfr2<-DHS_tfr2[!(DHS_tfr2$`Survey Year`==2007 & DHS_tfr2$Country=="Tanzania"),]

DHS_tfr2<-DHS_tfr2[!(DHS_tfr2$`Survey Year`==2005 & DHS_tfr2$Country=="Côte D'Ivoire"),]

DHS_tfr2<-DHS_tfr2[!(DHS_tfr2$`Survey Year`%in%c(2007,2013) & DHS_tfr2$Country=="Dominican Republic"),]


#Now country TFR specific cleaning

DHS_tfr2<-DHS_tfr2[!(DHS_tfr2$TFR<3 & DHS_tfr2$Country=="Haiti"),]
DHS_tfr2<-DHS_tfr2[!(DHS_tfr2$TFR<2 & DHS_tfr2$Country=="Albania"),]

DHS_tfr2<-DHS_tfr2[!(DHS_tfr2$TFR<3.5 & DHS_tfr2$Country=="Cameroon"),]


#Plot again to see if improved

ggplot()+
  geom_line(DHS_tfr2,
            mapping=aes(x=Year ,y=TFR,colour=`Survey Year`,group=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="TFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))



ggplot()+
  geom_ribbon(DHS_tfr2, mapping = aes(x=Year, ymin=TFR-`Standard Error`, ymax=TFR+`Standard Error`, group=`Survey Year`), fill="darkgrey")+
  geom_line(DHS_tfr2,
            mapping=aes(x=Year ,y=TFR,colour=`Survey Year`,group=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="TFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))


##Data is now relatively cleaned. Save the plots and later put all modified values in a new Excel!!!

############################################################################################################################################
############################################################################################################################################



#Then TFR for five year intervals
DHS_tfr_5<-read_excel("K:/project/BayesEdu/Fertility/Afua/TFR2 Datasets DHS/DHS datasets all countries.xlsx",sheet="TFR_5")
DHS_tfr_5<-DHS_tfr_5[!(DHS_tfr_5$Country=="Senegal" & DHS_tfr_5$`Survey Year`%in%c(2006,2008)),]
DHS_tfr_5<-DHS_tfr_5[!(is.na(DHS_tfr_5$`Survey Year`)),]


DHS_tfr_5$`Survey Year`<-as.factor(DHS_tfr_5$`Survey Year`)
###Plot the original datasets to know which values to delete

ggplot()+
  geom_line(DHS_tfr_5,
            mapping=aes(x=Year ,y=TFR,colour=`Survey Year`,group=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="TFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))


##Based on the plots, the following edits are made

#Delete any values less than 0.5 and anything above 20
DHS_tfr_5_2<-DHS_tfr_5[!(DHS_tfr_5$TFR<0.5),]

#Also delete for African countries with less than 2 tfr
DHS_tfr_5_2<-DHS_tfr_5_2[!(DHS_tfr_5_2$TFR<2 & DHS_tfr_5_2$Region=="Africa"),]


#Delete data for Survey years which results do not make sense

DHS_tfr_5_2<-DHS_tfr_5_2[!(DHS_tfr_5_2$`Survey Year`==2007 & DHS_tfr_5_2$Country=="Tanzania"),]
DHS_tfr_5_2<-DHS_tfr_5_2[!(DHS_tfr_5_2$`Survey Year`==2005 & DHS_tfr_5_2$Country=="Côte D'Ivoire"),]
DHS_tfr_5_2<-DHS_tfr_5_2[!(DHS_tfr_5_2$`Survey Year`%in%c(2007,2013) & DHS_tfr_5_2$Country=="Dominican Republic"),]
DHS_tfr_5_2<-DHS_tfr_5_2[!(DHS_tfr_5_2$Year=="1970-1975" & DHS_tfr_5_2$Country=="Cambodia"),]

#Now country TFR specific cleaning
DHS_tfr_5_2<-DHS_tfr_5_2[!(DHS_tfr_5_2$TFR<3 & DHS_tfr_5_2$Country=="Haiti"),]
DHS_tfr_5_2<-DHS_tfr_5_2[!(DHS_tfr_5_2$TFR<1.6 & DHS_tfr_5_2$Country=="Albania"),]
DHS_tfr_5_2<-DHS_tfr_5_2[!(DHS_tfr_5_2$TFR<4 & DHS_tfr_5_2$Country=="Cameroon"),]

#Plot again to see if improved
ggplot()+
  geom_line(DHS_tfr_5_2,
            mapping=aes(x=Year, y=TFR,colour=`Survey Year`,group=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="TFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))


ggplot()+
  geom_ribbon(DHS_tfr_5_2, mapping = aes(x=Year, ymin=TFR-`Standard Error`, ymax=TFR+`Standard Error`, group=`Survey Year`), fill="grey")+
  geom_line(DHS_tfr_5_2,
            mapping=aes(x=Year, y=TFR,colour=`Survey Year`,group=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="TFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))


###################################################################################################################################################################
###################################################################################################################################################################


#Then ESTFR for one year intervals
DHS_estfr<-read_excel("K:/project/BayesEdu/Fertility/Afua/TFR2 Datasets DHS/DHS datasets all countries.xlsx",sheet="ESTFR")
DHS_estfr<-DHS_estfr[!(DHS_estfr$Country=="Senegal" & DHS_estfr$`Survey Year`%in%c(2006,2008)),]
DHS_estfr<-DHS_estfr[!(is.na(DHS_estfr$`Survey Year`)),]





DHS_estfr$`Survey Year`<-as.factor(DHS_estfr$`Survey Year`)


DHS_estfr<-DHS_estfr[!(DHS_estfr$`Survey Year`==2007 & DHS_estfr$Country=="Tanzania"),]

DHS_estfr<-DHS_estfr[!(DHS_estfr$`Survey Year`==2005 & DHS_estfr$Country=="Côte D'Ivoire"),]


#Delete already the values close to 0 and values above 25  
DHS_estfr<-DHS_estfr[!(DHS_estfr$TFR>12),]
DHS_estfr<-DHS_estfr[!(DHS_estfr$TFR<0.01),]





###Plot the original datasets to know which values to delete
ggplot()+
  geom_line(DHS_estfr%>%filter(Education=="No Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="ESTFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))


ggplot()+
  geom_line(DHS_estfr%>%filter(Education=="Primary Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="ESTFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))



ggplot()+
  geom_line(DHS_estfr%>%filter(Education=="Secondary Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="ESTFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))



ggplot()+
  geom_line(DHS_estfr%>%filter(Education=="Higher Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="ESTFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))



##Based on the plots, the following edits are made

#Make Edits based on Educational attainment levels, starting with no education
DHS_estfr_2<-DHS_estfr[!(DHS_estfr$TFR<0.2 & DHS_estfr$Education=="No Education"),]
DHS_estfr_2<-DHS_estfr_2[!(DHS_estfr_2$TFR<2 & DHS_estfr_2$Education=="No Education" & DHS_estfr_2$Region=="Africa" & !DHS_estfr_2$Country %in% c("Tunisia","Lesotho")),]

DHS_estfr_2<-DHS_estfr_2[!(DHS_estfr_2$TFR<4 & DHS_estfr_2$Education=="No Education" & DHS_estfr_2$Country %in% c("Haiti","Cameroon")),]
DHS_estfr_2<-DHS_estfr_2[!(DHS_estfr_2$TFR>7 & DHS_estfr_2$Education=="No Education" & DHS_estfr_2$Country=="Azerbaijan"),]
DHS_estfr_2<-DHS_estfr_2[!(DHS_estfr_2$`Survey Year`%in%c(2007,2013) &DHS_estfr_2$Country=="Dominican Republic"),]



ggplot()+
  geom_line(DHS_estfr_2%>%filter(Education=="No Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="ESTFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))



#Primary Education
DHS_estfr_2<-DHS_estfr_2[!(DHS_estfr_2$TFR<=2.6 & DHS_estfr_2$Education=="Primary Education" & DHS_estfr_2$Region=="Africa"),]
DHS_estfr_2<-DHS_estfr_2[!(DHS_estfr_2$TFR<1.6 & DHS_estfr_2$Education=="Primary Education" & DHS_estfr_2$Country=="Albania"),]
DHS_estfr_2<-DHS_estfr_2[!(DHS_estfr_2$TFR<3.5 & DHS_estfr_2$Education=="Primary Education" & DHS_estfr_2$Country=="Cameroon"),]
DHS_estfr_2<-DHS_estfr_2[!(DHS_estfr_2$TFR>6 & DHS_estfr_2$Education=="Primary Education" & DHS_estfr_2$Country=="Armenia"),]



ggplot()+
  geom_line(DHS_estfr_2%>%filter(Education=="Primary Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="ESTFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))


#Secondary Education
DHS_estfr_2<-DHS_estfr_2[!(DHS_estfr_2$TFR<1 & DHS_estfr_2$Education=="Secondary Education" & DHS_estfr_2$Region=="Africa"),]
DHS_estfr_2<-DHS_estfr_2[!(DHS_estfr_2$TFR<0.8 & DHS_estfr_2$Education=="Secondary Education" & DHS_estfr_2$Country=="Albania"),]
DHS_estfr_2<-DHS_estfr_2[!(DHS_estfr_2$TFR<3.12 & DHS_estfr_2$Education=="Secondary Education" & DHS_estfr_2$Country=="Cameroon"),]


ggplot()+
  geom_line(DHS_estfr_2%>%filter(Education=="Secondary Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="ESTFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))


##Higher Education

DHS_estfr_2<-DHS_estfr_2[!(DHS_estfr_2$TFR>7 & DHS_estfr_2$Education=="Higher Education"),]


ggplot()+
  geom_line(DHS_estfr_2%>%filter(Education=="Higher Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="ESTFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))



DHS_estfr_2<-DHS_estfr_2[!(is.na(DHS_estfr_2$Education)),]
#####################################################
#Final ESTFR cleaned plots

ggplot()+
  geom_line(DHS_estfr_2,
            mapping=aes(x=Year ,y=TFR,group=interaction(`Survey Year`,Education),
                        colour=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))) + 
  facet_wrap(~Country)+
  labs(x="Year",y="ESTFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))+
  scale_color_discrete(name="Education")





##########################################################################################################################################################################
##########################################################################################################################################################################


#Then ESTFR for five year intervals
DHS_estfr_5<-read_excel("K:/project/BayesEdu/Fertility/Afua/TFR2 Datasets DHS/DHS datasets all countries.xlsx",sheet="ESTFR_5")
DHS_estfr_5<-DHS_estfr_5[!(DHS_estfr_5$Country=="Senegal" & DHS_estfr_5$`Survey Year`%in%c(2006,2008)),]
DHS_estfr_5<-DHS_estfr_5[!(is.na(DHS_estfr_5$`Survey Year`)),]

DHS_estfr_5$`Survey Year`<-as.factor(DHS_estfr_5$`Survey Year`)
DHS_estfr_5<-DHS_estfr_5[!(DHS_estfr_5$TFR<2 & DHS_estfr_5$Region=="Africa"),]

DHS_estfr_5<-DHS_estfr_5[!(DHS_estfr_5$`Survey Year`==1996 & DHS_estfr_5$Country=="Chad"),]
DHS_estfr_5<-DHS_estfr_5[!(DHS_estfr_5$`Survey Year`==2007 & DHS_estfr_5$Country=="Tanzania"),]
DHS_estfr_5<-DHS_estfr_5[!(DHS_estfr_5$`Survey Year`==2005 & DHS_estfr_5$Country=="Côte D'Ivoire"),]

#Delete already the values close to 0 and values above 25  
DHS_estfr_5<-DHS_estfr_5[!(DHS_estfr_5$TFR>12),]
DHS_estfr_5<-DHS_estfr_5[!(DHS_estfr_5$TFR<0.01),]



###Plot the original datasets to know which values to delete

ggplot()+
  geom_line(DHS_estfr_5,
            mapping=aes(x=Year ,y=TFR,group=interaction(`Survey Year`,Education),
                        colour=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))) + 
  facet_wrap(~Country)+
  labs(x="Year",y="estfr_5") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))+
  scale_color_discrete(name="Education")


ggplot()+
  geom_line(DHS_estfr_5%>%filter(Education=="No Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="estfr_5") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))


ggplot()+
  geom_line(DHS_estfr_5%>%filter(Education=="Primary Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="estfr_5") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))



ggplot()+
  geom_line(DHS_estfr_5%>%filter(Education=="Secondary Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="estfr_5") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))



ggplot()+
  geom_line(DHS_estfr_5%>%filter(Education=="Higher Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="estfr_5") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))



##Based on the plots, the following edits are made

#Make Edits based on Educational attainment levels, starting with no education
DHS_estfr_5_2<-DHS_estfr_5[!(DHS_estfr_5$TFR<2 & DHS_estfr_5$Region=="Africa" & DHS_estfr_5$Education=="No Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR<4.5 & DHS_estfr_5_2$Education=="No Education" & DHS_estfr_5_2$Country %in% c("Haiti","Cameroon")),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$`Survey Year`%in%c(2007,2013) &DHS_estfr_5_2$Country=="Dominican Republic"),]





ggplot()+
  geom_line(DHS_estfr_5_2%>%filter(Education=="No Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="estfr_5") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))





#Primary Education
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR<=2.6 & DHS_estfr_5_2$Education=="Primary Education" & DHS_estfr_5_2$Region=="Africa"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR<1.6 & DHS_estfr_5_2$Education=="Primary Education" & DHS_estfr_5_2$Country=="Albania"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR<3.5 & DHS_estfr_5_2$Education=="Primary Education" & DHS_estfr_5_2$Country=="Cameroon"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!( DHS_estfr_5_2$Education=="Primary Education" & DHS_estfr_5_2$Year=="1990-1995"  & DHS_estfr_5_2$Country=="Ukraine"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR>4 & DHS_estfr_5_2$Education=="Primary Education" & DHS_estfr_5_2$Country=="Armenia"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR<1 & DHS_estfr_5_2$Education=="Primary Education" & DHS_estfr_5_2$Country=="Armenia"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR>5 & DHS_estfr_5_2$Education=="Primary Education" & DHS_estfr_5_2$Country=="Kyrgyzstan"),]




ggplot()+
  geom_line(DHS_estfr_5_2%>%filter(Education=="Primary Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="estfr_5") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))


#Secondary Education
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR<1.5 & DHS_estfr_5_2$Education=="Secondary Education" & DHS_estfr_5_2$Region=="Africa"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR<0.8 & DHS_estfr_5_2$Education=="Secondary Education" & DHS_estfr_5_2$Country=="Albania"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR<3.12 & DHS_estfr_5_2$Education=="Secondary Education" & DHS_estfr_5_2$Country=="Cameroon"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$Year=="1955-1960" & DHS_estfr_5_2$Education=="Secondary Education" & DHS_estfr_5_2$Country=="Senegal"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR<2 & DHS_estfr_5_2$Education=="Secondary Education" & DHS_estfr_5_2$Country=="Cambodia"),]





ggplot()+
  geom_line(DHS_estfr_5_2%>%filter(Education=="Secondary Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="estfr_5") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))


##Higher Education
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR>8 & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR<1 & DHS_estfr_5_2$Region=="Africa" & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR<1 & DHS_estfr_5_2$Country=="Cambodia" & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR<=2.5 & DHS_estfr_5_2$Country=="Cameroon" & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR>=5& DHS_estfr_5_2$Country=="Senegal" & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR>7& DHS_estfr_5_2$Country=="Tanzania" & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR>5& DHS_estfr_5_2$Country=="Ghana" & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR>5.5& DHS_estfr_5_2$Country=="Benin" & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR>5.2& DHS_estfr_5_2$Country=="Burundi" & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR>5.5& DHS_estfr_5_2$Country=="Cameroon" & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR>5& DHS_estfr_5_2$Country%in%c("Dominican Republic","Ethiopia","Gabon","Liberia","Maldives","Uganda","Guatemala") &
                                 DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$`Survey Year`==1998 & DHS_estfr_5_2$Country=="Côte D'Ivoire" & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$`Survey Year`==2012 & DHS_estfr_5_2$Country=="Guniea" & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$`Survey Year`==1999 & DHS_estfr_5_2$Year=="1970-1975"  &
                                 DHS_estfr_5_2$Country=="Guniea" & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR>3.5 & DHS_estfr_5_2$TFR<1 & DHS_estfr_5_2$Country=="Haiti" & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$TFR>4.5 & DHS_estfr_5_2$Country=="Tanzania" & DHS_estfr_5_2$Education=="Higher Education"),]
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$`Survey Year`==1988 & DHS_estfr_5_2$Country=="Uganda" & DHS_estfr_5_2$Education=="Higher Education"),]


DHS_estfr_5_2<-DHS_estfr_5_2[!(is.na(DHS_estfr_5_2$Year)),]


ggplot()+
  geom_line(DHS_estfr_5_2%>%filter(Education=="Higher Education"),
            mapping=aes(x=Year ,y=TFR,group=`Survey Year`,
                        colour=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="estfr_5") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))



#####################################################
#Final estfr_5 cleaned plots

ggplot()+
  geom_line(DHS_estfr_5_2,
            mapping=aes(x=Year ,y=TFR,group=interaction(`Survey Year`,Education),
                        colour=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))) + 
  facet_wrap(~Country)+
  labs(x="Year",y="estfr_5") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))+
  scale_color_discrete(name="Education")



#######################################################################################################################################################################
#######################################################################################################################################################################

#Next is ASFR

#Then ASFR for one year intervals
DHS_asfr<-read_excel("K:/project/BayesEdu/Fertility/Afua/TFR2 Datasets DHS/DHS datasets all countries.xlsx",sheet="ASFR")
DHS_asfr<-DHS_asfr%>%mutate("SE/ASFR"=`Standard Error`/ASFR)


##Delete rows which se/ASFR is greater than 3
DHS_asfr<-DHS_asfr[!(DHS_asfr$`SE/ASFR`>3),]



DHS_asfr<-DHS_asfr[!(DHS_asfr$Country=="Senegal" & DHS_asfr$`Survey Year`%in%c(2006,2008)),]
DHS_asfr<-DHS_asfr[!(is.na(DHS_asfr$`Survey Year`)),]

DHS_asfr$`Survey Year`<-as.factor(DHS_asfr$`Survey Year`)

DHS_asfr<-DHS_asfr[!(DHS_asfr$`Survey Year`%in%c(2007,2013) & DHS_asfr$Country=="Dominican Republic"),]

###Plot the original datasets to know which values to delete

Countries<-unique(DHS_asfr$Country)
Country_plots<-list()

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_line(DHS_asfr%>%filter(Country==Country_),
              mapping=aes(x=Age,y=ASFR,
                          colour = `Survey Year`, 
                          group = `Survey Year`)) +
    facet_wrap(~Year)+
    scale_color_brewer()+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
}

##Based on the plots, the following edits are made

#Delete any values less than 0.009 and anything above 0.59
DHS_asfr2<-DHS_asfr[!(DHS_asfr$ASFR>0.59),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$ASFR<0.009),]


##Other deletions based on country specific information
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$ASFR>0.04 & DHS_asfr2$Age>30),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$ASFR>0.3 & DHS_asfr2$Country=="South Africa"),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$ASFR>0.45 & DHS_asfr2$Country=="Namibia"),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$ASFR>0.41 & DHS_asfr2$Country=="Sudan"),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$ASFR>0.5 & DHS_asfr2$Country=="Egypt"),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$`Survey Year`==2005 & DHS_asfr2$Country=="Côte D'Ivoire"),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$`Survey Year`==2011 & DHS_asfr2$Year==2005 & DHS_asfr2$Country=="Madagascar"),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$`Survey Year`==2013 & DHS_asfr2$Year==2007 & DHS_asfr2$Country=="Madagascar"),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$ASFR<0.1 & DHS_asfr2$Age==26 & DHS_asfr2$Year==2015 & DHS_asfr2$Country=="Rwanda"),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$`Survey Year`==2007 & DHS_asfr2$Country=="Tanzania"),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$ASFR>0.2 & DHS_asfr2$Year==1978 & DHS_asfr2$Country=="Azerbaijan"),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$ASFR>0.25 & DHS_asfr2$Year<1981 & DHS_asfr2$Country=="Guyana"),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$ASFR>0.1 & DHS_asfr2$Year>25 & DHS_asfr2$Country=="Guyana"),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$`Survey Year`==2000 & DHS_asfr2$Age>25 & DHS_asfr2$Year==1998 & DHS_asfr2$Country=="Haiti"),]

DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$ASFR>0.3 & DHS_asfr2$Country=="Moldova"),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$ASFR>0.3 & DHS_asfr2$Year<1990 & DHS_asfr2$Country=="Papua New Guinea"),]
DHS_asfr2<-DHS_asfr2[!(DHS_asfr2$ASFR>0.3 & DHS_asfr2$Year<1990 & DHS_asfr2$Country=="Ukraine"),]

DHS_asfr2<-DHS_asfr2[!(is.na(DHS_asfr2$Year)),]

#Plot again to see if improved

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_line(DHS_asfr2%>%filter(Country==Country_),
              mapping=aes(x=Age,y=ASFR,
                          colour = `Survey Year`, 
                          group = `Survey Year`)) +
    facet_wrap(~Year)+
    scale_color_brewer()+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
}

library(gridExtra)

pdf("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/ASFR_cleaned.pdf", onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()




for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(DHS_asfr2%>%filter(Country==Country_), 
                mapping = aes(x=Age, ymin=ASFR-`Standard Error`, ymax=ASFR+`Standard Error`, group=`Survey Year`),alpha=0.9, fill="grey") +
    geom_line(DHS_asfr2%>%filter(Country==Country_),
              mapping=aes(x=Age,y=ASFR,
                          colour = `Survey Year`, 
                          group = `Survey Year`)) +
    facet_wrap(~Year)+
    theme_bw() +
    scale_color_brewer()+
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
}

library(gridExtra)

pdf("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/ASFR_CI_cleaned.pdf", onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()

###########################################################################################################################################################################
###########################################################################################################################################################################


#Then ASFR for five year intervals
DHS_asfr_5<-read_excel("K:/project/BayesEdu/Fertility/Afua/TFR2 Datasets DHS/DHS datasets all countries.xlsx",sheet="ASFR_5")
DHS_asfr_5<-DHS_asfr_5%>%mutate("SE/ASFR"=`Standard Error`/ASFR)


##Delete rows which se/ASFR is greater than 1
DHS_asfr_5<-DHS_asfr_5[!(DHS_asfr_5$`SE/ASFR`>1),]


DHS_asfr_5<-DHS_asfr_5[!(DHS_asfr_5$Country=="Senegal" & DHS_asfr_5$`Survey Year`%in%c(2006,2008)),]
DHS_asfr_5<-DHS_asfr_5[!(is.na(DHS_asfr_5$`Survey Year`)),]

DHS_asfr_5$`Survey Year`<-as.factor(DHS_asfr_5$`Survey Year`)

DHS_asfr_5<-DHS_asfr_5[!(DHS_asfr_5$`Survey Year`==2005 & DHS_asfr_5$Country=="Côte D'Ivoire"),]
DHS_asfr_5<-DHS_asfr_5[!(DHS_asfr_5$`Survey Year`==2005 & DHS_asfr_5$Country=="Vietnam"),]
DHS_asfr_5<-DHS_asfr_5[!(DHS_asfr_5$`Survey Year`==2012 & DHS_asfr_5$Year=="2000-2005" & DHS_asfr_5$Country=="Burundi"),]
DHS_asfr_5<-DHS_asfr_5[!(DHS_asfr_5$`Survey Year`==2013 & DHS_asfr_5$Year=="2005-2010"& DHS_asfr_5$Country=="Rwanda"),]
DHS_asfr_5<-DHS_asfr_5[!(DHS_asfr_5$`Survey Year`==2007 & DHS_asfr_5$Country=="Tanzania"),]
DHS_asfr_5<-DHS_asfr_5[!(DHS_asfr_5$`Survey Year`%in%c(2007,2013) & DHS_asfr_5$Country=="Dominican Republic"),]

###Plot the original datasets to know which values to delete

Countries<-unique(DHS_asfr_5$Country)
Country_plots<-list()


##Based on the plots, the following edits are made

#Delete any values less than 0.009 and anything above 0.59
DHS_asfr_5_2<-DHS_asfr_5[!(DHS_asfr_5$ASFR>0.45),]
DHS_asfr_5_2<-DHS_asfr_5_2[!(DHS_asfr_5_2$ASFR<0.05 & DHS_asfr_5_2$Age<40),]
DHS_asfr_5_2<-DHS_asfr_5_2[!(DHS_asfr_5_2$ASFR==0 & DHS_asfr_5_2$Age>=40 & DHS_asfr_5_2$Region=="Africa"),]

DHS_asfr_5_2<-DHS_asfr_5_2[!(DHS_asfr_5_2$`Survey Year`==1999 & DHS_asfr_5_2$Country=="Mozambique"),]
DHS_asfr_5_2<-DHS_asfr_5_2[!(DHS_asfr_5_2$`Survey Year`==2009 & DHS_asfr_5_2$Year=="2005-2010" & DHS_asfr_5_2$Country=="Mozambique"),]
DHS_asfr_5_2<-DHS_asfr_5_2[!(DHS_asfr_5_2$`Survey Year`==1999 & DHS_asfr_5_2$Year%in%c("1980-1985","1985-1990") & DHS_asfr_5_2$Age>20 & DHS_asfr_5_2$Country=="Mozambique"),]

DHS_asfr_5_2<-DHS_asfr_5_2[!(DHS_asfr_5_2$ASFR>0.15 & DHS_asfr_5_2$Age>30 & DHS_asfr_5_2$Country=="Sao Tome and Principe"),]
DHS_asfr_5_2<-DHS_asfr_5_2[!(DHS_asfr_5_2$`Survey Year`==2018 & DHS_asfr_5_2$Year%in%c("1990-1995","1995-2000") & DHS_asfr_5_2$Country=="Cameroon"),]
DHS_asfr_5_2<-DHS_asfr_5_2[!(DHS_asfr_5_2$`Survey Year`==2018 & DHS_asfr_5_2$Year%in%c("1990-1995","1995-2000") & DHS_asfr_5_2$Country=="Guinea"),]
DHS_asfr_5_2<-DHS_asfr_5_2[!(DHS_asfr_5_2$`Survey Year`==2012 & DHS_asfr_5_2$Year%in%c("1980-1985","1985-1990") & DHS_asfr_5_2$Country=="Mali"),]



DHS_asfr_5_2<-DHS_asfr_5_2[!(DHS_asfr_5_2$Year=="1975-1980" & DHS_asfr_5_2$Country=="Cambodia"),]
DHS_asfr_5_2<-DHS_asfr_5_2[!(DHS_asfr_5_2$`Survey Year`==2016 & DHS_asfr_5_2$Year=="1985-1990" & DHS_asfr_5_2$Country=="Haiti"),]
DHS_asfr_5_2<-DHS_asfr_5_2[!(DHS_asfr_5_2$`Survey Year`==2016 & DHS_asfr_5_2$Year%in%c("1990-1995","1995-2000") & DHS_asfr_5_2$Age>20 & DHS_asfr_5_2$Country=="Haiti"),]

DHS_asfr_5_2<-DHS_asfr_5_2[!(is.na(DHS_asfr_5_2$Year)),]

#Plot again to see if improved

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_line(DHS_asfr_5_2%>%filter(Country==Country_),
              mapping=aes(x=Age,y=ASFR,
                          colour = `Survey Year`, 
                          group = `Survey Year`)) +
    facet_wrap(~Year)+
    scale_color_brewer()+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
}

library(gridExtra)

pdf("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/ASFR_5_cleaned.pdf", onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()


for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(DHS_asfr_5_2%>%filter(Country==Country_), 
                mapping = aes(x=Age, ymin=ASFR-`Standard Error`, ymax=ASFR+`Standard Error`, group=`Survey Year`),alpha=0.9, fill="grey") +
    geom_line(DHS_asfr_5_2%>%filter(Country==Country_),
              mapping=aes(x=Age,y=ASFR,
                          colour = `Survey Year`, 
                          group = `Survey Year`)) +
    facet_wrap(~Year)+
    scale_color_brewer()+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
}

library(gridExtra)

pdf("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/ASFR_5_CI_cleaned.pdf", onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()



###########################################################################################################################################################################
###########################################################################################################################################################################

#Then ESASFR for one year intervals
#Take time later to find out what's going on here!!!!!!!!!
#DHS_esasfr<-read_excel("K:/project/BayesEdu/Fertility/Afua/TFR2 Datasets DHS/DHS datasets all countries.xlsx",sheet="ESASFR")
#DHS_esasfr$`Survey Year`<-as.factor(DHS_esasfr$`Survey Year`)

####################

#Clean data based on No education
#Since almost all completed Schedules do not go above 0.6, delete ASFR above 0.6 and delete very small values that do not make sense

#DHS_esasfr_2<-DHS_esasfr[!(DHS_esasfr$ASFR>0.6 & DHS_esasfr$Education=="No Education"),]
#DHS_esasfr_2<-DHS_esasfr_2[!(DHS_esasfr_2$ASFR<0.02 & DHS_esasfr_2$Age<40 & DHS_esasfr_2$Education=="No Education"),]

#Delete very high values for over 35 years 
#DHS_esasfr_2<-DHS_esasfr_2[!(DHS_esasfr_2$ASFR<0.2 & DHS_esasfr_2$Age>35 & DHS_esasfr_2$Education=="No Education"),]
#DHS_esasfr_2<-na.omit(DHS_esasfr_2)


##Plot again to see improvements
#Countries<-unique(DHS_esasfr_2$Country)
#Country_plots<-list()
#for(Country_ in Countries) {
#  Country_plots[[Country_]] =ggplot() +
#    geom_line(DHS_esasfr_2%>%filter(Country==Country_),
#              mapping=aes(x=Age,y=ASFR,colour=`Survey Year`,
#                          group = interaction(Education,`Survey Year`))) +
#    facet_wrap(Education~Year)+
#    theme_bw() +
#    theme(axis.text.x = element_text(angle=90))+
#    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) + 
#    labs(x="Age Group",y="ESASFR") +
#    theme(plot.title = element_text(size = 10, hjust=0.5))
#  
#  print(Country_plots[[Country_]])
#}




#Then do as well for Primary Education

#for(Country_ in Countries) {
#  Country_plots[[Country_]] =ggplot() +
#    geom_line(DHS_esasfr%>%filter(Country==Country_, Education=="Primary Education"),
#              mapping=aes(x=Age,y=ASFR,colour=`Survey Year`,
#                          group = interaction(Education,`Survey Year`))) +
#    facet_wrap(~Year)+
#    theme_bw() +
#    theme(axis.text.x = element_text(angle=90))+
#    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) + 
#    labs(x="Age Group",y="ESASFR") +
#    theme(plot.title = element_text(size = 10, hjust=0.5))
  
#  print(Country_plots[[Country_]])
#}



#Now clean the data by level 






###########################################################################################################################################################################
###########################################################################################################################################################################


#Then ESASFR for five year intervals
DHS_esasfr_5<-read_excel("K:/project/BayesEdu/Fertility/Afua/TFR2 Datasets DHS/DHS datasets all countries.xlsx",sheet="ESASFR_5")
DHS_esasfr_5<-DHS_esasfr_5%>%mutate("SE/ASFR"=`Standard Error`/ASFR)



##Delete rows which se/ASFR is greater than 3
DHS_esasfr_5<-DHS_esasfr_5[!(DHS_esasfr_5$`SE/ASFR`>3),]

DHS_esasfr_5<-DHS_esasfr_5[!(DHS_esasfr_5$Country=="Senegal" & DHS_esasfr_5$`Survey Year`%in%c(2006,2008)),]
DHS_esasfr_5<-DHS_esasfr_5[!(is.na(DHS_esasfr_5$`Survey Year`)),]

DHS_esasfr_5$`Survey Year`<-as.factor(DHS_esasfr_5$`Survey Year`)


###
#Plot the data to see the changes to be done, starting with No Education

Countries<-unique(DHS_esasfr_5$Country)
Country_plots<-list()
for(Country_ in Countries) {
  Country_plots[[Country_]] =ggplot() +
    geom_line(DHS_esasfr_5%>%filter(Country==Country_, Education=="No Education"),
              mapping=aes(x=Age,y=ASFR,colour=`Survey Year`,
                          group = interaction(Education,`Survey Year`))) +
    facet_wrap(~Year)+
    scale_color_brewer()+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ESASFR") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
}




###From the plots we can delete for ASFR above 0.7 and anything close to 0 for ages below 40 (below 0.02)

DHS_esasfr_5_2<-DHS_esasfr_5[!(DHS_esasfr_5$ASFR>0.7),]
DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR<0.05 & DHS_esasfr_5_2$Age<40),]

##Based off on the graph make the following changes

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$`Survey Year`==2005 & DHS_esasfr_5_2$Country=="Côte D'Ivoire"),]
DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$`Survey Year`==2005 & DHS_esasfr_5_2$Country=="Vietnam"),]

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$`Survey Year`==2012 & DHS_esasfr_5_2$Year=="2000-2005" & DHS_esasfr_5_2$Country=="Burundi"),]
DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$`Survey Year`==2013 & DHS_esasfr_5_2$Year=="2005-2010"& DHS_esasfr_5_2$Country=="Rwanda"),]

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR==0 & DHS_esasfr_5_2$Age>=40 & DHS_esasfr_5_2$Region=="Africa"),]
DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$`Survey Year`==2007 & DHS_esasfr_5_2$Country=="Tanzania"),]

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.4 & DHS_esasfr_5_2$Age>30& DHS_esasfr_5_2$Country=="Dominican Republic"),]
DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$`Survey Year`%in%c(2007,2013) & DHS_esasfr_5_2$Country=="Dominican Republic"),]


DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$`Survey Year`==2009 & 
                                   DHS_esasfr_5_2$Year=="2005-2010" & DHS_esasfr_5_2$Country=="Mozambique"),]

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$Age>=30 & DHS_esasfr_5_2$ASFR>0.1 & DHS_esasfr_5_2$`Survey Year`==2018  & DHS_esasfr_5_2$Country=="Cameroon"),]


###Another plot to see how the data is going No Education is cleaned
Countries<-unique(DHS_esasfr_5_2$Country)
Country_plots<-list()
for(Country_ in Countries) {
  Country_plots[[Country_]] =ggplot() +
    geom_line(DHS_esasfr_5_2%>%filter(Country==Country_, Education=="No Education"),
              mapping=aes(x=Age,y=ASFR,colour=`Survey Year`,
                          group = interaction(Education,`Survey Year`))) +
    facet_wrap(~Year)+
    scale_color_brewer()+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ESASFR") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
}



DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.2 & 
                                   DHS_esasfr_5_2$Year=="2010-2015" & DHS_esasfr_5_2$Country=="Maldives" & DHS_esasfr_5_2$Education=="No Education"),]


DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$Year=="1995-2000" & DHS_esasfr_5_2$Country=="Moldova" & DHS_esasfr_5_2$Education=="No Education"),]

##Primary Education

#Delete Mozambique 2005-2010 Survey Year 2009 for Primary Education

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$`Survey Year`==2009 & 
                                   DHS_esasfr_5_2$Year=="2005-2010" & DHS_esasfr_5_2$Country=="Mozambique" & DHS_esasfr_5_2$Education=="Primary Education"),]

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.2 & DHS_esasfr_5_2$Age>=25 &
                                   DHS_esasfr_5_2$Year=="1995-2000" & DHS_esasfr_5_2$Country=="Ukraine" & DHS_esasfr_5_2$Education=="Primary Education"),]



#Also delete for Togo above 30 years with more than 0.2 for Togo

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.2 & DHS_esasfr_5_2$Country=="Togo" & DHS_esasfr_5_2$Age>30 & DHS_esasfr_5_2$Education=="Primary Education"),]

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.08 & DHS_esasfr_5_2$Country=="Azerbaijan" & DHS_esasfr_5_2$Age>30 & DHS_esasfr_5_2$Education=="Primary Education"),]
DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.1 & DHS_esasfr_5_2$Country=="Yemen" & DHS_esasfr_5_2$Age>40 & DHS_esasfr_5_2$Education=="Primary Education"),]

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.2 & DHS_esasfr_5_2$Country=="Armenia" & DHS_esasfr_5_2$Education=="Primary Education"),]


DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$Year%in%c("1965-1970","1970-1975","1975-1980","1980-1985","1985-1990","1990-1995","1995-2000") &
                                   DHS_esasfr_5_2$Country=="Kyrgyzstan" & DHS_esasfr_5_2$Education=="Primary Education"),]


#Data cleaned for Primary and No Education

##Secondary Education


for(Country_ in Countries) {
  Country_plots[[Country_]] =ggplot() +
    geom_line(DHS_esasfr_5_2%>%filter(Country==Country_, Education=="Secondary Education"),
              mapping=aes(x=Age,y=ASFR,colour=`Survey Year`,
                          group = interaction(Education,`Survey Year`))) +
    facet_wrap(~Year)+
    scale_color_brewer()+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ESASFR") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
}



###Delete  Mali, year 2000-2005 for Secondary Education, delete above 0.25

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.25 & DHS_esasfr_5_2$Country=="Mali" & DHS_esasfr_5_2$Year=="2000-2005" &
                                   DHS_esasfr_5_2$Education=="Secondary Education"),]
 

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$Country=="Bangladesh" & DHS_esasfr_5_2$Age>40 & DHS_esasfr_5_2$Year %in% c("1985-1990","1990-1995") &
                                   DHS_esasfr_5_2$Education=="Secondary Education"),]

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$Country=="Bangladesh" & DHS_esasfr_5_2$Age>30 & DHS_esasfr_5_2$Year%in% c("1980-1985","1985-1990") &
                                   DHS_esasfr_5_2$Education=="Secondary Education"),]


DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$Country=="Thailand" & DHS_esasfr_5_2$Year=="1970-1975" &
                                   DHS_esasfr_5_2$Education%in%c("Secondary Education","Higher Education")),]



#Data cleaned for Secondary!!


##Higher Education

for(Country_ in Countries) {
  Country_plots[[Country_]] =ggplot() +
    geom_line(DHS_esasfr_5_2%>%filter(Country==Country_, Education=="Higher Education"),
              mapping=aes(x=Age,y=ASFR,colour=`Survey Year`,
                          group = interaction(Education,`Survey Year`))) +
    facet_wrap(~Year)+
    scale_color_brewer()+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ESASFR") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
}






#Delete age above 30 more than 0.3 ASFR for higher Education

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.3 & DHS_esasfr_5_2$Age>30 & DHS_esasfr_5_2$Education=="Higher Education"),]



DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.1 & DHS_esasfr_5_2$Age>30 & DHS_esasfr_5_2$Education=="Higher Education" & DHS_esasfr_5_2$Country=="Ethiopia"),]
DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.2 & DHS_esasfr_5_2$Age>25 &
                                   DHS_esasfr_5_2$Education=="Higher Education" & DHS_esasfr_5_2$Country=="Central African Republic"),]

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.25 & DHS_esasfr_5_2$Education=="Higher Education" & 
                                   DHS_esasfr_5_2$Country%in%c("Chad","Mali","Tanzania","Senegal","Papua New Guinea","Timor-Leste")),]

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.2 &
                                   DHS_esasfr_5_2$Education=="Higher Education" & 
                                   DHS_esasfr_5_2$Country%in%c("Congo","Benin","Burkina Faso","Cameroon","Haiti","Morocco","Maldives","Guatemala")),]
DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.15 & DHS_esasfr_5_2$Age>25 & DHS_esasfr_5_2$Education=="Higher Education" & DHS_esasfr_5_2$Country=="Gabon"),]
DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$Age<20 & DHS_esasfr_5_2$Education=="Higher Education" & DHS_esasfr_5_2$Country=="Sao Tome and Principe"),]



DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.1 & DHS_esasfr_5_2$Age>30 &
                                   DHS_esasfr_5_2$Education=="Higher Education" & DHS_esasfr_5_2$Country=="Dominican Republic"),]

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$ASFR>0.3 & DHS_esasfr_5_2$Education=="Higher Education" & DHS_esasfr_5_2$Country=="Indonesia"),]

DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$Year=="1995-2000" & DHS_esasfr_5_2$`Survey Year`==2003 &
                                   DHS_esasfr_5_2$Education=="Higher Education" & DHS_esasfr_5_2$Country=="Ghana"),]


DHS_esasfr_5_2<-DHS_esasfr_5_2[!(DHS_esasfr_5_2$Year=="2010-2015" & DHS_esasfr_5_2$Age>40 &
                                   DHS_esasfr_5_2$Education=="Higher Education" & DHS_esasfr_5_2$Country=="Cambodia"),]


DHS_esasfr_5_2<-DHS_esasfr_5_2[!(is.na(DHS_esasfr_5_2$Year)),]
###############

#Plot cleaned Data
Countries<-unique(DHS_esasfr_5_2$Country)
Country_plots<-list()
# looping over unique countries
for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_line(DHS_esasfr_5_2%>% filter(Country==Country_),mapping = aes(x=Age,
                                                                      y=ASFR,group=`Survey Year`,colour=`Survey Year`))+
    facet_grid(factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education"))~Year)+
    scale_color_brewer()+
    theme_bw()+
    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR",colour="Estimate") +
    theme(strip.text.x = element_text(size=4.5),plot.title = element_text(size = 12, hjust=0.5), axis.title.x = element_text(size = 12), 
          axis.title.y = element_text(size = 18),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  print(Country_plots[[Country_]])
  
  
}

library(gridExtra)
pdf("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/ESASFR_5_cleaned.pdf", onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()




DHS_esasfr_5_2_<-DHS_esasfr_5_2%>%filter(Year%in%c("1985-1990","1990-1995","1995-2000","2000-2005","2005-2010","2010-2015"))
DHS_esasfr_5_2_<-DHS_esasfr_5_2_[!(is.na(DHS_esasfr_5_2_$Year)),]


Countries<-unique(DHS_esasfr_5_2_$Country)
Country_plots<-list()
for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(DHS_esasfr_5_2_%>% filter(Country==Country_),mapping = aes(x=Age,
                                                                        ymin=ASFR-`Standard Error`,ymax=ASFR+`Standard Error`,group=`Survey Year`),fill="grey")+
    geom_line(DHS_esasfr_5_2_%>% filter(Country==Country_),mapping = aes(x=Age,
                                                                        y=ASFR,group=`Survey Year`,colour=`Survey Year`))+
    facet_grid(factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education"))~Year)+
    
    theme_bw()+
    scale_color_brewer()+
    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR",colour="Estimate") +
    theme(strip.text.x = element_text(size=10),strip.text.y = element_text(size=10),plot.title = element_text(size = 14, hjust=0.5), axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 18),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  print(Country_plots[[Country_]])
  
  
}

library(gridExtra)
pdf("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/ESASFR_5_CI_cleaned.pdf", onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()

###########################################################################################################################################################################
###########################################################################################################################################################################


#We now save these Cleaned datasets into an Excel file

library(writexl)
library(tidyverse)
DHS_asfr2<-DHS_asfr2%>%mutate("SE/ASFR"=`Standard Error`/ASFR)
DHS_asfr_5_2<-DHS_asfr_5_2%>%mutate("SE/ASFR"=`Standard Error`/ASFR)
DHS_esasfr_5_2<-DHS_esasfr_5_2%>%mutate("SE/ASFR"=`Standard Error`/ASFR)


DHS_tfr2<-DHS_tfr2%>%mutate("SE/TFR"=`Standard Error`/TFR)
DHS_tfr_5_2<-DHS_tfr_5_2%>%mutate("SE/TFR"=`Standard Error`/TFR)
DHS_estfr_2<-DHS_estfr_5_2%>%mutate("SE/TFR"=`Standard Error`/TFR)
DHS_estfr_5_2<-DHS_estfr_5_2%>%mutate("SE/TFR"=`Standard Error`/TFR)








DHS_tfr2<-DHS_tfr2%>%mutate(Upper_CI=TFR+`Standard Error`, Lower_CI=TFR-`Standard Error`)

DHS_tfr_5_2<-DHS_tfr_5_2%>%mutate(Upper_CI=TFR+`Standard Error`, Lower_CI=TFR-`Standard Error`)

DHS_estfr_2<-DHS_estfr_2%>%mutate(Upper_CI=TFR+`Standard Error`, Lower_CI=TFR-`Standard Error`)

DHS_estfr_5_2<-DHS_estfr_5_2%>%mutate(Upper_CI=TFR+`Standard Error`, Lower_CI=TFR-`Standard Error`)

DHS_asfr2<-DHS_asfr2%>%mutate(Upper_CI=ASFR+`Standard Error`, Lower_CI=ASFR-`Standard Error`)

DHS_asfr_5_2<-DHS_asfr_5_2%>%mutate(Upper_CI=ASFR+`Standard Error`, Lower_CI=ASFR-`Standard Error`)

DHS_esasfr_5_2<-DHS_esasfr_5_2%>%mutate(Upper_CI=ASFR+`Standard Error`, Lower_CI=ASFR-`Standard Error`)





###For ESTFR remove if SE/TFR>1

DHS_estfr_2<-DHS_estfr_2[!(DHS_estfr_2$`SE/TFR`>1),]

###For ESTFR_5 remove if SE/TFR>3
DHS_estfr_5_2<-DHS_estfr_5_2[!(DHS_estfr_5_2$`SE/TFR`>3),]


library(writexl)

write_xlsx(list("TFR"=DHS_tfr2,"TFR_5"=DHS_tfr_5_2, "ESTFR"=DHS_estfr_2, "ESTFR_5"=DHS_estfr_5_2,
                "ASFR"=DHS_asfr2,"ASFR_5"=DHS_asfr_5_2, "ESASFR_5"=DHS_esasfr_5_2 ),
           path ="K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/Cleaned_DHS.xlsx", col_names=TRUE)



write_xlsx(list("TFR"=DHS_tfr2%>%filter(Region %in% c("Africa","North Africa")),"TFR_5"=DHS_tfr_5_2%>%filter(Region %in% c("Africa","North Africa")),
                "ESTFR"=DHS_estfr_2%>%filter(Region %in% c("Africa","North Africa")), "ESTFR_5"=DHS_estfr_5_2%>%filter(Region %in% c("Africa","North Africa")),
                "ASFR"=DHS_asfr2%>%filter(Region %in% c("Africa","North Africa")),"ASFR_5"=DHS_asfr_5_2%>%filter(Region %in% c("Africa","North Africa")),
                "ESASFR_5"=DHS_esasfr_5_2 %>%filter(Region %in% c("Africa","North Africa"))),
           path ="K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/Cleaned_DHS_Africa.xlsx", col_names=TRUE)


##############################################

#Make Plots with CIs for Africa

#TFR
ggplot()+
  geom_ribbon(DHS_tfr2%>%filter(Region %in% c("Africa","North Africa")), mapping = aes(x=Year, ymin=Lower_CI, ymax=Upper_CI, group=`Survey Year`), fill="darkgrey")+
  geom_line(DHS_tfr2%>%filter(Region %in% c("Africa","North Africa")),
            mapping=aes(x=Year ,y=TFR,colour=`Survey Year`,group=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="TFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))


#TFR_5
ggplot()+
  geom_ribbon(DHS_tfr_5_2%>%filter(Region %in% c("Africa","North Africa")), mapping = aes(x=Year, ymin=Lower_CI, ymax=Upper_CI, group=`Survey Year`), fill="darkgrey")+
  geom_line(DHS_tfr_5_2%>%filter(Region %in% c("Africa","North Africa")),
            mapping=aes(x=Year ,y=TFR,colour=`Survey Year`,group=`Survey Year`)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="TFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))



#ESFTR_5
DHS_estfr_5_2<-DHS_estfr_5_2%>%mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

ggplot()+
  geom_ribbon(DHS_estfr_5_2%>%filter(Region %in% c("Africa","North Africa")),
              mapping = aes(x=Year, ymin=Lower_CI, ymax=Upper_CI,group=interaction(`Survey Year`,Education)),alpha=0.5,fill="darkgrey")+
  geom_line(DHS_estfr_5_2%>%filter(Region %in% c("Africa","North Africa")),
            mapping=aes(x=Year ,y=TFR,group=interaction(`Survey Year`,Education),
                        colour=Education)) + 
  facet_wrap(~Country)+
  labs(x="Year",y="ESTFR") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))+
  scale_color_discrete(name="Education")







