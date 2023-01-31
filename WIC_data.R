library(readxl)
library(MCMCvis)
library(tidyverse)
library(coda)
library(zoo)
library(rjags)
library(glm2)
library(expss)



dat<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/Cleaned_DHS.xlsx",sheet="ESASFR_5")
dat<-dat%>%select("Country code","Country","Region")
dat<-unique(dat)
dat_edu<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Inputs plots (with glm predict)/glm_predict_all.xlsx",sheet="ESASFR_5")


library(wcde)
library(stringr)

wice_dat_<-past_epop
wice_dat_$age<-str_replace_all(wice_dat_$age,'--','-')
wice_dat_<-wice_dat_%>%filter(sex=="Female")%>%filter(age%in%c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))%>%
  filter(country_code%in%c(unique(dat_edu$`Country code`)))%>%filter(education!="Under 15")



wice_dat_$year[wice_dat_$year==1970]="1970-1975"
wice_dat_$year[wice_dat_$year==1975]="1975-1980"
wice_dat_$year[wice_dat_$year==1980]="1980-1985"
wice_dat_$year[wice_dat_$year==1985]="1985-1990"
wice_dat_$year[wice_dat_$year==1990]="1990-1995"
wice_dat_$year[wice_dat_$year==1995]="1995-2000"
wice_dat_$year[wice_dat_$year==2000]="2000-2005"
wice_dat_$year[wice_dat_$year==2005]="2005-2010" 
wice_dat_$year[wice_dat_$year==2010]="2010-2015"
wice_dat_$year[wice_dat_$year==2015]="2015-2020"

colnames(wice_dat_)[3]<-"Year"

wice_dat_$education[wice_dat_$education=="Incomplete Primary"]<-"Primary Education"
wice_dat_$education[wice_dat_$education=="Primary"]<-"Primary Education"
wice_dat_$education[wice_dat_$education=="Lower Secondary"]<-"Secondary Education"
wice_dat_$education[wice_dat_$education=="Upper Secondary"]<-"Secondary Education"
wice_dat_$education[wice_dat_$education=="Post Secondary"]<-"Higher Education"


wice_dat_<-wice_dat_[,-c(1,6)]
colnames(wice_dat_)[1]<-"Country code"
colnames(wice_dat_)[3]<-"Age Group"
colnames(wice_dat_)[4]<-"Education"
colnames(wice_dat_)[5]<-"Pop"


wice_dat_<-wice_dat_%>%filter(Year%in%c("1970-1975",
                                                      "1975-1980", "1980-1985", "1985-1990", "1990-1995",
                                                      "1995-2000", "2000-2005", "2005-2010", "2010-2015","2015-2020"))



wice_dat_<-wice_dat_%>%
  group_by(`Age Group`,`Country code`,Year,Education)%>%
  summarise(Pop=(sum(Pop)))

wice_dat_$Pop[wice_dat_$Pop==0]<-1

Newdat_edu2<-left_join(dat_edu,wice_dat_, by=c("Age Group","Year","Country code","Education"))

  
wice_dat_3<-Newdat_edu2[,-5]%>%spread(Education,Pop)


wice_dat_3_1<-wice_dat_3[,-c(6:8)]%>%spread(`Age Group`,`Higher Education`)
wice_dat_3_2<-wice_dat_3[,-c(5,7,8)]%>%spread(`Age Group`,`No Education`)
wice_dat_3_3<-wice_dat_3[,-c(5,6,8)]%>%spread(`Age Group`,`Primary Education`)
wice_dat_3_4<-wice_dat_3[,-c(5:7)]%>%spread(`Age Group`,`Secondary Education`)


#Saving WIC data

write_xlsx(list("WIC_highedu"=wice_dat_3_1,"WIC_noedu"=wice_dat_3_2,"WIC_priedu"=wice_dat_3_3,"WIC_secedu"=wice_dat_3_4),
           path ="K:/project/BayesEdu/Fertility/Afua/WIC_datasets.xlsx", col_names=TRUE)



##################################################################################33
#version 2

library(readxl)
library(MCMCvis)
library(tidyverse)
library(coda)
library(zoo)
library(rjags)
library(glm2)
library(expss)
library(writexl)



dat<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/Cleaned_DHS.xlsx",sheet="ESASFR_5")
dat<-dat%>%select("Country code","Country","Region")
dat<-unique(dat)
dat_edu<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Inputs plots (with glm predict)/glm_predict_all.xlsx",sheet="ESASFR_5")


library(wcde)
library(stringr)

wice_dat2_<-past_epop
wice_dat2_$age<-str_replace_all(wice_dat2_$age,'--','-')
wice_dat2_<-wice_dat2_%>%filter(sex=="Female")%>%filter(age%in%c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))%>%
  filter(country_code%in%c(unique(dat_edu$`Country code`)))%>%filter(education!="Under 15")


wice_dat2_$year[wice_dat2_$year==1950]="1950-1955"
wice_dat2_$year[wice_dat2_$year==1955]="1955-1960"
wice_dat2_$year[wice_dat2_$year==1960]="1960-1965"
wice_dat2_$year[wice_dat2_$year==1965]="1965-1970"
wice_dat2_$year[wice_dat2_$year==1970]="1970-1975"
wice_dat2_$year[wice_dat2_$year==1975]="1975-1980"
wice_dat2_$year[wice_dat2_$year==1980]="1980-1985"
wice_dat2_$year[wice_dat2_$year==1985]="1985-1990"
wice_dat2_$year[wice_dat2_$year==1990]="1990-1995"
wice_dat2_$year[wice_dat2_$year==1995]="1995-2000"
wice_dat2_$year[wice_dat2_$year==2000]="2000-2005"
wice_dat2_$year[wice_dat2_$year==2005]="2005-2010" 
wice_dat2_$year[wice_dat2_$year==2010]="2010-2015"
wice_dat2_$year[wice_dat2_$year==2015]="2015-2020"

colnames(wice_dat2_)[3]<-"Year"

wice_dat2_$education[wice_dat2_$education=="Incomplete Primary"]<-"Primary Education"
wice_dat2_$education[wice_dat2_$education=="Primary"]<-"Primary Education"
wice_dat2_$education[wice_dat2_$education=="Lower Secondary"]<-"Secondary Education"
wice_dat2_$education[wice_dat2_$education=="Upper Secondary"]<-"Secondary Education"
wice_dat2_$education[wice_dat2_$education=="Post Secondary"]<-"Higher Education"


wice_dat2_<-wice_dat2_[,-c(1,6)]
colnames(wice_dat2_)[1]<-"Country code"
colnames(wice_dat2_)[3]<-"Age Group"
colnames(wice_dat2_)[4]<-"Education"
colnames(wice_dat2_)[5]<-"Pop"



wice_dat2_<-wice_dat2_%>%
  group_by(`Age Group`,`Country code`,Year,Education)%>%
  summarise(Pop=(sum(Pop)))

wice_dat2_$Pop[wice_dat2_$Pop==0]<-1

wice_dat2_$Region<-vlookup(wice_dat2_$`Country code`,dat, "Region")
wice_dat2_$Country<-vlookup(wice_dat2_$`Country code`,dat, "Country")

wice_dat2_<-wice_dat2_%>%filter(!Year%in%c("1950-1955",2020),
                                Education%in%c("No Education","Primary Education","Secondary Education","Higher Education"))

wice_dat2_3<-wice_dat2_[,-6]%>%spread(Education,Pop)


wice_dat2_3_1<-wice_dat2_3[,-c(6:8)]%>%spread(`Age Group`,`Higher Education`)
wice_dat2_3_2<-wice_dat2_3[,-c(5,7,8)]%>%spread(`Age Group`,`No Education`)
wice_dat2_3_3<-wice_dat2_3[,-c(5,6,8)]%>%spread(`Age Group`,`Primary Education`)
wice_dat2_3_4<-wice_dat2_3[,-c(5:7)]%>%spread(`Age Group`,`Secondary Education`)


#Saving WIC data

write_xlsx(list("WIC_highedu"=wice_dat2_3_1,"WIC_noedu"=wice_dat2_3_2,"WIC_priedu"=wice_dat2_3_3,"WIC_secedu"=wice_dat2_3_4),
           path ="K:/project/BayesEdu/Fertility/Afua/WIC_datasets2.xlsx", col_names=TRUE)


