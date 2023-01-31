#Update the UN WPP 2019 to UN WPP 2022
library(readxl)
library(tidyverse)
library(zoo)
library(glm2)
library(expss)
library(writexl)

dat<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/Cleaned_DHS.xlsx",sheet="ESASFR_5")
dat<-dat%>%select("Country code","Country","Region")
dat<-unique(dat)

library(wpp2022)
data("percentASFR")
attach(percentASFR)
UNfert<-percentASFR[,c(1:3,5:17)]
UNfert<-UNfert%>%filter(!age%in%c("10-14","50-54"))


data(tfr)
attach(tfr)
UN<-tfr
head(UN)
UN<-UN[,-3]
UN<-UN %>% filter(country_code%in%c(unique(dat$`Country code`)))

UN<-as.data.frame(UN)

UN<-UN[rep(seq_len(nrow(UN)), each = 7), ]  
UNfert<-UNfert %>% filter(country_code%in%c(unique(dat$`Country code`)))


#Calculating ASFR for UN data
UNfert[4]<-UNfert[4]*UN[3]/500
UNfert[5]<-UNfert[5]*UN[4]/500
UNfert[6]<-UNfert[6]*UN[5]/500
UNfert[7]<-UNfert[7]*UN[6]/500
UNfert[8]<-UNfert[8]*UN[7]/500
UNfert[9]<-UNfert[9]*UN[8]/500
UNfert[10]<-UNfert[10]*UN[9]/500
UNfert[11]<-UNfert[11]*UN[10]/500
UNfert[12]<-UNfert[12]*UN[11]/500
UNfert[13]<-UNfert[13]*UN[12]/500
UNfert[14]<-UNfert[14]*UN[13]/500
UNfert[15]<-UNfert[15]*UN[14]/500
UNfert[16]<-UNfert[16]*UN[15]/500

UNfert<-gather(UNfert, "Year","ASFR",4:16)


#Calculating the number of women in the UN dataset
data(popF)
attach(popF)
Exposure<-popF%>%filter(country_code%in%c(unique(dat$`Country code`)))


Exposure<-Exposure%>%filter(age%in%c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44","45-49"))
Exposure<-as.data.frame(Exposure)
Exposure<-Exposure[,-c(4,18)]

colnames(Exposure)[4]<-"1955-1960"
colnames(Exposure)[5]<-"1960-1965"
colnames(Exposure)[6]<-"1965-1970"
colnames(Exposure)[7]<-"1970-1975"
colnames(Exposure)[8]<-"1975-1980"
colnames(Exposure)[9]<-"1980-1985"
colnames(Exposure)[10]<-"1985-1990"
colnames(Exposure)[11]<-"1990-1995"
colnames(Exposure)[12]<-"1995-2000"
colnames(Exposure)[13]<-"2000-2005"
colnames(Exposure)[14]<-"2005-2010"
colnames(Exposure)[15]<-"2010-2015"
colnames(Exposure)[16]<-"2015-2020"

Exposure<-gather(data=Exposure,"Year","Sample",4:16)
Exposure<-left_join(Exposure,UNfert,by=c("country_code","age","name","Year"))

Exposure<-Exposure%>%mutate(Births_=Exposure$ASFR*Exposure$Sample)

colnames(Exposure)[1]<-"Country code"
colnames(Exposure)[2]<-"Country"
colnames(Exposure)[3]<-"Age Group"
colnames(Exposure)[5]<-"Sample_"
colnames(Exposure)[6]<-"ASFR_"

Exposure$Region<-vlookup(Exposure$`Country code`,dat, "Region")
Exposure<-Exposure%>%filter(Year%in%c("1970-1975","1975-1980", "1980-1985", "1985-1990", "1990-1995",
                                      "1995-2000", "2000-2005", "2005-2010", "2010-2015","2015-2020"))

Exposure$Country<-vlookup(Exposure$`Country code`,dat, "Country")

#Calculating the births with WIC data

library(wcde)
library(stringr)

wice_dat_<-past_epop
wice_dat_$age<-str_replace_all(wice_dat_$age,'--','-')
wice_dat_<-wice_dat_%>%filter(sex=="Female")%>%filter(age%in%c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))%>%
  filter(country_code%in%c(unique(dat$`Country code`)))%>%filter(education!="Under 15")



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

wice_dat_<-wice_dat_%>%filter(Year%in%c("1970-1975","1975-1980", "1980-1985", "1985-1990", "1990-1995",
                                        "1995-2000", "2000-2005", "2005-2010", "2010-2015","2015-2020"))


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




wice_dat_<-wice_dat_%>%
  group_by(`Age Group`,`Country code`,Year,Education)%>%
  summarise(Pop=(sum(Pop)))


wice_dat_$Country<-vlookup(wice_dat_$`Country code`,dat, "Country")

dat_edu<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Inputs plots (with glm predict)/glm_predict_all.xlsx",sheet="ESASFR_5")

Wic_pop<-merge(dat_edu,wice_dat_, by=c("Country code","Age Group","Year","Education","Country"))

Wic_pop<-Wic_pop[,-6]
#Summing the population
wice_dat_$Pop[wice_dat_$Pop==0]<-1
wice_pop<-Wic_pop%>%
  group_by(`Age Group`,`Country code`,Year,Country)%>%
  summarise(Pop=(sum(Pop)))




new_dat<-left_join(Exposure, wice_pop,by=c("Country code","Age Group","Year","Country"))
new_dat<-new_dat%>%mutate(Births=ASFR_*Pop)

new_ASFR<-new_dat%>%mutate(Asfr=Births/Pop)
new_ASFR<-new_ASFR[,-c(5:7,9,10)]

births<-new_dat[,-c(5,6,7,9)]

ASFR<-Exposure[,-c(5,7)]


UN_tfr<-UN[,-2]
colnames(UN_tfr)[1]<-"Country code"
UN_tfr<-gather(UN_tfr,"Year","TFR_",2:14)
UN_tfr<-UN_tfr%>%filter(!Year %in% c("1950-1955","1955-1960","1960-1965","1965-1970"))
UN_tfr$Region<-vlookup(UN_tfr$`Country code`,dat, "Region")
UN_tfr$Country<-vlookup(UN_tfr$`Country code`,dat, "Country")
UN_tfr<-unique(UN_tfr)


write_xlsx(list("UN_tfr"=UN_tfr,"UN_asfr"=ASFR,"UN_births"=births,"New ASFR"=new_ASFR),
           path ="K:/project/BayesEdu/Fertility/Afua/UN_datasets3.xlsx", col_names=TRUE)


