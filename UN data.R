library(readxl)
library(tidyverse)
library(zoo)
library(glm2)
library(expss)
library(writexl)


##We read in the input data sets, dat_la contains the ASFR values and TFR_la has the TFR values for 5 year intervals.
dat<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/Cleaned_DHS.xlsx",sheet="ESASFR_5")
dat<-dat%>%select("Country code","Country","Region")
dat<-unique(dat)

library(wpp2019)
data("percentASFR")
attach(percentASFR)
UNfert<-percentASFR[,c(1:3,5:17)]

data(tfr)
attach(tfr)
UN<-tfr
head(UN)
UN<-UN[,-c(3,17)]
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
           path ="K:/project/BayesEdu/Fertility/Afua/UN_datasets.xlsx", col_names=TRUE)



library(readxl)
library(tidyverse)
library(zoo)
library(glm2)
library(expss)
library(writexl)

##################################################################################################################################
#version 2
##We read in the input data sets, dat_la contains the ASFR values and TFR_la has the TFR values for 5 year intervals.
dat<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/Cleaned_DHS.xlsx",sheet="ESASFR_5")
dat<-dat%>%select("Country code","Country","Region")
dat<-unique(dat)

library(wpp2019)
data("percentASFR")
attach(percentASFR)
UNfert2<-percentASFR[,c(1:3,5:17)]

data(tfr)
attach(tfr)
UN2<-tfr
head(UN2)
UN2<-UN2[,-c(3,17)]
UN2<-UN2 %>% filter(country_code%in%c(unique(dat$`Country code`)))

UN2<-as.data.frame(UN2)

UN2<-UN2[rep(seq_len(nrow(UN2)), each = 7), ]  
UNfert2<-UNfert2 %>% filter(country_code%in%c(unique(dat$`Country code`)))


#Calculating ASFR for UN data
UNfert2[4]<-UNfert2[4]*UN2[3]/500
UNfert2[5]<-UNfert2[5]*UN2[4]/500
UNfert2[6]<-UNfert2[6]*UN2[5]/500
UNfert2[7]<-UNfert2[7]*UN2[6]/500
UNfert2[8]<-UNfert2[8]*UN2[7]/500
UNfert2[9]<-UNfert2[9]*UN2[8]/500
UNfert2[10]<-UNfert2[10]*UN2[9]/500
UNfert2[11]<-UNfert2[11]*UN2[10]/500
UNfert2[12]<-UNfert2[12]*UN2[11]/500
UNfert2[13]<-UNfert2[13]*UN2[12]/500
UNfert2[14]<-UNfert2[14]*UN2[13]/500
UNfert2[15]<-UNfert2[15]*UN2[14]/500
UNfert2[16]<-UNfert2[16]*UN2[15]/500

UNfert2<-gather(UNfert2, "Year","ASFR",4:16)


#Calculating the number of women in the UN dataset
data(popF)
attach(popF)
Exposure2<-popF%>%filter(country_code%in%c(unique(dat$`Country code`)))


Exposure2<-Exposure2%>%filter(age%in%c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44","45-49"))
Exposure2<-as.data.frame(Exposure2)
Exposure2<-Exposure2[,-c(4,18)]

colnames(Exposure2)[4]<-"1955-1960"
colnames(Exposure2)[5]<-"1960-1965"
colnames(Exposure2)[6]<-"1965-1970"
colnames(Exposure2)[7]<-"1970-1975"
colnames(Exposure2)[8]<-"1975-1980"
colnames(Exposure2)[9]<-"1980-1985"
colnames(Exposure2)[10]<-"1985-1990"
colnames(Exposure2)[11]<-"1990-1995"
colnames(Exposure2)[12]<-"1995-2000"
colnames(Exposure2)[13]<-"2000-2005"
colnames(Exposure2)[14]<-"2005-2010"
colnames(Exposure2)[15]<-"2010-2015"
colnames(Exposure2)[16]<-"2015-2020"

Exposure2<-gather(data=Exposure2,"Year","Sample",4:16)
Exposure2<-left_join(Exposure2,UNfert2,by=c("country_code","age","name","Year"))

Exposure2<-Exposure2%>%mutate(Births_=Exposure2$ASFR*Exposure2$Sample)

colnames(Exposure2)[1]<-"Country code"
colnames(Exposure2)[2]<-"Country"
colnames(Exposure2)[3]<-"Age Group"
colnames(Exposure2)[5]<-"Sample_"
colnames(Exposure2)[6]<-"ASFR_"

Exposure2$Region<-vlookup(Exposure2$`Country code`,dat, "Region")

Exposure2$Country<-vlookup(Exposure2$`Country code`,dat, "Country")

#Calculating the births with WIC data

library(wcde)
library(stringr)

wice_dat_2<-past_epop
wice_dat_2$age<-str_replace_all(wice_dat_2$age,'--','-')
wice_dat_2<-wice_dat_2%>%filter(sex=="Female")%>%filter(age%in%c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))%>%
  filter(country_code%in%c(unique(dat$`Country code`)))%>%filter(education!="Under 15")


wice_dat_2$year[wice_dat_2$year==1950]="1950-1955"
wice_dat_2$year[wice_dat_2$year==1955]="1955-1960"
wice_dat_2$year[wice_dat_2$year==1960]="1960-1965"
wice_dat_2$year[wice_dat_2$year==1965]="1965-1970"
wice_dat_2$year[wice_dat_2$year==1970]="1970-1975"
wice_dat_2$year[wice_dat_2$year==1975]="1975-1980"
wice_dat_2$year[wice_dat_2$year==1980]="1980-1985"
wice_dat_2$year[wice_dat_2$year==1985]="1985-1990"
wice_dat_2$year[wice_dat_2$year==1990]="1990-1995"
wice_dat_2$year[wice_dat_2$year==1995]="1995-2000"
wice_dat_2$year[wice_dat_2$year==2000]="2000-2005"
wice_dat_2$year[wice_dat_2$year==2005]="2005-2010" 
wice_dat_2$year[wice_dat_2$year==2010]="2010-2015"
wice_dat_2$year[wice_dat_2$year==2015]="2015-2020"

colnames(wice_dat_2)[3]<-"Year"

wice_dat_2<-wice_dat_2%>%filter(!Year=="1950-1955")


wice_dat_2$education[wice_dat_2$education=="Incomplete Primary"]<-"Primary Education"
wice_dat_2$education[wice_dat_2$education=="Primary"]<-"Primary Education"
wice_dat_2$education[wice_dat_2$education=="Lower Secondary"]<-"Secondary Education"
wice_dat_2$education[wice_dat_2$education=="Upper Secondary"]<-"Secondary Education"
wice_dat_2$education[wice_dat_2$education=="Post Secondary"]<-"Higher Education"


wice_dat_2<-wice_dat_2[,-c(1,6)]
colnames(wice_dat_2)[1]<-"Country code"
colnames(wice_dat_2)[3]<-"Age Group"
colnames(wice_dat_2)[4]<-"Education"
colnames(wice_dat_2)[5]<-"Pop"




wice_dat_2<-wice_dat_2%>%
  group_by(`Age Group`,`Country code`,Year,Education)%>%
  summarise(Pop=(sum(Pop)))


wice_dat_2$Country<-vlookup(wice_dat_2$`Country code`,dat, "Country")


#Summing the population
wice_dat_2$Pop[wice_dat_2$Pop==0]<-1
wice_pop2<-wice_dat_2%>%
  group_by(`Age Group`,`Country code`,Year,Country)%>%
  summarise(Pop=(sum(Pop)))




new_dat2<-merge(Exposure2, wice_pop2,by=c("Country code","Age Group","Year","Country"))
new_dat2<-new_dat2%>%mutate(Births=ASFR_*Pop)

new_ASFR2<-new_dat2%>%mutate(Asfr=Births/Pop)
new_ASFR2<-new_ASFR2[,-c(5:7,9,10)]

births2<-new_dat2[,-c(5,6,7,9)]

ASFR2<-Exposure2[,-c(5,7)]


UN_tfr2<-UN2[,-2]
colnames(UN_tfr2)[1]<-"Country code"
UN_tfr2<-gather(UN_tfr2,"Year","TFR_",2:14)
UN_tfr2$Region<-vlookup(UN_tfr2$`Country code`,dat, "Region")
UN_tfr2$Country<-vlookup(UN_tfr2$`Country code`,dat, "Country")
UN_tfr2<-unique(UN_tfr2)


write_xlsx(list("UN_tfr"=UN_tfr2,"UN_asfr"=ASFR2,"UN_births"=births2,"New ASFR"=new_ASFR2),
           path ="K:/project/BayesEdu/Fertility/Afua/UN_datasets2.xlsx", col_names=TRUE)





####

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


