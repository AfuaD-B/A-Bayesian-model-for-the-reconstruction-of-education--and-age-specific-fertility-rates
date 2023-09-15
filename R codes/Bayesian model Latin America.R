#This code is the Bayesian model for Age-Specific Fertility Rates.

library(dplyr)
library(readxl)
library(MCMCvis)
library(tidyverse)
library(coda)
library(zoo)
library(rjags)
library(glm2)
library(ggplot2)
library(ggmcmc)
library(gridExtra)

#Caution; Please set all input data in the working directory before running the code with setwd

#Reading the glm estimated values
dat_laedu2_<-read_excel("./glm_predict_all.xlsx")%>%filter(Region%in%c("Latin America & Caribbean"),
                                                                                            Country%in%c("Bolivia ", "Brazil", "Colombia", "Ecuador", "Guatemala", "Honduras",
                                                                                                         "Mexico", "Nicaragua", "Paraguay",  "Peru"))

#Read in the TFR
cleaned_DHS<-read_excel("./Cleaned_DHS_LA.xlsx",sheet="ESTFR_5")
cleaned_DHS<-cleaned_DHS%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

cleaned_DHS<-cleaned_DHS%>%
  filter(Country%in%c("Bolivia ", "Brazil", "Colombia", "Ecuador", "Guatemala", "Honduras",
                      "Mexico", "Nicaragua", "Paraguay",  "Peru"),!Year %in% c("1955-1960","1960-1965","1965-1970"))



#calculate eta for the precision
eta<-round(min(cleaned_DHS$TFR),1)


#Bring in a data set with Regions of the countries
regions<-read_excel("./Cleaned_DHS_LA.xlsx",sheet="ESASFR_5")
regions<-regions[,c(2,13)]
regions<-unique(regions)%>%filter(Region%in%c("Latin America & Caribbean"))

#Choose the targeted region and years
dat_la<-dat_laedu2_%>%filter(Region=="Latin America & Caribbean", !Year%in%c("1955-1960", "1960-1965", "1965-1970"))
dat_la<-dat_la[,-c(7,8)]

dat_la<-dat_la%>%
  filter(Country%in%c("Bolivia ", "Brazil", "Colombia", "Ecuador", "Guatemala", "Honduras",
                      "Mexico", "Nicaragua", "Paraguay",  "Peru"))

#Read in UN data
UN_asfr<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")
asfr_UN_<-UN_asfr%>%filter(Region=="Latin America & Caribbean", !Year%in%c("1955-1960", "1960-1965", "1965-1970"))

asfr_UN_<-asfr_UN_%>%
  filter(Country%in%c("Bolivia ", "Brazil", "Colombia", "Ecuador", "Guatemala", "Honduras",
                      "Mexico", "Nicaragua", "Paraguay",  "Peru"))

asfr_UN<-spread(asfr_UN_[,-c(1,5)],`Age Group`,Asfr)




###Bringing the WIC data in to be population of women by level of education

wice_dat_la_1<-read_excel("./WIC_datasets.xlsx",sheet="WIC_highedu")
wice_dat_la_1<-wice_dat_la_1%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))


wice_dat_la_2<-read_excel("./WIC_datasets.xlsx",sheet="WIC_noedu")
wice_dat_la_2<-wice_dat_la_2%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))



wice_dat_la_3<-read_excel("./WIC_datasets.xlsx",sheet="WIC_priedu")
wice_dat_la_3<-wice_dat_la_3%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))


wice_dat_la_4<-read_excel("./WIC_datasets.xlsx",sheet="WIC_secedu")
wice_dat_la_4<-wice_dat_la_4%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))


##The education model code
#We make all JAGS calculations by different levels of education
mod_stringedu_la = "model {
#We define our levels as follows

  for (i in 1:length(period)){
  
    #This level is calculated for comparing UN tfrs and is not really used in the model, it is here for checking all parameters
    TFR_estimate[i]=(ASFR_estimate[i,1]+ASFR_estimate[i,2]+ASFR_estimate[i,3]+ASFR_estimate[i,4]+ASFR_estimate[i,5]+ASFR_estimate[i,6]+ASFR_estimate[i,7])%*%5 
    
    
    #Level 3; we divide the code by different levels of education
    ESTFR_estimate_1[i]~dnorm(ESTFR_derived_1[i],tau_estfr)T(0,) #equation 7, higher education
    ESTFR_estimate_2[i]~dnorm(ESTFR_derived_2[i],tau_estfr)T(0,) #equation 7, no education
    ESTFR_estimate_3[i]~dnorm(ESTFR_derived_3[i],tau_estfr)T(0,) #equation 7, primary education
    ESTFR_estimate_4[i]~dnorm(ESTFR_derived_4[i],tau_estfr)T(0,) #equation 7, secondary education
    
    
    #Level 3 equation 8 for the different levels of education
    ESTFR_derived_1[i]=(ESASFR_estimate_1[i,1]+ESASFR_estimate_1[i,2]+ESASFR_estimate_1[i,3]+ESASFR_estimate_1[i,4]+ESASFR_estimate_1[i,5]+ESASFR_estimate_1[i,6]+ESASFR_estimate_1[i,7])%*%5 
    ESTFR_derived_2[i]=(ESASFR_estimate_2[i,1]+ESASFR_estimate_2[i,2]+ESASFR_estimate_2[i,3]+ESASFR_estimate_2[i,4]+ESASFR_estimate_2[i,5]+ESASFR_estimate_2[i,6]+ESASFR_estimate_2[i,7])%*%5
    ESTFR_derived_3[i]=(ESASFR_estimate_3[i,1]+ESASFR_estimate_3[i,2]+ESASFR_estimate_3[i,3]+ESASFR_estimate_3[i,4]+ESASFR_estimate_3[i,5]+ESASFR_estimate_3[i,6]+ESASFR_estimate_3[i,7])%*%5
    ESTFR_derived_4[i]=(ESASFR_estimate_4[i,1]+ESASFR_estimate_4[i,2]+ESASFR_estimate_4[i,3]+ESASFR_estimate_4[i,4]+ESASFR_estimate_4[i,5]+ESASFR_estimate_4[i,6]+ESASFR_estimate_4[i,7])%*%5
    
    
    #We make age specific calculations
    for(j in 1:length(Age)){
      
      #This code is to calcuate the differences between estimates 
      no_edu_diff_hi[i,j] = ESASFR_estimate_2[i,j] - ESASFR_estimate_1[i,j]
      no_edu_diff_pri[i,j] = ESASFR_estimate_2[i,j] - ESASFR_estimate_3[i,j]
      no_edu_diff_sec[i,j] = ESASFR_estimate_2[i,j] - ESASFR_estimate_4[i,j]
      
    
     #We calculate the weights for level 1 here using WIC population data
      w_1[i,j]=pop_wic_1[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_2[i,j]=pop_wic_2[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_3[i,j]=pop_wic_3[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_4[i,j]=pop_wic_4[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      
      
      #Level 2, here we make EASFR calculations 
      #Equation 5
      ESASFR_estimate_1[i,j]~dnorm(ESASFR_dhs_1[i,j],tau_esasfr1[i,j])T(0,)
      ESASFR_estimate_2[i,j]~dnorm(ESASFR_dhs_2[i,j],tau_esasfr2[i,j])T(0,)
      ESASFR_estimate_3[i,j]~dnorm(ESASFR_dhs_3[i,j],tau_esasfr3[i,j])T(0,)
      ESASFR_estimate_4[i,j]~dnorm(ESASFR_dhs_4[i,j],tau_esasfr4[i,j])T(0,)
      
      #equation 6 (variables s_esasfr and seasfr are defined outside of the jags code, see below)
      tau_esasfr1[i,j]~dgamma(s_esasfr1,sesasfr1)
      tau_esasfr2[i,j]~dgamma(s_esasfr1,sesasfr1)
      tau_esasfr3[i,j]~dgamma(s_esasfr1,sesasfr1)
      tau_esasfr4[i,j]~dgamma(s_esasfr1,sesasfr1)
      
      
      
      #Level 1
      #equation 2
      ASFR_UN[i,j]~dnorm(ASFR_estimate[i,j],tau_asfr_UN)T(0,) 
      
      #equation 3
      ASFR_estimate[i,j]=ESASFR_estimate_1[i,j]*w_1[i,j] +  ESASFR_estimate_2[i,j]*w_2[i,j] +
      ESASFR_estimate_3[i,j]*w_3[i,j] + ESASFR_estimate_4[i,j]*w_4[i,j]
      
      
      
  }
      
  }
  #precisions
  #level 1 equation 4
  tau_asfr_UN=1/(sigma_asfr_UN^2)
  sigma_asfr_UN=ASFR_UN_var
  
  #level 3 equation 9
  tau_estfr=1/(sigma_estfr^2)
  #equation 10
  sigma_estfr~dnorm(eta,50)

}"


####We arrange the data in such a way that is compatible with the written model code
dat_laedu_<-dat_la[order(dat_la$Country), ]
dat_laedu2<-dat_laedu_[,-6]%>%distinct()%>%spread(Education,Pred)

dat_laedu2_1<-dat_laedu2[,-c(5:7)]%>%spread(`Age Group`,`Higher Education`)
dat_laedu2_1<-dat_laedu2_1[order(dat_laedu2_1$Country), ]


dat_laedu2_2<-dat_laedu2[,-c(4,6:7)]%>%spread(`Age Group`,`No Education`)
dat_laedu2_2<-dat_laedu2_2[order(dat_laedu2_2$Country), ]

dat_laedu2_3<-dat_laedu2[,-c(4,5,7)]%>%spread(`Age Group`,`Primary Education`)
dat_laedu2_3<-dat_laedu2_3[order(dat_laedu2_3$Country), ]

dat_laedu2_4<-dat_laedu2[,-c(4:6)]%>%spread(`Age Group`,`Secondary Education`)
dat_laedu2_4<-dat_laedu2_4[order(dat_laedu2_4$Country), ]



dat_laedu_se2<-dat_laedu2_[,-c(5,7,8)]%>%distinct()%>%spread(Education,SE)
dat_laedu_se2<-dat_laedu_se2[order(dat_laedu_se2$Country), ]


#We set seed and define our parameters to jags
set.seed(122)
jagsdat_la = as.list(dat_laedu2_1)
jagsdat_la$period=as.matrix(dat_laedu2_1[,2])
jagsdat_la$Age=as.matrix(dat_laedu2_1[1,3:9])
jagsdat_la$pop_wic_1=as.matrix(wice_dat_la_1[,4:10])
jagsdat_la$pop_wic_2=as.matrix(wice_dat_la_2[,4:10])
jagsdat_la$pop_wic_3=as.matrix(wice_dat_la_3[,4:10])
jagsdat_la$pop_wic_4=as.matrix(wice_dat_la_4[,4:10])
jagsdat_la$ESASFR_dhs_1=as.matrix(dat_laedu2_1[,3:9])
jagsdat_la$ESASFR_dhs_2=as.matrix(dat_laedu2_2[,3:9])
jagsdat_la$ESASFR_dhs_3=as.matrix(dat_laedu2_3[,3:9])
jagsdat_la$ESASFR_dhs_4=as.matrix(dat_laedu2_4[,3:9])
jagsdat_la$ASFR_UN=as.matrix(asfr_UN[,3:9])
jagsdat_la$ASFR_UN_var=sd(asfr_UN_$Asfr)^2
jagsdat_la$eta=eta


#this variable represents 2*sigma^asfr2 in equation 6; here sigma is the standard deviation parameter
jagsdat_la$sesasfr1=2*(sd(dat_laedu_se2$`No Education`)^2)

#this variable represents 1/sigma^asfr in equation 6
jagsdat_la$s_esasfr1=1/((sd(dat_laedu_se2$`No Education`)))


#We call in the parameters to follow and need
paramsedu_la =c("ESASFR_estimate_1","ESASFR_estimate_2","ESASFR_estimate_3","ESASFR_estimate_4",
                "ESTFR_estimate_1","ESTFR_estimate_2","ESTFR_estimate_3","ESTFR_estimate_4","ASFR_estimate","TFR_estimate","tau_asfr_UN",
                "sigma_asfr_UN","tau_esasfr1","tau_esasfr2","tau_esasfr3","tau_esasfr4",
                "tau_estfr","sigma_estfr","no_edu_diff_hi","no_edu_diff_pri","no_edu_diff_sec")


#Running the jags model
modedu_la = jags.model(textConnection(mod_stringedu_la), data=jagsdat_la,n.chains = 3)
#updating the model
update(modedu_la, 2e4)


#Sampling of estimates
mod_sim3edu_la = coda.samples(model=modedu_la,variable.names=paramsedu_la,
                              n.iter=2e4,thin = 200)



# get summary of posterior sample
#Higher Education
MCsumedu1_la_1<-MCMCsummary(mod_sim3edu_la, params ="ESASFR_estimate_1")
UPCIedu1_la_1<-MCsumedu1_la_1$`97.5%`
LWCIedu1_la_1<-MCsumedu1_la_1$`2.5%`
posterior_meansedu1_la_1=MCsumedu1_la_1$`50%`
posterior_meansedu1_la_1<-as.data.frame(posterior_meansedu1_la_1)
UPCIedu1_la_1<-as.data.frame(UPCIedu1_la_1)
LWCIedu1_la_1<-as.data.frame(LWCIedu1_la_1)
colnames(UPCIedu1_la_1)<-"Upper_CI"
colnames(LWCIedu1_la_1)<-"Lower_CI"
posterior_quantilesedu1_la_1<-cbind(UPCIedu1_la_1,LWCIedu1_la_1)
posterior_quantilesedu1_la_1<-as.data.frame(posterior_quantilesedu1_la_1)
###


#No Education
MCsumedu1_la_2<-MCMCsummary(mod_sim3edu_la, params ="ESASFR_estimate_2")
UPCIedu1_la_2<-MCsumedu1_la_2$`97.5%`
LWCIedu1_la_2<-MCsumedu1_la_2$`2.5%`
posterior_meansedu1_la_2=MCsumedu1_la_2$`50%`
posterior_meansedu1_la_2<-as.data.frame(posterior_meansedu1_la_2)
UPCIedu1_la_2<-as.data.frame(UPCIedu1_la_2)
LWCIedu1_la_2<-as.data.frame(LWCIedu1_la_2)
colnames(UPCIedu1_la_2)<-"Upper_CI"
colnames(LWCIedu1_la_2)<-"Lower_CI"
posterior_quantilesedu1_la_2<-cbind(UPCIedu1_la_2,LWCIedu1_la_2)
posterior_quantilesedu1_la_2<-as.data.frame(posterior_quantilesedu1_la_2)


#Primary Education
MCsumedu1_la_3<-MCMCsummary(mod_sim3edu_la, params ="ESASFR_estimate_3")
UPCIedu1_la_3<-MCsumedu1_la_3$`97.5%`
LWCIedu1_la_3<-MCsumedu1_la_3$`2.5%`
posterior_meansedu1_la_3=MCsumedu1_la_3$`50%`
posterior_meansedu1_la_3<-as.data.frame(posterior_meansedu1_la_3)
UPCIedu1_la_3<-as.data.frame(UPCIedu1_la_3)
LWCIedu1_la_3<-as.data.frame(LWCIedu1_la_3)
colnames(UPCIedu1_la_3)<-"Upper_CI"
colnames(LWCIedu1_la_3)<-"Lower_CI"
posterior_quantilesedu1_la_3<-cbind(UPCIedu1_la_3,LWCIedu1_la_3)
posterior_quantilesedu1_la_3<-as.data.frame(posterior_quantilesedu1_la_3)


#Secondary Education
MCsumedu1_la_4<-MCMCsummary(mod_sim3edu_la, params ="ESASFR_estimate_4")
UPCIedu1_la_4<-MCsumedu1_la_4$`97.5%`
LWCIedu1_la_4<-MCsumedu1_la_4$`2.5%`
posterior_meansedu1_la_4=MCsumedu1_la_4$`50%`
posterior_meansedu1_la_4<-as.data.frame(posterior_meansedu1_la_4)
UPCIedu1_la_4<-as.data.frame(UPCIedu1_la_4)
LWCIedu1_la_4<-as.data.frame(LWCIedu1_la_4)
colnames(UPCIedu1_la_4)<-"Upper_CI"
colnames(LWCIedu1_la_4)<-"Lower_CI"
posterior_quantilesedu1_la_4<-cbind(UPCIedu1_la_4,LWCIedu1_la_4)
posterior_quantilesedu1_la_4<-as.data.frame(posterior_quantilesedu1_la_4)
################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_la_1<-cbind("Mean"=posterior_meansedu1_la_1,"Upper_CI"=posterior_quantilesedu1_la_1[,1],"Lower_CI"=posterior_quantilesedu1_la_1[,2])
colnames(bayesdat_la_1)[1]<-"Median"


bayesdat_la_2<-cbind("Mean"=posterior_meansedu1_la_2,"Upper_CI"=posterior_quantilesedu1_la_2[,1],"Lower_CI"=posterior_quantilesedu1_la_2[,2])
colnames(bayesdat_la_2)[1]<-"Median"

bayesdat_la_3<-cbind("Mean"=posterior_meansedu1_la_3,"Upper_CI"=posterior_quantilesedu1_la_3[,1],"Lower_CI"=posterior_quantilesedu1_la_3[,2])
colnames(bayesdat_la_3)[1]<-"Median"

bayesdat_la_4<-cbind("Mean"=posterior_meansedu1_la_4,"Upper_CI"=posterior_quantilesedu1_la_4[,1],"Lower_CI"=posterior_quantilesedu1_la_4[,2])
colnames(bayesdat_la_4)[1]<-"Median"



e_1<-gather(dat_laedu2_1,"Age Group","ASFR",3:9)
e_1<-e_1%>%mutate(Education="Higher Education")
bayesdat_la_1_<-cbind(e_1,bayesdat_la_1)


e_2<-gather(dat_laedu2_2,"Age Group","ASFR",3:9)
e_2<-e_2%>%mutate(Education="No Education")
bayesdat_la_2_<-cbind(e_2,bayesdat_la_2)


e_3<-gather(dat_laedu2_3,"Age Group","ASFR",3:9)
e_3<-e_3%>%mutate(Education="Primary Education")
bayesdat_la_3_<-cbind(e_3,bayesdat_la_3)



e_4<-gather(dat_laedu2_4,"Age Group","ASFR",3:9)
e_4<-e_4%>%mutate(Education="Secondary Education")
bayesdat_la_4_<-cbind(e_4,bayesdat_la_4)

bayesdat_la1<-rbind(bayesdat_la_1_,bayesdat_la_2_)
bayesdat_la1<-rbind(bayesdat_la1,bayesdat_la_3_)
bayesdat_la1<-rbind(bayesdat_la1,bayesdat_la_4_)


bayesdat_la_<-bayesdat_la1%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

dat_laedu<-dat_la%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

# looping over unique countries and saving
Countries<-unique(dat_la$Country)
Country_plots<-list()

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(bayesdat_la_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                       ymax=unlist(Upper_CI),group=Education),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_la_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Bayesian Median")) +
    geom_line(dat_laedu %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=Pred,group=Education,colour="Initial Values")) +
    facet_grid(Education~Year)+
    theme_bw()+
    theme(plot.title = element_text(size = 16, hjust=0.5),axis.text.x = element_text(angle=90),
          axis.text = element_text(size = 16),
          axis.title = element_text(size=18),
          legend.title = element_text(size=16),
          strip.text.x = element_text(size=15),
          strip.text.y = element_text(size=15),
          legend.text = element_text(size=14))+
    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ESASFR",colour="Source") 
  
  
  
  print(Country_plots[[Country_]])
  
}



#Plot against DHS cleaned values
Cleaned_DHS_esasfr<-read_excel("./Cleaned_DHS_LA.xlsx",sheet="ESASFR_5")

colnames(Cleaned_DHS_esasfr)[8]<-"Age Group"
Cleaned_DHS_esasfr$`Age Group`[Cleaned_DHS_esasfr$`Age Group`==15]<-"15-19"
Cleaned_DHS_esasfr$`Age Group`[Cleaned_DHS_esasfr$`Age Group`==20]<-"20-24"
Cleaned_DHS_esasfr$`Age Group`[Cleaned_DHS_esasfr$`Age Group`==25]<-"25-29"
Cleaned_DHS_esasfr$`Age Group`[Cleaned_DHS_esasfr$`Age Group`==30]<-"30-34"
Cleaned_DHS_esasfr$`Age Group`[Cleaned_DHS_esasfr$`Age Group`==35]<-"35-39" 
Cleaned_DHS_esasfr$`Age Group`[Cleaned_DHS_esasfr$`Age Group`==40]<-"40-44"
Cleaned_DHS_esasfr$`Age Group`[Cleaned_DHS_esasfr$`Age Group`==45]<-"45-49"


Cleaned_DHS_esasfr<-Cleaned_DHS_esasfr%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

Cleaned_DHS_esasfr<-Cleaned_DHS_esasfr%>%
  filter(Country%in%c("Bolivia ", "Brazil", "Colombia", "Ecuador", "Guatemala", "Honduras",
                      "Mexico", "Nicaragua", "Paraguay",  "Peru"))

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(bayesdat_la_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                       ymax=unlist(Upper_CI),group=Education),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_la_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Bayesian Median")) +
    geom_line(Cleaned_DHS_esasfr%>% filter(Country==Country_, Region=="Latin America & Caribbean",
                                           !Year %in% c("1955-1960","1960-1965","1965-1970")),
              mapping=aes(x=`Age Group`,y=ASFR,group=interaction(`Survey Year`,Education), colour="DHS")) +
    facet_grid(Education~Year)+
    theme_bw()+
    theme(plot.title = element_text(size = 16, hjust=0.5),axis.text.x = element_text(angle=90),
          axis.text = element_text(size = 16),
          axis.title = element_text(size=18),
          legend.title = element_text(size=16),
          strip.text.x = element_text(size=15),
          strip.text.y = element_text(size=15),
          legend.text = element_text(size=14))+
    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ESASFR",colour="Source") 
  
  
  
  print(Country_plots[[Country_]])
  
}


#################################################################################################################################################################
#Check with UN values
MCsum1_UN_1<-MCMCsummary(mod_sim3edu_la, params ="ASFR_estimate")
UPCI1_UN_1<-MCsum1_UN_1$`97.5%`
LWCI1_UN_1<-MCsum1_UN_1$`2.5%`
posterior_means1_UN_1=MCsum1_UN_1$`50%`
posterior_means1_UN_1<-as.data.frame(posterior_means1_UN_1)
UPCI1_UN_1<-as.data.frame(UPCI1_UN_1)
LWCI1_UN_1<-as.data.frame(LWCI1_UN_1)
colnames(UPCI1_UN_1)<-"Upper_CI"
colnames(LWCI1_UN_1)<-"Lower_CI"
posterior_quantiles1_UN_1<-cbind(UPCI1_UN_1,LWCI1_UN_1)
posterior_quantiles1_UN_1<-as.data.frame(posterior_quantiles1_UN_1)

bayesdat_UN_1<-cbind("Mean"=posterior_means1_UN_1,"Upper_CI"=posterior_quantiles1_UN_1[,1],"Lower_CI"=posterior_quantiles1_UN_1[,2])
colnames(bayesdat_UN_1)[1]<-"Median"
bayesdat_UN_1<-cbind(gather(dat_laedu2_1,"Age Group","ASFR",3:9)[,-4],bayesdat_UN_1)

#Read in UN values of ASFR
ASFR_UN<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")
ASFR_UN<-ASFR_UN%>%filter(Region=="Latin America & Caribbean")
ASFR_UN<-ASFR_UN%>%
  filter(Country%in%c("Bolivia ", "Brazil", "Colombia", "Ecuador", "Guatemala", "Honduras",
                      "Mexico", "Nicaragua", "Paraguay",  "Peru"))


for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    geom_ribbon(bayesdat_UN_1%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                       ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_UN_1%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=1,colour="Bayesian Median")) +
    
    geom_line(ASFR_UN%>% filter(Country==Country_), mapping=aes(x=`Age Group`,y=Asfr,group=1,
                                                                colour ="UN")) +
    facet_wrap(~Year)+
    theme_bw() +
    theme(plot.title = element_text(size = 16, hjust=0.5),axis.text.x = element_text(angle=90),
          axis.text = element_text(size = 16),
          axis.title = element_text(size=18),
          legend.title = element_text(size=16),
          strip.text.x = element_text(size=15),
          strip.text.y = element_text(size=15),
          legend.text = element_text(size=14))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}

######################################################################################################################################

#Check with TFR values
MCsum1_TFR_1<-MCMCsummary(mod_sim3edu_la, params ="TFR_estimate")
UPCI1_TFR_1<-MCsum1_TFR_1$`97.5%`
LWCI1_TFR_1<-MCsum1_TFR_1$`2.5%`
posterior_means1_TFR_1=MCsum1_TFR_1$`50%`
posterior_means1_TFR_1<-as.data.frame(posterior_means1_TFR_1)
UPCI1_TFR_1<-as.data.frame(UPCI1_TFR_1)
LWCI1_TFR_1<-as.data.frame(LWCI1_TFR_1)
colnames(UPCI1_TFR_1)<-"Upper_CI"
colnames(LWCI1_TFR_1)<-"Lower_CI"
posterior_quantiles1_TFR_1<-cbind(UPCI1_TFR_1,LWCI1_TFR_1)
posterior_quantiles1_TFR_1<-as.data.frame(posterior_quantiles1_TFR_1)

bayesdat_TFR_1<-cbind("Mean"=posterior_means1_TFR_1,"Upper_CI"=posterior_quantiles1_TFR_1[,1],"Lower_CI"=posterior_quantiles1_TFR_1[,2])
colnames(bayesdat_TFR_1)[1]<-"Median"

#Read in TFR values of TFR
TFR_UN<-read_excel("./UN_datasets3.xlsx",sheet="UN_tfr")
TFR_UN<-TFR_UN%>%filter(Region=="Latin America & Caribbean")

TFR_UN<-TFR_UN%>%
  filter(Country%in%c("Bolivia ", "Brazil", "Colombia", "Ecuador", "Guatemala", "Honduras",
                      "Mexico", "Nicaragua", "Paraguay",  "Peru"))

bayesdat_TFR_1<-cbind(unique(bayesdat_UN_1[,c(1,2)]),bayesdat_TFR_1)


ggplot()+  
  geom_ribbon(bayesdat_TFR_1,mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                         ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
  geom_line(bayesdat_TFR_1,mapping=aes(x=Year ,y=unlist(Median),group=1,colour="Bayesian Median")) +
  
  geom_line(TFR_UN, mapping=aes(x=Year,y=TFR_,group=1,
                                colour ="TFR")) +
  facet_wrap(~Country)+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90))+
  ggtitle(paste0("Total Fertility Rate ", Country_)) + 
  labs(x="Year",y="TFR", colour="Source") +
  theme(plot.title = element_text(size = 10, hjust=0.5))



#############################################################################################################################################################
####################################################################################################################################

# get summary of posterior sample for TFR

#Higher Education
MCsumedu2_la_1<-MCMCsummary(mod_sim3edu_la, params ="ESTFR_estimate_1")
UPCIedu2_la_1<-MCsumedu2_la_1$`97.5%`
LWCIedu2_la_1<-MCsumedu2_la_1$`2.5%`
posterior_meansedu2_la_1=MCsumedu2_la_1$`50%`
posterior_meansedu2_la_1<-as.data.frame(posterior_meansedu2_la_1)
UPCIedu2_la_1<-as.data.frame(UPCIedu2_la_1)
LWCIedu2_la_1<-as.data.frame(LWCIedu2_la_1)
colnames(UPCIedu2_la_1)<-"Upper_CI"
colnames(LWCIedu2_la_1)<-"Lower_CI"
posterior_quantilesedu2_la_1<-cbind(UPCIedu2_la_1,LWCIedu2_la_1)
posterior_quantilesedu2_la_1<-as.data.frame(posterior_quantilesedu2_la_1)


###
#No Education
MCsumedu2_la_2<-MCMCsummary(mod_sim3edu_la, params ="ESTFR_estimate_2")
UPCIedu2_la_2<-MCsumedu2_la_2$`97.5%`
LWCIedu2_la_2<-MCsumedu2_la_2$`2.5%`
posterior_meansedu2_la_2=MCsumedu2_la_2$`50%`
posterior_meansedu2_la_2<-as.data.frame(posterior_meansedu2_la_2)
UPCIedu2_la_2<-as.data.frame(UPCIedu2_la_2)
LWCIedu2_la_2<-as.data.frame(LWCIedu2_la_2)
colnames(UPCIedu2_la_2)<-"Upper_CI"
colnames(LWCIedu2_la_2)<-"Lower_CI"
posterior_quantilesedu2_la_2<-cbind(UPCIedu2_la_2,LWCIedu2_la_2)
posterior_quantilesedu2_la_2<-as.data.frame(posterior_quantilesedu2_la_2)


#Primary Education
MCsumedu2_la_3<-MCMCsummary(mod_sim3edu_la, params ="ESTFR_estimate_3")
UPCIedu2_la_3<-MCsumedu2_la_3$`97.5%`
LWCIedu2_la_3<-MCsumedu2_la_3$`2.5%`
posterior_meansedu2_la_3=MCsumedu2_la_3$`50%`
posterior_meansedu2_la_3<-as.data.frame(posterior_meansedu2_la_3)
UPCIedu2_la_3<-as.data.frame(UPCIedu2_la_3)
LWCIedu2_la_3<-as.data.frame(LWCIedu2_la_3)
colnames(UPCIedu2_la_3)<-"Upper_CI"
colnames(LWCIedu2_la_3)<-"Lower_CI"
posterior_quantilesedu2_la_3<-cbind(UPCIedu2_la_3,LWCIedu2_la_3)
posterior_quantilesedu2_la_3<-as.data.frame(posterior_quantilesedu2_la_3)


#Secondary Education
MCsumedu2_la_4<-MCMCsummary(mod_sim3edu_la, params ="ESTFR_estimate_4")
UPCIedu2_la_4<-MCsumedu2_la_4$`97.5%`
LWCIedu2_la_4<-MCsumedu2_la_4$`2.5%`
posterior_meansedu2_la_4=MCsumedu2_la_4$`50%`
posterior_meansedu2_la_4<-as.data.frame(posterior_meansedu2_la_4)
UPCIedu2_la_4<-as.data.frame(UPCIedu2_la_4)
LWCIedu2_la_4<-as.data.frame(LWCIedu2_la_4)
colnames(UPCIedu2_la_4)<-"Upper_CI"
colnames(LWCIedu2_la_4)<-"Lower_CI"
posterior_quantilesedu2_la_4<-cbind(UPCIedu2_la_4,LWCIedu2_la_4)
posterior_quantilesedu2_la_4<-as.data.frame(posterior_quantilesedu2_la_4)

#############################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_la_12<-cbind("Mean"=posterior_meansedu2_la_1,"Upper_CI"=posterior_quantilesedu2_la_1[,1],"Lower_CI"=posterior_quantilesedu2_la_1[,2])
colnames(bayesdat_la_12)[1]<-"Median"


bayesdat_la_22<-cbind("Mean"=posterior_meansedu2_la_2,"Upper_CI"=posterior_quantilesedu2_la_2[,1],"Lower_CI"=posterior_quantilesedu2_la_2[,2])
colnames(bayesdat_la_22)[1]<-"Median"

bayesdat_la_32<-cbind("Mean"=posterior_meansedu2_la_3,"Upper_CI"=posterior_quantilesedu2_la_3[,1],"Lower_CI"=posterior_quantilesedu2_la_3[,2])
colnames(bayesdat_la_32)[1]<-"Median"

bayesdat_la_42<-cbind("Mean"=posterior_meansedu2_la_4,"Upper_CI"=posterior_quantilesedu2_la_4[,1],"Lower_CI"=posterior_quantilesedu2_la_4[,2])
colnames(bayesdat_la_42)[1]<-"Median"




e_12<-dat_laedu2_1[,c(1,2)]%>%mutate(Education="Higher Education")
bayesdat_la_12_<-cbind(e_12,bayesdat_la_12)

e_22<-dat_laedu2_1[,c(1,2)]%>%mutate(Education="No Education")
bayesdat_la_22_<-cbind(e_22,bayesdat_la_22)


e_32<-dat_laedu2_1[,c(1,2)]%>%mutate(Education="Primary Education")
bayesdat_la_32_<-cbind(e_32,bayesdat_la_32)

e_42<-dat_laedu2_1[,c(1,2)]%>%mutate(Education="Secondary Education")
bayesdat_la_42_<-cbind(e_42,bayesdat_la_42)


bayesdat_la2<-rbind(bayesdat_la_12_,bayesdat_la_22_)
bayesdat_la2<-rbind(bayesdat_la2,bayesdat_la_32_)
bayesdat_la2<-rbind(bayesdat_la2,bayesdat_la_42_)

#Recall Cleaned DHS data already read in at the beginning

# looping over unique countries
bayesdat_la2<-bayesdat_la2%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))


for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+ 
    geom_ribbon(bayesdat_la2%>% filter(Country==Country_),mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                      ymax=unlist(Upper_CI),group=Education),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_la2%>% filter(Country==Country_),
              mapping=aes(x=Year ,y=unlist(Median),group=Education, colour="Bayesian Median")) +
    geom_line(cleaned_DHS%>% filter(Country==Country_, Region=="Latin America & Caribbean",
                                    !Year %in% c("1955-1960","1960-1965","1965-1970")),
              mapping=aes(x=Year,y=TFR,group=interaction(`Survey Year`,Education), colour="DHS")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    facet_wrap(Education~.)+ 
    theme_bw()+ 
    theme(plot.title = element_text(size = 10, hjust=0.5),axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Education Specific Total Fertility Rates ", Country_)) + 
    labs(x="Year",y="ESTFR",colour="Estimates")
  
  
  
  print(Country_plots[[Country_]])
  
}



######################################################################################################################################################


#################################################################################
#Collect and plot differences estimates

# get summary of posterior sample
#Higher Education no education
MCdiffedu1_laedu_1<-MCMCsummary(mod_sim3edu_la, params ="no_edu_diff_hi")
UPCIdiffedu1_laedu_1<-MCdiffedu1_laedu_1$`97.5%`
LWCIdiffedu1_laedu_1<-MCdiffedu1_laedu_1$`2.5%`
posterior_meansdiffedu1_laedu_1=MCdiffedu1_laedu_1$`50%`
posterior_meansdiffedu1_laedu_1<-as.data.frame(posterior_meansdiffedu1_laedu_1)
UPCIdiffedu1_laedu_1<-as.data.frame(UPCIdiffedu1_laedu_1)
LWCIdiffedu1_laedu_1<-as.data.frame(LWCIdiffedu1_laedu_1)
colnames(UPCIdiffedu1_laedu_1)<-"Upper_CI"
colnames(LWCIdiffedu1_laedu_1)<-"Lower_CI"
posterior_quantilesedu1_laedu_1<-cbind(UPCIdiffedu1_laedu_1,LWCIdiffedu1_laedu_1)
posterior_quantilesedu1_laedu_1<-as.data.frame(posterior_quantilesedu1_laedu_1)
###


#No Education primary education diff
MCdiffedu1_laedu_2<-MCMCsummary(mod_sim3edu_la, params ="no_edu_diff_pri")
UPCIdiffedu1_laedu_2<-MCdiffedu1_laedu_2$`97.5%`
LWCIdiffedu1_laedu_2<-MCdiffedu1_laedu_2$`2.5%`
posterior_meansdiffedu1_laedu_2=MCdiffedu1_laedu_2$`50%`
posterior_meansdiffedu1_laedu_2<-as.data.frame(posterior_meansdiffedu1_laedu_2)
UPCIdiffedu1_laedu_2<-as.data.frame(UPCIdiffedu1_laedu_2)
LWCIdiffedu1_laedu_2<-as.data.frame(LWCIdiffedu1_laedu_2)
colnames(UPCIdiffedu1_laedu_2)<-"Upper_CI"
colnames(LWCIdiffedu1_laedu_2)<-"Lower_CI"
posterior_quantilesedu1_laedu_2<-cbind(UPCIdiffedu1_laedu_2,LWCIdiffedu1_laedu_2)
posterior_quantilesedu1_laedu_2<-as.data.frame(posterior_quantilesedu1_laedu_2)


#Secondary education no education diff
MCdiffedu1_laedu_3<-MCMCsummary(mod_sim3edu_la, params ="no_edu_diff_sec")
UPCIdiffedu1_laedu_3<-MCdiffedu1_laedu_3$`97.5%`
LWCIdiffedu1_laedu_3<-MCdiffedu1_laedu_3$`2.5%`
posterior_meansdiffedu1_laedu_3=MCdiffedu1_laedu_3$`50%`
posterior_meansdiffedu1_laedu_3<-as.data.frame(posterior_meansdiffedu1_laedu_3)
UPCIdiffedu1_laedu_3<-as.data.frame(UPCIdiffedu1_laedu_3)
LWCIdiffedu1_laedu_3<-as.data.frame(LWCIdiffedu1_laedu_3)
colnames(UPCIdiffedu1_laedu_3)<-"Upper_CI"
colnames(LWCIdiffedu1_laedu_3)<-"Lower_CI"
posterior_quantilesedu1_laedu_3<-cbind(UPCIdiffedu1_laedu_3,LWCIdiffedu1_laedu_3)
posterior_quantilesedu1_laedu_3<-as.data.frame(posterior_quantilesedu1_laedu_3)


################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_la_diff_edu_1<-cbind("Mean"=posterior_meansdiffedu1_laedu_1,"Upper_CI"=posterior_quantilesedu1_laedu_1[,1],
                              "Lower_CI"=posterior_quantilesedu1_laedu_1[,2])
colnames(bayesdat_la_diff_edu_1)[1]<-"Median"


bayesdat_la_diff_edu_2<-cbind("Mean"=posterior_meansdiffedu1_laedu_2,"Upper_CI"=posterior_quantilesedu1_laedu_2[,1],
                              "Lower_CI"=posterior_quantilesedu1_laedu_2[,2])
colnames(bayesdat_la_diff_edu_2)[1]<-"Median"



bayesdat_la_diff_edu_3<-cbind("Mean"=posterior_meansdiffedu1_laedu_3,"Upper_CI"=posterior_quantilesedu1_laedu_3[,1],
                              "Lower_CI"=posterior_quantilesedu1_laedu_3[,2])
colnames(bayesdat_la_diff_edu_3)[1]<-"Median"



diff_e_1<-gather(dat_laedu2_1,"Age Group","ASFR",3:9)

bayesdat_diff_la_1_<-cbind(diff_e_1[,1:3],bayesdat_la_diff_edu_1)
bayesdat_diff_la_1_$Difference<-"No Education - Higher Education"

bayesdat_diff_la_2_<-cbind(diff_e_1[,1:3],bayesdat_la_diff_edu_2)
bayesdat_diff_la_2_$Difference<-"No Education - Primary Education"

bayesdat_diff_la_3_<-cbind(diff_e_1[,1:3],bayesdat_la_diff_edu_3)
bayesdat_diff_la_3_$Difference<-"No Education - Secondary Education"


bayesdat_diff_la1<-rbind(bayesdat_diff_la_1_,bayesdat_diff_la_2_)
bayesdat_diff_la1<-rbind(bayesdat_diff_la1,bayesdat_diff_la_3_)

bayesdat_diff_la1<-bayesdat_diff_la1%>%mutate(Difference=factor(Difference, levels = c("No Education - Primary Education","No Education - Secondary Education","No Education - Higher Education")))
# looping over unique countries
Countries<-unique(dat_laedu$Country)
Country_plots<-list()

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(bayesdat_diff_la1%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                           ymax=unlist(Upper_CI),group=Difference),alpha=0.1)+
    geom_line(bayesdat_diff_la1%>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Difference,colour=Difference)) +
    facet_grid(~Year)+
    theme_bw()+
    theme(plot.title = element_text(size = 16, hjust=0.5),axis.text.x = element_text(angle=90),
          axis.text = element_text(size = 16),
          legend.title = element_text(size=16))+
    labs(x="Age Group",y="ASFR",colour="Difference") +
    ggtitle(paste0("Education Specific Differences in Age Specific Fertility Rate ", Country_)) 
  
  
  
  print(Country_plots[[Country_]])
  
}




####################################################################################################################################
#collect easfrs priors

#Higher Education
MCprioredu1_laedu_1<-MCMCsummary(mod_sim3edu_la, params ="tau_esasfr1")
UPCIprioredu1_laedu_1<-MCprioredu1_laedu_1$`97.5%`
LWCIprioredu1_laedu_1<-MCprioredu1_laedu_1$`2.5%`
posterior_meansprioredu1_laedu_1=MCprioredu1_laedu_1$`50%`
posterior_meansprioredu1_laedu_1<-as.data.frame(posterior_meansprioredu1_laedu_1)
UPCIprioredu1_laedu_1<-as.data.frame(UPCIprioredu1_laedu_1)
LWCIprioredu1_laedu_1<-as.data.frame(LWCIprioredu1_laedu_1)
colnames(UPCIprioredu1_laedu_1)<-"Upper_CI"
colnames(LWCIprioredu1_laedu_1)<-"Lower_CI"
posterior_quantilesedu1_laedu_1<-cbind(UPCIprioredu1_laedu_1,LWCIprioredu1_laedu_1)
posterior_quantilesedu1_laedu_1<-as.data.frame(posterior_quantilesedu1_laedu_1)
###


# no education
MCprioredu1_laedu_2<-MCMCsummary(mod_sim3edu_la, params ="tau_esasfr2")
UPCIprioredu1_laedu_2<-MCprioredu1_laedu_2$`97.5%`
LWCIprioredu1_laedu_2<-MCprioredu1_laedu_2$`2.5%`
posterior_meansprioredu1_laedu_2=MCprioredu1_laedu_2$`50%`
posterior_meansprioredu1_laedu_2<-as.data.frame(posterior_meansprioredu1_laedu_2)
UPCIprioredu1_laedu_2<-as.data.frame(UPCIprioredu1_laedu_2)
LWCIprioredu1_laedu_2<-as.data.frame(LWCIprioredu1_laedu_2)
colnames(UPCIprioredu1_laedu_2)<-"Upper_CI"
colnames(LWCIprioredu1_laedu_2)<-"Lower_CI"
posterior_quantilesedu1_laedu_2<-cbind(UPCIprioredu1_laedu_2,LWCIprioredu1_laedu_2)
posterior_quantilesedu1_laedu_2<-as.data.frame(posterior_quantilesedu1_laedu_2)


#primary education
MCprioredu1_laedu_3<-MCMCsummary(mod_sim3edu_la, params ="tau_esasfr3")
UPCIprioredu1_laedu_3<-MCprioredu1_laedu_3$`97.5%`
LWCIprioredu1_laedu_3<-MCprioredu1_laedu_3$`2.5%`
posterior_meansprioredu1_laedu_3=MCprioredu1_laedu_3$`50%`
posterior_meansprioredu1_laedu_3<-as.data.frame(posterior_meansprioredu1_laedu_3)
UPCIprioredu1_laedu_3<-as.data.frame(UPCIprioredu1_laedu_3)
LWCIprioredu1_laedu_3<-as.data.frame(LWCIprioredu1_laedu_3)
colnames(UPCIprioredu1_laedu_3)<-"Upper_CI"
colnames(LWCIprioredu1_laedu_3)<-"Lower_CI"
posterior_quantilesedu1_laedu_3<-cbind(UPCIprioredu1_laedu_3,LWCIprioredu1_laedu_3)
posterior_quantilesedu1_laedu_3<-as.data.frame(posterior_quantilesedu1_laedu_3)

#Secondary education
MCprioredu1_laedu_4<-MCMCsummary(mod_sim3edu_la, params ="tau_esasfr4")
UPCIprioredu1_laedu_4<-MCprioredu1_laedu_4$`97.5%`
LWCIprioredu1_laedu_4<-MCprioredu1_laedu_4$`2.5%`
posterior_meansprioredu1_laedu_4=MCprioredu1_laedu_4$`50%`
posterior_meansprioredu1_laedu_4<-as.data.frame(posterior_meansprioredu1_laedu_4)
UPCIprioredu1_laedu_4<-as.data.frame(UPCIprioredu1_laedu_4)
LWCIprioredu1_laedu_4<-as.data.frame(LWCIprioredu1_laedu_4)
colnames(UPCIprioredu1_laedu_4)<-"Upper_CI"
colnames(LWCIprioredu1_laedu_4)<-"Lower_CI"
posterior_quantilesedu1_laedu_4<-cbind(UPCIprioredu1_laedu_4,LWCIprioredu1_laedu_4)
posterior_quantilesedu1_laedu_4<-as.data.frame(posterior_quantilesedu1_laedu_4)

################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_la_prior_edu_1<-cbind("Mean"=posterior_meansprioredu1_laedu_1,"Upper_CI"=posterior_quantilesedu1_laedu_1[,1],
                              "Lower_CI"=posterior_quantilesedu1_laedu_1[,2])
colnames(bayesdat_la_prior_edu_1)[1]<-"Median"


bayesdat_la_prior_edu_2<-cbind("Mean"=posterior_meansprioredu1_laedu_2,"Upper_CI"=posterior_quantilesedu1_laedu_2[,1],
                              "Lower_CI"=posterior_quantilesedu1_laedu_2[,2])
colnames(bayesdat_la_prior_edu_2)[1]<-"Median"



bayesdat_la_prior_edu_3<-cbind("Mean"=posterior_meansprioredu1_laedu_3,"Upper_CI"=posterior_quantilesedu1_laedu_3[,1],
                              "Lower_CI"=posterior_quantilesedu1_laedu_3[,2])
colnames(bayesdat_la_prior_edu_3)[1]<-"Median"



bayesdat_la_prior_edu_4<-cbind("Mean"=posterior_meansprioredu1_laedu_4,"Upper_CI"=posterior_quantilesedu1_laedu_4[,1],
                               "Lower_CI"=posterior_quantilesedu1_laedu_4[,2])
colnames(bayesdat_la_prior_edu_4)[1]<-"Median"


prior_e_12<-dat_laedu2_1[,c(1,2)]%>%mutate(Education="Higher Education")
bayesdat_la_prior_edu_12_<-cbind(prior_e_12,bayesdat_la_prior_edu_1)

prior_e_22<-dat_laedu2_1[,c(1,2)]%>%mutate(Education="No Education")
bayesdat_la_prior_edu_22_<-cbind(prior_e_22,bayesdat_la_prior_edu_2)

prior_e_32<-dat_laedu2_1[,c(1,2)]%>%mutate(Education="Primary Education")
bayesdat_la_prior_edu_32_<-cbind(prior_e_32,bayesdat_la_prior_edu_3)


prior_e_42<-dat_laedu2_1[,c(1,2)]%>%mutate(Education="Secondary Education")
bayesdat_la_prior_edu_42_<-cbind(prior_e_42,bayesdat_la_prior_edu_4)


bayesdat_la_prior_edu2<-rbind(bayesdat_la_prior_edu_12_,bayesdat_la_prior_edu_22_)
bayesdat_la_prior_edu2<-rbind(bayesdat_la_prior_edu2,bayesdat_la_prior_edu_32_)
bayesdat_la_prior_edu2<-rbind(bayesdat_la_prior_edu2,bayesdat_la_prior_edu_42_)


####################################################################################################################################
#Saving the results as an excel sheet
library(writexl)

bayesdat_laesasfr<-bayesdat_la_%>%select("Country","Age Group","Education","Year","Upper_CI","Lower_CI","Median")
bayesdat_laestfr<-bayesdat_la2%>%select("Country","Year","Education","Upper_CI","Lower_CI","Median")
bayesdat_laasfr<-bayesdat_UN_1%>%select("Country","Year","Age Group","Upper_CI","Lower_CI","Median")
bayesdat_latfr<-bayesdat_TFR_1%>%select("Country","Year","Upper_CI","Lower_CI","Median")



write_xlsx(list("BESASFR"=bayesdat_laesasfr,"BESTFR"=bayesdat_laestfr,"ASFR"=bayesdat_laasfr,"TFR"=bayesdat_latfr),
           path ="./BESFR_estimates Latin America.xlsx", col_names=TRUE)


write_xlsx(bayesdat_la_prior_edu2,path ="./tau_sd_init_priors.xlsx", col_names=TRUE)

