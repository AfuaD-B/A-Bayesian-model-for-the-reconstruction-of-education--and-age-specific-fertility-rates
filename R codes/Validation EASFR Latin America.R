#In this model the focus is on model validation using cross-validation.
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

dat_laedu2_<-read_excel("./glm_predict_all.xlsx")%>%filter(Region=="Latin America & Caribbean",Country%in%c("Bolivia ", "Brazil", "Colombia", "Ecuador", "Guatemala", "Honduras",
                                                                                                                    "Mexico", "Nicaragua", "Paraguay",  "Peru"))


#Read in UN data
UN_asfr<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")
asfr_UN_<-UN_asfr%>%filter(Region=="Latin America & Caribbean", !Year%in%c("1955-1960", "1960-1965", "1965-1970"))
asfr_UN<-spread(asfr_UN_[,-c(1,5)],`Age Group`,Asfr)

###Bringing the WIC data in to be population of women by level of education

#This is for women with higher education
wice_dat_la_1<-read_excel("./WIC_datasets.xlsx",sheet="WIC_highedu")
wice_dat_la_1<-wice_dat_la_1%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))



#Data for women with no education
wice_dat_la_2<-read_excel("./WIC_datasets.xlsx",sheet="WIC_noedu")
wice_dat_la_2<-wice_dat_la_2%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))



#women with primary education
wice_dat_la_3<-read_excel("./WIC_datasets.xlsx",sheet="WIC_priedu")
wice_dat_la_3<-wice_dat_la_3%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))



#Women with secondary education
wice_dat_la_4<-read_excel("./WIC_datasets.xlsx",sheet="WIC_secedu")
wice_dat_la_4<-wice_dat_la_4%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))


##The main education model code
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
      
      ESASFR_dhs_1[i,j]~dnorm(ESASFR_estimate_1[i,j],tau_esasfr1[i,j])T(0,)
      ESASFR_dhs_2[i,j]~dnorm(ESASFR_estimate_2[i,j],tau_esasfr2[i,j])T(0,)
      ESASFR_dhs_3[i,j]~dnorm(ESASFR_estimate_3[i,j],tau_esasfr3[i,j])T(0,)
      ESASFR_dhs_4[i,j]~dnorm(ESASFR_estimate_4[i,j],tau_esasfr4[i,j])T(0,)
      
      
      #This line is for estimating fertility rates using data with the missing values
      ESASFR_dhs_1_pred[i,j]~dnorm(ESASFR_estimate_1[i,j],tau_esasfr1[i,j])T(0,)
      ESASFR_dhs_2_pred[i,j]~dnorm(ESASFR_estimate_2[i,j],tau_esasfr2[i,j])T(0,)
      ESASFR_dhs_3_pred[i,j]~dnorm(ESASFR_estimate_3[i,j],tau_esasfr3[i,j])T(0,)
      ESASFR_dhs_4_pred[i,j]~dnorm(ESASFR_estimate_4[i,j],tau_esasfr4[i,j])T(0,)
      
      
      
      tau_esasfr1[i,j]~dgamma(s_esasfr1,sesasfr1)
      tau_esasfr2[i,j]~dgamma(s_esasfr1,sesasfr1)
      tau_esasfr3[i,j]~dgamma(s_esasfr1,sesasfr1)
      tau_esasfr4[i,j]~dgamma(s_esasfr1,sesasfr1)

      #Level 1
      #equation 2
      ASFR_estimate[i,j]=ESASFR_dhs_1[i,j]*w_1[i,j] +  ESASFR_dhs_2[i,j]*w_2[i,j] +
      ESASFR_dhs_3[i,j]*w_3[i,j] + ESASFR_dhs_4[i,j]*w_4[i,j]
      
       #equation 3
      ASFR_UN[i,j]~dnorm(ASFR_estimate[i,j],tau_asfr_UN)T(0,) 
      

    }
    
      
    
}
  tau_asfr_UN=1/(var_asfr_UN)
  var_asfr_UN=ASFR_UN_var
  tau_estfr=1/(sigma_estfr^2)
  sigma_estfr~dnorm(1.1,50)

}"

#We remove parts of the data and use the data with missing information as our new input data
#Higher education
esasfr_bayesglm_na1<-dat_laedu2_%>%filter(Education=="Higher Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.05 * n()), replace = FALSE), NA))

esasfr_bayesglm_na1<-esasfr_bayesglm_na1[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



#No education
esasfr_bayesglm_na2<-dat_laedu2_%>%filter(Education=="No Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.05 * n()), replace = FALSE), NA))

esasfr_bayesglm_na2<-esasfr_bayesglm_na2[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)


#Primary education
esasfr_bayesglm_na3<-dat_laedu2_%>%filter(Education=="Primary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.05 * n()), replace = FALSE), NA))

esasfr_bayesglm_na3<-esasfr_bayesglm_na3[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



#Secondary education
esasfr_bayesglm_na4<-dat_laedu2_%>%filter(Education=="Secondary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(), size = ceiling(0.05 * n()), replace = FALSE), NA))

esasfr_bayesglm_na4<-esasfr_bayesglm_na4[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



####We arrange the data in such a way that is compatible with the written model code
dat_laedu_<-dat_laedu2_[order(dat_laedu2_$Country), ]


dat_laedu2<-dat_laedu_[,-c(6,7,8)]%>%distinct()%>%spread(Education,Pred)

dat_laedu2_1<-dat_laedu2[,-c(5:7)]%>%spread(`Age Group`,`Higher Education`)
dat_laedu2_1<-dat_laedu2_1[order(dat_laedu2_1$Country), ]


dat_laedu2_2<-dat_laedu2[,-c(4,6:7)]%>%spread(`Age Group`,`No Education`)
dat_laedu2_2<-dat_laedu2_2[order(dat_laedu2_2$Country), ]

dat_laedu2_3<-dat_laedu2[,-c(4,5,7)]%>%spread(`Age Group`,`Primary Education`)
dat_laedu2_3<-dat_laedu2_3[order(dat_laedu2_3$Country), ]

dat_laedu2_4<-dat_laedu2[,-c(4:6)]%>%spread(`Age Group`,`Secondary Education`)
dat_laedu2_4<-dat_laedu2_4[order(dat_laedu2_4$Country), ]


dat_laedu_se2<-dat_laedu2_[,-c(5,7,8)]%>%distinct()%>%spread(Education,SE)



set.seed(122)
jagsdat_la = as.list(dat_laedu2_1)
jagsdat_la$period=as.matrix(dat_laedu2_1[,2])
jagsdat_la$Age=as.matrix(dat_laedu2_1[1,3:9])
jagsdat_la$pop_wic_1=as.matrix(wice_dat_la_1[,4:10])
jagsdat_la$pop_wic_2=as.matrix(wice_dat_la_2[,4:10])
jagsdat_la$pop_wic_3=as.matrix(wice_dat_la_3[,4:10])
jagsdat_la$pop_wic_4=as.matrix(wice_dat_la_4[,4:10])
jagsdat_la$ESASFR_dhs_1=as.matrix(esasfr_bayesglm_na1[,3:9])
jagsdat_la$ESASFR_dhs_2=as.matrix(esasfr_bayesglm_na2[,3:9])
jagsdat_la$ESASFR_dhs_3=as.matrix(esasfr_bayesglm_na3[,3:9])
jagsdat_la$ESASFR_dhs_4=as.matrix(esasfr_bayesglm_na4[,3:9])

jagsdat_la$ESASFR_estimate_1=as.matrix(dat_laedu2_1[,3:9])
jagsdat_la$ESASFR_estimate_2=as.matrix(dat_laedu2_2[,3:9])
jagsdat_la$ESASFR_estimate_3=as.matrix(dat_laedu2_3[,3:9])
jagsdat_la$ESASFR_estimate_4=as.matrix(dat_laedu2_4[,3:9])


jagsdat_la$ASFR_UN=as.matrix(asfr_UN[,3:9])
jagsdat_la$ASFR_UN_var=(sd(asfr_UN_$Asfr)^2)
jagsdat_la$in_dhs=as.matrix(esasfr_bayesglm_na1[,2])


jagsdat_la$sesasfr1=2*(sd(dat_laedu_se2$`No Education`)^2)


jagsdat_la$s_esasfr1=1/((sd(dat_laedu_se2$`No Education`)))




paramsedu_la =c("ESASFR_estimate_1","ESASFR_estimate_2","ESASFR_estimate_3","ESASFR_estimate_4","ESASFR_dhs_1_pred","ESASFR_dhs_2_pred","ESASFR_dhs_3_pred","ESASFR_dhs_4_pred",
                    "ESTFR_estimate_1","ESTFR_estimate_2","ESTFR_estimate_3","ESTFR_estimate_4","ASFR_estimate","TFR_estimate","tau_asfr_UN",
                    "var_asfr_UN","tau_esasfr1","tau_esasfr2","tau_esasfr3","tau_esasfr4","tau_estfr","sd_estfr","no_edu_diff_hi","no_edu_diff_pri","no_edu_diff_sec")


modedu_la = jags.model(textConnection(mod_stringedu_la), data=jagsdat_la,n.chains = 3)
update(modedu_la, 2e5)

#################################################################################

mod_sim3edu_la = coda.samples(model=modedu_la,variable.names=paramsedu_la,
                                  n.iter=2e5,thin = 200)
#################################################################################

# get summary of posterior sample
#Higher Education
MCsumedupred1_laedu_1<-MCMCsummary(mod_sim3edu_la, params ="ESASFR_dhs_1_pred")
UPCIedupred1_laedu_1<-MCsumedupred1_laedu_1$`97.5%`
LWCIedupred1_laedu_1<-MCsumedupred1_laedu_1$`2.5%`
posterior_meansedupred1_laedu_1=MCsumedupred1_laedu_1$`50%`
posterior_meansedupred1_laedu_1<-as.data.frame(posterior_meansedupred1_laedu_1)
UPCIedupred1_laedu_1<-as.data.frame(UPCIedupred1_laedu_1)
LWCIedupred1_laedu_1<-as.data.frame(LWCIedupred1_laedu_1)
colnames(UPCIedupred1_laedu_1)<-"Upper_CI"
colnames(LWCIedupred1_laedu_1)<-"Lower_CI"
posterior_quantilesedupred1_laedu_1<-cbind(UPCIedupred1_laedu_1,LWCIedupred1_laedu_1)
posterior_quantilesedupred1_laedu_1<-as.data.frame(posterior_quantilesedupred1_laedu_1)
###


#No Education
MCsumedupred1_laedu_2<-MCMCsummary(mod_sim3edu_la, params ="ESASFR_dhs_2_pred")
UPCIedupred1_laedu_2<-MCsumedupred1_laedu_2$`97.5%`
LWCIedupred1_laedu_2<-MCsumedupred1_laedu_2$`2.5%`
posterior_meansedupred1_laedu_2=MCsumedupred1_laedu_2$`50%`
posterior_meansedupred1_laedu_2<-as.data.frame(posterior_meansedupred1_laedu_2)
UPCIedupred1_laedu_2<-as.data.frame(UPCIedupred1_laedu_2)
LWCIedupred1_laedu_2<-as.data.frame(LWCIedupred1_laedu_2)
colnames(UPCIedupred1_laedu_2)<-"Upper_CI"
colnames(LWCIedupred1_laedu_2)<-"Lower_CI"
posterior_quantilesedupred1_laedu_2<-cbind(UPCIedupred1_laedu_2,LWCIedupred1_laedu_2)
posterior_quantilesedupred1_laedu_2<-as.data.frame(posterior_quantilesedupred1_laedu_2)


#Primary Education
MCsumedupred1_laedu_3<-MCMCsummary(mod_sim3edu_la, params ="ESASFR_dhs_3_pred")
UPCIedupred1_laedu_3<-MCsumedupred1_laedu_3$`97.5%`
LWCIedupred1_laedu_3<-MCsumedupred1_laedu_3$`2.5%`
posterior_meansedupred1_laedu_3=MCsumedupred1_laedu_3$`50%`
posterior_meansedupred1_laedu_3<-as.data.frame(posterior_meansedupred1_laedu_3)
UPCIedupred1_laedu_3<-as.data.frame(UPCIedupred1_laedu_3)
LWCIedupred1_laedu_3<-as.data.frame(LWCIedupred1_laedu_3)
colnames(UPCIedupred1_laedu_3)<-"Upper_CI"
colnames(LWCIedupred1_laedu_3)<-"Lower_CI"
posterior_quantilesedupred1_laedu_3<-cbind(UPCIedupred1_laedu_3,LWCIedupred1_laedu_3)
posterior_quantilesedupred1_laedu_3<-as.data.frame(posterior_quantilesedupred1_laedu_3)


#Secondary Education
MCsumedupred1_laedu_4<-MCMCsummary(mod_sim3edu_la, params ="ESASFR_dhs_4_pred")
UPCIedupred1_laedu_4<-MCsumedupred1_laedu_4$`97.5%`
LWCIedupred1_laedu_4<-MCsumedupred1_laedu_4$`2.5%`
posterior_meansedupred1_laedu_4=MCsumedupred1_laedu_4$`50%`
posterior_meansedupred1_laedu_4<-as.data.frame(posterior_meansedupred1_laedu_4)
UPCIedupred1_laedu_4<-as.data.frame(UPCIedupred1_laedu_4)
LWCIedupred1_laedu_4<-as.data.frame(LWCIedupred1_laedu_4)
colnames(UPCIedupred1_laedu_4)<-"Upper_CI"
colnames(LWCIedupred1_laedu_4)<-"Lower_CI"
posterior_quantilesedupred1_laedu_4<-cbind(UPCIedupred1_laedu_4,LWCIedupred1_laedu_4)
posterior_quantilesedupred1_laedu_4<-as.data.frame(posterior_quantilesedupred1_laedu_4)
################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_laedupred_1<-cbind("Mean"=posterior_meansedupred1_laedu_1,"Upper_CI"=posterior_quantilesedupred1_laedu_1[,1],
                                "Lower_CI"=posterior_quantilesedupred1_laedu_1[,2])
colnames(bayesdat_laedupred_1)[1]<-"Median"


bayesdat_laedupred_2<-cbind("Mean"=posterior_meansedupred1_laedu_2,"Upper_CI"=posterior_quantilesedupred1_laedu_2[,1],
                                "Lower_CI"=posterior_quantilesedupred1_laedu_2[,2])
colnames(bayesdat_laedupred_2)[1]<-"Median"



bayesdat_laedupred_3<-cbind("Mean"=posterior_meansedupred1_laedu_3,"Upper_CI"=posterior_quantilesedupred1_laedu_3[,1],
                                "Lower_CI"=posterior_quantilesedupred1_laedu_3[,2])
colnames(bayesdat_laedupred_3)[1]<-"Median"



bayesdat_laedupred_4<-cbind("Mean"=posterior_meansedupred1_laedu_4,"Upper_CI"=posterior_quantilesedupred1_laedu_4[,1],
                                "Lower_CI"=posterior_quantilesedupred1_laedu_4[,2])
colnames(bayesdat_laedupred_4)[1]<-"Median"



e_1<-gather(dat_laedu2_1,"Age Group","ASFR",3:9)
e_1<-e_1%>%mutate(Education="Higher Education")
bayesdatpred_la_1_<-cbind(e_1,bayesdat_laedupred_1)


e_2<-gather(dat_laedu2_2,"Age Group","ASFR",3:9)
e_2<-e_2%>%mutate(Education="No Education")
bayesdatpred_la_2_<-cbind(e_2,bayesdat_laedupred_2)


e_3<-gather(dat_laedu2_3,"Age Group","ASFR",3:9)
e_3<-e_3%>%mutate(Education="Primary Education")
bayesdatpred_la_3_<-cbind(e_3,bayesdat_laedupred_3)



e_4<-gather(dat_laedu2_4,"Age Group","ASFR",3:9)
e_4<-e_4%>%mutate(Education="Secondary Education")
bayesdatpred_la_4_<-cbind(e_4,bayesdat_laedupred_4)

bayesdatpred_la1<-rbind(bayesdatpred_la_1_,bayesdatpred_la_2_)
bayesdatpred_la1<-rbind(bayesdatpred_la1,bayesdatpred_la_3_)
bayesdatpred_la1<-rbind(bayesdatpred_la1,bayesdatpred_la_4_)


bayesdatpred_la_1<-bayesdatpred_la1%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))


############################################################################################################################################################################################################
############################################################################################################################################################################################################


#Higher education
esasfr_bayesglm_na11<-dat_laedu2_%>%filter(Education=="Higher Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.1 * n()), replace = FALSE), NA))

esasfr_bayesglm_na11<-esasfr_bayesglm_na11[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



#No education
esasfr_bayesglm_na12<-dat_laedu2_%>%filter(Education=="No Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.1 * n()), replace = FALSE), NA))

esasfr_bayesglm_na12<-esasfr_bayesglm_na12[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)


#Primary education
esasfr_bayesglm_na13<-dat_laedu2_%>%filter(Education=="Primary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.1 * n()), replace = FALSE), NA))

esasfr_bayesglm_na13<-esasfr_bayesglm_na13[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



#Secondary education
esasfr_bayesglm_na14<-dat_laedu2_%>%filter(Education=="Secondary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(), size = ceiling(0.1 * n()), replace = FALSE), NA))

esasfr_bayesglm_na14<-esasfr_bayesglm_na14[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



set.seed(122)

jagsdat_la$ESASFR_dhs_1=as.matrix(esasfr_bayesglm_na11[,3:9])
jagsdat_la$ESASFR_dhs_2=as.matrix(esasfr_bayesglm_na12[,3:9])
jagsdat_la$ESASFR_dhs_3=as.matrix(esasfr_bayesglm_na13[,3:9])
jagsdat_la$ESASFR_dhs_4=as.matrix(esasfr_bayesglm_na14[,3:9])


modedu_la1 = jags.model(textConnection(mod_stringedu_la), data=jagsdat_la,n.chains = 3)
update(modedu_la1, 2e5)


mod_sim3edu_la1 = coda.samples(model=modedu_la1,variable.names=paramsedu_la,
                                   n.iter=2e5,thin = 200)
#################################################################################

# get summary of posterior sample
#Higher Education
MCsumedupred1_laedu_11<-MCMCsummary(mod_sim3edu_la1, params ="ESASFR_dhs_1_pred")
UPCIedupred1_laedu_11<-MCsumedupred1_laedu_11$`97.5%`
LWCIedupred1_laedu_11<-MCsumedupred1_laedu_11$`2.5%`
posterior_meansedupred1_laedu_11=MCsumedupred1_laedu_11$`50%`
posterior_meansedupred1_laedu_11<-as.data.frame(posterior_meansedupred1_laedu_11)
UPCIedupred1_laedu_11<-as.data.frame(UPCIedupred1_laedu_11)
LWCIedupred1_laedu_11<-as.data.frame(LWCIedupred1_laedu_11)
colnames(UPCIedupred1_laedu_11)<-"Upper_CI"
colnames(LWCIedupred1_laedu_11)<-"Lower_CI"
posterior_quantilesedupred1_laedu_11<-cbind(UPCIedupred1_laedu_11,LWCIedupred1_laedu_11)
posterior_quantilesedupred1_laedu_11<-as.data.frame(posterior_quantilesedupred1_laedu_11)
###


#No Education
MCsumedupred1_laedu_21<-MCMCsummary(mod_sim3edu_la1, params ="ESASFR_dhs_2_pred")
UPCIedupred1_laedu_21<-MCsumedupred1_laedu_21$`97.5%`
LWCIedupred1_laedu_21<-MCsumedupred1_laedu_21$`2.5%`
posterior_meansedupred1_laedu_21=MCsumedupred1_laedu_21$`50%`
posterior_meansedupred1_laedu_21<-as.data.frame(posterior_meansedupred1_laedu_21)
UPCIedupred1_laedu_21<-as.data.frame(UPCIedupred1_laedu_21)
LWCIedupred1_laedu_21<-as.data.frame(LWCIedupred1_laedu_21)
colnames(UPCIedupred1_laedu_21)<-"Upper_CI"
colnames(LWCIedupred1_laedu_21)<-"Lower_CI"
posterior_quantilesedupred1_laedu_21<-cbind(UPCIedupred1_laedu_21,LWCIedupred1_laedu_21)
posterior_quantilesedupred1_laedu_21<-as.data.frame(posterior_quantilesedupred1_laedu_21)


#Primary Education
MCsumedupred1_laedu_31<-MCMCsummary(mod_sim3edu_la1, params ="ESASFR_dhs_3_pred")
UPCIedupred1_laedu_31<-MCsumedupred1_laedu_31$`97.5%`
LWCIedupred1_laedu_31<-MCsumedupred1_laedu_31$`2.5%`
posterior_meansedupred1_laedu_31=MCsumedupred1_laedu_31$`50%`
posterior_meansedupred1_laedu_31<-as.data.frame(posterior_meansedupred1_laedu_31)
UPCIedupred1_laedu_31<-as.data.frame(UPCIedupred1_laedu_31)
LWCIedupred1_laedu_31<-as.data.frame(LWCIedupred1_laedu_31)
colnames(UPCIedupred1_laedu_31)<-"Upper_CI"
colnames(LWCIedupred1_laedu_31)<-"Lower_CI"
posterior_quantilesedupred1_laedu_31<-cbind(UPCIedupred1_laedu_31,LWCIedupred1_laedu_31)
posterior_quantilesedupred1_laedu_31<-as.data.frame(posterior_quantilesedupred1_laedu_31)


#Secondary Education
MCsumedupred1_laedu_41<-MCMCsummary(mod_sim3edu_la1, params ="ESASFR_dhs_4_pred")
UPCIedupred1_laedu_41<-MCsumedupred1_laedu_41$`97.5%`
LWCIedupred1_laedu_41<-MCsumedupred1_laedu_41$`2.5%`
posterior_meansedupred1_laedu_41=MCsumedupred1_laedu_41$`50%`
posterior_meansedupred1_laedu_41<-as.data.frame(posterior_meansedupred1_laedu_41)
UPCIedupred1_laedu_41<-as.data.frame(UPCIedupred1_laedu_41)
LWCIedupred1_laedu_41<-as.data.frame(LWCIedupred1_laedu_41)
colnames(UPCIedupred1_laedu_41)<-"Upper_CI"
colnames(LWCIedupred1_laedu_41)<-"Lower_CI"
posterior_quantilesedupred1_laedu_41<-cbind(UPCIedupred1_laedu_41,LWCIedupred1_laedu_41)
posterior_quantilesedupred1_laedu_41<-as.data.frame(posterior_quantilesedupred1_laedu_41)
################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_laedupred_11<-cbind("Mean"=posterior_meansedupred1_laedu_11,"Upper_CI"=posterior_quantilesedupred1_laedu_11[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_laedu_11[,2])
colnames(bayesdat_laedupred_11)[1]<-"Median"


bayesdat_laedupred_21<-cbind("Mean"=posterior_meansedupred1_laedu_21,"Upper_CI"=posterior_quantilesedupred1_laedu_21[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_laedu_21[,2])
colnames(bayesdat_laedupred_21)[1]<-"Median"



bayesdat_laedupred_31<-cbind("Mean"=posterior_meansedupred1_laedu_31,"Upper_CI"=posterior_quantilesedupred1_laedu_31[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_laedu_31[,2])
colnames(bayesdat_laedupred_31)[1]<-"Median"



bayesdat_laedupred_41<-cbind("Mean"=posterior_meansedupred1_laedu_41,"Upper_CI"=posterior_quantilesedupred1_laedu_41[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_laedu_41[,2])
colnames(bayesdat_laedupred_41)[1]<-"Median"



e_11<-gather(dat_laedu2_1,"Age Group","ASFR",3:9)
e_11<-e_11%>%mutate(Education="Higher Education")
bayesdatpred_la_11_<-cbind(e_11,bayesdat_laedupred_11)


e_21<-gather(dat_laedu2_2,"Age Group","ASFR",3:9)
e_21<-e_21%>%mutate(Education="No Education")
bayesdatpred_la_21_<-cbind(e_21,bayesdat_laedupred_21)


e_31<-gather(dat_laedu2_3,"Age Group","ASFR",3:9)
e_31<-e_31%>%mutate(Education="Primary Education")
bayesdatpred_la_31_<-cbind(e_31,bayesdat_laedupred_31)



e_41<-gather(dat_laedu2_4,"Age Group","ASFR",3:9)
e_41<-e_41%>%mutate(Education="Secondary Education")
bayesdatpred_la_41_<-cbind(e_41,bayesdat_laedupred_41)

bayesdatpred_la11<-rbind(bayesdatpred_la_11_,bayesdatpred_la_21_)
bayesdatpred_la11<-rbind(bayesdatpred_la11,bayesdatpred_la_31_)
bayesdatpred_la11<-rbind(bayesdatpred_la11,bayesdatpred_la_41_)


bayesdatpred_la_2<-bayesdatpred_la11%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))


############################################################################################################################################################################################################
############################################################################################################################################################################################################


#Higher education
esasfr_bayesglm_na21<-dat_laedu2_%>%filter(Education=="Higher Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.15 * n()), replace = FALSE), NA))

esasfr_bayesglm_na21<-esasfr_bayesglm_na21[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



#No education
esasfr_bayesglm_na22<-dat_laedu2_%>%filter(Education=="No Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.15 * n()), replace = FALSE), NA))

esasfr_bayesglm_na22<-esasfr_bayesglm_na22[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)


#Primary education
esasfr_bayesglm_na23<-dat_laedu2_%>%filter(Education=="Primary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.15 * n()), replace = FALSE), NA))

esasfr_bayesglm_na23<-esasfr_bayesglm_na23[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



#Secondary education
esasfr_bayesglm_na24<-dat_laedu2_%>%filter(Education=="Secondary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(), size = ceiling(0.15 * n()), replace = FALSE), NA))

esasfr_bayesglm_na24<-esasfr_bayesglm_na24[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



set.seed(122)

jagsdat_la$ESASFR_dhs_1=as.matrix(esasfr_bayesglm_na21[,3:9])
jagsdat_la$ESASFR_dhs_2=as.matrix(esasfr_bayesglm_na22[,3:9])
jagsdat_la$ESASFR_dhs_3=as.matrix(esasfr_bayesglm_na23[,3:9])
jagsdat_la$ESASFR_dhs_4=as.matrix(esasfr_bayesglm_na24[,3:9])


modedu_la2 = jags.model(textConnection(mod_stringedu_la), data=jagsdat_la,n.chains = 3)
update(modedu_la2, 2e5)


mod_sim3edu_la2 = coda.samples(model=modedu_la2,variable.names=paramsedu_la,
                                   n.iter=2e5,thin = 200)
#################################################################################

# get summary of posterior sample
#Higher Education
MCsumedupred1_laedu_12<-MCMCsummary(mod_sim3edu_la2, params ="ESASFR_dhs_1_pred")
UPCIedupred1_laedu_12<-MCsumedupred1_laedu_12$`97.5%`
LWCIedupred1_laedu_12<-MCsumedupred1_laedu_12$`2.5%`
posterior_meansedupred1_laedu_12=MCsumedupred1_laedu_12$`50%`
posterior_meansedupred1_laedu_12<-as.data.frame(posterior_meansedupred1_laedu_12)
UPCIedupred1_laedu_12<-as.data.frame(UPCIedupred1_laedu_12)
LWCIedupred1_laedu_12<-as.data.frame(LWCIedupred1_laedu_12)
colnames(UPCIedupred1_laedu_12)<-"Upper_CI"
colnames(LWCIedupred1_laedu_12)<-"Lower_CI"
posterior_quantilesedupred1_laedu_12<-cbind(UPCIedupred1_laedu_12,LWCIedupred1_laedu_12)
posterior_quantilesedupred1_laedu_12<-as.data.frame(posterior_quantilesedupred1_laedu_12)
###


#No Education
MCsumedupred1_laedu_22<-MCMCsummary(mod_sim3edu_la2, params ="ESASFR_dhs_2_pred")
UPCIedupred1_laedu_22<-MCsumedupred1_laedu_22$`97.5%`
LWCIedupred1_laedu_22<-MCsumedupred1_laedu_22$`2.5%`
posterior_meansedupred1_laedu_22=MCsumedupred1_laedu_22$`50%`
posterior_meansedupred1_laedu_22<-as.data.frame(posterior_meansedupred1_laedu_22)
UPCIedupred1_laedu_22<-as.data.frame(UPCIedupred1_laedu_22)
LWCIedupred1_laedu_22<-as.data.frame(LWCIedupred1_laedu_22)
colnames(UPCIedupred1_laedu_22)<-"Upper_CI"
colnames(LWCIedupred1_laedu_22)<-"Lower_CI"
posterior_quantilesedupred1_laedu_22<-cbind(UPCIedupred1_laedu_22,LWCIedupred1_laedu_22)
posterior_quantilesedupred1_laedu_22<-as.data.frame(posterior_quantilesedupred1_laedu_22)


#Primary Education
MCsumedupred1_laedu_32<-MCMCsummary(mod_sim3edu_la2, params ="ESASFR_dhs_3_pred")
UPCIedupred1_laedu_32<-MCsumedupred1_laedu_32$`97.5%`
LWCIedupred1_laedu_32<-MCsumedupred1_laedu_32$`2.5%`
posterior_meansedupred1_laedu_32=MCsumedupred1_laedu_32$`50%`
posterior_meansedupred1_laedu_32<-as.data.frame(posterior_meansedupred1_laedu_32)
UPCIedupred1_laedu_32<-as.data.frame(UPCIedupred1_laedu_32)
LWCIedupred1_laedu_32<-as.data.frame(LWCIedupred1_laedu_32)
colnames(UPCIedupred1_laedu_32)<-"Upper_CI"
colnames(LWCIedupred1_laedu_32)<-"Lower_CI"
posterior_quantilesedupred1_laedu_32<-cbind(UPCIedupred1_laedu_32,LWCIedupred1_laedu_32)
posterior_quantilesedupred1_laedu_32<-as.data.frame(posterior_quantilesedupred1_laedu_32)


#Secondary Education
MCsumedupred1_laedu_42<-MCMCsummary(mod_sim3edu_la2, params ="ESASFR_dhs_4_pred")
UPCIedupred1_laedu_42<-MCsumedupred1_laedu_42$`97.5%`
LWCIedupred1_laedu_42<-MCsumedupred1_laedu_42$`2.5%`
posterior_meansedupred1_laedu_42=MCsumedupred1_laedu_42$`50%`
posterior_meansedupred1_laedu_42<-as.data.frame(posterior_meansedupred1_laedu_42)
UPCIedupred1_laedu_42<-as.data.frame(UPCIedupred1_laedu_42)
LWCIedupred1_laedu_42<-as.data.frame(LWCIedupred1_laedu_42)
colnames(UPCIedupred1_laedu_42)<-"Upper_CI"
colnames(LWCIedupred1_laedu_42)<-"Lower_CI"
posterior_quantilesedupred1_laedu_42<-cbind(UPCIedupred1_laedu_42,LWCIedupred1_laedu_42)
posterior_quantilesedupred1_laedu_42<-as.data.frame(posterior_quantilesedupred1_laedu_42)
################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_laedupred_12<-cbind("Mean"=posterior_meansedupred1_laedu_12,"Upper_CI"=posterior_quantilesedupred1_laedu_12[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_laedu_12[,2])
colnames(bayesdat_laedupred_12)[1]<-"Median"


bayesdat_laedupred_22<-cbind("Mean"=posterior_meansedupred1_laedu_22,"Upper_CI"=posterior_quantilesedupred1_laedu_22[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_laedu_22[,2])
colnames(bayesdat_laedupred_22)[1]<-"Median"



bayesdat_laedupred_32<-cbind("Mean"=posterior_meansedupred1_laedu_32,"Upper_CI"=posterior_quantilesedupred1_laedu_32[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_laedu_32[,2])
colnames(bayesdat_laedupred_32)[1]<-"Median"



bayesdat_laedupred_42<-cbind("Mean"=posterior_meansedupred1_laedu_42,"Upper_CI"=posterior_quantilesedupred1_laedu_42[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_laedu_42[,2])
colnames(bayesdat_laedupred_42)[1]<-"Median"



e_12<-gather(dat_laedu2_1,"Age Group","ASFR",3:9)
e_12<-e_12%>%mutate(Education="Higher Education")
bayesdatpred_la_12_<-cbind(e_12,bayesdat_laedupred_12)


e_22<-gather(dat_laedu2_2,"Age Group","ASFR",3:9)
e_22<-e_22%>%mutate(Education="No Education")
bayesdatpred_la_22_<-cbind(e_22,bayesdat_laedupred_22)


e_32<-gather(dat_laedu2_3,"Age Group","ASFR",3:9)
e_32<-e_32%>%mutate(Education="Primary Education")
bayesdatpred_la_32_<-cbind(e_32,bayesdat_laedupred_32)



e_42<-gather(dat_laedu2_4,"Age Group","ASFR",3:9)
e_42<-e_42%>%mutate(Education="Secondary Education")
bayesdatpred_la_42_<-cbind(e_42,bayesdat_laedupred_42)

bayesdatpred_la31<-rbind(bayesdatpred_la_12_,bayesdatpred_la_22_)
bayesdatpred_la31<-rbind(bayesdatpred_la31,bayesdatpred_la_32_)
bayesdatpred_la31<-rbind(bayesdatpred_la31,bayesdatpred_la_42_)


bayesdatpred_la_3<-bayesdatpred_la31%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))




################################################################################################################################################################################################################
################################################################################################################################################################################################################

dat_laedu_<-dat_laedu_%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

Countries<-unique(dat_laedu_$Country)
Country_plots<-list()

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    geom_ribbon(bayesdatpred_la_1%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                               ymax=unlist(Upper_CI),group=Education),alpha=0.1)+
    geom_line(bayesdatpred_la_1%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=Education,colour="5%")) +
    
    
    geom_ribbon(bayesdatpred_la_2%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                               ymax=unlist(Upper_CI),group=Education),alpha=0.1)+
    geom_line(bayesdatpred_la_2%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=Education,colour="10%")) +
    
    
    geom_ribbon(bayesdatpred_la_3%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                               ymax=unlist(Upper_CI),group=Education),alpha=0.1)+
    geom_line(bayesdatpred_la_3%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=Education,colour="15%")) +
    geom_line(dat_laedu_%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=Pred,group=Education,colour="Initial values")) +
    facet_grid(Education~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}   
