#In this model the focus is on corss-validating the Education specific rates for African countries.
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


#Please set working directory as the same in input data before running the code

#We read in our data
dat_africaedu2_<-read_excel("./glm_predict_all.xlsx")%>%filter(Region%in%c("Africa","North Africa"))


TFR_africaedu<-read.csv("./cc_y_edu_all_paper_models.csv")%>%filter(Model.name=="UN-fully consistent",Chain=="1")
TFR_africaedu<-as.data.frame(TFR_africaedu)
Encoding(TFR_africaedu$Country) <- "UTF-8"

for_join<-unique(dat_africaedu2_[,c(1:3)])
TFR_africaedu<-TFR_africaedu%>%filter(Country%in%c(unique(dat_africaedu2_$Country)),Year%in%c(unique(dat_africaedu2_$Year)))
TFR_africaedu2_<-unique(full_join(for_join,TFR_africaedu,by=c("Country","Year","Education")))

#ESFTR by level of education
estfr_d1_2<-TFR_africaedu2_%>%filter(Education=="No Education")
estfr_d1_2<-estfr_d1_2[order(estfr_d1_2$Country), ]

estfr_d1_3<-TFR_africaedu2_%>%filter(Education=="Primary Education")
estfr_d1_3<-estfr_d1_3[order(estfr_d1_3$Country), ]


estfr_d1_4<-TFR_africaedu2_%>%filter(Education=="Secondary Education")
estfr_d1_4<-estfr_d1_4[order(estfr_d1_4$Country), ]

estfr_d1_1<-TFR_africaedu2_%>%filter(Education=="Higher Education")
estfr_d1_1<-estfr_d1_1[order(estfr_d1_1$Country), ]


#Read in UN data
UN_asfr<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")
asfr_UN_<-UN_asfr%>%filter(Region%in%c("Africa","North Africa"), !Year%in%c("1955-1960", "1960-1965", "1965-1970"))
asfr_UN<-spread(asfr_UN_[,-c(1,5)],`Age Group`,Asfr)

###Bringing the WIC data in to be population of women by level of education

#This is for women with higher education
wice_dat_africa_1<-read_excel("./WIC_datasets.xlsx",sheet="WIC_highedu")
wice_dat_africa_1<-wice_dat_africa_1%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))



#Data for women with no education
wice_dat_africa_2<-read_excel("./WIC_datasets.xlsx",sheet="WIC_noedu")
wice_dat_africa_2<-wice_dat_africa_2%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))



#women with primary education
wice_dat_africa_3<-read_excel("./WIC_datasets.xlsx",sheet="WIC_priedu")
wice_dat_africa_3<-wice_dat_africa_3%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))



#Women with secondary education
wice_dat_africa_4<-read_excel("./WIC_datasets.xlsx",sheet="WIC_secedu")
wice_dat_africa_4<-wice_dat_africa_4%>%filter(`Country code`%in%unique(asfr_UN_$`Country code`))


##The main education model code
mod_stringedu_africa = "model {

#We define the following levels

  for (i in 1:length(period)){
  
    #This level is calculated for comparing UN tfrs and is not really used in the model, it is here for checking all parameters as with the latin American code
    TFR_estimate[i]=(ASFR_estimate[i,1]+ASFR_estimate[i,2]+ASFR_estimate[i,3]+ASFR_estimate[i,4]+ASFR_estimate[i,5]+ASFR_estimate[i,6]+ASFR_estimate[i,7])%*%5
    
    #Level 3; we divide the code by different levels of education equation 11
    ESTFR_UN_DHS_1[i]~dnorm(ESTFR_derived_1[i],tau_estfr1[i])T(0,10)
    ESTFR_UN_DHS_2[i]~dnorm(ESTFR_derived_2[i],tau_estfr2[i])T(0,10)
    ESTFR_UN_DHS_3[i]~dnorm(ESTFR_derived_3[i],tau_estfr3[i])T(0,10)
    ESTFR_UN_DHS_4[i]~dnorm(ESTFR_derived_4[i],tau_estfr4[i])T(0,10)
    
    #Level 3 equation 12 for the different levels of education
    ESTFR_estimate_1[i]~dnorm(ESTFR_derived_1[i],tau_estfr1[i])T(0,10)
    ESTFR_estimate_2[i]~dnorm(ESTFR_derived_2[i],tau_estfr2[i])T(0,10)
    ESTFR_estimate_3[i]~dnorm(ESTFR_derived_3[i],tau_estfr3[i])T(0,10)
    ESTFR_estimate_4[i]~dnorm(ESTFR_derived_4[i],tau_estfr4[i])T(0,10)
    
    #Level 3 equation 8 for the different levels of education
    ESTFR_derived_1[i]=(ESASFR_estimate_1[i,1]+ESASFR_estimate_1[i,2]+ESASFR_estimate_1[i,3]+ESASFR_estimate_1[i,4]+ESASFR_estimate_1[i,5]+ESASFR_estimate_1[i,6]+ESASFR_estimate_1[i,7])%*%5
    ESTFR_derived_2[i]=(ESASFR_estimate_2[i,1]+ESASFR_estimate_2[i,2]+ESASFR_estimate_2[i,3]+ESASFR_estimate_2[i,4]+ESASFR_estimate_2[i,5]+ESASFR_estimate_2[i,6]+ESASFR_estimate_2[i,7])%*%5
    ESTFR_derived_3[i]=(ESASFR_estimate_3[i,1]+ESASFR_estimate_3[i,2]+ESASFR_estimate_3[i,3]+ESASFR_estimate_3[i,4]+ESASFR_estimate_3[i,5]+ESASFR_estimate_3[i,6]+ESASFR_estimate_3[i,7])%*%5
    ESTFR_derived_4[i]=(ESASFR_estimate_4[i,1]+ESASFR_estimate_4[i,2]+ESASFR_estimate_4[i,3]+ESASFR_estimate_4[i,4]+ESASFR_estimate_4[i,5]+ESASFR_estimate_4[i,6]+ESASFR_estimate_4[i,7])%*%5
    
    
    #WAge specific calculations
 
      for(j in 1:length(Age)){
      
      no_edu_diff_hi[i,j] = ESASFR_estimate_2[i,j] - ESASFR_estimate_1[i,j]
      no_edu_diff_pri[i,j] = ESASFR_estimate_2[i,j] - ESASFR_estimate_3[i,j]
      no_edu_diff_sec[i,j] = ESASFR_estimate_2[i,j] - ESASFR_estimate_4[i,j]
      
    
      w_1[i,j]=pop_wic_1[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_2[i,j]=pop_wic_2[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_3[i,j]=pop_wic_3[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_4[i,j]=pop_wic_4[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      
  
      
      ESASFR_dhs_1[i,j]~dnorm(ESASFR_estimate_1[i,j],tau_esasfr1[i,j])T(0,)
      ESASFR_dhs_2[i,j]~dnorm(ESASFR_estimate_2[i,j],tau_esasfr2[i,j])T(0,)
      ESASFR_dhs_3[i,j]~dnorm(ESASFR_estimate_3[i,j],tau_esasfr3[i,j])T(0,)
      ESASFR_dhs_4[i,j]~dnorm(ESASFR_estimate_4[i,j],tau_esasfr4[i,j])T(0,)
      
      
      #this line is for predicting our missing values for cross-validation
      ESASFR_dhs_1_pred[i,j]~dnorm(ESASFR_estimate_1[i,j],tau_esasfr1[i,j])T(0,)
      ESASFR_dhs_2_pred[i,j]~dnorm(ESASFR_estimate_2[i,j],tau_esasfr2[i,j])T(0,)
      ESASFR_dhs_3_pred[i,j]~dnorm(ESASFR_estimate_3[i,j],tau_esasfr3[i,j])T(0,)
      ESASFR_dhs_4_pred[i,j]~dnorm(ESASFR_estimate_4[i,j],tau_esasfr4[i,j])T(0,)

      ASFR_estimate[i,j]=ESASFR_dhs_1[i,j]*w_1[i,j] +  ESASFR_dhs_2[i,j]*w_2[i,j] +
      ESASFR_dhs_3[i,j]*w_3[i,j] + ESASFR_dhs_4[i,j]*w_4[i,j]
      
      
      ASFR_UN[i,j]~dnorm(ASFR_estimate[i,j],tau_asfr_UN)T(0,) 
      
      tau_esasfr1[i,j]~dgamma(s_esasfr1,sesasfr1)
      tau_esasfr2[i,j]~dgamma(s_esasfr1,sesasfr1)
      tau_esasfr3[i,j]~dgamma(s_esasfr1,sesasfr1)
      tau_esasfr4[i,j]~dgamma(s_esasfr1,sesasfr1)
    }
    
      
  tau_estfr1[i]~dgamma(tfr_mean,1/tfr_sd)
  tau_estfr2[i]~dgamma(tfr_mean,1/tfr_sd)
  tau_estfr3[i]~dgamma(tfr_mean,1/tfr_sd)
  tau_estfr4[i]~dgamma(tfr_mean,1/tfr_sd)
}
  tau_asfr_UN=1/(var_asfr_UN^2)
  var_asfr_UN=ASFR_UN_var
  
}"

#We take different proportions of the data out for each level of education
#Higher education
esasfr_glm_na1<-dat_africaedu2_%>%filter(Education=="Higher Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.05 * n()), replace = FALSE), NA))

esasfr_glm_na1<-esasfr_glm_na1[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



#No education
esasfr_glm_na2<-dat_africaedu2_%>%filter(Education=="No Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.05 * n()), replace = FALSE), NA))

esasfr_glm_na2<-esasfr_glm_na2[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)


#Primary education
esasfr_glm_na3<-dat_africaedu2_%>%filter(Education=="Primary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.05 * n()), replace = FALSE), NA))

esasfr_glm_na3<-esasfr_glm_na3[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



#Secondary education
esasfr_glm_na4<-dat_africaedu2_%>%filter(Education=="Secondary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(), size = ceiling(0.05 * n()), replace = FALSE), NA))

esasfr_glm_na4<-esasfr_glm_na4[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



####We arrange the data in such a way that is compatible with the written model code
dat_africaedu_<-dat_africaedu2_[order(dat_africaedu2_$Country), ]


dat_africaedu2<-dat_africaedu_[,-c(6,7,8)]%>%distinct()%>%spread(Education,Pred)

dat_africaedu2_1<-dat_africaedu2[,-c(5:7)]%>%spread(`Age Group`,`Higher Education`)
dat_africaedu2_1<-dat_africaedu2_1[order(dat_africaedu2_1$Country), ]


dat_africaedu2_2<-dat_africaedu2[,-c(4,6:7)]%>%spread(`Age Group`,`No Education`)
dat_africaedu2_2<-dat_africaedu2_2[order(dat_africaedu2_2$Country), ]

dat_africaedu2_3<-dat_africaedu2[,-c(4,5,7)]%>%spread(`Age Group`,`Primary Education`)
dat_africaedu2_3<-dat_africaedu2_3[order(dat_africaedu2_3$Country), ]

dat_africaedu2_4<-dat_africaedu2[,-c(4:6)]%>%spread(`Age Group`,`Secondary Education`)
dat_africaedu2_4<-dat_africaedu2_4[order(dat_africaedu2_4$Country), ]


dat_africaedu_se2<-dat_africaedu2_[,-c(5,7,8)]%>%distinct()%>%spread(Education,SE)



set.seed(122)
jagsdat_africa = as.list(dat_africaedu2_1)
jagsdat_africa$period=as.matrix(dat_africaedu2_1[,2])
jagsdat_africa$Age=as.matrix(dat_africaedu2_1[1,3:9])
jagsdat_africa$pop_wic_1=as.matrix(wice_dat_africa_1[,4:10])
jagsdat_africa$pop_wic_2=as.matrix(wice_dat_africa_2[,4:10])
jagsdat_africa$pop_wic_3=as.matrix(wice_dat_africa_3[,4:10])
jagsdat_africa$pop_wic_4=as.matrix(wice_dat_africa_4[,4:10])
jagsdat_africa$ESASFR_dhs_1=as.matrix(esasfr_glm_na1[,3:9])
jagsdat_africa$ESASFR_dhs_2=as.matrix(esasfr_glm_na2[,3:9])
jagsdat_africa$ESASFR_dhs_3=as.matrix(esasfr_glm_na3[,3:9])
jagsdat_africa$ESASFR_dhs_4=as.matrix(esasfr_glm_na4[,3:9])

jagsdat_africa$ESASFR_estimate_1=as.matrix(dat_africaedu2_1[,3:9])
jagsdat_africa$ESASFR_estimate_2=as.matrix(dat_africaedu2_2[,3:9])
jagsdat_africa$ESASFR_estimate_3=as.matrix(dat_africaedu2_3[,3:9])
jagsdat_africa$ESASFR_estimate_4=as.matrix(dat_africaedu2_4[,3:9])

jagsdat_africa$ESTFR_1=as.vector(unlist(estfr_d1_1[,5]))
jagsdat_africa$ESTFR_2=as.vector(unlist(estfr_d1_2[,5]))
jagsdat_africa$ESTFR_3=as.vector(unlist(estfr_d1_3[,5]))
jagsdat_africa$ESTFR_4=as.vector(unlist(estfr_d1_4[,5]))


jagsdat_africa$tfr_mean=sd(na.omit(estfr_d1_1$ESTFR))
jagsdat_africa$tfr_sd=var(na.omit(estfr_d1_1$ESTFR))

jagsdat_africa$ASFR_UN=as.matrix(asfr_UN[,3:9])
jagsdat_africa$ASFR_UN_var=(sd(asfr_UN_$Asfr)^2)



jagsdat_africa$sesasfr1=2*(sd(dat_africaedu_se2$`No Education`)^2)

jagsdat_africa$s_esasfr1=1/((sd(dat_africaedu_se2$`No Education`)))

jagsdat_africa$in_dhs=as.matrix(esasfr_glm_na1[,2])


paramsedu_africa =c("ESASFR_estimate_1","ESASFR_estimate_2","ESASFR_estimate_3","ESASFR_estimate_4","ESASFR_dhs_1_pred","ESASFR_dhs_2_pred","ESASFR_dhs_3_pred","ESASFR_dhs_4_pred",
                    "ESTFR_estimate_1","ESTFR_estimate_2","ESTFR_estimate_3","ESTFR_estimate_4","ASFR_estimate","TFR_estimate","tau_asfr_UN",
                    "var_asfr_UN","tau_esasfr1","tau_esasfr2","tau_esasfr3","tau_esasfr4","tau_estfr","sd_estfr","no_edu_diff_hi","no_edu_diff_pri","no_edu_diff_sec")


modedu_africa = jags.model(textConnection(mod_stringedu_africa), data=jagsdat_africa,n.chains = 3)
update(modedu_africa, 2e4)


mod_sim3edu_africa = coda.samples(model=modedu_africa,variable.names=paramsedu_africa,
                                  n.iter=2e4,thin = 200)
#################################################################################

# get summary of posterior sample
#Higher Education
MCsumedupred1_africaedu_1<-MCMCsummary(mod_sim3edu_africa, params ="ESASFR_dhs_1_pred")
UPCIedupred1_africaedu_1<-MCsumedupred1_africaedu_1$`97.5%`
LWCIedupred1_africaedu_1<-MCsumedupred1_africaedu_1$`2.5%`
posterior_meansedupred1_africaedu_1=MCsumedupred1_africaedu_1$`50%`
posterior_meansedupred1_africaedu_1<-as.data.frame(posterior_meansedupred1_africaedu_1)
UPCIedupred1_africaedu_1<-as.data.frame(UPCIedupred1_africaedu_1)
LWCIedupred1_africaedu_1<-as.data.frame(LWCIedupred1_africaedu_1)
colnames(UPCIedupred1_africaedu_1)<-"Upper_CI"
colnames(LWCIedupred1_africaedu_1)<-"Lower_CI"
posterior_quantilesedupred1_africaedu_1<-cbind(UPCIedupred1_africaedu_1,LWCIedupred1_africaedu_1)
posterior_quantilesedupred1_africaedu_1<-as.data.frame(posterior_quantilesedupred1_africaedu_1)
###


#No Education
MCsumedupred1_africaedu_2<-MCMCsummary(mod_sim3edu_africa, params ="ESASFR_dhs_2_pred")
UPCIedupred1_africaedu_2<-MCsumedupred1_africaedu_2$`97.5%`
LWCIedupred1_africaedu_2<-MCsumedupred1_africaedu_2$`2.5%`
posterior_meansedupred1_africaedu_2=MCsumedupred1_africaedu_2$`50%`
posterior_meansedupred1_africaedu_2<-as.data.frame(posterior_meansedupred1_africaedu_2)
UPCIedupred1_africaedu_2<-as.data.frame(UPCIedupred1_africaedu_2)
LWCIedupred1_africaedu_2<-as.data.frame(LWCIedupred1_africaedu_2)
colnames(UPCIedupred1_africaedu_2)<-"Upper_CI"
colnames(LWCIedupred1_africaedu_2)<-"Lower_CI"
posterior_quantilesedupred1_africaedu_2<-cbind(UPCIedupred1_africaedu_2,LWCIedupred1_africaedu_2)
posterior_quantilesedupred1_africaedu_2<-as.data.frame(posterior_quantilesedupred1_africaedu_2)


#Primary Education
MCsumedupred1_africaedu_3<-MCMCsummary(mod_sim3edu_africa, params ="ESASFR_dhs_3_pred")
UPCIedupred1_africaedu_3<-MCsumedupred1_africaedu_3$`97.5%`
LWCIedupred1_africaedu_3<-MCsumedupred1_africaedu_3$`2.5%`
posterior_meansedupred1_africaedu_3=MCsumedupred1_africaedu_3$`50%`
posterior_meansedupred1_africaedu_3<-as.data.frame(posterior_meansedupred1_africaedu_3)
UPCIedupred1_africaedu_3<-as.data.frame(UPCIedupred1_africaedu_3)
LWCIedupred1_africaedu_3<-as.data.frame(LWCIedupred1_africaedu_3)
colnames(UPCIedupred1_africaedu_3)<-"Upper_CI"
colnames(LWCIedupred1_africaedu_3)<-"Lower_CI"
posterior_quantilesedupred1_africaedu_3<-cbind(UPCIedupred1_africaedu_3,LWCIedupred1_africaedu_3)
posterior_quantilesedupred1_africaedu_3<-as.data.frame(posterior_quantilesedupred1_africaedu_3)


#Secondary Education
MCsumedupred1_africaedu_4<-MCMCsummary(mod_sim3edu_africa, params ="ESASFR_dhs_4_pred")
UPCIedupred1_africaedu_4<-MCsumedupred1_africaedu_4$`97.5%`
LWCIedupred1_africaedu_4<-MCsumedupred1_africaedu_4$`2.5%`
posterior_meansedupred1_africaedu_4=MCsumedupred1_africaedu_4$`50%`
posterior_meansedupred1_africaedu_4<-as.data.frame(posterior_meansedupred1_africaedu_4)
UPCIedupred1_africaedu_4<-as.data.frame(UPCIedupred1_africaedu_4)
LWCIedupred1_africaedu_4<-as.data.frame(LWCIedupred1_africaedu_4)
colnames(UPCIedupred1_africaedu_4)<-"Upper_CI"
colnames(LWCIedupred1_africaedu_4)<-"Lower_CI"
posterior_quantilesedupred1_africaedu_4<-cbind(UPCIedupred1_africaedu_4,LWCIedupred1_africaedu_4)
posterior_quantilesedupred1_africaedu_4<-as.data.frame(posterior_quantilesedupred1_africaedu_4)
################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_africaedupred_1<-cbind("Mean"=posterior_meansedupred1_africaedu_1,"Upper_CI"=posterior_quantilesedupred1_africaedu_1[,1],
                            "Lower_CI"=posterior_quantilesedupred1_africaedu_1[,2])
colnames(bayesdat_africaedupred_1)[1]<-"Median"


bayesdat_africaedupred_2<-cbind("Mean"=posterior_meansedupred1_africaedu_2,"Upper_CI"=posterior_quantilesedupred1_africaedu_2[,1],
                            "Lower_CI"=posterior_quantilesedupred1_africaedu_2[,2])
colnames(bayesdat_africaedupred_2)[1]<-"Median"



bayesdat_africaedupred_3<-cbind("Mean"=posterior_meansedupred1_africaedu_3,"Upper_CI"=posterior_quantilesedupred1_africaedu_3[,1],
                            "Lower_CI"=posterior_quantilesedupred1_africaedu_3[,2])
colnames(bayesdat_africaedupred_3)[1]<-"Median"



bayesdat_africaedupred_4<-cbind("Mean"=posterior_meansedupred1_africaedu_4,"Upper_CI"=posterior_quantilesedupred1_africaedu_4[,1],
                            "Lower_CI"=posterior_quantilesedupred1_africaedu_4[,2])
colnames(bayesdat_africaedupred_4)[1]<-"Median"



e_1<-gather(dat_africaedu2_1,"Age Group","ASFR",3:9)
e_1<-e_1%>%mutate(Education="Higher Education")
bayesdatpred_africa_1_<-cbind(e_1,bayesdat_africaedupred_1)


e_2<-gather(dat_africaedu2_2,"Age Group","ASFR",3:9)
e_2<-e_2%>%mutate(Education="No Education")
bayesdatpred_africa_2_<-cbind(e_2,bayesdat_africaedupred_2)


e_3<-gather(dat_africaedu2_3,"Age Group","ASFR",3:9)
e_3<-e_3%>%mutate(Education="Primary Education")
bayesdatpred_africa_3_<-cbind(e_3,bayesdat_africaedupred_3)



e_4<-gather(dat_africaedu2_4,"Age Group","ASFR",3:9)
e_4<-e_4%>%mutate(Education="Secondary Education")
bayesdatpred_africa_4_<-cbind(e_4,bayesdat_africaedupred_4)

bayesdatpred_africa1<-rbind(bayesdatpred_africa_1_,bayesdatpred_africa_2_)
bayesdatpred_africa1<-rbind(bayesdatpred_africa1,bayesdatpred_africa_3_)
bayesdatpred_africa1<-rbind(bayesdatpred_africa1,bayesdatpred_africa_4_)


bayesdatpred_africa_1<-bayesdatpred_africa1%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))


############################################################################################################################################################################################################
############################################################################################################################################################################################################


#Higher education
esasfr_glm_na11<-dat_africaedu2_%>%filter(Education=="Higher Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.1 * n()), replace = FALSE), NA))

esasfr_glm_na11<-esasfr_glm_na11[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



#No education
esasfr_glm_na12<-dat_africaedu2_%>%filter(Education=="No Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.1 * n()), replace = FALSE), NA))

esasfr_glm_na12<-esasfr_glm_na12[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)


#Primary education
esasfr_glm_na13<-dat_africaedu2_%>%filter(Education=="Primary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.1 * n()), replace = FALSE), NA))

esasfr_glm_na13<-esasfr_glm_na13[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



#Secondary education
esasfr_glm_na14<-dat_africaedu2_%>%filter(Education=="Secondary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(), size = ceiling(0.1 * n()), replace = FALSE), NA))

esasfr_glm_na14<-esasfr_glm_na14[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



set.seed(122)

jagsdat_africa$ESASFR_dhs_1=as.matrix(esasfr_glm_na11[,3:9])
jagsdat_africa$ESASFR_dhs_2=as.matrix(esasfr_glm_na12[,3:9])
jagsdat_africa$ESASFR_dhs_3=as.matrix(esasfr_glm_na13[,3:9])
jagsdat_africa$ESASFR_dhs_4=as.matrix(esasfr_glm_na14[,3:9])


modedu_africa1 = jags.model(textConnection(mod_stringedu_africa), data=jagsdat_africa,n.chains = 3)
update(modedu_africa1, 2e4)


mod_sim3edu_africa1 = coda.samples(model=modedu_africa1,variable.names=paramsedu_africa,
                                  n.iter=2e4,thin = 200)
#################################################################################

# get summary of posterior sample
#Higher Education
MCsumedupred1_africaedu_11<-MCMCsummary(mod_sim3edu_africa1, params ="ESASFR_dhs_1_pred")
UPCIedupred1_africaedu_11<-MCsumedupred1_africaedu_11$`97.5%`
LWCIedupred1_africaedu_11<-MCsumedupred1_africaedu_11$`2.5%`
posterior_meansedupred1_africaedu_11=MCsumedupred1_africaedu_11$`50%`
posterior_meansedupred1_africaedu_11<-as.data.frame(posterior_meansedupred1_africaedu_11)
UPCIedupred1_africaedu_11<-as.data.frame(UPCIedupred1_africaedu_11)
LWCIedupred1_africaedu_11<-as.data.frame(LWCIedupred1_africaedu_11)
colnames(UPCIedupred1_africaedu_11)<-"Upper_CI"
colnames(LWCIedupred1_africaedu_11)<-"Lower_CI"
posterior_quantilesedupred1_africaedu_11<-cbind(UPCIedupred1_africaedu_11,LWCIedupred1_africaedu_11)
posterior_quantilesedupred1_africaedu_11<-as.data.frame(posterior_quantilesedupred1_africaedu_11)
###


#No Education
MCsumedupred1_africaedu_21<-MCMCsummary(mod_sim3edu_africa1, params ="ESASFR_dhs_2_pred")
UPCIedupred1_africaedu_21<-MCsumedupred1_africaedu_21$`97.5%`
LWCIedupred1_africaedu_21<-MCsumedupred1_africaedu_21$`2.5%`
posterior_meansedupred1_africaedu_21=MCsumedupred1_africaedu_21$`50%`
posterior_meansedupred1_africaedu_21<-as.data.frame(posterior_meansedupred1_africaedu_21)
UPCIedupred1_africaedu_21<-as.data.frame(UPCIedupred1_africaedu_21)
LWCIedupred1_africaedu_21<-as.data.frame(LWCIedupred1_africaedu_21)
colnames(UPCIedupred1_africaedu_21)<-"Upper_CI"
colnames(LWCIedupred1_africaedu_21)<-"Lower_CI"
posterior_quantilesedupred1_africaedu_21<-cbind(UPCIedupred1_africaedu_21,LWCIedupred1_africaedu_21)
posterior_quantilesedupred1_africaedu_21<-as.data.frame(posterior_quantilesedupred1_africaedu_21)


#Primary Education
MCsumedupred1_africaedu_31<-MCMCsummary(mod_sim3edu_africa1, params ="ESASFR_dhs_3_pred")
UPCIedupred1_africaedu_31<-MCsumedupred1_africaedu_31$`97.5%`
LWCIedupred1_africaedu_31<-MCsumedupred1_africaedu_31$`2.5%`
posterior_meansedupred1_africaedu_31=MCsumedupred1_africaedu_31$`50%`
posterior_meansedupred1_africaedu_31<-as.data.frame(posterior_meansedupred1_africaedu_31)
UPCIedupred1_africaedu_31<-as.data.frame(UPCIedupred1_africaedu_31)
LWCIedupred1_africaedu_31<-as.data.frame(LWCIedupred1_africaedu_31)
colnames(UPCIedupred1_africaedu_31)<-"Upper_CI"
colnames(LWCIedupred1_africaedu_31)<-"Lower_CI"
posterior_quantilesedupred1_africaedu_31<-cbind(UPCIedupred1_africaedu_31,LWCIedupred1_africaedu_31)
posterior_quantilesedupred1_africaedu_31<-as.data.frame(posterior_quantilesedupred1_africaedu_31)


#Secondary Education
MCsumedupred1_africaedu_41<-MCMCsummary(mod_sim3edu_africa1, params ="ESASFR_dhs_4_pred")
UPCIedupred1_africaedu_41<-MCsumedupred1_africaedu_41$`97.5%`
LWCIedupred1_africaedu_41<-MCsumedupred1_africaedu_41$`2.5%`
posterior_meansedupred1_africaedu_41=MCsumedupred1_africaedu_41$`50%`
posterior_meansedupred1_africaedu_41<-as.data.frame(posterior_meansedupred1_africaedu_41)
UPCIedupred1_africaedu_41<-as.data.frame(UPCIedupred1_africaedu_41)
LWCIedupred1_africaedu_41<-as.data.frame(LWCIedupred1_africaedu_41)
colnames(UPCIedupred1_africaedu_41)<-"Upper_CI"
colnames(LWCIedupred1_africaedu_41)<-"Lower_CI"
posterior_quantilesedupred1_africaedu_41<-cbind(UPCIedupred1_africaedu_41,LWCIedupred1_africaedu_41)
posterior_quantilesedupred1_africaedu_41<-as.data.frame(posterior_quantilesedupred1_africaedu_41)
################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_africaedupred_11<-cbind("Mean"=posterior_meansedupred1_africaedu_11,"Upper_CI"=posterior_quantilesedupred1_africaedu_11[,1],
                                "Lower_CI"=posterior_quantilesedupred1_africaedu_11[,2])
colnames(bayesdat_africaedupred_11)[1]<-"Median"


bayesdat_africaedupred_21<-cbind("Mean"=posterior_meansedupred1_africaedu_21,"Upper_CI"=posterior_quantilesedupred1_africaedu_21[,1],
                                "Lower_CI"=posterior_quantilesedupred1_africaedu_21[,2])
colnames(bayesdat_africaedupred_21)[1]<-"Median"



bayesdat_africaedupred_31<-cbind("Mean"=posterior_meansedupred1_africaedu_31,"Upper_CI"=posterior_quantilesedupred1_africaedu_31[,1],
                                "Lower_CI"=posterior_quantilesedupred1_africaedu_31[,2])
colnames(bayesdat_africaedupred_31)[1]<-"Median"



bayesdat_africaedupred_41<-cbind("Mean"=posterior_meansedupred1_africaedu_41,"Upper_CI"=posterior_quantilesedupred1_africaedu_41[,1],
                                "Lower_CI"=posterior_quantilesedupred1_africaedu_41[,2])
colnames(bayesdat_africaedupred_41)[1]<-"Median"



e_11<-gather(dat_africaedu2_1,"Age Group","ASFR",3:9)
e_11<-e_11%>%mutate(Education="Higher Education")
bayesdatpred_africa_11_<-cbind(e_11,bayesdat_africaedupred_11)


e_21<-gather(dat_africaedu2_2,"Age Group","ASFR",3:9)
e_21<-e_21%>%mutate(Education="No Education")
bayesdatpred_africa_21_<-cbind(e_21,bayesdat_africaedupred_21)


e_31<-gather(dat_africaedu2_3,"Age Group","ASFR",3:9)
e_31<-e_31%>%mutate(Education="Primary Education")
bayesdatpred_africa_31_<-cbind(e_31,bayesdat_africaedupred_31)



e_41<-gather(dat_africaedu2_4,"Age Group","ASFR",3:9)
e_41<-e_41%>%mutate(Education="Secondary Education")
bayesdatpred_africa_41_<-cbind(e_41,bayesdat_africaedupred_41)

bayesdatpred_africa11<-rbind(bayesdatpred_africa_11_,bayesdatpred_africa_21_)
bayesdatpred_africa11<-rbind(bayesdatpred_africa11,bayesdatpred_africa_31_)
bayesdatpred_africa11<-rbind(bayesdatpred_africa11,bayesdatpred_africa_41_)


bayesdatpred_africa_2<-bayesdatpred_africa11%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))


############################################################################################################################################################################################################
############################################################################################################################################################################################################


#Higher education
esasfr_glm_na21<-dat_africaedu2_%>%filter(Education=="Higher Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.15 * n()), replace = FALSE), NA))

esasfr_glm_na21<-esasfr_glm_na21[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



#No education
esasfr_glm_na22<-dat_africaedu2_%>%filter(Education=="No Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.15 * n()), replace = FALSE), NA))

esasfr_glm_na22<-esasfr_glm_na22[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)


#Primary education
esasfr_glm_na23<-dat_africaedu2_%>%filter(Education=="Primary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(),  
                                      size = ceiling(0.15 * n()), replace = FALSE), NA))

esasfr_glm_na23<-esasfr_glm_na23[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



#Secondary education
esasfr_glm_na24<-dat_africaedu2_%>%filter(Education=="Secondary Education")%>%
  group_by(Country) %>%
  mutate(Pred =  replace(Pred, sample(row_number(), size = ceiling(0.15 * n()), replace = FALSE), NA))

esasfr_glm_na24<-esasfr_glm_na24[,-c(3,6,7,8)]%>%distinct()%>%spread(`Age Group`,Pred)



set.seed(122)

jagsdat_africa$ESASFR_dhs_1=as.matrix(esasfr_glm_na21[,3:9])
jagsdat_africa$ESASFR_dhs_2=as.matrix(esasfr_glm_na22[,3:9])
jagsdat_africa$ESASFR_dhs_3=as.matrix(esasfr_glm_na23[,3:9])
jagsdat_africa$ESASFR_dhs_4=as.matrix(esasfr_glm_na24[,3:9])


modedu_africa2 = jags.model(textConnection(mod_stringedu_africa), data=jagsdat_africa,n.chains = 3)
update(modedu_africa2, 2e4)


mod_sim3edu_africa2 = coda.samples(model=modedu_africa2,variable.names=paramsedu_africa,
                                   n.iter=2e4,thin = 200)
#################################################################################

# get summary of posterior sample
#Higher Education
MCsumedupred1_africaedu_12<-MCMCsummary(mod_sim3edu_africa2, params ="ESASFR_dhs_1_pred")
UPCIedupred1_africaedu_12<-MCsumedupred1_africaedu_12$`97.5%`
LWCIedupred1_africaedu_12<-MCsumedupred1_africaedu_12$`2.5%`
posterior_meansedupred1_africaedu_12=MCsumedupred1_africaedu_12$`50%`
posterior_meansedupred1_africaedu_12<-as.data.frame(posterior_meansedupred1_africaedu_12)
UPCIedupred1_africaedu_12<-as.data.frame(UPCIedupred1_africaedu_12)
LWCIedupred1_africaedu_12<-as.data.frame(LWCIedupred1_africaedu_12)
colnames(UPCIedupred1_africaedu_12)<-"Upper_CI"
colnames(LWCIedupred1_africaedu_12)<-"Lower_CI"
posterior_quantilesedupred1_africaedu_12<-cbind(UPCIedupred1_africaedu_12,LWCIedupred1_africaedu_12)
posterior_quantilesedupred1_africaedu_12<-as.data.frame(posterior_quantilesedupred1_africaedu_12)
###


#No Education
MCsumedupred1_africaedu_22<-MCMCsummary(mod_sim3edu_africa2, params ="ESASFR_dhs_2_pred")
UPCIedupred1_africaedu_22<-MCsumedupred1_africaedu_22$`97.5%`
LWCIedupred1_africaedu_22<-MCsumedupred1_africaedu_22$`2.5%`
posterior_meansedupred1_africaedu_22=MCsumedupred1_africaedu_22$`50%`
posterior_meansedupred1_africaedu_22<-as.data.frame(posterior_meansedupred1_africaedu_22)
UPCIedupred1_africaedu_22<-as.data.frame(UPCIedupred1_africaedu_22)
LWCIedupred1_africaedu_22<-as.data.frame(LWCIedupred1_africaedu_22)
colnames(UPCIedupred1_africaedu_22)<-"Upper_CI"
colnames(LWCIedupred1_africaedu_22)<-"Lower_CI"
posterior_quantilesedupred1_africaedu_22<-cbind(UPCIedupred1_africaedu_22,LWCIedupred1_africaedu_22)
posterior_quantilesedupred1_africaedu_22<-as.data.frame(posterior_quantilesedupred1_africaedu_22)


#Primary Education
MCsumedupred1_africaedu_32<-MCMCsummary(mod_sim3edu_africa2, params ="ESASFR_dhs_3_pred")
UPCIedupred1_africaedu_32<-MCsumedupred1_africaedu_32$`97.5%`
LWCIedupred1_africaedu_32<-MCsumedupred1_africaedu_32$`2.5%`
posterior_meansedupred1_africaedu_32=MCsumedupred1_africaedu_32$`50%`
posterior_meansedupred1_africaedu_32<-as.data.frame(posterior_meansedupred1_africaedu_32)
UPCIedupred1_africaedu_32<-as.data.frame(UPCIedupred1_africaedu_32)
LWCIedupred1_africaedu_32<-as.data.frame(LWCIedupred1_africaedu_32)
colnames(UPCIedupred1_africaedu_32)<-"Upper_CI"
colnames(LWCIedupred1_africaedu_32)<-"Lower_CI"
posterior_quantilesedupred1_africaedu_32<-cbind(UPCIedupred1_africaedu_32,LWCIedupred1_africaedu_32)
posterior_quantilesedupred1_africaedu_32<-as.data.frame(posterior_quantilesedupred1_africaedu_32)


#Secondary Education
MCsumedupred1_africaedu_42<-MCMCsummary(mod_sim3edu_africa2, params ="ESASFR_dhs_4_pred")
UPCIedupred1_africaedu_42<-MCsumedupred1_africaedu_42$`97.5%`
LWCIedupred1_africaedu_42<-MCsumedupred1_africaedu_42$`2.5%`
posterior_meansedupred1_africaedu_42=MCsumedupred1_africaedu_42$`50%`
posterior_meansedupred1_africaedu_42<-as.data.frame(posterior_meansedupred1_africaedu_42)
UPCIedupred1_africaedu_42<-as.data.frame(UPCIedupred1_africaedu_42)
LWCIedupred1_africaedu_42<-as.data.frame(LWCIedupred1_africaedu_42)
colnames(UPCIedupred1_africaedu_42)<-"Upper_CI"
colnames(LWCIedupred1_africaedu_42)<-"Lower_CI"
posterior_quantilesedupred1_africaedu_42<-cbind(UPCIedupred1_africaedu_42,LWCIedupred1_africaedu_42)
posterior_quantilesedupred1_africaedu_42<-as.data.frame(posterior_quantilesedupred1_africaedu_42)
################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_africaedupred_12<-cbind("Mean"=posterior_meansedupred1_africaedu_12,"Upper_CI"=posterior_quantilesedupred1_africaedu_12[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_africaedu_12[,2])
colnames(bayesdat_africaedupred_12)[1]<-"Median"


bayesdat_africaedupred_22<-cbind("Mean"=posterior_meansedupred1_africaedu_22,"Upper_CI"=posterior_quantilesedupred1_africaedu_22[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_africaedu_22[,2])
colnames(bayesdat_africaedupred_22)[1]<-"Median"



bayesdat_africaedupred_32<-cbind("Mean"=posterior_meansedupred1_africaedu_32,"Upper_CI"=posterior_quantilesedupred1_africaedu_32[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_africaedu_32[,2])
colnames(bayesdat_africaedupred_32)[1]<-"Median"



bayesdat_africaedupred_42<-cbind("Mean"=posterior_meansedupred1_africaedu_42,"Upper_CI"=posterior_quantilesedupred1_africaedu_42[,1],
                                 "Lower_CI"=posterior_quantilesedupred1_africaedu_42[,2])
colnames(bayesdat_africaedupred_42)[1]<-"Median"



e_12<-gather(dat_africaedu2_1,"Age Group","ASFR",3:9)
e_12<-e_12%>%mutate(Education="Higher Education")
bayesdatpred_africa_12_<-cbind(e_12,bayesdat_africaedupred_12)


e_22<-gather(dat_africaedu2_2,"Age Group","ASFR",3:9)
e_22<-e_22%>%mutate(Education="No Education")
bayesdatpred_africa_22_<-cbind(e_22,bayesdat_africaedupred_22)


e_32<-gather(dat_africaedu2_3,"Age Group","ASFR",3:9)
e_32<-e_32%>%mutate(Education="Primary Education")
bayesdatpred_africa_32_<-cbind(e_32,bayesdat_africaedupred_32)



e_42<-gather(dat_africaedu2_4,"Age Group","ASFR",3:9)
e_42<-e_42%>%mutate(Education="Secondary Education")
bayesdatpred_africa_42_<-cbind(e_42,bayesdat_africaedupred_42)

bayesdatpred_africa31<-rbind(bayesdatpred_africa_12_,bayesdatpred_africa_22_)
bayesdatpred_africa31<-rbind(bayesdatpred_africa31,bayesdatpred_africa_32_)
bayesdatpred_africa31<-rbind(bayesdatpred_africa31,bayesdatpred_africa_42_)


bayesdatpred_africa_3<-bayesdatpred_africa31%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))




################################################################################################################################################################################################################
################################################################################################################################################################################################################

dat_africaedu_<-dat_africaedu_%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

Countries<-unique(dat_africaedu_$Country)
Country_plots<-list()

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+  
    geom_ribbon(bayesdatpred_africa_1%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=Education),alpha=0.1)+
    geom_line(bayesdatpred_africa_1%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=Education,colour="5%")) +
    
    
    geom_ribbon(bayesdatpred_africa_2%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=Education),alpha=0.1)+
    geom_line(bayesdatpred_africa_2%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=Education,colour="10%")) +
    
    
    geom_ribbon(bayesdatpred_africa_3%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                         ymax=unlist(Upper_CI),group=Education),alpha=0.1)+
    geom_line(bayesdatpred_africa_3%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=unlist(Median),group=Education,colour="15%")) +
    geom_line(dat_africaedu_%>% filter(Country==Country_),mapping=aes(x=`Age Group` ,y=Pred,group=Education,colour="Initial values")) +
    facet_grid(Education~Year)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}   
