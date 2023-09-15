#In this model the focus is on Education specific rates but for African countries only.
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
dat_africaedu2_<-dat_africaedu2_[order(dat_africaedu2_$Country), ]

TFR_africaedu<-read.csv("./cc_y_edu_all_paper_models.csv")%>%filter(Model.name=="UN-fully consistent",Chain=="1")
TFR_africaedu<-as.data.frame(TFR_africaedu)
Encoding(TFR_africaedu$Country) <- "UTF-8"


#We join the data for easy selection of common variables
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
#We define the jags for each education level
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
      
      #This code is to calcuate the differences between estimates 
      no_edu_diff_hi[i,j] = ESASFR_estimate_2[i,j] - ESASFR_estimate_1[i,j]
      no_edu_diff_pri[i,j] = ESASFR_estimate_2[i,j] - ESASFR_estimate_3[i,j]
      no_edu_diff_sec[i,j] = ESASFR_estimate_2[i,j] - ESASFR_estimate_4[i,j]
      
    
      #We calculate the weights for level 1 here using WIC population data
      w_1[i,j]=pop_wic_1[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_2[i,j]=pop_wic_2[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_3[i,j]=pop_wic_3[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_4[i,j]=pop_wic_4[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      
      
  
      #Level 2, equation 5 
      ESASFR_estimate_1[i,j]~dnorm(ESASFR_dhs_1[i,j],tau_esasfr1[i,j])T(0,)
      ESASFR_estimate_2[i,j]~dnorm(ESASFR_dhs_2[i,j],tau_esasfr2[i,j])T(0,)
      ESASFR_estimate_3[i,j]~dnorm(ESASFR_dhs_3[i,j],tau_esasfr3[i,j])T(0,)
      ESASFR_estimate_4[i,j]~dnorm(ESASFR_dhs_4[i,j],tau_esasfr4[i,j])T(0,)
      
      
    
      tau_esasfr1[i,j]~dgamma(s_esasfr1,sesasfr1)
      tau_esasfr2[i,j]~dgamma(s_esasfr1,sesasfr1)
      tau_esasfr3[i,j]~dgamma(s_esasfr1,sesasfr1)
      tau_esasfr4[i,j]~dgamma(s_esasfr1,sesasfr1)
      
      
      #Level 1
      #equation 2
      ASFR_estimate[i,j]=ESASFR_estimate_1[i,j]*w_1[i,j] +  ESASFR_estimate_2[i,j]*w_2[i,j] +
      ESASFR_estimate_3[i,j]*w_3[i,j] + ESASFR_estimate_4[i,j]*w_4[i,j]
      
      #equation 3
      ASFR_UN[i,j]~dnorm(ASFR_estimate[i,j],tau_asfr_UN)T(0,) 
    
    }
    
  #Level 3 quation 13 (variables tfr_sd and tfr_var are defined outside of the jags code, see below)
  tau_estfr1[i]~dgamma(tfr_sd,tfr_var)
  tau_estfr2[i]~dgamma(tfr_sd,tfr_var)
  tau_estfr3[i]~dgamma(tfr_sd,tfr_var)
  tau_estfr4[i]~dgamma(tfr_sd,tfr_var)
  }

#level 1 equation 4
  tau_asfr_UN=1/(sigma_asfr_UN^2)
  sigma_asfr_UN=ASFR_UN_var
  
}"



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


#We set seed and define our parameters to jags
set.seed(122)
jagsdat_africa = as.list(dat_africaedu2_1)
jagsdat_africa$period=as.matrix(dat_africaedu2_1[,2])
jagsdat_africa$Age=as.matrix(dat_africaedu2_1[1,3:9])
jagsdat_africa$pop_wic_1=as.matrix(wice_dat_africa_1[,4:10])
jagsdat_africa$pop_wic_2=as.matrix(wice_dat_africa_2[,4:10])
jagsdat_africa$pop_wic_3=as.matrix(wice_dat_africa_3[,4:10])
jagsdat_africa$pop_wic_4=as.matrix(wice_dat_africa_4[,4:10])

jagsdat_africa$ESASFR_dhs_1=as.matrix(dat_africaedu2_1[,3:9])
jagsdat_africa$ESASFR_dhs_2=as.matrix(dat_africaedu2_2[,3:9])
jagsdat_africa$ESASFR_dhs_3=as.matrix(dat_africaedu2_3[,3:9])
jagsdat_africa$ESASFR_dhs_4=as.matrix(dat_africaedu2_4[,3:9])


jagsdat_africa$ASFR_UN=as.matrix(asfr_UN[,3:9])
jagsdat_africa$ASFR_UN_var=(sd(asfr_UN_$Asfr)^2)

jagsdat_africa$ESTFR_UN_DHS_1=as.vector(unlist(estfr_d1_1[,5]))
jagsdat_africa$ESTFR_UN_DHS_2=as.vector(unlist(estfr_d1_2[,5]))
jagsdat_africa$ESTFR_UN_DHS_3=as.vector(unlist(estfr_d1_3[,5]))
jagsdat_africa$ESTFR_UN_DHS_4=as.vector(unlist(estfr_d1_4[,5]))


#this variable represents sigma^asfr in equation 13
jagsdat_africa$tfr_sd=sd(na.omit(estfr_d1_1$ESTFR))
jagsdat_africa$tfr_var=var(na.omit(estfr_d1_1$ESTFR))



jagsdat_africa$sesasfr1=2*(sd(dat_africaedu_se2$`No Education`)^2)

jagsdat_africa$s_esasfr1=1/((sd(dat_africaedu_se2$`No Education`)))


#We select the parameters to follow and need
paramsedu_africa =c("ESASFR_estimate_1","ESASFR_estimate_2","ESASFR_estimate_3","ESASFR_estimate_4","ESTFR_derived_1","ESTFR_derived_2","ESTFR_derived_3","ESTFR_derived_4",
                    "ESTFR_estimate_1","ESTFR_estimate_2","ESTFR_estimate_3","ESTFR_estimate_4","ASFR_estimate","TFR_estimate","tau_asfr_UN",
                    "sigma_asfr_UN","tau_esasfr1","tau_esasfr2","tau_esasfr3","tau_esasfr4","no_edu_diff_hi","no_edu_diff_pri","no_edu_diff_sec")

#Running and updaing the jags model
modedu_africa = jags.model(textConnection(mod_stringedu_africa), data=jagsdat_africa,n.chains = 3)
update(modedu_africa,2e4)

#Sampling of estimates
mod_sim3edu_africa = coda.samples(model=modedu_africa,variable.names=paramsedu_africa,
                                  n.iter=2e4,thin = 200)

# get summary of posterior sample
#Higher Education
MCsumedu1_africaedu_1<-MCMCsummary(mod_sim3edu_africa, params ="ESASFR_estimate_1")
UPCIedu1_africaedu_1<-MCsumedu1_africaedu_1$`97.5%`
LWCIedu1_africaedu_1<-MCsumedu1_africaedu_1$`2.5%`
posterior_meansedu1_africaedu_1=MCsumedu1_africaedu_1$`50%`
posterior_meansedu1_africaedu_1<-as.data.frame(posterior_meansedu1_africaedu_1)
UPCIedu1_africaedu_1<-as.data.frame(UPCIedu1_africaedu_1)
LWCIedu1_africaedu_1<-as.data.frame(LWCIedu1_africaedu_1)
colnames(UPCIedu1_africaedu_1)<-"Upper_CI"
colnames(LWCIedu1_africaedu_1)<-"Lower_CI"
posterior_quantilesedu1_africaedu_1<-cbind(UPCIedu1_africaedu_1,LWCIedu1_africaedu_1)
posterior_quantilesedu1_africaedu_1<-as.data.frame(posterior_quantilesedu1_africaedu_1)
###


#No Education
MCsumedu1_africaedu_2<-MCMCsummary(mod_sim3edu_africa, params ="ESASFR_estimate_2")
UPCIedu1_africaedu_2<-MCsumedu1_africaedu_2$`97.5%`
LWCIedu1_africaedu_2<-MCsumedu1_africaedu_2$`2.5%`
posterior_meansedu1_africaedu_2=MCsumedu1_africaedu_2$`50%`
posterior_meansedu1_africaedu_2<-as.data.frame(posterior_meansedu1_africaedu_2)
UPCIedu1_africaedu_2<-as.data.frame(UPCIedu1_africaedu_2)
LWCIedu1_africaedu_2<-as.data.frame(LWCIedu1_africaedu_2)
colnames(UPCIedu1_africaedu_2)<-"Upper_CI"
colnames(LWCIedu1_africaedu_2)<-"Lower_CI"
posterior_quantilesedu1_africaedu_2<-cbind(UPCIedu1_africaedu_2,LWCIedu1_africaedu_2)
posterior_quantilesedu1_africaedu_2<-as.data.frame(posterior_quantilesedu1_africaedu_2)


#Primary Education
MCsumedu1_africaedu_3<-MCMCsummary(mod_sim3edu_africa, params ="ESASFR_estimate_3")
UPCIedu1_africaedu_3<-MCsumedu1_africaedu_3$`97.5%`
LWCIedu1_africaedu_3<-MCsumedu1_africaedu_3$`2.5%`
posterior_meansedu1_africaedu_3=MCsumedu1_africaedu_3$`50%`
posterior_meansedu1_africaedu_3<-as.data.frame(posterior_meansedu1_africaedu_3)
UPCIedu1_africaedu_3<-as.data.frame(UPCIedu1_africaedu_3)
LWCIedu1_africaedu_3<-as.data.frame(LWCIedu1_africaedu_3)
colnames(UPCIedu1_africaedu_3)<-"Upper_CI"
colnames(LWCIedu1_africaedu_3)<-"Lower_CI"
posterior_quantilesedu1_africaedu_3<-cbind(UPCIedu1_africaedu_3,LWCIedu1_africaedu_3)
posterior_quantilesedu1_africaedu_3<-as.data.frame(posterior_quantilesedu1_africaedu_3)


#Secondary Education
MCsumedu1_africaedu_4<-MCMCsummary(mod_sim3edu_africa, params ="ESASFR_estimate_4")
UPCIedu1_africaedu_4<-MCsumedu1_africaedu_4$`97.5%`
LWCIedu1_africaedu_4<-MCsumedu1_africaedu_4$`2.5%`
posterior_meansedu1_africaedu_4=MCsumedu1_africaedu_4$`50%`
posterior_meansedu1_africaedu_4<-as.data.frame(posterior_meansedu1_africaedu_4)
UPCIedu1_africaedu_4<-as.data.frame(UPCIedu1_africaedu_4)
LWCIedu1_africaedu_4<-as.data.frame(LWCIedu1_africaedu_4)
colnames(UPCIedu1_africaedu_4)<-"Upper_CI"
colnames(LWCIedu1_africaedu_4)<-"Lower_CI"
posterior_quantilesedu1_africaedu_4<-cbind(UPCIedu1_africaedu_4,LWCIedu1_africaedu_4)
posterior_quantilesedu1_africaedu_4<-as.data.frame(posterior_quantilesedu1_africaedu_4)
################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_africaedu_1<-cbind("Mean"=posterior_meansedu1_africaedu_1,"Upper_CI"=posterior_quantilesedu1_africaedu_1[,1],
                            "Lower_CI"=posterior_quantilesedu1_africaedu_1[,2])
colnames(bayesdat_africaedu_1)[1]<-"Median"


bayesdat_africaedu_2<-cbind("Mean"=posterior_meansedu1_africaedu_2,"Upper_CI"=posterior_quantilesedu1_africaedu_2[,1],
                            "Lower_CI"=posterior_quantilesedu1_africaedu_2[,2])
colnames(bayesdat_africaedu_2)[1]<-"Median"

bayesdat_africaedu_3<-cbind("Mean"=posterior_meansedu1_africaedu_3,"Upper_CI"=posterior_quantilesedu1_africaedu_3[,1],
                            "Lower_CI"=posterior_quantilesedu1_africaedu_3[,2])
colnames(bayesdat_africaedu_3)[1]<-"Median"

bayesdat_africaedu_4<-cbind("Mean"=posterior_meansedu1_africaedu_4,"Upper_CI"=posterior_quantilesedu1_africaedu_4[,1],
                            "Lower_CI"=posterior_quantilesedu1_africaedu_4[,2])
colnames(bayesdat_africaedu_4)[1]<-"Median"



e_1<-gather(dat_africaedu2_1,"Age Group","ASFR",3:9)
e_1<-e_1%>%mutate(Education="Higher Education")
bayesdat_africa_1_<-cbind(e_1,bayesdat_africaedu_1)


e_2<-gather(dat_africaedu2_2,"Age Group","ASFR",3:9)
e_2<-e_2%>%mutate(Education="No Education")
bayesdat_africa_2_<-cbind(e_2,bayesdat_africaedu_2)


e_3<-gather(dat_africaedu2_3,"Age Group","ASFR",3:9)
e_3<-e_3%>%mutate(Education="Primary Education")
bayesdat_africa_3_<-cbind(e_3,bayesdat_africaedu_3)



e_4<-gather(dat_africaedu2_4,"Age Group","ASFR",3:9)
e_4<-e_4%>%mutate(Education="Secondary Education")
bayesdat_africa_4_<-cbind(e_4,bayesdat_africaedu_4)

bayesdat_africa1<-rbind(bayesdat_africa_1_,bayesdat_africa_2_)
bayesdat_africa1<-rbind(bayesdat_africa1,bayesdat_africa_3_)
bayesdat_africa1<-rbind(bayesdat_africa1,bayesdat_africa_4_)


bayesdat_africa_<-bayesdat_africa1%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

dat_africaedu_<-dat_africaedu_%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

# looping over unique countries
Countries<-unique(dat_africaedu2_$Country)
Country_plots<-list()

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(bayesdat_africa_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                           ymax=unlist(Upper_CI),group=Education),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_africa_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Bayesian Median")) +
    geom_line(dat_africaedu_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=Pred,
                                                                       group=Education,colour="Initial Values")) +
    facet_grid(Education~Year)+
    theme_bw()+
    theme(plot.title = element_text(size = 16, hjust=0.5),axis.text.x = element_text(angle=90),
          axis.text = element_text(size = 16),
          legend.title = element_text(size=16))+
    labs(x="Age Group",y="ASFR",colour="Source") +
    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) 
  
  
  
  print(Country_plots[[Country_]])
  
}






#Plot against DHS cleaned values
Cleaned_DHS_esasfr<-read_excel("./Cleaned_DHS_Africa.xlsx",sheet="ESASFR_5")

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


for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(bayesdat_africa_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                           ymax=unlist(Upper_CI),group=Education),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_africa_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Bayesian Median")) +
    geom_line(Cleaned_DHS_esasfr%>% filter(Country==Country_, !Year %in% c("1955-1960","1960-1965","1965-1970")),
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
    labs(x="Age Group",y="ASFR",colour="Source") +
    ggtitle(paste0("Education Specific Age Specific Fertility Rate ", Country_)) 
  
  
  
  print(Country_plots[[Country_]])
  
}



#################################################################################################################################################################
#Check with UN values
MCsum1_UN_1<-MCMCsummary(mod_sim3edu_africa, params ="ASFR_estimate")
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
bayesdat_UN_1<-cbind(gather(dat_africaedu2_1,"Age Group","ASFR",3:9)[,-4],bayesdat_UN_1)

#Read in UN values of ASFR
ASFR_UN<-read_excel("./UN_datasets3.xlsx",sheet="New ASFR")


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
          legend.title = element_text(size=16))+
    ggtitle(paste0("Age Specific Fertility Rate ", Country_)) + 
    labs(x="Age Group",y="ASFR", colour="Source") +
    theme(plot.title = element_text(size = 10, hjust=0.5))
  
  print(Country_plots[[Country_]])
  
}



######################################################################################################################################

#Check with TFR values
MCsum1_TFR_1<-MCMCsummary(mod_sim3edu_africa, params ="TFR_estimate")
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
bayesdat_TFR_1<-cbind(dat_africaedu2_1[,c(1,2)],bayesdat_TFR_1)

#Read in TFR values of TFR
TFR_UN<-read_excel("./UN_datasets3.xlsx",sheet="UN_tfr")
TFR_UN<-TFR_UN%>%filter(Region%in%c("Africa","North Africa"))

TFR_UN<-ASFR_UN%>%
  group_by(`Country code`,Year,Country,Region)%>%
  summarise(TFR_=(sum(Asfr)*5))




ggplot()+  
  geom_ribbon(bayesdat_TFR_1,mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                         ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
  geom_line(bayesdat_TFR_1,mapping=aes(x=Year ,y=unlist(Median),group=1,colour="Bayesian Median")) +
  
  geom_line(TFR_UN%>%filter(Country%in%c(unique(bayesdat_TFR_1$Country))), mapping=aes(x=Year,y=TFR_,group=1,
                                colour ="UN's TFR")) +
  facet_wrap(~Country)+
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust=0.5),axis.text.x = element_text(angle=90),
        axis.text = element_text(size = 16),
        legend.title = element_text(size=16))+
  ggtitle(paste0("Total Fertility Rate")) + 
  labs(x="Year",y="TFR", colour="Source") +
  theme(plot.title = element_text(size = 10, hjust=0.5))




####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

# get summary of posterior sample for ESTFR

#Higher Education
MCsumedu2_africa_1<-MCMCsummary(mod_sim3edu_africa, params ="ESTFR_estimate_1")
UPCIedu2_africa_1<-MCsumedu2_africa_1$`97.5%`
LWCIedu2_africa_1<-MCsumedu2_africa_1$`2.5%`
posterior_meansedu2_africa_1=MCsumedu2_africa_1$`50%`
posterior_meansedu2_africa_1<-as.data.frame(posterior_meansedu2_africa_1)
UPCIedu2_africa_1<-as.data.frame(UPCIedu2_africa_1)
LWCIedu2_africa_1<-as.data.frame(LWCIedu2_africa_1)
colnames(UPCIedu2_africa_1)<-"Upper_CI"
colnames(LWCIedu2_africa_1)<-"Lower_CI"
posterior_quantilesedu2_africa_1<-cbind(UPCIedu2_africa_1,LWCIedu2_africa_1)
posterior_quantilesedu2_africa_1<-as.data.frame(posterior_quantilesedu2_africa_1)


###
#No Education
MCsumedu2_africa_2<-MCMCsummary(mod_sim3edu_africa, params ="ESTFR_estimate_2")
UPCIedu2_africa_2<-MCsumedu2_africa_2$`97.5%`
LWCIedu2_africa_2<-MCsumedu2_africa_2$`2.5%`
posterior_meansedu2_africa_2=MCsumedu2_africa_2$`50%`
posterior_meansedu2_africa_2<-as.data.frame(posterior_meansedu2_africa_2)
UPCIedu2_africa_2<-as.data.frame(UPCIedu2_africa_2)
LWCIedu2_africa_2<-as.data.frame(LWCIedu2_africa_2)
colnames(UPCIedu2_africa_2)<-"Upper_CI"
colnames(LWCIedu2_africa_2)<-"Lower_CI"
posterior_quantilesedu2_africa_2<-cbind(UPCIedu2_africa_2,LWCIedu2_africa_2)
posterior_quantilesedu2_africa_2<-as.data.frame(posterior_quantilesedu2_africa_2)


#Primary Education
MCsumedu2_africa_3<-MCMCsummary(mod_sim3edu_africa, params ="ESTFR_estimate_3")
UPCIedu2_africa_3<-MCsumedu2_africa_3$`97.5%`
LWCIedu2_africa_3<-MCsumedu2_africa_3$`2.5%`
posterior_meansedu2_africa_3=MCsumedu2_africa_3$`50%`
posterior_meansedu2_africa_3<-as.data.frame(posterior_meansedu2_africa_3)
UPCIedu2_africa_3<-as.data.frame(UPCIedu2_africa_3)
LWCIedu2_africa_3<-as.data.frame(LWCIedu2_africa_3)
colnames(UPCIedu2_africa_3)<-"Upper_CI"
colnames(LWCIedu2_africa_3)<-"Lower_CI"
posterior_quantilesedu2_africa_3<-cbind(UPCIedu2_africa_3,LWCIedu2_africa_3)
posterior_quantilesedu2_africa_3<-as.data.frame(posterior_quantilesedu2_africa_3)


#Secondary Education
MCsumedu2_africa_4<-MCMCsummary(mod_sim3edu_africa, params ="ESTFR_estimate_4")
UPCIedu2_africa_4<-MCsumedu2_africa_4$`97.5%`
LWCIedu2_africa_4<-MCsumedu2_africa_4$`2.5%`
posterior_meansedu2_africa_4=MCsumedu2_africa_4$`50%`
posterior_meansedu2_africa_4<-as.data.frame(posterior_meansedu2_africa_4)
UPCIedu2_africa_4<-as.data.frame(UPCIedu2_africa_4)
LWCIedu2_africa_4<-as.data.frame(LWCIedu2_africa_4)
colnames(UPCIedu2_africa_4)<-"Upper_CI"
colnames(LWCIedu2_africa_4)<-"Lower_CI"
posterior_quantilesedu2_africa_4<-cbind(UPCIedu2_africa_4,LWCIedu2_africa_4)
posterior_quantilesedu2_africa_4<-as.data.frame(posterior_quantilesedu2_africa_4)

#############################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_africa_12<-cbind("Mean"=posterior_meansedu2_africa_1,"Upper_CI"=posterior_quantilesedu2_africa_1[,1],"Lower_CI"=posterior_quantilesedu2_africa_1[,2])
colnames(bayesdat_africa_12)[1]<-"Median"


bayesdat_africa_22<-cbind("Mean"=posterior_meansedu2_africa_2,"Upper_CI"=posterior_quantilesedu2_africa_2[,1],"Lower_CI"=posterior_quantilesedu2_africa_2[,2])
colnames(bayesdat_africa_22)[1]<-"Median"

bayesdat_africa_32<-cbind("Mean"=posterior_meansedu2_africa_3,"Upper_CI"=posterior_quantilesedu2_africa_3[,1],"Lower_CI"=posterior_quantilesedu2_africa_3[,2])
colnames(bayesdat_africa_32)[1]<-"Median"

bayesdat_africa_42<-cbind("Mean"=posterior_meansedu2_africa_4,"Upper_CI"=posterior_quantilesedu2_africa_4[,1],"Lower_CI"=posterior_quantilesedu2_africa_4[,2])
colnames(bayesdat_africa_42)[1]<-"Median"




e_12<-dat_africaedu2_1[,c(1,2)]%>%mutate(Education="Higher Education")
bayesdat_africa_12_<-cbind(e_12,bayesdat_africa_12)

e_22<-dat_africaedu2_1[,c(1,2)]%>%mutate(Education="No Education")
bayesdat_africa_22_<-cbind(e_22,bayesdat_africa_22)


e_32<-dat_africaedu2_1[,c(1,2)]%>%mutate(Education="Primary Education")
bayesdat_africa_32_<-cbind(e_32,bayesdat_africa_32)

e_42<-dat_africaedu2_1[,c(1,2)]%>%mutate(Education="Secondary Education")
bayesdat_africa_42_<-cbind(e_42,bayesdat_africa_42)


bayesdat_africa2<-rbind(bayesdat_africa_12_,bayesdat_africa_22_)
bayesdat_africa2<-rbind(bayesdat_africa2,bayesdat_africa_32_)
bayesdat_africa2<-rbind(bayesdat_africa2,bayesdat_africa_42_)





#Plot with DHS cleaned data set
cleaned_DHS<-read_excel("./Cleaned_DHS_Africa.xlsx",sheet="ESTFR_5")
cleaned_DHS<-cleaned_DHS%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))


# looping over unique countries
bayesdat_africa2<-bayesdat_africa2%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))


for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+ 
    geom_ribbon(bayesdat_africa2%>% filter(Country==Country_),mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                          ymax=unlist(Upper_CI),group=Education),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_africa2%>% filter(Country==Country_),
              mapping=aes(x=Year ,y=unlist(Median),group=Education, colour="Bayesian Median")) +
    geom_point(cleaned_DHS%>% filter(Country==Country_, !Year %in% c("1955-1960","1960-1965","1965-1970")),
               mapping=aes(x=Year,y=TFR,group=interaction(`Survey Year`,Education), colour="DHS")) +
    theme(plot.title = element_text(size = 16, hjust=0.5),
          axis.text = element_text(size = 16),
          legend.title = element_text(size=16),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    facet_wrap(Education~.)+ 
    theme_bw()+ 
    theme(plot.title = element_text(size = 10, hjust=0.5))+ 
    labs(x="Year",y="ESTFR",colour="Estimates")+
    ggtitle(paste0("Education Specific Total Fertility Rate ", Country_)) 
  
  
  
  print(Country_plots[[Country_]])
  
}



for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+ 
    geom_ribbon(bayesdat_africa2%>% filter(Country==Country_),mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                          ymax=unlist(Upper_CI),group=Education),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_africa2%>% filter(Country==Country_),
              mapping=aes(x=Year ,y=unlist(Median),group=Education, colour="Bayesian Median")) +
      geom_ribbon(TFR_africaedu%>% filter(Country==Country_,!Year=="2015-2020"),mapping=aes(x=Year,ymin=lwr80,
                                                                          ymax=upp80,group=Education),alpha=0.1,fill = "green")+
    geom_line(TFR_africaedu%>% filter(Country==Country_,!Year=="2015-2020"),
              mapping=aes(x=Year,y=ESTFR,group=Education, colour="Initial Values")) +
    
    facet_wrap(Education~.)+ 
    theme_bw()+ 
    theme(plot.title = element_text(size = 16, hjust=0.5),
          axis.text = element_text(size = 16),
          legend.title = element_text(size=16),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(x="Year",y="ESTFR",colour="Estimates")+
    ggtitle(paste0("Education Specific Total Fertility Rate ", Country_)) 
  
  
  
  print(Country_plots[[Country_]])
  
}


#################################################################################
#Collect and plot differences estimates

# get summary of posterior sample
#Higher Education no education
MCdiffedu1_africaedu_1<-MCMCsummary(mod_sim3edu_africa, params ="no_edu_diff_hi")
UPCIdiffedu1_africaedu_1<-MCdiffedu1_africaedu_1$`97.5%`
LWCIdiffedu1_africaedu_1<-MCdiffedu1_africaedu_1$`2.5%`
posterior_meansdiffedu1_africaedu_1=MCdiffedu1_africaedu_1$`50%`
posterior_meansdiffedu1_africaedu_1<-as.data.frame(posterior_meansdiffedu1_africaedu_1)
UPCIdiffedu1_africaedu_1<-as.data.frame(UPCIdiffedu1_africaedu_1)
LWCIdiffedu1_africaedu_1<-as.data.frame(LWCIdiffedu1_africaedu_1)
colnames(UPCIdiffedu1_africaedu_1)<-"Upper_CI"
colnames(LWCIdiffedu1_africaedu_1)<-"Lower_CI"
posterior_quantilesedu1_africaedu_1<-cbind(UPCIdiffedu1_africaedu_1,LWCIdiffedu1_africaedu_1)
posterior_quantilesedu1_africaedu_1<-as.data.frame(posterior_quantilesedu1_africaedu_1)
###


#No Education primary education diff
MCdiffedu1_africaedu_2<-MCMCsummary(mod_sim3edu_africa, params ="no_edu_diff_pri")
UPCIdiffedu1_africaedu_2<-MCdiffedu1_africaedu_2$`97.5%`
LWCIdiffedu1_africaedu_2<-MCdiffedu1_africaedu_2$`2.5%`
posterior_meansdiffedu1_africaedu_2=MCdiffedu1_africaedu_2$`50%`
posterior_meansdiffedu1_africaedu_2<-as.data.frame(posterior_meansdiffedu1_africaedu_2)
UPCIdiffedu1_africaedu_2<-as.data.frame(UPCIdiffedu1_africaedu_2)
LWCIdiffedu1_africaedu_2<-as.data.frame(LWCIdiffedu1_africaedu_2)
colnames(UPCIdiffedu1_africaedu_2)<-"Upper_CI"
colnames(LWCIdiffedu1_africaedu_2)<-"Lower_CI"
posterior_quantilesedu1_africaedu_2<-cbind(UPCIdiffedu1_africaedu_2,LWCIdiffedu1_africaedu_2)
posterior_quantilesedu1_africaedu_2<-as.data.frame(posterior_quantilesedu1_africaedu_2)


#Secondary education no education diff
MCdiffedu1_africaedu_3<-MCMCsummary(mod_sim3edu_africa, params ="no_edu_diff_sec")
UPCIdiffedu1_africaedu_3<-MCdiffedu1_africaedu_3$`97.5%`
LWCIdiffedu1_africaedu_3<-MCdiffedu1_africaedu_3$`2.5%`
posterior_meansdiffedu1_africaedu_3=MCdiffedu1_africaedu_3$`50%`
posterior_meansdiffedu1_africaedu_3<-as.data.frame(posterior_meansdiffedu1_africaedu_3)
UPCIdiffedu1_africaedu_3<-as.data.frame(UPCIdiffedu1_africaedu_3)
LWCIdiffedu1_africaedu_3<-as.data.frame(LWCIdiffedu1_africaedu_3)
colnames(UPCIdiffedu1_africaedu_3)<-"Upper_CI"
colnames(LWCIdiffedu1_africaedu_3)<-"Lower_CI"
posterior_quantilesedu1_africaedu_3<-cbind(UPCIdiffedu1_africaedu_3,LWCIdiffedu1_africaedu_3)
posterior_quantilesedu1_africaedu_3<-as.data.frame(posterior_quantilesedu1_africaedu_3)


################################################################################################################################################################
#############################################################################################################################################################
###Plotting 

bayesdat_africa_diff_edu_1<-cbind("Mean"=posterior_meansdiffedu1_africaedu_1,"Upper_CI"=posterior_quantilesedu1_africaedu_1[,1],
                                  "Lower_CI"=posterior_quantilesedu1_africaedu_1[,2])
colnames(bayesdat_africa_diff_edu_1)[1]<-"Median"


bayesdat_africa_diff_edu_2<-cbind("Mean"=posterior_meansdiffedu1_africaedu_2,"Upper_CI"=posterior_quantilesedu1_africaedu_2[,1],
                                  "Lower_CI"=posterior_quantilesedu1_africaedu_2[,2])
colnames(bayesdat_africa_diff_edu_2)[1]<-"Median"



bayesdat_africa_diff_edu_3<-cbind("Mean"=posterior_meansdiffedu1_africaedu_3,"Upper_CI"=posterior_quantilesedu1_africaedu_3[,1],
                                  "Lower_CI"=posterior_quantilesedu1_africaedu_3[,2])
colnames(bayesdat_africa_diff_edu_3)[1]<-"Median"

diff_e_1<-gather(dat_africaedu2_1,"Age Group","ASFR",3:9)

bayesdat_diff_africa_1_<-cbind(diff_e_1[,1:3],bayesdat_africa_diff_edu_1)
bayesdat_diff_africa_1_$Difference<-"No Education - Higher Education"

bayesdat_diff_africa_2_<-cbind(diff_e_1[,1:3],bayesdat_africa_diff_edu_2)
bayesdat_diff_africa_2_$Difference<-"No Education - Primary Education"

bayesdat_diff_africa_3_<-cbind(diff_e_1[,1:3],bayesdat_africa_diff_edu_3)
bayesdat_diff_africa_3_$Difference<-"No Education - Secondary Education"


bayesdat_diff_africa1<-rbind(bayesdat_diff_africa_1_,bayesdat_diff_africa_2_)
bayesdat_diff_africa1<-rbind(bayesdat_diff_africa1,bayesdat_diff_africa_3_)

bayesdat_diff_africa1<-bayesdat_diff_africa1%>%mutate(Difference=factor(Difference, levels = c("No Education - Primary Education","No Education - Secondary Education","No Education - Higher Education")))
# looping over unique countries
Countries<-unique(dat_africaedu2_$Country)
Country_plots<-list()

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(bayesdat_diff_africa1%>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                               ymax=unlist(Upper_CI),group=Difference),alpha=0.1)+
    geom_line(bayesdat_diff_africa1%>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Difference,colour=Difference)) +
    facet_wrap(~Year)+
    theme_bw() +
    theme(plot.title = element_text(size = 16, hjust=0.5),axis.text.x = element_text(angle=90),
          axis.text = element_text(size = 16),
          axis.title = element_text(size=18),
          legend.title = element_text(size=16),
          strip.text.x = element_text(size=15),
          strip.text.y = element_text(size=15),
          legend.text = element_text(size=14))+
    labs(x="Age Group",y="ASFR",colour="Difference") +
    ggtitle(paste0("Education Specific Differences in Age Specific Fertility Rate ", Country_)) 
  
  
  
  print(Country_plots[[Country_]])
  
}


#################################################################################################################################################
#Higher Education
MCprioredu1_africaedu_1<-MCMCsummary(mod_sim3edu_africa, params ="tau_esasfr1")
UPCIprioredu1_africaedu_1<-MCprioredu1_africaedu_1$`97.5%`
LWCIprioredu1_africaedu_1<-MCprioredu1_africaedu_1$`2.5%`
posterior_meansprioredu1_africaedu_1=MCprioredu1_africaedu_1$`50%`
posterior_meansprioredu1_africaedu_1<-as.data.frame(posterior_meansprioredu1_africaedu_1)
UPCIprioredu1_africaedu_1<-as.data.frame(UPCIprioredu1_africaedu_1)
LWCIprioredu1_africaedu_1<-as.data.frame(LWCIprioredu1_africaedu_1)
colnames(UPCIprioredu1_africaedu_1)<-"Upper_CI"
colnames(LWCIprioredu1_africaedu_1)<-"Lower_CI"
posterior_quantilesedu1_africaedu_1<-cbind(UPCIprioredu1_africaedu_1,LWCIprioredu1_africaedu_1)
posterior_quantilesedu1_africaedu_1<-as.data.frame(posterior_quantilesedu1_africaedu_1)
###
  
  
  # no education
MCprioredu1_africaedu_2<-MCMCsummary(mod_sim3edu_africa, params ="tau_esasfr2")
UPCIprioredu1_africaedu_2<-MCprioredu1_africaedu_2$`97.5%`
LWCIprioredu1_africaedu_2<-MCprioredu1_africaedu_2$`2.5%`
posterior_meansprioredu1_africaedu_2=MCprioredu1_africaedu_2$`50%`
posterior_meansprioredu1_africaedu_2<-as.data.frame(posterior_meansprioredu1_africaedu_2)
UPCIprioredu1_africaedu_2<-as.data.frame(UPCIprioredu1_africaedu_2)
LWCIprioredu1_africaedu_2<-as.data.frame(LWCIprioredu1_africaedu_2)
colnames(UPCIprioredu1_africaedu_2)<-"Upper_CI"
colnames(LWCIprioredu1_africaedu_2)<-"Lower_CI"
posterior_quantilesedu1_africaedu_2<-cbind(UPCIprioredu1_africaedu_2,LWCIprioredu1_africaedu_2)
posterior_quantilesedu1_africaedu_2<-as.data.frame(posterior_quantilesedu1_africaedu_2)

  
  #primary education
MCprioredu1_africaedu_3<-MCMCsummary(mod_sim3edu_africa, params ="tau_esasfr3")
UPCIprioredu1_africaedu_3<-MCprioredu1_africaedu_3$`97.5%`
LWCIprioredu1_africaedu_3<-MCprioredu1_africaedu_3$`2.5%`
posterior_meansprioredu1_africaedu_3=MCprioredu1_africaedu_3$`50%`
posterior_meansprioredu1_africaedu_3<-as.data.frame(posterior_meansprioredu1_africaedu_3)
UPCIprioredu1_africaedu_3<-as.data.frame(UPCIprioredu1_africaedu_3)
LWCIprioredu1_africaedu_3<-as.data.frame(LWCIprioredu1_africaedu_3)
colnames(UPCIprioredu1_africaedu_3)<-"Upper_CI"
colnames(LWCIprioredu1_africaedu_3)<-"Lower_CI"
posterior_quantilesedu1_africaedu_3<-cbind(UPCIprioredu1_africaedu_3,LWCIprioredu1_africaedu_3)
posterior_quantilesedu1_africaedu_3<-as.data.frame(posterior_quantilesedu1_africaedu_3)

#Secondary education
MCprioredu1_africaedu_4<-MCMCsummary(mod_sim3edu_africa, params ="tau_esasfr4")
UPCIprioredu1_africaedu_4<-MCprioredu1_africaedu_4$`97.5%`
LWCIprioredu1_africaedu_4<-MCprioredu1_africaedu_4$`2.5%`
posterior_meansprioredu1_africaedu_4=MCprioredu1_africaedu_4$`50%`
posterior_meansprioredu1_africaedu_4<-as.data.frame(posterior_meansprioredu1_africaedu_4)
UPCIprioredu1_africaedu_4<-as.data.frame(UPCIprioredu1_africaedu_4)
LWCIprioredu1_africaedu_4<-as.data.frame(LWCIprioredu1_africaedu_4)
colnames(UPCIprioredu1_africaedu_4)<-"Upper_CI"
colnames(LWCIprioredu1_africaedu_4)<-"Lower_CI"
posterior_quantilesedu1_africaedu_4<-cbind(UPCIprioredu1_africaedu_4,LWCIprioredu1_africaedu_4)
posterior_quantilesedu1_africaedu_4<-as.data.frame(posterior_quantilesedu1_africaedu_4)




bayesdat_africa_prior_edu_1<-cbind("Mean"=posterior_meansprioredu1_africaedu_1,"Upper_CI"=posterior_quantilesedu1_africaedu_1[,1],
                                                                "Lower_CI"=posterior_quantilesedu1_africaedu_1[,2])
colnames(bayesdat_africa_prior_edu_1)[1]<-"Median"


bayesdat_africa_prior_edu_2<-cbind("Mean"=posterior_meansprioredu1_africaedu_2,"Upper_CI"=posterior_quantilesedu1_africaedu_2[,1],
                                                                "Lower_CI"=posterior_quantilesedu1_africaedu_2[,2])
colnames(bayesdat_africa_prior_edu_2)[1]<-"Median"


bayesdat_africa_prior_edu_3<-cbind("Mean"=posterior_meansprioredu1_africaedu_3,"Upper_CI"=posterior_quantilesedu1_africaedu_3[,1],
                                                                "Lower_CI"=posterior_quantilesedu1_africaedu_3[,2])
colnames(bayesdat_africa_prior_edu_3)[1]<-"Median"


bayesdat_africa_prior_edu_4<-cbind("Mean"=posterior_meansprioredu1_africaedu_4,"Upper_CI"=posterior_quantilesedu1_africaedu_4[,1],
                                                                 "Lower_CI"=posterior_quantilesedu1_africaedu_4[,2])
colnames(bayesdat_africa_prior_edu_4)[1]<-"Median"


prior_e_12<-dat_africaedu2_1[,c(1,2)]%>%mutate(Education="Higher Education")
bayesdat_africa_prior_edu_12_<-cbind(prior_e_12,bayesdat_africa_prior_edu_1)

prior_e_22<-dat_africaedu2_1[,c(1,2)]%>%mutate(Education="No Education")
bayesdat_africa_prior_edu_22_<-cbind(prior_e_22,bayesdat_africa_prior_edu_2)

prior_e_32<-dat_africaedu2_1[,c(1,2)]%>%mutate(Education="Primary Education")
bayesdat_africa_prior_edu_32_<-cbind(prior_e_32,bayesdat_africa_prior_edu_3)


prior_e_42<-dat_africaedu2_1[,c(1,2)]%>%mutate(Education="Secondary Education")
bayesdat_africa_prior_edu_42_<-cbind(prior_e_42,bayesdat_africa_prior_edu_4)


bayesdat_africa_prior_edu2<-rbind(bayesdat_africa_prior_edu_12_,bayesdat_africa_prior_edu_22_)
bayesdat_africa_prior_edu2<-rbind(bayesdat_africa_prior_edu2,bayesdat_africa_prior_edu_32_)
bayesdat_africa_prior_edu2<-rbind(bayesdat_africa_prior_edu2,bayesdat_africa_prior_edu_42_)
bayesdat_africa_prior_edu2

#################################################################################
#Save results in Excel

library(writexl)

bayesdat_africaesasfr<-bayesdat_africa_%>%dplyr::select("Country","Age Group","Education","Year","Upper_CI","Lower_CI","Median")
bayesdat_africaestfr<-bayesdat_africa2%>%dplyr::select("Country","Year","Education","Upper_CI","Lower_CI","Median")
bayesdat_africaasfr<-bayesdat_UN_1%>%dplyr::select("Country","Year","Age Group","Upper_CI","Lower_CI","Median")
bayesdat_africatfr<-bayesdat_TFR_1%>%dplyr::select("Country","Year","Upper_CI","Lower_CI","Median")





write_xlsx(list("BESASFR"=bayesdat_africaesasfr,"BESTFR"=bayesdat_africaestfr,
                "ASFR"=bayesdat_africaasfr,"TFR"=bayesdat_africatfr),
           path ="./BESFR_estimates Africa.xlsx", col_names=TRUE)



write_xlsx(bayesdat_africa_prior_edu2, path ="./edu_spec_sd_init_prior.xlsx", col_names=TRUE)
write_xlsx(bayesdat_diff_africa1, path ="./edu_diff.xlsx", col_names=TRUE)

