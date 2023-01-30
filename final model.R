#In this model the focus is on Education specific rates. We include UN births in the Education specific Bayesian model
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

#We first read in our data sets
#This is our data set from the predicted glm model
dat_africaedu<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Inputs plots (with glm predict)/glm_predict_africa3.xlsx",sheet="ESASFR_5")

TFR_africaedu<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Inputs plots (with glm predict)/glm_predict_africa3.xlsx",sheet="ESTFR_5")




#Rearrange the order for smooth reading during vlookup

dat_africaedu<-dat_africaedu[,c(6,1,2,3,4,5)]


#Read in UN data
UN_asfr<-read_excel("K:/project/BayesEdu/Fertility/Afua/UN_datasets3.xlsx",sheet="New ASFR")
asfr_UN_<-UN_asfr%>%filter(Region%in%c("Africa","North Africa"), !Year%in%c("1955-1960", "1960-1965", "1965-1970"))
asfr_UN<-spread(asfr_UN_[,-c(1,5)],`Age Group`,Asfr)

###Bringing the WIC data in to be population of women by level of education

#This is for women with higher education
wice_dat_africa_1<-read_excel("K:/project/BayesEdu/Fertility/Afua/WIC_datasets.xlsx",sheet="WIC_highedu")
wice_dat_africa_1<-wice_dat_africa_1%>%filter(`Country code`%in%unique(dat_africaedu$`Country code`))



#Data for women with no education
wice_dat_africa_2<-read_excel("K:/project/BayesEdu/Fertility/Afua/WIC_datasets.xlsx",sheet="WIC_noedu")
wice_dat_africa_2<-wice_dat_africa_2%>%filter(`Country code`%in%unique(dat_africaedu$`Country code`))



#women with primary education
wice_dat_africa_3<-read_excel("K:/project/BayesEdu/Fertility/Afua/WIC_datasets.xlsx",sheet="WIC_priedu")
wice_dat_africa_3<-wice_dat_africa_3%>%filter(`Country code`%in%unique(dat_africaedu$`Country code`))



#Women with secondary education
wice_dat_africa_4<-read_excel("K:/project/BayesEdu/Fertility/Afua/WIC_datasets.xlsx",sheet="WIC_secedu")
wice_dat_africa_4<-wice_dat_africa_4%>%filter(`Country code`%in%unique(dat_africaedu$`Country code`))


##The main education model code
mod_stringedu_africa = "model {
  for (i in 1:length(period)){
  
    TFR_star[i]=(ASFR_star[i,1]+ASFR_star[i,2]+ASFR_star[i,3]+ASFR_star[i,4]+ASFR_star[i,5]+ASFR_star[i,6]+ASFR_star[i,7])%*%5
    
    ESTFR_star_1[i]~dnorm(ESTFR_1[i],prec_estfr)T(0,)
    ESTFR_star_2[i]~dnorm(ESTFR_2[i],prec_estfr)T(0,)
    ESTFR_star_3[i]~dnorm(ESTFR_3[i],prec_estfr)T(0,)
    ESTFR_star_4[i]~dnorm(ESTFR_4[i],prec_estfr)T(0,)
    
    ESTFR_1[i]=(ESASFR_star_1[i,1]+ESASFR_star_1[i,2]+ESASFR_star_1[i,3]+ESASFR_star_1[i,4]+ESASFR_star_1[i,5]+ESASFR_star_1[i,6]+ESASFR_star_1[i,7])%*%5
    ESTFR_2[i]=(ESASFR_star_2[i,1]+ESASFR_star_2[i,2]+ESASFR_star_2[i,3]+ESASFR_star_2[i,4]+ESASFR_star_2[i,5]+ESASFR_star_2[i,6]+ESASFR_star_2[i,7])%*%5
    ESTFR_3[i]=(ESASFR_star_3[i,1]+ESASFR_star_3[i,2]+ESASFR_star_3[i,3]+ESASFR_star_3[i,4]+ESASFR_star_3[i,5]+ESASFR_star_3[i,6]+ESASFR_star_3[i,7])%*%5
    ESTFR_4[i]=(ESASFR_star_4[i,1]+ESASFR_star_4[i,2]+ESASFR_star_4[i,3]+ESASFR_star_4[i,4]+ESASFR_star_4[i,5]+ESASFR_star_4[i,6]+ESASFR_star_4[i,7])%*%5
    
    
    for(j in 1:length(Age)){
      
      
      w_1[i,j]=pop_wic_1[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_2[i,j]=pop_wic_2[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_3[i,j]=pop_wic_3[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      w_4[i,j]=pop_wic_4[i,j]/(pop_wic_1[i,j]+pop_wic_2[i,j]+pop_wic_3[i,j]+pop_wic_4[i,j])
      
      
      ESASFR_star_1[i,j]~dnorm(ESASFR_dhs_1[i,j],prec_esasfr)T(0,)
      ESASFR_star_2[i,j]~dnorm(ESASFR_dhs_2[i,j],prec_esasfr)T(0,)
      ESASFR_star_3[i,j]~dnorm(ESASFR_dhs_3[i,j],prec_esasfr)T(0,)
      ESASFR_star_4[i,j]~dnorm(ESASFR_dhs_4[i,j],prec_esasfr)T(0,)
    
      
      ASFR_star[i,j]=ESASFR_star_1[i,j]*w_1[i,j] +  ESASFR_star_2[i,j]*w_2[i,j] +
      ESASFR_star_3[i,j]*w_3[i,j] + ESASFR_star_4[i,j]*w_4[i,j]
      
      
      ASFR_UN[i,j]~dnorm(ASFR_star[i,j],prec_asfr_UN)T(0,) 


  }
    
}
  prec_asfr_UN=1/(sd_asfr_UN^2)
  #sd_asfr_UN~dnorm(0.3,120)
  sd_asfr_UN=0.0004
  prec_esasfr=1/(sd_esasfr^2)
  sd_esasfr=0.025
  prec_estfr=1/(sd_estfr^2)
  sd_estfr~dnorm(1.1,50)

}"


####We arrange the data in such a way that is compatible with the written model code
dat_africaedu2<-spread(unique(dat_africaedu[,-1]),Education,Pred)
dat_africaedu2[is.na(dat_africaedu2)]<-0

dat_africaedu2_1<-dat_africaedu2[,-c(5:7)]%>%spread(`Age Group`,`Higher Education`)
dat_africaedu2_2<-dat_africaedu2[,-c(4,6:7)]%>%spread(`Age Group`,`No Education`)
dat_africaedu2_3<-dat_africaedu2[,-c(4,5,7)]%>%spread(`Age Group`,`Primary Education`)
dat_africaedu2_4<-dat_africaedu2[,-c(4:6)]%>%spread(`Age Group`,`Secondary Education`)

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

paramsedu_africa =c("ESASFR_star_1","ESASFR_star_2","ESASFR_star_3","ESASFR_star_4",
                    "ESTFR_star_1","ESTFR_star_2","ESTFR_star_3","ESTFR_star_4","ASFR_star","TFR_star","prec_asfr_UN",
                    "sd_asfr_UN","prec_esasfr","sd_esasfr","prec_estfr","sd_estfr")


modedu_africa = jags.model(textConnection(mod_stringedu_africa), data=jagsdat_africa,n.chains = 3)
update(modedu_africa, 2e4)


mod_sim3edu_africa = coda.samples(model=modedu_africa,variable.names=paramsedu_africa,
                                  n.iter=2e4,thin = 200)



# get summary of posterior sample
#Higher Education
MCsumedu1_africaedu_1<-MCMCsummary(mod_sim3edu_africa, params ="ESASFR_star_1")
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
MCsumedu1_africaedu_2<-MCMCsummary(mod_sim3edu_africa, params ="ESASFR_star_2")
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
MCsumedu1_africaedu_3<-MCMCsummary(mod_sim3edu_africa, params ="ESASFR_star_3")
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
MCsumedu1_africaedu_4<-MCMCsummary(mod_sim3edu_africa, params ="ESASFR_star_4")
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

dat_africaaedu<-dat_africaedu%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

# looping over unique countries
Countries<-unique(dat_africaedu$Country)
Country_plots<-list()

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(bayesdat_africa_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                           ymax=unlist(Upper_CI),group=Education),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_africa_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Bayesian Median")) +
    geom_line(dat_africaaedu %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=Pred,
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

pdf("K:/project/BayesEdu/Fertility/Afua/African Model/10072022 SSA WPP22 benchmark ESFR/ESASFR.pdf", width=12,onefile = T)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()





#Plot against DHS cleaned values
Cleaned_DHS_esasfr<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/Cleaned_DHS_Africa.xlsx",sheet="ESASFR_5")

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


pdf("K:/project/BayesEdu/Fertility/Afua/African Model/10072022 SSA WPP22 benchmark ESFR/ESASFR_dhs.pdf", width=15,height=12,onefile = T)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()


#################################################################################################################################################################
#Check with UN values
MCsum1_UN_1<-MCMCsummary(mod_sim3edu_africa, params ="ASFR_star")
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
ASFR_UN<-read_excel("K:/project/BayesEdu/Fertility/Afua/UN_datasets3.xlsx",sheet="New ASFR")


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

pdf("K:/project/BayesEdu/Fertility/Afua/African Model/10072022 SSA WPP22 benchmark ESFR/ASFR.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()

######################################################################################################################################

#Check with TFR values
MCsum1_TFR_1<-MCMCsummary(mod_sim3edu_africa, params ="TFR_star")
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
TFR_UN<-read_excel("K:/project/BayesEdu/Fertility/Afua/UN_datasets3.xlsx",sheet="UN_tfr")
TFR_UN<-TFR_UN%>%filter(Region%in%c("Africa","North Africa"))

TFR_UN<-ASFR_UN%>%
  group_by(`Country code`,Year,Country,Region)%>%
  summarise(TFR_=(sum(Asfr)*5))



pdf("K:/project/BayesEdu/Fertility/Afua/African Model/10072022 SSA WPP22 benchmark ESFR/TFR.pdf",width = 12, onefile = TRUE)

ggplot()+  
  geom_ribbon(bayesdat_TFR_1,mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                         ymax=unlist(Upper_CI),group=1),alpha=0.1,fill = "red1")+
  geom_line(bayesdat_TFR_1,mapping=aes(x=Year ,y=unlist(Median),group=1,colour="Bayesian Median")) +
  
  geom_line(TFR_UN, mapping=aes(x=Year,y=TFR_,group=1,
                                                             colour ="UN's TFR")) +
  facet_wrap(~Country)+
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust=0.5),axis.text.x = element_text(angle=90),
        axis.text = element_text(size = 16),
        legend.title = element_text(size=16))+
  ggtitle(paste0("Total Fertility Rate")) + 
  labs(x="Year",y="TFR", colour="Source") +
  theme(plot.title = element_text(size = 10, hjust=0.5))

dev.off()


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

# get summary of posterior sample for ESTFR

#Higher Education
MCsumedu2_africa_1<-MCMCsummary(mod_sim3edu_africa, params ="ESTFR_star_1")
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
MCsumedu2_africa_2<-MCMCsummary(mod_sim3edu_africa, params ="ESTFR_star_2")
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
MCsumedu2_africa_3<-MCMCsummary(mod_sim3edu_africa, params ="ESTFR_star_3")
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
MCsumedu2_africa_4<-MCMCsummary(mod_sim3edu_africa, params ="ESTFR_star_4")
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
cleaned_DHS<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/Cleaned_DHS_Africa.xlsx",sheet="ESTFR_5")
cleaned_DHS<-cleaned_DHS%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))


# looping over unique countries
bayesdat_africa2<-bayesdat_africa2%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))


TFR_africa<-TFR_africaedu%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))



pdf("K:/project/BayesEdu/Fertility/Afua/African Model/10072022 SSA WPP22 benchmark ESFR/ESTFR_dhs.pdf",width = 12, onefile = TRUE)

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

dev.off()




pdf("K:/project/BayesEdu/Fertility/Afua/African Model/10072022 SSA WPP22 benchmark ESFR/ESTFR.pdf",width = 12, onefile = TRUE)

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+ 
    geom_ribbon(bayesdat_africa2%>% filter(Country==Country_),mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                          ymax=unlist(Upper_CI),group=Education),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_africa2%>% filter(Country==Country_),
              mapping=aes(x=Year ,y=unlist(Median),group=Education, colour="Bayesian Median")) +
    geom_point(TFR_africa%>% filter(Country==Country_),
              mapping=aes(x=Year,y=ESTFR,group=Education, colour="Initial Values")) +
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

dev.off()





#################################################################################
#Save results in Exce;

library(writexl)

bayesdat_africaesasfr<-bayesdat_africa_%>%select("Country","Age Group","Education","Year","Upper_CI","Lower_CI","Median")
bayesdat_africaestfr<-bayesdat_africa2%>%select("Country","Year","Education","Upper_CI","Lower_CI","Median")
bayesdat_africaasfr<-bayesdat_UN_1%>%select("Country","Year","Age Group","Upper_CI","Lower_CI","Median")
bayesdat_africatfr<-bayesdat_TFR_1%>%select("Country","Year","Upper_CI","Lower_CI","Median")





write_xlsx(list("BESASFR"=bayesdat_africaesasfr,"BESTFR"=bayesdat_africaestfr,
                "ASFR"=bayesdat_africaasfr,"TFR"=bayesdat_africatfr),
           path ="K:/project/BayesEdu/Fertility/Afua/African Model/10072022 SSA WPP22 benchmark ESFR/BESFR_model.xlsx", col_names=TRUE)
