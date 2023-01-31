#This code is the Bayesian model for Age-Specific Fertility Rates. In this model, there are benchmarks for the model
#UN values are assumed to have higher values than our initial values from glm.predict. Thus the bias is limited to only negative values
library(readxl)
library(MCMCvis)
library(tidyverse)
library(coda)
library(zoo)
library(rjags)
library(glm2)
library(expss)
library(ggmcmc)

##We read in the input data sets, dat_la contains the ASFR values and TFR_la has the TFR values for 5 year intervals.
dat_la<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Inputs plots (with glm predict)/glm_predict_all.xlsx",sheet="ESASFR_5")

#Bring in a data set with Regions of the countries
regions<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/Cleaned_DHS.xlsx",sheet="ESASFR_5")
regions<-regions[,c(2,13)]
regions<-unique(regions)

dat_la<-merge(dat_la,regions, by="Country code")
dat_la<-dat_la%>%filter(Region=="Latin America & Caribbean", !Year%in%c("1955-1960", "1960-1965", "1965-1970"))
dat_la<-dat_la[,-7]


dat_la<-dat_la%>%
  filter(Country%in%c("Bolivia ", "Brazil", "Colombia", "Ecuador", "Guatemala", "Honduras",
                   "Mexico", "Nicaragua", "Paraguay",  "Peru"))


#Read in the TFR codes for the sake of comparison 
TFR_la<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Inputs plots (with glm predict)/glm_predict_all.xlsx",sheet="ESTFR_5")

TFR_la<-merge(TFR_la,regions, by="Country code")
TFR_la<-TFR_la%>%filter(Region=="Latin America & Caribbean", !Year%in%c("1955-1960", "1960-1965", "1965-1970"))
TFR_la<-TFR_la%>%
  filter(Country%in%c("Bolivia ", "Brazil", "Colombia", "Ecuador", "Guatemala", "Honduras",
                      "Mexico", "Nicaragua", "Paraguay",  "Peru"))


#Check the mean and precision of the Precision for the model
sd(dat_la$Pred)
1/(sd(dat_la$Pred))**2

sd(TFR_la$ESTFR)
1/sd(TFR_la$ESTFR)^2


#Read in UN data
UN_asfr<-read_excel("K:/project/BayesEdu/Fertility/Afua/UN_datasets3.xlsx",sheet="New ASFR")
asfr_UN_<-UN_asfr%>%filter(Region=="Latin America & Caribbean", !Year%in%c("1955-1960", "1960-1965", "1965-1970"))

asfr_UN_<-asfr_UN_%>%
  filter(Country%in%c("Bolivia ", "Brazil", "Colombia", "Ecuador", "Guatemala", "Honduras",
                      "Mexico", "Nicaragua", "Paraguay",  "Peru"))

asfr_UN<-spread(asfr_UN_[,-c(1,5)],`Age Group`,Asfr)

###Bringing the WIC data in to be population of women by level of education

wice_dat_la_1<-read_excel("K:/project/BayesEdu/Fertility/Afua/WIC_datasets.xlsx",sheet="WIC_highedu")
wice_dat_la_1<-wice_dat_la_1%>%filter(`Country code`%in%unique(dat_la$`Country code`))


wice_dat_la_2<-read_excel("K:/project/BayesEdu/Fertility/Afua/WIC_datasets.xlsx",sheet="WIC_noedu")
wice_dat_la_2<-wice_dat_la_2%>%filter(`Country code`%in%unique(dat_la$`Country code`))



wice_dat_la_3<-read_excel("K:/project/BayesEdu/Fertility/Afua/WIC_datasets.xlsx",sheet="WIC_priedu")
wice_dat_la_3<-wice_dat_la_3%>%filter(`Country code`%in%unique(dat_la$`Country code`))


wice_dat_la_4<-read_excel("K:/project/BayesEdu/Fertility/Afua/WIC_datasets.xlsx",sheet="WIC_secedu")
wice_dat_la_4<-wice_dat_la_4%>%filter(`Country code`%in%unique(dat_la$`Country code`))


##The education model code
mod_stringedu_la = "model {
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
  # sd_asfr_UN~dnorm(0.03,170)T(0,)
  sd_asfr_UN = 0.0001
  prec_esasfr=1/(sd_esasfr^2)
  #sd_esasfr~dnorm(0.08,150)T(0,) #ESASFR=0.08
  sd_esasfr = 0.008
  prec_estfr=1/(sd_estfr^2)
  sd_estfr~dnorm(1.4,10)

}"



####We arrange the data in such a way that is compatible with the written model code

dat_laaedu2<-spread(dat_la[,-1],Education,Pred)
dat_laaedu2[is.na(dat_laaedu2)]<-0



dat_laaedu2_1<-dat_laaedu2[,-c(5:7)]%>%spread(`Age Group`,`Higher Education`)
dat_laaedu2_2<-dat_laaedu2[,-c(4,6:7)]%>%spread(`Age Group`,`No Education`)
dat_laaedu2_3<-dat_laaedu2[,-c(4,5,7)]%>%spread(`Age Group`,`Primary Education`)
dat_laaedu2_4<-dat_laaedu2[,-c(4:6)]%>%spread(`Age Group`,`Secondary Education`)



set.seed(122)
jagsdat_laa = as.list(dat_laaedu2_1)
jagsdat_laa$period=as.matrix(dat_laaedu2_1[,2])
jagsdat_laa$Age=as.matrix(dat_laaedu2_1[1,3:9])
jagsdat_laa$pop_wic_1=as.matrix(wice_dat_la_1[,4:10])
jagsdat_laa$pop_wic_2=as.matrix(wice_dat_la_2[,4:10])
jagsdat_laa$pop_wic_3=as.matrix(wice_dat_la_3[,4:10])
jagsdat_laa$pop_wic_4=as.matrix(wice_dat_la_4[,4:10])
jagsdat_laa$ESASFR_dhs_1=as.matrix(dat_laaedu2_1[,3:9])
jagsdat_laa$ESASFR_dhs_2=as.matrix(dat_laaedu2_2[,3:9])
jagsdat_laa$ESASFR_dhs_3=as.matrix(dat_laaedu2_3[,3:9])
jagsdat_laa$ESASFR_dhs_4=as.matrix(dat_laaedu2_4[,3:9])
jagsdat_laa$ASFR_UN=as.matrix(asfr_UN[,3:9])




paramsedu_la =c("ESASFR_star_1","ESASFR_star_2","ESASFR_star_3","ESASFR_star_4",
                "ESTFR_star_1","ESTFR_star_2","ESTFR_star_3","ESTFR_star_4","ASFR_star","TFR_star","prec_asfr_UN",
                "sd_asfr_UN","prec_esasfr","sd_esasfr","prec_estfr","sd_estfr")
modedu_la = jags.model(textConnection(mod_stringedu_la), data=jagsdat_laa,n.chains = 3)
update(modedu_la, 2e5)


mod_sim3edu_la = coda.samples(model=modedu_la,variable.names=paramsedu_la,
                              n.iter=2e5,thin = 200)




#Set to saving trace plots for all the variables
#pdf("K:/project/BayesEdu/Fertility/Afua/Latin America 111821/LA UN WPP 22 update/MCMCdensityprec.pdf", width=12,onefile = TRUE)
#ggs_density(ggs(mod_sim3edu_la,family="prec_estfr"))
#dev.off()



#id_1<-gather(dat_laaedu2_1,"Age Group","ASFR",3:9)
#id_1<-id_1[,-4]%>%mutate(Education="Higher Education")
#id_1<-id_1[rep(seq_len(nrow(id_1)), each = 3000), ]

#id_2<-id_1%>%mutate(Education="No Education")
#id_3<-id_1%>%mutate(Education="Primary Education")
#id_4<-id_1%>%mutate(Education="Secondary Education")

#ESASFR_1<- ggs(mod_sim3edu_la,family = "ESASFR_star_1")%>%mutate(id_1)
#ESASFR_2<- ggs(mod_sim3edu_la,family = "ESASFR_star_2")%>%mutate(id_2)
#ESASFR_3<- ggs(mod_sim3edu_la,family = "ESASFR_star_3")%>%mutate(id_3)
#ESASFR_4<- ggs(mod_sim3edu_la,family = "ESASFR_star_4")%>%mutate(id_4)

#ESASFR<-rbind(ESASFR_1,ESASFR_2)
#ESASFR<-rbind(ESASFR,ESASFR_3)
#ESASFR<-rbind(ESASFR,ESASFR_4)



#ESASFR<-ESASFR%>%
#  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

#Countries<-unique(ESASFR$Country)
#Country_plots<-list()

#for(Country_ in Countries) {
#  Country_plots[[Country_]] =ggs_density(ESASFR%>%filter(Country==Country_)) +
#    facet_grid(Education+`Age Group`~Year)+
#    ggtitle(paste0("Density Plot ESASFR", Country_)) 
  
#  print(Country_plots[[Country_]])
#}
#library(gridExtra)
#pdf("K:/project/BayesEdu/Fertility/Afua/Latin America 111821/LA UN WPP 22 update/MCMCdensityesasfr.pdf", width=12,height=40,onefile = TRUE)

#for (Country_ in seq(length(Country_plots))){
#  grid.arrange(Country_plots[[Country_]])  
#}
#dev.off()



#For estfr
#id_12<-dat_laaedu2_1[,c(1,2)]%>%mutate(Education="Higher Education")
#id_12<-id_12[rep(seq_len(nrow(id_12)), each = 3000), ]

#id_22<-id_12%>%mutate(Education="No Education")
#id_32<-id_12%>%mutate(Education="Primary Education")
#id_42<-id_12%>%mutate(Education="Secondary Education")

#ESTFR_1<- ggs(mod_sim3edu_la,family = "ESTFR_star_1")%>%mutate(id_12)
#ESTFR_2<- ggs(mod_sim3edu_la,family = "ESTFR_star_2")%>%mutate(id_22)
#ESTFR_3<- ggs(mod_sim3edu_la,family = "ESTFR_star_3")%>%mutate(id_32)
#ESTFR_4<- ggs(mod_sim3edu_la,family = "ESTFR_star_4")%>%mutate(id_42)

#ESTFR<-rbind(ESTFR_1,ESTFR_2)
#ESTFR<-rbind(ESTFR,ESTFR_3)
#ESTFR<-rbind(ESTFR,ESTFR_4)

#ESTFR<-ESTFR%>%
#  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

#Countries<-unique(ESTFR$Country)
#Country_plots<-list()

#for(Country_ in Countries) {
#  Country_plots[[Country_]] =ggs_density(ESTFR%>%filter(Country==Country_)) +
#    facet_grid(Education~Year)+
#    ggtitle(paste0("Density Plot ESTFR ", Country_)) 
  
#  print(Country_plots[[Country_]])
#}

#pdf("K:/project/BayesEdu/Fertility/Afua/Latin America 111821/LA UN WPP 22 update/MCMCdensityestfr.pdf", width=12,onefile = TRUE)

#for (Country_ in seq(length(Country_plots))){
#  grid.arrange(Country_plots[[Country_]])  
#}
#dev.off()

#####plots for asfr

#id<-id_1[rep(seq_len(nrow(id_1)), times=5), ]
#id<-id[,-4]

#ASFR<- ggs(mod_sim3edu_la,family = "ASFR_star")%>%mutate(id)

#Countries<-unique(ASFR$Country)
#Country_plots<-list()

#for(Country_ in Countries) {
#  Country_plots[[Country_]] =ggs_density(ASFR%>%filter(Country==Country_)) +
#    facet_grid(`Age Group`~Year)+
#    ggtitle(paste0("Density Plot ASFR ", Country_)) 
  
#  print(Country_plots[[Country_]])
#}

#pdf("K:/project/BayesEdu/Fertility/Afua/Latin America 111821/LA UN WPP 22 update/MCMCdensityasfr.pdf", width=12,onefile = TRUE)

#for (Country_ in seq(length(Country_plots))){
#  grid.arrange(Country_plots[[Country_]])  
#}
#dev.off()

###TFR density plots
#id_<-id_12[rep(seq_len(nrow(id_12)), times = 5), ]
#id_<-id_[,-4]

#TFR<- ggs(mod_sim3edu_la,family = "TFR_star")%>%mutate(id_)


#Countries<-unique(TFR$Country)
#Country_plots<-list()

#for(Country_ in Countries) {
#  Country_plots[[Country_]] =ggs_density(TFR%>%filter(Country==Country_)) +
#    facet_grid(Year~.)+
#    ggtitle(paste0("Density Plot TFR ", Country_)) 
  
#  print(Country_plots[[Country_]])
#}
#pdf("K:/project/BayesEdu/Fertility/Afua/Latin America 111821/LA UN WPP 22 update/MCMCdensitytfr.pdf", width=12,onefile = TRUE)

#for (Country_ in seq(length(Country_plots))){
#  grid.arrange(Country_plots[[Country_]])  
#}
#dev.off()


# get summary of posterior sample
#Higher Education
MCsumedu1_la_1<-MCMCsummary(mod_sim3edu_la, params ="ESASFR_star_1")
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
MCsumedu1_la_2<-MCMCsummary(mod_sim3edu_la, params ="ESASFR_star_2")
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
MCsumedu1_la_3<-MCMCsummary(mod_sim3edu_la, params ="ESASFR_star_3")
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
MCsumedu1_la_4<-MCMCsummary(mod_sim3edu_la, params ="ESASFR_star_4")
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



e_1<-gather(dat_laaedu2_1,"Age Group","ASFR",3:9)
e_1<-e_1%>%mutate(Education="Higher Education")
bayesdat_la_1_<-cbind(e_1,bayesdat_la_1)


e_2<-gather(dat_laaedu2_2,"Age Group","ASFR",3:9)
e_2<-e_2%>%mutate(Education="No Education")
bayesdat_la_2_<-cbind(e_2,bayesdat_la_2)


e_3<-gather(dat_laaedu2_3,"Age Group","ASFR",3:9)
e_3<-e_3%>%mutate(Education="Primary Education")
bayesdat_la_3_<-cbind(e_3,bayesdat_la_3)



e_4<-gather(dat_laaedu2_4,"Age Group","ASFR",3:9)
e_4<-e_4%>%mutate(Education="Secondary Education")
bayesdat_la_4_<-cbind(e_4,bayesdat_la_4)

bayesdat_la1<-rbind(bayesdat_la_1_,bayesdat_la_2_)
bayesdat_la1<-rbind(bayesdat_la1,bayesdat_la_3_)
bayesdat_la1<-rbind(bayesdat_la1,bayesdat_la_4_)


bayesdat_la_<-bayesdat_la1%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

dat_laaedu<-dat_la%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

# looping over unique countries
Countries<-unique(dat_la$Country)
Country_plots<-list()

for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot() +
    geom_ribbon(bayesdat_la_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,ymin=unlist(Lower_CI),
                                                                       ymax=unlist(Upper_CI),group=Education),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_la_ %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=unlist(Median),group=Education,colour="Bayesian Median")) +
    geom_line(dat_laaedu %>% filter(Country==Country_),mapping=aes(x=`Age Group`,y=Pred,group=Education,colour="Initial Values")) +
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

library(gridExtra)

pdf("K:/project/BayesEdu/Fertility/Afua/Latin America 111821/LA UN WPP 22 update/ESASFR_la_results_modified_plots.pdf", width=15,height=12,onefile = T)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}

dev.off()


#Plot against DHS cleaned values
Cleaned_DHS_esasfr<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/Cleaned_DHS.xlsx",sheet="ESASFR_5")

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


pdf("K:/project/BayesEdu/Fertility/Afua/Latin America 111821/LA UN WPP 22 update/ESASFR_la_results_dhs_plots.pdf", width=15,height=12,onefile = T)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()


#################################################################################################################################################################
#Check with UN values
MCsum1_UN_1<-MCMCsummary(mod_sim3edu_la, params ="ASFR_star")
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
bayesdat_UN_1<-cbind(gather(dat_laaedu2_1,"Age Group","ASFR",3:9)[,-4],bayesdat_UN_1)

#Read in UN values of ASFR
ASFR_UN<-read_excel("K:/project/BayesEdu/Fertility/Afua/UN_datasets3.xlsx",sheet="New ASFR")
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


pdf("K:/project/BayesEdu/Fertility/Afua/Latin America 111821/LA UN WPP 22 update/ASFR_la_results_plots.pdf",width = 12, onefile = TRUE)
for (Country_ in seq(length(Country_plots))){
  grid.arrange(Country_plots[[Country_]])  
}
dev.off()
######################################################################################################################################

#Check with TFR values
MCsum1_TFR_1<-MCMCsummary(mod_sim3edu_la, params ="TFR_star")
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
TFR_UN<-read_excel("K:/project/BayesEdu/Fertility/Afua/UN_datasets3.xlsx",sheet="UN_tfr")
TFR_UN<-TFR_UN%>%filter(Region=="Latin America & Caribbean")

TFR_UN<-TFR_UN%>%
  filter(Country%in%c("Bolivia ", "Brazil", "Colombia", "Ecuador", "Guatemala", "Honduras",
                      "Mexico", "Nicaragua", "Paraguay",  "Peru"))

bayesdat_TFR_1<-cbind(unique(bayesdat_UN_1[,c(1,2)]),bayesdat_TFR_1)


pdf("K:/project/BayesEdu/Fertility/Afua/Latin America 111821/LA UN WPP 22 update/TFR_la_results_plots.pdf",width = 12, onefile = TRUE)
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
  

dev.off()

#############################################################################################################################################################
####################################################################################################################################

# get summary of posterior sample for TFR

#Higher Education
MCsumedu2_la_1<-MCMCsummary(mod_sim3edu_la, params ="ESTFR_star_1")
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
MCsumedu2_la_2<-MCMCsummary(mod_sim3edu_la, params ="ESTFR_star_2")
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
MCsumedu2_la_3<-MCMCsummary(mod_sim3edu_la, params ="ESTFR_star_3")
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
MCsumedu2_la_4<-MCMCsummary(mod_sim3edu_la, params ="ESTFR_star_4")
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




e_12<-dat_laaedu2_1[,c(1,2)]%>%mutate(Education="Higher Education")
bayesdat_la_12_<-cbind(e_12,bayesdat_la_12)

e_22<-dat_laaedu2_1[,c(1,2)]%>%mutate(Education="No Education")
bayesdat_la_22_<-cbind(e_22,bayesdat_la_22)


e_32<-dat_laaedu2_1[,c(1,2)]%>%mutate(Education="Primary Education")
bayesdat_la_32_<-cbind(e_32,bayesdat_la_32)

e_42<-dat_laaedu2_1[,c(1,2)]%>%mutate(Education="Secondary Education")
bayesdat_la_42_<-cbind(e_42,bayesdat_la_42)


bayesdat_la2<-rbind(bayesdat_la_12_,bayesdat_la_22_)
bayesdat_la2<-rbind(bayesdat_la2,bayesdat_la_32_)
bayesdat_la2<-rbind(bayesdat_la2,bayesdat_la_42_)





#Plot with DHS cleaned data set
cleaned_DHS<-read_excel("K:/project/BayesEdu/Fertility/Afua/Cleaned Data All_DHS/Cleaned_DHS.xlsx",sheet="ESTFR_5")
cleaned_DHS<-cleaned_DHS%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))

cleaned_DHS<-cleaned_DHS%>%
  filter(Country%in%c("Bolivia ", "Brazil", "Colombia", "Ecuador", "Guatemala", "Honduras",
                      "Mexico", "Nicaragua", "Paraguay",  "Peru"))

# looping over unique countries
bayesdat_la2<-bayesdat_la2%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))


TFR_la<-TFR_la%>%
  mutate(Education=factor(Education, levels=c("No Education","Primary Education","Secondary Education","Higher Education")))




pdf("K:/project/BayesEdu/Fertility/Afua/Latin America 111821/LA UN WPP 22 update/ESTFR_la_results_dhs_plots.pdf",width=12, onefile = TRUE)
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

dev.off()


pdf("K:/project/BayesEdu/Fertility/Afua/Latin America 111821/LA UN WPP 22 update/ESTFR_la_results_modified_plots.pdf",width=12, onefile = TRUE)
for(Country_ in Countries) {
  Country_plots[[Country_]] = ggplot()+ 
    geom_ribbon(bayesdat_la2%>% filter(Country==Country_),mapping=aes(x=Year,ymin=unlist(Lower_CI),
                                                                             ymax=unlist(Upper_CI),group=Education),alpha=0.1,fill = "red1")+
    geom_line(bayesdat_la2%>% filter(Country==Country_),
              mapping=aes(x=Year ,y=unlist(Median),group=Education, colour="Bayesian Median")) +
    geom_line(TFR_la%>% filter(Country==Country_),
              mapping=aes(x=Year,y=ESTFR,group=Education, colour="Initial Values")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    facet_wrap(Education~.)+ 
    theme_bw()+ 
    theme(plot.title = element_text(size = 10, hjust=0.5),axis.text.x = element_text(angle=90))+
    ggtitle(paste0("Education Specific Total Fertility Rates ", Country_)) + 
    labs(x="Year",y="ESTFR",colour="Estimates")
  
  
  
  print(Country_plots[[Country_]])
  
}

dev.off()

######################################################################################################################################################


####################################################################################################################################
#Saving the results as an excel sheet
library(writexl)

bayesdat_laesasfr<-bayesdat_la_%>%select("Country","Age Group","Education","Year","Upper_CI","Lower_CI","Median")
bayesdat_laestfr<-bayesdat_la2%>%select("Country","Year","Education","Upper_CI","Lower_CI","Median")
bayesdat_laasfr<-bayesdat_UN_1%>%select("Country","Year","Age Group","Upper_CI","Lower_CI","Median")
bayesdat_latfr<-bayesdat_TFR_1%>%select("Country","Year","Upper_CI","Lower_CI","Median")



write_xlsx(list("BESASFR"=bayesdat_laesasfr,"BESTFR"=bayesdat_laestfr,"ASFR"=bayesdat_laasfr,"TFR"=bayesdat_latfr),
           path ="K:/project/BayesEdu/Fertility/Afua/Latin America 111821/LA UN WPP 22 update/BFR_benchmark_model_la.xlsx", col_names=TRUE)



