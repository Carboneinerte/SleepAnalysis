
### Required package
if(!require(tidyr)){install.packages("tidyr")}
if(!require(openxlsx)){install.packages("openxlsx")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(tidyverse)){install.packages("tidyverse")}


temp_3161 = read.xlsx("3061.xlsx", sheet="Phase - Mean")
temp_3161$ID = c(rep("3061",6))

temp_3160 = read.xlsx("3060.xlsx", sheet="Phase - Mean")
temp_3160$ID = c(rep("3060",6))

temp_3086 = read.xlsx("3086.xlsx", sheet="Phase - Mean")
temp_3086$ID = c(rep("3086",6))

Temp = data.frame(temp_3060)
Temp = rbind.data.frame(Temp,temp_3061,temp_3086)

Temp_light = subset.data.frame(Temp, Phase =="Light")
Temp_dark = subset.data.frame(Temp,Phase=="Dark")

e = ggplot(Temp_light,aes(fill=ID, x= Stage_sleep, y=Mean_Phase_one))+
  geom_col(position="dodge",na.rm=T)+
  geom_errorbar(aes(ymin=Mean_Phase_one-SEM_Phase_one, ymax=Mean_Phase_one+SEM_Phase_one),
                width=0.25, colour='black', linewidth = 0.75,position = "dodge")+
  theme_classic()+
  labs(x=NULL, y="Average",title = "Light Phase")+
  geom_hline(yintercept = 0)

f = ggplot(Temp_dark,aes(fill=ID, x= Stage_sleep, y=Mean_Phase_one))+
  geom_col(position="dodge",na.rm=T)+
  geom_errorbar(aes(ymin=Mean_Phase_one-SEM_Phase_one, ymax=Mean_Phase_one+SEM_Phase_one),
                width=0.25, colour='black', linewidth = 0.75,position = "dodge")+
  theme_classic()+
  labs(x=NULL, y="Average",title = "Dark Phase")+
  geom_hline(yintercept = 0)

 print(e)
 print(f)

 
 temp_3060_all = read.xlsx("3060.xlsx", sheet="All days")
 temp_3060_all$ID = c(rep("3060",length(temp_3160_all$ZT)))
 
 temp_3061_all = read.xlsx("3061.xlsx", sheet="All days")
 temp_3061_all$ID = c(rep("3061",length(temp_3161_all$ZT)))
 
 temp_3086_all = read.xlsx("3086.xlsx", sheet="All days")
 temp_3086_all$ID = c(rep("3086",length(temp_3186_all$ZT)))
 
 Temp_all = data.frame(temp_3060_all)
 Temp_all = rbind.data.frame(Temp_all,temp_3061_all,temp_3086_all)
 Temp_all[Temp_all==0] = NA
 
 
 
 ggplot(Temp_all, aes(x=ZT))+
   geom_jitter(aes(y=Wake),color="red", position = position_jitter(0.5), shape=15,na.rm=T)+
   geom_jitter(aes(y=Sleep),color="green", position = position_jitter(0.5), shape=15,na.rm=T)+
   geom_jitter(aes(y=Paradoxical), color="blue",position = position_jitter(0.5), shape=15,na.rm=T)+
   xlab("ZT")+
   theme_classic()
 