if(!require(gridExtra)){install.packages("gridExtra")}
if(!require(pracma)){install.packages("pracma")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(openxlsx)){install.packages("openxlsx")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(lubridate)){install.packages("lubridate")}

#### Open RawData, from Sleep_analysis.R
RawData = list.files(path = file_path, pattern = "*.csv", full.names = T) %>% map_df(~read.csv(.))
RawData = na.omit(RawData)
ID = file_path
ID2 = gsub("/","_", file_path)

### Separate date and time
RawData = separate(RawData,time_stamp, into = c("date","hour"),sep=" ")
RawData = arrange(RawData, date, hour,idx)
RawData$rodent_sleep_cluster = RawData$rodent_sleep
for (x in 1:nrow(RawData)){
  if (RawData$rodent_sleep[x] != "W" & RawData$rodent_sleep[x] !="P"){
    RawData$rodent_sleep[x] = "S"
  }
}
date = unique(RawData$date)
first_day = date[1]

# del_first = which(RawData$ZT==min(RawData$ZT))
# RawData = RawData[del_first[1]:nrow(RawData),]
# del_last = which(RawData$ZT == max(RawData$ZT))
# RawData = RawData[1:del_last[length(del_last)],]
# suffix = ""


### Analysis of ultradian temperature rhythms

  y = RawData$Temp[RawData$ZT < 12]
  x = seq(1,length(y), by = 1)
  maxima  = findpeaks(y,  minpeakdistance = 300, minpeakheight = mean(y), sortstr = T)
  # maxima
  
  vline_day=c(0)
  for (d in date){
         n_point = length(RawData$Temp[RawData$ZT <12 & RawData$date==d])
         n_point = n_point+vline_day[length(vline_day)]
         vline_day = c(vline_day, n_point)
  }
  vline_day = vline_day[2:length(vline_day)]
  # plot(y, type = "l")
  # points(maxima[,2],maxima[,1],pch=20,col="red")
  light_title = paste0("Light Phase - ", ID2)
  max_plot = ggplot() +
    geom_point(aes(x = maxima[,2], y=maxima[,1]), color = "red", size = 2) +
    geom_line(aes(x=x, y = y), linewidth = 1) +
    scale_y_continuous(limits= c((mean(y)-3),(mean(y)+3)))+
    labs(x = "Time (t)", y = "Temprature (°C)") +
    ggtitle(light_title) +
    geom_vline(xintercept = vline_day)+
    geom_hline(yintercept = mean(y), linetype = 'dotted')+
    theme_minimal()
  # print(max_plot) 
  
  
  y2 = RawData$Temp[RawData$ZT >= 12]
  x2 = seq(1,length(y2), by = 1)
  maxima2  = findpeaks(y2,  minpeakdistance = 300, minpeakheight = mean(y2), sortstr = T)
  # maxima2
  #plot(y2, type = "l", ylim=(mean(y2)-1))
  #points(maxima[,2],maxima[,1],pch=20,col="green")
  
  vline_day2=c(0)
  for (d in date){
    n_point = length(RawData$Temp[RawData$ZT >=12 & RawData$date==d])
    n_point = n_point+vline_day2[length(vline_day2)]
    vline_day2 = c(vline_day2, n_point)
  }
  vline_day2 = vline_day2[2:length(vline_day2)]
  
  max2_plot = ggplot() +
                geom_point(aes(x = maxima2[,2], y=maxima2[,1]), color = "blue", size = 2) +
                geom_line(aes(x=x2, y = y2), linewidth = 1) +
                scale_y_continuous(limits= c((mean(y2)-3),(mean(y2)+3)))+
                labs(x = "Time (t)", y = "Temprature (°C)") +
                ggtitle("Dark Phase") +
                geom_vline(xintercept = vline_day2)+
                geom_hline(yintercept = mean(y2), linetype = 'dotted')+
                theme_minimal()
  # print(max2_plot)
  
  grid.arrange(max_plot, max2_plot, nrow = 2)
  
  
##### TEST Detrend
  df = data.frame(x,y)
  quadratic_model <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = df)
  coefficients <- coef(quadratic_model)
  c <- coefficients["(Intercept)"]
  d = coefficients["x"]
  e <- coefficients["I(x^2)"]
  f <- coefficients["I(x^3)"]
  g <- coefficients["I(x^4)"]
  df$y_fitted <- c + d * x + e * x^2 + f * x^3 + g * x^4
  df$y_detrend = df$y - df$y_fitted
  maxima_detrend  = findpeaks(df$y_detrend,  minpeakdistance = 200, minpeakheight = mean(df$y_detrend), sortstr = T)
  
  a_plot = ggplot(df, aes(x=x))+
    geom_line(aes(y=y), linewidth = 1, color = "blue")+
    geom_line(aes(y=y_fitted), linewidth = 1, color = 'red')+
    scale_y_continuous(limits = c(33,39))+
    theme_classic()
  
  b_plot = ggplot()+
    geom_line(aes(x=df$x, y=df$y_detrend), linewidth = 1, color = "black")+
    geom_point(aes(x = maxima_detrend[,2], y=maxima_detrend[,1]), color = "green", size = 2) +
    scale_y_continuous(limits = c(-1.5,1.5))+
    geom_hline(yintercept = 0, color = 'grey', linewidth = 1, linetype = 'dotted')+
    theme_classic()

    grid.arrange(a_plot, b_plot, nrow = 2)
    
    
