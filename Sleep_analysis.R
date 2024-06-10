###TODO
### Autosave figures?
### Detailed bouts analysis -> Mean . ZT + Phase
### ADD UNIT IN TABLES


######### Required package ##########
if(!require(tidyr)){install.packages("tidyr")}
if(!require(tidyr)){install.packages("dplyr")}
if(!require(openxlsx)){install.packages("openxlsx")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(lubridate)){install.packages("lubridate")}
library(gtools)


Sleep_preparation <- function(file_path) {
  # Data Import and Preparation --------------------
  # Import data from all .csv files in the given folder and omit NAs
  raw_data <- list.files(path = file_path, pattern = "\\.csv$", full.names = TRUE) %>%
    purrr::map_df(~read.csv(.)) #%>%
    #na.omit()
  
  # Generate ID and ID2 from file path
  id <- file_path
  id2 <- gsub("/", "_", file_path)
  
  # Separate date and time and arrange the data
  raw_data <- raw_data %>%
    tidyr::separate(time_stamp, into = c("date", "hour"), sep = " ") %>%
    dplyr::arrange(date, hour, idx)
  
  # Modify rodent_sleep based on conditions
  raw_data$rodent_sleep_cluster <- raw_data$rodent_sleep
  raw_data$rodent_sleep <- ifelse(raw_data$rodent_sleep %in% c("W", "P"), 
                                  raw_data$rodent_sleep, "S")
  
  # Get unique dates and determine the first day
  dates <- unique(raw_data$date)
  first_day <- dates[1]
  
  # Remove first and last ZT values to focus the dataset
  del_first <- which(raw_data$ZT == min(raw_data$ZT))
  raw_data <- raw_data[del_first[1]:nrow(raw_data), ]
  del_last <- which(raw_data$ZT == max(raw_data$ZT))
  raw_data <- raw_data[1:del_last[length(del_last)], ]
  
  # Suffix for the analysis
  suffix <- ""
  
  # Perform sleep analysis
  Sleep_analysis(raw_data, suffix, id, file_path)
}


Sleep_analysis = function(raw_data,
                          suffix = NULL,
                          ID,
                          file_path){

# Base parameters-------------
ID2 = gsub("/","_", file_path)
date = unique(raw_data$date)
ZT = unique(raw_data$ZT)
wb = createWorkbook()
wb2 = createWorkbook()
addWorksheet(wb2, "raw_data")
writeData(wb2, sheet="raw_data", x=raw_data)
State = c("W", "S", "P")
Cluster = c("1","2","3","4","5","6")

#### Sleep state extraction--------------------
for (t in date){
  Subfull = subset(raw_data, date == t)
  date_ca = rep(t,length(ZT))
  df = data.frame(date_ca,ZT) #Reset the dataframe at each loop
  df2 = data.frame(date_ca,ZT)
  for (z in State){
    count_table = c() #reset the array at each loop
    count_table_max = c()
    count_table_mean = c()
    for (y in ZT){
      count <- nrow(Subfull[Subfull$ZT == y & Subfull$rodent_sleep == z,])
      count_table = c(count_table, count)
      
      ### Count of sequential same state
      sub_Subfull = subset(Subfull, ZT == y)
      if (length(sub_Subfull$ZT !=0)){
      r = rle(sub_Subfull$rodent_sleep == z)
      count_ser = r$lengths[r$values == TRUE]
      max_count = max(count_ser)
      ave_count = mean(count_ser)
      } else {
        max_count = NA
        ave_count = NA
      }
    }
    df = cbind(df, count_table)
  }
  names(df)=c("Date","ZT", "Wake", "NREM", "REM")

  df_cluster = data.frame(date_ca, ZT)
  for (c in Cluster){
    count_table = c() #reset the array at each loop
    count_table_max = c()
    count_table_mean = c()
    for (y in ZT){
      count <- nrow(Subfull[Subfull$ZT == y & Subfull$rodent_sleep_cluster == c,])
      count_table = c(count_table, count)
    }
    df_cluster = cbind(df_cluster, count_table)
  }

  names(df_cluster) = c("Date","ZT","cluster1","cluster2","cluster3","cluster4","cluster5","cluster6")

  df = cbind(df, df_cluster[3:8])
  
  ### Create sum and percentage columns
  max_epoch_per_ZT = (ZT[10]-ZT[9])*360
  df$Total_Sleep = df$NREM + df$REM
  df$Sum = df$Wake + df$NREM + df$REM
  df$Wake_percent = df$Wake / max_epoch_per_ZT * 100
  df$NREM_percent = df$NREM / max_epoch_per_ZT * 100
  df$REM_Percent = df$REM / max_epoch_per_ZT * 100
  df$Total_Sleep_Percent = df$Total_Sleep / max_epoch_per_ZT * 100
  df$Uncategorized_percent = (max_epoch_per_ZT-df$Sum)/max_epoch_per_ZT*100

  ### add data to workbook
  addWorksheet(wb, t)
  writeData(wb, sheet= t, x = df)


}

### Combine all days-------------------------
df_one = data.frame()
for (t in date){
  temp = read.xlsx(wb, sheet=t)
  df_one = rbind(df_one,temp)
}

df_one = df_one[df_one$Sum > 250,]

addWorksheet(wb2, "State-All-days")
writeData(wb2,"State-All-days", x=df_one)


date_phase= c(date,date)
date_phase = sort(date_phase)
Phase_day = rep(c("Light","Dark"),length(date_phase)/2)
df3 = data.frame(date_phase, Phase_day, row.names = NULL)
df3_gne = data.frame()
temp_df3 = c()
for (t in date){
  Mean_Light_day = c(sum(df_one$Wake[df_one$ZT<12 & df_one$Date ==t]),
                     sum(df_one$NREM[df_one$ZT<12 & df_one$Date ==t]),
                     sum(df_one$REM[df_one$ZT<12 & df_one$Date ==t]),
                     # sd(df_one$Wake[df_one$ZT<12 & df_one$Date ==t]),
                     # sd(df_one$Sleep[df_one$ZT<12 & df_one$Date ==t]),
                     # sd(df_one$Paradoxical[df_one$ZT<12 & df_one$Date ==t]),
                     length(df_one$Wake[df_one$ZT<12 & df_one$Date ==t]),
                     length(df_one$NREM[df_one$ZT<12 & df_one$Date ==t]),
                     length(df_one$REM[df_one$ZT<12 & df_one$Date ==t]))
  
  Mean_Dark_day = c(sum(df_one$Wake[df_one$ZT>=12 & df_one$Date ==t]),
                    sum(df_one$NREM[df_one$ZT>=12 & df_one$Date ==t]),
                    sum(df_one$REM[df_one$ZT>=12 & df_one$Date ==t]),
                    # sd(df_one$Wake[df_one$ZT>=12&df_one$Date ==t]),
                    # sd(df_one$Sleep[df_one$ZT>=12&df_one$Date ==t]),
                    # sd(df_one$Paradoxical[df_one$ZT>=12&df_one$Date ==t]),
                    length(df_one$Wake[df_one$ZT>=12&df_one$Date ==t]),
                    length(df_one$NREM[df_one$ZT>=12&df_one$Date ==t]),
                    length(df_one$REM[df_one$ZT>=12&df_one$Date ==t]))
  
  temp_df3 = rbind(temp_df3, Mean_Light_day,Mean_Dark_day)
}
df3 = cbind(df3,temp_df3)
names(df3) = c("Date","Phase","Mean_Wake","Mean_NREM","Mean_NREM",
               # "SD_Wake","SD_Sleep","SD_Paradoxical",
               "N_Wake","N_NREM","N_REM")
rownames(df3)=NULL
df3 = df3[df3$Mean_Wake !=0,]

addWorksheet(wb2, "Phase-All-Days")
writeData(wb2, sheet="Phase-All-Days", x= df3)

df_ZT_Mean = data.frame(ZT)
mean_table = data.frame()
for (y in ZT){
  ZT_mean = c(mean(df_one$Wake[df_one$ZT == y]),mean(df_one$NREM[df_one$ZT == y]),mean(df_one$REM[df_one$ZT == y]),mean(df_one$Total_Sleep[df_one$ZT == y]),
              # sd(df_one$Wake[df_one$ZT ==y]),sd(df_one$NREM[df_one$ZT ==y]),sd(df_one$REM[df_one$ZT ==y]),
              length(df_one$Wake[df_one$ZT == y]),length(df_one$NREM[df_one$ZT==y]),length(df_one$REM[df_one$ZT==y]),
              nrow(raw_data[raw_data$rodent_sleep == "W" & raw_data$ZT == y,])/nrow(raw_data[raw_data$ZT == y,])*100,
              nrow(raw_data[raw_data$rodent_sleep == "S" & raw_data$ZT == y,])/nrow(raw_data[raw_data$ZT == y,])*100,
              nrow(raw_data[raw_data$rodent_sleep == "P" & raw_data$ZT == y,])/nrow(raw_data[raw_data$ZT == y,])*100,
              mean(df_one$cluster1[df_one$ZT == y]),mean(df_one$cluster2[df_one$ZT == y]),mean(df_one$cluster3[df_one$ZT == y]),
              mean(df_one$cluster4[df_one$ZT == y]),mean(df_one$cluster5[df_one$ZT == y]),mean(df_one$cluster6[df_one$ZT == y]),
              # sd(df_one$cluster1[df_one$ZT ==y]),sd(df_one$cluster2[df_one$ZT ==y]),sd(df_one$cluster3[df_one$ZT ==y]),
              # sd(df_one$cluster4[df_one$ZT ==y]),sd(df_one$cluster5[df_one$ZT ==y]),sd(df_one$cluster6[df_one$ZT ==y]),
              nrow(raw_data[raw_data$rodent_sleep_cluster == 1 & raw_data$ZT == y,])/nrow(raw_data[raw_data$ZT == y & raw_data$rodent_sleep == "S",])*100,
              nrow(raw_data[raw_data$rodent_sleep_cluster == 2 & raw_data$ZT == y,])/nrow(raw_data[raw_data$ZT == y & raw_data$rodent_sleep == "S",])*100,
              nrow(raw_data[raw_data$rodent_sleep_cluster == 3 & raw_data$ZT == y,])/nrow(raw_data[raw_data$ZT == y & raw_data$rodent_sleep == "S",])*100,
              nrow(raw_data[raw_data$rodent_sleep_cluster == 4 & raw_data$ZT == y,])/nrow(raw_data[raw_data$ZT == y & raw_data$rodent_sleep == "S",])*100,
              nrow(raw_data[raw_data$rodent_sleep_cluster == 5 & raw_data$ZT == y,])/nrow(raw_data[raw_data$ZT == y & raw_data$rodent_sleep == "S",])*100,
              nrow(raw_data[raw_data$rodent_sleep_cluster == 6 & raw_data$ZT == y,])/nrow(raw_data[raw_data$ZT == y & raw_data$rodent_sleep == "S",])*100
              )
  
  mean_table = rbind(mean_table, ZT_mean)
  names(mean_table) = c("Wake","NREM","REM","Total_Sleep",
                        # "Wake_SD","Sleep_SD","Paradoxical_SD",
                        "N_Wake","N_NREM","N_REM",
                        "Wake_Percent","NREM_Percent","REM_Percent","Cluster1","Cluster2","Cluster3","Cluster4","Cluster5","Cluster6",
                        # "Cluster1_SD","Cluster2_SD","Cluster3_SD","Cluster4_SD","Cluster5_SD","Cluster6_SD",
                        "Cluster1_Percent","Cluster2_Percent","Cluster3_Percent","Cluster4_Percent","Cluster5_Percent","Cluster6_Percent"
  )
}

df_ZT_Mean = cbind(df_ZT_Mean,mean_table)
df_ZT_Mean = arrange(df_ZT_Mean, ZT)
addWorksheet(wb2, "ZT-Mean")
writeData(wb2,"ZT-Mean", x=df_ZT_Mean)

### Summary of state per phase
Phase = c(rep("Light",4),rep("Dark",4))
Stage_sleep = rep(c("Wake","NREM","REM","Sleep"),2)

Mean_Phase_one = c(sum(df_ZT_Mean$Wake[df_ZT_Mean$ZT < 12]),sum(df_ZT_Mean$NREM[df_ZT_Mean$ZT < 12]),sum(df_ZT_Mean$REM[df_ZT_Mean$ZT < 12]),sum(df_ZT_Mean$Total_Sleep[df_ZT_Mean$ZT < 12]),
                   sum(df_ZT_Mean$Wake[df_ZT_Mean$ZT >= 12]),sum(df_ZT_Mean$NREM[df_ZT_Mean$ZT >= 12]),sum(df_ZT_Mean$REM[df_ZT_Mean$ZT >= 12]),sum(df_ZT_Mean$Total_Sleep[df_ZT_Mean$ZT >= 12]))

df_mean_one = data.frame(Phase, Stage_sleep, Mean_Phase_one)
names(df_mean_one) = c("Phase","Stage","Mean_ep")
df_mean_one$Mean_min = df_mean_one$Mean_ep * (1/6)
df_mean_one$Mean_h = df_mean_one$Mean_min / 60

addWorksheet(wb2, "Phase - Mean")
writeData(wb2, sheet="Phase - Mean", x= df_mean_one)

### Summary of cluster per phase
Phase_cluster = c(rep("Light",6),rep("Dark",6))
Cluster_ = rep(c("Cluster1","Cluster2","Cluster3","Cluster4","Cluster5","Cluster6"),2)

Mean_cluster = c(sum(df_ZT_Mean$Cluster1[df_ZT_Mean$ZT < 12]),sum(df_ZT_Mean$Cluster2[df_ZT_Mean$ZT < 12]),sum(df_ZT_Mean$Cluster3[df_ZT_Mean$ZT < 12]),sum(df_ZT_Mean$Cluster4[df_ZT_Mean$ZT < 12]),sum(df_ZT_Mean$Cluster5[df_ZT_Mean$ZT < 12]),sum(df_ZT_Mean$Cluster6[df_ZT_Mean$ZT < 12]),
                 sum(df_ZT_Mean$Cluster1[df_ZT_Mean$ZT >= 12]),sum(df_ZT_Mean$Cluster2[df_ZT_Mean$ZT >= 12]),sum(df_ZT_Mean$Cluster3[df_ZT_Mean$ZT >= 12]),sum(df_ZT_Mean$Cluster4[df_ZT_Mean$ZT >= 12]),sum(df_ZT_Mean$Cluster5[df_ZT_Mean$ZT >= 12]),sum(df_ZT_Mean$Cluster6[df_ZT_Mean$ZT >= 12]))

df_cluster_mean = data.frame(Phase_cluster, Cluster, Mean_cluster)
names(df_cluster_mean) = c("Phase","Cluster","Cluster_Mean_ep")
df_cluster_mean$Cluster_Mean_min = df_cluster_mean$Cluster_Mean_ep * (1/6)
df_cluster_mean$Cluster_Mean_h = df_cluster_mean$Cluster_Mean_min / 60

addWorksheet(wb2, "Phase-Cluster-Mean")
writeData(wb2, sheet="Phase-Cluster-Mean", x= df_cluster_mean)


#### Bouts of sleep states #####
bouts_df = raw_data
bouts_df$rodent_sleep[bouts_df$rodent_sleep == "P"] = "S"
bouts_df$durations = 0  #new column to store the duration
current_letter <- ""
current_duration <- 0

# Loop through each row and calculate the durations ####### I
for (i in 1:(nrow(bouts_df)-1)){
  if (bouts_df$rodent_sleep[i] != current_letter 
      #& bouts_df$rodent_sleep[i+1] != current_letter
      ){
    current_letter <- bouts_df$rodent_sleep[i]
    current_duration <- 1
  } else {
    current_duration <- current_duration + 1
  }
  
  bouts_df$durations[i] <- current_duration
}

### Sleep to Wake
sleep_duration_epoch = 12

valid_indices <- which(bouts_df$durations == 1 & 
                         lag(bouts_df$rodent_sleep, n = 1) == "S" & 
                         lag(bouts_df$durations, n = 1) > sleep_duration_epoch)

prewakesleep_list <- vector("list", length(valid_indices))
# Loop through the valid indices
for (idx in seq_along(valid_indices)) {
  v <- valid_indices[idx]
  idx_temp <- bouts_df$idx[v]
  bouts_df_temp <- bouts_df[(v - sleep_duration_epoch):v, ]
  
  # Extract relevant indices and handle NAs
  idx_range <- (idx_temp - sleep_duration_epoch):(idx_temp - 1)
  temp <- ifelse(idx_range %in% bouts_df_temp$idx, 
                 bouts_df_temp$rodent_sleep_cluster[match(idx_range, bouts_df_temp$idx)], 
                 "NA")
  
  # Store the result in the list
  prewakesleep_list[[idx]] <- temp
}
prewakesleep <- do.call(rbind, prewakesleep_list)

colnames(prewakesleep) <- paste0("wake-", sleep_duration_epoch:1)
prewakesleep <- as.data.frame(prewakesleep)

long_df = prewakesleep %>%
      pivot_longer(all_of(colnameepo))

frequency_df <- long_df %>%
       group_by(name, value) %>%
       summarize(frequency = n(), .groups = 'drop')

wide_df <- frequency_df %>%
       pivot_wider(names_from = value, values_from = frequency, names_prefix = "cluster_")
wide_df = wide_df %>%
  mutate(num = as.numeric(gsub("wake-","",name))) %>%
  arrange(num) %>%
  select(-num) %>%
  mutate(total_sleep_measured = nrow(prewakesleep))


wide_df <- wide_df %>%
  mutate(across(starts_with("cluster_"), ~ .x / total_sleep_measured * 100, .names = "{.col}-percent"))


addWorksheet(wb2, "Pre_wake")
writeData(wb2, sheet = "Pre_wake" , x=wide_df)
####


letter_changes <- bouts_df$rodent_sleep[c(TRUE, bouts_df$rodent_sleep[-1] != bouts_df$rodent_sleep[-nrow(bouts_df)])]

# Extract the duration each time the letter changes
duration_changes <- bouts_df$durations[c(which(bouts_df$rodent_sleep %in% letter_changes), nrow(bouts_df))]

for (j in 1:nrow(bouts_df)-1){
  if (bouts_df$durations[j+1]!=1){
    bouts_df$max_dur[j] = NA
  } else {bouts_df$max_dur[j] = bouts_df$durations[j]}
}
bouts_df$max_dur[nrow(bouts_df)] = bouts_df$durations[nrow(bouts_df)]

bouts_df_co = data.frame(bouts_df$date,bouts_df$hour,bouts_df$ZT,bouts_df$rodent_sleep,bouts_df$max_dur)
bouts_df_co = bouts_df_co[complete.cases(bouts_df_co), ]
names(bouts_df_co) = c("Date","Hour","ZT","Sleep_state","Bouts_Duration")
bouts_df_co$'Duration (min)' = bouts_df_co$`Bouts_Duration`*(1/6)
bouts_df_co_S = bouts_df_co[bouts_df_co$Sleep_state == "S",]
bouts_df_co_S = arrange(bouts_df_co_S, ZT)
bouts_df_co_W = bouts_df_co[bouts_df_co$Sleep_state == "W",]
bouts_df_co_W = arrange(bouts_df_co_W, ZT)

# Bouts density by total length
Breaks =c("0-.5","0.5-1","1-2","2-4","4-8","8-16","16-32","32+")
Breaks_ep_co =data.frame()

for (d_ in date){
date_ep = rep(d_, length(Breaks))
Breaks_ep = data.frame(date_ep, Breaks)
breaks_df =data.frame(f_=c(0,3,6,12,24,48,96,192), l_=c(3,6,12,24,48,96,192,600))
Combi = c()
  if (nrow(bouts_df_co_S[bouts_df_co_S$Date == d_ & bouts_df_co_S$ZT == 22,]>0)){
    for (n in 1:nrow(breaks_df)){
      Sum_Light = sum(bouts_df_co_S$Bouts_Duration[bouts_df_co_S$Date == d_ &
                                                   bouts_df_co_S$ZT < 12 &
                                                   bouts_df_co_S$Bouts_Duration>=breaks_df$f_[n] &
                                                   bouts_df_co_S$Bouts_Duration<breaks_df$l_[n]])
      Sum_Dark = sum(bouts_df_co_S$Bouts_Duration[bouts_df_co_S$Date == d_ &
                                                    bouts_df_co_S$ZT >= 12 &
                                                    bouts_df_co_S$Bouts_Duration>breaks_df$f_[n] &
                                                    bouts_df_co_S$Bouts_Duration<breaks_df$l_[n]])
      Temp_ = c(Sum_Light, Sum_Dark)
      Combi = rbind(Combi,Temp_)
    }
  } else {
      for (n in 1:nrow(breaks_df)){
        Sum_Light = sum(bouts_df_co_S$Bouts_Duration[bouts_df_co_S$Date == d_ &
                                                       bouts_df_co_S$ZT < 12 &
                                                       bouts_df_co_S$Bouts_Duration>=breaks_df$f_[n] &
                                                       bouts_df_co_S$Bouts_Duration<breaks_df$l_[n]])
        
        Sum_Dark = NA
        
        Temp_ = c(Sum_Light, Sum_Dark)
        Combi = rbind(Combi,Temp_)    
    }
  }


Breaks_ep = cbind(Breaks_ep,Combi)
rownames(Breaks_ep)=NULL
names(Breaks_ep) = c('Date','Breaks(min)','Light_Phase_ep','Dark_Phase_ep')
Breaks_ep$Light_phase_min = Breaks_ep$Light_Phase_ep*(1/6)
Breaks_ep$Dark_phase_min = Breaks_ep$Dark_Phase_ep*(1/6)

Breaks_ep_co = rbind(Breaks_ep_co, Breaks_ep)

}

#Mean by phase
Light_mean = c()
Dark_mean = c()
for (n in Breaks){
  Temp = mean(Breaks_ep_co$Light_Phase_ep[Breaks_ep_co$`Breaks(min)` == n], na.rm = T)
  Light_mean = c(Light_mean, Temp)
  Temp = mean(Breaks_ep_co$Dark_Phase_ep[Breaks_ep_co$`Breaks(min)` == n], na.rm = T)
  Dark_mean = c(Dark_mean, Temp)
}
Breaks_Mean = data.frame(Breaks,Light_mean,Dark_mean)
names(Breaks_Mean) = c('Breaks(min)','Light_phase_ep','Dark_phase_ep')
Breaks_Mean$Light_phase_min = Breaks_Mean$Light_phase_ep*(1/6)
Breaks_Mean$Dark_phase_min = Breaks_Mean$Dark_phase_ep*(1/6)
Breaks_Mean$Light_phase_h = Breaks_Mean$Light_phase_min / 60
Breaks_Mean$Dark_phase_h = Breaks_Mean$Dark_phase_min / 60

### Mean by ZT
Bouts_ZT_Mean = data.frame(ZT)
Bouts_mean_table = data.frame()
for (y in ZT){
  Bouts = c(mean(bouts_df_co$'Duration (min)'[bouts_df_co$ZT == y & bouts_df_co$Sleep_state == "W"]),
            median(bouts_df_co$'Duration (min)'[bouts_df_co$ZT == y & bouts_df_co$Sleep_state == "W"]),
            max(bouts_df_co$'Duration (min)'[bouts_df_co$ZT == y & bouts_df_co$Sleep_state == "W"]),
            # sd(bouts_df_co$'Duration (min)'[bouts_df_co$ZT == y & bouts_df_co$Sleep_state == "W"]),
            length(bouts_df_co$'Duration (min)'[bouts_df_co$ZT==y & bouts_df_co$Sleep_state == "W"]),
            mean(bouts_df_co$'Duration (min)'[bouts_df_co$ZT == y & bouts_df_co$Sleep_state == "S"]),
            median(bouts_df_co$'Duration (min)'[bouts_df_co$ZT == y & bouts_df_co$Sleep_state == "S"]),
            max(bouts_df_co$'Duration (min)'[bouts_df_co$ZT == y & bouts_df_co$Sleep_state == "S"]),
            # sd(bouts_df_co$'Duration (min)'[bouts_df_co$ZT == y & bouts_df_co$Sleep_state == "S"]),
            length(bouts_df_co$'Duration (min)'[bouts_df_co$ZT==y & bouts_df_co$Sleep_state == "S"])
  )
  
  Bouts_mean_table = rbind(Bouts_mean_table, Bouts)

  names(Bouts_mean_table) = c("Bouts_W_Mean (min)","Bouts_W_Median (min)","Bouts_W_Max (min)",
                              #"Bouts_W_SD",
                              "Bouts_W_n",
                              "Bouts_S_Mean (min)","Bouts_S_Median (min)","Bouts_S_Max (min)",
                              #"Bouts_S_SD",
                              "Bouts_S_n")
}
Bouts_ZT_Mean = cbind(Bouts_ZT_Mean, Bouts_mean_table)
 
### Wake event ###
date_acute = unique(raw_data$date)
count_=0
count_temp = c()
date_ZT = data.frame()
# ZT_list = c()
for (d in date_acute){
  Temp_df = raw_data[raw_data$date==d,]
  ZT_acute = sort(unique(Temp_df$ZT))
  # ZT_list = c(ZT_list,ZT_acute)
  test_df = data.frame(date = rep(d,length(ZT_acute)),ZT = ZT_acute)
  date_ZT = rbind(date_ZT,test_df)
  for (z in ZT_acute){
    Temp_df2 = Temp_df[Temp_df$ZT==z,]
    count_=0
    for (x in 2:nrow(Temp_df2)){
      if (Temp_df2$rodent_sleep[x] == "W" & Temp_df2$rodent_sleep[x-1] == "S"
          #& Temp_df$ZT[x]==z & Temp_df$date[x] == d
          ){
        count_ = count_ + 1
      }
    }
    count_temp = c(count_temp, count_)
  }
}
date_ZT_count = cbind(date_ZT, count_temp)
rownames(date_ZT_count)=NULL
names(date_ZT_count)=c('date','ZT','Wake_event')

count_mean = c()
for (z in ZT){
  temp_ave = mean(date_ZT_count$Wake_event[date_ZT_count$ZT==z])
  count_mean = c(count_mean, temp_ave)
}

count_date = c()
un_phase = c("Light","Dark")
un_date = sort(rep(unique(date_ZT_count$date),2))
Wake_event_sum_phase = data.frame(Date = un_date, Phase = rep(un_phase,length(unique(date_ZT_count$date))))
for (d in unique(date_ZT_count$date)){
  temp_sum_l = sum(date_ZT_count$Wake_event[date_ZT_count$date == d & date_ZT_count$ZT < 12])
  temp_sum_d = sum(date_ZT_count$Wake_event[date_ZT_count$date == d & date_ZT_count$ZT >11])
  count_date = c(count_date, temp_sum_l, temp_sum_d)
}
Wake_event_sum_phase = cbind(Wake_event_sum_phase,count_date)
Wake_event_sum_phase = Wake_event_sum_phase[Wake_event_sum_phase$count_date >0,]

count_phase = c(mean(date_ZT_count$Wake_event[date_ZT_count$ZT < 12]),
                mean(date_ZT_count$Wake_event[date_ZT_count$ZT >11]))

Wake_event_sum_phase_av = c(mean(Wake_event_sum_phase$count_date[Wake_event_sum_phase$Phase == "Light"]),
                            mean(Wake_event_sum_phase$count_date[Wake_event_sum_phase$Phase == "Dark"]))

Wake_event_mean = count_mean
Bouts_ZT_Mean = cbind(Bouts_ZT_Mean,Wake_event_mean)

addWorksheet(wb2, "Bouts-All-Days")
writeData(wb2, sheet = "Bouts-All-Days" , x=bouts_df_co)

addWorksheet(wb2, "Bouts-S-All-Days")
writeData(wb2, sheet = "Bouts-S-All-Days" , x=bouts_df_co_S)

addWorksheet(wb2, "Bouts-W-All-Days")
writeData(wb2, sheet = "Bouts-W-All-Days" , x=bouts_df_co_W)

addWorksheet(wb2, "Bouts-ZT-Mean")
writeData(wb2, sheet = "Bouts-ZT-Mean" , x=Bouts_ZT_Mean)

addWorksheet(wb2, "Uninterrupted-sleep-All-Days")
writeData(wb2, sheet = "Uninterrupted-sleep-All-Days" , x=Breaks_ep_co)

addWorksheet(wb2, "Uninterrupted-sleep-Mean")
writeData(wb2, sheet = "Uninterrupted-sleep-Mean" , x=Breaks_Mean)

##### onset of sleep at light on
onset_date = data.frame(date)
onset_list = c()
onset_W_list = c()
siesta_delay = c()
for (t in date){
  #### First sleep
  Sub_onset = subset(raw_data, date == t & ZT == 0)
  if (nrow(Sub_onset != 0)){
      Sub_onset$rodent_sleep[Sub_onset$rodent_sleep == "P"] = "S"
      len_Sub_onset = nrow(Sub_onset) - 3
      sleep_episode = c()
          for (x in 1:len_Sub_onset){if (Sub_onset$rodent_sleep[x] == "S" & Sub_onset$rodent_sleep[x+1] == "S" & Sub_onset$rodent_sleep[x+2] == "S" )
                                       {sleep_episode[x]=1}
                                    else {sleep_episode[x]=0}
              first_sleep = which(sleep_episode == 1)
              onset_sleep = (first_sleep[1]-1)*10
          }
       
      } else {onset_sleep = NA}
     
      onset_list = c(onset_list, onset_sleep)
      
  #### First Wake
      
        Sub_onset2 = subset(raw_data, date == t & ZT == 12)
  if (nrow(Sub_onset2 != 0)){
    Sub_onset2$rodent_sleep[Sub_onset2$rodent_sleep == "P"] = "S"
    #Sub_onset2[,1:2] = NULL
    #Sub_onset2[,3:17] = NULL
    
    wake_episode = c()
    len_Sub_onset = nrow(Sub_onset2) - 3
    for (x in 1:len_Sub_onset){
      if (Sub_onset2$rodent_sleep[x] == "W" & Sub_onset2$rodent_sleep[x+1] == "W" &
          Sub_onset2$rodent_sleep[x+2] == "W" ){
        wake_episode[x]=1
      } else {wake_episode[x]=0}
    }
    first_wake = which(wake_episode == 1)
    onset_wake = (first_wake[1]-1)*10
    
  } else {onset_wake = NA}
  onset_W_list = c(onset_W_list, onset_wake)
  
  ##### first siesta ####
  
  d = which(bouts_df_co$Sleep_state=="W" & bouts_df_co$Date == t & bouts_df_co$ZT>=12)[1]
  q = which(bouts_df_co$Sleep_state=="S"&bouts_df_co$`Duration (min)`>=4 & bouts_df_co$Date==t & bouts_df_co$ZT>=12)
  q = q[q>d]
  if (!is_empty(q)){
  siesta = abs((as.numeric(hms(bouts_df_co[q[1],2]))-(round(bouts_df_co[q[1],6]*60)))-(as.numeric(hms(bouts_df_co[d,2]))-(round(bouts_df_co[d,6]*60))))/60
  } else { siesta = "none"}
  siesta_delay = c(siesta_delay, siesta)
}

onset_date = cbind(onset_date, onset_list, onset_W_list)
names(onset_date) = c("Date", "First_Sleep_30s_ZT0_s","First_Wake_30s_ZT12_s")
onset_date$First_Sleep_30s_ZT0_min = onset_date$First_Sleep_30s_ZT0_s / 60
onset_date$First_Wake_30s_ZT12_min = onset_date$First_Wake_30s_ZT12_s / 60
onset_date = cbind(onset_date,siesta_delay)
onset_date = na.omit(onset_date)

addWorksheet(wb2, "Bouts-First-Sleep-Wake")
writeData(wb2, sheet="Bouts-First-Sleep-Wake", x=onset_date)

#### Loop #####
wb=createWorkbook()
col_=c(names(raw_data)[5:11],names(raw_data[28]))

aggre_date = data.frame()
for (t in date){
     date_ca = data.frame()
     date_ca = cbind(rep(t,length(ZT)))
     aggre_date = rbind(aggre_date,date_ca)
}
aggre_ZT = data.frame()
aggre_ZT= cbind(rep(ZT,length(date)))

all_aggregate = data.frame(aggre_date,aggre_ZT)

for (c in col_){
  Subset_raw = raw_data[,c("date","ZT",c)]
  for (t in date){
    #Loop_df = data.frame()
    count_table_temp = c()
    for (y in ZT) {
      count_temp = mean(Subset_raw[Subset_raw$ZT == y & Subset_raw$date == t,c])
      if (is.nan(count_temp)){
        count_temp = 0
        count_table_temp = c(count_table_temp, count_temp)
      }else{
      count_table_temp = c(count_table_temp, count_temp)
      }
    }
    #Loop_df = cbind(Loop_df, count_table_temp)
    Temp_sheet = paste0(t,"-", c)
    addWorksheet(wb, Temp_sheet)
    writeData(wb, sheet= Temp_sheet, x = count_table_temp)
  }
  
  Loop_df_one = data.frame()
  for (t in date){
    Temp_sheet = paste0(t,"-",c)
    temp4 = read.xlsx(wb, sheet=Temp_sheet, colNames = F)
    names(temp4) = c
    Loop_df_one = rbind(Loop_df_one,temp4)
  }
  all_aggregate = cbind(all_aggregate,Loop_df_one)
}
if (!is_empty(raw_data$Activity)){
  col_ = c(col_, "Activity")
  c = "Activity"
  for (t in date){
    #Loop_df = data.frame()
    count_table_temp = c()
    for (y in ZT) {
      count_temp = sum(raw_data$Activity[raw_data$ZT == y & raw_data$date == t])
      if (is.nan(count_temp)){
        count_temp = 0
        count_table_temp = c(count_table_temp, count_temp)
      }else{
        count_table_temp = c(count_table_temp, count_temp)
      }
    }
    #Loop_df = cbind(Loop_df, count_table_temp)
    Temp_sheet = paste0(t,"-", c)
    addWorksheet(wb, Temp_sheet)
    writeData(wb, sheet= Temp_sheet, x = count_table_temp)
  }
  
  Loop_df_one = data.frame()
  for (t in date){
    Temp_sheet = paste0(t,"-",c)
    temp4 = read.xlsx(wb, sheet=Temp_sheet, colNames = F)
    names(temp4) = c
    Loop_df_one = rbind(Loop_df_one,temp4)
  }
  all_aggregate = cbind(all_aggregate,Loop_df_one)
}
  all_aggregate = all_aggregate[all_aggregate$EMG_z !=0,]
  col_2 = c("Date","ZT",col_)
  names(all_aggregate) = col_2
  addWorksheet(wb2, "Other-All-Days")
  writeData(wb2, "Other-All-Days", x=all_aggregate)
  
  ### Mean by ZT
  Loop_ZT_Mean = data.frame(ZT)
  for (c in col_){
    Loop_mean_table = data.frame()
    for (y in ZT){
      Loop = c(mean(all_aggregate[all_aggregate$ZT == y,c]),
               #sd(all_aggregate[all_aggregate$ZT == y,c]),
               length(all_aggregate[all_aggregate$ZT==y,c])
      )
      
      Loop_mean_table = rbind(Loop_mean_table, Loop)
      names_mean = paste0(c,"_Mean")
      #names_SD = paste0(c,"_SD")
      names_n = paste0(c,"_N")
      names(Loop_mean_table) = c(names_mean,
                                 #names_SD,
                                 names_n)
    }
  Loop_ZT_Mean = cbind(Loop_ZT_Mean, Loop_mean_table)  
  }
  
  addWorksheet(wb2, "Other-ZT-Mean")
  writeData(wb2,"Other-ZT-Mean", x=Loop_ZT_Mean)
  
  ### Per Phase
  Phase_acti = c("Light","Dark")
  Acti = c(sum(Loop_ZT_Mean$Activity_Mean[Loop_ZT_Mean$ZT < 12]),sum(Loop_ZT_Mean$Activity_Mean[Loop_ZT_Mean$ZT >= 12]))
  Temp = c(mean(Loop_ZT_Mean$Temp_Mean[Loop_ZT_Mean$ZT < 12]),mean(Loop_ZT_Mean$Temp_Mean[Loop_ZT_Mean$ZT >= 12]))
  
  Phase_acti_df = data.frame(Phase_acti,Acti,Temp,count_phase, Wake_event_sum_phase_av)
  names(Phase_acti_df)= c("Phase","Activity","Temperature","Wake_event_mean","Wake_event_sum")
  
  addWorksheet(wb2, "Phase-Activity")
  writeData(wb2, "Phase-Activity", x= Phase_acti_df)

#### Output file #####
base_folder = getwd()

if (dir.exists("output")==FALSE){
  dir.create("output")
}
setwd("output")

splits=unlist(data.table::tstrsplit(file_path,"/", fill=NA, type.convert=FALSE, names=F))

exp_name = splits[2]
file_name = paste0(splits[3],"_",splits[2])
if (dir.exists(exp_name)==F){
  dir.create(exp_name)
}

setwd(exp_name)

ID = gsub("/","_", file_path)
filename = paste0(file_name,suffix,".xlsx")
saveWorkbook(wb2 , file = filename, overwrite = T)


setwd(base_folder) # Restore main folder
#setwd("C:/Users/hcall/OneDrive/Documents/R/Sleep")
#setwd("E:/R/Sleep")

print(paste0("Successful Analysis of ",ID,suffix))
}


Sleep_select = function(criteria = "csv"){
  print(date())
  crit = as.character(criteria)
  
  crit2=c()
  for (b in crit){
         if (is_empty(crit2)){
               crit2 = paste0(b)
           } else {crit2=paste0(crit2,".+",b)
           }
  }

  base_folder = getwd()
  setwd("input")
  a_unique = unique(dirname(list.files(pattern = crit2, recursive = T, ignore.case = T))) #Change the pattern to choose more precisely an experiment, a mouse ID, etc.
                                                                                          # To use multiple arguments, use pattern = "arg1.+arg2+arg3"
                                                                                          # WARNING: Arguments should be in the same order as in the file name.
                                                                                          # It can take a while if there is a lot of recordings to analyze
  setwd(base_folder)
  if (!is_empty(a_unique)){
    for (a in a_unique){
      a2 = paste0("input/",a)
      Sleep_preparation(a2
                     #,graphs_
                     )
    }
  } else {
    print("No corresponding file found. Check order of arguments?")
    print("Remember that it has to match the file name, not the folder name")
  }
  print(date())
}
