### Probably not all package necessary


if(!require(tidyr)){install.packages("tidyr")}
if(!require(tidyr)){install.packages("dplyr")}
if(!require(openxlsx)){install.packages("openxlsx")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(lubridate)){install.packages("lubridate")}
library(gtools)

### Data import and preparation

filename = c('test_sleep_transition') ### for the output
raw_data = read.csv("E:/R/Sleep/input/2025-06-16_Normal-Sleep/1238359/6-17-2025_1238359-NS.csv") ### raw_data should be the output from the scoring package, combined into one dataframe
raw_data$rodent_sleep_cluster <- raw_data$rodent_sleep
raw_data$rodent_sleep <- ifelse(raw_data$rodent_sleep %in% c("W", "P"), 
                                raw_data$rodent_sleep, "S")
raw_data <- raw_data %>%
  tidyr::separate(time_stamp, into = c("date", "hour"), sep = " ") %>%
  dplyr::arrange(date, hour, idx)

### Create the workbook that will be saved as .xlsx
wb2 = createWorkbook()
addWorksheet(wb2, "raw_data")
writeData(wb2, sheet="raw_data", x=raw_data)


### Sleep transitions
States = c("W","P","1","2","3","4","5","6")
States_name = c("W","P","c1","c2","c3","c4","c5","c6")

matrix_trans = matrix(0,8,8)
matrix_trans_light = matrix(0,8,8)
matrix_trans_dark = matrix(0,8,8)
colnames(matrix_trans) = States_name
rownames(matrix_trans) = States_name
colnames(matrix_trans_light) = States_name
rownames(matrix_trans_light) = States_name
colnames(matrix_trans_dark) = States_name
rownames(matrix_trans_dark) = States_name

for (i in 1:length(States)){
  for (j in 1:length(States)){
    temp_light = nrow(raw_data[raw_data$rodent_sleep_cluster == States[i] & lag(raw_data$rodent_sleep_cluster, n = 1) == States[j] 
                               & raw_data$ZT < 12,] )
    temp_dark = nrow(raw_data[raw_data$rodent_sleep_cluster == States[i] & lag(raw_data$rodent_sleep_cluster, n = 1) == States[j] 
                              & raw_data$ZT >= 12,] )
    temp = nrow(raw_data[raw_data$rodent_sleep_cluster == States[i] & lag(raw_data$rodent_sleep_cluster, n = 1) == States[j], ])
    
    matrix_trans_light[i,j] = temp_light
    matrix_trans_dark[i,j] = temp_dark
    matrix_trans[i,j] = temp
  }
}
# heatmap(matrix_trans, Rowv = NA, Colv = NA)
# pheatmap(matrix_trans, display_numbers = T, scale = "column", order = NA, cluster_rows = F, cluster_cols = F)

matrix2 = as.data.frame(matrix_trans)

matrix2 = matrix2 %>% mutate(
  W_percent = W / sum(W) * 100,
  P_percent = P / sum(P) * 100,
  c1_percent = c1 / sum(c1) * 100,
  c2_percent = c2 / sum(c2) * 100,
  c3_percent = c3 / sum(c3) * 100,
  c4_percent = c4 / sum(c4) * 100,
  c5_percent = c5 / sum(c5) * 100,
  c6_percent = c6 / sum(c6) * 100,
)

matrix3 = data.frame(States_name,matrix2)

###

matrix2_light = as.data.frame(matrix_trans_light)

matrix2_light = matrix2_light %>% mutate(
  W_percent = W / sum(W) * 100,
  P_percent = P / sum(P) * 100,
  c1_percent = c1 / sum(c1) * 100,
  c2_percent = c2 / sum(c2) * 100,
  c3_percent = c3 / sum(c3) * 100,
  c4_percent = c4 / sum(c4) * 100,
  c5_percent = c5 / sum(c5) * 100,
  c6_percent = c6 / sum(c6) * 100,
)

matrix3_light = data.frame(States_name,matrix2_light)

###

matrix2_dark = as.data.frame(matrix_trans_dark)

matrix2_dark = matrix2_dark %>% mutate(
  W_percent = W / sum(W) * 100,
  P_percent = P / sum(P) * 100,
  c1_percent = c1 / sum(c1) * 100,
  c2_percent = c2 / sum(c2) * 100,
  c3_percent = c3 / sum(c3) * 100,
  c4_percent = c4 / sum(c4) * 100,
  c5_percent = c5 / sum(c5) * 100,
  c6_percent = c6 / sum(c6) * 100,
)

matrix3_dark = data.frame(States_name,matrix2_dark)

addWorksheet(wb2, "Sleep-transition-all")
writeData(wb2, sheet = "Sleep-transition-all" , x=matrix3)

addWorksheet(wb2, "Sleep-transition-light")
writeData(wb2, sheet = "Sleep-transition-light" , x=matrix3_light)

addWorksheet(wb2, "Sleep-transition-dark")
writeData(wb2, sheet = "Sleep-transition-dark" , x=matrix3_dark)



filename = paste0(filename,".xlsx")
saveWorkbook(wb2 , file = filename, overwrite = T) #### Carefull ! It will overwrite the previous file if you don't change the filename
