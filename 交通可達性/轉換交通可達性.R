library(sf)
library(dplyr)
library(readr)
library(tidyverse)

# 讀取數據
mrt_stations <- read.csv('C:\\D-disk\\Tsung-yu\\綠建築論文\\stata\\rawdata\\臺北捷運車站出入口座標.csv')
transaction_info <- read.csv('C:\\D-disk\\Tsung-yu\\綠建築論文\\stata\\workdata\\所有建築交通可達性地址.csv')

# 使用座標資訊將您的資料轉為 sf 物件
transaction_info <- st_as_sf(transaction_info, coords = c("經度", "緯度"), crs = 4326)
mrt_stations <- st_as_sf(mrt_stations, coords = c("經度", "緯度"), crs = 4326)
# 檢查NA----
transaction_info <- transaction_info|>
  mutate(
    緯度=as.numeric(緯度),
    經度=as.numeric(經度)
  ) 

transaction_info$緯度 |> is.na() |> which()
transaction_info[19759,]
transaction_info$緯度 |> is.na() -> na
na <- as.data.frame(na)
count_num = 0
for(i in na$na){
  if(i == TRUE) { 
    count_num = count_num + 1 
  }
}

geometry_373 <- head(mrt_stations$geometry, 373)
stations_373 <- head(mrt_stations$X104年, 373)
mrt_stations_104_108 <- st_as_sf(data.frame('捷運站出入口' = stations_373, 'geometry' = geometry_373))
mrt_stations_109_112 <- st_as_sf(data_frame('捷運站出入口' = mrt_stations$出入口名稱, 'geometry' = mrt_stations$geometry))
mrt_stations_104_108 <- st_as_sf(tibble('捷運站出入口' = stations_373, 'geometry' = geometry_373))
mrt_stations_109_112 <- st_as_sf(tibble('捷運站出入口' = mrt_stations$出入口名稱, 'geometry' = mrt_stations$geometry))
#建立緩衝區
buffer_104_108 <- st_buffer(mrt_stations_104_108, dist=500)
buffer_109_112 <- st_buffer(mrt_stations_109_112, dist=500)

#新增“交通可達性”欄位
transaction_info <- transaction_info %>% 
  mutate(交通可達性 = case_when(
    交易年 %in% 104:108 & map_lgl(geometry, ~any(lengths(st_within(.x, buffer_104_108)) > 0)) ~ 1,
    交易年 %in% 109:112 & map_lgl(geometry, ~any(lengths(st_within(.x, buffer_109_112)) > 0)) ~ 1,
    TRUE ~ 0))

library(foreach)
library(doParallel)

# set up the parallel backend
cl <- makeCluster(detectCores())
registerDoParallel(cl)

transaction_info <- transaction_info %>% 
  mutate(交通可達性 = foreach(i = 1:nrow(transaction_info), .combine='cbind') %dopar% {
    if(交易年[i] %in% 104:108 & any(lengths(st_within( geometry[i], buffer_104_108)) > 0)) {
      return(1)
    } else if (交易年[i] %in% 109:112 & any(lengths(st_within( geometry[i], buffer_109_112)) > 0)) {
      return(1)
    } else {
      return(0)
    }
  })
# 將更新後的數據框寫入一個新的CSV檔案
write.csv(transaction_info, file = 'C:\\D-disk\\Tsung-yu\\綠建築論文\\stata\\workdata\\成功更新所有建築交通可達性地址.csv')
# stop the parallel backend
stopCluster(cl)

