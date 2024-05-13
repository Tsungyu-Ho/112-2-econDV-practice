library(sf)
library(dplyr)

# 讀取數據
mrt_stations <- read.csv('C:\\D-disk\\Tsung-yu\\ma_1\\econDV\\112-2-econDV-practice\\交通可達性\\臺北捷運車站出入口座標.csv')
green_buildings <- read.csv('C:\\D-disk\\Tsung-yu\\ma_1\\econDV\\112-2-econDV-practice\\交通可達性\\綠建築位置.csv')

# 將座標轉換為空間對象
mrt_stations_sf <- st_as_sf(mrt_stations, coords = c('經度', '緯度'), crs = 4326) 
green_buildings_sf <- st_as_sf(green_buildings, coords = c('Longitude', 'Latitude'), crs = 4326)

# 為每個捷運站創建一個500公尺的緩衝區
buffer_mrt <- st_buffer(mrt_stations_sf, dist = 500)

# 判斷每個綠建築是否在緩衝區內
green_buildings_in_buffer <- st_intersects(green_buildings_sf, buffer_mrt)

# 在綠建築的數據框中創建一個新的欄位表示交通可達性
green_buildings_sf$交通可達性 <- sapply(1:length(green_buildings_in_buffer), function(i) ifelse(length(green_buildings_in_buffer[[i]]) >0, 1, 0))

# 將空間對象轉回數據框
green_buildings_df <- as.data.frame(green_buildings_sf)

# 將更新後的數據框寫入一個新的CSV檔案
write.csv(green_buildings_df, 'C:\\D-disk\\Tsung-yu\\ma_1\\econDV\\112-2-econDV-practice\\交通可達性\\Updated_Green_Buildings.csv')

# 列印數據
head(green_buildings_df)

# 畫圖
ggplot()+
  geom_sf(
    data = buffer_mrt
  )+
  geom_sf(
    data = mrt_stations_sf
  )+
  geom_sf(
    data = green_buildings_sf
  )