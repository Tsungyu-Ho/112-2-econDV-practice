library(sf)
detach("package:sf", unload=TRUE)
library(tidyverse)
library(dplyr)
library(sf)
library(lwgeom)
st_drivers() |> View() #串接到大寫的View

# 畫台灣地圖----
# 設定 shapefile 的路徑
shp_file_path <- 'C:\\D-disk\\Tsung-yu\\ma_1\\econDV\\112-2-econDV-practice\\week 10\\mapdata202301070205\\COUNTY_MOI_1090820.shp'

# 讀取 shapefile
sf_data_taiwan <- st_read(shp_file_path)
class(sf_data_taiwan) # 包含sf(simple feature)以及data.frame
# 查看讀取結果
glimpse(sf_data_taiwan)

# 使用 st_simplify 函數來簡化 simple feature
sf_data_simplified_taiwan <- st_simplify(sf_data_taiwan, preserveTopology = TRUE, dTolerance = 1)
# 查看簡化後結果
glimpse(sf_data_simplified_taiwan)

# 獲取 bounding box
bbox <- st_bbox(sf_data_taiwan)

# 調整 bounding box
# bbox['xmin'] <- your_value
bbox['ymin'] <- 21
# bbox['xmax'] <- your_value
# bbox['ymax'] <- your_value

# 將調整後的 bbox 應用回原物件
sf_data_adjusted <- st_crop(sf_data_taiwan, bbox)

# 查看調整後結果
glimpse(sf_data_adjusted)

# 畫圖
ggplot()+
  geom_sf(
    data = sf_data_adjusted
  )

#畫捷運圖----
# 設定 shapefile 的路徑
shp_file_path <- 'C:\\D-disk\\Tsung-yu\\ma_1\\econDV\\112-2-econDV-practice\\week 10\\捷運\\MRT_1130216\\MRT_1130216.shp'

# 讀取 shapefile
sf_data <- st_read(shp_file_path)
class(sf_data) # 包含sf(simple feature)以及data.frame
# 查看讀取結果
glimpse(sf_data)
object.size(sf_data)

sf_data_simplified <- st_simplify(sf_data, preserveTopology = TRUE, dTolerance = 1)
# 查看簡化後結果
glimpse(sf_data_simplified)

# 獲取 bounding box
bbox <- st_bbox(sf_data)

# 調整 bounding box
# bbox['xmin'] <- your_value
bbox['ymin'] <- 21
# bbox['xmax'] <- your_value
# bbox['ymax'] <- 21

# 將調整後的 bbox 應用回原物件
sf_data_adjusted_mrt <- st_crop(sf_data, bbox)

# 畫全台地圖和捷運圖
ggplot()+
  geom_sf(
    data = sf_data_adjusted
  )+
  geom_sf(
    data = sf_data,color = "red"
  )
# 畫台北新北桃園基隆地圖，並修改緯度----
library(dplyr)
sf_data_filtered <- sf_data_adjusted %>%
  filter(COUNTYNAME %in% c('新北市', '基隆市', '臺北市', '桃園市'))

# 獲取 bounding box
bbox <- st_bbox(sf_data_filtered)

# 調整 bounding box
# bbox['xmin'] <- your_value
bbox['ymin'] <- 21
# bbox['xmax'] <- your_value
bbox['ymax'] <- 25.4

# 將調整後的 bbox 應用回原物件
sf_data_filtered <- st_crop(sf_data_filtered, bbox)

# 查看篩選後的資料
glimpse(sf_data_filtered)
# 畫圖
ggplot()+
  geom_sf(
    data = sf_data_filtered,
    #aes(fill = "red", color = "blue")
  )
# 挑選出新北桃園台北捷運----
sf_data_filtered_mrt <- sf_data_adjusted_mrt %>%
  filter(MRTSYS %in% c('臺北捷運', '新北捷運', '臺灣桃園國際機場捷運'))
sf_data_adjusted_mrt$MRTCODE <- as.factor(sf_data_adjusted_mrt$MRTCODE)


# 畫出北部捷運圖----
sf_data_filtered_mrt <- sf_data_filtered_mrt[!is.na(sf_data_filtered_mrt$MRTCODE), ]

ggplot() +
  geom_sf(
    data = sf_data_filtered
  ) +
  geom_sf(
    data = sf_data_filtered_mrt, aes(col = MRTCODE),linewidth = 0.8
  )+
  scale_color_manual(
    values = c('三鶯線' = '#79bce8',
               '小碧潭線' = '#cfdb00',
               '中和新蘆線' = '#f8b61c',
               '文湖線' = '#c48c31',
               '松山新店線' = '#008659',
               '板南線' = '#0070bd',
               '淡水信義線' = '#e3002c',
               '新北投線' = '#fd92a3',
               '機場捷運' = '#8246AF',
               '貓空纜車' = '#77bc1f',
               '環狀線' = '#ffdb00'),
    na.value = "transparent") + # set NA values to transparent or the same as your plot background
  labs(color = "捷運線路") # rename the color legend






