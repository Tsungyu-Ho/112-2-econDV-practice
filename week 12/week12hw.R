library(sf)
detach("package:sf", unload=TRUE)
library(tidyverse)
library(dplyr)
library(sf)
library(lwgeom)
library(palmerpenguins)
library(purrr)
# library(rgdal)
library(ggmap)

penguins <- penguins %>% 
  drop_na()
# st_drivers() |> View() #串接到大寫的View

# 畫台灣地圖----
# 設定 shapefile 的路徑
shp_file_path <- 'C:\\D-disk\\Tsung-yu\\ma_1\\econDV\\112-2-econDV-practice\\week 12\\鄉鎮市區界線(TWD97經緯度)\\TOWN_MOI_1120317.shp'

# 讀取 shapefile
sf_data_taiwan <- st_read(shp_file_path)
class(sf_data_taiwan) # 包含sf(simple feature)以及data.frame
# 查看讀取結果
glimpse(sf_data_taiwan)

# 篩選雙北----
sf_data_taipei_newtaipei <- sf_data_taiwan %>%
  filter(COUNTYNAME %in% c("新北市", "臺北市", "基隆市"))

glimpse(sf_data_taipei_newtaipei)

# 使用 st_simplify 函數來簡化 simple feature
sf_data_simplified_taipei_newtaipei <- st_simplify(sf_data_taipei_newtaipei, preserveTopology = TRUE, dTolerance = 1)
# 查看簡化後結果
glimpse(sf_data_simplified_taipei_newtaipei)

# 畫捷運路線
shp_file_path <- 'C:\\D-disk\\Tsung-yu\\ma_1\\econDV\\112-2-econDV-practice\\week 10\\捷運\\MRT_1130216\\MRT_1130216.shp'

# 讀取 shapefile
sf_data_MRT <- st_read(shp_file_path)
class(sf_data_MRT) # 包含sf(simple feature)以及data.frame

# 查看簡化後結果
sf_data_simplified_MRT <- st_simplify(sf_data_MRT, preserveTopology = TRUE, dTolerance = 1)
# 挑選出新北台北捷運----
sf_data_filtered_mrt <- sf_data_simplified_MRT %>%
  filter(MRTSYS %in% c('臺北捷運', '新北捷運'))
sf_data_simplified_MRT$MRTCODE <- as.factor(sf_data_simplified_MRT$MRTCODE)


# 獲取 bounding box
bbox <- st_bbox(sf_data_simplified_taipei_newtaipei)
# 調整 bounding box
bbox['xmin'] <- 121.28269
bbox['ymin'] <- 24.9 # 24.67319
bbox['xmax'] <- 121.7
bbox['ymax'] <- 25.22 #25.4

# 將調整後的 bbox 應用回原物件
sf_data_simplified_taipei_newtaipei <- st_crop(sf_data_simplified_taipei_newtaipei, bbox)

# 畫出北部捷運圖----
sf_data_filtered_mrt <- sf_data_filtered_mrt[!is.na(sf_data_filtered_mrt$MRTCODE), ]

# 匯入綠建築
shp_file_path <- "C:\\D-disk\\Tsung-yu\\綠建築論文\\QGIS\\QGIS_paper\\GB_community.shp"

# 指明編碼
options(encoding = "UTF-8")

# 讀取 shapefile
sf_data_GB <- st_read(shp_file_path)

# 生成500公尺的緩衝區
sf_data_GB_buffer <- st_buffer(sf_data_GB, dist = 500)
# st_buffer(sf_data_GB, dist = units::set_units(500, "meters"))

# 找出每個绿建筑500米缓冲区内是否有捷运线路
# 檢查當前座標系統
st_crs(sf_data_filtered_mrt)
st_crs(sf_data_GB_buffer)

# 如果座標系統不一致，將 sf_data_filtered_mrt 轉換至 sf_data_GB_buffer 的座標系統
sf_data_filtered_mrt <- st_transform(sf_data_filtered_mrt, st_crs(sf_data_GB_buffer))


intersect_mrt <- st_intersects(sf_data_filtered_mrt, sf_data_GB_buffer)
# 使用apply函數將每個綠建築的緩沖區是否與捷運線路交集的結果匯總為一個邏輯向量
has_mrt <- apply(intersect_mrt, MARGIN = 2, function(x) any(x))
# has_mrt <- factor(sf_data_GB_buffer$has_mrt, levels = c("TRUE", "FALSE"))

# 畫圖
ggplot()+
  geom_sf(data = sf_data_simplified_taipei_newtaipei)+
  geom_sf(data = sf_data_filtered_mrt, aes(col = MRTCODE), linewidth = 0.8)+
  scale_color_manual(values = c('三鶯線' = '#79bce8',
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
                     na.value = "transparent") + 
  labs(color = "捷運線路") + 
  geom_sf(data = sf_data_GB, color = "green" , size = 0.5)


# 畫圖
ggplot() +
  geom_sf(data = sf_data_simplified_taipei_newtaipei) +
  geom_sf(data = sf_data_filtered_mrt, aes(col = MRTCODE), linewidth = 0.8) +
  scale_color_manual(values = c('三鶯線' = '#79bce8',
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
                     na.value = "transparent") + 
  labs(color = "捷運線路") + 
  geom_sf(data = sf_data_GB, color = "green", size = 0.5) +
  geom_sf(data = sf_data_GB_buffer, aes(fill = has_mrt), color = NA) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"), 
                    #labels = c("有捷運", "無捷運"),
                    name = "500公尺內是否有捷運") +
  geom_sf(data = sf_data_GB, color = "green", size = 0.5)

ggplot() +
  geom_sf(data = sf_data_simplified_taipei_newtaipei) +
  geom_sf(data = sf_data_filtered_mrt, aes(col = MRTCODE), linewidth = 0.8) +
  scale_color_manual(values = c('三鶯線' = '#79bce8',
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
                     na.value = "transparent") + 
  labs(color = "捷運線路") + 
  geom_sf(data = sf_data_GB, color = "green", size = 0.5) +
  geom_sf(data = sf_data_GB_buffer, aes(fill = has_mrt), color = NA) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"), 
                    #labels = c("有捷運", "無捷運"),
                    name = "500公尺內是否有捷運") +
  geom_sf(data = sf_data_GB, color = "green", size = 0.5) +
  ggtitle("新北市綠建築的交通可達性") +
  labs(caption = "資料來源: 本研究自行整理")

ggplot() +
  geom_sf(data = sf_data_simplified_taipei_newtaipei) +
  geom_sf(data = sf_data_filtered_mrt, aes(col = MRTCODE), linewidth = 0.8) +
  scale_color_manual(values = c('三鶯線' = '#79bce8',
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
                     na.value = "transparent") + 
  labs(color = "捷運線路") + 
  geom_sf(data = sf_data_GB_buffer, aes(fill = has_mrt), color = NA) +
  scale_fill_manual(values = c("TRUE" = rgb(0, 0, 1, 0.6), 
                               "FALSE" = rgb(1, 0, 0, 0.6)), 
                    name = "500公尺內是否有捷運") +
  geom_sf(data = sf_data_GB, color = "green", size = 0.5) +
  ggtitle("新北市綠建築的交通可達性") +
  labs(caption = "資料來源: 財團法人台灣建築中心、開放資料 - 國土測繪圖資e商城、本研究自行整理") +
  xlab("經度") + 
  ylab("緯度") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(angle = 360, vjust = 0.5))


ggplot() +
  geom_sf(data = sf_data_simplified_taipei_newtaipei, color = "white", size = 0.3)+
  geom_sf(data = sf_data_filtered_mrt, aes(col = MRTCODE), linewidth = 0.9) +
  scale_color_manual(values = c('三鶯線' = '#79bce8',
                                '小碧潭線' = '#cfdb00',
                                '中和新蘆線' = '#f8b61c',
                                '文湖線' = '#c48c31',
                                '松山新店線' = '#008659',
                                '板南線' = '#0070bd',
                                '淡水信義線' = '#e3002c',
                                '新北投線' = '#fd92a3',
                                '機場捷運' = '#8246AF',
                                '貓空纜車' = '#77bc1f',
                                '環狀線' = '#ffdb00',
                                'TRUE' = "blue", 'FALSE' = "red"), 
                     na.value = "transparent") +
  labs(color = "捷運線路 與 綠建築附近是否有捷運") + 
  geom_sf(data = sf_data_GB_buffer, aes(fill = has_mrt), color = NA) +
  scale_fill_manual(values = c("TRUE" = rgb(0, 0, 1, 0.2), 
                               "FALSE" = rgb(1, 0, 0, 0.2)), 
                    name = "綠建築方圓500公尺緩衝區") +
  geom_sf(data = sf_data_GB, aes(color = has_mrt), size = 0.5) +
  ggtitle("新北市綠建築的交通可達性") +
  labs(caption = "資料來源: 財團法人台灣建築中心、開放資料 - 國土測繪圖資e商城、本研究自行整理") +
  xlab("經度") + 
  ylab("緯度") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(angle = 360, vjust = 0.5))

register_stadiamaps("2f04c610-4c67-41e1-8c9f-278b151ecd4e", write = FALSE)
weekbbox <- c(left = 121.28269, bottom = 24.9, right = 121.7, top = 25.22)

#============ 以下是錯的
get_stadiamap(weekbbox, zoom = 10, maptype = "stamen_toner_lite") |> ggmap() -> ggmap

ggmap + 
  geom_sf(data = sf_data_simplified_taipei_newtaipei) +
  geom_sf(data = sf_data_filtered_mrt, aes(col = MRTCODE), linewidth = 0.9) +
  scale_color_manual(values = c('三鶯線' = '#79bce8',
                                '小碧潭線' = '#cfdb00',
                                '中和新蘆線' = '#f8b61c',
                                '文湖線' = '#c48c31',
                                '松山新店線' = '#008659',
                                '板南線' = '#0070bd',
                                '淡水信義線' = '#e3002c',
                                '新北投線' = '#fd92a3',
                                '機場捷運' = '#8246AF',
                                '貓空纜車' = '#77bc1f',
                                '環狀線' = '#ffdb00',
                                'TRUE' = "blue", 'FALSE' = "red"), 
                     na.value = "transparent") +
  labs(color = "捷運線路 與 綠建築附近是否有捷運") + 
  geom_sf(data = sf_data_GB_buffer, aes(fill = has_mrt), color = NA) +
  scale_fill_manual(values = c("TRUE" = rgb(0, 0, 1, 0.2), 
                               "FALSE" = rgb(1, 0, 0, 0.2)), 
                    name = "500公尺內是否有捷運") +
  geom_sf(data = sf_data_GB, aes(color = has_mrt), size = 0.5) +
  ggtitle("新北市綠建築的交通可達性") +
  labs(caption = "資料來源: 財團法人台灣建築中心、開放資料 - 國土測繪圖資e商城、本研究自行整理") +
  xlab("經度") + 
  ylab("緯度") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(angle = 360, vjust = 0.5))