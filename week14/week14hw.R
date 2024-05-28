library(sf)
detach("package:sf", unload=TRUE)
library(tidyverse)
library(dplyr)
library(sf)
library(lwgeom)
library(palmerpenguins)
library(purrr)
library(gganimate)
library(animation)
library(tidyr)
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

# 獲取 bounding box
bbox <- st_bbox(sf_data_simplified_taipei_newtaipei)
# 調整 bounding box
bbox['xmin'] <- 121.28269
bbox['ymin'] <- 24.67319 # 24.67319
bbox['xmax'] <- 122.10915
bbox['ymax'] <- 25.4 #25.4

# 將調整後的 bbox 應用回原物件
sf_data_simplified_taipei_newtaipei <- st_crop(sf_data_simplified_taipei_newtaipei, bbox)


# 匯入價格資料
unit_price_data <- read.csv("C:\\D-disk\\Tsung-yu\\綠建築論文\\stata\\dta\\mean_unit_price_by_year_dist.csv")
glimpse(unit_price_data)

# 重命名 sf_data_simplified_taipei_newtaipei 李的 TOWNNAME 欄位為 dist
sf_data_simplified_taipei_newtaipei <- rename(sf_data_simplified_taipei_newtaipei, dist = TOWNNAME)

# 確認兩個數據集應該可以被合併了
merged_data <- sf_data_simplified_taipei_newtaipei %>%
  left_join(unit_price_data, by = "dist")

ggplot()+
  geom_sf(data = sf_data_simplified_taipei_newtaipei, color = "white", size = 0.3)

# 繪製動態熱力圖
ggplot(merged_data, aes(frame = year), color = "white", size = 0.3) +
  geom_sf(aes(fill = mean_price)) +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal() +
  transition_time(year) +
  labs(title = "Year: {frame_time}", fill = "Mean Price")

# 繪製動態地圖
p = ggplot(merged_data, aes(frame = year, fill = mean_price)) + # 使用year作為時間軸, mean_price作為熱力圖的填充
  geom_sf(color = "white", size = 0.3) + # 繪製地理圖形
  scale_fill_viridis_c(option = "magma") + # 使用母岩色彩做為填充尺度
  labs(title = "Year: {frame_time}") + # 圖表標題會隨時間變動
  theme_minimal() + # 使用最小主題
  transition_time(year) # 指定時間軸

# 輸出動畫
animate(p, nframes = 100, fps = 10, end_pause = 30, output = 'C:\\D-disk\\Tsung-yu\\ma_1\\econDV\\112-2-econDV-practice\\week14\\animation.gif')


# 填補資料
complete_data <- merged_data %>%
  complete(dist, year, fill = list(mean_price = NA)) # 對每個行政區和每一年進行填補，若缺失則使用NA

# 繪製動態地圖
p = ggplot(merged_data, aes(group = dist, frame = year, fill = mean_price)) + 
  geom_sf(data = merged_data, aes(), color = "white", size = 0.3) + 
  scale_fill_viridis_c(option = "magma", na.value = "lightgray") +  # 使用scale_fill_viridis_c并设定na.value为"lightgray"
  labs(
    title = "{frame_time}年各行政區綠建築每坪價格變化", 
    caption = "資料來源:本研究整理" )  + 
  theme_minimal() + 
  transition_time(year)
p


# 輸出動畫
animate(p, nframes = 100, fps = 10, end_pause = 30, output = 'C:\\D-disk\\Tsung-yu\\ma_1\\econDV\\112-2-econDV-practice\\week14\\animation.gif')
anim_save("animation.gif", animation = p, nframes = 100, fps = 10, end_pause = 30 )



# 輸出動畫
animate(p, nframes = 100, fps = 10, end_pause = 30, output = 'animation.gif')


# 繪製動態地圖
p = ggplot(merged_data, aes(frame = year, fill = mean_price)) + 
  geom_sf(data = merged_data, aes(), color = "white", size = 0.3) + 
  scale_fill_viridis_c(option = "magma", na.value = "lightgray") +  # 使用scale_fill_viridis_c并设定na.value为"lightgray"
  labs(
    title = "104~112年各行政區綠建築每坪價格變化",  # 添加主標題
    caption = "資料來源:本研究整理"  # 添加字幕
  ) +
  theme_minimal() + 
  transition_time(year) 









saveGIF({
  for(i in 1:100){
    img.name <- sprintf("./gganim_plot%04d.png", i)
    img <- png::readPNG(img.name)
    grid::grid.raster(img)
    animation::ani.pause()}
})