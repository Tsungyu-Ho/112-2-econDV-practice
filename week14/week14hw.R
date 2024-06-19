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
library(scales)
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

# 建立"年分"欄位，放入104、105到112年
years <- 104:112 
dist <- sf_data_simplified_taipei_newtaipei$TOWNNAME

# 使用tidyverse的expand_grid函數來產生每個區域和年份的組合
new_taipei_df <- expand_grid(dist, years) %>%
  # 更名欄位名稱為"縣市"與"年分"
  rename("縣市" = dist, "年分" = years)

# 因為視覺呈現用到的是df，所以使用glimplse
#藉由dplyr中的glimpse()函數取得資料框內容的快速概覽
glimpse(new_taipei_df)

# 左側合併兩個數據框並新增geometry欄位 -----
new_taipei_df <- new_taipei_df %>%
  left_join(sf_data_simplified_taipei_newtaipei, by = c("縣市" = "TOWNNAME"))


# 匯入價格資料
unit_price_data <- read.csv("C:\\D-disk\\Tsung-yu\\綠建築論文\\stata\\dta\\mean_unit_price_by_year_dist.csv")
glimpse(unit_price_data)

# 重命名 sf_data_simplified_taipei_newtaipei 李的 TOWNNAME 欄位為 dist
sf_data_simplified_taipei_newtaipei <- rename(sf_data_simplified_taipei_newtaipei, dist = TOWNNAME)

# 確認兩個數據集應該可以被合併了
merged_data <- new_taipei_df %>%
  left_join(unit_price_data, by = c("縣市" = "dist", "年分" = "year") )

# Remove unwanted columns from data -----
merged_data <- merged_data %>% 
  select(-c("TOWNID", "TOWNCODE", "TOWNENG", "COUNTYID", "COUNTYCODE")
         )
# Replace NA values with 0 -----
# merged_data$mean_price[is.na(merged_data$mean_price)] <- 0
# Rename column -----
names(merged_data)[names(merged_data) == "縣市"] <- "行政區"

# 建立一個變數用於顯示熱力圖的顏色或灰色
merged_data$價格區間 <- ifelse(merged_data$mean_price == 0, 0, merged_data$mean_price)

# 修改標題，並給軸添加標籤 -----
basic_map <- ggplot(merged_data) + 
  geom_sf(aes(geometry = geometry, fill = 價格區間, group = 行政區), color = "white") +
  scale_fill_gradientn(colors = c("lightgray", "yellow", "red"), 
                       values = rescale(c(0, 1, max(merged_data$mean_price, na.rm = TRUE))), 
                       breaks = seq(0, max(merged_data$mean_price, na.rm = TRUE), 5),  
                       labels = scales::comma 
  ) +
  theme_minimal() +
  theme(legend.key.size = unit(1, "cm"), axis.title.y = element_text(angle = 360, vjust = 0.5, margin = margin(r = 15))) +
  labs(
    title = "{frame_time}年新北市各行政區綠建築每坪價格變化",  # 修改此行
    x = "經度",
    y = "緯度",
    caption = "資料來源:本研究整理"    # 添加此行
  )

# 篩選年份為112的數據 -----
filtered_data <- merged_data %>% 
  filter(年分 == 112)

glimpse(filtered_data)

# 將行政區區分出來
basic_map <- ggplot(merged_data) + 
  geom_sf(aes(geometry = geometry, fill = 價格區間, group = 行政區), color = "white") +
  scale_fill_gradientn(colors = c("lightgray", "yellow", "red"), 
                       values = rescale(c(0, 1, max(merged_data$mean_price, na.rm = TRUE))), 
                       breaks = seq(0, max(merged_data$mean_price, na.rm = TRUE), 5),  
                       labels = scales::comma 
  ) +
  theme_minimal() +
  theme(legend.key.size = unit(1, "cm"), axis.title.y = element_text(angle = 360, vjust = 0.5, margin = margin(r = 15))) +
  labs(
    title = "{frame_time}年新北市各行政區綠建築每坪價格變化",  # 修改此行
    x = "經度",
    y = "緯度",
    caption = "資料來源:本研究整理"    # 添加此行
  ) + facet_grid(vars(merged_data$行政區) , scales = "free")
basic_map

# 將行政區區分出來
basic_map_filter <- ggplot(filtered_data) + 
  geom_sf(aes(geometry = geometry, fill = 價格區間, group = 行政區), color = "white") +
  scale_fill_gradientn(colors = c("lightgray", "yellow", "red"), 
                       values = rescale(c(0, 1, max(merged_data$mean_price, na.rm = TRUE))), 
                       breaks = seq(0, max(merged_data$mean_price, na.rm = TRUE), 5),  
                       labels = scales::comma 
  ) +
  theme_minimal() +
  theme(legend.key.size = unit(1, "cm"), axis.title.y = element_text(angle = 360, vjust = 0.5, margin = margin(r = 15))) +
  labs(
    title = "{frame_time}年新北市各行政區綠建築每坪價格變化",  # 修改此行
    x = "經度",
    y = "緯度",
    caption = "資料來源:本研究整理"    # 添加此行
  ) + facet_wrap(vars(merged_data$行政區) , scales = "free")
basic_map_filter

anim_map <- basic_map +
  transition_time(年分)

anim_map

animate(anim_map, nframes = 200, fps = 10, end_pause = 30, height = 600, width = 800, output = 'animation0604.gif')
anim_save("animation0604.gif", animation = anim_map, height = 600, width = 800, nframes = 200, fps = 10, end_pause = 30 )


saveGIF({
  for(i in 1:100){
    img.name <- sprintf("./gganim_plot%04d.png", i)
    img <- png::readPNG(img.name)
    grid::grid.raster(img)
    animation::ani.pause()}
})