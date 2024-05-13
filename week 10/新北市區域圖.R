library(sf)
detach("package:sf", unload=TRUE)
library(tidyverse)
library(dplyr)
library(sf)
library(lwgeom)
st_drivers() |> View() #串接到大寫的View
library(rmapshaper)

# 畫台灣地圖----
# 設定 shapefile 的路徑
shp_file_path <- 'C:\\D-disk\\Tsung-yu\\ma_1\\econDV\\112-2-econDV-practice\\week 10\\新北市里界圖\\新北市里界圖\\新北市里界圖.shp'

# 讀取 shapefile
sf_data_newtaipei <- st_read(shp_file_path)
class(sf_data_newtaipei) # 包含sf(simple feature)以及data.frame
# 查看讀取結果
glimpse(sf_data_newtaipei)

# 使用 st_simplify 函數來簡化 simple feature
sf_data_newtaipei %>%
  rmapshaper::ms_simplify() -> sf_data_simplified_newtaipei
# 查看簡化後結果
glimpse(sf_data_simplified_newtaipei)

# 畫圖
ggplot()+
  geom_sf(
    data = sf_data_simplified_newtaipei
  )


# 轉換至地理座標系統
sf_data_simplified_newtaipei <- st_transform(sf_data_simplified_newtaipei, crs = 4326)
st_crs(sf_data_simplified_newtaipei)

# 畫圖
ggplot() + geom_sf(data = sf_data_simplified_newtaipei)

