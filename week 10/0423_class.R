scales::muted("red")
[1] "#832424"
scales::show_col(c("red", "#832424"))
## mute 灰階
# 務必記得確認sf是否為最新匯入的套件，不然filter會沒辦法轉成sf----
library(sf)
detach("package:sf", unload=TRUE)
library(tidyverse)
library(sf)
st_drivers() |> View() #串接到大寫的View

# 讀取shapefile -----
# 首先，我們需要安裝必要的套件

# 安裝 當 sf 尚未安裝，用以下程式安裝
# install.packages('sf', dependencies=TRUE)

# 載入套件
library(sf)
library(tidyverse)

# 設定 shapefile 的路徑
shp_file_path <- 'C:\\D-disk\\Tsung-yu\\ma_1\\econDV\\112-2-econDV-practice\\week 10\\mapdata202301070205\\COUNTY_MOI_1090820.shp'

# 讀取 shapefile
sf_data <- st_read(shp_file_path)
class(sf_data) # 包含sf(simple feature)以及data.frame
# 查看讀取結果
glimpse(sf_data)
# using driver `ESRI Shapefile'
# Simple feature collection with 22 features and 4 fields
# Geometry type: MULTIPOLYGON 多多邊體
# Dimension:     XY
# Bounding box:  xmin: 114.3593 ymin: 10.37135 xmax: 124.5611 ymax: 26.38528 經緯度
# Geodetic CRS:  TWD97#

# 獲取物件大小 -----
# 可以使用 object.size 函數來獲取 R 中物件的大小
object.size(sf_data)

# For geographical data, always use `geom_sf` for plotting unless impossible. When simplifying simple features, always use `st_simplify` with `preserveTopology = TRUE` and `dTolerance =1`.
# 如果simple feature object size太大或是圖畫超過一分鐘，dTolerance可以設定大一點

# 簡化 simple feature -----
# 使用 st_simplify 函數來簡化 simple feature
sf_data_simplified <- st_simplify(sf_data, preserveTopology = TRUE, dTolerance = 1)
# 查看簡化後結果
glimpse(sf_data_simplified)

# 畫圖
ggplot()+
  geom_sf(
    data = sf_data_simplified
  )

# 調整bounding box -----
# 可以使用 st_bbox 函數獲取物件的bounding box，並直接修改返回結果的 xmin, ymin, xmax, ymax 以調整 bounding box

# 獲取 bounding box
bbox <- st_bbox(sf_data)

# 調整 bounding box
# bbox['xmin'] <- your_value
bbox['ymin'] <- 21
# bbox['xmax'] <- your_value
# bbox['ymax'] <- your_value

# 將調整後的 bbox 應用回原物件
sf_data_adjusted <- st_crop(sf_data, bbox)

# 查看調整後結果
glimpse(sf_data_adjusted)

# 畫圖
ggplot()+
  geom_sf(
    data = sf_data_adjusted
  )

# 篩選 COUNTYNAME = '新北市' -----
# 可以使用 dplyr 套件的 filter 函數來篩選資料
library(dplyr)
sf_data_filtered <- sf_data_adjusted %>% filter(COUNTYNAME == '新北市')

# 查看篩選後的資料
glimpse(sf_data_filtered)
# 畫圖
ggplot()+
  geom_sf(
    data = sf_data_filtered,
    fill = "red", color = "blue", linewidth = 1.2
    #aes(fill = "red", color = "blue")
  )

# 分區----
# create a column ` zone` in ` sf_data_adjusted` whose value is either "north", "south", "east" and "west" randomly assigned.
# `sf_data_adjusted` is a sf, data frame class object. Plot it with filled color determined by `zone` column
# 建立 `zone` 欄位並隨機指派 'north', 'south', 'east', 'west' 中的一個給每個列 -----
# 可以使用 dplyr 的 mutate 函數來新增一個欄位，並使用 sample 函數來產生隨機的值

zones <- c('north', 'south', 'east', 'west')
sf_data_adjusted <- sf_data_adjusted %>% 
     mutate(zone = sample(zones, size = nrow(sf_data_adjusted), replace = TRUE))

# 查看新增欄位後的資料
glimpse(sf_data_adjusted)

# 畫圖
ggplot()+
  geom_sf(
    data = sf_data_adjusted,
    mapping = aes(fill = zone)
  )

