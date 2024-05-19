library(ggplot2)
library(tidyverse)

# Create some sample data
data <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(10, 15, 13, 17, 20)
)

# #要先複製到剪貼簿，input$size 這個為設定控制面板的標籤
# Create the plot
plot <- ggplot(data, aes(x = x, y = y)) +
  geom_point(
    size = 1, #input$size
    color= "red" #input$color
  ) +
  labs(
    title = "Example Point Plot",
    subtitle = "This is a subtitle",
    caption = "Footer text"
  ) +
  theme(
    plot.caption = element_text(size = 12 #input$footer
    ),
    plot.title =  element_text(size=13 #input$title
    ))  

# Display the plot
print(plot)


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
  filter(COUNTYNAME %in% c("新北市", "臺北市", "基隆市"))# 
# 獲取 bounding box
bbox <- st_bbox(sf_data_simplified_taipei_newtaipei)
# 調整 bounding box
bbox['xmin'] <- 121.28269
bbox['ymin'] <- 24.67319
bbox['xmax'] <- 122.5
bbox['ymax'] <- 25.4

# 將調整後的 bbox 應用回原物件
sf_data_simplified_taipei_newtaipei <- st_crop(sf_data_simplified_taipei_newtaipei, bbox)
glimpse(sf_data_taipei_newtaipei)
ggplot()+
  geom_sf(data = sf_data_simplified_taipei_newtaipei)

# 使用 st_simplify 函數來簡化 simple feature
# sf_data_simplified_taipei_newtaipei <- st_simplify(sf_data_taipei_newtaipei, preserveTopology = TRUE, dTolerance = 1)
# 查看簡化後結果
# glimpse(sf_data_simplified_taipei_newtaipei)

election2020 = jsonlite::fromJSON(
  "https://www.dropbox.com/s/a3torx0p41hheb6/presidentElection2020.json?dl=1"
)

mp <- econDV2::Map()
mp$sf$get_sf_taiwan_simplified() -> list_sf_taiwan
list_sf_taiwan$鄉鎮區 -> tw_township


# Calculate support rate based on election2020 data frame -----
library(tidyverse)

# Assuming election2020 is the name of your data frame
election2020 <- election2020 %>%
  mutate(支持率 = if_else(`(2)\n 韓國瑜\n 張善政` > `(3)\n 蔡英文\n 賴清德`, -(`(2)\n 韓國瑜\n 張善政`), `(3)\n 蔡英文\n 賴清德`))

# Showing first 3 rows of the updated data frame
glimpse(election2020 %>% slice_head(n = 3))

# 將數據與地理資訊合併
map_data <- left_join(sf_data_simplified_taipei_newtaipei, election2020, by = c("TOWNNAME" = "鄉(鎮、市、區)別"))

# 繪製地圖
ggplot() +
  geom_sf(data = map_data, aes(fill = 支持率, alpha = COUNTYNAME)) +
  scale_fill_gradient2(low = "#000095", mid = "white", high = "#1B9431", midpoint = 0, limits = c(-1, 1)) +
  scale_alpha_manual(values = c(臺北市 = 0.5, 基隆市 = 0.5), guide = 'none') 

  