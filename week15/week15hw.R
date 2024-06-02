library(dplyr)
library(tidyverse)
library(scales)

# Create vectors with the data
Year <- c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)
一般廢棄物 <- c(4579096, 4644478, 4563817, 4523994, 4371405, 4496108, 4535133, 4559218, 4441197, 4234971, 4204289, 4214871, 4192142, 4329863, 4271179, 5088471, 4781393, 4789190, 4651485, 4680888, 4439593.31, 4478371.061)
一般事業廢棄物 <- c(1111856, 1133412, 1273030, 1243659, 1449623, 1588097, 1648950, 1727384, 1965584, 2272792, 2302618, 2256895, 2228258, 2292207, 2170820, 1162725, 1682791, 1740889, 1820129, 1682560, 1762399.78, 1536950.239)
合計 <- c(5690952, 5777890, 5836847, 5767653, 5821028, 6084205, 6184083, 6286601, 6406781, 6507763, 6506907, 6471767, 6420400, 6622071, 6441999, 6251196, 6464184, 6530079, 6471613, 6363448, 6201993, 6015321)
一般事業廢棄物占比 <- c("19.5%", "19.6%", "21.8%", "21.6%", "24.9%", "26.1%", "26.7%", "27.5%", "30.7%", "34.9%", "35.4%", "34.9%", "34.7%", "34.6%", "33.7%", "18.6%", "26.0%", "26.7%", "28.1%", "26.4%", "28.4%", "25.6%")

# Create a data frame
waste_data <- data.frame(
  Year,
  一般廢棄物,
  一般事業廢棄物,
  合計,
  一般事業廢棄物占比
)

# 將"一般事業廢棄物占比"從字元轉為數值
waste_data <- waste_data %>%
  mutate(`一般事業廢棄物占比` = str_replace(`一般事業廢棄物占比`, "%", "") %>% 
           as.numeric() / 100)

# 繪製長條圖與折線圖
ggplot(waste_data, aes(x = Year)) +
  geom_bar(aes(y = `一般廢棄物` + `一般事業廢棄物`, fill = "一般廢棄物"), stat = "identity") +  # 指定圖例名稱
  geom_bar(aes(y = `一般事業廢棄物`, fill = "事業廢棄物"), stat = "identity") +  # 指定圖例名稱
  scale_fill_manual(values = c("一般廢棄物" = "skyblue", "事業廢棄物" = "blue"), name = "類型") +  # 定義圖例名稱和顏色
  geom_line(aes(y = `一般事業廢棄物占比` * (`一般廢棄物` + `一般事業廢棄物`)), group = 1, color = "red", size = 0.8) +  # 移除具名color，直接指定
  geom_text(aes(y = `一般事業廢棄物占比` * (`一般廢棄物` + `一般事業廢棄物`),
                label = scales::percent(一般事業廢棄物占比, accuracy = .1)), 
            vjust = -1.5, hjust = 0.5, color = "darkblue", size = 3.5) +
  geom_point(aes(y = `一般事業廢棄物占比` * (`一般廢棄物` + `一般事業廢棄物`)), color = "red", size = 2.5) +  # 移除具名color，直接指定
  scale_y_continuous(sec.axis = sec_axis(~ . / max(`一般廢棄物` + `一般事業廢棄物`) * 100, name = "百分比(%)"), name = "公噸數") +
  labs(title = "事業廢棄物占焚化廠處理量比例變化", caption = "資料來源: 環境部") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 360, vjust = 0.5),
        axis.title.y.right = element_text(angle = 360, vjust = 0.5),
        plot.title = element_text(hjust = 0.5))-> waste
waste





ggplot(waste_data, aes(x = Year)) +
  geom_bar(aes(y = `一般廢棄物` + `一般事業廢棄物`, fill = "總廢棄物"), stat = "identity") +  # 添加圖例名稱
  geom_bar(aes(y = `一般事業廢棄物`, fill = "事業廢棄物"), stat = "identity") +  # 添加圖例名稱
  scale_fill_manual(values = c("總廢棄物" = "skyblue", "事業廢棄物" = "blue"), name = "類型") +  # 定義圖例名稱和顏色
  geom_line(aes(y = `一般事業廢棄物占比` * (`一般廢棄物` + `一般事業廢棄物`), color = "占比線"), group = 1) +  # 添加圖例名稱
  scale_color_manual(values = c("占比線" = "red"), name = "趨勢") +  # 定義線圖圖例名稱和顏色
  geom_text(aes(y = `一般事業廢棄物占比` * (`一般廢棄物` + `一般事業廢棄物`),
                label = scales::percent(一般事業廢棄物占比, accuracy = .1)),
            vjust = -1.5, hjust = 0.5, color = "darkblue", size = 3.5) +
  geom_point(aes(y = `一般事業廢棄物占比` * (`一般廢棄物` + `一般事業廢棄物`), color = "占比點"), shape = 21) +  # 添加圖例名稱
  scale_color_manual(values = c("占比線" = "red", "占比點" = "red"), name = "趨勢") +  # 更新線圖和點圖圖例名稱和顏色
  scale_y_continuous(sec.axis = sec_axis(~ . / max(`一般廢棄物` + `一般事業廢棄物`) * 100, name = "百分比(%)"), name = "公噸數") +
  labs(title = "事業廢棄物占焚化廠處理量比例變化", caption = "資料來源: 環境部") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 360, vjust = 0.5),
        axis.title.y.right = element_text(angle = 360, vjust = 0.5), # 副Y軸標籤旋轉
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend("廢棄物類型"), color = guide_legend("趨勢線型"))   # 自定義圖例標籤 


