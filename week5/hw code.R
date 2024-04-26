library(tidyverse)

# 定義每個學年的 CSV 下載按鈕對應的網址
csv_urls <- list(
  "104學年" = "https://stats.moe.gov.tw/files/ebook/native/104/104native_A1-1.csv",
  "105學年" = "https://stats.moe.gov.tw/files/ebook/native/105/105native_A1-1.csv",
  "106學年" = "https://stats.moe.gov.tw/files/ebook/native/106/106native_A1-1.csv",
  "107學年" = "https://stats.moe.gov.tw/files/ebook/native/107/107native_A1-1.csv",
  "108學年" = "https://stats.moe.gov.tw/files/ebook/native/108/108native_A1-1.csv",
  "109學年" = "https://stats.moe.gov.tw/files/ebook/native/109/109native_A1-1.csv",
  "110學年" = "https://stats.moe.gov.tw/files/ebook/native/110/110native_A1-1.csv",
  "111學年" = "https://stats.moe.gov.tw/files/ebook/native/111/111native_A1-1.csv",
  "112學年" = "https://stats.moe.gov.tw/files/ebook/native/112/112native_A1-1.csv"
)

# 將結果存入results中
results <- list(csv_urls = csv_urls)

## download and import ----
# 建立一個空的list來存儲下載的資料
data_list <- list()

# 循環下載每個CSV檔案並讀取到R中
for (year in names(csv_urls)) {
  # 下載CSV檔案
  download.file(csv_urls[[year]], paste0(year, ".csv"))
  
  # 讀取CSV檔案
  data <- read.csv(paste0(year, ".csv"))
  
  # 將讀取的資料存入data_list中
  data_list[[year]] <- data
}

# 將結果存儲在環境中已有的list物件results中
results$data_list <- data_list

library(purrr)
library(dplyr)

# 使用imap()函數向每個DataFrame添加"學年度"欄位
data_list <- imap(data_list, ~ mutate(.x, 學年度 = as.integer(gsub("\\D", "", .y))))

# 將結果存儲在環境中已有的list物件results中
results$data_list_with_year <- data_list

# merge ----
library(dplyr)

# 垂直合併data_list中的每個DataFrame
combined_data <- bind_rows(data_list)

# 將結果存儲在環境中已有的list物件results中
results$combined_data <- combined_data
# 1.
library(tidyverse)

# 先把學年度轉換成 x 軸可用的格式 (ex: "104學年度")
combined_data <- combined_data %>%
  mutate(學年度 = paste(學年度, '學年度'))

# 把數據從寬格式轉換成長格式
data_long <- combined_data %>%
  gather(key = "學制", value = "人數", 在學學生人數_博士班, 在學學生人數_碩士班, 在學學生人數_學士班) %>%
  group_by(學年度, 學制) %>%
  summarise(人數 = sum(人數, na.rm = TRUE)) %>% 
  ungroup()

# 使用自訂的顏色來指定每個學制的顏色
colors <- c("在學學生人數_博士班" = "red",
            "在學學生人數_碩士班" = "green",
            "在學學生人數_學士班" = "blue")
# 繪製直方圖
ggplot(data_long, aes(x = 學年度, y = 人數, fill = 學制)) +
  geom_bar(stat = "identity", position = "fill") +scale_fill_manual(values = colors)+
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "學年度", y = "比例", fill = "學制", title = "各學年各學制在學學生人數佔比") +
  theme_minimal()

# 2.
library(tidyverse)

# 假設 `combined_data` 為之前合併的數據框
combined_data <- results$combined_data

# 先把學年度轉換成 x 軸可用的格式 (ex: "104學年度")
combined_data <- combined_data %>%
  mutate(學年度 = paste(學年度, '學年度'))

# 把數據從寬格式轉換成長格式
data_long <- combined_data %>%
  gather(key = "學制", value = "人數", 在學學生人數_博士班, 在學學生人數_碩士班, 在學學生人數_學士班) %>%
  group_by(學年度, 學制) %>%
  summarise(人數 = sum(人數, na.rm = TRUE)) %>% 
  ungroup()

# 使用自訂的顏色來指定每個學制的顏色
colors <- c("在學學生人數_博士班" = "red",
            "在學學生人數_碩士班" = "green",
            "在學學生人數_學士班" = "blue")

# 繪製直方圖
ggplot(data_long, aes(x = 學年度, y = 人數, fill = 學制)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = colors) +
  labs(x = "學年度", y = "在學學生人數", fill = "學制", title = "各學年各學制在學學生人數") +
  theme_minimal()