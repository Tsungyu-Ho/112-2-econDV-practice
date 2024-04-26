# 載入需要的套件
library(tidyverse)
library(lubridate)

# 建立日期範圍
date_range <- seq(as.Date("1960-01-01"), as.Date("1962-02-01"), by = "month")

# 建立假設的匯率升值率數據
set.seed(123)
usd_rate <- runif(length(date_range), -0.03, 0.03)
jpy_rate <- runif(length(date_range), -0.03, 0.03)
gbp_rate <- runif(length(date_range), -0.03, 0.03)

# 建立資料框
df <- data.frame(
  Date = rep(date_range, 3),
  Country = factor(rep(c("美元", "日元", "英鎊"), each = length(date_range))),
  Rate = c(usd_rate, jpy_rate, gbp_rate)
)

# 使用ggplot2繪製折線圖
ggplot(df, aes(x = Date, y = Rate, color = Country)) +
  geom_line() +
  labs(x = "日期", y = "對台幣匯率升值率", color = "國家") +
  theme_minimal()


# 引入資料----
library(readr)
BP01M01 <- read_csv("BP01M01.csv")
View(exchangeRate_real)

# 轉換日期格式(老師)----
exchangeRate_real <- exchangeRate_real %>%
  tidyr::separate(期間, into = c("year", "month"), sep = "M", remove = FALSE) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d"))

head(exchangeRate_real)

library(dplyr)

# 計算新台幣對美元的匯率
ntd_usd_rate <- exchangeRate_real$`新台幣NTD/USD`

# 計算其他貨幣的匯率，對新台幣的匯率，並儲存到新的欄位
exchangeRate_real <- exchangeRate_real %>%
  mutate(`日圓` = ntd_usd_rate / `日圓JPY/USD`,
         `英鎊` = ntd_usd_rate / (1/`英鎊USD/GBP`),
         `港幣` = ntd_usd_rate / `港幣HKD/USD`,
         `韓元` = ntd_usd_rate / `韓元KRW/USD`,
         `美元` = ntd_usd_rate) %>%
  select(date,`日圓`:`美元`)



# 計算匯率成長率
exchangeRate_real <- exchangeRate_real %>%
  arrange(期間) %>%
  mutate(across(c("日圓", "英鎊", "港幣", "韓元", "美元"), 
                ~( . - lag(.) ) / lag(.), 
                .names = "成長率_{.col}"))

head(exchangeRate_real)
