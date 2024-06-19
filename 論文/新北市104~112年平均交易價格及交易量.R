# 使用data.frame創建數據表格 -----
year <- c(104, 105, 106, 107, 108, 109, 110, 111, 112)
transaction_volume <- c(25052, 23131, 30150, 35801, 44318, 54656, 49668, 37832, 41190)
average_price <- c(1433.212522, 1408.591, 1370.920, 1399.957, 1462.107, 1461.356, 1534.618, 1584.469, 1680.184)

data <- data.frame(year, transaction_volume, average_price)

# 在直條圖藍色部分底部顯示交易量，x軸以民國年度表示 -----

ggplot(data, aes(x = year)) +
  geom_col(aes(y = transaction_volume / ratio), fill = "skyblue") + 
  geom_line(aes(y = average_price), color = "red") +
  geom_point(aes(y = average_price), color = "red") +
  geom_text(aes(y = average_price, label = round(average_price, 2)), vjust = -0.5, color = "black") +
  geom_text(aes(y = 0, label = transaction_volume), vjust = -2, color = "darkblue") +
  scale_y_continuous(
    name = "平均價格",
    sec.axis = sec_axis(~.*ratio, name="交易量")
  ) +
  labs(x = "Year", title = "新北市 104 ~ 112 年的平均交易價格及交易量") +
  scale_x_continuous(breaks = year, labels = year) +
  theme(
    plot.title = element_text(hjust = 0.5),   # 調整標題置中
    axis.title.y = element_text(angle = 360,vjust = 0.5),# 將 y 軸標題旋轉90度
    axis.title.y.right = element_text(angle = 360,vjust = 0.5)
  ) -> p


# 注意: x軸的刻度已經設定為"104年","105年",...,"112年"的形式。

# 修正錯誤並移除未使用的引數 -----
ggplot(data, aes(x = year)) +
  geom_col(aes(y = transaction_volume / ratio), fill = "skyblue") + 
  geom_line(aes(y = average_price), color = "red") +
  geom_point(aes(y = average_price), color = "red") +
  geom_text(aes(y = average_price, label = round(average_price, 2)), vjust = -0.5, color = "black") +
  geom_text(aes(y = 0, label = transaction_volume), vjust = -2, color = "darkblue") +
  scale_y_continuous(
    name = "平均價格",
    sec.axis = sec_axis(~.*ratio, name="交易量")
  ) +
  labs(x = "Year", title = "新北市 104 ~ 112 年的平均交易價格及交易量") +
  scale_x_continuous(breaks = year, labels = year) +
  theme(plot.title = element_text(hjust = 0.5))   # 調整標題置中