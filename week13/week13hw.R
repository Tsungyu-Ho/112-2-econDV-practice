library(ggplot2)

# 創建數據框
df <- data.frame(
  model = c("所有ols，所有建築，包含時間空間固定效應 (無電動車)", "所有ols，所有建築，包含時間空間固定效應 (無電動車)"),
  period = c("104~106", "107~112"),
  coefficient = c(-12.103, 3.252),   # 係數
  lower_bound = c(-12.855, 2.885),   # 95%信賴區間下界
  upper_bound = c(-11.403, 3.625)    # 95%信賴區間上界
)
# 數據
coefficients <- c(-0.15041, 0.08546, -0.13498, 0.00904, 0.08004, 0.04707, 0.04289, 0.03562, 0.04394)*100
lower_bounds <- c(-0.16389, 0.0693, -0.1453, -0.001, 0.07144, 0.03977, 0.03355, 0.02429, 0.03045)*100
upper_bounds <- c(-0.13584, 0.10186, -0.12541, 0.01816, 0.08763, 0.05443, 0.05127, 0.04812, 0.0576)*100

# 建立數據框
df <- data.frame(
  model = rep("所有ols，所有建築，包含時間空間固定效應 (無電動車)", 9),  # 假設你所有的模型都是同一個模型
  period = sprintf("%d年度", 104:112),
  coefficient = coefficients,
  lower_bound = lower_bounds,
  upper_bound = upper_bounds
)

# 顯示數據框
print(df)

#建立誤差棒圖

# 計算最大小值
min_val <- floor(min(df$lower_bound))
max_val <- ceiling(max(df$upper_bound))

# 產生奇數序列，並確保包含0
breaks <- seq(from = min_val - (min_val %% 2), to = max_val + (max_val %% 2), by = 2)
breaks <- sort(unique(c(breaks, 0)))

# 創建特殊年份標記
df$是否顯著 <- ifelse(df$period == "107年度", "不顯著", "顯著")

# 建立圖表
ggplot(df, aes(x = period, y = coefficient, color = 是否顯著, group = 1)) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = .1) +
  scale_color_manual(values = c("不顯著" = "red", "顯著" = "black")) +
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf('%.2f%%', coefficient)), hjust = -0.4) +
  xlab("時間區段") + 
  ylab("綠建築溢價率(%)") +
  ggtitle("不同時間區段的綠建築溢價率及其信賴區間(橫斷面分析)") +
  labs(caption = "資料來源: 本研究整理") +
  theme_bw() +
  theme(
    plot.caption = element_text(hjust = 0), 
    plot.title = element_text(hjust = 0.5),
    plot.title.position = "plot",
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    axis.text = element_text(size=12),
    axis.title = element_text(size=14)
  ) +
  scale_y_continuous(breaks = breaks) +
  geom_hline(yintercept = 0, linetype="dashed", color = "blue", size = 1) +
  theme(
    panel.grid.major = element_line(color = "grey", linewidth = 0.1), 
    panel.grid.minor = element_line(color = "grey", linewidth = 0.05)
  ) -> premium
premium



# 載入ggplot2套件
# library(ggplot2)

# 創建資料
#df_1 <- data.frame(
#  Periods = c("104~106", "107~112"),  # 時間區段
#  Estimates = c(-12.103, 3.252),   # 係數
#  Lower_CI = c(-12.855, 2.885),   # 信賴區間下界
#  Upper_CI = c(-11.403, 3.625),   # 信賴區間上界
#  Group = factor(1:2)  # 組別 (這邊我們只用數字來表示，你也可以替換成其他的類別變數)
#)

# 建立信賴區間圖
#ggplot(data = df_1, aes(x = Estimates, y = Group)) + 
#  geom_point() + 
#  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.2) + 
#  labs(x = "係數及其信賴區間", y = "時間期間", title = "不同時間區段之係數的信賴區間圖")
