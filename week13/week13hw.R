library(ggplot2)

# 創建數據框
df <- data.frame(
  model = c("所有ols，所有建築，包含時間空間固定效應 (無電動車)", "所有ols，所有建築，包含時間空間固定效應 (無電動車)"),
  period = c("104~106", "107~112"),
  coefficient = c(-12.103, 3.252),   # 係數
  lower_bound = c(-12.855, 2.885),   # 95%信賴區間下界
  upper_bound = c(-11.403, 3.625)    # 95%信賴區間上界
)

# 用ggplot2畫盒形圖
ggplot(df, aes(x = period, y = coefficient)) +
  geom_boxplot(aes(lower = lower_bound, upper = upper_bound, middle = coefficient, ymin = lower_bound, ymax = upper_bound), stat = "identity")

#建立誤差棒圖
ggplot(df, aes(x=period, y=coefficient, group=1)) +
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width=.1,
                position=position_dodge(0.05)) +
  geom_point() +
  xlab("時間區段") + 
  ylab("係數") +
  ggtitle("不同時間區段的係數值及其信賴區間") +
  labs(caption = "資料來源: 本研究整理") +
  theme(plot.caption = element_text(hjust = 0.5)) + 
  theme_minimal()

ggplot(df, aes(x=period, y=coefficient, group=1)) +
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width=.1, position=position_dodge(0.05)) +
  geom_point() +
  xlab("時間區段") + 
  ylab("綠建築溢價率(%)") +
  ggtitle("不同時間區段的綠建築溢價率及其信賴區間") +
  labs(caption = "資料來源: 本研究整理") +
  theme(plot.caption = element_text(hjust = 0.5), 
        plot.title = element_text(hjust = 0.5),
        plot.title.position = "plot",
        axis.title.y = element_text(angle = 270, vjust = 0.5)) + 
  theme_minimal() +
  scale_y_continuous(breaks = seq(from = -13, to = 6, by = 1)) +
  coord_fixed(ratio = 0.5/2)

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
