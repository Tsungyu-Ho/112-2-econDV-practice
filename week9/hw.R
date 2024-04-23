# 載入需要的函式庫
library(tidyverse)

# 建立一個包含候選人與其支持率的資料框
election_data <- data.frame(
  Candidates = c('Candidate A', 'Candidate B', 'Candidate C'),
  Supporting_Rate = c(40.05, 33.49, 26.45),
  Fill_Color = c('#67c167', '#4372c4', '#7ededd') # 添加填充顏色
)

# 建立柱狀圖
myPlot <- ggplot(election_data, aes(x = Candidates, y = Supporting_Rate, fill = Fill_Color)) +
  geom_bar(stat = 'identity') +
  labs(title = '2024 Presidential Election',
       subtitle = 'Unit: percentage',
       x = '', 
       y = 'Supporting Rate') +
  scale_y_continuous(expand = expansion(0, 0)) + # y軸無放縮
  scale_fill_identity() + # 使用填充顏色
  geom_text(aes(label = Supporting_Rate), vjust = -0.3) # 在柱狀圖上加入數據標籤

# 將 y 轴的标题设置为空
myPlot <- myPlot + ylab('')

# 使用 annotate 函数在图表的顶部添加一段文字，作为 y 轴的标题
myPlot <- myPlot + annotate('text', x = 0, y = Inf, label = 'Supporting Rate', hjust = 0, vjust = 1)

print(myPlot)

# Find the maximum y value
max_y <- max(election_data$Supporting_Rate, na.rm=TRUE)

# Remove the original y-axis title
myPlot <- myPlot + ylab('')

# Adjust the plot margins: top, right, bottom, left. You might need to adjust the numbers to fit your specific plot.
myPlot <- myPlot + theme(plot.margin = unit(c(3,1,1,1), 'cm'))

# Add the y-axis title above the highest y-axis value
myPlot <- myPlot + annotate('text', x = -Inf, y = max_y, label = 'Supporting Rate', hjust = 0, vjust = 1)

print(myPlot)

# 首先，您需要找出數據中的最大y值，假設它是max_y
max_y <- max(election_data$Supporting_Rate, na.rm = TRUE)

# 移除原始的y軸標籤
myPlot <- myPlot + ylab('')

# 在y軸的最大值位置添加y軸標題
myPlot <- myPlot + annotate('text', x = 0, y = max_y, label = 'Supporting Rate', hjust = 0, vjust = 0)

print(myPlot)

# Find the maximum y value
max_y <- max(election_data$Supporting_Rate, na.rm=TRUE)

# Remove the original y-axis title
myPlot <- myPlot + ylab('')

# Plot the graph
print(myPlot)

# Add the y-axis title above the highest y-axis value
mtext('Supporting Rate', side=2, line=1, at=max_y, las=0, font=2)
print(myPlot)