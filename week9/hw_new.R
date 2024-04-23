library(ggplot2)
library(scales)
library(dplyr)

Grades_Data <- tibble(
  Semester = factor(c('一上', '一下', '二上', '二下', '三上', '三下'), levels = c('一上', '一下', '二上', '二下', '三上', '三下')),
  Grades = c(87.1, 91.6, 89.5, 92.0, 92.2, 93.4),
  Ranking = c(7, 1, 4, 4, 1, 2)
)

gg <- ggplot(Grades_Data, aes(x=Semester)) +
  geom_bar(aes(y=Grades), stat='identity', fill='steelblue') +
  geom_text(aes(y=Grades, label=Grades), vjust=-0.5, size=3.5) +
  
  geom_line(aes(y = Ranking*-5+100, group=1), stat='identity', color='red', size=1.0, linetype='dashed') +
  geom_text(aes(y=Ranking*-5+100, label=Ranking), vjust=-0.5, size=3.5) +
  
  scale_y_continuous(
    name = "Grades",
    sec.axis = sec_axis(trans = ~(. - 100) / -5, name = "Ranking")
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Grades And Ranking \n\n資料來源:宗祐國立台北大學成績單")

print(gg)