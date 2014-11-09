require(ggplot2)
require(tabplot)
require(PerformanceAnalytics)

# Data summary and visualivation
summary(data.emg)
tableplot(data.emg)
chart.Correlation(data.emg[-5])

hist(data.angle$V1, breaks = 25)

# Basic Scatterplot Matrix
pairs(data.angle$V1 ~ ., data = data.emg)
