# Data summary and visualivation
summary(tmp.emg)

tableplot(tmp.emg)
dev.copy(png,'pictures/tableplot.emg.png',width = 1920, height = 1080)
dev.off()

#chart.Correlation(tmp.emg[-5])
chart.Correlation(tmp.emg)
dev.copy(png,'pictures/chart.Correlation.emg.png',width = 1920, height = 1080)
dev.off()

hist(data.angle$V1, breaks = 25)
dev.copy(png,'pictures/hist.angle.png',width = 1920, height = 1080)
dev.off()

# Basic Scatterplot Matrix
pairs(data.angle$V1 ~ ., data = tmp.emg)
dev.copy(png,'pictures/pairs.angle-emg.png',width = 1920, height = 1080)
dev.off()