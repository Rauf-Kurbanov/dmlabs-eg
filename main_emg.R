library(randomForest)
library(rminer)

unfactorVec <- function(vec) {
  tmp <- vec
  tmp <- as.character(tmp)
  tmp <- as.numeric(tmp)
  
  # Replacing missing values by adjacent
  while (any(is.na(tmp))) {
    naInd <- which(tmp %in% NA)
    tmp[naInd] <- tmp[naInd - 1]  
  }
  tmp
}

### Load data
data.angle = read.csv('data/Natehin/0-angle.csv', header = F, sep = ";")
data.axis = read.csv('data/Natehin/0-axis.csv', header = F, sep = ";")
data.emg = read.csv('data/Natehin/0-emg.csv', header = F, sep = ";")

### Correct data.emg
data.emg$V5 <- unfactorVec(data.emg$V5)

### Filtering features
### now we have emg.filtered
source('filtering.R') 
tmp.emg <- emg.filtered

### Test and training set
set <- 1:dim(data.angle)[1]
training.set <- sample(1:length(set), size = length(set)*0.7)
test.set <- set[-training.set]

### Random forest for angle and axis
source("rand.forest.R")
rf <- rand.forest(tmp.emg, data.angle, data.axis, training.set, test.set)
print(rf)

### Visualization
source("visualization.R")

### Save workspase
#save.image("emg.RData")
