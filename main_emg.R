library(randomForest)
library(rminer)
library(ggplot2)
library(tabplot)
library(PerformanceAnalytics)

unfactor.df <- function(df){
    # Find the factors
    id <- sapply(df, is.factor)
    # Convert to characters
    df[id] <- lapply(df[id], function(x) as.numeric(as.character(x)))
    df
}

### Load data
data.angle = read.csv('data/Natehin/0-angle.csv', header = F, sep = ";")
data.axis = read.csv('data/Natehin/0-axis.csv', header = F, sep = ";")
data.emg = read.csv('data/Natehin/0-emg.csv', header = F, sep = ";")

### Correct data.emg
data.emg = unfactor.df(data.emg)
tmp = data.emg$V5
mask = abs(tmp) > 10 
data.emg$V5[which(mask)] = NA

### Thin out data.emg
tmp.emg = data.frame(matrix(ncol = 0, nrow = nrow(data.emg)/10))
for(v in colnames(data.emg)) {
    tmp.var = vector(mode='numeric')
    for(i in seq(1, nrow(data.emg)/10)) {
        # TODO: proc NA if it in a 10 elem vector
        #slice = data.emg[[v]][i:(i+9)]
        slice = data.emg[[v]][ ((i-1)*10 + 1) : (i*10) ]        
        #tmp.var[i] = max(slice, na.rm = T)        
        tmp.var[i] = median(slice, na.rm = T)
    }
    tmp.emg[v] = tmp.var
}

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