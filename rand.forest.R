### Random forest for angle and axis
### Return R-square for our prediction

rand.forest <- function(data.emg, data.angle, data.axis, training.set, test.set){
    
    ### Random forest
    
    data.emg.train <- data.emg[training.set,]
    data.emg.test <- data.emg[test.set,]

    rand.forest.angle <- randomForest(data.angle[training.set,1] ~ . , 
                                     data = data.emg.train, ntree = 500) 
    rand.forest.angle.predict <- predict(rand.forest.angle, data.emg.test)
    
    rand.forest.axis.predict <- matrix(NA, dim(data.emg.test)[1], 3)
    for (i in 1:dim(data.axis)[2]){
        rand.forest.axis <- randomForest(data.axis[training.set,i] ~ . , 
                                         data = data.emg.train, ntree = 500) 
        rand.forest.axis.predict[,i] <- predict(rand.forest.angle, data.emg.test)
    }

    ### R-square for our prediction

    R2.rf.angle <- mmetric(y = data.angle[test.set,1], x = 
                                  rand.forest.angle.predict, metric = 'R2')

    R2.rf.axis <- sapply(1:3, function(i) {mmetric(y = data.axis[test.set,i], x = 
                                  rand.forest.axis.predict[,i], metric = 'R2') } )

    return( list(R2.rf.angle = R2.rf.angle, R2.rf.axis = R2.rf.axis) )
}


