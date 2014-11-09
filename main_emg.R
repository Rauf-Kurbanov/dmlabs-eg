unfactor.df <- function(df){
    # Find the factors
    id <- sapply(df, is.factor)
    # Convert to characters
    df[id] <- lapply(df[id], function(x) as.numeric(as.character(x)))
    df
}

# Load data
data.angle = read.csv('data/Natehin/0-angle.csv', header = F, sep = ";")
data.axis = read.csv('data/Natehin/0-axis.csv', header = F, sep = ";")
data.emg = read.csv('data/Natehin/0-emg.csv', header = F, sep = ";")

# Correct data.emg
data.emg = unfactor.df(data.emg)
tmp = data.emg$V5
mask = abs(tmp) > 10 
data.emg$V5[which(mask)] = NA

# Thin out data.emg
tmp.emg = data.frame(matrix(ncol = 0, nrow = nrow(data.emg)/10))
for(v in colnames(data.emg)) {
    tmp.var = vector(mode='numeric')
    for(i in seq(1, nrow(data.emg)/10)) {
        # TODO: proc NA if it in a 10 elem vector
        tmp.var[i] = median(data.emg[[v]][i:(i+9)])
    }
    tmp.emg[v] = tmp.var
}
data.emg = tmp.emg

# Main routine
