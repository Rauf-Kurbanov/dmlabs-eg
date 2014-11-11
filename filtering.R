# Graphical explanation
# http://goo.gl/03AwlJ
library(TTR)

chunkByMax <- function(vec) {
  tens <- seq(from= 1, to = length(vec), by= 10)
  chunks <- sapply(tens, function(x) vec[seq(from = x, length.out = 10)])
  maxed <- apply(chunks, 2, max)
}

# raw data
emg.raw <- data.emg
ind <- 1: dim(emg.raw)[1]
#plot(ind, emg.raw[ind, 3], type='l')

## raw to abs
emg.abs <- apply(emg.raw, 2, abs)
emg.abs <- as.data.frame(emg.abs)
#plot(ind, emg.abs[ind, 3], type='l')

## abs to chunked
emg.chunked <- apply(emg.abs, 2, chunkByMax)

tInd <- ind <- 1: (dim(data.emg)[1] / 10)
#plot(tInd, emg.chunked[tInd, 3], type = 'l')

## chunked to moving average
emg.moving.average <- apply(emg.chunked, 2, function(x) SMA(x, n= 400))
#plot(tInd, emg.moving.average[tInd, 3], type = 'l')

## result
emg.filtered <- emg.moving.average 