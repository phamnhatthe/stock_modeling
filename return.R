setwd("C:/hw/stock")
# V stock
datV<-read.csv("V.csv",sep=",")


perc_return<-as.numeric()
for (i in 2:length(datV$Close)){
  perc_return[i] <- (datV$Close[i] - datV$Close[i - 1]) / datV$Close[i - 1]
}

perc_return<-perc_return[-which(is.na(perc_return))]

plot(density(perc_return))
mean(perc_return)

# making a function

plot_return <- function(data){
  perc_return <- as.numeric()
  for (i in 2:length(data)){
    perc_return[i] <- (data[i] - data[i-1]) / data[i-1]
  }
  perc_return <- perc_return[-which(is.na(perc_return))]
  return(arglist=c(mean(data),plot(density(perc_return))))
}

plot_return(datV$Close)
