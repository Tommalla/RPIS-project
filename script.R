# Tomasz Zakrzewski, tz336079, RPIS 2013/2014
estMean = function(x) {
  return (2 * mean(x))
}

estMax = function(x) {
  n = length(x)
  return (max(x) * (n + 1) / n)
}

# place values here, since R sucks...
k = 60
a = 1000
numTests = 1000

meanRes = seq(numTests)
maxRes = seq(numTests)

for (i in seq(numTests)) {
  x = runif(k, 0, a)
  meanRes[i] = estMean(x)
  maxRes[i] = estMax(x)
}

plot(meanRes, type="l", col="red", ylim=c(0, 2 * a), xlim=c(0, numTests))
par(new=T)
plot(maxRes, type="l", col="blue", ylim=c(0, 2 * a), xlim=c(0, numTests))