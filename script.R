# Tomasz Zakrzewski, tz336079, RPIS 2013/2014
# FIXME fix labels on plots

estMean = function(x) {
  return (2 * mean(x))
}

estMax = function(x) {
  n = length(x)
  return (max(x) * (n + 1) / n)
}

#global variables/constants
#for sd experiments
minKFrac = 1
maxKFrac = 100
fracStep = 1
defA = 100

#for ALL experiments
numTests = 1000

#global containers
meanRes = seq(numTests)
maxRes = seq(numTests)
meanError = seq(numTests)
maxError = seq(numTests)

experiment = function(k, a) {
  for (i in seq(numTests)) {
    x = runif(k, 0, a)
    meanRes[i] <<- estMean(x)
    maxRes[i] <<- estMax(x)
    meanError[i] <<- meanRes[i] - a
    maxError[i] <<- maxRes[i] - a
  }
}

plotExperiment = function(k, a) {
  experiment(k, a)

  plot(meanRes, type="l", col="red", ylim=c(0, 2 * a), xlim=c(0, numTests))
  par(new=T)
  plot(maxRes, type="l", col="blue", ylim=c(0, 2 * a), xlim=c(0, numTests))

  hist(meanError, col="red", xlim=c(-a, a), ylim=c(0, numTests))
  par(new=T)
  hist(maxError, col="blue", xlim=c(-a, a), ylim=c(0, numTests))
}

plotExperiment(60, 100)
plotExperiment(200, 1000)
plotExperiment(500, 10000)

meanDevs = seq(maxKFrac - minKFrac + 1)
maxDevs = seq(maxKFrac - minKFrac + 1)

for(kFrac in seq(minKFrac, maxKFrac, by=fracStep)) {
  experiment((kFrac / maxKFrac) * defA, defA)
  meanDevs[kFrac - minKFrac + 1] = sd(meanRes)
  maxDevs[kFrac - minKFrac + 1] = sd(maxRes)
}

plot(meanDevs, type="l", col="red", ylim=c(0, 2* sqrt(a)), xlim=c(minKFrac, maxKFrac))
par(new=T)
plot(maxDevs, type="l", col="blue", ylim=c(0, 2* sqrt(a)), xlim=c(minKFrac, maxKFrac))
