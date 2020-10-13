x <- seq(1, 1000, 1)
attach(original_118)

par(mfrow=c(3, 2))
horizontal <- c(options_h, -options_h)
hist(options_h, main = "Histogram of Current Horizontal", xlab = "Horizontal", xlim = c(0,80), freq = FALSE)
curve(dnorm(x, mean(horizontal), sd(horizontal)*1.25)*2, add=TRUE, xlim = c(0,80), lty = "dashed")

hist(options_v, main = "Histogram of Current Vertical", xlab = "Vertical", freq = FALSE)
curve(dnorm(x, mean(options_v), sd(options_v)*1.25), add=TRUE, lty = "dashed")

radius <- c(options_r, -(options_r-max(options_r))+max(options_r))
hist(options_r, main = "Histogram of Current Radius", xlab = "Radius", xlim = c(0,120), freq = FALSE)
curve(dnorm(x, mean(radius), sd(radius)*1.25)*2, add=TRUE, xlim = c(0, 110), lty = "dashed")

hist(options_hue, main = "Histogram of Current Hue", xlab = "Hue", xlim = c(0,360), freq = FALSE)
abline(h = 1/360, lty = "dashed", xlim = c(0,360))

saturation <- c(options_sat, -(options_sat-max(options_sat))+max(options_sat))
hist(options_sat, main = "Histogram of Current Saturation", xlab = "Saturation", xlim = c(0,100), freq = FALSE)
curve(dnorm(x, mean(saturation), sd(saturation)*1.25)*2, add=TRUE, xlim = c(0,100), lty = "dashed")

hist(options_light, main = "Histogram of Current Light", xlab = "Light", xlim = c(0,100), freq = FALSE)
curve(dnorm(x, mean(options_light), sd(options_light)*1.25), add=TRUE, xlim = c(0,100), lty = "dashed")
title("Proposed Sampling Distributions", line = -1, outer = TRUE, cex.main = 1.5)


## Sampling
par(mfrow=c(3,2))
sample_h <- abs(rnorm(500, mean(horizontal), sd(horizontal)*1.25))
hist(sample_h)

sample_v <- rnorm(500, mean(options_v), sd(options_v)*1.25)
hist(sample_v)

sample_r <- rnorm(2500, mean(radius), sd(radius)*1.25)
high_sample <- sample_r[sample_r >= mean(radius)]
high_sample <- -(high_sample - mean(radius)) + mean(radius)
sample_r <- c(sample_r[sample_r < mean(radius)], high_sample)
sample_r <- sample_r[sample_r > 0][1:500]
hist(sample_r)






sample_hue <- runif(500, 0, 360)
hist(sample_hue)

sample_sat <- rnorm(2500, mean(saturation), sd(saturation)*1.25)
high_sample <- sample_sat[sample_sat >= mean(saturation)]
high_sample <- -(high_sample - mean(saturation)) + mean(saturation)
sample_sat <- c(sample_sat[sample_sat < mean(saturation)], high_sample)
sample_sat <- sample_sat[sample_sat > 0][1:500]
hist(sample_sat)

sample_light <- rnorm(2500, mean(options_light), sd(options_light)*1.25)
sample_light <- sample_light[sample_light < 90 & sample_light > 0][1:500]
hist(sample_light)

## Create Data Frame
prob_sample <- data.frame(h = sample_h, v = sample_v, r = sample_r, hue = sample_hue, sat = sample_sat, light = sample_light,
                          x = random_sample$x, y = random_sample$y)
write.csv(prob_sample, "prob_sample.csv")
