library(MASS)
data(geyser)
head(geyser)

mydens <- density(geyser$waiting)

dev.new()
hist(geyser$waiting, freq = FALSE,
    main = 'Histogram of Waiting Time', xlab = 'Waiting Time')
ord <- order(mydens$x)
lines(mydens$x[ord], mydens$y[ord], type = 'l', col = 'blue', lwd = 2.1)
#plot(mydens)

##Sampling algorithm taken from
##https://stats.stackexchange.com/questions/321542/how-can-i-draw-a-value-randomly-from-a-kernel-density-estimate
##also see the R documention for density

new_sample <- function(base_sample, size, density_obj)
{
    rkernel <- function(n) rnorm(n, sd = density_obj$bw)
    ns <- sample(base_sample, size, replace=TRUE) + rkernel(size)
    return(ns)
}

set.seed(2130)
my_samp <- new_sample(base_sample = geyser$waiting,
                        size = nrow(geyser), density_obj= mydens)

qqplot(my_samp, geyser$waiting,
        xlab = 'Quantiles of Generated Sample',
        ylab = 'Quantiles of Waiting Time',
        xlim = c(40,110), ylim = c(40,110))
abline(0,1, lwd = 2.1, lty = 'dashed', col = 'blue')
grid(10, 10)
