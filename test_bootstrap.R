library("boot")

set.seed(123)
N  <- 100
X1 <- rnorm(N, 175, 7)
X2 <- rnorm(N,  30, 8)
X3 <- abs(rnorm(N, 60, 30))
Y  <- 0.5*X1 - 0.3*X2 - 0.4*X3 + 10 + rnorm(N, 0, 3)
dfRegr <- data.frame(X1, X2, X3, Y)

(fit <- lm(Y ~ X1 + X2 + X3, data=dfRegr))

getRegr <- function(dat, idx) {
    bsFit <- lm(Y ~ X1 + X2 + X3, subset=idx, data=dat)
    coef(bsFit)
}
library(boot)
nR <- 999
(bsRegr <- boot(dfRegr, statistic=getRegr, R=nR))

boot.ci(bsRegr, conf=0.95, type="bca", index=1)$bca


Population <- rweibull(100000, shape=2, scale=20)
Sample <- sample(Population, 10, replace=FALSE)
mean(Sample)
sd(Sample)/sqrt(10)

r <- replicate(1000, mean(sample(Population, 10, replace=F)))
sd(r)
boot <- replicate(999, mean(sample(Sample, replace=T)))
sd(boot)
quantile(boot, probs=c(0.025, 0.975))
c(mean(boot) - 2*sd(Sample)/sqrt(10), mean(boot) + 2*sd(Sample)/sqrt(10))


fit1 <- lm(weight ~ Time*Diet, data=ChickWeight)
newdat <- data.frame(Time=15, Diet=levels(ChickWeight$Diet))
p <- predict(fit1, newdat, se=TRUE)
p$se.fit

library(car)
bootfit1 <- bootCase(fit1, function(x)predict(x, newdat), B=999)
apply(bootfit1, 2, sd)