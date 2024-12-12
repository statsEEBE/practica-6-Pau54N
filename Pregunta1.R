
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)
var = 25
C = 0.9
X = mean(x); X
## Intervalo confianza 95%
n = length(x)
sigma = sqrt(var)
Za = qnorm(0.975)
IE = c(X-Za*sigma/sqrt(n),X+Za*sigma/sqrt(n));IE

###
#install.packages("BSDA")
library(BSDA)
z.test(x,sigma.x=sigma,conf.level=0.9)

#H1: mu! =500
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)
var = 25
X = mean(x); X
n = length(x)
sigma = sqrt(var)
zc = qnorm(0.975); zc

zobs = (X-500)/(sigma/sqrt(n)); zobs


n = (qnorm(0.025)*sigma)^2;n

#3
X = mean(x)
s = sd(x)
n = length(x)
t005 = qt(0.995,n-1)
c(X-t005*s/sqrt(n),X+t005*s/sqrt(n))

t.test(x,conf.level = 0.99)

#### Probemos peso medio caja no 500g

#H0: mu =500
#H1: mu! =500

mu0=500

t.test(x,conf.level = 0.99, mu=mu0)
