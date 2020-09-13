## A file I started to ask a question on stackoverflow, but it got answered
## (sum with integrate(), not with sum()) while I was writing it.

xs <- seq(from=0, to=20, by=1)
ys <- dgamma(xs, shape=2, scale=5)
sum(ys)
ys <- dgamma(xs, shape=1, scale=0.5)

scale <- 0.5

shape <- 1
result <- integrate(function(x) dgamma(x, shape=shape, scale=scale), 0, Inf)

sum(dgamma(-100:100, shape=shape, scale=scale))
