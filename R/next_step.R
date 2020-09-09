library(purrr)
library(ggplot2)
library(magrittr)

p_step <- function(fn, params, from, stepsize=1) {
  start <- do.call(partial(fn, from), params)
  end <- do.call(partial(fn, from + stepsize), params)
  end - start
}

distributions <- list(
  "gamma" = list(p = pgamma, d = dgamma, x=seq(from=0, to=30, by=1),
                 params=list(shape=7.5, scale=1)),
  "normal" = list(p = pnorm, d = dnorm, x=seq(from=-4, to=6, by=0.01),
                  params=list(mean=0, sd=1)))
choice <- "normal"
distribution <- distributions[[choice]]

p_under <- function(from) {
  with(distribution, p_step(fn=p, params=params, from=from, stepsize=0.01))
}

p_rest <- function(from) {
  with(distribution, p_step(fn=p, params=params, from=from, stepsize=1000))
}

ratio_next_to_rest <- function(from) {
  p_under(from) / p_rest(from)
}

distribution$y <- distribution %$% do.call(partial(d, x), params)

next_probas <- distribution %$% sapply(x, ratio_next_to_rest)

dat <- with(distribution, tibble::tibble(x=x, y=y, p_next=next_probas))

dat %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = y)) +
  geom_line(aes(y = p_next), color = "purple") +
  theme_bw()

