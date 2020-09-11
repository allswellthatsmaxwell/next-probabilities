library(purrr)
library(ggplot2)
library(magrittr)
library(dplyr)

p_step <- function(fn, params, from, stepsize=1) {
  start <- do.call(partial(fn, from), params)
  end <- do.call(partial(fn, from + stepsize), params)
  end - start
}

make_distribution <- function(parameter_set, xs) {
  ys <- parameter_set %$% dgamma(range, shape=shape, scale=scale)
  parameter_set %$% 
    tibble::tibble(xs=range, ys=ys, shape=shape, scale=scale,
                   name=glue::glue("shape={shape}, scale={scale}"))
}

distributions <- list(
  "gamma" = list(p = pgamma, d = dgamma, x=seq(from=0, to=30, by=1),
                 params=list(shape=7.5, scale=1)),
  "normal" = list(p = pnorm, d = dnorm, x=seq(from=-4, to=6, by=0.01),
                  params=list(mean=0, sd=1)))
choice <- "gamma"
distribution <- distributions[[choice]]

parameter_sets <- list(
  list(shape=1, scale=2),
  list(shape=2, scale=2),
  list(shape=3, scale=2),
  list(shape=5, scale=1),
  list(shape=9, scale=0.5),
  # list(shape=0.5, scale=1),
  list(shape=7.5, scale=1))
range <- seq(from=0, to=20, by=1)

distros_df <- lapply(parameter_sets, 
                     function(pset) make_distribution(pset, range)) %>%
  dplyr::bind_rows()

distros_df %>%
  group_by(name) %>%
  summarize(sum(ys))

distros_df %>%
  ggplot(aes(x=xs, y=ys, color=name)) +
  geom_line() +
  theme_bw()
################################################################################

p_under <- function(from)
  with(distribution, p_step(fn=p, params=params, from=from, stepsize=0.01))
p_rest <- function(from) 
  with(distribution, p_step(fn=p, params=params, from=from, stepsize=1000))
ratio_next_to_rest <- function(from)
  p_under(from) / p_rest(from)

distribution$y <- distribution %$% do.call(partial(d, x), params)

next_probas <- distribution %$% sapply(x, ratio_next_to_rest)

dat <- with(distribution, tibble::tibble(x=x, y=y, p_next=next_probas))

dat %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = y)) +
  geom_line(aes(y = p_next), color = "purple") +
  theme_bw()

