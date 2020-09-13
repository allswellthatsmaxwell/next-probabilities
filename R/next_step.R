library(purrr)
library(ggplot2)
library(magrittr)
library(dplyr)

OUTDIR <- '../plots'

p_step <- function(shape, scale, from, stepsize=1) {
  start <- pgamma(q=from, shape=shape, scale=scale)
  end <- pgamma(q = from + stepsize, shape=shape, scale=scale)
  end - start
}

make_distribution <- function(parameter_set, xs) {
  ys <- parameter_set %$% dgamma(range, shape=shape, scale=scale)
  parameter_set %$% 
    tibble::tibble(xs=range, ys=ys, shape=shape, scale=scale,
                   name=glue::glue("shape={shape}, scale={scale}"))
}

parameter_sets <- list(
  list(shape=1, scale=2),
  list(shape=2, scale=2),
  list(shape=3, scale=2),
  list(shape=5, scale=1),
  list(shape=9, scale=0.5),
  list(shape=0.5, scale=1),
  list(shape=7.5, scale=1))
stepsize <- 1
range <- seq(from=0, to=25, by=stepsize)

distros_df <- lapply(parameter_sets, 
                     function(pset) make_distribution(pset, range)) %>%
  dplyr::bind_rows()

distros_df %<>%
  mutate(p_under = p_step(scale=scale, shape=shape, from=xs, stepsize=stepsize),
         p_rest  = p_step(scale=scale, shape=shape, from=xs, stepsize=1000),
         p_next_unnorm = p_under / p_rest) %>%
  group_by(name) %>%
  mutate(sum_p_next_unnorm = sum(p_next_unnorm)) %>%
  mutate(p_next = p_next_unnorm / sum_p_next_unnorm) %>%
  dplyr::ungroup()

modes_df <- distros_df %>%
  group_by(name) %>%
  mutate(peak = max(ys)) %>%
  mutate(is_peak = ys == peak) %>%
  dplyr::filter(is_peak) %>%
  distinct(name, xs, peak, p_next)

wiki_plot <- distros_df %>%
  ggplot(aes(x = xs, y = ys, color = name)) +
  geom_line() +
  theme_bw()
wiki_plot

bases_with_pnexts_plot <- distros_df %>%
  ggplot(aes(x = xs)) +
  geom_line(aes(y = ys), color = "black") +
  geom_line(aes(y = p_next), color = "purple") +
  facet_wrap(~name) + #, scales = "free_y") +
  theme_bw()
bases_with_pnexts_plot

bump <- 0.001
pnext_only_plot <- distros_df %>%
  ggplot(aes(x = xs, color = name)) +
  geom_line(aes(y = p_next)) +
  geom_point(data=modes_df, aes(y = p_next)) +
  geom_segment(data=modes_df, aes(xend = xs, 
                                  y = p_next - bump, yend = p_next + bump)) +
  theme_bw()
pnext_only_plot

## We should extend this to more than just next-day. Findings should generalize
## to any positive step-size, if the p_next functions are all monotonic, like
## my examples are.
## On the way: prove monotonicity of the pnext function, or prove not all 
## are monotonic.

plots_to_save <- list(wiki_plot = wiki_plot, 
                      bases_with_pnexts_plot = bases_with_pnexts_plot, 
                      pnext_only_plot = pnext_only_plot)

do_save <- function(name, plot) {
  ggsave(filename = paste0(name, '.png'), plot = plot, path = OUTDIR, 
         dpi = 90, device = 'png')
}

invisible(Map(do_save, names(plots_to_save), plots_to_save))


