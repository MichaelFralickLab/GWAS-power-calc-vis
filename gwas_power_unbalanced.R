# Any chance you can familiarize yourself with it over next week or so?
#
# Here is what she has sent us:
#
#   “The sample size of 60 individuals with SGLT2 inhibitor DKA provides >80% power to uncover a significant association (P<5x10-8) with genetic variants (minor allele frequency 0.20; odds-ratio>5) and DKA, as calculated using the R package, genpwr.”
#
# Our plan is to match cases N=60 3:1 (or so) thus we will have 180 controls.
#
# We will be able to control far more controls, but I’m not sure how much more incremental power that would provide us with. I’ve also attached a one pager on the DANGER study here as reminder for what it is all about (in a nut shell)


# - sample size
# - detectable effect size (or odds ratio in the case of a binary outcome variable)
# - power

## assume two values to calculate the third...
# N = (60 cases + 180 controls) = 240
# OR = 5
# power = ?
#
## Also assume MAF (often 0.05)
# MAF = 0.2

library(tidyverse)

sample_sizes <-
  tibble(
    cases = 60,
    controls = 180,
    added_controls = seq(-1, 10, .5) |> exp() |> round(),
    N_total = cases + controls + added_controls,
    k = (N_total - cases) / cases,
    Case.Rate = cases / N_total
  ) |>
  distinct()

gwas_power <- genpwr::genpwr.calc(
  calc = 'power',
  model = 'logistic',
  ge.interaction = NULL,
  N = sample_sizes$N_total,
  Case.Rate = sample_sizes$Case.Rate,
  MAF = .2,
  OR = 5,
  Alpha = 5 * 10^-8,
  True.Model = c('Dominant', 'Recessive', 'Additive'),
  Test.Model = c('Dominant', 'Recessive', 'Additive', '2df')
) |>
  as_tibble() |>
  glimpse()

sample_sizes |>
  left_join(gwas_power, by = join_by(N_total, Case.Rate)) |>
  ggplot(aes(
    x = added_controls + 1,
    y = `Power_at_Alpha_5e-08`,
    color = True.Model
  )) +
  geom_line(alpha = .5) +
  geom_point(size = .51) +
  scale_color_viridis_d(end = .8) +
  facet_wrap(Test.Model~.) +
  scale_y_continuous(
    labels = scales::label_percent()
  ) +
  scale_x_log10() +
  labs(
    x = 'Additional controls (N + 1)',
    y = expression('Power at a = 5 x 10^-8*'),
    title = 'Power to detect OR > 5 where MAF 0.2',
    subtitle = 'Panels show different test models,\ncurves show the true genetic model (unknown)'
  ) +
  theme(
    axis.title.y = ggtext::element_markdown()
  )


# usually test an additive model