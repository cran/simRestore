## ----setup--------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7)

library(simRestore)
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)

## ----simulate-----------------------------------------------------------------
sim_pop <- simulate_policy(initial_population_size = 100,
                           num_generations = 20,
                           starting_freq = 0.2,
                           K = 400)

## ----simulate_plot------------------------------------------------------------
ggplot(sim_pop$results, aes(x = t, y = freq_focal_ancestry)) +
  geom_line() +
  ylim(0, 1) +
  ggtitle("Frequency change over time") +
  xlab("Generations") +
  ylab("Focal ancestry")

ggplot(sim_pop$results, aes(x = t, y = num_individuals)) +
  geom_line() +
  ggtitle("Number of individuals") +
  xlab("Generations") +
  ylab("Number of individuals")

sim_pop$results %>%
  gather(key = "sex", value = "num_indiv", c(num_males, num_females)) %>%
  ggplot(aes(x = t, y = num_indiv, col = sex)) +
    geom_line() +
    ggtitle("Number of individuals per sex") +
    xlab("Generations") +
    ylab("Number of individuals")

## ----simulate_pull------------------------------------------------------------
sim_pop <- simulate_policy(initial_population_size = 100,
                           num_generations = 20,
                           pull = 20,
                           starting_freq = 0.2,
                           K = 400)
ggplot(sim_pop$results, aes(x = t, y = freq_focal_ancestry)) +
  geom_line() +
  ylim(0, 1) +
  ggtitle("Frequency change over time") +
  xlab("Generations") +
  ylab("Focal ancestry")

## ----simulate_put1------------------------------------------------------------
sim_pop <- simulate_policy(initial_population_size = 100,
                           num_generations = 20,
                           put = 20,
                           starting_freq = 0.2,
                           K = 400)
ggplot(sim_pop$results, aes(x = t, y = freq_focal_ancestry)) +
  geom_line() +
  ylim(0, 1) +
  ggtitle("Frequency change over time") +
  xlab("Generations") +
  ylab("Focal ancestry")

## ----simulate_put2------------------------------------------------------------
sim_pop <- simulate_policy(initial_population_size = 100,
                           num_generations = 20,
                           put = 20,
                           K = 400,
                           starting_freq = 0.2,
                           ancestry_put = 0.0)
ggplot(sim_pop$results, aes(x = t, y = freq_focal_ancestry)) +
  geom_line() +
  ylim(0, 1) +
  ggtitle("Frequency change over time") +
  xlab("Generations") +
  ylab("Focal ancestry")

## ----static_optimization------------------------------------------------------
opt_res <- optimize_static(target_frequency = 0.99,
                           optimize_put = TRUE,
                           num_generations = 20,
                           starting_freq = 0.2,
                           initial_population_size = 100)
opt_res$put

## ----static_optimization_plot-------------------------------------------------
ggplot(opt_res$results,
       aes(x = t, y = freq_focal_ancestry)) +
  geom_line() +
  ylim(0, 1) +
  ggtitle("Frequency change over time") +
  xlab("Generations") +
  ylab("Focal ancestry")

## ----adaptive_optimization----------------------------------------------------
opt_res <- optimize_adaptive(target_frequency = 0.99,
                             optimize_put = 1000,
                             num_generations = 20,
                             starting_freq = 0.2,
                             initial_population_size = 100)

## ----adaptive_optimization_plot-----------------------------------------------
ggplot(opt_res$results,
       aes(x = t, y = freq_focal_ancestry)) +
  geom_line() +
  ylim(0, 1) +
  ggtitle("Frequency change over time") +
  xlab("Generations") +
  ylab("Focal ancestry") +
  ggtitle("Frequency change over time") +
  xlab("Generations") +
  ylab("Focal ancestry")

ggplot(opt_res$curve,
       aes(x = t, y = put)) +
  geom_step() +
  ggtitle("Number of individuals required to add") +
  xlab("Generation") +
  ylab("Number of individuals")

## ----adaptive_pull------------------------------------------------------------
opt_res <- optimize_adaptive(target_frequency = 0.99,
                             optimize_pull = 1000,
                             optimize_put = 0,
                             num_generations = 20,
                             starting_freq = 0.2,
                             initial_population_size = 100)
ggplot(opt_res$results,
       aes(x = t, y = freq_focal_ancestry)) +
  geom_line() +
  ylim(0, 1) +
  ggtitle("Frequency change over time") +
  xlab("Generations") +
  ylab("Focal ancestry")

ggplot(opt_res$curve,
       aes(x = t, y = pull)) +
  geom_step() +
    ggtitle("Number of individuals required to change") +
  xlab("Generation") +
  ylab("Number of individuals")

## ----adaptive_both------------------------------------------------------------
opt_res <- optimize_adaptive(target_frequency = 0.99,
                             optimize_pull = 1000,
                             optimize_put = 100,
                             num_generations = 20,
                             starting_freq = 0.2,
                             initial_population_size = 100)
ggplot(opt_res$results,
       aes(x = t, y = freq_focal_ancestry)) +
  geom_line() +
  ylim(0, 1)

opt_res$curve %>%
  tidyr::pivot_longer(names_to = "type", values_to = "amount", -t) %>%
  ggplot(aes(x = t, y = amount, col = type)) +
    geom_step()


## ----compare models-----------------------------------------------------------
sim_pop <- simulate_policy(initial_population_size = 100,
                           num_generations = 50,
                           put = 10,
                           starting_freq = 0.2,
                           genetic_model = "point",
                           num_replicates = 100,
                           K = 400)

sim_pop2 <- simulate_policy(initial_population_size = 100,
                            num_generations = 50,
                            put = 10,
                            starting_freq = 0.2,
                            genetic_model = "junctions",
                            num_replicates = 100,
                            K = 400)
# prepare data frame for plotting:
to_plot1 <- sim_pop$results
to_plot1$model <- "simplified"
to_plot2 <- sim_pop2$results
to_plot2$model <- "junctions"
to_plot2$replicate <- to_plot2$replicate + 100
to_plot <- rbind(to_plot1, to_plot2)

# plot all replicates:
ggplot(to_plot,
       aes(x = t, y = freq_focal_ancestry, col = model, group = replicate)) +
  geom_line() +
  # scale_color_brewer(type = "qual") +
  theme_classic()


# summarise across replicates:
to_plot %>%
  group_by(t, model) %>%
  summarise("mean_ancestry" = mean(freq_focal_ancestry)) %>%
  ggplot(aes(x = t, y = mean_ancestry, col = model)) +
    geom_line()

