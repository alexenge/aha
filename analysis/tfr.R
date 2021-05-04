library(reticulate)
library(tidyverse)
library(magrittr)
library(eegUtils)

read_csv("data/processed/aha-montage.csv", col_types = cols()) %>%
  mutate(amplitude = py$dat_diff) %>%
  topoplot(limits = c(-10, 10), palette = "viridis", contour = FALSE)

py$avgs["III"][[1]] %>%
  t() %>%
  as_tibble() %>%
  mutate(time = py$tfr$times) %>%
  pivot_longer(-time, names_to = "frequency", values_to = "power_perc") %>%
  mutate(frequency = factor(frequency, levels = unique(frequency), labels = py$freqs)) %>%
  ggplot(aes(x = time, y = frequency, fill = power_perc)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(-1, 1), oob = scales::squish)
