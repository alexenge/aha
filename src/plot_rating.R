plot_figs1 <- function(rating) {
  #' Plots rating study results as a violin plot

  require(dplyr)
  require(stringr)
  require(ggplot2)

  rating %>%
    filter(!is.na(cosine)) %>%
    mutate(condition_comb = str_to_sentence(paste(familiarity, condition))) %>%
    ggplot(aes(x = condition_comb, y = cosine, color = condition_comb)) +
    geom_violin(fill = NA) +
    geom_boxplot(width = 0.2) +
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) +
    scale_color_manual(values = c("#5e3c99", "#ca0020", "#0571b0")) +
    labs(x = NULL, y = "Cosine distance to keywords") +
    coord_cartesian(ylim = c(0.0, 1.0)) +
    theme_classic() +
    theme(
      axis.title = element_text(color = "black", size = 10.9),
      axis.text = element_text(color = "black", size = 10.9),
      legend.position = "none",
      panel.grid.major.y = element_line(),
      panel.grid.minor.y = element_line()
    )
}

plot_figs2 <- function(rating) {
  #' Plots rating study results on the item level (mean +/- standard error)

  require(dplyr)
  require(forcats)
  require(stringr)
  require(ggplot2)

  rating %>%
    filter(!is.na(cosine) & condition == "matching") %>%
    mutate(condition_comb = str_to_sentence(paste(familiarity, condition))) %>%
    select(item_id, condition_comb, cosine) %>%
    group_by(item_id, condition_comb) %>%
    summarize(
      mean = mean(cosine),
      sd = sd(cosine),
      n = n(),
      se = sd / sqrt(n),
      .groups = "drop"
    ) %>%
    mutate(item_id = fct_reorder(item_id, desc(mean))) %>%
    ggplot(
      aes(
        x = item_id, y = mean, color = condition_comb, group = 1,
        ymin = mean - se, ymax = mean + se
      )
    ) +
    geom_linerange(show.legend = FALSE) +
    geom_line() +
    labs(x = "Stimulus ID", y = "Cosine distance to keywords", color = NULL) +
    scale_x_discrete(guide = guide_axis(angle = 90, n.dodge = 3)) +
    scale_y_continuous(limits = c(0.0, 1.0), breaks = seq(0.0, 1.0, by = 0.2)) +
    scale_color_manual(values = c("#5e3c99", "#ca0020")) +
    theme_classic() +
    theme(
      axis.title = element_text(color = "black", size = 10.9),
      axis.text = element_text(color = "black", size = 10.9),
      legend.position = c(0.93, 0.92),
      panel.grid.major.x = element_line(),
      panel.grid.major.y = element_line(),
      panel.grid.minor.y = element_line()
    )
}
