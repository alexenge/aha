plot_fig1a <- function(files_dir, condition_colors) {
  #' Creates a plot showing the trial structure of the EEG experiment

  require(tidyverse)
  require(cowplot)

  example_stim <- magick::image_read(here::here("example_stimuli", "103.png"))
  stim_bg_color <- "#a9d9ff"
  ggplot() +
    coord_cartesian(xlim = c(-100, 100), ylim = c(0, 100), expand = FALSE) +
    theme_void() +
    theme(aspect.ratio = 0.5) +
    ## INSIGHT PHASE ##
    # Title
    annotate(
      geom = "text", x = 0, y = 97.2, label = "Insight phase", size = 14 / .pt,
      family = "Helvetica", fontface = "bold"
    ) +
    # Arrow
    annotate(
      geom = "segment", x = -30, xend = 24, y = 79, yend = 85,
      arrow = arrow(length = unit(0.1, "inches"))
    ) +
    # Squares
    annotate(
      geom = "rect", xmin = -37, xmax = -17, ymin = 55, ymax = 75,
      color = "black", fill = stim_bg_color
    ) +
    annotate(
      geom = "rect", xmin = -19, xmax = 1, ymin = 45, ymax = 65,
      color = condition_colors[2], fill = stim_bg_color
    ) +
    annotate(
      geom = "rect", xmin = -19, xmax = 1, ymin = 69, ymax = 89,
      color = condition_colors[1], fill = stim_bg_color
    ) +
    annotate(
      geom = "rect", xmin = -1, xmax = 19, ymin = 59, ymax = 79,
      color = "black", fill = stim_bg_color
    ) +
    annotate(
      geom = "rect", xmin = 17, xmax = 37, ymin = 61, ymax = 81,
      color = "black", fill = stim_bg_color
    ) +
    annotate(
      geom = "text", x = -27, y = 65.3, label = "+", size = 20 / .pt,
      family = "Helvetica"
    ) +
    annotate(
      geom = "text", x = -9, y = 55, label = "message\nsignaling",
      family = "Helvetica", color = condition_colors[2]
    ) +
    annotate(
      geom = "text", x = -9, y = 79, label = "potatoes\nmashing",
      family = "Helvetica", color = condition_colors[1]
    ) +
    annotate(
      geom = "text", x = 9, y = 67, label = "*", size = 30 / .pt,
      family = "Helvetica"
    ) +
    draw_image(example_stim, x = 18, y = 62, width = 18, height = 18) +
    # Timings
    annotate(
      geom = "text", x = -9, y = 67.3, label = "or", family = "Helvetica"
    ) +
    annotate(
      geom = "text", x = -27, y = 52, label = "0.5 s", family = "Helvetica"
    ) +
    annotate(
      geom = "text", x = -9, y = 42, label = "2.5 s", family = "Helvetica"
    ) +
    annotate(
      geom = "text", x = 9, y = 56, label = "0.5 s", family = "Helvetica"
    ) +
    annotate(
      geom = "text", x = 27, y = 56.3, label = "3 s or\nresponse",
      family = "Helvetica", lineheight = 1
    ) +
    # Response options
    annotate(geom = "segment", x = 27, y = 39, xend = 27, yend = 51.5) +
    annotate(geom = "segment", x = 0, y = 39, xend = 27, yend = 39) +
    annotate(geom = "segment", x = 0, y = 35, xend = 0, yend = 39) +
    annotate(geom = "segment", x = -32, y = 35, xend = 32, yend = 35) +
    annotate(geom = "segment", x = -32, y = 30.5, xend = -32, yend = 35) +
    annotate(geom = "segment", x = -12, y = 32, xend = -12, yend = 35) +
    annotate(geom = "segment", x = 12, y = 32, xend = 12, yend = 35) +
    annotate(geom = "segment", x = 32, y = 32, xend = 32, yend = 35) +
    annotate(
      geom = "text", x = -32, y = 26,
      label = "A. \"I know\nwhat this is\"",
      family = "Helvetica", lineheight = 1
    ) +
    annotate(
      geom = "text", x = -12, y = 26,
      label = "B. \"I rather have\nan assumption\nwhat this is\"",
      family = "Helvetica", lineheight = 1
    ) +
    annotate(
      geom = "text", x = 12, y = 26,
      label = "C. \"I rather have\nno assumption\nwhat this is\"",
      family = "Helvetica", lineheight = 1
    ) +
    annotate(
      geom = "text", x = 32, y = 26,
      label = "D. \"I don\'t\n know what\nthis is\"",
      family = "Helvetica", lineheight = 1
    ) +
    # Conditions
    annotate(
      geom = "segment", x = -32, y = 17, xend = -32, yend = 22,
      color = condition_colors[1]
    ) +
    annotate(
      geom = "segment", x = -12, y = 17, xend = -12, yend = 20,
      color = condition_colors[1]
    ) +
    annotate(
      geom = "segment", x = 12, y = 17, xend = 12, yend = 20,
      color = condition_colors[2]
    ) +
    annotate(
      geom = "segment", x = 32, y = 17, xend = 32, yend = 20,
      color = condition_colors[2]
    ) +
    annotate(
      geom = "segment", x = -32, y = 17, xend = -12, yend = 17,
      color = condition_colors[1]
    ) +
    annotate(
      geom = "segment", x = 12, y = 17, xend = 32, yend = 17,
      color = condition_colors[2]
    ) +
    annotate(
      geom = "segment", x = -22, y = 15, xend = -22, yend = 17,
      color = condition_colors[1]
    ) +
    annotate(
      geom = "segment", x = 22, y = 15, xend = 22, yend = 17,
      color = condition_colors[2]
    ) +
    annotate(
      geom = "text", x = -22, y = 13, label = "Informed condition",
      family = "Helvetica", fontface = "bold", color = condition_colors[1]
    ) +
    annotate(
      geom = "text", x = 22, y = 13, label = "Uninformed condition",
      family = "Helvetica", fontface = "bold", color = condition_colors[2]
    ) +
    annotate(
      geom = "text", x = -22, y = 7.9,
      label = "Matching keywords\nand response A or B",
      family = "Helvetica", color = condition_colors[1], lineheight = 1
    ) +
    annotate(
      geom = "text", x = 22, y = 7.9,
      label = "Non-matching keywords\nand response C or D",
      family = "Helvetica", color = condition_colors[2], lineheight = 1
    ) +
    ## PRE-INSIGHT PHASE ##
    # Title
    annotate(
      geom = "text", x = -72, y = 56, label = "Pre-insight phase",
      size = 14 / .pt, family = "Helvetica", fontface = "bold"
    ) +
    # Arrow
    annotate(
      geom = "segment", x = -84, xend = -66, y = 46, yend = 48,
      arrow = arrow(length = unit(0.1, "inches"))
    ) +
    # Squares
    annotate(
      geom = "rect", xmin = -91, xmax = -71, ymin = 22, ymax = 42,
      color = "black", fill = stim_bg_color
    ) +
    annotate(
      geom = "rect", xmin = -53, xmax = -73, ymin = 24, ymax = 44,
      color = "black", fill = stim_bg_color
    ) +
    annotate(
      geom = "text", x = -81, y = 32.3, label = "+", size = 20 / .pt,
      family = "Helvetica"
    ) +
    draw_image(example_stim, x = -72, y = 25, width = 18, height = 18) +
    # Timings
    annotate(
      geom = "text", x = -81, y = 19, label = "0.5 s", family = "Helvetica"
    ) +
    annotate(
      geom = "text", x = -63, y = 19.3, label = "3 s or\nresponse",
      family = "Helvetica", lineheight = 1
    ) +
    # Conditions
    annotate(
      geom = "text", x = -72, y = 7.9,
      label = paste(
        "Objects classified according",
        "to conditions in the insight phase",
        sep = "\n"
      ),
      family = "Helvetica", lineheight = 1
    ) +
    ## POST-INSIGHT PHASE ##
    # Title
    annotate(
      geom = "text", x = 72, y = 56, label = "Post-insight phase",
      size = 14 / .pt, family = "Helvetica", fontface = "bold"
    ) +
    # Arrow
    annotate(
      geom = "segment", x = 60, xend = 78, y = 46, yend = 48,
      arrow = arrow(length = unit(0.1, "inches"))
    ) +
    # Squares
    annotate(
      geom = "rect", xmin = 53, xmax = 73, ymin = 22, ymax = 42,
      color = "black", fill = stim_bg_color
    ) +
    annotate(
      geom = "rect", xmin = 71, xmax = 91, ymin = 24, ymax = 44,
      color = "black", fill = stim_bg_color
    ) +
    annotate(
      geom = "text", x = 63, y = 32.3, label = "+", size = 20 / .pt,
      family = "Helvetica"
    ) +
    draw_image(example_stim, x = 72, y = 25, width = 18, height = 18) +
    # Timings
    annotate(
      geom = "text", x = 63, y = 19, label = "0.5 s", family = "Helvetica"
    ) +
    annotate(
      geom = "text", x = 81, y = 19.3, label = "3 s or\nresponse",
      family = "Helvetica", lineheight = 1
    ) +
    # Conditions
    annotate(
      geom = "text", x = 72, y = 7.9,
      label = paste(
        "Objects classified according",
        "to conditions in the insight phase",
        sep = "\n"
      ),
      family = "Helvetica", lineheight = 1
    )
}

plot_fig2a <- function(files_dir, condition_colors) {
  #' Creates a plot with the condition assignment for Figure 2

  require(tidyverse)
  require(cowplot)

  ggplot() +
    coord_cartesian(xlim = c(-100, 100), ylim = c(0, 42), expand = FALSE) +
    theme_void() +
    theme(aspect.ratio = 0.21) +
    ## INSIGHT PHASE ##
    # Title
    annotate(
      geom = "text", x = 0, y = 40, label = "Insight phase", size = 14 / .pt,
      family = "Helvetica", fontface = "bold"
    ) +
    # Response options
    annotate(geom = "segment", x = -32, y = 35, xend = 32, yend = 35) +
    annotate(geom = "segment", x = -32, y = 30.5, xend = -32, yend = 35) +
    annotate(geom = "segment", x = -12, y = 32, xend = -12, yend = 35) +
    annotate(geom = "segment", x = 12, y = 32, xend = 12, yend = 35) +
    annotate(geom = "segment", x = 32, y = 32, xend = 32, yend = 35) +
    annotate(
      geom = "text", x = -32, y = 26,
      label = "A. \"I know\nwhat this is\"",
      family = "Helvetica", lineheight = 1
    ) +
    annotate(
      geom = "text", x = -12, y = 26,
      label = "B. \"I rather have\nan assumption\nwhat this is\"",
      family = "Helvetica", lineheight = 1
    ) +
    annotate(
      geom = "text", x = 12, y = 26,
      label = "C. \"I rather have\nno assumption\nwhat this is\"",
      family = "Helvetica", lineheight = 1
    ) +
    annotate(
      geom = "text", x = 32, y = 26,
      label = "D. \"I don\'t\n know what\nthis is\"",
      family = "Helvetica", lineheight = 1
    ) +
    # Conditions
    annotate(
      geom = "segment", x = -32, y = 17, xend = -32, yend = 22,
      color = condition_colors[1]
    ) +
    annotate(
      geom = "segment", x = -12, y = 17, xend = -12, yend = 20,
      color = condition_colors[1]
    ) +
    annotate(
      geom = "segment", x = 12, y = 17, xend = 12, yend = 20,
      color = condition_colors[2]
    ) +
    annotate(
      geom = "segment", x = 32, y = 17, xend = 32, yend = 20,
      color = condition_colors[2]
    ) +
    annotate(
      geom = "segment", x = -32, y = 17, xend = -12, yend = 17,
      color = condition_colors[1]
    ) +
    annotate(
      geom = "segment", x = 12, y = 17, xend = 32, yend = 17,
      color = condition_colors[2]
    ) +
    annotate(
      geom = "segment", x = -22, y = 15, xend = -22, yend = 17,
      color = condition_colors[1]
    ) +
    annotate(
      geom = "segment", x = 22, y = 15, xend = 22, yend = 17,
      color = condition_colors[2]
    ) +
    annotate(
      geom = "text", x = -22, y = 13, label = "Informed condition",
      family = "Helvetica", fontface = "bold", color = condition_colors[1]
    ) +
    annotate(
      geom = "text", x = 22, y = 13, label = "Unsuccessful condition",
      family = "Helvetica", fontface = "bold", color = condition_colors[2]
    ) +
    annotate(
      geom = "text", x = -22, y = 7.9,
      label = "Matching keywords\nand response A or B",
      family = "Helvetica", color = condition_colors[1], lineheight = 1
    ) +
    annotate(
      geom = "text", x = 22, y = 7.9,
      label = "Matching keywords\nand response C or D",
      family = "Helvetica", color = condition_colors[2], lineheight = 1
    ) +
    ## PRE-INSIGHT PHASE ##
    # Title
    annotate(
      geom = "text", x = -72, y = 15, label = "Pre-insight phase",
      size = 14 / .pt, family = "Helvetica", fontface = "bold"
    ) +
    # Conditions
    annotate(
      geom = "text", x = -72, y = 7.9,
      label = paste(
        "Objects classified according",
        "to conditions in the insight phase",
        sep = "\n"
      ),
      family = "Helvetica", lineheight = 1
    ) +
    ## POST-INSIGHT PHASE ##
    # Title
    annotate(
      geom = "text", x = 72, y = 15, label = "Post-insight phase",
      size = 14 / .pt, family = "Helvetica", fontface = "bold"
    ) +
    # Conditions
    annotate(
      geom = "text", x = 72, y = 7.9,
      label = paste(
        "Objects classified according",
        "to conditions in the insight phase",
        sep = "\n"
      ),
      family = "Helvetica", lineheight = 1
    )
}

plot_erps <- function(evokeds,
                      config,
                      channel_locations,
                      models,
                      conditions,
                      condition_colors,
                      topo_palette) {
  #' Plots ERP time courses (waveforms) and topographies for two conditions

  require(tidyverse)
  require(cowplot)

  components <- config$components
  components$model <- models[components$name]
  pmap(components, function(name, tmin, tmax, roi, model) {
    phases <- c("Pre-insight", "Insight", "Post-insight")
    map(phases, function(this_phase) {
      xmin <- -0.2
      xmax <- 0.8
      xstep <- 0.2
      xticks <- seq(xmin, xmax, by = xstep)
      xticks <- xticks[!xticks == 0]
      yticks <- c(-3, 3)

      evokeds_plot <- filter(
        evokeds,
        phase == this_phase &
          condition %in% conditions &
          between(time, xmin, xmax)
      )

      evokeds_tc <- evokeds_plot %>%
        mutate(dep = evokeds_plot[[name]]) %>%
        Rmisc::summarySEwithin(
          measurevar = "dep",
          withinvars = c("time", "phase", "condition"),
          idvar = "participant_id"
        ) %>%
        mutate(time = as.numeric(levels(time))[time])

      contrast_conditions <- str_c(conditions, collapse = " - ")
      p_value <- as_tibble(model$contrasts) %>%
        filter(phase == this_phase & contrast == contrast_conditions) %>%
        pull(p.value)
      asterisks <- case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ ""
      )

      wave <- ggplot(
        evokeds_tc,
        aes(
          x = time,
          y = dep,
          ymin = dep - se,
          ymax = dep + se,
          color = condition,
          fill = condition
        )
      ) +
        annotate(
          geom = "rect", xmin = tmin, xmax = tmax, ymin = -Inf, ymax = Inf,
          fill = "gray90"
        ) +
        annotate(
          geom = "text", x = (tmin + tmax) / 2, y = 7.9, label = asterisks,
          size = 5
        ) +
        annotate(
          geom = "segment", x = min(xticks), y = 0, xend = max(xticks),
          yend = 0
        ) +
        annotate(
          geom = "segment", x = xticks, y = 0, xend = xticks, yend = -0.2
        ) +
        annotate(
          geom = "text", x = xticks, y = -0.6, label = xticks
        ) +
        annotate(
          geom = "text", x = mean(xticks), y = -1.5, label = "Time (s)"
        ) +
        annotate(
          geom = "segment", x = 0, y = min(yticks), xend = 0,
          yend = max(yticks)
        ) +
        annotate(
          geom = "segment", x = -0.01, y = yticks, xend = 0, yend = yticks
        ) +
        annotate(
          geom = "text", x = -0.02, y = yticks, label = yticks, hjust = 1
        ) +
        annotate(
          geom = "text", x = -0.06, y = -0.2, label = "Ampl. (µV)", angle = 90
        ) +
        geom_ribbon(color = NA, alpha = 0.2) +
        geom_line(size = 0.8) +
        scale_color_manual(
          values = condition_colors,
          labels = str_c(conditions, "condition", sep = " "),
          aesthetics = c("color", "fill")
        ) +
        coord_cartesian(ylim = c(-3, 8), expand = TRUE) +
        theme_void() +
        theme(legend.position = "none")

      wave_legend <<- get_legend(
        wave +
          guides(
            color = guide_legend(title = "Conditions"),
            fill = guide_legend(title = "Conditions"),
          ) +
          theme(
            legend.position = "right",
            legend.title = element_blank(),
            legend.text = element_text(family = "Helvetica", size = 10)
          )
      )

      channels <- channel_locations$channel
      evokeds_plus <- filter(evokeds_plot, condition == conditions[1])
      evokeds_minus <- filter(evokeds_plot, condition == conditions[2])
      evokeds_diff <- evokeds_plus
      evokeds_diff[channels] <-
        evokeds_plus[channels] - evokeds_minus[channels]
      evokeds_diff$condition <- "Difference"

      topo <- evokeds_diff %>%
        filter(between(time, tmin, tmax)) %>%
        pivot_longer(
          all_of(channels),
          names_to = "electrode", values_to = "amplitude"
        ) %>%
        group_by(electrode) %>%
        summarise(amplitude = mean(amplitude)) %>%
        left_join(channel_locations, by = c("electrode" = "channel")) %>%
        eegUtils::topoplot(
          r = 85,
          contour = FALSE,
          interp_limit = "skirt",
          highlights = roi,
          scaling = 0.9
        ) +
        scale_fill_distiller(
          palette = topo_palette,
          limits = c(-1.0, 1.0),
          oob = scales::squish
        ) +
        theme(legend.position = "none")
      topo$layers[[3]]$aes_params$size <- 0.5 # Adjust head and channel markers
      topo$layers[[4]]$aes_params$size <- 0.5
      topo$layers[[5]]$aes_params$size <- 0.5
      topo$layers[[6]]$aes_params$colour <- NA
      topo$layers[[7]]$aes_params$size <- 0.4
      topo$layers[[7]]$aes_params$colour <- "black"

      colorbar_title <- str_c(
        conditions[1], " -\n", str_to_lower(conditions[2]), "\nampl. (µV)"
      )
      colorbar <<- get_legend(
        topo +
          guides(
            fill = guide_colorbar(
              title.hjust = 0.5,
              title = colorbar_title,
              title.position = "left",
              barheight = 6.5,
              ticks = FALSE
            )
          ) +
          theme(
            legend.position = "right",
            legend.title = element_text(
              size = 10, angle = 90, family = "Helvetica",
            ),
            legend.text = element_text(size = 10, family = "Helvetica"),
          )
      )

      wave + draw_plot(
        plot = topo, x = tmin - 0.47, y = 2.7, width = 0.6, height = 6.0
      )
    }) %>%
      plot_grid(plotlist = ., nrow = 1)
  }) %>%
    plot_grid(
      plotlist = ., ncol = 1, labels = c("B", "C", "D"), label_size = 14,
      label_fontfamily = "Helvetica"
    )
}

plot_fig1 <- function(files_dir,
                      evokeds,
                      config,
                      channel_locations,
                      models,
                      conditions = c("Informed", "Uninformed"),
                      condition_colors = c("#ca0020", "#0571b0"),
                      topo_palette = "RdBu") {
  #' Combines trial structure + ERP plot for Figure 1 (informed - uninformed)

  require(tidyverse)
  require(cowplot)

  plot_grid(
    plot_fig1a(files_dir, condition_colors),
    plot_erps(
      evokeds,
      config,
      channel_locations,
      models,
      conditions,
      condition_colors,
      topo_palette
    ),
    ncol = 1, labels = c("A", NULL), label_size = 14,
    label_fontfamily = "Helvetica"
  ) +
    draw_plot(wave_legend, x = 0.323, y = 0.434) +
    draw_plot(colorbar, x = 0.45, y = 0.434) +
    annotate("segment", x = 0.331, xend = 0.331, y = -Inf, yend = 0.48) +
    annotate("segment", x = 0.665, xend = 0.665, y = -Inf, yend = 0.48) +
    annotate("segment", x = 0.331, xend = 0.274, y = 0.48, yend = 0.55) +
    annotate("segment", x = 0.665, xend = 0.726, y = 0.48, yend = 0.55) +
    annotate("segment", x = 0.274, xend = 0.274, y = 0.55, yend = Inf) +
    annotate("segment", x = 0.726, xend = 0.726, y = 0.55, yend = Inf)
}

plot_fig2 <- function(files_dir,
                      evokeds,
                      config,
                      channel_locations,
                      models,
                      conditions = c("Informed", "Unsuccessful"),
                      condition_colors = c("#ca0020", "#0571b0"),
                      topo_palette = "RdBu") {
  #' Combines trial structure + ERP plot for Figure 2 (informed - unsuccessful)

  require(tidyverse)
  require(cowplot)

  plot_grid(
    plot_fig2a(files_dir, condition_colors),
    plot_erps(
      evokeds,
      config,
      channel_locations,
      models,
      conditions,
      condition_colors,
      topo_palette
    ),
    ncol = 1, rel_heights = c(0.42, 1.0), labels = c("A", NULL),
    label_size = 14, label_fontfamily = "Helvetica"
  ) +
    draw_plot(wave_legend, x = 0.323, y = 0.41) +
    draw_plot(colorbar, x = 0.45, y = 0.41) +
    annotate("segment", x = 0.331, xend = 0.331, y = -Inf, yend = 0.68) +
    annotate("segment", x = 0.665, xend = 0.665, y = -Inf, yend = 0.68) +
    annotate("segment", x = 0.331, xend = 0.274, y = 0.68, yend = 0.75) +
    annotate("segment", x = 0.665, xend = 0.726, y = 0.68, yend = 0.75) +
    annotate("segment", x = 0.274, xend = 0.274, y = 0.75, yend = Inf) +
    annotate("segment", x = 0.726, xend = 0.726, y = 0.75, yend = Inf)
}
