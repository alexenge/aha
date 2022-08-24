aha_colors <- c("#ca0020", "#0571b0")

plot_fig1a <- function(files_dir) {

  # Make sure that packages are loaded
  require(tidyverse)
  require(cowplot)

  # Trial structure
  example_stim <- magick::image_read(here::here(files_dir, "example_stim.png"))
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
      color = "black", fill = "white"
    ) +
    annotate(
      geom = "rect", xmin = -19, xmax = 1, ymin = 45, ymax = 65,
      color = aha_colors[2], fill = "white"
    ) +
    annotate(
      geom = "rect", xmin = -19, xmax = 1, ymin = 69, ymax = 89,
      color = aha_colors[1], fill = "white"
    ) +
    annotate(
      geom = "rect", xmin = -1, xmax = 19, ymin = 59, ymax = 79,
      color = "black", fill = "white"
    ) +
    annotate(
      geom = "rect", xmin = 17, xmax = 37, ymin = 61, ymax = 81,
      color = "black", fill = "white"
    ) +
    annotate(
      geom = "text", x = -27, y = 65.3, label = "+", size = 20 / .pt,
      family = "Helvetica"
    ) +
    annotate(
      geom = "text", x = -9, y = 55, label = "message\nsignaling",
      family = "Helvetica", color = aha_colors[2]
    ) +
    annotate(
      geom = "text", x = -9, y = 79, label = "potatoes\nmashing",
      family = "Helvetica", color = aha_colors[1]
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
      label = "B. \"I have an\nassumption\nwhat this is\"",
      family = "Helvetica", lineheight = 1
    ) +
    annotate(
      geom = "text", x = 12, y = 26,
      label = "C. \"I have rather\nno assumption\nwhat this is\"",
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
      color = aha_colors[1]
    ) +
    annotate(
      geom = "segment", x = -12, y = 17, xend = -12, yend = 20,
      color = aha_colors[1]
    ) +
    annotate(
      geom = "segment", x = 12, y = 17, xend = 12, yend = 20,
      color = aha_colors[2]
    ) +
    annotate(
      geom = "segment", x = 32, y = 17, xend = 32, yend = 20,
      color = aha_colors[2]
    ) +
    annotate(
      geom = "segment", x = -32, y = 17, xend = -12, yend = 17,
      color = aha_colors[1]
    ) +
    annotate(
      geom = "segment", x = 12, y = 17, xend = 32, yend = 17,
      color = aha_colors[2]
    ) +
    annotate(
      geom = "segment", x = -22, y = 15, xend = -22, yend = 17,
      color = aha_colors[1]
    ) +
    annotate(
      geom = "segment", x = 22, y = 15, xend = 22, yend = 17,
      color = aha_colors[2]
    ) +
    annotate(
      geom = "text", x = -22, y = 13, label = "Informed condition",
      family = "Helvetica", fontface = "bold", color = aha_colors[1]
    ) +
    annotate(
      geom = "text", x = 22, y = 13, label = "Naive condition",
      family = "Helvetica", fontface = "bold", color = aha_colors[2]
    ) +
    annotate(
      geom = "text", x = -22, y = 7.9,
      label = "Matching keywords\nand response A or B",
      family = "Helvetica", color = aha_colors[1], lineheight = 1
    ) +
    annotate(
      geom = "text", x = 22, y = 7.9,
      label = "Non-matching keywords\nand response C or D",
      family = "Helvetica", color = aha_colors[2], lineheight = 1
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
      color = "black", fill = "white"
    ) +
    annotate(
      geom = "rect", xmin = -53, xmax = -73, ymin = 24, ymax = 44,
      color = "black", fill = "white"
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
      color = "black", fill = "white"
    ) +
    annotate(
      geom = "rect", xmin = 71, xmax = 91, ymin = 24, ymax = 44,
      color = "black", fill = "white"
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

plot_fig1b <- function(evokeds, config, channel_locations, models) {

  # Make sure that packages are loaded
  require(tidyverse)
  require(cowplot)

  # Loop over ERP components of interest
  components <- config$components
  components$model <- models
  pmap(components, function(name, tmin, tmax, roi, model) {

    # Loop over phases of the experiment
    phases <- c("Pre-insight", "Insight", "Post-insight")
    map(phases, function(this_phase) {

      # Define axis limits and labels
      xmin <- -0.2
      xmax <- 0.8
      xstep <- 0.2
      xticks <- seq(xmin, xmax, by = xstep)
      xticks <- xticks[!xticks == 0]
      yticks <- c(-3, 3)

      # Select dependent variable, conditions, and time window
      evokeds %>%
        filter(phase == this_phase) %>%
        filter(condition %in% c("Informed", "Naive")) %>%
        filter(between(time, xmin, xmax)) -> evokeds_plot

      # Create means and SEs across participants for time course
      evokeds_plot %>%
        mutate(dep = evokeds_plot[[name]]) %>%
        Rmisc::summarySEwithin(
          measurevar = "dep",
          withinvars = c("time", "phase", "condition"),
          idvar = "participant_id"
        ) %>%
        mutate(time = as.numeric(levels(time))[time]) -> evokeds_tc

      # Extract significance from model
      as_tibble(model$contrasts) %>%
        filter(phase == this_phase) %>%
        pull(p.value) -> p_value
      asterisks <- case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ ""
      )

      # Plot time course
      ggplot(
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
        # Annotate component time window
        annotate(
          geom = "rect", xmin = tmin, xmax = tmax, ymin = -Inf, ymax = Inf,
          fill = "gray90"
        ) +
        annotate(
          geom = "text", x = (tmin + tmax) / 2, y = 7.9, label = asterisks
        ) +
        # X (time) axis
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
          geom = "text", x = mean(xticks), y = -1.5, label = "Time (ms)"
        ) +
        # Y (amplitude) axis
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
        # Data
        geom_ribbon(color = NA, alpha = 0.2) +
        geom_line(size = 0.8) +
        # Styling
        scale_color_manual(
          values = aha_colors,
          labels = c("Informed condition", "Naive condition"),
          aesthetics = c("color", "fill")
        ) +
        coord_cartesian(ylim = c(-3, 8), expand = TRUE) +
        theme_void() +
        theme(legend.position = "none") -> wave

      # Extract legend
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

      # Compute difference between conditions for topography
      channels <- channel_locations$channel
      evokeds_informed <- filter(evokeds_plot, condition == "Informed")
      evokeds_naive <- filter(evokeds_plot, condition == "Naive")
      evokeds_diff <- evokeds_informed
      evokeds_diff[channels] <-
        evokeds_informed[channels] - evokeds_naive[channels]
      evokeds_diff$condition <- "Difference"

      # Plot topography
      evokeds_diff %>%
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
        # scale_fill_viridis_c(
        #   option = "viridis", limits = c(-1, 1), oob = scales::squish
        # ) +
        scale_fill_distiller(
          palette = "RdBu", limits = c(-1, 1), oob = scales::squish
        ) +
        theme(legend.position = "none") -> topo
      # Adjust size of head elements and channel markers
      topo$layers[[3]]$aes_params$size <- 0.5
      topo$layers[[4]]$aes_params$size <- 0.5
      topo$layers[[5]]$aes_params$size <- 0.5
      topo$layers[[6]]$aes_params$colour <- NA
      topo$layers[[7]]$aes_params$size <- 0.4
      topo$layers[[7]]$aes_params$colour <- "black"

      # Extract colorbar
      colorbar <<- get_legend(
        topo +
          guides(
            fill = guide_colorbar(
              title.hjust = 0.5,
              title = "Informed - naive\nampl. (µV)",
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

      # Combine time course and topography
      wave + draw_plot(topo, x = -0.4, y = 2.7, width = 0.6, height = 6.0)
    }) %>%
      # Combine plots for the current ERP component and all three phases
      plot_grid(plotlist = ., nrow = 1)
  }) %>%
    # Combine plots for all ERP components
    plot_grid(
      plotlist = ., ncol = 1, labels = c("B", "C", "D"), label_size = 14,
      label_fontfamily = "Helvetica"
    )
}

plot_fig1 <- function(files_dir, evokeds, config, channel_locations, models) {

  # Make sure that packages are loaded
  require(tidyverse)
  require(cowplot)

  # Plot 2 main panels
  plot_grid(
    plot_fig1a(files_dir),
    plot_fig1b(evokeds, config, channel_locations, models),
    ncol = 1, labels = c("A", NULL), label_size = 14,
    label_fontfamily = "Helvetica"
  ) +
    # Add legend and colorbar
    draw_plot(wave_legend, x = 0.330, y = 0.434) +
    draw_plot(colorbar, x = 0.454, y = 0.434) +
    # Add some lines to separate the three experimental parts
    annotate("segment", x = 0.331, xend = 0.331, y = -Inf, yend = 0.48) +
    annotate("segment", x = 0.665, xend = 0.665, y = -Inf, yend = 0.48) +
    annotate("segment", x = 0.331, xend = 0.274, y = 0.48, yend = 0.55) +
    annotate("segment", x = 0.665, xend = 0.726, y = 0.48, yend = 0.55) +
    annotate("segment", x = 0.274, xend = 0.274, y = 0.55, yend = Inf) +
    annotate("segment", x = 0.726, xend = 0.726, y = 0.55, yend = Inf)
}

# plot_fig1(files_dir, evokeds, config, channel_locations, models)
# ggsave("fig1.pdf", width = 12, height = 12)

plot_tfr_topos <- function(tfr_grand_ave,
                           tfr_clusters,
                           p_cluster,
                           condition_plus = "Informed",
                           condition_minus = "Naive",
                           tmin = -0.4,
                           tmax = 1.4,
                           tstep = 0.2,
                           fmin = 4,
                           fmax = 40,
                           fstep = 4) {

  # Load required packages
  require(dplyr)
  require(cowplot)

  # Compute time bins and frequency bins to plot
  tmins <- seq(tmin, tmax - tstep, tstep)
  fmins <- seq(fmin, fmax - fstep, fstep)

  # Compute difference in power between conditions
  channels <- unique(tfr_clusters$channel)
  tfr_grand_ave_informed <- filter(tfr_grand_ave, condition == "Informed")
  tfr_grand_ave_naive <- filter(tfr_grand_ave, condition == "Naive")
  tfr_grand_ave_diff <- mutate(tfr_grand_ave_naive, condition = "Difference")
  tfr_grand_ave_diff[channels] <- tfr_grand_ave_informed[channels] -
    tfr_grand_ave_naive[channels]

  # Plot each time bin as a column of topographies
  map(tmins, function(tmin) {
    tmax <- tmin + tstep

    # Plot each frequency bin as one topography
    map(fmins, function(fmin) {
      fmax <- fmin + fstep

      # Extract significant channels from cluster-based permutation tests
      tfr_clusters %>%
        filter(time >= tmin & time < tmax & freq >= fmin & freq < fmax) %>%
        filter(p_val < p_cluster) %>%
        pull(channel) %>%
        unique() -> significant_channels

      # Plot topography based on grand-averaged data
      tfr_grand_ave_diff %>%
        filter(time >= tmin & time < tmax & freq >= fmin & freq < fmax) %>%
        pivot_longer(
          cols = all_of(channels), names_to = "electrode",
          values_to = "power"
        ) %>%
        mutate(power = power * 100) %>% # Convert from decimal to percent
        left_join(channel_locations, by = c("electrode" = "channel")) %>%
        group_by(electrode, x, y) %>%
        summarise(power = mean(power), .groups = "drop") %>%
        eegUtils::topoplot(
          r = 85,
          quantity = "power",
          contour = FALSE,
          interp_limit = "skirt",
          highlights = significant_channels,
          scaling = 1.1
        ) +
        scale_fill_distiller(limits = c(-25, 25), palette = "RdBu") +
        # scale_fill_viridis_c(limits = c(-0.25, 0.25)) +
        theme(
          plot.margin = unit(rep(-0.2, 4), "cm"),
          legend.position = "none"
        ) -> topo
      colorbar <<- get_legend(
        topo +
          guides(
            fill = guide_colorbar(
              title.hjust = 0.5,
              title.vjust = 0.6,
              title = "Informed - naive\npower\n(% signal change)",
              title.position = "left",
              barheight = 1.5,
              barwidth = 8.0,
              ticks = FALSE
            )
          ) +
          theme(
            legend.box.background = element_rect(
              fill = "white", color = NA
            ),
            legend.box.margin = margin(20, 30, 20, 30),
            legend.position = "top",
            legend.title = element_text(size = 10, family = "Helvetica"),
            legend.text = element_text(size = 10, family = "Helvetica"),
          )
      )

      # Adjust size and color of some elements
      topo$layers[[3]]$aes_params$size <- 0.7
      topo$layers[[4]]$aes_params$size <- 0.7
      topo$layers[[5]]$aes_params$size <- 0.7
      topo$layers[[6]]$aes_params$colour <- NA
      topo$layers[[7]]$aes_params$size <- 0.4
      topo$layers[[7]]$aes_params$colour <- "black"
      topo
    }) -> plotlist

    # Combine all plots for the current time bin
    plotlist <- c(rev(plotlist), list(NULL))
    rel_heights <- c(rep(1, length(fmins)), 0.15)

    # Add frequency bin labels if this is the first time bin
    if (tmin == min(tmins)) {
      labels <- paste(fmins, "to", fmins + fstep, "Hz")
      labels <- c(rev(labels), "")
      plot_grid(
        plotlist = plotlist, nrow = length(plotlist),
        rel_heights = rel_heights, labels = labels,
        label_size = 10, label_fontfamily = "Helvetica",
        label_fontface = "plain", label_x = -0.3, label_y = 0.5, vjust = 0.5,
        hjust = 0.5
      )
    } else {
      plot_grid(
        plotlist = plotlist, nrow = length(plotlist),
        rel_heights = rel_heights
      )
    }
  }) -> plotlist

  # Combine plots from all time bins
  tmins_str <- format(tmins, trim = TRUE, nsmall = 1)
  tmaxs_str <- format(tmins + tstep, trim = TRUE, nsmall = 1)
  labels <- paste(tmins_str, "to", tmaxs_str, "s")
  plotlist <- c(list(NULL), plotlist)
  labels <- c("", labels)
  plot_grid(
    plotlist = plotlist, nrow = 1,
    rel_widths = c(0.6, rep(1, length(tmins))), labels = labels,
    label_size = 10, label_fontfamily = "Helvetica",
    label_fontface = "plain", label_x = 0.5, label_y = 0.01, vjust = 0.5,
    hjust = 0.5
  ) +
    draw_plot(colorbar, x = -0.28, y = 0.445)
}
