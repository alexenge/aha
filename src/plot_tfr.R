plot_tfr_topos <- function(tfr_grand_ave,
                           tfr_clusters,
                           p_cluster,
                           conditions = c("Informed", "Uninformed"),
                           condition_colors = c("#ca0020", "#0571b0"),
                           topo_palette = "RdBu",
                           tmin = -0.4,
                           tmax = 1.4,
                           tstep = 0.2,
                           fmin = 4,
                           fmax = 40,
                           fstep = 4) {
  #' Plots a grid of topographies for all time windows and frequency bins

  require(dplyr)
  require(cowplot)

  tmins <- seq(tmin, tmax - tstep, tstep)
  fmins <- seq(fmin, fmax - fstep, fstep)

  channels <- unique(tfr_clusters$channel)
  tfr_grand_ave_plus <- filter(tfr_grand_ave, condition == conditions[1])
  tfr_grand_ave_minus <- filter(tfr_grand_ave, condition == conditions[2])
  tfr_grand_ave_diff <- mutate(tfr_grand_ave_plus, condition = "Difference")
  tfr_grand_ave_diff[channels] <- tfr_grand_ave_plus[channels] -
    tfr_grand_ave_minus[channels]

  plotlist <- map(tmins, function(tmin) {
    tmax <- tmin + tstep

    plotlist <- map(fmins, function(fmin) {
      fmax <- fmin + fstep

      significant_channels <- tfr_clusters %>%
        filter(time >= tmin & time < tmax & freq >= fmin & freq < fmax) %>%
        filter(p_val < p_cluster) %>%
        pull(channel) %>%
        unique()

      topo <- tfr_grand_ave_diff %>%
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
        scale_fill_distiller(limits = c(-25.0, 25.0), palette = topo_palette) +
        theme(
          plot.margin = unit(rep(-0.2, 4), "cm"),
          legend.position = "none"
        )
      colorbar <<- get_legend(
        topo +
          guides(
            fill = guide_colorbar(
              title.hjust = 0.5,
              title.vjust = 0.6,
              title = paste(
                paste(conditions[1], "-", str_to_lower(conditions[2])),
                "power",
                "(% signal change)",
                sep = "\n"
              ),
              title.position = "left",
              barheight = 1.5,
              barwidth = 8.0,
              ticks = FALSE
            )
          ) +
          theme(
            legend.box.background = element_rect(fill = "white", color = NA),
            legend.box.margin = margin(20, 18, 20, 18),
            legend.position = "top",
            legend.title = element_text(size = 10, family = "Helvetica"),
            legend.text = element_text(size = 10, family = "Helvetica"),
          )
      )

      topo$layers[[3]]$aes_params$size <- 0.7
      topo$layers[[4]]$aes_params$size <- 0.7
      topo$layers[[5]]$aes_params$size <- 0.7
      topo$layers[[6]]$aes_params$colour <- NA
      topo$layers[[7]]$aes_params$size <- 0.4
      topo$layers[[7]]$aes_params$colour <- "black"
      topo
    })

    plotlist <- c(rev(plotlist), list(NULL))
    rel_heights <- c(rep(1, length(fmins)), 0.15)

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
  })

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
    draw_plot(colorbar, x = -0.23, y = 0.445, width = 0.9)
}
