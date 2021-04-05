####################################################################################################
## FUNCTIONS TO CREATE PLOTS AND TABLES ##
####################################################################################################

# Function to plot the trial structure
plot_trials <- function(){
  suppressMessages(require(magick))
  color_informed <- viridisLite::viridis(1, begin = 0.1)
  color_naive <- viridisLite::viridis(1, begin = 0.5)
  ggplot() +
    coord_cartesian(xlim = c(-100, 100), ylim = c(0, 100), expand = FALSE) +
    ## PART II ##
    # Title
    annotate("text", x = 0, y = 97.2, label = "Insight part", size = 14 / .pt, family = "Helvetica", fontface = "bold") +
    # Arrow
    annotate("segment", x = -30, xend = 24, y = 79, yend = 85, arrow = arrow(length = unit(0.1, "inches"))) +
    # Squares
    annotate("rect", xmin = -37, xmax = -17, ymin = 55, ymax = 75, color = "black", fill = "white") +
    annotate("rect", xmin = -19, xmax = 1, ymin = 45, ymax = 65, color = color_informed, fill = "white") +
    annotate("rect", xmin = -19, xmax = 1, ymin = 69, ymax = 89, color = color_naive, fill = "white") +
    annotate("rect", xmin = -1, xmax = 19, ymin = 59, ymax = 79, color = "black", fill = "white") +
    annotate("rect", xmin = 17, xmax = 37, ymin = 61, ymax = 81, color = "black", fill = "white") +
    annotate("text", x = -27, y = 65.3, label = "+", size = 20 / .pt, family = "Helvetica") +
    annotate("text", x = -9, y = 55, label = "potatoes\nmashing", size = 10 / .pt, family = "Helvetica", color = color_informed) +
    annotate("text", x = -9, y = 79, label = "message\nsignaling", size = 10 / .pt, family = "Helvetica", color = color_naive) +
    annotate("text", x = 9, y = 67, label = "*", size = 30 / .pt, family = "Helvetica") +
    draw_image("analysis/manuscript_files/potato_masher.png", x = 18, y = 62, width = 18, height = 18) +
    # Timings
    annotate("text", x = -9, y = 67.3, label = "or", size = 10 / .pt, family = "Helvetica") +
    annotate("text", x = -27, y = 52, label = "0.5 s", size = 10 / .pt, family = "Helvetica") +
    annotate("text", x = -9, y = 42, label = "2.5 s", size = 10 / .pt, family = "Helvetica") +
    annotate("text", x = 9, y = 56, label = "0.5 s", size = 10 / .pt, family = "Helvetica") +
    annotate("text", x = 27, y = 56.3, label = "3 s or\nresponse", size = 10 / .pt, family = "Helvetica", lineheight = 1) +
    # Response options
    annotate("segment", x = 27, y = 39, xend = 27, yend = 51.5) +
    annotate("segment", x = 0, y = 39, xend = 27, yend = 39) +
    annotate("segment", x = 0, y = 35, xend = 0, yend = 39) +
    annotate("segment", x = -32, y = 35, xend = 32, yend = 35) +
    annotate("segment", x = -32, y = 30.5, xend = -32, yend = 35) +
    annotate("segment", x = -12, y = 32, xend = -12, yend = 35) +
    annotate("segment", x = 12, y = 32, xend = 12, yend = 35) +
    annotate("segment", x = 32, y = 32, xend = 32, yend = 35) +
    annotate("text", x = -32, y = 26, label = "A. \"I know\nwhat this is\"", size = 10 / .pt, family = "Helvetica", lineheight = 1) +
    annotate("text", x = -12, y = 26, label = "B. \"I have an\nassumption\nwhat this is\"", size = 10 / .pt, family = "Helvetica", lineheight = 1) +
    annotate("text", x = 12, y = 26, label = "C. \"I have rather\nno assumption\nwhat this is\"", size = 10 / .pt, family = "Helvetica", lineheight = 1) +
    annotate("text", x = 32, y = 26, label = "D. \"I don\'t\n know what\nthis is\"", size = 10 / .pt, family = "Helvetica", lineheight = 1) +
    # Conditions
    annotate("segment", x = -32, y = 17, xend = -32, yend = 22, color = color_informed) +
    annotate("segment", x = -12, y = 17, xend = -12, yend = 20, color = color_informed) +
    annotate("segment", x = 12, y = 17, xend = 12, yend = 20, color = color_naive) +
    annotate("segment", x = 32, y = 17, xend = 32, yend = 20, color = color_naive) +
    annotate("segment", x = -32, y = 17, xend = -12, yend = 17, color = color_informed) +
    annotate("segment", x = 12, y = 17, xend = 32, yend = 17, color = color_naive) +
    annotate("segment", x = -22, y = 15, xend = -22, yend = 17, color = color_informed) +
    annotate("segment", x = 22, y = 15, xend = 22, yend = 17, color = color_naive) +
    annotate("text", x = -22, y = 13, label = "Informed condition", size = 10 / .pt, family = "Helvetica", fontface = "bold", color = color_informed) +
    annotate("text", x = 22, y = 13, label = "Naive condition", size = 10 / .pt, family = "Helvetica", fontface = "bold", color = color_naive) +
    annotate("text", x = -22, y = 7.9, label = "Matching keywords\nand response A or B", size = 10 / .pt, family = "Helvetica", color = color_informed, lineheight = 1) +
    annotate("text", x = 22, y = 7.9, label = "Non-matching keywords\nand response C or D", size = 10 / .pt, family = "Helvetica", color = color_naive, lineheight = 1) +
    ## PART I ##
    # Title
    annotate("text", x = -72, y = 56, label = "Pre-insight part", size = 14 / .pt, family = "Helvetica", fontface = "bold") +
    # Arrow
    annotate("segment", x = -84, xend = -66, y = 46, yend = 48, arrow = arrow(length = unit(0.1, "inches"))) +
    # Squares
    annotate("rect", xmin = -91, xmax = -71, ymin = 22, ymax = 42, color = "black", fill = "white") +
    annotate("rect", xmin = -53, xmax = -73, ymin = 24, ymax = 44, color = "black", fill = "white") +
    annotate("text", x = -81, y = 32.3, label = "+", size = 20 / .pt, family = "Helvetica") +
    draw_image("analysis/manuscript_files/potato_masher.png", x = -72, y = 25, width = 18, height = 18) +
    # Timings
    annotate("text", x = -81, y = 19, label = "0.5 s", size = 10 / .pt, family = "Helvetica") +
    annotate("text", x = -63, y = 19.3, label = "3 s or\nresponse", size = 10 / .pt, family = "Helvetica", lineheight = 1) +
    # Conditions
    annotate("text", x = -72, y = 7.9, label = "Objects classified according\nto conditions in the insight part", size = 10 / .pt, family = "Helvetica", lineheight = 1) +
    # PART III
    # Title
    annotate("text", x = 72, y = 56, label = "Post-insight part", size = 14 / .pt, family = "Helvetica", fontface = "bold") +
    # Arrow
    annotate("segment", x = 60, xend = 78, y = 46, yend = 48, arrow = arrow(length = unit(0.1, "inches"))) +
    # Squares
    annotate("rect", xmin = 53, xmax = 73, ymin = 22, ymax = 42, color = "black", fill = "white") +
    annotate("rect", xmin = 71, xmax = 91, ymin = 24, ymax = 44, color = "black", fill = "white") +
    annotate("text", x = 63, y = 32.3, label = "+", size = 20 / .pt, family = "Helvetica") +
    draw_image("analysis/manuscript_files/potato_masher.png", x = 72, y = 25, width = 18, height = 18) +
    # Timings
    annotate("text", x = 63, y = 19, label = "0.5 s", size = 10 / .pt, family = "Helvetica") +
    annotate("text", x = 81, y = 19.3, label = "3 s or\nresponse", size = 10 / .pt, family = "Helvetica", lineheight = 1) +    
    # Conditions
    annotate("text", x = 72, y = 7.9, label = "Objects classified according\nto conditions in the insight part", size = 10 / .pt, family = "Helvetica", lineheight = 1) +    
    theme_void()
}

# Function to plot ERP waveforms and topographies
plot_erps <- function(comps, evokeds, models, montage) {
  suppressWarnings(suppressMessages(
    pmap(comps, function(name, tmin, tmax, roi) {
      # N400 gets a different scale than P1 and N170
      ymin <- ifelse(name == "N400", -3, -4)
      # Loop through parts
      parts <- map(c("I", "II", "III"), function(part) {
        # Create (conditions x time points) x electrodes tibble
        data_plot <- evokeds[[part]] %>%
          map(function(data) {
            data %>%
              t() %>%
              as_tibble(.name_repair = "unique") %>%
              set_colnames(montage$electrode) %>%
              mutate(.time = seq(-500, 1498, 2), roi = rowMeans(select(., all_of(roi))))
          }) %>% bind_rows(.id = "condition")
        # Shade background depending on whether the effect is significant or not
        asterisks <- models[[name]]$contrasts %>%
          filter(part == !!part) %>%
          mutate(asterisks = case_when(p.value < .001 ~ "***", p.value < .01 ~ "**", p.value < .05 ~ "*")) %>%
          pull(asterisks)
        xasterisks <- case_when(str_length(asterisks) == 1 ~ tmin + 11, str_length(asterisks) == 3 ~ tmin + 15)
        if (is.na(asterisks)) {
          shade <- annotate("rect", xmin = tmin, xmax = tmax, ymin = ymin + 0.1, ymax = ymin + 11.95, color = "black", fill = NA)
        } else {
          shade <- annotate("rect", xmin = tmin, xmax = tmax, ymin = ymin + 0.1, ymax = ymin + 11.95, color = "black", fill = "gray90")
        }
        # Plot waveform
        wave <- ggplot(data = data_plot, aes(x = .time, y = roi, color = condition)) +
          shade +
          annotate("text", label = asterisks, x = xasterisks, y = ymin + 11, size = 6, hjust = 0, family = "Helvetica") +
          annotate("segment", x = -200, xend = 800, y = 0, yend = 0) +
          annotate("segment", x = 0, xend = 0, y = ymin, yend = ymin + 12) +
          annotate("segment", x = seq(-100, 700, 200), xend = seq(-100, 700, 200), y = -0.3, yend = 0) +
          annotate("segment", x = -12, xend = 0, y = seq(-2, 8, 4), yend = seq(-2, 8, 4)) +
          annotate("text", x = seq(-100, 700, 200), y = -0.9, label = seq(-100, 700, 200), size = 10 / .pt, family = "Helvetica") +
          annotate("text", x = -20, y = seq(-2, 8, 4), label = seq(-2, 8, 4), size = 10 / .pt, family = "Helvetica", hjust = 1) +
          annotate("text", x = 400, y = ymin + 0.8, label = "Time (ms)", size = 10 / .pt, family = "Helvetica", lineheight = 0.9) +
          annotate("text", x = -100, y = 4, label = paste(name, "ampl.\n(µV)"), size = 10 / .pt, family = "Helvetica", angle = 90, lineheight = 0.9) +
          geom_line() +
          scale_color_viridis_d(begin = 0.1, end = 0.5) +
          coord_cartesian(xlim = c(-200, 800), ylim = c(ymin, ymin + 14), expand = FALSE) +
          theme_void() +
          theme(legend.position = "none")
        # Plot topography
        topo <- data_plot %>%
          filter(.time >= tmin & .time <= tmax) %>%
          group_by(condition) %>%
          summarise(across(montage$electrode, mean)) %>%
          pivot_longer(-condition) %>%
          pivot_wider(names_from = condition) %>%
          transmute(amplitude = Informed - Naive) %>%
          bind_cols(montage) %>%
          eegUtils::topoplot(limits = c(-1, 1), palette = "viridis", contour = FALSE, highlights = roi, scaling = 0.1) +
          theme(legend.position = "none")
        topo$layers[[3]]$aes_params$size <- topo$layers[[4]]$aes_params$size <- topo$layers[[5]]$aes_params$size <- 0.6
        # Combine waveform and topography
        wave + draw_plot(topo, width = 320, height = 9, x = 500, y = ymin + 6.4)
      })
      # Combine plots for the different parts
      plot_grid(parts[[1]], NULL, parts[[2]], NULL, parts[[3]], nrow = 1, rel_widths = c(10, 1, 10, 1, 10))
      # Combine plots for the different components
    }) %>% plot_grid(plotlist = ., nrow = 3)
  ))
}

# Function to create a combined legend and colorbar
plot_legends_erps <- function(direction) {
  if (direction == "vertical") {
    mar <- margin(l = 12, r = 10, b = 6, t = 7); vjust <- 3.5
  }
  else {
    mar <- margin(l = 8, r = 12, b = 4, t = 6); vjust <- 0.7
  }
  get_legend(
    tibble(amplitude = c(-1, 0, 1), condition = c("Informed", "Naive", "Informed")) %>%
      ggplot(aes(x = 0, y = 0, color = condition, fill = amplitude)) +
      geom_line() +
      geom_raster() +
      labs(color = "Conditions", fill = "Informed - naive (µV)") +
      scale_color_viridis_d(begin = 0.1, end = 0.5, guide = guide_legend(direction = "vertical", title.position = "top")) +
      scale_fill_viridis_c(guide = guide_colorbar(direction = "horizontal", ticks = FALSE, title.position = "top", title.vjust = vjust)) +
      theme(
        legend.direction = direction,
        legend.box = direction,
        legend.box.background = element_rect(colour = "black", size = 0.5),
        legend.box.margin = mar,
        legend.background = element_blank(),
        legend.title = element_text(family = "Helvetica", size = 10, face = "bold"),
        legend.text = element_text(family = "Helvetica", size = 10),
        legend.key = element_rect(fill = NA),
        legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.4, "cm"),
        legend.spacing.x = unit(0.3, "cm")
      )
  )
}

# Function to plot headings for Parts I, II, and III
plot_headings <- function(spacing){
  plot_grid(
    ggplot() + annotate("text", label = "Pre-insight part", x = 0, y = 0, size = 14 / .pt, family = "Helvetica", fontface = "bold") + theme_void(), NULL,
    ggplot() + annotate("text", label = "Insight part", x = 0, y = 0, size = 14 / .pt, family = "Helvetica", fontface = "bold") + theme_void(), NULL,
    ggplot() + annotate("text", label = "Post-insight part", x = 0, y = 0, size = 14 / .pt, family = "Helvetica", fontface = "bold") + theme_void(),
    nrow = 1, rel_widths = spacing
  )
}

# First helper function for split violins (kudos to https://stackoverflow.com/a/45614547)
GeomSplitViolin <- ggproto(
  "GeomSplitViolin", GeomViolin,
  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
    grp <- data[1, "group"]
    newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
    newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
    newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
      quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
    }
    else {
      ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  }
)

# Second helper function for split violins (kudos to https://stackoverflow.com/a/45614547)
geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ...,
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...)
  )
}

# Function for our custom violin plot
plot_violins <- function(data, models) {
  suppressWarnings(suppressMessages(
    parts <- map(c("I", "II", "III"), function(part) {
      map(c("P1", "N170", "N400"), function(comp) {
        modmeans <- models[[comp]]$means %>% filter(part == !!part) %>% rename(amplitude = emmean)
        ymin <- case_when(comp == "P1" ~ -5, comp == "N170" ~ -11, comp == "N400" ~ -11)
        asterisks <- models[[comp]]$contrasts %>%
          filter(part == !!part) %>%
          mutate(asterisks = case_when(p.value < .001 ~ "***", p.value < .01 ~ "**", p.value < .05 ~ "*")) %>%
          pull(asterisks)
        yastersisks <- case_when(
          comp == "P1" ~ 17.6,
          comp == "N170" ~ 10,
          comp == "N400" & part == "II" ~ 9.7,
          comp == "N400" & part == "III" ~ 8.9
        )
        data %>%
          filter(part == !!part) %>%
          rename(amplitude = !!comp) %>%
          group_by(participant, part, condition) %>%
          summarise(across(amplitude, mean)) %>%
          ggplot(aes(x = 0, y = amplitude, fill = condition)) +
          annotate("text", x = 0, y = yastersisks, label = asterisks, size = 6, family = "Helvetica") +
          geom_split_violin(trim = TRUE, color = "gray70") +
          geom_boxplot(position = position_dodge(width = 0.8), width = 0.3, color = "gray70") +
          geom_pointrange(
            data = modmeans,
            aes(ymin = lower.CL, ymax = upper.CL),
            position = position_dodge(width = 0.25),
            size = 0.4,
            color = viridisLite::viridis(n = 1, begin = 1)
          ) +
          coord_cartesian(ylim = c(ymin, ymin + 25), xlim = c(-0.6, 0.6)) +
          scale_x_continuous(breaks = c(-0.2, 0.2), labels = c("Informed", "Naive")) +
          scale_y_continuous(breaks = seq(ymin, ymin + 25, 5)) +
          ylab(paste(comp, "ampl. (µV)")) +
          scale_fill_viridis_d(begin = 0.1, end = 0.5) +
          theme_classic() +
          theme(
            legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 10, color = "black", family = "Helvetica"),
            axis.text = element_text(size = 10, color = "black", family = "Helvetica")
          )
      }) %>% plot_grid(plotlist = ., align = "v", nrow = 3)
    })
  ))
  plot_grid(parts[[1]], NULL, parts[[2]], NULL, parts[[3]], ncol = 5, rel_widths = c(10, 2, 10, 2, 10))
}

# Function for the legend for the violin plot
plot_legends_violins <- function() {
  get_legend(
    tibble(Conditions = c("Informed", "Naive")) %>%
      ggplot(aes(x = 0, y = 0, color = "LMM mean\n± 95% CI", fill = Conditions)) +
      geom_raster() +
      geom_pointrange(aes(ymin = -1, ymax = 1)) +
      scale_fill_viridis_d(begin = 0.1, end = 0.5) +
      scale_color_viridis_d(begin = 1) +
      guides(
        fill = guide_legend(override.aes = list(color = NA)),
        color = guide_legend(title = NULL)
      ) +
      theme(
        legend.box.background = element_rect(colour = "black", size = 0.5),
        legend.box.margin = margin(l = 9, r = 9, b = 9, t = 7),
        legend.background = element_blank(),
        legend.spacing = unit(0.01, "inches"),
        legend.title = element_text(family = "Helvetica", size = 10, face = "bold"),
        legend.text = element_text(family = "Helvetica", size = 10),
        legend.key = element_rect(fill = "gray70")
      )
  )
}

# Function to print an ANOVA-style table
create_table <- function(models, stub_anova, stub_contrasts, caption, note) {
  stub_anova_indent <- paste("\\,\\,", stub_anova)
  stub_contrasts_indent <- paste("\\,\\,", stub_contrasts)
  anov <- map(models, function(model) {
    data.frame(
      "f" = format(round(model$anova$`F value`, 2), trim = FALSE, nsmall = 2),
      "df" = paste0("(", model$anova$NumDF, ", ", format(round(model$anova$DenDF, 1), trim = TRUE, nsmall = 1), ")"),
      "p" = format(round(model$anova$`Pr(>F)`, 3), trim = FALSE, nsmall = 3)
    ) %>%
      mutate(p = ifelse(p == "0.000", "< .001", substr(p, 2, nchar(p)))) %>%
      set_rownames(c(stub_anova))
  })
  suppressMessages(
    anov_print <- anov %>%
      map(unite, col = fdf, f, df, sep = " ") %>%
      map(add_row, fdf = "\\textit{F} (\\textit{df})", p = "\\textit{p}", .before = 1) %>%
      bind_cols() %>%
      set_rownames(c("\\textbf{Fixed effects}", stub_anova_indent))
  )
  suppressMessages(
    anov %<>%
      bind_cols() %>%
      set_colnames(paste(rep(names(models), each = length(names(models))), c("f", "df", "p"), sep = "_"))
  )
  conts <- map(models, function(model) {
    data.frame(
      "est" = format(round(model$contrasts$estimate, 2), trim = FALSE, nsmall = 2),
      "ci" = paste0(
        "[", format(round(model$contrasts$lower.CL, 2), trim = TRUE, nsmall = 2), ", ",
        format(round(model$contrasts$upper.CL, 2), trim = TRUE, nsmall = 2), "]"
      ),
      "t" = format(round(model$contrasts$`t.ratio`, 2), trim = FALSE, nsmall = 2),
      "df" = paste0("(", format(round(model$contrasts$df, 1), trim = TRUE, nsmall = 1), ")"),
      "p" = format(round(model$contrasts$`p.value`, 3), trim = FALSE, nsmall = 3)
    ) %>%
      mutate(p = ifelse(p == "0.000", "< .001", substr(p, 2, nchar(p)))) %>%
      set_rownames(stub_contrasts)
  })
  suppressMessages(
    conts_print <- conts %>%
      map(unite, col = estci, est, ci, sep = " ") %>%
      map(unite, col = tdf, t, df, sep = " ") %>%
      map(mutate, estci = paste0("\\Gape[6pt][-2pt]{", estci, "}"), tdf = paste0("\\Gape[-2pt][6pt]{", tdf, "}")) %>%
      map(unite, col = estt, estci, tdf, sep = "\n") %>%
      map(mutate, estt = kableExtra::linebreak(estt, align = "c", linebreaker = "\n")) %>%
      map(add_row, estt = "Est. [CI]; \\textit{t} (\\textit{df})", p = "\\textit{p}", .before = 1) %>%
      bind_cols() %>%
      set_rownames(c("\\textbf{Informed - naive}", stub_contrasts_indent))
  )
  suppressMessages(
    conts %<>%
      bind_cols() %>%
      set_colnames(paste(rep(names(models), each = 5), c("est", "ci", "t", "df", "p"), sep = "_"))
  )
  suppressMessages(
    map_dfc(models, ~ data.frame(
      perf = c(
        paste0(
          logLik(.x$model) %>% round(1) %>% format(digits = 1, nsmall = 1, trim = TRUE), " (",
          attr(logLik(.x$model), "df") %>% round(1) %>% format(nsmall = 0, digits = 0, trim = TRUE), ")"
        ),
        performance::rmse(.x$model) %>% format(digits = 2, nsmall = 2),
        performance::r2_nakagawa(.x$model)$R2_conditional %>% round(3) %>% format(digits = 3, nsmall = 3),
        performance::r2_nakagawa(.x$model)$R2_marginal %>% round(3) %>% format(digits = 2, nsmall = 2)
      ),
      nothing = ""
    )) %>%
      set_rownames(c("logLik (\\textit{df})", "RMSE", "Conditional $R^2$", "Marginal $R^2$")) -> perf_print
  )
  cnames <- c("\\textbf{Fixed effects}", as.character(anov_print[1, ]))
  list(anov_print[2:nrow(anov_print), ], conts_print, perf_print) %>%
    map(set_colnames, paste0("V", 1:ncol(anov_print))) %>%
    apa_table(
      col.names = cnames,
      col_spanners = list("\\textbf{P1}" = 2:3, "\\textbf{N170}" = 4:5, "\\textbf{N400}" = 6:7),
      midrules = c(nrow(anov_print), nrow(anov_print) + nrow(conts_print) - 1),
      font_size = "footnotesize",
      align = "lcccccc",
      escape = FALSE,
      caption = caption,
      note = note
    ) %>%
    cat()
  return(list("anov" = anov, "conts" = conts))
}

# # Old function without t-values and performance measures
# create_table <- function(models, stub_anova, stub_contrasts, caption, note) {
#   anov <- map(models, function(model) {
#     data.frame(
#       "f" = format(round(model$anova$`F value`, 2), trim = FALSE, nsmall = 2),
#       "df" = paste0("(", model$anova$NumDF, ", ", format(round(model$anova$DenDF, 1), trim = TRUE, nsmall = 1), ")"),
#       "p" = format(round(model$anova$`Pr(>F)`, 3), trim = FALSE, nsmall = 3)
#     ) %>%
#       mutate(p = ifelse(p == "0.000", "< .001", substr(p, 2, nchar(p)))) %>%
#       set_rownames(c(stub_anova))
#   })
#   suppressMessages(
#     anov_print <- anov %>%
#       map(unite, col = fdf, f, df, sep = " ") %>%
#       map(add_row, fdf = "\\textit{F} (\\textit{df})", p = "\\textit{p}", .before = 1) %>%
#       bind_cols() %>%
#       set_rownames(c("\\textbf{Fixed effects}", stub_anova))
#   )
#   suppressMessages(
#     anov %<>%
#       bind_cols() %>%
#       set_colnames(paste(rep(names(models), each = length(names(models))), c("f", "df", "p"), sep = "_"))
#   )
#   conts <- map(models, function(model) {
#     data.frame(
#       "est" = format(round(model$contrasts$estimate, 2), trim = FALSE, nsmall = 2),
#       "ci" = paste0(
#         "[", format(round(model$contrasts$lower.CL, 2), trim = TRUE, nsmall = 2), ", ",
#         format(round(model$contrasts$upper.CL, 2), trim = TRUE, nsmall = 2), "]"
#       ),
#       "p" = format(round(model$contrasts$`p.value`, 3), trim = FALSE, nsmall = 3)
#     ) %>%
#       mutate(p = ifelse(p == "0.000", "< .001", substr(p, 2, nchar(p)))) %>%
#       set_rownames(stub_contrasts)
#   })
#   suppressMessages(
#     conts_print <- conts %>%
#       map(unite, col = estci, est, ci, sep = " ") %>%
#       map(add_row, estci = "Est. [95% CI]", p = "\\textit{p}", .before = 1) %>%
#       bind_cols() %>%
#       set_rownames(c("\\textbf{Informed - naive}", stub_contrasts))
#   )
#   suppressMessages(
#     conts %<>%
#       bind_cols() %>%
#       set_colnames(paste(rep(names(models), each = length(names(models))), c("est", "ci", "p"), sep = "_"))
#   )
#   cnames <- c("\\textbf{Fixed effects}", as.character(anov_print[1, ]))
#   list(anov_print[2:nrow(anov_print), ], conts_print) %>%
#     map(set_colnames, paste0("V", 1:ncol(anov_print))) %>%
#     apa_table(
#       col.names = cnames, col_spanners = list("\\textbf{P1}" = 2:3, "\\textbf{N170}" = 4:5, "\\textbf{N400}" = 6:7),
#       midrules = nrow(anov_print), font_size = "footnotesize", align = "lcccccc", escape = FALSE,
#       caption = caption, note = note
#     ) %>%
#     cat()
#   return(list("anov" = anov, "conts" = conts))
# }
