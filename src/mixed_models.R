fit_mixed_model <- function(dep, formula, data) {
  #' Computes a parsimonious mixed model and follow up contrasts

  require(buildmer)
  require(emmeans)
  require(parallel)
  require(dplyr)

  formula <- formula %>%
    tabulate.formula() %>%
    mutate(block = replace(block, is.na(grouping), "fixed"))

  n_cores <- detectCores()
  cl <- makeCluster(n_cores)

  # See Matuschek et al. (2017), https://doi.org/10.1016/j.jml.2017.01.001
  build <- buildmer(
    buildmerControl = buildmerControl(
      formula, data,
      args = list(control = lme4::lmerControl(
        optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)
      )),
      direction = c("backward", "backward"),
      cl = cl,
      elim = LRTalpha(.20),
      calc.anova = TRUE,
      ddf = "Satterthwaite",
      dep = dep
    )
  )
  stopCluster(cl)

  emm_options(lmer.df = "Satterthwaite", lmerTest.limit = Inf)
  emm_means <- emmeans(build@model, specs = c("phase", "condition"))
  emm_contrasts <- contrast(
    emm_means,
    method = "trt.vs.ctrl", simple = "condition", reverse = TRUE
  ) %>%
    summary(by = "contrast", adjust = "bonferroni")

  list(
    model = build@model,
    summary = build@summary,
    anova = build@anova,
    means = emm_means,
    contrasts = emm_contrasts
  )
}

extract_anova_table <- function(models) {
  #' Extracts table of ANOVA (Type III) outputs from fitted models

  require(dplyr)
  require(purrr)

  map(models, function(model) as_tibble(model$anova, rownames = "effect")) %>%
    bind_rows(.id = "component") %>%
    rename(f_value = `F value`, p_value = `Pr(>F)`)
}

extract_contrast_table <- function(models) {
  #' Extracts table of follow-up contrast outputs from fitted models

  require(dplyr)
  require(purrr)

  map(models, function(model) as_tibble(model$contrasts)) %>%
    bind_rows(.id = "component") %>%
    rename(t_value = `t.ratio`, p_value = `p.value`)
}
