#' Helper function to format degrees of freedom in the paper
print_dof <- function(x) format(round(x), big.mark = ",")

print_materials_table <- function(keywords) {
  #' Prints an APA-style formatted table of object stimuli and keywords

  require(dplyr)

  keywords %>%
    filter(familiarity == "unfamiliar") %>%
    transmute(
      img = paste0(
        "\\includegraphics[valign=c,width=18mm]{materials/unfamiliar_objects/",
        filename,
        "}"
      ),
      item_id = as.character(item_id),
      match_translated = paste(match, match_en, sep = "\n") %>%
        kableExtra::linebreak(align = "l", linebreaker = "\n"),
      nonmatch_translated = paste(nonmatch, nonmatch_en, sep = "\n") %>%
        kableExtra::linebreak(align = "l", linebreaker = "\n")
    ) %>%
    papaja::apa_table(
      booktabs = TRUE,
      col.names = c(
        "Stimulus", "ID", "Matching keywords", "Non-matching keywords"
      ),
      escape = FALSE,
      longtable = TRUE,
      caption = "Unfamiliar Object Stimuli\\smallskip",
      font_size = "footnotesize"
    )
}

print_model_outputs <- function(model_object) {
  #' Prints model summary, ANOVA (F tests), and contrasts

  model_object$summary$call$data <- NULL # Don't want to print the raw data
  print(model_object$summary)
  cat("\n")
  print(model_object$anova)
  cat("\nPairwise Contrasts (Simple Effects)\n")
  print(model_object$contrasts)
  cat("\n\n")
}
