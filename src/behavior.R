read_log <- function(log_file) {
  #' Read and preprocess a behavioral log file from Presentation

  require(readr)
  require(dplyr)

  log <- read_tsv(
    log_file,
    col_types = cols(),
    locale = locale(encoding = "latin1"),
    progress = FALSE
  ) %>%
    transmute(
      phase = factor(
        Wdh,
        levels = c(211, 212, 213),
        labels = c("Pre-insight", "Insight", "Post-insight")
      ),
      familiarity = factor(
        bek_unbek,
        levels = c("bekannt", "unbekannt"),
        labels = c("familiar", "unfamiliar")
      ),
      keywords = factor(
        Bed,
        levels = c("richtig", "falsch"),
        labels = c("Match", "Non-match")
      ),
      # Response code 201: "I know what this is or have a strong assumption"
      # Response code 202: "I have an assumption what this is"
      # Response code 203: "I have rather no assumption what this is"
      # Response code 204: "I don't know what this is and have no assumption"
      # Response code 0: Technical glitch
      response = Tastencode,
      rt = RT,
      item_id = factor(StimID)
    ) %>%
    filter(familiarity == "unfamiliar")

  items_per_condition <- with(log, {
    list(
      Informed = item_id[
        phase == "Insight" & keywords == "Match" & response %in% c(201, 202)
      ],
      Uninformed = item_id[
        phase == "Insight" & keywords == "Non-match" & response %in% c(203, 204)
      ],
      Unsuccessful = item_id[
        phase == "Insight" & keywords == "Match" & response %in% c(203, 204)
      ],
      Exclude_uninformed = item_id[
        phase == "Insight" & keywords == "Non-match" & response %in% c(201, 202)
      ],
      Exclude_known = item_id[phase == "Pre-insight" & response == 201],
      Exclude_glitch = item_id[phase == "Insight" & response == 0]
    )
  })

  log$condition <- NA
  for (condition in names(items_per_condition)) {
    item_id <- items_per_condition[[condition]]
    log$condition[log$item_id %in% item_id] <- condition
  }

  log %>%
    select(item_id, phase, condition, response, rt)
}
