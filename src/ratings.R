compute_lsa <- function(rating_file, keywords, word2vec_file) {
  #' Computes cosine similarities between rating study and experiment keywords

  require(here)
  require(readr)
  require(dplyr)
  require(stringr)
  require(purrr)
  require(tidyr)

  rating_substantives <- select(rating_raw, S001_01:S242_01)
  rating_verbs <- select(rating_raw, V001_01:V242_01)

  rating_n <- nrow(rating_raw)
  keywords_for_rating <- keywords %>%
    arrange(familiarity, filename) %>%
    select(item_id, familiarity, filename, match, nonmatch) %>%
    replicate(rating_n, ., simplify = FALSE) %>%
    bind_rows()

  word2vec <- load_word2vec(word2vec_file)

  rating <-
    map2_dfc(rating_substantives, rating_verbs, str_c, sep = ", ") %>%
    mutate(participant_id = rating_raw$CASE) %>%
    pivot_longer(
      -participant_id,
      names_to = "question_id", values_to = "response"
    ) %>%
    bind_cols(keywords_for_rating, .) %>%
    pivot_longer(
      c(match, nonmatch),
      names_to = "condition", values_to = "keywords"
    ) %>%
    mutate(
      condition = ifelse(condition == "match", "matching", "non-matching"),
      across(c(keywords, response), str_to_lower),
      keywords = str_replace_all(keywords, keywords_corrections),
      response = str_replace_all(response, response_corrections),
      cosine = compute_cosine(keywords, response, word2vec)
    )

  write_csv(rating, rating_file)
}

load_word2vec <- function(vectors_file) {
  #' Loads a word2vec embedding space trained on the German wikipedia
  #'
  #' See https://www.deepset.ai/german-word-embeddings

  if (!file.exists(vectors_file)) {
    options(timeout = 1800)
    vectors_url <- "https://int-emb-word2vec-de-wiki.s3.eu-central-1.amazonaws.com/vectors.txt"
    download.file(vectors_url, vectors_file)
  }

  word2vec <- as.matrix(read.table(vectors_file, row.names = 1))
  rownames(word2vec) <- purrr::map_chr(
    rownames(word2vec),
    ~ stringr::str_sub(unescape_unicode(.x), start = 3, end = -2)
  )

  word2vec
}

unescape_unicode <- function(x) {
  #' Helper function to convert unicode codes into actual letters

  stopifnot(is.character(x) && length(x) == 1)
  m <- gregexpr("(\\\\)+x[0-9a-z]{2}", x, ignore.case = TRUE)
  if (m[[1]][1] > -1) {
    p <- vapply(regmatches(x, m)[[1]], function(txt) {
      gsub(
        "\\", "\\\\", parse(text = paste0('"', txt, '"'))[[1]],
        fixed = TRUE, useBytes = TRUE
      )
    }, character(1), USE.NAMES = FALSE)
    regmatches(x, m) <- list(p)
  }
  x
}

compute_cosine <- function(x, y, tvectors) {
  #' Computes row-wise cosine distances between two vectors of multiword stings

  require(purrr)
  options(rgl.useNULL = TRUE)
  res_list <- map2(x, y, quietly(LSAfun::costring), tvectors)
  map_dbl(res_list, function(res) {
    cosine <- res$result
    warning <- res$output
    cosine[warning != ""] <- NA
    cosine
  })
}

compute_rating_stats <- function(rating) {
  #' Computes a linear mixed model and contrasts for the rating LSA distances

  require(emmeans)
  require(lmerTest)

  rating <- rating %>%
    filter(!is.na(cosine)) %>%
    mutate(
      fam_condition = paste(familiarity, condition) %>%
        factor(
          levels = c(
            "familiar matching",
            "unfamiliar matching",
            "unfamiliar non-matching"
          ),
          labels = c("familiar", "matching", "non-matching")
        )
    )

  contrasts(rating$fam_condition) <- MASS::contr.sdif(3)

  mod <- lmer(
    cosine ~ fam_condition +
      (fam_condition | participant_id) +
      (1 | item_id),
    data = rating,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))
  )

  emm_rating <- emmeans(
    mod,
    specs = consec ~ fam_condition,
    rev = TRUE,
    adjust = "bonferroni"
  )
  means <- as.data.frame(emm_rating$emmeans)
  conts <- as.data.frame(emm_rating$contrasts) %>%
    rename(t_value = `t.ratio`, p_value = `p.value`)

  list(model = mod, means = means, contrasts = conts)
}

compute_average_cosines <- function(rating) {
  #' Computes the average cosine similarity for each item

  require(dplyr)

  rating %>%
    group_by(item_id) %>%
    summarize(cosine = mean(cosine)) %>%
    mutate(cosine = scale(cosine))
}

#' Manual corrections of typos and words that are not in the word2vec space
#' for "our" keywords:
keywords_corrections <- c(
  aufstemmen = "aufbrechen",
  tierkäfig = "käfig",
  wegkehren = "kehren",
  verühren = "rühren",
  schraubenschlüssel = "werkzeug",
  verquirlen = "rühren",
  tackern = "tacker",
  sprengstoffexplosion = "explosion",
  tankfüllstand = "füllstand",
  tonpott = "tongefäß",
  eierkarton = "eier verpackung",
  musikgerät = "musikinstrument",
  türgelenk = "scharnier",
  telegrafieren = "telegraf",
  sternenbilder = "sternbild",
  aufspulen = "aufwickeln",
  häckseln = "zerkleinern",
  `mikroskop-proben` = "mikroskop proben",
  streckenmaß = "maß",
  windgeschwindigkeit = "windstärke",
  nussöl = "nuss öl",
  entkörnen = "entkernen",
  katzenklo = "katze klo",
  kutschrad = "kutsche rad"
)

#' Manual corrections of typos and words that are not in the word2vec space
#' for the rating study-generated keywords:
response_corrections <- c(
  aufhebeln = "aufbrechen",
  föhnen = "föhn",
  kasette = "kassette",
  aufkehren = "kehren",
  tackern = "tacker",
  sprayapparat = "spray apparat",
  anästhesieren = "betäuben",
  reaktionsspiel = "reaktion spiel",
  mulchgerät = "mulch gerät",
  makierung = "markierung",
  wissenschaftlich = "wissenschaft",
  spannwerkzeug = "werkzeug",
  wärmestab = "wärme stab",
  ampfel = "ampel",
  scheere = "schere",
  pflägen = "pflügen",
  knetten = "kneten",
  pfandkuchen = "pfannkuchen",
  anmacher = "anmachen",
  injezieren = "injizieren",
  telfon = "telefon",
  beschrifter = "beschriften",
  aufbewaren = "aufbewahren",
  spähne = "späne",
  separator = "trennen",
  stämpfer = "stampfen",
  fliente = "flinte",
  tromphete = "trompete",
  makieren = "markieren",
  stöbseln = "stöpsel",
  hiernwellen = "gehirnwellen",
  kartofeln = "kartoffeln",
  vaccum = "vakuum",
  saubermachen = "putzen",
  ankeln = "anker",
  schuessel = "schüssel",
  `zangen\\\\` = "zange",
  kleiderbuegel = "kleiderbügel",
  buerste = "bürste",
  buersten = "bürsten",
  druecken = "drücken",
  obstscheider = "obst schneiden",
  àumachen = "ausmachen",
  bieroeffner = "flaschenöffner",
  oeffnen = "öffnen",
  giesskanne = "gießkanne",
  gluehbirne = "glühbirne",
  schettem = "manschetten",
  kurzhantel = "kurzhanteln",
  tierekiste = "tiere kiste",
  kaffekocher = "kaffeemaschine",
  kinderwaagen = "kinderwagen",
  klobürste = "toilette bürste",
  orangenpresser = "orangen presse",
  foodmixer = "essen mixer",
  schuessel = "schüssel",
  naehen = "nähen",
  teigwalze = "nudelholz",
  pffefern = "pfeffer",
  maehen = "mähen",
  gesichtsbürste = "gesicht bürste",
  regenchirm = "regenschirm",
  kaesegratin = "käse gratin",
  slotsmaschine = "slot maschine",
  piggybank = "sparschwein",
  stampen = "kleben",
  arztwekzeug = "arzt werkzeug",
  gradskala = "grad skala",
  trichten = "trichter",
  spulsmaschine = "spülmaschine",
  zahnbrueste = "zahnbürste",
  bruesten = "bürsten",
  foltermaschine = "folter maschine",
  bieröffner = "flaschenöffner",
  oeffnen = "öffnen",
  pizzacutter = "pizza schneider",
  kerzestande = "kerzenständer",
  naehen = "nähen",
  geruest = "gerüst",
  nagelschneider = "nagel schere",
  burste = "bürste",
  bursten = "bürsten",
  loefel = "löffel",
  hamsterspielzeug = "hamster spielzeug",
  breadboard = "schneide brett",
  druceken = "drücken",
  nussknacnker = "nussknacker",
  ueberwachen = "überwachen",
  trainingsmaschine = "training maschine",
  padeln = "paddeln",
  schäumer = "aufschäumen",
  aufhanger = "aufhänger",
  hangt = "hängen",
  `eier-guillotine` = "eier messer",
  ruhrgerat = "rührgerät",
  katzentrager = "katzen träger",
  hinzufugen = "hinzufügen",
  kuchenform = "backform",
  nagelfeile = "nagel feile",
  fondues = "fondue",
  zuhoren = "zuhören",
  rasenmaher = "rasenmäher",
  geschlitzter = "schlitz",
  breifmarke = "briefmarke",
  bimmeln = "läuten",
  rasma = "",
  kassete = "kassette",
  nudelmaschine = "nudel maschine",
  ruhrer = "rührer",
  feuerloscher = "feuerlöscher",
  entfernungsmesser = "entfernung messer",
  heißluftpistole = "heißluft pistole",
  schuhlöffel = "schuhe anziehen",
  uben = "üben",
  naturlich = "natürlich",
  schißen = "schießen",
  rührgerät = "mixer",
  aufkehren = "kehren",
  rausziehen = "herausziehen",
  `mini-meißel` = "mini meißel",
  kaffeemischer = "kaffee mischer",
  nachttischlampe = "nachttisch lampe",
  arbeitsschlüssel = "arbeit schlüssel",
  `boom-box` = "lautsprecher",
  feststampfen = "fest stampfen",
  `casino-maschine` = "casino maschine",
  `herzfrequenz-test` = "herzfrequenz test",
  zaichnen = "zeichnen",
  schneidin = "schneiden",
  vergleichsmaschine = "vergleich maschine",
  rosenabzeichen = "rosen abzeichen",
  `dummy-stecker` = "dummy stecker",
  `post-punk-sanduhr` = "post punk sanduhr",
  schneidemaschine = "schneiden maschine",
  fusszeile = "fußzeile",
  gartenlampe = "garten lampe",
  trenneimer = "trennen eimer",
  fußabstreifer = "fußmatte",
  wichten = "gewicht",
  perlenständer = "perlen ständer",
  briefmarker = "briefmarke",
  `elektronische-schaltung` = "elektronische schaltung",
  fläschchenheizung = "flächen heizung",
  elektrodrucker = "elektro drucker",
  metalllinse = "metall linse",
  kugelheizung = "kugel heizung",
  abschrauber = "abschrauben",
  `mini-korb` = "mini korb",
  elektronenrechner = "elektronen rechner",
  spießfaum = "spieß",
  tierträger = "tier träger",
  rührgerät = "rührgerät",
  kuchenform = "backform",
  friteuse = "fritteuse",
  `ein-` = "ein",
  greigfen = "greifen",
  zerknittern = "falten bekommen",
  salatzange = "salat zange",
  wwrm = "wurm",
  duschwasser = "dusche wasser",
  verbreitene = "verbreiten",
  aufpieksen = "anstechen",
  restriktieren = "einschränken",
  rückgeld = "wechselgeld",
  aufkehren = "zusammenkehren",
  bratgut = "grillgut",
  bepinseln = "bestreichen",
  verquirlen = "vermischen",
  temparatur = "temperatur",
  aufspulen = "aufwickeln",
  elekronik = "elektronik",
  festnieten = "fest nieten",
  festspannen = "fest spannen",
  hebelkräfte = "hebelkraft",
  meiseln = "meißeln",
  betäüben = "betäuben",
  verschraüben = "verschrauben",
  schraüben = "schrauben",
  seperator = "trennen",
  schraübendreher = "schraubendreher",
  gegewicht = "gewicht"
)
