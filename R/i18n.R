# Interface and envelope text in three languages. The Chinese original is the key,
# so an untranslated string falls back to Chinese; inst/i18n/strings.csv holds en and pl.
.rp_i18n_cache <- new.env(parent = emptyenv())

.rp_lang <- function(language = getOption("randomPlatform.language", "zh-CN")) {
  lang <- tolower(substr(as.character(language %||% "zh")[1], 1, 2))
  if (!lang %in% c("zh", "en", "pl")) stop("language must be one of zh-CN, en-US, pl-PL.", call. = FALSE)
  lang
}

.rp_dict <- function() {
  if (is.null(.rp_i18n_cache$dict)) {
    path <- system.file("i18n", "strings.csv", package = "randomPlatform")
    .rp_i18n_cache$dict <- utils::read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  }
  .rp_i18n_cache$dict
}

rp_t <- function(zh, language = getOption("randomPlatform.language", "zh-CN")) {
  lang <- .rp_lang(language)
  if (identical(lang, "zh")) return(zh)
  dict <- .rp_dict()
  hit <- dict[[lang]][match(zh, dict$zh)]
  ifelse(is.na(hit), zh, hit)
}
