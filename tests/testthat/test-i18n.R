
cjk_chars <- function(x) {
  cp <- utf8ToInt(paste(x, collapse = ""))
  intToUtf8(cp[(cp >= 0x3000 & cp <= 0x9fff) | (cp >= 0xff00 & cp <= 0xffef)], multiple = TRUE)
}

rp_t_keys <- function() {
  ns <- asNamespace("randomPlatform")
  keys <- character()
  walk <- function(e) {
    if (is.call(e)) {
      fn <- e[[1]]
      if ((identical(fn, as.name("rp_t")) || identical(fn, as.name(".rp_bilingual"))) && is.character(e[[2]])) keys <<- c(keys, e[[2]])
      for (a in as.list(e)[-1]) if (!missing(a)) walk(a)
    }
  }
  for (nm in ls(ns, all.names = TRUE)) {
    f <- get(nm, envir = ns)
    if (is.function(f)) walk(body(f))
  }
  unique(keys)
}

test_that("every translatable string has an en and pl entry", {
  dict <- randomPlatform:::.rp_dict()
  keys <- rp_t_keys()
  expect_gt(length(keys), 150)
  missing <- setdiff(keys, dict$zh)
  expect_identical(missing, character())
  expect_true(all(nzchar(dict$en) & nzchar(dict$pl)))
  expect_length(cjk_chars(c(dict$en, dict$pl)), 0)
})

test_that("English and Polish UI contain no Chinese besides the language name", {
  for (lang in c("en-US", "pl-PL")) {
    withr::local_options(randomPlatform.language = lang)
    html <- paste(as.character(rp_app_ui()), collapse = "\n")
    expect_setequal(unique(cjk_chars(gsub("\u4E2D\u6587", "", html, fixed = TRUE))), character())
  }
  withr::local_options(randomPlatform.language = "pl-PL")
  expect_true(grepl("Schemat randomizacji", paste(as.character(rp_app_ui()), collapse = ""), fixed = TRUE))
})

test_that("envelope labels follow the language and the allocation does not", {
  row <- data.frame(envelope_no = "No. 001", center = "C01", stringsAsFactors = FALSE)
  labels <- function(lang) {
    withr::local_options(randomPlatform.language = lang)
    randomPlatform:::.rp_envelope_common_rows(row, "P", "X", "S", "center")$label
  }
  expect_identical(labels("en-US")[1], "Project name")
  expect_identical(labels("pl-PL")[1], "Nazwa projektu")
  expect_length(cjk_chars(c(labels("en-US"), labels("pl-PL"))), 0)
  expect_match(labels("zh-CN")[1], "Project name", fixed = TRUE)

  run <- function(lang) rp_randomize("P", "X", "S", c(A = "Test", B = "Control"), method = "block", seed = 5,
    n_total = 8, block_sizes = 4L, language = lang, generate_random_envelope = TRUE,
    generate_emergency_envelope = TRUE, generate_report = FALSE, generate_reproducibility = FALSE,
    output_dir = tempfile("rp-lang-"))
  pl <- run("pl-PL")
  expect_true(file.exists(pl$files$random_envelope))
  expect_identical(pl$audit$table_hash, run("en-US")$audit$table_hash)
  expect_error(run("de-DE"), "language must be one of")
})
