test_that("FigGruppertFordeling works without group variable", {
  df <- data.frame(
    kategori = factor(c("A", "B", "A", NA), levels = c("A", "B", "C")),
    stringsAsFactors = FALSE
  )

  p <- FigGruppertFordeling(
    data = df,
    kategoriVariabel = "kategori"
  )

  expect_s3_class(p, "ggplot")
  expect_true(all(as.character(p$data$gruppe) == "Totalt"))
  expect_setequal(as.character(p$data$kategori), c("A", "B"))
  expect_equal(unique(as.character(p$labels$fill)), "kategori")
  expect_true("text" %in% names(p$mapping))
})

test_that("FigGruppertFordeling supports group variable, recoding and custom legend", {
  df <- data.frame(
    gruppe = factor(c("G2", "G1", "G2", "G1", NA), levels = c("G1", "G2", "G3")),
    kategori = factor(c("0", "1", "0", "1", "1"), levels = c("0", "1", "2")),
    stringsAsFactors = FALSE
  )

  p <- FigGruppertFordeling(
    data = df,
    gruppeVariabel = "gruppe",
    kategoriVariabel = "kategori",
    kategoriNavn = c("0" = "Nei", "1" = "Ja", "2" = "Ukjent"),
    totalLabel = "Totalt",
    legendTitle = "Svar"
  )

  expect_s3_class(p, "ggplot")
  expect_true("Totalt" %in% as.character(p$data$gruppe))
  expect_true(all(c("Ja", "Nei") %in% as.character(p$data$kategori)))
  expect_equal(p$labels$fill, "Svar")

  gruppe_levels <- levels(p$data$gruppe)
  expect_equal(gruppe_levels[length(gruppe_levels)], "Totalt")
})

test_that("FigGruppertFordeling sorts numeric-like category and group values", {
  df <- data.frame(
    gruppe = c("10", "2", "10", "2", "1", "1"),
    kategori = c("10", "2", "2", "10", "10", "2"),
    stringsAsFactors = FALSE
  )

  p <- FigGruppertFordeling(
    data = df,
    gruppeVariabel = "gruppe",
    kategoriVariabel = "kategori"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(levels(p$data$gruppe), c("1", "2", "10", "Totalt"))
  expect_equal(levels(p$data$kategori), c("2", "10"))
})

test_that("FigGruppertFordeling computes proportions and tooltip text", {
  df <- data.frame(
    gruppe = c("A", "A", "A", "B", "B"),
    kategori = c("x", "x", "y", "x", "y"),
    stringsAsFactors = FALSE
  )

  p <- FigGruppertFordeling(
    data = df,
    gruppeVariabel = "gruppe",
    kategoriVariabel = "kategori"
  )

  expect_true(all(p$data$andel > 0))
  sums <- tapply(p$data$andel, p$data$gruppe, sum)
  expect_true(all(abs(as.numeric(sums) - 1) < 1e-10))
  expect_true(all(grepl("Antall:", p$data$tooltip)))
  expect_true(all(grepl("Andel:", p$data$tooltip)))
})
