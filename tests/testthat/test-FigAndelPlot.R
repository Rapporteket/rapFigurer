test_that("FigAndelPlot returns a ggplot object", {
  df <- data.frame(
    gruppe = c("A", "A", "B", "B"),
    verdi = c(TRUE, FALSE, TRUE, FALSE)
  )

  p <- FigAndelPlot(df, andelVariabel = "verdi", gruppeVariabel = "gruppe")
  expect_s3_class(p, "ggplot")
})

test_that("FigAndelPlot removes NA and computes percent true of true+false", {
  df <- data.frame(
    gruppe = c("A", "A", "A", "A", "B", "B", "B"),
    verdi = c(TRUE, FALSE, NA, TRUE, TRUE, FALSE, FALSE)
  )

  p <- FigAndelPlot(df, andelVariabel = "verdi", gruppeVariabel = "gruppe")
  d <- p$data

  # Totalt: TRUE=3, FALSE=3 -> 50
  totalRow <- d[d$group == "Totalt", , drop = FALSE]
  expect_equal(totalRow$n, 6)
  expect_equal(totalRow$pctTrue, 50)
})

test_that("FigAndelPlot keeps total on top and sorts bars low to high", {
  df <- data.frame(
    gruppe = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
    verdi = c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)
  )

  p <- FigAndelPlot(df, andelVariabel = "verdi", gruppeVariabel = "gruppe", terskel = 1)
  d <- p$data

  expect_equal(d$group[1], "Totalt")

  bars <- d[!d$isTotal & d$eligible, c("group", "pctTrue"), drop = FALSE]
  expect_equal(as.character(bars$group), c("A", "B", "C"))
  expect_true(all(diff(bars$pctTrue) >= 0))
})

test_that("FigAndelPlot shows below-threshold groups as label-only", {
  df <- data.frame(
    gruppe = c("A", "A", "B", "C"),
    verdi = c(TRUE, FALSE, TRUE, FALSE)
  )

  p <- FigAndelPlot(df, andelVariabel = "verdi", gruppeVariabel = "gruppe", terskel = 2)
  d <- p$data

  # A has n=2 (eligible), B and C have n=1 (label-only)
  expect_true(d$eligible[d$group == "A"])
  expect_false(d$eligible[d$group == "B"])
  expect_false(d$eligible[d$group == "C"])
  expect_true(is.na(d$barValue[d$group == "B"]))
  expect_true(is.na(d$barValue[d$group == "C"]))
})

test_that("FigAndelPlot uses Norwegian percentage labels with one decimal", {
  df <- data.frame(
    gruppe = c("A", "A", "A", "A"),
    verdi = c(TRUE, TRUE, TRUE, FALSE)
  )

  p <- FigAndelPlot(df, andelVariabel = "verdi", gruppeVariabel = "gruppe", terskel = 1)
  d <- p$data
  aRow <- d[d$group == "A", , drop = FALSE]

  expect_equal(aRow$pctLabel, "75,0 %")
})

test_that("FigAndelPlot includes n in group label as number only", {
  df <- data.frame(
    gruppe = c("A", "A", "B"),
    verdi = c(TRUE, FALSE, TRUE)
  )

  p <- FigAndelPlot(df, andelVariabel = "verdi", gruppeVariabel = "gruppe", terskel = 1)
  d <- p$data

  expect_true(any(grepl("A \\(2\\)$", d$groupLabel)))
  expect_false(any(grepl("n=", d$group_label)))
})

test_that("FigAndelPlot supports built-in mapping and drops unmapped values", {
  df <- data.frame(
    gruppe = c("A", "A", "A", "A", "B", "B"),
    verdi = c("Ja", "Nei", "Y", "N", "MAYBE", "No")
  )

  p <- FigAndelPlot(df, andelVariabel = "verdi", gruppeVariabel = "gruppe", terskel = 1)
  d <- p$data

  # "MAYBE" is dropped as unmapped
  totalRow <- d[d$group == "Totalt", , drop = FALSE]
  expect_equal(totalRow$n, 5)
})

test_that("FigAndelPlot draws quality background and legend when breaks are provided", {
  df <- data.frame(
    gruppe = c("A", "A", "B", "B"),
    verdi = c(TRUE, FALSE, TRUE, TRUE)
  )

  p <- FigAndelPlot(
    df,
    andelVariabel = "verdi",
    gruppeVariabel = "gruppe",
    breaks = c(75, 90),
    highGood = TRUE
  )

  expect_true("fill" %in% names(p$scales$scales[[1]]$aesthetics) ||
                any(vapply(p$scales$scales, function(s) "fill" %in% s$aesthetics, logical(1))))

  layerClasses <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRect" %in% layerClasses)
})

test_that("FigAndelPlot validates breaks", {
  df <- data.frame(gruppe = c("A", "A"), verdi = c(TRUE, FALSE))

  expect_error(
    FigAndelPlot(df, andelVariabel = "verdi", gruppeVariabel = "gruppe", breaks = c(90, 75)),
    "strictly increasing"
  )

  expect_error(
    FigAndelPlot(df, andelVariabel = "verdi", gruppeVariabel = "gruppe", breaks = c(70)),
    "exactly two"
  )
})
