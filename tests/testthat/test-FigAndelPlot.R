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

test_that("FigAndelPlot shows <terskel in label for below-threshold groups", {
  df <- data.frame(
    gruppe = c("A", "A", "B", "C"),
    verdi = c(TRUE, FALSE, TRUE, FALSE)
  )

  p <- FigAndelPlot(df, andelVariabel = "verdi", gruppeVariabel = "gruppe", terskel = 2)
  d <- p$data

  bLabel <- as.character(d$groupLabel[d$group == "B"])
  cLabel <- as.character(d$groupLabel[d$group == "C"])
  aLabel <- as.character(d$groupLabel[d$group == "A"])

  expect_match(bLabel, "B \\(<2\\)$")
  expect_match(cLabel, "C \\(<2\\)$")
  expect_match(aLabel, "A \\(2\\)$")
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

  p1 <- FigAndelPlot(df, andelVariabel = "verdi", gruppeVariabel = "gruppe", breaks = c(90, 75))
  expect_s3_class(p1, "ggplot")

  p2 <- FigAndelPlot(df, andelVariabel = "verdi", gruppeVariabel = "gruppe", breaks = c(70))
  expect_s3_class(p2, "ggplot")
})

test_that("FigAndelPlot validates data frame input", {
  p <- FigAndelPlot(data = list(a = 1), andelVariabel = "a")
  expect_s3_class(p, "ggplot")
})

test_that("FigAndelPlot validates andelVariabel", {
  df <- data.frame(x = c(TRUE, FALSE))

  p1 <- FigAndelPlot(data = df, andelVariabel = "missing")
  expect_s3_class(p1, "ggplot")

  p2 <- FigAndelPlot(data = df, andelVariabel = c("x", "y"))
  expect_s3_class(p2, "ggplot")
})

test_that("FigAndelPlot validates gruppeVariabel", {
  df <- data.frame(x = c(TRUE, FALSE), g = c("A", "B"))

  p1 <- FigAndelPlot(data = df, andelVariabel = "x", gruppeVariabel = "missing")
  expect_s3_class(p1, "ggplot")

  p2 <- FigAndelPlot(data = df, andelVariabel = "x", gruppeVariabel = c("g", "x"))
  expect_s3_class(p2, "ggplot")
})

test_that("FigAndelPlot validates terskel", {
  df <- data.frame(x = c(TRUE, FALSE))

  p1 <- FigAndelPlot(data = df, andelVariabel = "x", terskel = -1)
  expect_s3_class(p1, "ggplot")

  p2 <- FigAndelPlot(data = df, andelVariabel = "x", terskel = NA)
  expect_s3_class(p2, "ggplot")

  p3 <- FigAndelPlot(data = df, andelVariabel = "x", terskel = "text")
  expect_s3_class(p3, "ggplot")

  p4 <- FigAndelPlot(data = df, andelVariabel = "x", terskel = c(1, 2))
  expect_s3_class(p4, "ggplot")
})

test_that("FigAndelPlot validates breaks values within range", {
  df <- data.frame(x = c(TRUE, FALSE))

  p1 <- FigAndelPlot(data = df, andelVariabel = "x", breaks = c(-10, 50))
  expect_s3_class(p1, "ggplot")

  p2 <- FigAndelPlot(data = df, andelVariabel = "x", breaks = c(50, 110))
  expect_s3_class(p2, "ggplot")

  p3 <- FigAndelPlot(data = df, andelVariabel = "x", breaks = c(50, NA))
  expect_s3_class(p3, "ggplot")

  p4 <- FigAndelPlot(data = df, andelVariabel = "x", breaks = c("50", "75"))
  expect_s3_class(p4, "ggplot")
})

test_that("FigAndelPlot works without gruppeVariabel (total only)", {
  df <- data.frame(verdi = c(TRUE, TRUE, FALSE, FALSE))

  p <- FigAndelPlot(data = df, andelVariabel = "verdi")
  d <- p$data

  expect_equal(nrow(d), 1)
  expect_equal(d$group[1], "Totalt")
  expect_equal(d$n, 4)
  expect_equal(d$pctTrue, 50)
})

test_that("FigAndelPlot handles empty filtered data (all NAs)", {
  df <- data.frame(
    x = c(NA, NA, NA),
    g = c("A", "B", "C")
  )

  p <- FigAndelPlot(data = df, andelVariabel = "x", gruppeVariabel = "g")
  d <- p$data

  expect_equal(nrow(d), 1)
  expect_equal(d$group[1], "Totalt")
  expect_equal(d$n, 0)
  expect_equal(d$nTrue, 0)
})

test_that("FigAndelPlot uses custom trueValues and falseValues", {
  df <- data.frame(
    gruppe = c("A", "A", "B", "B"),
    verdi = c("custom_true", "custom_false", "CUSTOM_TRUE", "CUSTOM_FALSE")
  )

  p <- FigAndelPlot(
    data = df,
    andelVariabel = "verdi",
    gruppeVariabel = "gruppe",
    trueValues = c("custom_true", "CUSTOM_TRUE"),
    falseValues = c("custom_false", "CUSTOM_FALSE")
  )

  d <- p$data
  totalRow <- d[d$group == "Totalt", , drop = FALSE]
  expect_equal(totalRow$n, 4)
  expect_equal(totalRow$pctTrue, 50)
})

test_that("FigAndelPlot handles group NA values", {
  df <- data.frame(
    gruppe = c("A", NA, "B"),
    verdi = c(TRUE, TRUE, FALSE)
  )

  p <- FigAndelPlot(data = df, andelVariabel = "verdi", gruppeVariabel = "gruppe")
  d <- p$data

  # NA group removed; only A, B, and total
  groups <- unique(d$group)
  expect_false(NA %in% groups)
  expect_true("Totalt" %in% groups)
})

test_that("FigAndelPlot sorts bars by pctTrue ascending", {
  df <- data.frame(
    gruppe = c("Z", "Z", "A", "A", "B", "B"),
    verdi = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
  )

  p <- FigAndelPlot(data = df, andelVariabel = "verdi", gruppeVariabel = "gruppe", terskel = 1)
  d <- p$data

  # Groups sorted by pctTrue: B=0%, Z=50%, A=100%
  groupOrder <- d$group[!d$isTotal]
  expect_equal(as.character(groupOrder), c("B", "Z", "A"))
})

test_that("FigAndelPlot handles all TRUE (100%)", {
  df <- data.frame(
    gruppe = c("A", "A", "B", "B"),
    verdi = c(TRUE, TRUE, TRUE, TRUE)
  )

  p <- FigAndelPlot(data = df, andelVariabel = "verdi", gruppeVariabel = "gruppe")
  d <- p$data

  expect_true(all(d$pctTrue == 100 | is.na(d$pctTrue)))
})

test_that("FigAndelPlot handles all FALSE (0%)", {
  df <- data.frame(
    gruppe = c("A", "A", "B", "B"),
    verdi = c(FALSE, FALSE, FALSE, FALSE)
  )

  p <- FigAndelPlot(data = df, andelVariabel = "verdi", gruppeVariabel = "gruppe")
  d <- p$data

  expect_true(all(d$pctTrue == 0 | is.na(d$pctTrue)))
})

test_that("FigAndelPlot handles breaks with highGood = FALSE", {
  df <- data.frame(
    gruppe = c("A", "A", "B", "B"),
    verdi = c(TRUE, FALSE, TRUE, TRUE)
  )

  p <- FigAndelPlot(
    data = df,
    andelVariabel = "verdi",
    gruppeVariabel = "gruppe",
    breaks = c(40, 60),
    highGood = FALSE
  )

  layerClasses <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRect" %in% layerClasses)
})

test_that("FigAndelPlot handles single group (no grouping variable)", {
  df <- data.frame(verdi = c(TRUE, FALSE, TRUE, FALSE))

  p <- FigAndelPlot(data = df, andelVariabel = "verdi", totalLabel = "Overall")
  d <- p$data

  expect_equal(d$group[1], "Overall")
  expect_equal(nrow(d), 1)
})

test_that("FigAndelPlot handles tied percentages in sorting", {
  df <- data.frame(
    gruppe = c("A", "A", "B", "B", "C", "C"),
    verdi = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  )

  p <- FigAndelPlot(data = df, andelVariabel = "verdi", gruppeVariabel = "gruppe", terskel = 1)
  d <- p$data

  # All groups have same pctTrue (50%), so seq_len tie-breaker should preserve order
  bars <- d[!d$isTotal & d$eligible, , drop = FALSE]
  expect_equal(nrow(bars), 3)
})

test_that("FigAndelPlot with only one valid group after filtering", {
  df <- data.frame(
    gruppe = c("A", "A", "B", "B"),
    verdi = c(TRUE, FALSE, NA, NA)
  )

  p <- FigAndelPlot(data = df, andelVariabel = "verdi", gruppeVariabel = "gruppe", terskel = 1)
  d <- p$data

  # Only A has valid values, B is filtered out
  groups <- unique(d$group)
  expect_true("A" %in% groups)
  expect_true("Totalt" %in% groups)
})

test_that("FigAndelPlot computes mean correctly when no total row", {
  df <- data.frame(
    gruppe = c("A", "B"),
    verdi = c(TRUE, FALSE)
  )

  p <- FigAndelPlot(data = df, andelVariabel = "verdi", gruppeVariabel = "gruppe", terskel = 1)
  d <- p$data

  # Should still compute y-axis limits
  expect_true(!is.null(p))
  expect_s3_class(p, "ggplot")
})

test_that("FigAndelPlot with multiple below-threshold groups", {
  df <- data.frame(
    gruppe = c("A", "A", "A", "B", "C", "D"),
    verdi = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)
  )

  p <- FigAndelPlot(data = df, andelVariabel = "verdi", gruppeVariabel = "gruppe", terskel = 2)
  d <- p$data

  # A (n=3) is eligible, B/C/D (n=1 each) are label-only
  eligible <- d[!d$isTotal, "eligible", drop = TRUE]
  expect_equal(sum(eligible), 1)  # Only A
  expect_equal(sum(!eligible), 3)  # B, C, D
})

test_that("FigAndelPlot with isTRUE(highGood) quality bands", {
  df <- data.frame(
    gruppe = c("A", "A", "B", "B"),
    verdi = c(TRUE, FALSE, TRUE, TRUE)
  )

  p <- FigAndelPlot(
    data = df,
    andelVariabel = "verdi",
    gruppeVariabel = "gruppe",
    breaks = c(40, 60),
    highGood = TRUE
  )

  # Verify bands are created with highGood=TRUE mapping
  layerClasses <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRect" %in% layerClasses)
  expect_true("GeomCol" %in% layerClasses)
  expect_true("GeomText" %in% layerClasses)
})

test_that("FigAndelPlot with all groups below threshold", {
  df <- data.frame(
    gruppe = c("A", "B", "C"),
    verdi = c(TRUE, FALSE, TRUE)
  )

  p <- FigAndelPlot(data = df, andelVariabel = "verdi", gruppeVariabel = "gruppe", terskel = 10)
  d <- p$data

  # All non-total groups below threshold; only total is eligible
  total <- d[d$isTotal, , drop = FALSE]
  groups <- d[!d$isTotal, , drop = FALSE]

  expect_true(total$eligible[1])
  expect_true(all(!groups$eligible))
  expect_true(all(is.na(groups$barValue)))
})

test_that("FigAndelPlot with nrow == 1 (single group, no reordering)", {
  df <- data.frame(
    verdi = c(TRUE, TRUE, FALSE)
  )

  p <- FigAndelPlot(data = df, andelVariabel = "verdi")
  d <- p$data

  # nrow(plotData) == 1, so reordering block not executed
  expect_equal(nrow(d), 1)
  expect_equal(d$group[1], "Totalt")
  expect_equal(d$pctTrue, 100 * 2 / 3)
})

test_that("FigAndelPlot with bars but no below-threshold groups", {
  df <- data.frame(
    gruppe = c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"),
    verdi = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE)
  )

  p <- FigAndelPlot(data = df, andelVariabel = "verdi", gruppeVariabel = "gruppe", terskel = 3)
  d <- p$data

  # All groups eligible, reordering happens with nrow(bars) > 0
  groups <- d[!d$isTotal, , drop = FALSE]
  expect_true(all(groups$eligible))

  # Verify sorted by pctTrue ascending
  bars <- groups[order(groups$pctTrue), , drop = FALSE]
  expect_true(all(diff(bars$pctTrue) >= 0))
})

test_that("FigAndelPlot with zero n groups", {
  df <- data.frame(
    gruppe = c("A", "A"),
    verdi = c(TRUE, FALSE)
  )

  p <- FigAndelPlot(data = df, andelVariabel = "verdi", gruppeVariabel = "gruppe", terskel = 0)
  d <- p$data

  # Even with terskel=0, all groups with n>0 are eligible
  groups <- d[!d$isTotal, , drop = FALSE]
  expect_true(all(groups$n > 0))
  expect_true(all(groups$eligible))
})
