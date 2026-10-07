testthat::test_that("plotIndikator returns a ggplot with title and labels", {
  indikatorData <- data.frame(
    year = c(2023, 2023, 2023, 2024, 2024, 2024, 2024),
    orgnr = c("A", "B", "C", "A", "B", "C", "D"),
    var = c(0.12, 0.18, 0.25, 0.15, 0.20, 0.28, 0.22),
    denominator = c(30, 50, 90, 35, 60, 100, 80)
  )

  result <- suppressWarnings(
    plotIndikator(
      indikatorData = indikatorData,
      title = "Indikator test",
      shortDescription = "Kort beskrivelse",
      showYear = 2024,
      terskel = 10,
      levelDirection = 1,
      kvalIndgrenser = c(20, 50)
    )
  )

  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_equal(result$labels$title, "Indikator test")
  testthat::expect_equal(result$labels$subtitle, "Kort beskrivelse")
  testthat::expect_equal(result$labels$x, "Prosent")
  testthat::expect_equal(result$labels$shape, "Tidligere år")
})

testthat::test_that("plotIndikator handles the no-quality-band branch and labelAtBase = FALSE", {
  indikatorData <- data.frame(
    year = c(2023, 2023, 2024, 2024, 2024),
    orgnr = c("A", "B", "A", "B", "C"),
    var = c(0.10, 0.18, 0.12, 0.19, 0.05),
    denominator = c(8, 12, 9, 15, 5)
  )

  result <- suppressWarnings(
    plotIndikator(
      indikatorData = indikatorData,
      title = "Uten kvalitetsbånd",
      shortDescription = "Test uten bakgrunnsmarkering",
      showYear = 2024,
      terskel = 10,
      labelAtBase = FALSE,
      showNlabel = TRUE
    )
  )

  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_equal(result$labels$title, "Uten kvalitetsbånd")
  testthat::expect_equal(result$labels$subtitle, "Test uten bakgrunnsmarkering")
})

testthat::test_that("plotIndikator covers the reverse direction and N<terskel label path", {
  indikatorData <- data.frame(
    year = c(2022, 2022, 2023, 2023, 2024, 2024, 2024),
    orgnr = c("A", "B", "A", "B", "A", "B", "C"),
    var = c(0.40, 0.60, 0.35, 0.65, 0.42, 0.68, 0.15),
    denominator = c(25, 30, 20, 35, 28, 40, 7)
  )

  result <- suppressWarnings(
    plotIndikator(
      indikatorData = indikatorData,
      title = "Omvendt retningsvalg",
      shortDescription = "Tester lav-er-bedre",
      showYear = 2024,
      terskel = 10,
      levelDirection = 0,
      kvalIndgrenser = c(20, 50),
      showNlabel = FALSE
    )
  )

  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_equal(result$labels$title, "Omvendt retningsvalg")
  testthat::expect_equal(result$labels$subtitle, "Tester lav-er-bedre")
  testthat::expect_equal(result$labels$shape, "Tidligere år")
})
