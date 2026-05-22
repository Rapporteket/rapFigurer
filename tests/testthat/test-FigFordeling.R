
test_that("FigFordeling handles insufficient observations", {
  # Test with N$Hoved < 5
  AggVerdier <- list(
    Hoved = c(50, 30, 20),
    Rest = c(45, 35, 20)
  )
  N <- list(Hoved = 3, Rest = 10)
  grtxt <- c("Gruppe 1", "Gruppe 2", "Gruppe 3")

  # Expect no error and plot should be created
  expect_no_error({
    pdf(tempfile(fileext = ".pdf"))
    FigFordeling(
      AggVerdier = AggVerdier,
      tittel = "Test Figure",
      N = N,
      grtxt = grtxt,
      outfile = ""
    )
    dev.off()
  })
})

test_that("FigFordeling handles total < 11 observations", {
  # Test with sum(N$Hoved + N$Rest) < 11
  AggVerdier <- list(
    Hoved = c(50, 30, 20),
    Rest = c(45, 35, 20)
  )
  N <- list(Hoved = 5, Rest = 3)  # sum = 8, less than 11
  grtxt <- c("Gruppe 1", "Gruppe 2", "Gruppe 3")

  expect_no_error({
    pdf(tempfile(fileext = ".pdf"))
    FigFordeling(
      AggVerdier = AggVerdier,
      tittel = "Test Figure",
      N = N,
      grtxt = grtxt,
      outfile = ""
    )
    dev.off()
  })
})

test_that("FigFordeling creates horizontal bar chart", {
  # Test horizontal bars (retn = 'H')
  AggVerdier <- list(
    Hoved = c(50, 30, 20),
    Rest = c(45, 35, 20)
  )
  N <- list(Hoved = 50, Rest = 60)
  Nfig <- list(Hoved = 50, Rest = 60)
  grtxt <- c("Gruppe 1", "Gruppe 2", "Gruppe 3")

  expect_no_error({
    pdf(tempfile(fileext = ".pdf"))
    FigFordeling(
      AggVerdier = AggVerdier,
      tittel = "Horisontalt diagram",
      hovedgrTxt = "Hovedgruppe",
      smltxt = "Sammenligning",
      N = N,
      Nfig = Nfig,
      retn = "H",
      grtxt = grtxt,
      medSml = 1,
      outfile = ""
    )
    dev.off()
  })
})

test_that("FigFordeling creates vertical bar chart", {
  # Test vertical bars (retn = 'V')
  AggVerdier <- list(
    Hoved = c(50, 30, 20),
    Rest = c(45, 35, 20)
  )
  N <- list(Hoved = 50, Rest = 60)
  Nfig <- list(Hoved = 50, Rest = 60)
  grtxt <- c("Gruppe 1", "Gruppe 2", "Gruppe 3")

  expect_no_error({
    pdf(tempfile(fileext = ".pdf"))
    FigFordeling(
      AggVerdier = AggVerdier,
      tittel = "Vertikalt diagram",
      hovedgrTxt = "Hovedgruppe",
      smltxt = "Sammenligning",
      N = N,
      Nfig = Nfig,
      retn = "V",
      grtxt = grtxt,
      medSml = 1,
      outfile = ""
    )
    dev.off()
  })
})

test_that("FigFordeling handles Nfig conversion", {
  # Test that non-numeric Nfig is converted to N
  AggVerdier <- list(
    Hoved = c(50, 30, 20),
    Rest = c(45, 35, 20)
  )
  N <- list(Hoved = 50, Rest = 60)
  Nfig <- "not numeric"  # Should be converted to N
  grtxt <- c("Gruppe 1", "Gruppe 2", "Gruppe 3")

  expect_no_error({
    pdf(tempfile(fileext = ".pdf"))
    FigFordeling(
      AggVerdier = AggVerdier,
      tittel = "Test Nfig Conversion",
      N = N,
      Nfig = Nfig,
      grtxt = grtxt,
      outfile = ""
    )
    dev.off()
  })
})

test_that("FigFordeling handles many groups (>20)", {
  # Test with more than 20 groups
  n_groups <- 25
  AggVerdier <- list(
    Hoved = rep(50 / n_groups, n_groups),
    Rest = rep(50 / n_groups, n_groups)
  )
  N <- list(Hoved = 100, Rest = 100)
  Nfig <- list(Hoved = 50, Rest = 60)
  grtxt <- paste("Gruppe", 1:n_groups)

  expect_no_error({
    pdf(tempfile(fileext = ".pdf"))
    FigFordeling(
      AggVerdier = AggVerdier,
      tittel = "Many Groups",
      N = N,
      Nfig = Nfig,
      grtxt = grtxt,
      retn = "H",
      outfile = ""
    )
    dev.off()
  })
})

test_that("FigFordeling without comparison group (medSml = 0)", {
  # Test without comparison group
  AggVerdier <- list(
    Hoved = c(50, 30, 20),
    Rest = c(45, 35, 20)
  )
  N <- list(Hoved = 50, Rest = 60)
  Nfig <- list(Hoved = 50, Rest = 60)
  grtxt <- c("Gruppe 1", "Gruppe 2", "Gruppe 3")

  expect_no_error({
    pdf(tempfile(fileext = ".pdf"))
    FigFordeling(
      AggVerdier = AggVerdier,
      tittel = "Uten sammenligning",
      hovedgrTxt = "Hovedgruppe",
      N = N,
      Nfig = Nfig,
      grtxt = grtxt,
      medSml = 0,
      outfile = ""
    )
    dev.off()
  })
})

test_that("FigFordeling handles multiple decimal places", {
  # Test different antDes values
  AggVerdier <- list(
    Hoved = c(50.123, 30.456, 20.789),
    Rest = c(45.111, 35.222, 20.333)
  )
  N <- list(Hoved = 50, Rest = 60)
  grtxt <- c("Gruppe 1", "Gruppe 2", "Gruppe 3")

  expect_no_error({
    pdf(tempfile(fileext = ".pdf"))
    FigFordeling(
      AggVerdier = AggVerdier,
      tittel = "Med desimaler",
      N = N,
      Nfig = "Not numeric",
      grtxt = grtxt,
      antDes = 2,
      outfile = ""
    )
    dev.off()
  })
})

test_that("FigFordeling handles subtitle and selection text", {
  # Test with subtxt and utvalgTxt
  AggVerdier <- list(
    Hoved = c(50, 30, 20),
    Rest = c(45, 35, 20)
  )
  N <- list(Hoved = 50, Rest = 60)
  grtxt <- c("Gruppe 1", "Gruppe 2", "Gruppe 3")
  utvalgTxt <- c("Utvalg: Pasienter > 18 år", "Diagnose: A00-A99")

  expect_no_error({
    pdf(tempfile(fileext = ".pdf"))
    FigFordeling(
      AggVerdier = AggVerdier,
      tittel = "Med undertittel",
      subtxt = "Undertittel",
      N = N,
      Nfig = "Not numeric",
      grtxt = grtxt,
      utvalgTxt = utvalgTxt,
      outfile = ""
    )
    dev.off()
  })
})

test_that("FigFordeling handles long group names (text wrapping)", {
  # Test text wrapping for long group names
  AggVerdier <- list(
    Hoved = c(50, 30, 20),
    Rest = c(45, 35, 20)
  )
  N <- list(Hoved = 50, Rest = 60)
  grtxt <- c(
    "Dette er en veldig lang gruppenavn som bør brytes over flere linjer",
    "En annen veldig lang beskrivelse av en gruppe som tar mye plass",
    "Og en tredje som også er lang"
  )

  expect_no_error({
    pdf(tempfile(fileext = ".pdf"))
    FigFordeling(
      AggVerdier = AggVerdier,
      tittel = "Long Group Names",
      N = N,
      Nfig = "Not numeric",
      grtxt = grtxt,
      retn = "H",
      outfile = ""
    )
    dev.off()
  })
})

test_that("FigFordeling with different color palettes", {
  # Test with different fargepalett
  AggVerdier <- list(
    Hoved = c(50, 30, 20),
    Rest = c(45, 35, 20)
  )
  N <- list(Hoved = 50, Rest = 60)
  grtxt <- c("Gruppe 1", "Gruppe 2", "Gruppe 3")

  expect_no_error({
    pdf(tempfile(fileext = ".pdf"))
    FigFordeling(
      AggVerdier = AggVerdier,
      tittel = "Med palett",
      N = N,
      Nfig = "Not numeric",
      grtxt = grtxt,
      fargepalett = "BlaaOff",
      outfile = ""
    )
    dev.off()
  })
})

test_that("FigFordeling output validation", {
  # Ensure the function returns invisibly (no explicit return)
  AggVerdier <- list(
    Hoved = c(50, 30, 20),
    Rest = c(45, 35, 20)
  )
  N <- list(Hoved = 50, Rest = 60)
  grtxt <- c("Gruppe 1", "Gruppe 2", "Gruppe 3")

  pdf(tempfile(fileext = ".pdf"))
  result <- FigFordeling(
    AggVerdier = AggVerdier,
    tittel = "Test",
    N = N,
    Nfig = "Not numeric",
    grtxt = grtxt,
    outfile = ""
  )
  dev.off()

  # Function should return NULL invisibly
  expect_null(result)
})
