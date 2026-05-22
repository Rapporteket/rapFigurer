
describe("figtype", {

  it("returns a list with correct names", {
    result <- figtype(outfile = "")
    expect_type(result, "list")
    expect_named(result, c("res", "width", "height", "farger"))
  })

  it("returns correct default values", {
    result <- figtype(outfile = "")
    expect_equal(result$res, 3 * 72)
    expect_equal(result$width, 3 * 595)
    expect_equal(result$height, 3 * 595)
  })

  it("accepts custom width and height", {
    result <- figtype(outfile = "", width = 1000, height = 500)
    expect_equal(result$width, 1000)
    expect_equal(result$height, 500)
  })

  it("accepts custom resolution", {
    result <- figtype(outfile = "", res = 300)
    expect_equal(result$res, 300)
  })

  it("handles PNG file extension", {
    outfile <- file.path(tempdir(), "test_fig.png")
    expect_silent(figtype(outfile = outfile))
    plot(1:10)
    invisible(dev.off())
    expect_true(file.exists(outfile))
  })

  it("handles PDF file extension", {
    outfile <- file.path(tempdir(), "test_fig.pdf")
    expect_silent(figtype(outfile = outfile))
    expect_true(file.exists(outfile))
  })

  it("handles JPG file extension", {
    outfile <- file.path(tempdir(), "test_fig.jpg")
    expect_silent(figtype(outfile = outfile))
    plot(1:10)
    invisible(dev.off())
    expect_true(file.exists(outfile))
  })

  it("handles BMP file extension", {
    outfile <- file.path(tempdir(), "test_fig.bmp")
    expect_silent(figtype(outfile = outfile))
    plot(1:10)
    invisible(dev.off())
    expect_true(file.exists(outfile))
  })

  it("handles TIF file extension", {
    outfile <- file.path(tempdir(), "test_fig.tif")
    expect_silent(figtype(outfile = outfile))
    plot(1:10)
    invisible(dev.off())
    if (capabilities()["tiff"]) {
      expect_true(file.exists(outfile))
    }
  })

  it("handles SVG file extension", {
    outfile <- file.path(tempdir(), "test_fig.svg")
    expect_silent(figtype(outfile = outfile))
    expect_true(file.exists(outfile))
  })

  describe("Color palettes", {

    it("returns BlaaRapp colors", {
      result <- figtype(outfile = "", fargepalett = "BlaaRapp")
      expect_type(result$farger, "character")
      expect_length(result$farger, 4)
    })

    it("returns BlaaOff colors (default)", {
      result <- figtype(outfile = "", fargepalett = "BlaaOff")
      expect_type(result$farger, "character")
      expect_length(result$farger, 4)
    })

    it("returns BlaaOffAlle colors", {
      result <- figtype(outfile = "", fargepalett = "BlaaOffAlle")
      expect_type(result$farger, "character")
      expect_length(result$farger, 6)
    })

    it("returns StotteOff colors", {
      result <- figtype(outfile = "", fargepalett = "StotteOff")
      expect_type(result$farger, "character")
      expect_length(result$farger, 5)
    })

    it("returns OffAlleFarger colors", {
      result <- figtype(outfile = "", fargepalett = "OffAlleFarger")
      expect_type(result$farger, "character")
      expect_length(result$farger, 11)
    })

    it("returns BlaaHNpms287 colors", {
      result <- figtype(outfile = "", fargepalett = "BlaaHNpms287")
      expect_type(result$farger, "character")
      expect_length(result$farger, 5)
    })

    it("returns GronnHNpms624 colors", {
      result <- figtype(outfile = "", fargepalett = "GronnHNpms624")
      expect_type(result$farger, "character")
      expect_length(result$farger, 5)
    })

    it("returns GronnHNpms342 colors", {
      result <- figtype(outfile = "", fargepalett = "GronnHNpms342")
      expect_type(result$farger, "character")
      expect_length(result$farger, 5)
    })

    it("returns NULL colors if wrong name", {
      result <- figtype(outfile = "", fargepalett = "qwerty")
      expect_null(result$farger)
    })
  })
})
