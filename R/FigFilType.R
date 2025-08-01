#' Funksjon som angir hvilket format en figur skal lagres i. Inneholder også definisjon av fargepaletter.
#'
#' Formatet er spesifisert gjennom endinga til filnavnet
#' Tillatt: png, pdf, jpg, bmp, tif, svg og ps
#' Velge farger: BlaaRapp, BlaaHNpms287, GronnHNpms624, GronnHNpms342
#'
#' @param outfile Navn på fil som skal skrives til. Tom tekststreng skriver til skjerm
#' @param res Oppløsningen på raster-output
#' @param width Bredde på output ved gitt oppløsning
#' @param height Høyde på output ved gitt oppløsning
#' @param pointsizePDF Skrifstørrelse i pdf-output
#' @param fargepalett Hvilken fargepalett skal brukes
#' @return UtFigFil List med en del figurparametre
#' @keywords rapporteket norgast
#' @importFrom grDevices bmp jpeg pdf png rgb tiff
#' @export

figtype <- function(
  outfile = '',
  width = 3 * 595,
  height = 3 * 595,
  res = 3 * 72,
  pointsizePDF = 11,
  fargepalett = 'BlaaOff'
) {

  filtype <- substr(outfile, nchar(outfile) - 2, nchar(outfile))
  if (substr(filtype, 1, 1) == '.') {
    filtype <- substr(filtype, 2, 3)
  }


  fonttype <- 'sans'

  switch(
    filtype,
    png = png(filename = outfile, res = res, family = fonttype, width = width, height = height),
    jpg = jpeg(filename = outfile, res = res, width = width, height = height),
    pdf = cairo_pdf(file = outfile, width = 7, height = 7 * height / width, family = fonttype,
                    pointsize = pointsizePDF),
    bmp = bmp(filename = outfile, res = res, width = width, height = height),
    tif = tiff(filename = outfile, res = res, width = width, height = height),
    wmf = grDevices::win.metafile(filename = outfile, width = 7, height = 7 * height / width,
                                  pointsize = pointsizePDF),
    svg = svg(file = outfile, width = 7, height = 7 * height / width, family = fonttype,
              pointsize = pointsizePDF)
  )


  #col2rgb for å oversette hex
  metning <- rev(c(1, 0.7, 0.45, 0.25, 0.1)) * 255
  #Alle standard "offentliggjøringsfarger":
  #HEX: #c6dbef #6baed6 #4292c6 #2171b5 #084594 #000059 #FF7260 #4D4D4D #737373 #A6A6A6 #DADADA
  OffAlleFarger <- c('#c6dbef', '#6baed6', '#4292c6', '#2171b5', '#084594', '#000059',
                     '#FF7260', '#4D4D4D', '#737373', '#A6A6A6', '#DADADA')
  rgb(red = c(198, 107, 66, 33, 8, 0, 255, 077, 115, 166, 204),  #rev(
      green = c(219, 174, 146, 113, 69, 0, 114, 077, 115, 166, 204),
      blue = c(239, 214, 198, 181, 148, 89, 096, 077, 115, 166, 204),
      maxColorValue = 255)
  UtFarger <- switch(
    fargepalett,
    BlaaRapp = rgb(red = c(0, 86, 149, 218), green = c(79, 139, 189, 230), blue = c(158, 191, 230, 242),
                   maxColorValue = 255),
    BlaaOff = OffAlleFarger[rev(c(1, 2, 4, 5))],
    BlaaOffAlle = OffAlleFarger[6:1],
    StotteOff = OffAlleFarger[7:11],
    OffAlleFarger = OffAlleFarger,
    BlaaHNpms287 = rgb(red = 0, green = 50, blue = 131, maxColorValue = 255, alpha = metning),
    GronnHNpms624 = rgb(red = 131, green = 156, blue = 143, maxColorValue = 255, alpha = metning),
    GronnHNpms342 = rgb(red = 47, green = 101, blue = 74, maxColorValue = 255, alpha = metning)
  )

  #NB: Mørke farger vil dekke over lysere når bruker samme farge med ulik metning!

  UtFigFil <- list(res, width, height, UtFarger)
  names(UtFigFil) <- c('res', 'width', 'height', 'farger')
  return(invisible(UtFigFil))
}
