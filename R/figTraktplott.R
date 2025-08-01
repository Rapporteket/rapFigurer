#' Traktplott for sykehus
#'
#' Fortrinnsvis for resultater som andel per sykehus. Bør evt. legge  til visning av utvalgstekst.
#'
#' @param sykehus Vektor med sykehusnavn, sortert i ønsket rekkefølge
#' @param andel Andel som ønskes vist. Verdi mellom 0 og 1
#' @param nevner Nevner (Vektor med antall observasjoner ved hvert sykehus)
#' @param data datasettet (dataramme)
#' @param tittel Tittel
#' @param undertittel evt. undertittel
#' @param punktetikett evt. kommentar ved hvert av punktene. Kommer til høyre i plottet
#' @param xlab undertittel x-akse
#' @param ylab undertittel y-akse
#' @param midtlinje Legge midtlinja på ønsket verdi
#' @param maal Mållinje..
#' @param sort sortering i plottet: andel, nevner(standard)
#'
#' @return Traktplott med andeler av gitt variabel for hvert sykehus
#' @export
#'
FigTraktplott <- function(sykehus = c('Sykehus A', 'Sykehus B', 'Sykehus C'),
                          andel = c(0.8, 0.9, 0.7),
                          nevner = c(100, 200, 150),
                          data  =  NULL,
                          tittel = 'Eksempeltittel',
                          undertittel = '',
                          punktetikett = '',
                          xlab = '',
                          ylab = '',
                          midtlinje = NA * 1, #Kan benyttes til å legge midtlinja på ønsket verdi
                          maal = NA,
                          sort = 'nevner',
                          ...) {

  sykehus <- as.factor(paste0(sykehus, ' (', nevner, ')'))
  sykehus <- reorder(sykehus, switch(sort, nevner = nevner, andel = andel)) #"Sorterer" sykehus

  qicharts2::qic(
    x = sykehus,
    y = round(andel * nevner),
    n = nevner,
    data = data,
    cl = midtlinje, #Kan benyttes til å legge midtlinja på ønsket verdi
    title = tittel,
    subtitle = undertittel,
    notes = punktetikett,
    xlab = xlab,
    ylab = ylab,
    flip = TRUE,
    chart = 'p'
  )
}
