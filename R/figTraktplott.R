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
FigTraktplott <- function(sykehus=DataTraktplott$Sykehus,
                          andel=DataTraktplott$Andel,
                          nevner=DataTraktplott$N,
                          data = NULL,
                          tittel='Eksempeltittel',
                          undertittel = '',
                          punktetikett = '',
                          xlab = '',
                          ylab = '',
                          midtlinje = NA*1, #Kan benyttes til å legge midtlinja på ønsket verdi
                          maal = NA,
                          sort = 'nevner',
                          #utvalgTxt = NULL,
                          ...) {

  sykehus <- as.factor(paste0(sykehus, ' (', nevner, ')'))  #'\n N=', nevner)
  sykehus <- reorder(sykehus, switch(sort, nevner=nevner, andel=andel)) #"Sorterer" sykehus
  # data$Sykehus <- as.factor(paste0(data$Sykehus, '\n N=', data$N))
  # data$Sykehus <- reorder(data$Sykehus, data$N) #"Sorterer" sykehus uten at dette påvirker selve matrisa
  # DataPlott$Sykehus <- forcats::fct_inorder(data$Sykehus) #as.factor(data$Sykehus)
  #Sykehus = with(Data, fct_reorder(Sykehus, N)

 #Pt ikke funksjonell
  #  if (!is.null(utvalgTxt)) { #Hvis ønsker å spesifisere utvalget over selve figuren
  # vmarg <- max(0, strwidth(sykehus, units='figure', cex=0.7))
  # #NB: strwidth oppfører seg ulikt avh. av device...
  # par('fig'=c(vmarg, 1, 0, 1-0.02*length(utvalgTxt))) }

  qicharts2::qic(x=sykehus,
               y=round(andel*nevner),
               n=nevner,
               data = data,
               cl = midtlinje, #Kan benyttes til å legge midtlinja på ønsket verdi
               #target = maal ,
               title=tittel,
                subtitle = undertittel,
                notes = punktetikett,
                xlab = xlab,
                ylab = ylab,
               flip = T, #strip.horizontal = T,
                chart='p')
#mtext(utvalgTxt, side=3, las=1, cex=1, adj=0, line=c(2+0.8*(NutvTxt:1)))
}
