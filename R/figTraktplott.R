#' Traktplott for sykehus
#'
#' For å få ønsket sortering lages variabelen Sykehus som en faktor
#'
#' @param sykehus Vektor med sykehusnavn, sortert i ønsket rekkefølge
#' @param teller Teller (Vektor med antall "hendelser" ved hvert sykehus, sortert i ønsket rekkefølge)
#' @param nevner Nevner (Vektor med antall observasjoner ved hvert sykehus), sortert i ønsket rekkefølge)
#' @param data datasettet (dataramme)
#' @param tittel Tittel
#' @param undertittel evt. undertittel
#' @param punktetikett evt. kommentar ved hvert av punktene. Kommer til høyre i plottet
#' @param xlab undertittel x-akse
#' @param ylab undertittel y-akse
#' @param midtlinje Legge midtlinja på ønsket verdi
#' @param maal Mållinje..
#'
#' @return Traktplott med andeler av gitt variabel for hvert sykehus
#' @export
#'
#' @examples
#' #Not run
#' #figTraktplott()
figTraktplott <- function(sykehus=DataTraktplott$Sykehus,
                          teller=DataTraktplott$Andel*DataTraktplott$N,
                          nevner=DataTraktplott$N,
                          data = DataTraktplott,
                          tittel='Eksempeltittel',
                          undertittel = 'undertittel',
                          punktetikett = '',
                          xlab = '',
                          ylab = '',
                          midtlinje = 0.7, #Kan benyttes til å legge midtlinja på ønsket verdi
                          maal = NA
) {

  sykehus <- as.factor(paste0(sykehus, '\n N=', nevner))
  sykehus <- reorder(sykehus, nevner) #"Sorterer" sykehus
  # data$Sykehus <- as.factor(paste0(data$Sykehus, '\n N=', data$N))
  # data$Sykehus <- reorder(data$Sykehus, data$N) #"Sorterer" sykehus uten at dette påvirker selve matrisa
  # DataPlott$Sykehus <- forcats::fct_inorder(data$Sykehus) #as.factor(data$Sykehus)
  #Sykehus = with(Data, fct_reorder(Sykehus, N)

#Denne kan legges til som et ekstra valg i AndelGrVar
  # qicharts2::qic(x=DataTraktplott$Sykehus,
  #   y=DataTraktplott$Andel*DataTraktplott$N,
  #   n=DataTraktplott$N,
  #   data = DataTraktplott,
  #   title='Eksempeltittel',
  #   subtitle = 'undertittel',
  #   chart = 'p'
  # )


qicharts2::qic(x=sykehus,
               y=teller,
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
}
