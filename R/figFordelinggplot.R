#'Fordelingsfigur
#'
#'ggplot2 utgaven av figFordeling funksjonen. Denne funksjonen lager et horisontalt eller vertikalt søylediagram som viser andeler (fordeling)
#' av en gitt variabel, basert på de aggregerte verdier som gis inn til funksjonen.

#'
#' @param AggVerdier Aggregerte verdier (andeler) som skal visualiseres. Liste med to vektorer Hoved og Rest
#' @param tittel Figurtittel
#' @param hovedgrTxt hovedgruppe, eks. hele landet eller UNN, Tromsø
#' @param smltxt sammenligningstekst, eks. landet forøvrig
#' @param N Totalt antall. Listevariabel: N$Hoved/Rest - antall i hovedgruppa/sammenligningsgruppa
#' @param retn V-vertikale søyler. H-horisontale søyler
#' @param utvalgTxt Tekst over flere linjer som angir hvilket utvalg som er gjort
#' @param grtxt Tekst som angir navn på hver av gruppene i fordelinga.Vektor med lenge lik antall grupper/søyler
#' @param grtxt2 Evt. undertekst til hver av gruppene. Vektor med lenge lik antall grupper/søyler
#' @param medSml 1-med sammenlikningsgruppe, 0-uten sammenlikningsgruppe
#' @param fargepalett hvilken fargepalett fra "rapfig::figfiltype" som skal benyttes
# #' @param outfile hvordan figuren skal vises: ''-på Skjerm, "filnavn.pdf/png/.." - gir fil av angitt type.
#'
#' @return Søylediagram (fordeling).


figFordelinggplot <- function(AggVerdier, tittel='mangler tittel', hovedgrTxt='', smltxt='',
                        N, retn = 'H', utvalgTxt='', grtxt , grtxt2="",
                        medSml=0, fargepalett='BlaaOff', #outfile=''
                        #medKI=0, KImaal = NA, KImaaltxt = '', Ngr, cexgr=1, grTypeTxt='', pstTxt=list(Hoved='', Rest=''),
                        ) {

  #samler dataen til en data frame
  df <- data.frame(AggVerdier,grtxt)
  names(df) <- c("Hoved", "Rest", "under_grupper")
  names(N)  <- c("Hoved","Rest")


  if (!( grtxt2 == "" ) ){
  df$under_grupper <- paste0(df$under_grupper,"\n",grtxt2) }

  farger=rapFigurer::figtype(fargepalett = fargepalett)$farger

  N_hoved <- df %>% dplyr::select(Hoved) %>% sum()
  N_rest <-  df %>% dplyr::select(Rest) %>% sum()


  ymax <- min(1, signif(max(c((df$Hoved/N_hoved)*1.2, (df$Rest/N_rest)*1.2 )), digits = 2) )


  # text som skal stå under x-aksen hvis søylene er vertikale.
  if (retn == "V"){
    if (medSml) {
     txtgb <-  ggplot2::ggplotGrob( ggplot2::ggplot(data = df, ggplot2::aes(x= under_grupper, y = Hoved)) +
                            ggplot2::geom_text(ggplot2::aes(x= under_grupper, y=1.3, label= paste0( signif( (Hoved / N_hoved) * 100, digits = 2), " %" ) ), color = farger[1] ) +
                            ggplot2::geom_text(ggplot2::aes(x= under_grupper, y=0.8, label= paste0( signif( (Rest / N_rest) * 100, digits = 2), " %" ) ), color = farger[3] ) +
                            ylim(0,2) + ggthemes::theme_void())

     }else{
      txtgb <-  ggplot2::ggplotGrob( ggplot2::ggplot(data = df, ggplot2::aes(x = under_grupper, y = Hoved)) +
                          ggplot2::geom_text(ggplot2::aes(x = under_grupper, y = 1, label = as.character( signif( (Hoved / N_hoved) * 100, digits = 2) ) ), color = farger[1] ) +
                          ylim(0,2) + ggthemes::theme_void())
     }
  }


  #Hvis for få observasjoner..
  if ((N$Hoved < 5) | (sum(N$Hoved+N$Rest)<11)){
    #-----------Figur---------------------------------------
    FigTypUt <- rapFigurer::figtype(outfile)  #FigTypUt <- figtype(outfile)
    farger <- rapFigurer::FigTypUt$farger
    graphics::plot.new()
    graphics::title(tittel)	#, line=-6)
    graphics::legend('topleft',legend=utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    tekst <- 'For få registreringer i egen eller sammenligningsgruppe'
    graphics::text(0.5, 0.6, tekst, cex=1.2)
    if ( outfile != '') {grDevices::dev.off()}

  }else {

    f1 <- df %>% ggplot2::ggplot(ggplot2::aes(x = under_grupper, y = Hoved / N_hoved)) +
      ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = farger[1])) +
      ggplot2::labs( title = tittel , x=grtxt, y = "Andel")
      if (!medSml) {
        f1 <- f1 + ggplot2::scale_fill_manual(name="",values=farger[1] ,labels =paste0(hovedgrTxt, " (N = ", N$Hoved, ")" ) ,
                                              guide = ggplot2::guide_legend(override.aes = ggplot2::aes(color = NA)) )

        if(retn == "H"){
            f1 <- f1 +ggplot2::geom_text(ggplot2::aes(x=under_grupper, y=ymax, label = paste0(signif((Hoved /N_hoved)* 100, digit = 2 )," %")), color = farger[1])
          }

      } else{
        f1 <- f1 + ggplot2::geom_point(ggplot2::aes(x = under_grupper, y = Rest / N_rest, fill = farger[3]),
                              shape = 23, color=farger[3], size = 4) +
          ggplot2::scale_fill_manual(name="",values=c(farger[1],farger[3]) ,labels = c(paste0(hovedgrTxt, " (N = ", N$Hoved, ")" ),
                                                                              paste0(smltxt, " (N = ", N$Rest, ")" )),
                          guide = ggplot2::guide_legend(override.aes = ggplot2::aes(color = NA)) )

        if (retn == "H"){
          f1 <- f1 + ggplot2::geom_text(ggplot2::aes(x=under_grupper, y=ymax*0.9, label = paste0(signif((Hoved/N_hoved)*100, digit = 2 )," %")), color = farger[1]) +
            ggplot2::geom_text(ggplot2::aes(x=under_grupper, y=ymax  , label = paste0(signif((Rest/N_rest)*100, digit = 2 )," %")),color= farger[3])
        }
      }

    f1 <- f1 + ggplot2::labs( title = utvalgTxt ,subtitle = tittel , x='', y = "Andel pasienter") +
      ggplot2::scale_y_continuous(limits = c(signif( seq(0, ymax, length.out = 5), digits = 2)[1],signif( seq(0, ymax, length.out = 5), digits = 2)[5]) ,
                         breaks=signif( seq(0, ymax, length.out = 5), digits = 2),labels = scales::percent_format())+
      ggthemes::theme_tufte(base_size = 22) + ggplot2::theme(legend.position = "top",
                                          legend.text = ggplot2::element_text(size = 14, face = "plain", family = "sans" ),
                                          plot.title = ggplot2::element_text(size = 12, face ="plain" , family = "sans", colour = farger[1]),
                                          plot.subtitle = ggplot2::element_text(size=20, face = "bold", family = "sans", hjust = 0.5,),
                                          axis.title = ggplot2::element_text(family = "sans", size = 16, face = "plain"),
                                          axis.text = ggplot2::element_text(family = "sans", size = 14)  ) +
      ggplot2::annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = ymax, size = 1, lineend = "square")

   }


  #horisontale søyler.
  if (retn == "H") {
    f1 <- f1 + ggplot2::coord_flip() + ggplot2::theme(plot.margin = ggplot2::margin(r=30),
                                    axis.ticks.y = ggplot2::element_blank() )

  } else {
      if(grtxt2 == ""){
          f1 <- f1 + ggplot2::annotation_custom( txtgb, xmin = 0.5, xmax = length( df$Hoved) + 0.5, ymax = -ymax/10, ymin = -ymax/3.2) +
            ggplot2::theme (axis.ticks.x = ggplot2::element_blank(),
                     plot.margin =  ggplot2::margin(b = 30))
        } else { f1 <- f1 + ggplot2::annotation_custom( txtgb, xmin = 0.5, xmax = length( df$Hoved) + 0.5, ymax = -ymax/10, ymin = -ymax/2.6) +
        ggplot2::theme (axis.ticks.x = ggplot2::element_blank(),
             plot.margin =  ggplot2::margin(b = 30))
      }

  }

  return (f1 )

}


