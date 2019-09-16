#'Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#'Ggplot2 versjon av andelGrVar.
#'
#'@param Aggverdier Aggregerte verdier (andeler) som skal visualiseres.
#'@param AggTot Den totale aggregerte verdien.
#'@param N  Det totale antallet.
#'@param Ngr En vector med det totale antallet i hvergruppe.
#'@param grtxt  Tekst som angir navn på hver av gruppene i fordelinga.Vektor med lenge lik antall grupper/søyler
#'@param grtxt2 Evt. undertekst til hver av gruppene. Vektor med lenge lik antall grupper/søyler
#'@param maalopp Måloppnåelse: TRUE - med måloppnåelse, FALSE - Uten måloppnåelse
#'@param oppnivaa Måloppnåelsenivå: en liste med grenseverdiene for hæy og moderat oppnåelse. list(hoy = c(min, max), mod = c(min, max))
#'@param medKI  Konfidenseintervall: TRUE - med konfidensintervall og False - uten konfidensintervall.
#'@param Knivaa Målenivå mellom 0 og 1. 0.95 er satt som standard.
#'@param tittel figur tittel
#'@param retn V-vertikale søyler. H-horisontale søyler
#'@param utvalgTxt Tekst over flere linjer som angir hvilket utvalg som er gjort
#'@param fargepalett hvilken fargepalett fra "rapfig::figfiltype" som skal benyttes
#'
#'@return søylediagram
#'
#'@export
#'
figAndelGrVarggplot <- function (AggVerdier, AggTot, N, Ngr, grtxt, grtxt2 = "", maalopp = FALSE, oppnivaa = list(hoy = c(0,0.2),mod = c (0.2,0.4)),
                    medKI =FALSE, Knivaa = 0.95, tittel = "mangler tittel",retn = "H", #xAkseTxt=xAkseTxt, #NIRVarSpes$xAkseTxt,KImaaltxt = KImaaltxt,
                    #grTypeTxt=RyggUtvalg$grTypeTxt,
                     utvalgTxt = " ", fargepalett = "BlaaOff"  ){

  grtxt <- paste0(grtxt," (",Ngr,")" )

  if(!( grtxt2 == "" ) ) { grtxt <- paste0(grtxt, "\n", grtxt2) }

  df <- data.frame(AggVerdier, Ngr, grtxt)
  names(df) <- c("Aggverdier","Ngr","grtxt")
  ymax <- min(1, signif(max(df$Aggverdier * 1.15),digits = 2))
  farger <- rapFigurer::figtype(fargepalett = "BlaaOff")$farger


  f1 <- df %>% ggplot2::ggplot( ggplot2::aes( x = stats::reorder( grtxt,-Aggverdier), y = Aggverdier ))
    if (maalopp){
      f1 <- f1 + ggplot2::geom_rect(data = df, ggplot2::aes(ymin = min(oppnivaa$hoy), ymax = max(oppnivaa$hoy), fill = "høy"), xmin = -Inf, xmax = Inf)+
        ggplot2::geom_rect(data = df, ggplot2::aes(ymin = min(oppnivaa$mod), ymax = max(oppnivaa$mod), fill = "moderat"), xmin = -Inf, xmax = Inf) +
        ggplot2::scale_fill_manual(name="Måloppnåelse:",values=c("green", "yellow") ,labels = c("Høy", "Moderat" ) )
    }

    if(medKI){f1 <- f1

    }else{
      f1 <-  f1 + ggplot2::geom_text(ggplot2::aes(x = stats::reorder( grtxt,-Aggverdier), y = Aggverdier+0.04),
                            label = paste0(signif(df$Aggverdier * 100,digits = 3 )," %"),
                            color = farger[1], size = 6, )  }

  f1 <- f1+ ggplot2::geom_bar(stat = "identity", fill = farger[4]) +
    ggplot2::geom_hline( ggplot2::aes(yintercept = AggTot,
                   color = paste0("Hele landet (", signif(AggTot * 100,digits = 3) , " %), N = ", N)), size = 2) +
    ggplot2::labs( title = utvalgTxt ,subtitle = tittel , x='', y = "Andel pasienter") +
    ggplot2::scale_y_continuous(limits = c(signif( seq(0, ymax, length.out = 5), digits = 2)[1], signif( seq(0, ymax, length.out = 5), digits = 2)[5]) ,
                       breaks = signif( seq(0, ymax, length.out = 5), digits = 2) , labels = scales::percent_format())+
    ggplot2::scale_color_manual(name= "", values = farger[1]) +
    ggplot2::annotate("segment", x = 0.4, xend = 0.4, y = 0, yend = ymax)+
    ggthemes::theme_tufte() +
    ggplot2::theme( plot.title = ggplot2::element_text(size = 12, face ="plain" , family = "sans", colour = farger[1]),
           plot.subtitle = ggplot2::element_text(size=20, face = "bold", family = "sans", hjust = 0.5,),
           axis.text = ggplot2::element_text( family = "sans", face = "plain", size = 12 , color = "black"),
           legend.position = "top",
           legend.box = "vertical",
           legend.spacing = ggplot2::unit(-3,"mm"),
           legend.text = ggplot2::element_text(size = 12, face = "plain", family = "sans" ),
           legend.title = ggplot2::element_text(size = 14, face = "plain", family = "sans",margin = ggplot2::margin((r=-10) )),
           legend.justification = 0.5 )

  if (retn == "H") {
    f1 <- f1 + ggplot2::coord_flip() +
      ggplot2::theme( axis.title.x = ggplot2::element_text(family = "sans" , face = "plain" ,size = 14 ),
             axis.title.y = ggplot2::element_blank(),
             axis.ticks.y = ggplot2::element_blank(),
             axis.ticks.length.x = ggplot2::unit(2.5, "mm"),
             axis.line.y  = ggplot2::element_blank(),
             axis.text.y.left = ggplot2::element_text( margin = ggplot2::margin(r = -25 )) )
  }

  if (retn== "V"){
    f1 <- f1 + ggplot2::theme( axis.title.y = ggplot2::element_text(family = "sans" , face = "plain" ,size = 14 ),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.ticks.length.y = ggplot2::unit(2.5, "mm"),
                     axis.line.x  = ggplot2::element_blank(),
                     axis.text.x.bottom = ggplot2::element_text( margin = ggplot2::margin(t = -20 )) )
  }
  return(f1)
}
