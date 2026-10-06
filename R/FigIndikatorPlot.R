#' Plot indikator
#'
#' @param indikatorData Dataframe i samme form som dataene som brukes i
#' SKDE-indikatorer. Kolonnene som må være med er: "year", "orgnr", "var",
#' "denominator".
#' @param title Valfri titel på plottet.
#' @param shortDescription Kort undertittel for plottet.
#' @param showYear Årstall som skal vises i plottet. Default er inneværende år.
#' @param terskel Minimum antall observasjoner for å inkludere sykehus i plottet.
#' Default er 10.
#' @param levelDirection Retning for kvalitetsindikatoren. 1 betyr høyere verdi er
#' bedre, 0 betyr lavere verdi er bedre.
#' @param kvalIndgrenser Valgfrie kvalitetsgrenser som angir nivåer for bakgrunnsmarkering.
#' @param labelAtBase Logisk verdi som avgjør om etikettene for de aktuelle sykehusene
#' skal plasseres ved x=0 eller ved hver stolpe.
#' @param showNlabel Logisk verdi som avgjør om sykehusetikettene skal inkludere antall
#' observasjoner (N=...) eller bare sykehusnavn.
#' @return ggplot-objekt
#' @export


plotIndikator <- function(
  indikatorData,
  title = NULL,
  shortDescription = NULL,
  showYear = lubridate::year(Sys.Date()),
  terskel = 10,
  levelDirection = 1,
  kvalIndgrenser = NULL,
  labelAtBase = TRUE,
  showNlabel = TRUE
) {
  compareYears <- c(showYear - 1, showYear - 2)

  indikatorData <- indikatorData |>
    dplyr::mutate(
      aar = as.integer(.data$year),
      sykehusnavn = as.character(.data$orgnr),
      teller = as.numeric(.data$var) * as.numeric(.data$denominator),
      nevner = as.numeric(.data$denominator),
      prosent = .data$var
    )


  maksAndel <- min(max(indikatorData$prosent, na.rm = TRUE) * 1.15, 100)
  prettyVals <- pretty(c(0, maksAndel), n = 5)

  plotData <- indikatorData |>
    dplyr::filter(.data$aar == showYear) |>
    dplyr::mutate(
      overTerskel = .data$nevner >= terskel,
      prosentBar = dplyr::if_else(.data$overTerskel, .data$prosent, 0),
      prosentLabel = dplyr::if_else(.data$overTerskel, scales::percent(.data$prosent, accuracy = 0.1), "")
    )

  # Sortering:
  # - sykehus sorteres etter prosent
  # - nasjonal beholdes i sin naturlige plassering etter prosent
  plotData <- plotData |>
    dplyr::arrange(dplyr::desc(.data$overTerskel), dplyr::desc(.data$prosent)) |>
    dplyr::mutate(
      sykehusnavn_display = if (showNlabel) {
        dplyr::if_else(
          .data$overTerskel,
          paste0(.data$sykehusnavn, " (N=", .data$nevner, ")"),
          paste0(.data$sykehusnavn, " (N<", terskel, ")")
        )
      } else {
        as.character(.data$sykehusnavn)
      },
      sykehusnavn_display = factor(
        .data$sykehusnavn_display,
        levels = unique(.data$sykehusnavn_display)
      )
    )

  dotData <- indikatorData |>
    dplyr::filter(.data$aar %in% compareYears, .data$nevner >= terskel) |>
    dplyr::semi_join(
      plotData |>
        dplyr::filter(.data$overTerskel) |>
        dplyr::select(.data$sykehusnavn),
      by = "sykehusnavn"
    ) |>
    dplyr::left_join(
      plotData |> dplyr::select(.data$sykehusnavn, .data$sykehusnavn_display),
      by = "sykehusnavn"
    ) |>
    dplyr::mutate(
      sykehusnavn_display = factor(.data$sykehusnavn_display, levels = levels(plotData$sykehusnavn_display)),
      aar = factor(.data$aar, levels = sort(compareYears, decreasing = TRUE))
    )

  maxProsent <- max(c(plotData$prosentBar, dotData$prosent), na.rm = TRUE)
  if (!is.finite(maxProsent) || maxProsent <= 0) {
    maxProsent <- 1
  }

  if (!is.null(kvalIndgrenser)) {
    if (as.integer(levelDirection) == 1) {
      kvalIndLegend <- c("Lav", "Middels", "Høy")
      kvalIndFarger <- c("#e30713", "#fd9c00", "#3baa34")
    } else {
      kvalIndLegend <- c("Høy", "Middels", "Lav")
      kvalIndFarger <- c("#3baa34", "#fd9c00", "#e30713")
    }

    kvalIndgrenser <- sort(as.numeric(kvalIndgrenser))
    if (max(kvalIndgrenser, na.rm = TRUE) > 1) {
      kvalIndgrenser <- kvalIndgrenser / 100
    }

    kvalBreaks <- c(0, kvalIndgrenser, 1)
    indikatorBand <- data.frame(
      xmin = kvalBreaks[-length(kvalBreaks)],
      xmax = kvalBreaks[-1],
      ymin = 0.5,
      ymax = length(unique(plotData$sykehusnavn)) + 0.5,
      indLevels = factor(kvalIndLegend, levels = kvalIndLegend)
    )

    # Hvis aksen ikke går til 100, klipp båndene til ovre grense
    indikatorBand$xmin <- pmax(indikatorBand$xmin, 0)
    indikatorBand$xmax <- pmin(indikatorBand$xmax, maxProsent * 1.15)

    # Fjern bånd som ender opp tomme
    indikatorBand <- indikatorBand[indikatorBand$xmax > indikatorBand$xmin, ]
  }

  p <- ggplot2::ggplot(plotData, ggplot2::aes(
    x = .data$prosentBar,
    y = .data$sykehusnavn_display,
    fill = "Sykehus"
  ))
  if (!is.null(kvalIndgrenser)) {
    p <- p + ggplot2::geom_rect(
      data = indikatorBand,
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax
      ),
      inherit.aes = FALSE,
      fill = kvalIndFarger[seq_len(nrow(indikatorBand))],
      alpha = 0.20
    )
  } else {
    NULL
  }
  p <- p + ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_point(
      data = dotData,
      mapping = ggplot2::aes(
        x = .data$prosent,
        y = .data$sykehusnavn_display,
        color = .data$aar,
        shape = .data$aar
      ),
      inherit.aes = FALSE,
      size = 2.8,
      stroke = 1.1,
      fill = "white"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = if (labelAtBase) 0 else .data$prosentBar,
        label = .data$prosentLabel
      ),
      hjust = -0.1,
      size = 5.5,
      color = if (labelAtBase) "white" else "#2171b5"
    ) +
    ggplot2::scale_x_continuous(
      breaks = prettyVals,
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, maxProsent * 1.15),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +

    ggplot2::scale_fill_manual(
      values = c(
        "Sykehus" = "#2171b5"
      ),
      guide = "none"
    ) +
    ggplot2::scale_color_manual(
      values = stats::setNames(
        c("#9ecae1", "#6baed6"),
        as.character(c(showYear - 1, showYear - 2))
      )
    ) +
    ggplot2::scale_shape_manual(
      values = stats::setNames(
        c(16, 17),
        as.character(c(showYear - 1, showYear - 2))
      )
    ) +
    ggplot2::labs(
      title = title,
      subtitle = shortDescription,
      x = "Prosent",
      y = NULL,
      fill = NULL,
      color = "Tidligere år",
      shape = "Tidligere år"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(r = 30),
      axis.ticks.x = ggplot2::element_line(color = "black"),
      axis.line.x = ggplot2::element_line(color = "black"),
      axis.line.y = ggplot2::element_line(color = "black"),
      legend.position = "top",
      legend.justification = "center",
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 14)
    )

  p
}
