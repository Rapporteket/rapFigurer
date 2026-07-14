#' Plot indikator
#'
#' @param indikatorData Dataframe med kolonnene: aar, sykehusnavn, teller, nevner
#' @param showYear Årstall som skal vises i plottet. Default
#'  er inneværende år.
#' @param terskel Minimum antall observasjoner for å inkludere sykehus i plottet. Default er 10.
#' @param maalretn Målretning for indikatoren. Kan være "lav" eller "høy". Default er "lav".
#' @return ggplot-objekt
#' @export


plotIndikator <- function(
  indikatorData,
  showYear = lubridate::year(Sys.Date()),
  terskel = 10,
  maalretn = "høy",
  kvalIndBreaks = NULL,
  labelAtBase = TRUE,
  showNlabel = TRUE
) {
  compareYears <- c(showYear - 1, showYear - 2)

  sykehusData <- indikatorData |>
    dplyr::group_by(.data$aar, .data$sykehusnavn) |>
    dplyr::summarise(
      teller = sum(.data$teller, na.rm = TRUE),
      nevner = sum(.data$nevner, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      prosent = .data$teller / .data$nevner,
      type = "Sykehus"
    )

  nasjonalData <- indikatorData |>
    dplyr::group_by(.data$aar) |>
    dplyr::summarise(
      sykehusnavn = "Nasjonalt",
      teller = sum(.data$teller, na.rm = TRUE),
      nevner = sum(.data$nevner, na.rm = TRUE),
      prosent = .data$teller / .data$nevner,
      .groups = "drop"
    ) |>
    dplyr::filter(.data$nevner >= terskel) |>
    dplyr::mutate(
      type = "Nasjonalt"
    )

  allYearsData <- dplyr::bind_rows(sykehusData, nasjonalData) |>
    dplyr::mutate(
      sykehusnavn = dplyr::if_else(.data$type == "Nasjonalt", "Nasjonalt", .data$sykehusnavn)
    )
  maksAndel <- min(max(allYearsData$prosent, na.rm = TRUE) * 1.15, 100)
  prettyVals <- pretty(c(0, maksAndel), n = 5)

  plotData <- allYearsData |>
    dplyr::filter(.data$aar == showYear) |>
    dplyr::mutate(
      overTerskel = .data$nevner >= terskel
    ) |>
    dplyr::mutate(
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

  dotData <- allYearsData |>
    dplyr::filter(.data$aar %in% compareYears) |>
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

  if (!is.null(kvalIndBreaks)) {
    # Farger og legend for kvalitetsindikatorer
    kvalIndLegend <- switch(maalretn,
      "høy" = c("Lav", "Middels", "Høy"),
      "lav" = c("Høy", "Middels", "Lav")
    )
    kvalIndFarger <- switch(maalretn,
      "lav" = c("#3baa34", "#fd9c00", "#e30713"),
      "høy" = c("#e30713", "#fd9c00", "#3baa34")
    )

    kvalIndBreaks <- sort(as.numeric(kvalIndBreaks))
    if (max(kvalIndBreaks, na.rm = TRUE) > 1) {
      kvalIndBreaks <- kvalIndBreaks / 100
    }

    kvalBreaks <- c(0, kvalIndBreaks, 1)
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
    fill = .data$type
  ))
  if (!is.null(kvalIndBreaks)) {
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
        "Sykehus" = "#2171b5",
        "Nasjonalt" = "#084594"
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
      title = paste("Indikator", showYear),
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
      axis.text.y = ggplot2::element_text(size = 12)
    )

  p
}
