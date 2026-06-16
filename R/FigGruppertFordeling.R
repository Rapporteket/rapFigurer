#' Plot Grouped Distribution
#'
#' Creates a horizontal stacked bar chart showing the distribution of a categorical
#' variable, optionally split into rows by a grouping variable.
#'
#' @param data A data frame containing the variables to plot.
#' @param gruppeVariabel Character. Optional name of the variable whose values become
#'   the rows of the chart. If NULL, a single row labelled by `totalLabel` is shown.
#' @param kategoriVariabel Character. Name of the variable whose values are stacked
#'   within each row (the fill variable).
#' @param kategoriNavn Named character vector. Optional recoding for category
#'   values, e.g. `c(old = "new")`.
#' @param totalLabel Character. Label used for the aggregate/total row when
#'   `gruppeVariabel` is provided. Default: "Totalt".
#' @param yEtikett Character. Label for the proportion axis. Default: "Andel".
#' @param legendTitle Character. Title for the fill legend. If NULL, uses `kategoriVariabel`.
#'
#' @return A ggplot2 plot object.
#'
#' @details
#' The function returns a regular ggplot object with tooltip text stored in the
#' `text` aesthetic. This makes the function useful both for static plotting and
#' as a thin wrapper around plotly in Shiny applications.
#'
#' A typical Shiny usage pattern is to create the figure in a reactive and then
#' wrap it with `plotly::ggplotly(tooltip = "text")` inside `plotly::renderPlotly()`.
#' This preserves the custom tooltip text defined in the plot and avoids plotly
#' falling back to default hover content.
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' plotGruppertFordeling(data, kategoriVariabel = "outcome")
#'
#' # Shiny + plotly wrapper
#' output$gruppert_fordeling_plot <- plotly::renderPlotly({
#'   plot_fordeling_reactive() |>
#'     plotly::ggplotly(tooltip = "text") |>
#'     plotly::config(displayModeBar = FALSE)
#' })
#' }
#'
#' @export
plotGruppertFordeling <- function(
  data,
  gruppeVariabel = NULL,
  kategoriVariabel,
  kategoriNavn = NULL,
  totalLabel = "Totalt",
  yEtikett = "Andel",
  legendTitle = NULL
) {
  fillColors <- c(
    "#c6dbef", "#6baed6", "#4292c6", "#2171b5", "#084594", "#000059",
    "#FF7260", "#4D4D4D", "#737373", "#A6A6A6", "#DADADA"
  )

  if (is.null(legendTitle)) {
    legendTitle <- kategoriVariabel
  }

  kategoriKilde <- data[[kategoriVariabel]]
  gruppeKilde <- if (is.null(gruppeVariabel)) NULL else data[[gruppeVariabel]]

  finnNivaaRekkefolge <- function(verdier) {
    unikeVerdier <- unique(verdier)
    numeriskeVerdier <- suppressWarnings(as.numeric(unikeVerdier))

    if (all(!is.na(numeriskeVerdier))) {
      unikeVerdier[order(numeriskeVerdier)]
    } else {
      unikeVerdier
    }
  }

  # Remove rows with missing values in the category variable
  dataFiltered <- data |>
    dplyr::filter(!is.na(.data[[kategoriVariabel]]))

  # Standardise to two working columns: `gruppe` and `kategori`
  if (is.null(gruppeVariabel)) {
    dataFiltered <- dataFiltered |>
      dplyr::mutate(
        gruppe = totalLabel,
        kategori = as.character(.data[[kategoriVariabel]])
      )
  } else {
    dataFiltered <- dataFiltered |>
      dplyr::filter(!is.na(.data[[gruppeVariabel]])) |>
      dplyr::mutate(
        gruppe = as.character(.data[[gruppeVariabel]]),
        kategori = as.character(.data[[kategoriVariabel]])
      )
  }

  # Optionally recode category labels
  if (!is.null(kategoriNavn)) {
    dataFiltered <- dataFiltered |>
      dplyr::mutate(
        kategori = dplyr::recode(.data$kategori, !!!kategoriNavn)
      )
  }

  # Preserve factor level order from source data when available
  categoryLevels <- if (is.factor(kategoriKilde)) {
    kategoriNivaaer <- levels(kategoriKilde)

    if (!is.null(kategoriNavn)) {
      kategoriNivaaer <- dplyr::recode(kategoriNivaaer, !!!kategoriNavn)
    }

    unique(kategoriNivaaer[kategoriNivaaer %in% dataFiltered$kategori])
  } else {
    finnNivaaRekkefolge(dataFiltered$kategori)
  }

  dataFiltered <- dataFiltered |>
    dplyr::mutate(
      kategori = factor(.data$kategori, levels = categoryLevels)
    )

  # Append a total row that aggregates all groups
  if (is.null(gruppeVariabel)) {
    plotData <- dataFiltered
  } else {
    totalRowData <- dataFiltered |>
      dplyr::mutate(gruppe = totalLabel)

    plotData <- dplyr::bind_rows(dataFiltered, totalRowData)
  }

  # Compute counts, proportions and hover tooltip per group × category
  plotData <- plotData |>
    dplyr::count(.data$gruppe, .data$kategori, name = "antall") |>
    dplyr::group_by(.data$gruppe) |>
    dplyr::mutate(
      andel = .data$antall / sum(.data$antall),
      tooltip = paste0(
        .data$kategori,
        "<br>Antall: ", .data$antall,
        "<br>Andel: ", scales::percent(.data$andel, accuracy = 0.1)
      )
    ) |>
    dplyr::ungroup()

  # Preserve factor level order for groups when available, with total at the bottom
  if (is.null(gruppeVariabel)) {
    plotData <- plotData |>
      dplyr::mutate(
        gruppe = factor(.data$gruppe, levels = totalLabel)
      )
  } else {
    gruppeNivaaer <- if (is.factor(gruppeKilde)) {
      levels(gruppeKilde)
    } else {
      finnNivaaRekkefolge(plotData$gruppe[plotData$gruppe != totalLabel])
    }

    plotData <- plotData |>
      dplyr::mutate(
        gruppe = factor(
          .data$gruppe,
          levels = c(
            gruppeNivaaer[gruppeNivaaer %in% .data$gruppe],
            totalLabel
          )
        )
      )
  }

  plot <- ggplot2::ggplot(
    plotData,
    ggplot2::aes(
      x = .data$gruppe,
      y = .data$andel,
      fill = .data$kategori,
      text = .data$tooltip
    )
  ) +
    ggplot2::geom_col(
      width = 0.9,
      position = ggplot2::position_stack(reverse = TRUE)
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      y = yEtikett,
      fill = legendTitle
    ) +
    ggplot2::scale_fill_manual(
      values = fillColors,
      drop = FALSE
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = "black"),
      axis.text = ggplot2::element_text(size = 11),
      axis.title.x = ggplot2::element_text(size = 11),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(
        t = 15,
        r = 70,
        b = 15,
        l = 20
      )
    )

  plot
}
