#' @importFrom rlang .data
NULL

#' Horisontalt andelsplott for true/false-variabler
#'
#' Lager et horisontalt stolpediagram som viser andel TRUE per gruppe,
#' beregnet som TRUE / (TRUE + FALSE). Manglende verdier i andelsvariabel
#' fjernes før beregning.
#'
#' @param data Data frame med inputdata.
#' @param andelVariabel Navn på variabel med true/false-verdier.
#' @param gruppeVariabel Valgfritt navn på grupperingsvariabel. Hvis `NULL`
#'   vises kun total-rad.
#' @param terskel Nedre grense for gyldig antall per gruppe. Grupper under
#'   terskel vises uten stolpe. Standard: `5`.
#' @param totalLabel Navn på total-rad. Standard: `"Totalt"`.
#' @param trueValues Valgfri vektor med ekstra verdier som skal tolkes som
#'   TRUE.
#' @param falseValues Valgfri vektor med ekstra verdier som skal tolkes som
#'   FALSE.
#' @param tittel Valgfri tittel for figuren. Standard: `NULL`.
#' @param breaks Valgfri vektor med to prosentgrenser for kvalitetsbånd i
#'   bakgrunnen, for eksempel `c(75, 90)`. Hvis `NULL` tegnes ingen bånd.
#' @param highGood Logisk. `TRUE` betyr at høy andel er bra. `FALSE` betyr at
#'   lav andel er bra. Standard: `TRUE`.
#'
#' @return Et `ggplot2`-objekt.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   gruppe = c("A", "A", "B", "B", "C", "C", "C", "D", "D"),
#'   verdi = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, NA, TRUE)
#' )
#'
#' FigAndelPlot(
#'   data = df,
#'   andelVariabel = "verdi",
#'   gruppeVariabel = "gruppe"
#' )
#'
#' FigAndelPlot(
#'   data = df,
#'   andelVariabel = "verdi",
#'   gruppeVariabel = "gruppe",
#'   tittel = "Andel TRUE per gruppe",
#'   breaks = c(75, 90),
#'   highGood = TRUE
#' )
FigAndelPlot <- function(
  data,
  andelVariabel,
  gruppeVariabel = NULL,
  terskel = 5,
  totalLabel = "Totalt",
  trueValues = NULL,
  falseValues = NULL,
  tittel = NULL,
  breaks = NULL,
  highGood = TRUE
) {
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }

  if (!is.character(andelVariabel) || length(andelVariabel) != 1 ||
        !andelVariabel %in% names(data)) {
    stop("andelVariabel must be the name of a column in data")
  }

  if (!is.null(gruppeVariabel) && (!is.character(gruppeVariabel) ||
                                     length(gruppeVariabel) != 1 ||
                                     !gruppeVariabel %in% names(data))) {
    stop("gruppeVariabel must be NULL or the name of a column in data")
  }

  if (!is.numeric(terskel) || length(terskel) != 1 || is.na(terskel) || terskel < 0) {
    stop("terskel must be a single non-negative number")
  }

  if (!is.null(breaks)) {
    if (!is.numeric(breaks) || length(breaks) != 2 || any(is.na(breaks))) {
      stop("breaks must be NULL or exactly two numeric values")
    }
    if (breaks[1] >= breaks[2]) {
      stop("breaks must be strictly increasing")
    }
    if (breaks[1] < 0 || breaks[2] > 100) {
      stop("breaks must be within 0 to 100")
    }
  }

  normalizeValue <- function(x) {
    tolower(trimws(as.character(x)))
  }

  builtInTrue <- c("true", "t", "1", "ja", "yes", "y", "sant")
  builtInFalse <- c("false", "f", "0", "nei", "no", "n", "usant")

  # Bygg opp tillatte verdier for TRUE/FALSE (innebygde + evt. brukerdefinerte)
  trueSet <- unique(c(builtInTrue, normalizeValue(trueValues)))
  falseSet <- unique(c(builtInFalse, normalizeValue(falseValues)))

  indicatorValuesRaw <- data[[andelVariabel]]
  hasIndicatorValue <- !is.na(indicatorValuesRaw)

  # Fjern rader uten verdi i andelsvariabelen
  filteredData <- data[hasIndicatorValue, , drop = FALSE]
  indicatorValues <- indicatorValuesRaw[hasIndicatorValue]
  normalizedIndicatorValues <- normalizeValue(indicatorValues)

  # Map tekstverdier til TRUE/FALSE-etikett
  # Start med NA, og sett verdi til TRUE/FALSE der teksten matcher definerte sett
  booleanLabel <- rep(NA_character_, length(normalizedIndicatorValues))
  booleanLabel[normalizedIndicatorValues %in% trueSet] <- "TRUE"
  booleanLabel[normalizedIndicatorValues %in% falseSet] <- "FALSE"

  # Behold bare rader som faktisk ble mappet til TRUE/FALSE
  isMapped <- !is.na(booleanLabel)
  filteredData <- filteredData[isMapped, , drop = FALSE]
  booleanLabel <- booleanLabel[isMapped]

  if (is.null(gruppeVariabel)) {
    # Uten grupperingsvariabel legges alle i total-gruppen
    groupValues <- rep(totalLabel, length(booleanLabel))
    groupOrder <- totalLabel
  } else {
    # Med grupperingsvariabel fjernes rader uten gruppeverdi
    groupValuesRaw <- filteredData[[gruppeVariabel]]
    hasGroupValue <- !is.na(groupValuesRaw)
    filteredData <- filteredData[hasGroupValue, , drop = FALSE]
    booleanLabel <- booleanLabel[hasGroupValue]
    # Bevar opprinnelig rekkefølge på grupper
    groupValues <- as.character(groupValuesRaw[hasGroupValue])
    groupOrder <- unique(groupValues)
  }

  if (length(booleanLabel) == 0) {
    plotData <- data.frame(
      group = totalLabel,
      n = 0,
      nTrue = 0,
      pctTrue = 0,
      isTotal = TRUE,
      eligible = TRUE,
      stringsAsFactors = FALSE
    )
  } else {
    nByGroup <- tapply(booleanLabel, groupValues, length)
    nTrueByGroup <- tapply(booleanLabel == "TRUE", groupValues, sum)

    groupLevels <- names(nByGroup)
    if (!is.null(gruppeVariabel)) {
      groupLevels <- groupOrder[groupOrder %in% groupLevels]
    }

    groupData <- data.frame(
      group = groupLevels,
      n = as.integer(nByGroup[groupLevels]),
      nTrue = as.integer(nTrueByGroup[groupLevels]),
      stringsAsFactors = FALSE
    )

    totalN <- length(booleanLabel)
    totalNTrue <- sum(booleanLabel == "TRUE")
    totalRow <- data.frame(
      group = totalLabel,
      n = as.integer(totalN),
      nTrue = as.integer(totalNTrue),
      stringsAsFactors = FALSE
    )

    if (is.null(gruppeVariabel)) {
      plotData <- totalRow
    } else {
      groupData <- groupData[groupData$group != totalLabel, , drop = FALSE]
      plotData <- rbind(totalRow, groupData)
    }

    plotData$pctTrue <- ifelse(plotData$n > 0, 100 * plotData$nTrue / plotData$n, 0)
    plotData$isTotal <- plotData$group == totalLabel
    plotData$eligible <- plotData$isTotal | (plotData$n >= terskel & plotData$n > 0)
  }

  # Formater prosent med norsk desimalskilletegn
  formatPctNb <- function(x) {
    paste0(gsub("\\.", ",", sprintf("%.1f", x)), " %")
  }

  plotData$groupLabel <- paste0(plotData$group, " (", plotData$n, ")")
  plotData$barValue <- ifelse(plotData$eligible, plotData$pctTrue, NA_real_)
  plotData$pctLabel <- ifelse(plotData$eligible, formatPctNb(plotData$pctTrue), NA_character_)

  if (nrow(plotData) > 1) {
    labelOnly <- plotData[!plotData$isTotal & !plotData$eligible, , drop = FALSE]
    bars <- plotData[!plotData$isTotal & plotData$eligible, , drop = FALSE]
    if (nrow(bars) > 0) {
      bars <- bars[order(bars$pctTrue, seq_len(nrow(bars))), , drop = FALSE]
    }
    total <- plotData[plotData$isTotal, , drop = FALSE]
    plotData <- rbind(total, labelOnly, bars)
  }

  plotData$groupLabel <- factor(plotData$groupLabel, levels = rev(plotData$groupLabel))

  nTicks <- 5
  gjennomsnittY <- if (any(plotData$isTotal)) {
    plotData$pctTrue[plotData$isTotal][1]
  } else {
    mean(plotData$pctTrue, na.rm = TRUE)
  }
  maksAndel <- min(max(plotData$pctTrue, na.rm = TRUE) * 1.15, 100)
  prettyVals <- pretty(c(0, maksAndel), n = nTicks)
  ovreGrense <- max(prettyVals, gjennomsnittY, na.rm = TRUE)

  plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = .data$groupLabel, y = .data$barValue))

  if (!is.null(breaks)) {
    b1 <- breaks[1]
    b2 <- breaks[2]

    if (isTRUE(highGood)) {
      bands <- data.frame(
        ymin = c(0, b1, b2),
        ymax = c(b1, b2, 100),
        quality = factor(c("Lav", "Middels", "Høy"), levels = c("Lav", "Middels", "Høy"))
      )
    } else {
      bands <- data.frame(
        ymin = c(0, b1, b2),
        ymax = c(b1, b2, 100),
        quality = factor(c("Høy", "Middels", "Lav"), levels = c("Lav", "Middels", "Høy"))
      )
    }

    plot <- plot +
      ggplot2::geom_rect(
        data = bands,
        ggplot2::aes(
          xmin = -Inf,
          xmax = Inf,
          ymin = .data$ymin,
          ymax = .data$ymax,
          fill = .data$quality
        ),
        inherit.aes = FALSE,
        alpha = 0.20
      ) +
      ggplot2::scale_fill_manual(
        name = "Kvalitet",
        values = c("Lav" = "#d73027", "Middels" = "#fee08b", "Høy" = "#1a9850")
      )
  }

  plot <- plot +
    ggplot2::geom_col(width = 0.7, fill = "#4292c6", na.rm = TRUE) +
    ggplot2::geom_text(
      data = plotData[plotData$eligible, , drop = FALSE],
      ggplot2::aes(label = .data$pctLabel),
      hjust = -0.2,
      color = "#4292c6",
      size = 4.5,
      na.rm = TRUE
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(
      limits = c(0, ovreGrense),
      breaks = prettyVals,
      oob = scales::squish,
      expand = ggplot2::expansion(mult = c(0, 0.08)),
      labels = function(x) paste0(x, " %")
    ) +
    ggplot2::labs(x = NULL, y = "Andel (%)", title = tittel) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(r = 30),
      axis.ticks.x = ggplot2::element_line(color = "black"),
      axis.line.x = ggplot2::element_line(color = "black"),
      axis.line.y = ggplot2::element_line(color = "black"),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 16),
      plot.title = ggplot2::element_text(size = 18, face = "bold"),
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 13),
      legend.position = "top"
    )

  plot
}
