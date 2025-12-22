#' Tabla Reactable Formateada UES
#'
#' Genera tablas avanzadas con `reactable` incluyendo columnas fijas, barras,
#' porcentajes, estilos personalizados y filas destacadas.
#'
#' @param df Data frame a mostrar.
#' @param fijas Columnas fijas a la izquierda.
#' @param grupos Lista nombrada con grupos de columnas.
#' @param titulos Vector con nombres personalizados para columnas.
#' @param filtrar Si TRUE activa el buscador.
#' @param barras Columnas donde aplicar barras.
#' @param color_barra Paleta de colores para las barras.
#' @param destacar_col Columnas a resaltar.
#' @param color_destacar Color de resaltado de columnas.
#' @param cols_porcentaje Columnas interpretadas como porcentaje.
#' @param destacar_row Filas a resaltar (por valor en la primera columna).
#' @param highlight_color Color de fondo para la fila destacada.
#'
#' @return Widget reactable.
#' @export

rt_tabla <- function (
  df,
  fijas = NULL,
  grupos = NULL,
  titulos = NULL,
  filtrar = TRUE,
  barras = NULL,
  color_barra = c("#cd0000", "#ffa500", "#00cd00", "#0000ee", "#551a8b"),
  destacar_col = NULL,
  color_destacar = "#e3e3e3",
  cols_porcentaje = NULL,
  destacar_row = NULL,
  highlight_color = "#f0e68c",
  decimales = 0,
  decimales_col = NULL
) {

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  titulos <- titulos %||% list()
  decimales_col <- decimales_col %||% list()

  destacar_col <- intersect(destacar_col %||% character(0), names(df))
  cols_porcentaje <- intersect(cols_porcentaje %||% character(0), names(df))
  barras <- intersect(barras %||% character(0), names(df))
  fijas <- intersect(fijas %||% character(0), names(df))

  get_decimales <- function(col) {
    if (!is.null(decimales_col[[col]])) decimales_col[[col]] else decimales
  }

  css_js <- htmltools::tagList(
    htmltools::tags$style(
      htmltools::HTML(sprintf("
/* headers */
.reactable .rt-th,
.reactable .rt-th-group {
  display: flex !important;
  align-items: center !important;
  justify-content: center !important;
}

/* columnas fijas */
.reactable .rt-th.col-fija {
  justify-content: flex-start !important;
}

/* celdas sin barras */
.reactable .rt-td-inner:not(:has(.barra-outer)) {
  display: flex;
  align-items: center;
  justify-content: center;
  height: 100%%;
}

/* celdas fijas */
.reactable .rt-td.col-fija .rt-td-inner {
  justify-content: flex-start !important;
}

.reactable .rt-tr:hover .rt-td:not(.col-fija),
.rt-td.column-hover:not(.col-fija) {
  background-color: %s !important;
}

.rt-td.col-fija {
  background-color: #191970 !important;
  color: white !important;
}

.barra-outer {
  border: 1px solid #d0d0d0 !important;
  border-radius: 4px !important;
}
", highlight_color))
    )
  )

  clean_numeric <- function(x) {
    if (is.numeric(x)) return(x)
    x <- gsub("(?<=\\d)\\.(?=\\d{3})", "", x, perl = TRUE)
    as.numeric(gsub(",", ".", x))
  }

  columnas <- lapply(names(df), function(col) {

    class_col <- paste0("col-", gsub("\\s+", "_", col))
    estilo_base <- list(fontFamily = "Arial", fontSize = "14px")

    if (col %in% fijas) {
      return(
        reactable::colDef(
          name = titulos[[col]] %||% col,
          sticky = "left",
          class = paste(class_col, "col-fija"),
          align = "left",
          style = list(background = "#191970", color = "white", fontWeight = "bold")
        )
      )
    }

    if (col %in% barras) {

      valores <- clean_numeric(df[[col]])
      es_pct <- col %in% cols_porcentaje
      digs <- get_decimales(col)
      pal <- color_barra

      return(
        reactable::colDef(
          name = titulos[[col]] %||% col,
          html = TRUE,
          cell = function(value, index) {

            val <- clean_numeric(value)
            prop <- if (es_pct) min(val, 1) else scales::rescale(val, to = c(0, 1), from = range(valores, na.rm = TRUE))

            label <- if (es_pct)
              paste0(formatC(val * 100, digits = digs, format = "f", decimal.mark = ","), "%")
            else
              formatC(val, digits = digs, format = "f", big.mark = ".", decimal.mark = ",")

            htmltools::HTML(sprintf("
              <div style='display:flex;align-items:center;gap:6px;'>
                <div class='barra-label' style='min-width:50px;text-align:right;'>%s</div>
                <div class='barra-outer' style='flex-grow:1;height:14px;background:#f0f0f0;'>
                  <div style='height:100%%;width:%s%%;background:%s;'></div>
                </div>
              </div>", label, prop * 100, pal[3]))
          }
        )
      )
    }

    if (is.numeric(df[[col]])) {

      es_pct <- col %in% cols_porcentaje
      digs <- get_decimales(col)

      return(
        reactable::colDef(
          name = titulos[[col]] %||% col,
          format =
            if (es_pct)
              reactable::colFormat(percent = TRUE, digits = digs, locale = "es")
            else
              reactable::colFormat(separators = TRUE, digits = digs, locale = "es"),
          style = estilo_base
        )
      )
    }

    reactable::colDef(name = titulos[[col]] %||% col, style = estilo_base)
  })

  names(columnas) <- names(df)

  tbl <- reactable::reactable(
    df,
    columns = columnas,
    searchable = filtrar,
    highlight = TRUE,
    bordered = TRUE,
    pagination = FALSE
  )

  htmltools::browsable(htmltools::tagList(css_js, tbl))
}
