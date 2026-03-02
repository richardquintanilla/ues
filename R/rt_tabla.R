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
#' @param decimales Cantidad de decimales por defecto.
#' @param decimales_col Vector nombrado con decimales por columna.
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

  # --- SOPORTE PARA crosstalk::SharedData ---
  if (inherits(df, "SharedData")) {
    df_data <- df$data()
  } else {
    df_data <- df
  }

  titulos <- titulos %||% list()
  decimales_col <- decimales_col %||% list()

  get_decimales <- function(col) {
    if (!is.null(decimales_col[[col]])) decimales_col[[col]] else decimales
  }

  destacar_col   <- intersect(destacar_col %||% character(0), names(df_data))
  cols_porcentaje <- intersect(cols_porcentaje %||% character(0), names(df_data))
  barras         <- intersect(barras %||% character(0), names(df_data))
  fijas          <- intersect(fijas %||% character(0), names(df_data))

  font_family_base <- "sans-serif"
  font_size_base   <- "13px"

  clean_numeric <- function(x) {
    if (is.numeric(x)) return(as.numeric(x))
    xch <- gsub("(?<=\\d)\\.(?=\\d{3}(?:\\D|$))", "", trimws(as.character(x)), perl = TRUE)
    xch <- gsub(",", ".", xch, fixed = TRUE)
    suppressWarnings(as.numeric(xch))
  }

  columnas <- lapply(names(df_data), function(colname) {
    local({
      col <- colname
      class_col <- paste0("col-", gsub("\\s+", "_", col))

      estilo_base <- list(
        fontFamily = font_family_base,
        fontSize = font_size_base,
        fontWeight = "normal",
        textAlign = "center"
      )

      if (col %in% fijas) {
        return(
          reactable::colDef(
            name = titulos[[col]] %||% col,
            sticky = "left",
            align = "left",
            class = paste(class_col, "col-fija"),
            headerStyle = list(
              background = "#191970",
              color = "white",
              fontWeight = "bold",
              fontFamily = font_family_base,
              textAlign = "center"
            ),
            style = list(
              background = "#191970",
              color = "white",
              fontFamily = font_family_base,
              fontSize = font_size_base,
              fontWeight = "bold",
              borderRight = "2px solid white"
            )
          )
        )
      }

      if (col %in% barras) {

        valores_limpios <- clean_numeric(df_data[[col]])
        pal <- if (length(color_barra) == 5) color_barra else rep("#ccc", 5)
        es_pct <- col %in% cols_porcentaje
        digs <- get_decimales(col)
        is_dest_col <- col %in% destacar_col

        return(
          reactable::colDef(
            name = titulos[[col]] %||% col,
            class = class_col,
            align = "center",
            html = TRUE,
            sortable = TRUE,
            style = if (is_dest_col)
              list(
                background = color_destacar,
                fontWeight = "normal",
                fontFamily = font_family_base,
                fontSize = font_size_base
              )
            else estilo_base,

            cell = function(value, index) {

              val_num <- clean_numeric(df_data[[col]][index])

              if (!is.finite(val_num)) {
                displayed <- ""
                prop <- 0
                color_fill <- pal[1]
              } else {

                displayed <- if (es_pct) {
                  paste0(
                    formatC(val_num * 100, format = "f", digits = digs, decimal.mark = ","),
                    "%"
                  )
                } else {
                  formatC(
                    val_num,
                    format = "f",
                    digits = digs,
                    big.mark = ".",
                    decimal.mark = ","
                  )
                }

                min_col <- min(valores_limpios, na.rm = TRUE)
                max_col <- max(valores_limpios, na.rm = TRUE)

                if (es_pct) {
                  prop <- min(val_num, 1)
                  qs <- seq(0, 1, length.out = 6)
                } else if (max_col - min_col == 0) {
                  prop <- 1
                  qs <- seq(0, 1, length.out = 6)
                } else {
                  prop <- (val_num - min_col) / (max_col - min_col)
                  qs <- quantile(valores_limpios, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
                }

                grp <- findInterval(val_num, qs, all.inside = TRUE)
                color_fill <- pal[grp]
              }

              htmltools::HTML(sprintf("
                <div style='display:flex;align-items:center;gap:6px;'>
                  <div class='barra-label' style='min-width:45px;text-align:right;font-family:sans-serif;font-size:13px;'>%s</div>
                  <div class='barra-outer' style='flex-grow:1;height:14px;background:#f0f0f0;overflow:hidden;'>
                    <div style='height:100%%;width:%s%%;background:%s;'></div>
                  </div>
                </div>
              ", displayed, prop * 100, color_fill))
            }
          )
        )
      }

      if (col %in% destacar_col) {
        return(
          reactable::colDef(
            name = titulos[[col]] %||% col,
            class = class_col,
            align = "center",
            style = list(
              background = color_destacar,
              fontWeight = "normal",
              fontFamily = font_family_base,
              fontSize = font_size_base
            ),
            format = if (col %in% cols_porcentaje)
              reactable::colFormat(percent = TRUE, digits = get_decimales(col), locale = "es")
            else
              reactable::colFormat(separators = TRUE, digits = get_decimales(col), locale = "es")
          )
        )
      }

      if (is.numeric(df_data[[col]])) {

        es_pct <- col %in% cols_porcentaje
        digs <- get_decimales(col)

        return(
          reactable::colDef(
            name = titulos[[col]] %||% col,
            class = class_col,
            align = "center",
            style = estilo_base,
            format = if (es_pct)
              reactable::colFormat(percent = TRUE, digits = digs, locale = "es")
            else
              reactable::colFormat(separators = TRUE, digits = digs, locale = "es")
          )
        )
      }

      reactable::colDef(
        name = titulos[[col]] %||% col,
        class = class_col,
        align = "center",
        style = estilo_base
      )
    })
  })

  names(columnas) <- names(df_data)

  fila_style_fun <- function(i) {
    if (!is.null(destacar_row) && df_data[[1]][i] %in% destacar_row) {
      return(list(background = color_destacar, fontWeight = "bold"))
    }
    list()
  }

  tbl <- reactable::reactable(
    df,   # 👈 MANTENEMOS df para que SharedData siga funcionando
    columns = columnas,
    rowStyle = fila_style_fun,
    highlight = TRUE,
    searchable = filtrar,
    striped = TRUE,
    bordered = TRUE,
    pagination = FALSE,
    language = reactable::reactableLang(searchPlaceholder = "Filtrar"),
    defaultColDef = reactable::colDef(
      align = "center",
      html = TRUE,
      headerStyle = list(
        background = "#191970",
        color = "white",
        fontWeight = "bold",
        fontFamily = font_family_base,
        textAlign = "center"
      ),
      style = list(fontFamily = font_family_base, fontSize = font_size_base)
    )
  )

  htmltools::browsable(tbl)
}
