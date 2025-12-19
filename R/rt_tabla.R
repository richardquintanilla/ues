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

rt_tabla <- function (df, fijas = NULL, grupos = NULL, titulos = NULL, filtrar = TRUE, 
          barras = NULL, color_barra = c("#cd0000", "#ffa500", "#00cd00", 
                                         "#0000ee", "#551a8b"),
          destacar_col = NULL, color_destacar = "#e3e3e3", 
          cols_porcentaje = NULL, destacar_row = NULL, 
          highlight_color = "#f0e68c",
          decimales = 0) 
{
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  titulos <- titulos %||% list()
  
  destacar_col <- intersect(destacar_col %||% character(0), names(df))
  cols_porcentaje <- intersect(cols_porcentaje %||% character(0), names(df))
  barras <- intersect(barras %||% character(0), names(df))
  fijas <- intersect(fijas %||% character(0), names(df))
  
  css_js <- htmltools::tagList(
    htmltools::tags$style(
      htmltools::HTML(
        sprintf("
      .reactable .rt-tr:hover .rt-td:not(.col-fija) {
        background-color: %s !important;
      }

      .rt-td.column-hover:not(.col-fija) {
        background-color: %s !important;
      }

      .rt-td.col-fija {
        background-color: #191970 !important;
        color: white !important;
      }

      .rt-tr:hover .rt-td.col-fija {
        background-color: #191970 !important;
      }

      .rt-td, .rt-td .rt-td-inner, .barra-outer, .barra-label {
        transition: font-size 0.14s ease, transform 0.12s ease;
      }

      .rt-td {
        vertical-align: middle !important;
      }

      .rt-td-inner {
        display: flex;
        align-items: center;
        justify-content: center;
        height: 100%%;
      }

      .rt-td.col-fija .rt-td-inner {
        justify-content: flex-start !important;
      }

      .rt-td.cell-hover:not(.col-fija) {
        background-color: khaki !important;
        z-index: 999 !important;
        box-shadow: 0 0 0 2px midnightblue !important;
        font-weight: bold !important;
      }

      .rt-td.cell-hover:not(.col-fija) .rt-td-inner,
      .rt-td.cell-hover:not(.col-fija) .barra-label {
        font-size: 16px !important;
        font-weight: bold !important;
      }

      .reactable .rt-thead-group,
      .reactable .rt-th-group {
        background-color: #191970 !important;
        color: white !important;
        font-weight: bold !important;
        text-align: center !important;
        font-family: Arial !important;
      }

      .barra-outer {
        border: 1px solid #d0d0d0 !important;
        border-radius: 4px !important;
      }
    ", highlight_color, highlight_color)
      )
    )
  )
  
  clean_numeric <- function(x) {
    if (is.numeric(x)) return(as.numeric(x))
    xch <- gsub("(?<=\\d)\\.(?=\\d{3}(?:\\D|$))", "", trimws(as.character(x)), perl = TRUE)
    xch <- gsub(",", ".", xch, fixed = TRUE)
    suppressWarnings(as.numeric(xch))
  }
  
  columnas <- lapply(names(df), function(colname) {
    local({
      col <- colname
      class_col <- paste0("col-", gsub("\\s+", "_", col))
      
      if (col %in% fijas) {
        return(
          reactable::colDef(
            name = titulos[[col]] %||% col,
            sticky = "left",
            align = "left",
            class = paste(class_col, "col-fija")
          )
        )
      }
      
      if (col %in% barras) {
        valores_limpios <- clean_numeric(df[[col]])
        
        escala <- if (decimales > 0) 100 else 1
        valores_barra <- valores_limpios * escala
        
        pal <- if (length(color_barra) == 5) color_barra else rep("#ccc", 5)
        es_pct <- col %in% cols_porcentaje
        
        return(
          reactable::colDef(
            name = titulos[[col]] %||% col,
            class = class_col,
            html = TRUE,
            align = "center",
            cell = function(value, index) {
              
              val_raw <- clean_numeric(df[[col]][index])
              val_barra <- val_raw * escala
              
              if (!is.finite(val_barra)) {
                displayed <- ""
                prop <- 0
                grp <- 1
              } else {
                displayed <- if (es_pct) {
                  paste0(formatC(val_raw * 100, format = "f",
                                 digits = decimales, decimal.mark = ","), "%")
                } else {
                  formatC(val_raw, format = "f",
                          digits = decimales,
                          big.mark = ".", decimal.mark = ",")
                }
                
                min_col <- min(valores_barra, na.rm = TRUE)
                max_col <- max(valores_barra, na.rm = TRUE)
                
                if (max_col - min_col == 0) {
                  prop <- 1
                } else {
                  prop <- (val_barra - min_col) / (max_col - min_col)
                  prop <- max(min(prop, 1), 0)
                }
                
                qs <- quantile(valores_barra, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
                grp <- findInterval(val_barra, qs, all.inside = TRUE)
              }
              
              htmltools::HTML(sprintf("
              <div style='display:flex;align-items:center;gap:6px;'>
                <div class='barra-label' style='min-width:45px;text-align:right;'>%s</div>
                <div class='barra-outer' style='flex-grow:1;height:14px;background:#f0f0f0;'>
                  <div style='height:100%%;width:%s%%;background:%s;'></div>
                </div>
              </div>
              ", displayed, prop * 100, pal[grp]))
            }
          )
        )
      }
      
      if (is.numeric(df[[col]])) {
        return(
          reactable::colDef(
            name = titulos[[col]] %||% col,
            align = "center",
            format = reactable::colFormat(
              separators = TRUE,
              digits = decimales,
              locale = "es"
            )
          )
        )
      }
      
      reactable::colDef(
        name = titulos[[col]] %||% col,
        align = "center"
      )
    })
  })
  
  names(columnas) <- names(df)
  
  tbl <- reactable::reactable(
    df,
    columns = columnas,
    searchable = filtrar,
    striped = TRUE,
    bordered = TRUE,
    pagination = FALSE
  )
  
  htmltools::browsable(htmltools::tagList(css_js, tbl))
}
