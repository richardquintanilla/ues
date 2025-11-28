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
rt_tabla <- function(
    df, 
    fijas = NULL, 
    grupos = NULL, 
    titulos = NULL, 
    filtrar = TRUE, 
    barras = NULL,
    color_barra = c('red3','yellow2','green3','blue2','purple4'),
    destacar_col = NULL, 
    color_destacar = "gray", 
    cols_porcentaje = NULL,
    destacar_row = NULL, 
    highlight_color = "khaki"
) {
  
  `%||%` <- function(a,b) if(!is.null(a)) a else b
  
  destacar_col <- intersect(destacar_col %||% character(0), names(df))
  cols_porcentaje <- intersect(cols_porcentaje %||% character(0), names(df))
  barras <- intersect(barras %||% character(0), names(df))
  
  to_rgba <- function(col, alpha = 0.35) {
    rgb <- tryCatch(grDevices::col2rgb(col), error = function(e) NULL)
    if (is.null(rgb)) rgb <- grDevices::col2rgb("gray")
    sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
  }
  
  col_rgba <- to_rgba(color_destacar, 0.35)
  columnas <- list()
  
  for (col in names(df)) {
    
    es_fija   <- col %in% fijas
    es_destac <- col %in% destacar_col
    es_pct    <- col %in% cols_porcentaje
    es_barra  <- col %in% barras
    
    class_col <- paste0("col-", gsub("\\s+", "_", col))
    
    estilo_base <- if (es_destac) {
      list(
        boxShadow = sprintf("inset 0 0 0 9999px %s", col_rgba),
        fontWeight = "bold"
      )
    } else list()
    
    # --- Columna fija ---
    if (es_fija) {
      columnas[[col]] <- reactable::colDef(
        name = titulos[[col]] %||% col,
        sticky = "left",
        align = "left",
        class = paste(class_col, "col-fija"),
        headerStyle = list(background="midnightblue", color="white", fontWeight="bold"),
        style = list(background="midnightblue", color="white", fontWeight="bold")
      )
      next
    }
    
    # --- Columna con barras ---
    if (es_barra) {
      
      columnas[[col]] <- reactable::colDef(
        name = titulos[[col]] %||% col,
        align = "center",
        class = class_col,
        style = estilo_base,
        cell = reactablefmtr::data_bars(
          data = df,
          columns = col,
          text_position = "above",
          fill_color = reactablefmtr::color_scales(df[[col]], colors = color_barra),
          number_fmt = if (es_pct)
            scales::percent_format(accuracy = 0.1, decimal.mark = ",")
          else
            scales::comma_format(big.mark=".", decimal.mark=","),
          min_value = 0
        )
      )
      next
    }
    
    # --- Columna numérica ---
    if (is.numeric(df[[col]])) {
      if (es_pct) {
        columnas[[col]] <- reactable::colDef(
          name = titulos[[col]] %||% col,
          align = "center",
          class = class_col,
          style = estilo_base,
          cell = reactable::JS("function(v){ return (v*100).toFixed(1).replace('.',',') + '%'; }")
        )
      } else {
        columnas[[col]] <- reactable::colDef(
          name = titulos[[col]] %||% col,
          align = "center",
          class = class_col,
          style = estilo_base,
          format = reactable::colFormat(separators=TRUE, digits=0, locale="es")
        )
      }
      next
    }
    
    # --- Texto ---
    columnas[[col]] <- reactable::colDef(
      name = titulos[[col]] %||% col,
      align = "center",
      class = class_col,
      style = estilo_base
    )
  }
  
  row_style <- function(index) {
    if (!is.null(destacar_row)) {
      val <- df[[1]][index]
      if (val %in% destacar_row) {
        return(list(background="khaki", fontWeight="bold"))
      }
    }
    list()
  }
  
  # Grupos de columnas
  col_groups <- NULL
  if (!is.null(grupos)) {
    col_groups <- lapply(names(grupos), function(g) {
      reactable::colGroup(
        name = g,
        columns = grupos[[g]],
        headerStyle = list(background="midnightblue", color="white", fontWeight="bold")
      )
    })
  }
  
  reactable::reactable(
    df,
    columns = columnas,
    columnGroups = col_groups,
    compact = TRUE,
    bordered = TRUE,
    striped = TRUE,
    pagination = FALSE,
    highlight = TRUE,
    fullWidth = TRUE,
    wrap = TRUE,
    searchable = filtrar,
    rowStyle = row_style,
    defaultColDef = reactable::colDef(
      align="center",
      vAlign="center",
      headerVAlign="center",
      headerStyle=list(background="midnightblue", color="white", fontWeight="bold")
    )
  )
}
