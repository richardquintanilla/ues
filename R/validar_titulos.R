#' Validar y opcionalmente ordenar columnas
#'
#' Esta función verifica si un data frame contiene los nombres de columnas
#' indicados en `titulos`. Si `ordenar = TRUE`, reordena el data frame en el
#' mismo orden solicitado.
#'
#' @param df Data frame a validar.
#' @param titulos Vector con nombres de columnas requeridas.
#' @param ordenar Logical. Si TRUE, reordena el data frame según `titulos`.
#'
#' @return El data frame original o reordenado.
#'
#' @export
validar_titulos <- function(df, titulos, ordenar = FALSE) {

  faltan <- setdiff(titulos, names(df))

  if (length(faltan) == 0) {
    message("Validación exitosa: todos los títulos requeridos están presentes.")

    if (ordenar) {
      df <- df[, titulos, drop = FALSE]
      message("Columnas reordenadas según 'titulos'.")
    }

  } else {
    message("Problemas en los nombres de columnas:")
    message("   Faltan: ", paste(faltan, collapse = ", "))
    stop("Deteniendo ejecución por validación fallida.")
  }

  return(df)
}
