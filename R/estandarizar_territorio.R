#' Estandarizar comuna y agregar jerarquía territorial
#'
#' Estandariza el código y nombre de la comuna a partir de una variable de
#' comuna existente en el data frame, y permite agregar información de
#' provincia y región según corresponda.
#'
#' La función renombra internamente la variable de comuna a
#' \code{codigo_comuna}, actualiza los nombres de comuna usando
#' \code{cut_actual}, y cruza la jerarquía territorial desde
#' \code{poblacion_ine}.
#'
#' @param df Un data frame o tibble.
#' @param comuna Variable que identifica la comuna (generalmente el código de comuna).
#'   Se pasa sin comillas.
#' @param agregar Caracter vector indicando qué niveles territoriales agregar.
#'   Puede incluir uno o más de: \code{"comuna"}, \code{"provincia"},
#'   \code{"region"}. Por defecto agrega todos.
#'
#' @return Un data frame con las columnas territoriales estandarizadas.
#'
#' @details
#' La función elimina cualquier columna previa llamada
#' \code{nombre_comuna}, \code{nombre_provincia} o \code{nombre_region}
#' antes de realizar los cruces, para evitar duplicaciones.
#'
#' @seealso \code{\link{poblacion_ine}}, \code{\link{cut_actual}}
#'
#' @examples
#' \dontrun{
#' nac %>%
#'   estandarizar_territorio(codigo_comuna)
#'
#' nac %>%
#'   estandarizar_territorio(codigo_comuna, agregar = c("comuna", "region"))
#' }
#'
#' @export
estandarizar_territorio <- function(
  df,
  comuna,
  agregar = c("comuna", "provincia", "region")
) {

  agregar <- match.arg(
    agregar,
    choices = c("comuna", "provincia", "region"),
    several.ok = TRUE
  )

  comuna_sym  <- rlang::ensym(comuna)
  comuna_name <- rlang::as_string(comuna_sym)

  # --- renombrar variable de comuna si es necesario
  if (comuna_name != "codigo_comuna") {
    df <- df %>%
      dplyr::rename(codigo_comuna = !!comuna_sym)
  }

  # --- eliminar posibles columnas previas
  df <- df %>%
    dplyr::select(
      -dplyr::any_of(
        c("nombre_comuna", "nombre_provincia", "nombre_region")
      )
    )

  # --- tablas base
  cut <- ues::poblacion_ine %>%
    dplyr::distinct(codigo_comuna, nombre_comuna)

  cod_cpr <- ues::poblacion_ine %>%
    dplyr::distinct(
      codigo_comuna,
      nombre_comuna,
      nombre_provincia,
      nombre_region
    )

  # --- estandarizar nombre de comuna
  df <- df %>%
    dplyr::left_join(ues::cut, by = "codigo_comuna") %>%
    dplyr::left_join(
      ues::cut_actual,
      by = "nombre_comuna",
      suffix = c("_old", "")
    ) %>%
    dplyr::mutate(
      nombre_comuna = dplyr::coalesce(nombre_comuna, nombre_comuna_old)
    ) %>%
    dplyr::select(-nombre_comuna_old)

  # --- agregar jerarquía territorial
  df <- df %>%
    dplyr::left_join(
      cod_cpr,
      by = "codigo_comuna"
    )

  # --- filtrar columnas según 'agregar'
  columnas <- c(
    comuna    = "nombre_comuna",
    provincia = "nombre_provincia",
    region    = "nombre_region"
  )

  df %>%
    dplyr::select(
      -dplyr::any_of(
        setdiff(columnas, columnas[agregar])
      )
    )
}
