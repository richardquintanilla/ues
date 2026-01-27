#' Estandarizar comuna y agregar jerarquía territorial
#'
#' A partir de un código de comuna, estandariza nombres oficiales de comuna,
#' provincia y región. Si el código de comuna corresponde a una versión
#' antigua, se actualiza al código vigente junto con su nombre oficial.
#'
#' La variable original del código de comuna **no se modifica**. La función
#' crea una columna \code{codigo_comuna} estandarizada y agrega únicamente
#' las columnas territoriales solicitadas.
#'
#' @param df Un data frame o tibble.
#' @param codigo_comuna Variable que identifica el código de comuna.
#'   Se pasa sin comillas y **no se modifica**.
#' @param agregar Vector de caracteres indicando qué niveles territoriales
#'   agregar. Puede incluir uno o más de: \code{"comuna"},
#'   \code{"provincia"}, \code{"region"}. Por defecto agrega todos.
#'
#' @return Un data frame con columnas territoriales estandarizadas según
#'   lo indicado en \code{agregar}. Las variables originales se conservan.
#'
#' @details
#' El flujo de la función es:
#' \enumerate{
#'   \item Crear un \code{codigo_comuna} desde la variable original.
#'   \item Estandarizar el código y nombre de la comuna usando
#'         \code{ues::cut_actual}.
#'   \item Agregar provincia y región desde fuentes oficiales.
#' }
#'
#' No se generan columnas auxiliares ni temporales en el resultado final.
#'
#' @seealso
#' \code{\link{cut}},
#' \code{\link{cut_actual}},
#' \code{\link{poblacion_ine}}
#'
#' @importFrom rlang ensym
#' @importFrom dplyr mutate left_join distinct select any_of if_else
#' @export
estandarizar_territorio <- function(
  df,
  codigo_comuna,
  agregar = c("comuna", "provincia", "region")
) {

  agregar <- match.arg(
    agregar,
    choices = c("comuna", "provincia", "region"),
    several.ok = TRUE
  )

  comuna_sym <- rlang::ensym(codigo_comuna)

  # --- 1. crear codigo_comuna desde variable original
  df <- df %>%
    dplyr::mutate(
      codigo_comuna = !!comuna_sym
    )

  # --- 2. estandarizar codigo y nombre de comuna (vigente)
  df <- df %>%
    dplyr::left_join(
      ues::cut_actual %>%
        dplyr::select(codigo_comuna, nombre_comuna),
      by = "codigo_comuna"
    )

  # --- 3. agregar provincia y región
  df <- df %>%
    dplyr::left_join(
      ues::poblacion_ine %>%
        dplyr::distinct(
          codigo_comuna,
          nombre_comuna,
          nombre_provincia,
          nombre_region
        ),
      by = c("codigo_comuna", "nombre_comuna")
    )

  # --- 4. regla explícita: comuna ignorada
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        c(nombre_comuna, nombre_provincia, nombre_region),
        ~ dplyr::if_else(
          codigo_comuna == "99999",
          "Ignorada",
          .
        )
      )
    )

  # --- 5. devolver solo columnas solicitadas
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
