#' Estandarizar comuna y agregar jerarquía territorial
#'
#' Estandariza un código de comuna y agrega nombres oficiales de comuna,
#' provincia y región sin modificar ni renombrar la variable original
#' del data frame.
#'
#' La función utiliza \code{ues::cut} para obtener el nombre histórico
#' de la comuna, actualiza dicho nombre usando \code{ues::cut_actual}
#' cuando corresponde, y cruza la jerarquía territorial desde
#' \code{ues::poblacion_ine}.
#'
#' No se crean ni devuelven columnas auxiliares. Solo se agregan las
#' columnas solicitadas explícitamente en \code{agregar}.
#'
#' Si el código de comuna es \code{"99999"}, los nombres de comuna,
#' provincia y región se asignan como \code{"Ignorada"}.
#'
#' @param df Un data frame o tibble.
#' @param codigo_comuna Variable que identifica el código de comuna.
#'   Se pasa sin comillas y no se modifica.
#' @param agregar Vector de caracteres indicando qué niveles territoriales
#'   agregar. Puede incluir uno o más de: \code{"comuna"},
#'   \code{"provincia"}, \code{"region"}. Por defecto agrega todos.
#'
#' @return Un data frame con columnas territoriales agregadas según
#'   \code{agregar}. El resto del data frame se mantiene intacto.
#'
#' @seealso
#' \code{\link{cut}},
#' \code{\link{cut_actual}},
#' \code{\link{poblacion_ine}}
#'
#' @examples
#' \dontrun{
#' nac %>%
#'   estandarizar_territorio(comuna)
#'
#' nac %>%
#'   estandarizar_territorio(
#'     comuna,
#'     agregar = c("comuna", "region")
#'   )
#' }
#'
#' @importFrom rlang ensym
#' @importFrom dplyr mutate left_join distinct select any_of coalesce if_else
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

  # --- referencia interna (NO se devuelve)
  df <- df %>%
    dplyr::mutate(.codigo_comuna_std = !!comuna_sym)

  # --- nombre histórico de comuna
  df <- df %>%
    dplyr::left_join(
      ues::cut %>%
        dplyr::distinct(
          codigo_comuna,
          nombre_comuna
        ),
      by = c(".codigo_comuna_std" = "codigo_comuna")
    )

  # --- actualizar nombre de comuna (histórico → actual)
  df <- df %>%
    dplyr::left_join(
      ues::cut_actual,
      by = "nombre_comuna",
      suffix = c("_old", "")
    ) %>%
    dplyr::mutate(
      nombre_comuna = dplyr::coalesce(
        nombre_comuna,
        nombre_comuna_old
      )
    ) %>%
    dplyr::select(-dplyr::any_of("nombre_comuna_old"))

  # --- agregar provincia y región
  df <- df %>%
    dplyr::left_join(
      ues::poblacion_ine %>%
        dplyr::distinct(
          codigo_comuna,
          nombre_comuna,
          nombre_provincia,
          nombre_region
        ),
      by = c(
        ".codigo_comuna_std" = "codigo_comuna",
        "nombre_comuna"
      )
    )

  # --- regla explícita: comuna ignorada
  df <- df %>%
    dplyr::mutate(
      nombre_comuna = dplyr::if_else(
        .codigo_comuna_std == "99999",
        "Ignorada",
        nombre_comuna
      ),
      nombre_provincia = dplyr::if_else(
        .codigo_comuna_std == "99999",
        "Ignorada",
        nombre_provincia
      ),
      nombre_region = dplyr::if_else(
        .codigo_comuna_std == "99999",
        "Ignorada",
        nombre_region
      )
    )

  # --- eliminar referencia interna
  df <- df %>%
    dplyr::select(-.codigo_comuna_std)

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
