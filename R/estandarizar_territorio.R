#' Estandarizar comuna y agregar jerarquía territorial
#'
#' Estandariza un código de comuna y agrega nombres oficiales de comuna,
#' provincia y región, sin modificar ni renombrar la variable original
#' del data frame.
#'
#' La función crea internamente una referencia a \code{codigo_comuna},
#' cruza información oficial desde \code{ues::cut} y
#' \code{ues::poblacion_ine}, y actualiza nombres de comuna utilizando
#' \code{ues::cut_actual} para reflejar cambios históricos de códigos.
#'
#' No se crean ni devuelven columnas auxiliares (como *_old).
#' Solo se agregan las columnas solicitadas explícitamente en
#' \code{agregar}.
#'
#' Si \code{codigo_comuna == "99999"}, los nombres de comuna, provincia
#' y región se asignan como \code{"Ignorada"}.
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

  # --- crear referencia interna estandarizada
  df <- df %>%
    dplyr::mutate(.codigo_comuna_std = !!comuna_sym)

  # --- obtener nombre base de comuna
  df <- df %>%
    dplyr::left_join(
      ues::cut %>%
        dplyr::distinct(
          codigo_comuna,
          nombre_comuna
        ),
      by = c(".codigo_comuna_std" = "codigo_comuna")
    )

  # --- actualizar nombre de comuna (si hay match)
  df <- df %>%
    dplyr::left_join(
      ues::cut_actual %>%
        dplyr::distinct(
          codigo_comuna,
          nombre_comuna
        ),
      by = "nombre_comuna",
      suffix = c("", "_actual")
    ) %>%
    dplyr::mutate(
      nombre_comuna = dplyr::if_else(
        !is.na(nombre_comuna_actual),
        nombre_comuna_actual,
        nombre_comuna
      )
    ) %>%
    dplyr::select(-nombre_comuna_actual)

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
        "nombre_comuna"      = "nombre_comuna"
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
