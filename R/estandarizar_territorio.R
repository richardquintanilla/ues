#' Estandarizar comuna y agregar jerarquía territorial
#'
#' Estandariza un código de comuna y agrega nombres oficiales de comuna,
#' provincia y región **sin modificar la variable original** del data frame.
#'
#' La función crea una nueva columna \code{codigo_comuna} a partir de la
#' variable indicada, cruza información oficial desde \code{ues::cut} y
#' \code{ues::poblacion_ine}, y actualiza nombres de comuna usando
#' \code{ues::cut_actual} cuando corresponde.
#'
#' Si el código de comuna es \code{"99999"}, los nombres de comuna, provincia
#' y región se asignan explícitamente como \code{"Ignorada"}.
#'
#' @param df Un data frame o tibble.
#' @param codigo_comuna Variable que identifica el código de comuna.
#'   Se pasa sin comillas y **no se modifica**.
#' @param agregar Vector de caracteres indicando qué niveles territoriales
#'   agregar. Puede incluir uno o más de: \code{"comuna"},
#'   \code{"provincia"}, \code{"region"}. Por defecto agrega todos.
#'
#' @return
#' Un data frame con columnas territoriales estandarizadas según lo indicado
#' en \code{agregar}. La variable original se conserva intacta y se agrega
#' la columna \code{codigo_comuna}.
#'
#' @details
#' La función **no renombra, elimina ni duplica** la variable original de
#' comuna. Solo agrega columnas nuevas con información territorial
#' estandarizada.
#'
#' No se crean columnas auxiliares visibles (como \code{*_old}); cualquier
#' lógica intermedia se maneja de forma interna y limpia.
#'
#' @seealso
#' \code{\link{cut}},
#' \code{\link{cut_actual}},
#' \code{\link{poblacion_ine}}
#'
#' @examples
#' \dontrun{
#' nac %>%
#'   estandarizar_territorio(COMUNA)
#'
#' nac %>%
#'   estandarizar_territorio(
#'     COMUNA,
#'     agregar = c("comuna", "region")
#'   )
#' }
#'
#' @importFrom rlang ensym
#' @importFrom dplyr
#'   select any_of distinct left_join mutate coalesce if_else across
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

  # --- crear codigo_comuna estandarizado (sin tocar el original)
  df <- df %>%
    dplyr::mutate(
      codigo_comuna = !!comuna_sym
    )

  # --- agregar nombre oficial de comuna
  df <- df %>%
    dplyr::left_join(
      ues::cut,
      by = "codigo_comuna"
    )

  # --- actualizar nombre de comuna sin crear columnas *_old
  if ("nombre_comuna" %in% names(df)) {

    df <- df %>%
      dplyr::left_join(
        ues::cut_actual %>%
          dplyr::select(
            nombre_comuna,
            nombre_comuna_actual = nombre_comuna
          ),
        by = "nombre_comuna"
      ) %>%
      dplyr::mutate(
        nombre_comuna = dplyr::coalesce(
          nombre_comuna_actual,
          nombre_comuna
        )
      ) %>%
      dplyr::select(-nombre_comuna_actual)
  }

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
      by = c("codigo_comuna", "nombre_comuna")
    )

  # --- regla explícita: comuna ignorada (99999)
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        c("nombre_comuna", "nombre_provincia", "nombre_region"),
        ~ dplyr::if_else(
          codigo_comuna == "99999",
          "Ignorada",
          .
        )
      )
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
