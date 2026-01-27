#' Estandarizar comuna y agregar jerarquía territorial
#'
#' Estandariza un código de comuna histórico y agrega nombres oficiales de
#' comuna, provincia y región, respetando cambios de código ocurridos en
#' el tiempo.
#'
#' La función no modifica la variable original del data frame. Crea una
#' columna de trabajo \code{codigo_comuna}, estandariza el nombre de la comuna
#' usando \code{ues::cut}, y luego utiliza ese nombre para actualizar códigos
#' antiguos a los códigos vigentes mediante \code{ues::cut_actual}. Finalmente,
#' agrega la jerarquía territorial desde \code{ues::poblacion_ine}.
#'
#' Si el código de comuna es \code{"99999"}, los nombres territoriales
#' disponibles se asignan explícitamente como \code{"Ignorada"}.
#'
#' @param df Un data frame o tibble.
#' @param codigo_comuna Variable que identifica el código de comuna.
#'   Se pasa sin comillas y no se modifica.
#' @param agregar Vector de caracteres indicando qué niveles territoriales
#'   agregar. Puede incluir uno o más de: \code{"comuna"},
#'   \code{"provincia"}, \code{"region"}. Por defecto agrega todos.
#'
#' @return
#' Un data frame con columnas territoriales estandarizadas según lo indicado
#' en \code{agregar}. La variable original se conserva intacta.
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

  # --- crear codigo_comuna de trabajo
  df <- df %>%
    dplyr::mutate(
      codigo_comuna = !!comuna_sym
    )

  # --- obtener nombre base de comuna
  df <- df %>%
    dplyr::left_join(
      ues::cut,
      by = "codigo_comuna"
    )

  # --- actualizar código usando nombre de comuna
  if (
    "nombre_comuna" %in% names(df) &&
    all(c("nombre_comuna", "codigo_comuna") %in% names(ues::cut_actual))
  ) {

    df <- df %>%
      dplyr::left_join(
        ues::cut_actual %>%
          dplyr::rename(codigo_comuna_actual = codigo_comuna),
        by = "nombre_comuna"
      ) %>%
      dplyr::mutate(
        codigo_comuna = dplyr::coalesce(
          codigo_comuna_actual,
          codigo_comuna
        )
      ) %>%
      dplyr::select(-codigo_comuna_actual)
  }

  # --- agregar jerarquía territorial
  df <- df %>%
    dplyr::left_join(
      ues::poblacion_ine %>%
        dplyr::distinct(
          codigo_comuna,
          nombre_comuna,
          nombre_provincia,
          nombre_region
        ),
      by = "codigo_comuna"
    )

  # --- regla comuna ignorada (solo sobre columnas existentes)
  cols_ignorada <- intersect(
    c("nombre_comuna", "nombre_provincia", "nombre_region"),
    names(df)
  )

  if (length(cols_ignorada) > 0) {
    df <- df %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(cols_ignorada),
          ~ dplyr::if_else(
            codigo_comuna == "99999",
            "Ignorada",
            .
          )
        )
      )
  }

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
