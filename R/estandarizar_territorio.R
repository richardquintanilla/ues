#' Estandarizar comuna y agregar jerarquía territorial
#'
#' Estandariza el código de comuna y agrega nombres oficiales de comuna,
#' provincia y región a partir de una variable de código de comuna existente
#' en el data frame.
#'
#' La función renombra internamente la variable indicada a
#' \code{codigo_comuna}, elimina columnas territoriales previas si existen,
#' y cruza información oficial desde \code{ues::cut} y
#' \code{ues::poblacion_ine}. Cuando es posible, actualiza nombres de comuna
#' usando \code{ues::cut_actual}.
#'
#' Si el código de comuna es \code{"99999"}, los nombres de comuna, provincia
#' y región se asignan explícitamente como \code{"Ignorada"}.
#'
#' @param df Un data frame o tibble.
#' @param codigo_comuna Variable que identifica el código de comuna.
#'   Se pasa sin comillas.
#' @param agregar Vector de caracteres indicando qué niveles territoriales
#'   agregar. Puede incluir uno o más de: \code{"comuna"},
#'   \code{"provincia"}, \code{"region"}. Por defecto agrega todos.
#'
#' @return Un data frame con columnas territoriales estandarizadas según
#'   lo indicado en \code{agregar}.
#'
#' @importFrom rlang ensym as_string
#' @importFrom dplyr rename select any_of distinct left_join mutate coalesce
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

  comuna_sym  <- rlang::ensym(codigo_comuna)
  comuna_name <- rlang::as_string(comuna_sym)

  # --- renombrar variable de código de comuna si es necesario
  if (comuna_name != "codigo_comuna") {
    df <- df %>%
      dplyr::rename(codigo_comuna = !!comuna_sym)
  }

  # --- eliminar posibles columnas territoriales previas
  df <- df %>%
    dplyr::select(
      -dplyr::any_of(
        c("nombre_comuna", "nombre_provincia", "nombre_region")
      )
    )

  # --- agregar nombre oficial de comuna
  df <- df %>%
    dplyr::left_join(
      ues::cut,
      by = "codigo_comuna"
    )

  # --- actualizar nombre de comuna si corresponde
  if ("nombre_comuna" %in% names(df)) {

    df <- df %>%
      dplyr::left_join(
        ues::cut_actual,
        by = "nombre_comuna",
        suffix = c("_old", "")
      )

    if ("nombre_comuna_old" %in% names(df)) {
      df <- df %>%
        dplyr::mutate(
          nombre_comuna = dplyr::coalesce(
            .data$nombre_comuna,
            .data$nombre_comuna_old
          )
        ) %>%
        dplyr::select(-nombre_comuna_old)
    }
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

  # --- regla explícita: comuna ignorada (codigo 99999)
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
