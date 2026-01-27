#' Estandarizar comuna y agregar jerarquía territorial
#'
#' A partir de un código de comuna, estandariza los nombres oficiales de
#' comuna, provincia y región. Si una comuna ha cambiado su código en el
#' tiempo, la actualización se realiza usando el nombre de la comuna como
#' clave, obteniendo así el código vigente.
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
#' El proceso de estandarización sigue estos pasos:
#' \enumerate{
#'   \item Crear un \code{codigo_comuna} desde la variable original.
#'   \item Obtener el nombre histórico de la comuna desde \code{ues::cut}.
#'   \item Actualizar nombre y código de comuna usando \code{ues::cut_actual},
#'         cruzando exclusivamente por \code{nombre_comuna}.
#'   \item Agregar provincia y región usando el código vigente.
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

  # --- 2. obtener nombre histórico de comuna
  df <- df %>%
    dplyr::left_join(
      ues::cut %>%
        dplyr::select(codigo_comuna, nombre_comuna),
      by = "codigo_comuna"
    )

  # --- 3. actualizar nombre y codigo usando SOLO el nombre
  idx <- match(df$nombre_comuna, ues::cut_actual$nombre_comuna)

  df$nombre_comuna[!is.na(idx)] <-
    ues::cut_actual$nombre_comuna[idx[!is.na(idx)]]

  df$codigo_comuna[!is.na(idx)] <-
    ues::cut_actual$codigo_comuna[idx[!is.na(idx)]]

  # --- 4. agregar provincia y región con codigo vigente
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

  # --- 5. regla explícita: comuna ignorada
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

  # --- 6. devolver solo columnas solicitadas
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
