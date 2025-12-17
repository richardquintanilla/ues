#' Comprimir archivos en un ZIP según un patrón
#'
#' Comprime archivos de una carpeta que coincidan con un patrón y
#' genera un archivo ZIP con fecha en el nombre.
#'
#' @param ruta_archivos Ruta del directorio que contiene los archivos.
#' @param patron Patrón (regex) para filtrar archivos.
#' @param nombre_zip Nombre base del ZIP (opcional, sin .zip).
#' @param borrar Logical. Si TRUE, elimina los archivos originales.
#'
#' @return Lista invisible con:
#' \describe{
#'   \item{zip}{Ruta completa del archivo ZIP creado}
#'   \item{archivos}{Vector de archivos comprimidos}
#' }
#'
#' @examples
#' \dontrun{
#' comprimir_archivos(
#'   ruta_archivos = tempdir(),
#'   patron = "\\\\.csv$",
#'   nombre_zip = "respaldo",
#'   borrar = FALSE
#' )
#' }
#'
#' @export
comprimir_archivos <- function(ruta_archivos,
                               patron,
                               nombre_zip = NULL,
                               borrar = FALSE) {

  ruta_archivos <- normalizePath(ruta_archivos, mustWork = FALSE)

  if (!dir.exists(ruta_archivos)) {
    stop("La ruta especificada no existe: ", ruta_archivos)
  }

  archivos_full <- list.files(
    path = ruta_archivos,
    pattern = patron,
    full.names = TRUE
  )

  if (length(archivos_full) == 0) {
    stop("No se encontraron archivos con el patrón: ", patron)
  }

  # 👇 SOLO nombres, sin ruta
  archivos <- basename(archivos_full)

  fecha <- format(Sys.Date(), "%y%m%d")
  base <- if (is.null(nombre_zip) || nombre_zip == "") {
    "hoy"
  } else {
    sub("\\.zip$", "", basename(nombre_zip), ignore.case = TRUE)
  }

  nombre_zip_final <- paste0(fecha, "_", base, ".zip")
  ruta_zip <- file.path(ruta_archivos, nombre_zip_final)

  # Zip plano SIN setwd()
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(ruta_archivos)

  utils::zip(zipfile = nombre_zip_final, files = archivos)

  if (isTRUE(borrar)) {
    res <- file.remove(archivos_full)
    message(sum(res, na.rm = TRUE),
            " de ", length(archivos_full),
            " archivos comprimidos y eliminados.")
  } else {
    message(length(archivos_full), " archivos comprimidos y conservados.")
  }

  message("ZIP creado en: ", ruta_zip)

  invisible(list(zip = ruta_zip, archivos = archivos))
}
