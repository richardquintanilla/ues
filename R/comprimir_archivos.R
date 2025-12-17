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

        archivos <- list.files(
                path = ruta_archivos,
                pattern = patron,
                full.names = TRUE
        )

        if (length(archivos) == 0) {
                stop("No se encontraron archivos con el patrón: ", patron)
        }

        fecha <- format(Sys.Date(), "%y%m%d")

        if (is.null(nombre_zip) || nombre_zip == "") {
                base <- "hoy"
        } else {
                base <- sub("\\.zip$", "", basename(nombre_zip), ignore.case = TRUE)
                if (base == "") base <- "hoy"
        }

        nombre_zip_final <- paste0(fecha, "_", base, ".zip")
        ruta_zip <- file.path(ruta_archivos, nombre_zip_final)

        tryCatch(
                utils::zip(zipfile = ruta_zip, files = archivos),
                error = function(e) {
                        stop("Error al crear el ZIP: ", conditionMessage(e))
                }
        )

        if (isTRUE(borrar)) {
                res_borrar <- file.remove(archivos)
                message(sum(res_borrar, na.rm = TRUE),
                        " de ", length(archivos),
                        " archivos comprimidos y eliminados.")
        } else {
                message(length(archivos), " archivos comprimidos y conservados.")
        }

        message("ZIP creado en: ", ruta_zip)

        invisible(list(zip = ruta_zip, archivos = archivos))
}
