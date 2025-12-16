#' Calcular dígito verificador (DV) de un RUN/RUT chileno
#'
#' Calcula el dígito verificador usando el algoritmo módulo 11 y
#' devuelve el RUN/RUT completo (sin formato).
#'
#' @param run Vector de RUN/RUT como character o numeric.
#'            Puede incluir puntos y guión.
#'
#' @return Un vector character con el RUN/RUT concatenado con su DV.
#'         Devuelve NA cuando la entrada es NA o vacía.
#'
#' @examples
#' calcular_dv("12.345.678")
#' calcular_dv(c("12345678", "87654321"))
#'
#' @export
calcular_dv <- function(run) {
        run <- as.character(run)
        run <- gsub("[^0-9]", "", run)  # quitar todo lo que no sea dígito

        sapply(run, function(r) {
                # Si es NA o cadena vacía → devolver NA
                if (is.na(r) || r == "") return(NA_character_)

                r_vec <- as.integer(rev(strsplit(r, "")[[1]]))
                factores <- rep(2:7, length.out = length(r_vec))

                suma <- sum(r_vec * factores, na.rm = TRUE)
                resto <- 11 - (suma %% 11)

                dv <- if (resto == 11) {
                        "0"
                } else if (resto == 10) {
                        "K"
                } else {
                        as.character(resto)
                }

                paste0(r, dv)
        })
}
