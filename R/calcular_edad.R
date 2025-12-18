#' Calcular edad exacta en formato AAA MM DD
#'
#' Calcula la diferencia entre dos fechas en años, meses y días,
#' devolviendo un string con formato \code{AAAMMDD}.
#'
#' @param fecha_inicial Fecha de inicio (nacimiento), coercible a Date.
#' @param fecha_final Fecha final de referencia. Por defecto \code{Sys.Date()}.
#'
#' @return Un vector character con la edad en formato \code{AAAMMDD}.
#'         Devuelve NA cuando alguna fecha es inválida.
#'
#' @details
#' Si \code{fecha_final} es anterior a \code{fecha_inicial}, se ajusta
#' automáticamente para evitar edades negativas.
#'
#' @examples
#' calcular_edad("1990-05-10", "2024-05-09")
#' calcular_edad(c("2000-01-01", "2010-06-15"))
#'
#' @importFrom lubridate year month day years months `%m+%`
#' @export}

calcular_edad <- function(fecha_inicial, fecha_final = Sys.Date()) {

        # Intentar convertir a Date
        fecha_inicial <- suppressWarnings(as.Date(fecha_inicial))
        fecha_final   <- suppressWarnings(as.Date(fecha_final))

        # Vectorización
        n <- max(length(fecha_inicial), length(fecha_final))
        fecha_inicial <- rep(fecha_inicial, length.out = n)
        fecha_final   <- rep(fecha_final,   length.out = n)

        resultado <- rep(NA_character_, n)
        validas <- !is.na(fecha_inicial) & !is.na(fecha_final)

        if (!any(validas)) {
                warning("Todas las fechas son NA o inválidas.")
                return(resultado)
        }

        fi <- fecha_inicial[validas]
        ff <- pmax(fi, fecha_final[validas])  # Evita negativos

        # Diferencias simples
        anios  <- lubridate::year(ff)  - lubridate::year(fi)
        meses <- lubridate::month(ff) - lubridate::month(fi)
        dias  <- lubridate::day(ff)   - lubridate::day(fi)

        # Ajustes
        ajuste_mes <- dias < 0
        meses[ajuste_mes] <- meses[ajuste_mes] - 1

        ajuste_anio <- meses < 0
        anios[ajuste_anio] <- anio[ajuste_anio] - 1
        meses[ajuste_anio] <- meses[ajuste_anio] + 12

        # Recalcular fecha base para días
        fecha_base <- fi %m+% lubridate::years(anios) %m+% lubridate::months(meses)
        dias <- as.integer(ff - fecha_base)

        resultado[validas] <- paste0(
                sprintf("%03d", anios),
                sprintf("%02d", meses),
                sprintf("%02d", dias)
        )

        resultado
}
