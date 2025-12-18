#' Calcular edad exacta entre dos fechas
#'
#' @param fecha_inicial Fecha inicial (Date o coercible)
#' @param fecha_final Fecha final (Date o coercible). Por defecto Sys.Date()
#'
#' @return character en formato AAAMMDD
#' @export
calcular_edad <- function(fecha_inicial, fecha_final = Sys.Date()) 
{
  fecha_inicial <- suppressWarnings(as.Date(fecha_inicial))
  fecha_final   <- suppressWarnings(as.Date(fecha_final))
  
  n <- max(length(fecha_inicial), length(fecha_final))
  fecha_inicial <- rep(fecha_inicial, length.out = n)
  fecha_final   <- rep(fecha_final, length.out = n)
  
  resultado <- rep(NA_character_, n)
  validas <- !is.na(fecha_inicial) & !is.na(fecha_final)
  
  if (!any(validas)) {
    warning("Todas las fechas son NA o inválidas.")
    return(resultado)
  }
  
  fi <- fecha_inicial[validas]
  ff <- pmax(fi, fecha_final[validas])
  
  anios <- lubridate::year(ff)  - lubridate::year(fi)
  meses <- lubridate::month(ff) - lubridate::month(fi)
  dias  <- lubridate::day(ff)   - lubridate::day(fi)
  
  meses[dias < 0] <- meses[dias < 0] - 1
  anios[meses < 0] <- anios[meses < 0] - 1
  meses[meses < 0] <- meses[meses < 0] + 12
  
  fecha_base <- lubridate::add_with_rollback(
    fi,
    lubridate::period(years = anios, months = meses)
  )
  
  dias <- as.integer(ff - fecha_base)
  
  resultado[validas] <- paste0(
    sprintf("%03d", anios),
    sprintf("%02d", meses),
    sprintf("%02d", dias)
  )
  
  resultado
}
