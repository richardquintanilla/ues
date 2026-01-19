\name{mapa_estadistico}
\alias{mapa_estadistico}
\title{Mapa estadístico interactivo por comuna}
\description{
Genera un mapa estadístico interactivo a nivel comunal usando
\code{ggplot2} y \code{plotly}, permitiendo escalas continuas o discretas
y tooltips personalizables.
}
\usage{
mapa_estadistico(
  df_mapa,
  nombre_comuna,
  valor,
  titulo = "Mapa estadístico",
  tipo = c("continuo", "discreto"),
  colores = c("red1", "yellow1", "green3"),
  titulo_leyenda = "Valor",
  comunas_requeridas = c(
    "Chépica", "Chimbarongo", "Codegua", "Coinco", "Coltauco",
    "Doñihue", "Graneros", "La Estrella", "Las Cabras", "Litueche",
    "Lolol", "Machalí", "Malloa", "Marchihue", "Mostazal",
    "Nancagua", "Navidad", "Olivar", "Palmilla", "Paredones",
    "Peralillo", "Peumo", "Pichidegua", "Pichilemu", "Placilla",
    "Pumanque", "Quinta de Tilcoco", "Rancagua", "Rengo",
    "Requínoa", "San Fernando", "San Vicente", "Santa Cruz"
  ),
  tooltip_text = NULL
)
}
\arguments{
  \item{df_mapa}{Data frame con la información a mapear.}
  \item{nombre_comuna}{Nombre de la columna con el nombre de la comuna.}
  \item{valor}{Nombre de la columna con el valor numérico a representar.}
  \item{titulo}{Título del mapa.}
  \item{tipo}{Tipo de escala: \code{"continuo"} o \code{"discreto"}.}
  \item{colores}{Vector de colores para la escala.}
  \item{titulo_leyenda}{Título de la leyenda.}
  \item{comunas_requeridas}{Vector opcional para exigir comunas específicas.}
  \item{tooltip_text}{Texto plantilla para personalizar el tooltip.}
}
\value{
Un objeto \code{plotly} interactivo.
}
\examples{
\dontrun{
mapa_estadistico(
  df_mapa = datos,
  nombre_comuna = "comuna",
  valor = "tasa",
  tipo = "continuo"
)
}
}
