#' Genera un mapa estadístico interactivo para las comunas
#'
#' @param df_mapa Data frame con la información a mapear.
#' @param nombre_comuna Nombre de la columna con el nombre de la comuna (string).
#' @param valor Nombre de la columna con el valor numérico a representar (string).
#' @param titulo Título del mapa.
#' @param tipo Tipo de escala: "continuo" o "discreto".
#' @param colores Vector de colores para la escala.
#' @param titulo_leyenda Título de la leyenda.
#' @param comunas_requeridas Vector opcional para forzar comunas específicas.
#' @param tooltip_text Texto plantilla opcional para personalizar el tooltip, ej: "Comuna: {nombre_comuna}<br>N° casos: {n}<br>Tasa: {valor}"
#'
#' @return Un objeto plotly.
#' @export
mapa_estadistico <- function(
                df_mapa,
                nombre_comuna,
                valor,
                titulo = "Mapa estadístico",
                tipo = c("continuo", "discreto"),
                colores = c("red1", "yellow1", "green3"),
                titulo_leyenda = "Valor",
                comunas_requeridas = c("Chépica", "Chimbarongo", "Codegua", "Coinco", "Coltauco",
    "Doñihue", "Graneros", "La Estrella", "Las Cabras", "Litueche", "Lolol", "Machalí",
    "Malloa", "Marchihue", "Mostazal", "Nancagua", "Navidad", "Olivar", "Palmilla",
    "Paredones", "Peralillo", "Peumo", "Pichidegua", "Pichilemu", "Placilla", "Pumanque",
    "Quinta de Tilcoco", "Rancagua", "Rengo", "Requínoa", "San Fernando", "San Vicente",
    "Santa Cruz"),
                tooltip_text = NULL
) {
        library(dplyr)
        library(ggplot2)
        library(plotly)
        library(sf)
        library(glue)
        
        tipo <- match.arg(tipo)
        
        # nombres de columna
        nn <- nombre_comuna
        pv <- valor
        
        # dataframe usuario con columnas estandarizadas
        df_user <- df_mapa %>%
                mutate(
                        nombre_comuna = .data[[nn]],
                        valor = .data[[pv]]
                )
        
        # filtrar comunas requeridas
        if (!is.null(comunas_requeridas)) {
                faltan <- setdiff(comunas_requeridas, df_user$nombre_comuna)
                if (length(faltan) > 0) {
                        stop("Faltan comunas requeridas en el df: ", paste(faltan, collapse = ", "))
                }
                df_user <- df_user %>% filter(nombre_comuna %in% comunas_requeridas)
        }
        
        # mapa sf tal cual, sin tocar geometry
        mapa_sf <- ues::mapa_comunas
        
        mapa_join <- mapa_sf %>%
                left_join(df_user, by = "nombre_comuna")
        
        if (any(is.na(mapa_join$geometry))) {
                faltan <- unique(mapa_join$nombre_comuna[is.na(mapa_join$geometry)])
                stop("Las siguientes comunas no tienen geometría en ues::mapa_comunas: ",
                     paste(faltan, collapse = ", "))
        }
        
        # variable de color
        if (tipo == "discreto") {
                mapa_join <- mapa_join %>%
                        mutate(
                                grupo = ntile(-valor, 3),
                                grupo = factor(grupo, labels = c("Superior", "Medio", "Inferior")),
                                fill_var = grupo
                        )
        } else {
                mapa_join <- mapa_join %>%
                        mutate(fill_var = valor)
        }
        
        # ---- tooltip ----
        if (!is.null(tooltip_text)) {
                # tooltip personalizado usando glue, con coma decimal y dos decimales
                mapa_join <- mapa_join %>%
                        rowwise() %>%
                        mutate(
                                text_label = glue::glue_data(
                                        cur_data(), 
                                        tooltip_text
                                ) %>% as.character()
                        ) %>%
                        ungroup()
        } else {
                # tooltip por defecto
                mapa_join <- mapa_join %>%
                        mutate(
                                text_label = paste0(
                                        "Comuna: ", nombre_comuna, "<br>",
                                        titulo_leyenda, ": ", format(round(valor, 2), decimal.mark = ",")
                                )
                        )
        }
        
        # gráfico
        g <- ggplot(mapa_join) +
                geom_sf(aes(geometry = geometry, fill = fill_var)) +
                geom_sf_text(aes(label = nombre_comuna, text = text_label), size = 3.1, fontface = "bold", color = "black") +
                labs(title = titulo, fill = titulo_leyenda, x = "Longitud", y = "Latitud") +
                theme_light(base_size = 10) +
                theme(legend.position = "bottom", legend.key.size = unit(1, "cm"))
        
        if (tipo == "discreto") {
                g <- g + scale_fill_manual(values = rev(colores))
        } else {
                g <- g + scale_fill_gradientn(colours = colores)
        }
        
        plotly::ggplotly(g, tooltip = "text") %>%
                layout(
                        xaxis = list(autorange = TRUE, scaleanchor = "y", scaleratio = 1),
                        yaxis = list(autorange = TRUE)
                )
}
