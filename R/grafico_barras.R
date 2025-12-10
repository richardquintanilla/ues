#' Gráfico de barras comparativo región vs país
#'
#' Esta función genera un gráfico interactivo comparando comunas de la región y la cobertura del país.
#' @param df_region Dataframe con los valores de la región
#' @param df_pais Dataframe con los valores del país
#' @param comuna Nombre de la columna con las comunas
#' @param cant Nombre de la columna con los valores absolutos
#' @param valor Nombre de la columna con los valores proporcionales (0-1)
#' @param ejey Título del eje Y
#' @param titulo Título del gráfico
#' @param comunas_requeridas Vector con los nombres de comunas a validar
#' @return Objeto plotly
#' @export

grafico_barras <- function(df_region, df_pais, comuna, cant, valor,
                           ejey = "Porcentaje de cobertura (N° de XXX)", 
                           titulo = "Porcentaje de cobertura O'Higgins - XXX",
                           comunas_requeridas = c("Chépica", "Chimbarongo", "Codegua", "Coinco", "Coltauco", "Doñihue", "Graneros",
                                                  "La Estrella", "Las Cabras", "Litueche", "Lolol", "Machalí", "Malloa", "Marchihue",
                                                  "Mostazal", "Nancagua", "Navidad", "Olivar", "Palmilla", "Paredones", "Peralillo",
                                                  "Peumo", "Pichidegua", "Pichilemu", "Placilla", "Pumanque", "Quinta de Tilcoco",
                                                  "Rancagua", "Rengo", "Requínoa", "San Fernando", "San Vicente", "Santa Cruz", 
                                                  "Región")) {
        
        # --- Cargar librerías necesarias ---
        pkgs <- c("dplyr", "ggplot2", "plotly")
        for (p in pkgs) {
                if (!requireNamespace(p, quietly = TRUE)) {
                        stop(paste0("❌ El paquete '", p, "' no está instalado. Instálalo con install.packages('", p, "')"))
                }
        }
        
        # --- Validaciones de columnas ---
        tryCatch({
                df_region %>% dplyr::select({{comuna}}, {{cant}}, {{valor}})
                df_pais    %>% dplyr::select({{comuna}}, {{valor}})
        }, error = function(e) {
                stop("❌ No se encontraron las columnas definidas en 'comuna', 'cant' o 'valor' en los dataframes")
        })
        
        # --- Validaciones de contenido ---
        faltantes <- setdiff(comunas_requeridas, df_region %>% dplyr::pull({{comuna}}))
        if (length(faltantes) > 0) {
                stop(paste0("❌ df_region no contiene todas las filas requeridas. Faltan: ", 
                            paste(faltantes, collapse = ", ")))
        }
        
        if (!("País" %in% (df_pais %>% dplyr::pull({{comuna}})))) {
                stop("❌ df_pais no contiene la fila 'País'")
        }
        
        # --- Preprocesamiento ---
        pais_val <- df_pais %>% 
                dplyr::filter({{comuna}} == "País") %>% 
                dplyr::pull({{valor}}) %>% 
                as.numeric()
        
        region_ref <- df_region %>% 
                dplyr::arrange(dplyr::desc({{valor}})) %>% 
                dplyr::mutate(posicion = seq(1:n())) %>% 
                dplyr::filter({{comuna}} == "Región")
        
        # --- Gráfico ---
        fig <- df_region %>% 
                dplyr::arrange(dplyr::desc({{valor}})) %>% 
                dplyr::mutate(fill_col = seq_along({{comuna}}), height = 1) %>% 
                ggplot2::ggplot(ggplot2::aes(x = reorder({{comuna}}, -{{valor}}), 
                                             y = {{porc}}, 
                                             fill = fill_col)) + 
                ggplot2::geom_col(colour = "black", show.legend = FALSE) +
                ggplot2::ylab(ejey) +
                ggplot2::labs(title = titulo) +
                ggplot2::coord_flip() +
                ggplot2::geom_text(ggplot2::aes(label = paste0(" ", 
                                                               format(round(100*{{valor}}, 2), decimal.mark = ","),
                                                               "%  (",
                                                               format({{cant}}, big.mark = "."), 
                                                               ")"),
                                                text = paste0("Comuna: ", {{comuna}},
                                                              "<br>", "N° de vacunados: ", format({{cant}}, big.mark = "."),
                                                              "<br>", "% de Cobertura: ", 
                                                              format(round(100*{{valor}}, 2), decimal.mark = ","), "%")),
                                   hjust = -0.9, size = 3) +
                ggplot2::geom_vline(xintercept = region_ref$posicion, linetype = 2, color = "hotpink", alpha = 0.5) +
                ggplot2::geom_text(ggplot2::aes(x = region_ref$posicion + 0.5,
                                                y = max(df_region %>% dplyr::pull({{valor}})) * 0.9,
                                                label = paste0("Región: ", 
                                                               format(round(100*region_ref %>% dplyr::pull({{valor}}), 2), decimal.mark = ","), "%")),
                                   color = "hotpink", vjust = -1, size = 3.5) +
                ggplot2::geom_text(ggplot2::aes(x = region_ref$posicion + 1.5,
                                                y = max(df_region %>% dplyr::pull({{valor}})) * 0.9,
                                                label = paste0("País: ", 
                                                               format(round(100*pais_val, 2), decimal.mark = ","), "%")),
                                   color = "black", vjust = -1, size = 3.5) +
                ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                               legend.position = "none") +
                ggplot2::scale_fill_viridis_c(option = "turbo") +
                ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                               axis.ticks.x = ggplot2::element_blank()) + 
                ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.12)))
        
        # --- Plotly ---
        fig_plotly <- plotly::ggplotly(fig, tooltip = "text") %>%
                plotly::style(textposition = "right") %>% 
                plotly::layout(plot_bgcolor = "white",
                               xaxis = list(autorange = TRUE))
        
        return(fig_plotly)
}
