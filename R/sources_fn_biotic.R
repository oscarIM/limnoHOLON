#### funciones para medio biótico####
#' @title fn_plot_bar_biotic
#' @description función para graficar de barras para la abundancia de las especies/taxones de un taxón determinado
#' @param data archivo entrada que tiene que tener, al menos, las columnas que representen al taxón, grupo taxonómico, sitios y N. Tiene que estar en formato "long". Se recomida usar csv o tsv como formatos.
#' @param col_sitio cadena de texto que indica el nombre de la columna que tiene los sitios de muestreo.
#' @param code_sitio cadena de texto que indica el código (generalmente una letra seguida un guión) que utilizada para nombras las estaciones (eg. "E-", "P-", "D-").
#' @param col_zona Si aplica, cadena de texto que indica el nombre de la columna que contiene los grupos o factores de agrupamiento en la base de datos de entrada.
#' @param col_taxa cadena de texto que indica el nombre de la columna que contiene a los taxones (generalmente se llama especie, taxa, etc.) en la base de datos de entrada.
#' @param col_N cadena de texto que indica el nombre de la columna que contiene el N (abundancia) en la base de datos de entrada.
#' @param taxa_grupo cadena de texto que indica la clasificación taxonómica de los taxones (e.g. Familia, Clase, etc.) en la base de datos de entrada.
#' @param ord_sitio cadena de texto que indica el orden en el que se quiere aparezcan los sitios en el gráfico (de forma ascendente o descendente de acuerdo al número de estaciones o puntos de muestreo). Por defecto, toma valor "asc" (orden ascendente).
#' @param ord_zonas Si aplica, cadena de texto que indica el orden (en la leyenda, ejes, etc.) del grupo o factor de agrupamiento. Por defecto, NULO.
#' @param taxa_id cadena de texto que indica el nombre del "grupo funcional" al cual pertenecen los taxones ("fitoplancton", "zooplancton", "macrofitas", "perifiton", "ictiofauna",etc.) 
#' @param width ancho del gráfico. Por defecto, toma valor 12
#' @param height alto del gráfico. Por defecto, toma valor 12
#' @import tidyverse
#' @import RColorBrewer
#' @import grDevices
#' @return gráfico de barras de abundancia/riqueza de especies de taxones por grupo taxonómico
#' @export fn_plot_bar_biotic
#' @examples
#' \dontrun{
#' # Para gráfico sin variable o factor de agrupamiento 
#' data <- read_tsv("data_fitoplancton.tsv")
#' col_taxa <- "Taxa"
#' taxa_grupo <- "Clase"
#' col_N <- "N"
#' col_sitio <- "Sitio"
#' ord_sitio <- "asc"
#' code_sitio <- "P-"
#' taxa_id <- "fitoplancton"
#' width <- 12
#' height <- 12
#' fn_plot_bar_biotic(data = data, col_taxa = col_taxa, taxa_grupo = taxa_grupo,col_N = col_N, col_sitio = col_sitio, ord_sitio = ord_sitio, code_sitio = code_sitio, taxa_id = taxa_id, width = width, height = height)
#' # Para gráfico con 1 variable o factor de agrupamiento, en orden alfanumérico. 
#' data <- read_tsv("data_fitoplancton.tsv")
#' #se necesita que la variable o factor este en el dataframe
#' data_grupos <- read_tsv("data_grupos.tsv") 
#' data <- left_join(data, data_grupos, by = c("Sitio" = "Sitio"))
#' col_taxa <- "Taxa"
#' taxa_grupo <- "Clase"
#' col_N <- "N"
#' col_sitio <- "Sitio"
#' ord_sitio <- "asc"
#' code_sitio <- "P-"
#' col_zonas <- "Grupo"
#' taxa_id <- "fitoplancton"
#' width <- 12
#' height <- 12
#' fn_plot_bar_biotic(data = data, col_taxa = col_taxa, taxa_grupo = taxa_grupo,col_N = col_N, col_sitio = col_sitio, ord_sitio = ord_sitio, code_sitio = code_sitio, col_zonas = col_zonas, taxa_id = taxa_id, width = width, height = height)
#' # Para gráfico con 1 variable o factor de agrupamiento, en algún orden definido. 
#' data <- read_tsv("data_fitoplancton.tsv")
#' #se necesita que la variable o factor este en el dataframe
#' data_grupos <- read_tsv("data_grupos.tsv") 
#' data <- left_join(data, data_grupos, by = c("Sitio" = "Sitio"))
#' col_taxa <- "Taxa"
#' taxa_grupo <- "Clase"
#' col_N <- "N"
#' col_sitio <- "Sitio"
#' ord_sitio <- "asc"
#' code_sitio <- "P-"
#' col_zonas <- "Grupo"
#' ord_zonas <- c("Q1", "Q2", "Q3", "Q4", "Q5","L")
#' taxa_id <- "fitoplancton"
#' width <- 12
#' height <- 12
#' fn_plot_bar_biotic(data = data, col_taxa = col_taxa, taxa_grupo = taxa_grupo,col_N = col_N, col_sitio = col_sitio, ord_sitio = ord_sitio, code_sitio = code_sitio, col_zonas = col_zonas,ord_zonas = ord_zonas, taxa_id = taxa_id, width = width, height = height)
#' }

fn_plot_bar_biotic <- function(data, col_sitio, col_N, col_zonas = NULL, col_taxa, taxa_grupo, ord_sitio = "asc", ord_zonas = NULL, taxa_id, code_sitio, width = 12, height = 12) {
  options(scipen = 999)
  # setting dataframe#
  vars <- c(col_sitio, taxa_grupo, col_taxa, col_N)
  data_plot <- data %>% select(all_of(vars))
  data_plot <- data_plot %>% 
    dplyr::select(all_of(vars)) %>% 
    dplyr::rename_at(vars, ~ c( "col_sitio", "taxa_grupo", "col_taxa", "col_N"))
  #setting some vars#
  if (taxa_id == "ictiofauna") {
    n_taxa <- data_plot %>%
    dplyr::pull(col_taxa) %>%
    unique() %>%
    length()
    } else {
      n_taxa <- data_plot %>%
        dplyr::pull(taxa_grupo) %>%
        unique() %>%
        length()
    }
  sitios_tmp <-  data_plot %>% 
    dplyr::pull(col_sitio) %>% 
    stringr::str_extract_all(string = ., "\\d+",simplify = T) %>% 
    as.numeric() %>% 
    unique()
  
  sitios_ord <- dplyr::case_when(
    ord_sitio == "asc" ~ paste0(code_sitio, sitios_tmp),
    ord_sitio == "desc" ~ paste0(code_sitio, rev(sitios_tmp)),
    TRUE ~ NA_character_
  )
  # flujo si col_zonas == NULL hay una columna de zona, se indica/determina la zona y se crear un dataplot dependiendo si es o no un ouputd_id == "macrofita"
  if (is.null(col_zonas)) {
    if (str_detect(taxa_id, "(?i)macrofita")) {
      # que hacer si no hay columna de zonas y es para macrofitas
      data_plot <- data_plot %>%
        dplyr::mutate(
          col_N = case_when(
            stringr::str_detect(col_N, "1") ~ 3,
            stringr::str_detect(col_N, "\\+") ~ 2,
            stringr::str_detect(col_N, "r") ~ 1,
            stringr::str_detect(col_N, "2") ~ 4,
            stringr::str_detect(col_N, "3") ~ 5,
            stringr::str_detect(col_N, "4") ~ 6,
            stringr::str_detect(col_N, "5") ~ 7,
            TRUE ~ as.numeric(col_N)
          )
        ) %>%
        dplyr::mutate(
          col_N = tidyr::replace_na(col_N, 0),
          col_taxa = factor(col_taxa, levels = unique(col_taxa)),
          col_sitio = factor(col_sitio, levels = sitios_ord),
          taxa_grupo = factor(taxa_grupo, levels = sort(unique(taxa_grupo)))
        )
      color <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Paired"))(n_taxa)
      plot <- ggplot() +
        geom_col(data = data_plot, aes(x = col_sitio, y = col_N, fill = col_taxa), position = "dodge") +
        labs(
          x = "Sitio",
          y = "Densidad cualitativa"
        ) +
        scale_fill_manual("Especie", values = color) +
        guides(fill = guide_legend(ncol = 2)) +
        facet_grid(scales = "free_y", switch = "y", rows = vars(taxa_grupo)) +
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
        theme_light()
      ggsave(filename = paste0("bar_", taxa_id, "_by_", taxa_grupo, ".png"), plot = plot, width = width, height = height, dpi = 300)
      } else {
        data_plot <- data_plot %>% 
          dplyr::mutate(
            col_N = tidyr::replace_na(col_N, 0),
            col_taxa = factor(col_taxa, levels = unique(col_taxa)),
            col_sitio = factor(col_sitio, levels = sitios_ord),
            taxa_grupo = factor(taxa_grupo, levels = sort(unique(taxa_grupo))),
            S = if_else(col_N >= 1, 1, 0)
      )
        data_plot <- data_plot %>%
        dplyr::group_by(taxa_grupo, col_sitio) %>%
        dplyr::summarise(
          N = sum(col_N, na.rm = TRUE),
          S = sum(S, na.rm = TRUE)) %>%
        tidyr::pivot_longer(cols = c("N", "S"), names_to = "vars", values_to = "values") %>%
        dplyr::group_by(vars) %>%
        dplyr::group_split()
        data_plot <- purrr::map(data_plot, ~ dplyr::arrange(., desc(values))) 
        data_plot <- dplyr::bind_rows(data_plot) %>%
          dplyr::mutate(label = dplyr::case_when(
            vars == "N" ~ "Abundancia relativa (ind/m³)",
            vars == "S" ~ "Número de taxa (S)"
        ))
      color <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Paired"))(n_taxa)
      plot <- ggplot() +
        geom_col(data = data_plot, aes(x = col_sitio, y = values, fill = fct_inorder(taxa_grupo)), position = "dodge") +
        labs(x = "Sitio",y = "Valor") +
        scale_fill_manual(taxa_grupo, values = color) +
        guides(fill = guide_legend(ncol = 1)) +
        facet_grid(scales = "free", space = "free_x", switch = "y", rows = vars(label)) +
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
        theme_light()
      ggsave(filename = paste0("bar_", taxa_id, "_by_", taxa_grupo, ".png"), plot = plot, width = width, height = height, dpi = 300)
      }
  }
  if (!is.null(col_zonas) && is.null(ord_zonas)) {
    if (stringr::str_detect(taxa_id, "(?i)macrofita")) {
      #formatear datos con presencia col_zonas, con orden y para macrofitas y no macrofitas
      col_zonas <- data %>% dplyr::pull({{col_zonas}})
      data_plot <- data_plot %>% dplyr::mutate(col_zonas = col_zonas)
      order_zone <- data_plot %>% 
        dplyr::pull(col_zonas) %>% 
        unique() %>% 
        sort()
      data_plot <- data_plot %>%
        dplyr::mutate(
          col_N = case_when(
            stringr::str_detect(col_N, "1") ~ 3,
            stringr::str_detect(col_N, "\\+") ~ 2,
            stringr::str_detect(col_N, "r") ~ 1,
            stringr::str_detect(col_N, "2") ~ 4,
            stringr::str_detect(col_N, "3") ~ 5,
            stringr::str_detect(col_N, "4") ~ 6,
            stringr::str_detect(col_N, "5") ~ 7,
            TRUE ~ as.numeric(col_N))) %>% 
        dplyr::mutate(
          col_N = tidyr::replace_na(col_N, 0),
          col_taxa = factor(col_taxa, levels = unique(col_taxa)),
          col_sitio = factor(col_sitio, levels = sitios_ord),
          taxa_grupo = factor(taxa_grupo, levels = sort(unique(taxa_grupo))),
          col_zonas = factor(col_zonas, levels = order_zone))
      color <- colorRampPalette(brewer.pal(9, "Paired"))(n_taxa)
      plot <- ggplot() + 
        geom_col(data = data_plot, aes(x = col_sitio, y = col_N, fill = taxa_grupo), position = "dodge") +
        labs(x = "Sitio", y = "Densidad cualitativa") +
        scale_fill_manual(stringr::str_to_sentence(taxa_grupo), values = color) +
        guides(fill = guide_legend(ncol = 1)) +
        facet_grid(scales = "free", switch = "y", cols = vars(col_zonas)) +
        #facet_grid(scales = "free", switch = "y", rows = vars(taxa_grupo), cols = vars(col_zonas)) +
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
        theme_light()
      ggsave(filename = paste0("bar_", taxa_id, "_by_", taxa_grupo, ".png"), plot = plot, width = width, height = height, dpi = 300)
    } else {
      #formatear datos con presencia col_zonas, sin orden y para todo menos macrofitas
      col_zonas <- data %>% dplyr::pull({{col_zonas}})
      data_plot <- data_plot %>% dplyr::mutate(col_zonas = col_zonas)
      order_zone <- data_plot %>% 
        dplyr::pull(col_zonas) %>% 
        unique() %>% 
        sort()
      data_plot <- data_plot %>% dplyr::mutate(
        col_N = tidyr::replace_na(col_N, 0),
        col_taxa = factor(col_taxa, levels = unique(col_taxa)),
        col_sitio = factor(col_sitio, levels = sitios_ord),
        taxa_grupo = factor(taxa_grupo, levels = sort(unique(taxa_grupo))),
        col_zonas = factor(col_zonas, levels = order_zone),
        S = if_else(col_N >= 1, 1, 0)) %>% 
        dplyr::group_by(taxa_grupo, col_sitio, col_zonas) %>%
        dplyr::summarise(
          N = sum(col_N, na.rm = TRUE),
          S = sum(S, na.rm = TRUE)) %>% 
        tidyr::pivot_longer(cols = c("N", "S"), names_to = "vars", values_to = "values") %>%
        dplyr::group_by(vars) %>%
        dplyr::group_split()
      data_plot <- purrr::map(data_plot, ~ dplyr::arrange(., desc(values)))
      data_plot <- dplyr::bind_rows(data_plot) %>% 
        dplyr::mutate(label = dplyr::case_when(
          vars == "N" ~ "Abundancia relativa (ind/m³)",
          vars == "S" ~ "Número de taxa (S)"))
      color <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Paired"))(n_taxa)
      plot <- ggplot() + 
        geom_col(data = data_plot, aes(x = col_sitio, y = values, fill = fct_inorder(taxa_grupo)), position = "dodge") + 
        labs(x = "Sitio", y = "Valor") + 
        scale_fill_manual(taxa_grupo, values = color) + 
        guides(fill = guide_legend(ncol = 1)) + 
        facet_grid(scales = "free", space = "free_x", switch = "y", rows = vars(label), cols = vars(col_zonas)) + 
        theme_light() 
      ggsave(filename = paste0("bar_", taxa_id, "_by_", taxa_grupo, ".png"), plot = plot, width = width, height = height, dpi = 300)
      
    }
  }
  if (!is.null(col_zonas) && !is.null(ord_zonas)) {
    if (str_detect(taxa_id, "(?i)macrofita")) {
        #formatear datos con presencia col_zonas, con orden y para macrofitas y no macrofitas
      col_zonas <- data %>% dplyr::pull({{col_zonas}})
      data_plot <- data_plot %>% dplyr::mutate(col_zonas = col_zonas)
      order_zone <- ord_zonas
      data_plot <- data_plot %>%
          dplyr::mutate(
            col_N = case_when(
              str_detect(col_N, "1") ~ 3,
              str_detect(col_N, "\\+") ~ 2,
              str_detect(col_N, "r") ~ 1,
              str_detect(col_N, "2") ~ 4,
              str_detect(col_N, "3") ~ 5,
              str_detect(col_N, "4") ~ 6,
              str_detect(col_N, "5") ~ 7,
              TRUE ~ as.numeric(col_N)
            )
          ) %>%
          dplyr::mutate(
            col_N = tidyr::replace_na(col_N, 0),
            col_taxa = factor(col_taxa, levels = unique(col_taxa)),
            col_sitio = factor(col_sitio, levels = sitios_ord),
            taxa_grupo = factor(taxa_grupo, levels = sort(unique(taxa_grupo))),
            col_zonas = factor(col_zonas, levels = order_zone)
          )
      color <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Paired"))(length(unique(data_plot$col_taxa)))
        plot <- ggplot() + 
          geom_col(data = data_plot, aes(x = col_sitio, y = col_N, fill = col_taxa), position = "dodge") +
          labs(x = "Sitio", y = "Densidad cualitativa") +
          scale_fill_manual("Especie", values = color) +
          guides(fill = guide_legend(ncol = 2)) +
          facet_grid(scales = "free", switch = "y", rows = vars(taxa_grupo), cols = vars(col_zonas)) +
          theme_light()
        ggsave(filename = paste0("bar_", taxa_id, "_by_", taxa_grupo, ".png"), plot = plot, width = width, height = height, dpi = 300)
      } else {
        #formatear datos con presencia col_zonas, con orden y todo menos macrofitas
        col_zonas <- data %>% dplyr::pull({{col_zonas}})
        data_plot <- data_plot %>% dplyr::mutate(col_zonas = col_zonas)
        order_zone <- ord_zonas
        data_plot <- data_plot %>% 
          dplyr::mutate(
            col_N = tidyr::replace_na(col_N, 0),
            col_taxa = factor(col_taxa, levels = unique(col_taxa)),
            col_sitio = factor(col_sitio, levels = sitios_ord),
            taxa_grupo = factor(taxa_grupo, levels = sort(unique(taxa_grupo))),
            col_zonas = factor(col_zonas, levels = order_zone),
            S = if_else(col_N >= 1, 1, 0)) %>% 
          dplyr::group_by(taxa_grupo, col_sitio, col_zonas) %>%
          dplyr::summarise(
            N = sum(col_N, na.rm = TRUE),
            S = sum(S, na.rm = TRUE)) %>% 
          tidyr::pivot_longer(cols = c("N", "S"), names_to = "vars", values_to = "values") %>%
          dplyr::group_by(vars) %>%
          dplyr::group_split()
        data_plot <- purrr::map(data_plot, ~ dplyr::arrange(., desc(values)))
        data_plot <- dplyr::bind_rows(data_plot) %>% 
          dplyr::mutate(label = dplyr::case_when(
            vars == "N" ~ "Abundancia relativa (ind/m³)",
            vars == "S" ~ "Número de taxa (S)"))
        color <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Paired"))(n_taxa)
        plot <- ggplot() + 
          geom_col(data = data_plot, aes(x = col_sitio, y = values, fill = fct_inorder(taxa_grupo)), position = "dodge") + 
          labs(x = "Sitio", y = "Valor") + 
          scale_fill_manual(taxa_grupo, values = color) + 
          guides(fill = guide_legend(ncol = 1)) + 
          facet_grid(scales = "free", space = "free_x", switch = "y", rows = vars(label), cols = vars(col_zonas)) + 
          theme_light() 
        ggsave(filename = paste0("bar_", taxa_id, "_by_", taxa_grupo, ".png"), plot = plot, width = width, height = height, dpi = 300)
        
      }
    }
}

#' @title fn_plot_pie
#' @description función para graficar la composición porcentual de taxones por grupos taxonómicos en gráficos de tortas.
#' @param data archivo entrada que tiene que tener, al menos, las columnas que representen al taxón, grupo taxonómico, taxones y N. Tiene que estar en formato "long". Se recomida usar csv o tsv como formatos.
#' @param col_taxa cadena de texto que indica el nombre de la columna que contiene a los taxones (generalmente se llama especie, taxa, etc) en la base de datos de entrada.
#' @param taxa_grupo cadena de texto que indica la columna que contiene la clasificación taxonómica de los taxones (e.g. Familia, Clase, etc) en la base de datos de entrada.
#' @param col_N cadena de texto que indica el nombre de la columna que contiene el N (abundancia) en la base de datos de entrada.
#' @param  cumsum_cut Valor entero o decimal de la suma acumulada hasta la cual se quiera representar sin la etiqueta de "Otros". Por defecto, toma valor 90. 
#' @param  n_size Valor entero que indica el número de piezas en el gráfico de torta. Por defecto, toma valor de 5.
#' @param width ancho del gráfico. Por defecto, toma valor 12.
#' @param height alto del gráfico. Por defecto, toma valor 13.
#' @import tidyverse
#' @import RColorBrewer
#' @import grDevices
#' @import patchwork
#' @return gráfico de barras de abundancia/riqueza de especies de taxones por grupo taxonómico
#' @export fn_plot_pie
#' @examples
#' \dontrun{
#' # Para gráfico sin variable o factor de agrupamiento 
#' data <- read_tsv("data_fitoplancton.tsv")
#' taxa_id <- "fitoplancton"
#' col_N <- "N"
#' col_taxa <- "Taxa"
#' taxa_grupo <- "Clase"
#' cumsum_cut <- 90
#' n_size <- 5
#' width = width
#' height = height
#' fn_plot_pie <- function(data = data, taxa_grupo = taxa_grupo, cumsum_cut = 90, n_size = 5, width = width, height = height)
#' }
fn_plot_pie <- function(data, taxa_grupo, col_N, cumsum_cut = 90, n_size = 5, width = 12, height = 13) {
  options(scipen = 999)
  vars <- c(taxa_grupo, col_taxa, col_N)
  data_plot <- data %>%
    select(all_of(vars)) %>%
    rename_at(vars , ~c("taxa_grupo", "col_taxa", "col_N"))
  fn_to_data_frame_high <- function(data_list) {
    list <- map(data_list, ~ {
      .x %>%
        group_by(col_taxa) %>%
        summarise(N = sum(col_N)) %>%
        mutate(Prop = round(N / sum(N) * 100, 2)) %>%
        arrange(desc(Prop)) %>%
        mutate(
          cum_sum = cumsum(Prop),
          col_taxa = ifelse(cum_sum > cumsum_cut, "Otros", as.character(col_taxa))
        ) %>%
        group_by(col_taxa) %>%
        summarise(Prop = sum(Prop))
    })
    list_otros <- map(list, ~ filter(., col_taxa == "Otros"))
    list_main <- map(list, ~ {
      filter(., col_taxa != "Otros") %>%
        arrange(desc(Prop))
    })
    final <- map2(list_main, list_otros, ~ bind_rows(.x, .y))
    final
  }
  fn_to_data_frame_low <- function(data_list) {
    final <- map(data_list, ~ {
      .x %>%
        group_by(col_taxa) %>%
        summarise(N = sum(col_N)) %>%
        mutate(Prop = round(N / sum(N) * 100, 2)) %>%
        select(-N) %>%
        arrange(desc(Prop)) %>%
        mutate(col_taxa = as.character(col_taxa))
    })
    final
  }
  
  sum_taxa <- data_plot %>%
    group_by(taxa_grupo) %>%
    summarise(n = n_distinct(col_taxa)) %>%
    arrange(desc(n)) %>%
    ungroup()
  n_high <- filter(sum_taxa, n > n_size)
  n_low <- filter(sum_taxa, n <= n_size)
  
  data_list_high <- data_plot %>%
    filter(taxa_grupo %in% n_high$taxa_grupo) %>%
    group_split(taxa_grupo) %>%
    setNames(map(., ~ unique(as.character(.$taxa_grupo))))
  data_list_low <- data_plot %>%
    filter(taxa_grupo %in% n_low$taxa_grupo) %>%
    group_split(taxa_grupo) %>%
    setNames(map(., ~ unique(as.character(.$taxa_grupo))))
  
  num_rows_high <- map_int(data_list_high, ~ n_distinct(.$col_taxa))
  data_list_high <- data_list_high[order(num_rows_high, decreasing = TRUE)]
  num_rows_low <- map_int(data_list_low, ~ n_distinct(.$col_taxa))
  data_list_low <- data_list_low[order(num_rows_low, decreasing = TRUE)]
  
  list_high <- fn_to_data_frame_high(data_list = data_list_high)
  list_low <- fn_to_data_frame_low(data_list = data_list_low)
  final_list <- c(list_high, list_low)
  
  all_palettes <- as.data.frame(RColorBrewer::brewer.pal.info) %>%
    filter(category == "seq", colorblind == "TRUE") %>%
    mutate(name_pal = row.names(.)) %>%
    filter(name_pal != "Greys") %>%
    slice(1:length(final_list))
  
  color_palette <- all_palettes$name_pal
  
  color_list <- map2(final_list, color_palette, ~ {
    colorRampPalette(brewer.pal(9, .y))(length(unique(.x$col_taxa)))
  })
  color_list <- map(color_list, ~ rev(.))
  
  dataframe_list <- map(final_list, ~ mutate(., col_taxa = factor(col_taxa, levels = col_taxa)))
  names_list <- names(dataframe_list)
  
  n_col_leg <- map_vec(dataframe_list, ~length(unique(.[["col_taxa"]])))
  n_col <- ifelse(n_col_leg >= 10, 2, 1)
  
  plot_list <- pmap(list(dataframe_list, color_list, names_list, n_col), ~ {
    ggplot(..1, aes(x = "", y = Prop, fill = fct_inorder(col_taxa))) +
      geom_bar(stat = "identity", width = 1, color = "gray") +
      coord_polar("y", start = 0, direction = -1) +
      geom_text(
        size = 2, aes(label = paste(round(Prop, 1), "%"), x = 1.3),
        position = position_stack(vjust = 0.5)
      ) +
      guides(fill = guide_legend(ncol = ..4))+
      scale_fill_manual(values = ..2) +
      labs(x = NULL, y = NULL, fill = ..3) +
      theme_classic() +
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#74add1"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7)
      )
  })
  
  plot <- patchwork::wrap_plots(plot_list) + patchwork::plot_layout(ncol = 2)
  
  ggsave(
    filename = paste0("pie_", taxa_id, "_by_", taxa_grupo, ".png"),
    plot,
    width = width,
    height = height ,
    dpi = 300
  )
}
#' @title fn_plot_nmds
#' @description función para graficar la composición de especies en un NMDS.
#' @param data archivo entrada que tiene que tener, al menos, las columnas que representen al taxón, grupo taxonómico, taxones y N. Tiene que estar en formato "long". Se recomida usar csv o tsv como formatos.
#' @param col_sitio cadena de texto que indica el nombre de la columna que tiene los sitios de muestreo en la base de datos de entrada.
#' @param col_taxa cadena de texto que indica el nombre de la columna que contiene los taxones (generalmente se llama especie, taxa, sigla) en la base de datos de entrada.
#' @param col_N cadena de texto que indica el nombre de la columna que contiene el N (abundancia) en la base de datos de entrada.
#' @param col_factor cadena de texto que indica el nombre de la columna que contiene al factor de agrupamiento (zonas, campañas, etc) en la base de datos de entrada. Por defecto, Nulo.
#' @param col_replica cadena de texto que indica el nombre de la columna que contiene las réplicas (zonas, campañas, etc) en la base de datos de entrada. Por defecto, Nulo.
#' @param dist cadena de texto que indica la distancia a usar para el calculo de la disimilitud en el NMDS ("euc", "bray"). Por defecto, toma valor "bray".
#' @param taxa_id cadena de texto que indica el nombre del "grupo funcional" al cual pertenecen los taxones ("fitoplancton", "zooplancton", "macrofitas", "perifiton", "ictiofauna",etc.).
#' @param title_factor cadena de texto que indica el nombre que se prefiere en la leyenda y subtititulo del gráfico. Por defecto, NULL.
#' @param ord_factor cadena de texto qeu indica el orden en el cual se quiere que aprazcan los niveles del factor en el leyenda.
#' @param height alto del gráfico. Por defecto, toma valor 6.
#' @param width ancho del gráfico. Por defecto, toma valor 7.
#' @import tidyverse
#' @import RColorBrewer
#' @import vegan
#' @return gráfico NMDS para la composición de especies.
#' @export fn_plot_nmds
#' @examples
#' \dontrun{
#' # plot sin factor de agrupamiento
#' data <- read_tsv("data_fitoplancton.tsv")
#' col_sitio <- "Sitio"
#' col_taxa <- "Sigla"
#' col_N <- "N"
#' dist <- "euc"
#' taxa_id <-  "fitoplancton"
#' height <- 6
#' width <- 6
#' fn_plot_nmds(data = data,col_sitio = col_sitio, col_taxa = col_taxa, col_N = col_N, dist = dist, taxa_id = taxa_id, height = height, width = width)
#' # plot con factor de agrupamiento: hace permanova y colorea elipses#
#' # fake data
#' data <- read_tsv("data_fitoplancton.tsv")
#' data <- data %>% mutate(grupo = if_else(Sitio %in% c("P-1", "P-2", "P3"), "area1", "area2"))
#' col_sitio <- "Sitio"
#' col_taxa <- "Sigla"
#' col_N <- "N"
#' col_factor <- "grupo"
#' dist <- "euc"
#' taxa_id <-  "fitoplancton"
#' height <- 6
#' width <- 6
#' title_factor <- "Area"
#' fn_plot_nmds(data = data,col_sitio = col_sitio, col_taxa = col_taxa,col_factor = col_factor, col_N = col_N, dist = dist, taxa_id = taxa_id, height = height, width = width,title_factor = title_factor)
#' # plot con factor de agrupamiento: hace permanova y colorea elipses, con orden en la leyenda según orden preferido y con réplica#
#' # fake data
#' data <- read_tsv("data_fitoplancton.tsv")
#' data <- data %>% mutate(grupo = if_else(Sitio %in% c("P-1", "P-2", "P3"), "area1", "area2"))
#' col_sitio <- "Sitio"
#' col_taxa <- "Sigla"
#' col_N <- "N"
#' col_factor <- "grupo"
#' col_replica <- "Replica"
#' dist <- "euc"
#' taxa_id <-  "fitoplancton"
#' title_factor <- "Area"
#' ord_factor <- c("area2", "area1")
#' height <- 6
#' width <- 6
#' fn_plot_nmds(data = data,col_sitio = col_sitio, col_taxa = col_taxa,col_factor = col_factor, col_N = col_N,col_replica = col_replica,ord_factor = ord_factor, dist = dist, taxa_id = taxa_id, height = height, width = width,title_factor = title_factor)
#' }
fn_plot_nmds <- function(data, col_sitio, col_taxa, col_N, dist = "bray", col_replica = NULL, col_factor = NULL, ord_factor = NULL, taxa_id, height = 6, width = 7, title_factor = NULL){
  options(scipen = 999)
  # setting dataframe base, with out replica neither factor#
  vars <- c(col_sitio, col_taxa, col_N)
  data_plot <- data %>% 
    dplyr::select(all_of(vars)) %>% 
    dplyr::rename_at(vars, ~  c( "col_sitio", "col_taxa", "col_N"))
  if (str_detect(taxa_id , "(?i)macrofita")) {
    data_plot <- data_plot %>%
      dplyr::mutate(
        col_N = dplyr::case_when(
          stringr::str_detect(col_N, "1") ~ 3,
          stringr::str_detect(col_N, "\\+") ~ 2,
          stringr::str_detect(col_N, "r") ~ 1,
          stringr::str_detect(col_N, "2") ~ 4,
          stringr::str_detect(col_N, "3") ~ 5,
          stringr::str_detect(col_N, "4") ~ 6,
          stringr::str_detect(col_N, "5") ~ 7,
          TRUE ~ as.numeric(col_N)))
  } else {
    data_plot <- data_plot
  }
  if (!is.null(col_replica)) {
    col_replica <- data %>% dplyr::pull({{col_replica}})
    data_plot <- data_plot %>% dplyr::mutate(col_replica = col_replica)
  } else {
    data_plot <- data_plot
  }
  if (!is.null(col_factor)) {
    col_factor <- data %>% dplyr::pull({{col_factor}})
    data_plot <- data_plot %>% dplyr::mutate(col_factor = col_factor)
  } else {
    data_plot <- data_plot
  }
  #plot nmds empty: Quizas se pueda agregar cluster de estaciones#
  if (is.null(col_replica) && is.null(col_factor)) {
    labels <- data_plot %>% 
      dplyr::select(col_sitio) %>% 
      dplyr::distinct()
    data_nmds <-  data_plot %>%
      dplyr::group_by(col_sitio, col_taxa) %>%
      dplyr::summarise(col_N = sum(col_N, na.rm = TRUE)) %>%
      tidyr::pivot_wider(names_from = col_taxa, values_from = col_N, values_fill = 0) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(col_sitio)) %>% 
      as.data.frame()
    data_nmds <- bind_cols(labels, data_nmds) %>% na.omit()
    begin <- ncol(labels) + 1
    NMDS1 <- metaMDS(data_nmds[, begin:ncol(data_nmds)], k = 2, trymax = 5000, distance = dist)
    sco_sites <- scores(NMDS1)[[1]]
    data_nmds <- bind_cols(sco_sites, data_nmds)
    png(filename = paste0("NMDS_", taxa_id, ".png"), width = width,height = height, units = "in", res = 300,family = "Arial")
    ordiplot(NMDS1,type = "n",
             main = paste0("NMDS de ", str_to_sentence(taxa_id)))
    orditorp(NMDS1, 
             display = "sites", 
             label = data_nmds[["col_sitio"]], 
             cex = 0.5, 
             air = 0.005)
    orditorp(NMDS1,
             display = "species",
             col = "gray",
             cex = 0.55,
             air = -1,
             font = 3)
    abline(h = 0,
           v = 0,
           lty = 3,
           col = "gray0")
    legend("bottomleft", paste0("Stress = ", round(NMDS1$stress, 3)), bty = "n", cex = 0.5, text.font = 3)
    dev.off()
  }
  if (is.null(col_replica) && !is.null(col_factor)) {
    if (is.null(ord_factor)) {
      ord <- data_plot %>% 
        dplyr::pull(col_factor) %>% 
        unique() %>% 
        sort()
      } else {
        ord <- ord_factor
        }
    labels <- data_plot %>% 
      dplyr::select(col_sitio, col_factor) %>% 
      dplyr::distinct()
    data_nmds <-  data_plot %>%
      dplyr::group_by(col_sitio,col_taxa, col_factor) %>%
      dplyr::summarise(col_N = sum(col_N, na.rm = TRUE)) %>%
      tidyr::pivot_wider(names_from = col_taxa, values_from = col_N, values_fill = 0) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(col_sitio, col_factor)) %>% 
      as.data.frame()# %>%  
      #    #dplyr::select(which(colSums(.) / sum(.) > 0.01)) 
    data_nmds <- dplyr::bind_cols(labels, data_nmds) %>% na.omit()
    begin <- ncol(labels) + 1
    NMDS1 <- metaMDS(data_nmds[, begin:ncol(data_nmds)], k = 2, trymax = 5000, distance = dist)
    sco_sites <- scores(NMDS1)[[1]]
    data_nmds <- dplyr::bind_cols(sco_sites, data_nmds)
    formula <- paste("sco_sites", "~", "col_factor")
    list_test <- purrr::map(formula, ~adonis2(as.formula(.), data = data_nmds, method = "euc"))
    names(list_test) <- title_factor
    sig_factor <- dplyr::bind_rows(list_test,.id = "factor") %>% 
      dplyr::rename(p = ncol(.))
    factor <- sig_factor$factor[1]
    color <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(length(unique(data_nmds$col_factor)))
    names(color) <- ord
    png(filename = paste0("NMDS_", taxa_id, ".png"), width = width,height = height,units = "in",
            res = 300, family = "Arial")
    ordiplot(NMDS1,type = "n", main = paste0("NMDS ",str_to_sentence(taxa_id), " (PERMANOVA R² = ", round(sig_factor$R2[1], 2),"; P = ", round(sig_factor$p[1],3),")"), cex.main = 0.9)
    mtext(paste0("Factor Permanova: ", title_factor), side = 3, line = -4, cex = 0.9, outer = TRUE)
    orditorp(NMDS1, 
                 display = "sites", 
                 label = data_nmds[["col_sitio"]], 
                 cex = 0.5, 
                 air = 0.005)
    ordiellipse(NMDS1,
                groups = data_nmds[["col_factor"]],
                kind = "sd", 
                draw = "polygon", 
                col = color[names(color)],
                lwd = 0.3,
                label = F,
                cex = 0.6,
                font = 3,
                alpha = 0.2)
    orditorp(NMDS1,
             display = "species",
             col = "gray",
             cex = 0.55,
             air = -1,
             font = 3)
    abline(h = 0,
           v = 0,
           lty = 3,
           col = "gray0")
    legend("bottomleft", paste0("Stress = ", round(NMDS1$stress, 3)), bty = "n", cex = 0.5, text.font = 3)
    legend('topright', title = title_factor, legend = names(color), col = color, pch = 16)
    dev.off()
  }
  if (!is.null(col_replica) && !is.null(col_factor)) {
    if (is.null(ord_factor)) {
      ord <- data_plot %>% 
        dplyr::pull(col_factor) %>% 
        unique() %>% 
        sort()
      } else {
        ord <- ord_factor
        }
    labels <- data_plot %>% 
      dplyr::select(col_sitio, col_factor, col_replica) %>% 
      dplyr::distinct()
    data_nmds <-  data_plot %>%
      dplyr::group_by(col_sitio,col_taxa, col_factor, col_replica) %>%
      dplyr::summarise(col_N = sum(col_N, na.rm = TRUE)) %>%
      tidyr::pivot_wider(names_from = col_taxa, values_from = col_N, values_fill = 0) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(col_sitio, col_factor, col_replica)) %>% 
      as.data.frame()# %>%  
    #    #dplyr::select(which(colSums(.) / sum(.) > 0.01)) 
    data_nmds <- bind_cols(labels, data_nmds) %>% na.omit()
    begin <- ncol(labels) + 1
    NMDS1 <- metaMDS(data_nmds[, begin:ncol(data_nmds)], k = 2, trymax = 5000, distance = dist)
    sco_sites <- scores(NMDS1)[[1]]
    data_nmds <- bind_cols(sco_sites, data_nmds)
    formula <- paste("sco_sites", "~", "col_factor")
    list_test <- purrr::map(formula, ~adonis2(as.formula(.), data = data_nmds, method = "euc"))
    names(list_test) <- title_factor
    sig_factor <- bind_rows(list_test,.id = "factor") %>% 
      dplyr::rename(p = ncol(.))
    factor <- sig_factor$factor[1]
    color <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(length(unique(data_nmds$col_factor)))
    names(color) <- ord
    png(filename = paste0("NMDS_", taxa_id, ".png"), width = width,height = height,units = "in",
        res = 300, family = "Arial")
    ordiplot(NMDS1,type = "n", main = paste0("NMDS ",str_to_sentence(taxa_id), " (PERMANOVA R² = ", round(sig_factor$R2[1], 2),"; P = ", round(sig_factor$p[1],3),")"), cex.main = 0.9)
    mtext(paste0("Factor Permanova: ", title_factor), side = 3, line = -4, cex = 0.9, outer = TRUE)
    orditorp(NMDS1, 
             display = "sites", 
             label = paste0(data_nmds[["col_sitio"]], " — ", data_nmds[["col_replica"]]), 
             cex = 0.5, 
             air = 0.005)
    ordiellipse(NMDS1,
                groups = data_nmds[["col_factor"]],
                kind = "sd", 
                draw = "polygon", 
                col = color[names(color)],
                lwd = 0.3,
                label = F,
                cex = 0.6,
                font = 3,
                alpha = 0.2)
    orditorp(NMDS1,
             display = "species",
             col = "gray",
             cex = 0.55,
             air = -1,
             font = 3)
    abline(h = 0,
           v = 0,
           lty = 3,
           col = "gray0")
    legend("bottomleft", paste0("Stress = ", round(NMDS1$stress, 3)), bty = "n", cex = 0.5, text.font = 3)
    legend('topright', title = title_factor, legend = names(color), col = color, pch = 16)
    dev.off()
  }
}  
  
  
  
  
  
  
  
