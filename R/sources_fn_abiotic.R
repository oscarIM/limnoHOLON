##### funciones para medio abiótico abiótico####
#' @title fn_stats
#' @description función para obtener la tabla de estadística descriptiva.
#' @param data archivo de entrada. Tiene que tener, al menos, las columnas: sitio, parámetros (nombre o sigla), valor de parámetros. Tiene que estar en formato "long". Se recomida usar csv o tsv como formatos.
#' @param col_pars string que indica el nombre de la columna en data que tiene los parámetros (su sigla).
#' @param data_pars "dataframe" que contenga los parámetros (sus siglas) tal como aparecen en el "dataframe" de entrada, junto a un columna que indique a que tipo de parámetro pertenece (i.e. Físico, Químico, Nutriente, Metal, Orgánico e inorgánico y Biológico).Las columnas se tienen que llamar: Param y cat_pars
#' @param col_valor string que indica el nombre de la columna que tiene el valor de los parámetros.
#' @param matriz string que indica el nombre de la "matriz" de datos que se esta analizando. Sirve principalmente como ID para las salidas y para diferenciar entre sedimentos y aguas.
#' @param round  integro de determina el número de decimales para redondeo en la tabla. Por defecto, round =2.
#' @import tidyverse
#' @import scales
#' @import knitr
#' @return tabla básica estadisticos
#' @export fn_stats
#' @examples
#' \dontrun{
#' data <- readr::read_tsv("data_agua_RDLP_RDLP.tsv")
#' col_pars <- "Param"
#' data_pars <- readr::read_tsv("tabla_pars_master.tsv")
#' col_valor <- "Resultado"
#' matriz <- "agua"
#' round <- 4
#'fn_stats(data = data,col_pars = col_pars,col_valor = col_valor,data_pars = data_pars,matriz = matriz,round = round)
#' }
fn_stats <- function(data, col_pars, col_valor, data_pars, matriz, round = 2) {
  options(scipen = 999)
  # setting vars and aux functions#
  `%!in%` <- Negate(`%in%`)
  vars  <- c(col_pars, col_valor)
  data_plot <- data %>% select(all_of(vars))
  pars_gran <- c("LIM", "AMF", "AF", "AM", "AG", "AMG", "GRAN")
  patterns <- c("(?i)param", "(?i)resultado|(?i)valor")
  new_names <- c("col_pars", "col_valor")
  data_plot <- data_plot %>%
    dplyr::select(matches(patterns)) %>%
    dplyr::rename_at(vars(matches(patterns)), ~new_names)
  order_type <- c("Físico", "Químico", "Nutriente", "Metal", "Orgánicos e inorgánicos", "Biológico")
  if (matriz == "sedimento") {
    data_plot <- data_plot %>% dplyr::filter(col_pars %!in% pars_gran)
  } else {
    data_plot <- data_plot
  }
  summ_pars <- data_plot %>%
    dplyr::group_by(col_pars) %>%
    dplyr::summarise(
      Nobs = n(),
      min = min(col_valor),
      max = max(col_valor),
      prom = mean(col_valor),
      desvest = sd(col_valor),
      cv_num = desvest / prom,
      "cv%" = scales::label_percent()(desvest / prom)
    ) %>%
    dplyr::rename(Sigla = col_pars) %>%
    dplyr::mutate_at(vars(3:6), list(~ round(., round)))
  table_export <- summ_pars %>% dplyr::select(-cv_num)
  table_export <- dplyr::left_join(table_export, data_pars, by = c("Sigla" = "Param"))
  table_export <- table_export %>%
    slice(order(factor(cats_pars, levels = order_type))) %>%
    select(-c(nombre_par, cats_pars))
  print(knitr::kable(table_export, format = "markdown", align = "c", caption = "Tabla parámetros"))
  write.table(table_export, file = paste0("descript_stats_table_", matriz, ".csv"), sep = ";", na = "NA", dec = ",", row.names = F, col.names = T)
}
#' @title fn_plot_bar_abiotic
#' @description función para graficar las concentraciones o niveles de los parámetros a los largo de las estaciones de muestro, coloreadolos por tipo (físicos, químicos, nutrientes, metales, orgánico e inorgánico y biológico; en este mismo orden)
#' @param data archivo entrada que tiene que tener, al menos, las columnas: sitio, parámetros (nombre o sigla), valor de parámetros. Tiene que estar en formato "long". Se recomida usar csv o tsv como formatos.
#' @param col_pars string que indica el nombre de la columna en data que tiene los parámetros (su sigla).
#' @param col_sitio string que indica el nombre de la columna que tiene los sitios de muestreo.
#' @param col_valor string que indica el nombre de la columna que tiene el valor de los parámetros.
#' @param code_sitio string que indica el código (generalmente una letra seguida un guión) que utilizada para nombras las estaciones (eg. "E-", "P-", "D-"). Se esperan puntos de muestreos asignados de forma consecutiva y sin gaps.
#' @param matriz string que indica el nombre de la "matriz" de datos que se esta analizando. Sirve principalmente como ID para las salidas y para diferenciar entre sedimentos y aguas.
#' @param data_pars "dataframe" que contenga los parámetros (sus siglas) tal como aparecen en el "dataframe" de entrada, junto a una columna que indique a que tipo de parámetro pertenece (i.e. Físico, Químico, Nutriente, Metal, Orgánico e inorgánico y Biológico).Las columnas se tienen que llamar: Param y cat_pars
#' @param ord_sitio string que indica el orden en que se quiere que aparezcan los sitios en el gráfico (de forma ascendente o descendente de acuerdo al número de estación o punto de muestreo). Por defecto, toma valor "asc" (órden ascendente)
#' @param col_grupo string que indica el nombre del de la columna que contiene una variable de agrupamiento para el plot (e.g. zonas, campañas, etc.). Por defecto, Nulo.
#' @param ord_grupo string que indica el orden en el que se quiera que aparezan en las leyendas y/o ejes los iteml del grupo. Solo tiene sentido si se usa col_grupo. Por defecto, Nulo.
#' @param width ancho del gráfico. Por defecto, toma valor 8
#' @param height alto del gráfico. Por defecto, toma valor 6
#' @param aspect_ratio relación alto-ancho. Por defecto, toma valor 1.
#' @import rlang
#' @import tidyverse
#' @import RColorBrewer
#' @import grDevices
#' @return gráfico de barras por tipo de parámetros
#' @export fn_plot_bar_abiotic
#' @examples
#' \dontrun{
#' Plot sin grupos
#' data <- readr::read_tsv("data_agua_RDLP.tsv")
#' col_pars <- "Param"
#' col_sitio <- "Sitio"
#' col_valor <- "Resultado"
#' col_unidad <- "Unidad"
#' matriz <- "agua"
#' code_sitio <- "P-"
#' ord_sitio <- "asc"# "desc o asc"
#' data_pars <- readr::read_tsv("tabla_pars_master.tsv")
#' width <- 8
#' height <- 11
#' fn_plot_bar_abiotic(data = data,col_pars = col_pars,col_sitio = col_sitio,col_valor = col_valor,aspect_ratio = 1,col_unidad = col_unidad,code_sitio = code_sitio,matriz = matriz,data_pars = data_pars,ord_sitio = ord_sitio,width = width,height = height)
#' # con 1 grupo
#' data <- readr::read_tsv("data_agua_VEN.tsv")
#' col_pars <- "Param"
#' col_sitio <- "Sitio"
#' col_valor <- "Valor"
#' col_unidad <- "Unidad"
#' matriz <- "agua"
#' code_sitio <- "P-"
#' ord_sitio <- "asc"# "desc o asc"
#' data_pars <- readr::read_tsv("tabla_pars_master.tsv")
#' col_grupo <- "zonas"
#' width <- 9
#' height <- 8
#' fn_plot_bar_abiotic(data = data,col_pars = col_pars,col_sitio = col_sitio,col_valor = col_valor,col_grupo = col_grupo,aspect_ratio = 1,col_unidad = col_unidad,code_sitio = code_sitio,matriz = matriz,data_pars = data_pars,ord_sitio = ord_sitio,width = width,height = height)
#' }

fn_plot_bar_abiotic <- function(data, col_pars, col_sitio, col_valor, col_grupo = NULL, col_unidad, code_sitio, matriz, data_pars, ord_sitio = "asc", width = 8, height = 6, ord_grupo = NULL, aspect_ratio = 1) {
  options(scipen = 999)
  # setting vars#
  pars_gran <- c("LIM", "AMF", "AF", "AM", "AG", "AMG", "GRAN")
  vars <- c(col_sitio, col_pars, col_unidad, col_valor)
  data_plot <- data %>%
    dplyr::select(all_of(vars)) %>%
    dplyr::rename_at(vars, ~c("col_sitio", "col_pars", "col_unidad", "col_valor"))
  if (!is.null(col_grupo) && length(col_grupo) == 1) {
    grupo <- data %>% dplyr::pull({{ col_grupo }})
    data_plot <- data_plot %>% dplyr::mutate(col_grupo = grupo)
  }
  if (!is.null(col_grupo) && length(col_grupo) == 2) {
    grupos <- data %>%
      dplyr::select(all_of(col_grupo)) %>%
      dplyr::rename(col_grupo1 = 1, col_grupo2 = 2)
    data_plot <- dplur::bind_cols(data_plot, grupos)
  }
  cols_type <- c("#66C2A5", "#C6B18B", "#D58EC4", "#F8D348", "#A89BB0", "#B7D84C")
  order_type <- c("Físico", "Químico", "Nutriente", "Metal", "Orgánicos e inorgánicos", "Biológico")
  names(cols_type) <- order_type
  # selection vars#
  selected_pars <- data_plot %>%
    dplyr::group_by(col_pars) %>%
    dplyr::summarise(
      prom = mean(col_valor),
      desvest = sd(col_valor),
      cv_num = desvest / prom
    ) %>%
    dplyr::filter(cv_num > 0) %>%
    dplyr::pull(col_pars)
  if (stringr::str_detect(string = matriz, pattern = "(?i)sedimento?")) {
    selected_pars <- setdiff(selected_pars, pars_gran)
  }
  data_plot <- data_plot %>%
    dplyr::filter(col_pars %in% selected_pars)
  data_plot <- dplyr::left_join(data_plot, data_pars, by = c("col_pars" = "Param"))
  sitios_tmp <- stringr::str_extract_all(data_plot$col_sitio, "\\d+", simplify = T) %>%
    as.numeric() %>%
    unique() %>%
    sort()
  sitios_ord <- dplyr::case_when(
    ord_sitio == "asc" ~ paste0(code_sitio, sitios_tmp),
    ord_sitio == "desc" ~ paste0(code_sitio, rev(sitios_tmp)),
    TRUE ~ NA_character_
  )
  if (length(sitios_ord) <= 10) {
    angle <- 0
    } else {
      angle <- 90
      }
  if (is.null(col_grupo)) {
    data_plot <- data_plot %>%
      dplyr::mutate(
        col_sitio = factor(col_sitio, levels = sitios_ord),
        label = paste0(col_pars, " (", col_unidad, ")"),
        cats_pars = factor(cats_pars, levels = order_type)
      ) %>%
      dplyr::mutate(label = stringr::str_replace(label, "pH \\(-\\)", replacement = "pH"))
    plot <- ggplot2::ggplot(data_plot, aes(x = col_sitio, y = col_valor, fill = cats_pars)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~ reorder(label, match(cats_pars, order_type)),
                 scales = "free_y",
                 ncol = 2,
                 strip.position = "left"
      ) +
      # facet_grid(taxa_grupo ~ zona, scales = "free", switch = "y")
      scale_fill_manual(values = cols_type) +
      scale_y_continuous(
        breaks = waiver(),
        n.breaks = 5
      ) +
      labs(
        x = "Estación",
        y = "Valor parámetro",
        fill = " Tipo parámetro"
      ) +
      theme_bw() +
      theme(
        text = element_text(size = 10, family = "Arial"),
        strip.text = element_text(size = 6, family = "Arial"),
        axis.text.x = element_text(angle = angle,
                                       hjust = 1,
                                       vjust = 0.5)

      )
    ggsave(filename = paste0("bar_pars_", ord_sitio, "_", matriz, ".png"), plot = plot, width = width, height = height, dpi = 300)
  }
  if (!is.null(col_grupo) && length(col_grupo) == 1) {
    if (!is.null(ord_grupo)) {
      order_grupo <- ord_grupo
    } else {
      order_grupo <- data_plot %>%
        dplyr::pull(col_grupo) %>%
        unique() %>%
        sort()
    }
    data_plot <- data_plot %>%
      mutate(
        col_sitio = factor(col_sitio, levels = sitios_ord),
        label = paste0(col_pars, " (", col_unidad, ")"),
        cats_pars = factor(cats_pars, levels = order_type),
        col_grupo = factor(col_grupo, levels = order_grupo)
      ) %>%
      mutate(label = stringr::str_replace(label, "pH \\(-\\)", replacement = "pH"))
    data_plot <- data_plot %>%
      arrange(cats_pars) %>%
      mutate(label = factor(label, levels = unique(label)))
    fn_plot_aux <- function(data) {
      plot <- ggplot(data, aes(x = col_sitio, y = col_valor, fill = cats_pars)) +
        geom_bar(stat = "identity", position = "dodge", show.legend = F) +
        facet_grid(label ~ col_grupo, scales = "free", switch = "y") +
        scale_fill_manual(values = cols_type) +
        scale_y_continuous(breaks = waiver(), n.breaks = 5) +
        labs(
          x = "Estación",
          y = "Valor parámetro"
          # fill = "Tipo parámetro"
        ) +
        theme_bw() +
        theme(
          text = element_text(size = 10, family = "Arial"),
          axis.text.x = element_text(angle = angle, vjust = 0.5, hjust = 1),
          aspect.ratio = aspect_ratio
        )

      ggsave(filename = paste0("bar_pars_", unique(data$cats_pars), "_", col_grupo, "_", ord_sitio, "_", matriz, ".png"), plot = plot, width = width, height = height, dpi = 300)
    }
    df_list <- data_plot %>%
      dplyr::group_by(cats_pars) %>%
      dplyr::group_split() %>%
      setNames(purrr::map(., ~ paste0(unique(.[["cats_pars"]]))))
    purrr::walk(df_list, ~ fn_plot_aux(.))
  }
  if (!is.null(col_grupo) && length(col_grupo) == 2) {
    data_plot <- data_plot %>%
      dplyr::mutate(
        col_sitio = factor(col_sitio, levels = sitios_ord),
        label = paste0(col_pars, " (", col_unidad, ")"),
        cats_pars = factor(cats_pars, levels = order_type),
        col_grupo1 = factor(col_grupo1, levels = sort(unique(col_grupo1))),
        col_grupo2 = factor(col_grupo2, levels = sort(unique(col_grupo2)))
      ) %>%
      dplyr::mutate(label = stringr::str_replace(label, "pH \\(-\\)", replacement = "pH"))
    data_plot <- data_plot %>%
      dplyr::arrange(cats_pars) %>%
      mutate(label = factor(label, levels = unique(label)))
    df_list <- data_plot %>%
      dplyr::group_by(cats_pars) %>%
      dplyr::group_split() %>%
      setNames(purrr::map(., ~ paste0(unique(.[["cats_pars"]]))))
    all_palettes <- as.data.frame(RColorBrewer::brewer.pal.info) %>%
      dplyr::filter(category == "div", colorblind == "TRUE") %>%
      dplyr::mutate(name_pal = row.names(.)) %>%
      dplyr::filter(name_pal != "Greys") %>%
      dplyr::mutate(to_pars = order_type)
    fn_plot_aux <- function(data) {
      col <- intersect(names(cols_type), unique(data$cats_pars))
      col <- cols_type[col]
      plot <- ggplot(data, aes(x = col_sitio, y = col_valor)) +
        geom_bar(stat = "identity", position = "dodge", fill = col) +
        facet_grid(label ~ col_grupo2 + col_grupo1, scales = "free_y", switch = "y") +
        # scale_fill_manual(values = cols_pars) +
        scale_y_continuous(breaks = waiver(), n.breaks = 5) +
        labs(
          x = "Estación",
          y = "Valor parámetro",
          fill = "Parámetro"
        ) +
        theme_bw() +
        theme(
          text = element_text(size = 10, family = "Arial"),
          aspect.ratio = aspect_ratio,
          axis.text.x = element_text(angle = angle,
                                       hjust = 1,
                                       vjust = 0.5)
        )
      ggsave(filename = paste0("bar_pars_both_gr_", ord_sitio, "_", matriz, "_", unique(data$cats_pars), ".png"), plot = plot, width = width, height = height, dpi = 300)
    }
    purrr::walk(df_list, ~ fn_plot_aux(.))
  }
}
####################################################################################################
#' @title fn_plot_correlogram
#' @description función para graficar el correlograma de las variables.
#' @param data archivo entrada que tiene que tener, al menos, las columnas: sitio, parámetros (nombre o sigla), valor de parámetros. Tiene que estar en formato "long". Se recomida usar csv o tsv como formatos.
#' @param col_pars string que indica el nombre de la columna en data que tiene los parámetros (su sigla).
#' @param col_sitio string que indica el nombre de la columna que tiene los sitios de muestreo.
#' @param col_valor string que indica el nombre de la columna que tiene el valor de los parámetros.
#' @param code_sitio string que indica el código (generalmente una letra seguida un guión) que utilizada para nombras las estaciones (eg. "E-", "P-", "D-"). Se esperan puntos de muestreos asignados de forma consecutiva y sin gaps.
#' @param matriz string que indica el nombre de la "matriz" de datos que se esta analizando. Sirve principalmente como ID para las salidas y para diferenciar entre sedimentos y aguas.
#' @param data_pars "dataframe" que contenga los parámetros (sus siglas) tal como aparecen en el "dataframe" de entrada, junto a un columna que indique a que tipo de parámetro pertenece (i.e. Físico, Químico, Nutriente, Metal, Orgánico e inorgánico y Biológico).Las columnas se tienen que llamar: Param y cat_pars
#' @param width ancho del gráfico. Por defecto, toma valor 6
#' @param height alto del gráfico. Por defecto, toma valor 6
#' @import corrplot
#' @import tidyverse
#' @import rstatix
#' @return gráfico de correlaciones de spearmann
#' @export fn_plot_correlogram
#' @examples
#' \dontrun{
#' data <- readr::read_tsv("data_sedimento (copia).tsv")
#' col_pars <- "Param"
#' col_sitio <- "Estacion"
#' col_valor <- "Valor"
#' matriz <- "sedimento"
#' code_sitio <- "E-"
#' data_pars <- readr::read_tsv("tabla_pars.tsv")
#' width <- 8
#' height <- 9
#' fn_plot_correlogram(data = data, col_pars = col_pars, col_sitio = col_sitio, matriz = matriz, code_sitio, code_sitio, data_pars = data_pars, width = width,height = width)
#' }
fn_plot_correlogram <- function(data, col_pars, col_sitio, matriz, code_sitio, data_pars, width = 6, height = 6) {
  # setting vars#
  pars_gran <- c("LIM", "AMF", "AF", "AM", "AG", "AMG", "GRAN")
  order_type <- c("Físico", "Químico", "Nutriente", "Metal", "Orgánicos e inorgánicos", "Biológico")
  data <- data %>%
    dplyr::select(all_of(vars)) %>%
    dplyr::rename_at(vars, ~c("col_sitio", "col_pars", "col_valor"))
  # setting base dataframe#
  selected_pars <- data %>%
    dplyr::group_by(col_pars) %>%
    dplyr::summarise(
      prom = mean(col_valor),
      desvest = sd(col_valor),
      cv_num = desvest / prom
    ) %>%
    dplyr::filter(cv_num > 0) %>%
    dplyr::pull(col_pars)
  if (stringr::str_detect(string = matriz, pattern = "(?i)sedimento?")) {
    selected_pars <- setdiff(selected_pars, pars_gran)
  }
  data_plot <- dplyr::left_join(data, data_pars, by = c("col_pars" = "Param")) %>%
    dplyr::slice(order(factor(cats_pars, levels = order_type))) %>%
    dplyr::filter(col_pars %in% selected_pars) %>%
    dplyr::select(c(col_pars, col_valor, col_sitio)) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = col_pars, values_from = col_valor) %>%
    dplyr::select(-c(col_sitio)) %>%
    as.data.frame() # %>%
  n_sitios <- data %>%
    distinct(col_sitio) %>%
    nrow()
  if (n_sitios <= 4) {
    warning(paste0("¡Atención!, hay solo n = ", n_sitios, " puntos. Interpretar coeficientes de correlación de spearman y p-values con cuidado.\n"))
  }
  sitios_tmp <- str_extract_all(data$col_sitio, "\\d+", simplify = T) %>%
    as.numeric() %>%
    unique()
  rownames(data_plot) <- paste0(code_sitio, sitios_tmp)
  # correlation r matrix#
  coor_r <- rstatix::cor_mat(data_plot,
                             method = "spearman",
                             alternative = "two.sided",
                             conf.level = 0.95
  ) %>%
    select(-rowname) %>%
    as.matrix()
  rownames(coor_r) <- colnames(coor_r)
  # correlation p-values matrix#
  coor_p <- rstatix::cor_pmat(data_plot,
                              method = "spearman",
                              alternative = "two.sided",
                              conf.level = 0.95
  ) %>%
    select(-rowname) %>%
    as.matrix()
  rownames(coor_p) <- colnames(coor_p)
  if (any(coor_p < 0.05)) {
    png(paste0("fig_correlograma_", matriz, ".png"),
        width = width, height = width, units = "in", res = 300,
        family = "Arial"
    )
    corrplot::corrplot(coor_r,
                       type = "lower",
                       order = "original",
                       p.mat = coor_p,
                       sig.level = 0.05,
                       insig = "blank",
                       method = "circle",
                       diag = FALSE,
                       addCoef.col = "black",
                       number.cex = 0.7,
                       tl.col = "black",
                       tl.srt = 0,
                       tl.cex = 0.7
    )
    dev.off()
  } else {
    png(paste0("fig_correlograma_", matriz, ".png"), width = width, height = width, units = "in", res = 300)
    corrplot::corrplot(coor_r,
                       type = "lower",
                       order = "original",
                       p.mat = coor_p,
                       sig.level = 0.05,
                       insig = "pch",
                       pch = 1,
                       pch.cex = 4,
                       method = "circle",
                       diag = FALSE,
                       addCoef.col = "black",
                       number.cex = 0.7,
                       tl.col = "black",
                       tl.srt = 0,
                       tl.cex = 0.7
    )
    dev.off()
  }
}
#' @title fn_plot_pca
#' @description función para graficar resultados de análisis de PCA.
#' @param data archivo entrada que tiene que tener, al menos, las columnas: sitio, parámetros (nombre o sigla), valor de parámetros. Tiene que estar en formato "long". Se recomida usar csv o tsv como formatos.
#' @param col_pars string que indica el nombre de la columna en data que tiene los parámetros (su sigla).
#' @param col_sitio string que indica el nombre de la columna que tiene los sitios de muestreo.
#' @param col_valor string que indica el nombre de la columna que tiene el valor de los parámetros.
#' @param code_sitio string que indica el código (generalmente una letra seguida un guión) que utilizada para nombras las estaciones (eg. "E-", "P-", "D-"). Se esperan puntos de muestreos asignados de forma consecutiva y sin gaps.
#' @param matriz string que indica el nombre de la "matriz" de datos que se esta analizando. Sirve principalmente como ID para las salidas y para diferenciar entre sedimentos y aguas.
#' @param data_pars "dataframe" que contenga los parámetros (sus siglas) tal como aparecen en el "dataframe" de entrada, junto a un columna que indique a que tipo de parámetro pertenece (i.e. Físico, Químico, Nutriente, Metal, Orgánico e inorgánico y Biológico).Las columnas se tienen que llamar: Param y cat_pars
#' @param col_grupo string que indica el nombre del de la columna que contiene una variable de agrupamiento para el plot (e.g. zonas, campañas, etc.). Por defecto, Nulo.
#' @param ord_grupo string que indica el orden en el que se quiera que aparezan en las leyendas y/o ejes los iteml del grupo. Solo tiene sentido si se usa col_grupo. Por defecto, Nulo.
#' @param width ancho del gráfico. Por defecto, toma valor 6
#' @param height alto del gráfico. Por defecto, toma valor 6
#' @import tidyverse
#' @import scales
#' @import vegan
#' @import RColorBrewer
#' @import grDevices
#' @return gráfico pca
#' @export fn_plot_pca
#' @examples
#' \dontrun{
#' #figura sin factor de agrupamiento
#' data <- readr::read_tsv("data_agua_RDLP.tsv")
#' col_pars <- "Param"
#' col_sitio <- "Sitio"
#' col_valor <- "Resultado"
#' matriz <- "agua"
#' data_pars <- readr::read_tsv("tabla_pars_master.tsv")
#' width <- 7
#' height <- 7
#' fn_plot_pca(data = data,col_pars = col_pars,col_sitio = col_sitio, col_valor = col_valor, data_pars = data_pars,width = width, height = height, matriz = matriz)
#' #figura con 1 factor de agrupamiento#
#' data <- readr::read_tsv("data_agua_VEN.tsv")
#' col_pars <- "Param"
#' col_sitio <- "Sitio"
#' col_valor <- "Valor"
#' matriz <- "agua"
#' code_sitio <- "P-"
#' data_pars <- readr::read_tsv("tabla_pars_master.tsv")
#' col_grupo <- "zonas"
#' ord_grupo <- c("Q5","Q3","Q2","Q1","L","Q4")
#' width <- 9
#' height <- 8
#' fn_plot_pca(data = data,col_pars = col_pars,col_sitio = col_sitio, col_valor = col_valor, data_pars = data_pars,width = width, height = height,matriz = matriz,col_grupo = col_grupo,ord_grupo = ord_grupo)
#' }
fn_plot_pca <- function(data, col_pars, col_sitio, col_valor, col_grupo = NULL, ord_grupo = NULL, matriz, data_pars, width = 6, height = 6) {
  # aux fn: stolen from ggbiplot#
  get_data_pca_plot <- function(pca_obj) {
    choices <- 1:2
    scale <- 1
    obs.scale <- 1 - scale
    var.scale <- scale
    varname.adjust <- 1.5
    nobs.factor <- sqrt(nrow(pca_obj$x) - 1)
    d <- pca_obj$sdev
    u <- sweep(pca_obj$x, 2, 1 / (d * nobs.factor), FUN = "*")
    v <- pca_obj$rotation
    # Scores
    choices <- pmin(choices, ncol(u))
    df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale, FUN = "*"))
    # Directions
    v <- sweep(v, 2, d^var.scale, FUN = "*")
    df.v <- as.data.frame(v[, choices])
    names(df.u) <- c("xvar", "yvar")
    names(df.v) <- names(df.u)
    df.u <- df.u * nobs.factor
    df.u <- as.data.frame(df.u)
    r <- sqrt(qchisq(0.69, df = 2)) * prod(colMeans(df.u^2))^(1 / 4)
    # Scale directions
    v.scale <- rowSums(v^2)
    df.v <- r * df.v / sqrt(max(v.scale))
    df.v$varname <- rownames(v)
    # Variables for text label placement
    df.v$angle <- with(df.v, (180 / pi) * atan(yvar / xvar))
    df.v$hjust <- with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
    return(list(scores = df.u, directions = df.v))
  }
  # pars setting#
  pars_gran <- c("LIM", "AMF", "AF", "AM", "AG", "AMG", "GRAN")
  order_type <- c("Físico", "Químico", "Nutriente", "Metal", "Orgánicos e inorgánicos", "Biológico")
  cols_type <- c("#66C2A5", "#C6B18B", "#D58EC4", "#F8D348", "#A89BB0", "#B7D84C")
  names(cols_type) <- order_type
  vars <- c(col_sitio, col_pars, col_valor)
  data_plot <- data %>% select(all_of(vars))
  patterns <- c("(?i)sitio|(?i)estacion", "(?i)param", "(?i)resultado|(?i)valor")
  new_names <- c("col_sitio", "col_pars", "col_valor")
  data_plot <- data_plot %>%
    dplyr::select(matches(patterns)) %>%
    dplyr::rename_at(vars(matches(patterns)), ~new_names)
  if (!is.null(col_grupo)) {
    grupo <- data %>% pull({{ col_grupo }})
    data_plot <- data_plot %>% mutate(col_grupo = grupo)
  }
  selected_pars <- data_plot %>%
    dplyr::group_by(col_pars) %>%
    dplyr::summarise(
      prom = mean(col_valor),
      desvest = sd(col_valor),
      cv_num = desvest / prom
    ) %>%
    dplyr::filter(cv_num > 0) %>%
    dplyr::pull(col_pars)
  if (stringr::str_detect(string = matriz, pattern = "(?i)sedimento?")) {
    selected_pars <- setdiff(selected_pars, pars_gran)
  } else {
    selected_pars <- selected_pars
  }
  data_pca <- data_plot %>%
    dplyr::filter(col_pars %in% selected_pars) %>%
    dplyr::left_join(., data_pars, by = c("col_pars" = "Param"))
  # begin pca plots
  if (is.null(col_grupo)) {
    to_pca <- data_pca %>%
      dplyr::select(col_pars, col_valor, col_sitio) %>%
      tidyr::pivot_wider(names_from = col_pars, values_from = col_valor)
    begin <- data_pca %>%
      dplyr::select(col_sitio) %>%
      ncol(.) + 1
    pca <- prcomp(to_pca[begin:ncol(to_pca)], scale = T)
    prop_pca <- pca$sdev^2 / sum(pca$sdev^2)
    tmp_data <- get_data_pca_plot(pca_obj = pca)
    directions <- tmp_data$directions
    directions <- left_join(directions, data_pars, by = c("varname" = "Param"))
    scores <- tmp_data$scores
    scores <- bind_cols(to_pca, scores)
    xmax <- ceiling(max(directions$xvar, scores$xvar)) + 0.5
    xmin <- floor(min(directions$xvar, scores$xvar)) - 0.5
    ymax <- ceiling(max(directions$yvar, scores$yvar)) + 0.5
    ymin <- floor(min(directions$yvar, scores$yvar)) - 0.5
    plot <- ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
      xlim(xmin + 0.25, xmax - 0.25) +
      ylim(ymin + 0.25, ymax - 0.25) +
      labs(
        x = paste0("Primera Componente Principal (", percent(prop_pca[1]), ")", sep = ""),
        y = paste0("Segunda Componente Principal (", percent(prop_pca[2]), ")", sep = "")
      ) +
      theme_bw() +
      theme(text = element_text(size = 10, family = "Arial")) +
      geom_segment(
        data = directions,
        mapping = aes(x = 0, y = 0, xend = xvar, yend = yvar, color = cats_pars),
        arrow = arrow(length = unit(1 / 2, "picas"))
      ) +
      geom_text(
        data = directions,
        mapping = aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust, color = cats_pars),
        size = 3, family = "Arial"
      ) +
      scale_color_manual(name = "Tipo parámetro", values = cols_type, breaks = names(cols_type)) +
      guides(color = guide_legend(title.position = "top", override.aes = list(label = " "))) +
      geom_point(data = scores, mapping = aes(x = xvar, y = yvar)) +
      geom_text(data = scores, mapping = aes(x = xvar, y = yvar, label = col_sitio), nudge_y = 0.1, family = "Arial")
    ggsave(filename = paste0("fig_pca_", matriz, ".png"), plot = plot, device = "png", width = width, height = height)
  } else {
    to_pca <- data_pca %>%
      dplyr::select(col_pars, col_valor, col_sitio, col_grupo) %>%
      tidyr::pivot_wider(names_from = col_pars, values_from = col_valor)
    grupo <- data %>% pull({{ col_grupo }})
    begin <- data_pca %>%
      dplyr::select(col_sitio, col_grupo) %>%
      ncol(.) + 1
    pca <- prcomp(to_pca[begin:ncol(to_pca)], scale = T)
    sco <- scores(pca)
    # to_pca$factor_col_grupo <- as.factor(to_pca[[col_grupo]])
    test <- adonis2(sco ~ as.factor(col_grupo), data = to_pca, method = "bray")
    prop_pca <- pca$sdev^2 / sum(pca$sdev^2)
    tmp_data <- get_data_pca_plot(pca_obj = pca)
    directions <- tmp_data$directions
    directions <- left_join(directions, data_pars, by = c("varname" = "Param"))
    scores <- tmp_data$scores
    scores <- bind_cols(to_pca, scores)
    # scores$gr <- grupos
    directions$gr <- "constante"
    # stat_ellipse((level = 0.68, geom = "polygon"))
    col_pca <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(unique(scores$col_grupo)))
    if (!is.null(ord_grupo)) {
      order_grupo <- ord_grupo
    } else {
      order_grupo <- scores %>%
        dplyr::pull(col_grupo) %>%
        unique() %>%
        sort()
    }
    names(col_pca) <- unique(order_grupo)
    ## cambiar esta mierda#
    scores$gr <- factor(scores$col_grupo, levels = order_grupo)
    plot <- ggplot(data = scores, mapping = aes(x = xvar, y = yvar, fill = gr, group = gr)) +
      stat_ellipse(level = 0.68, geom = "polygon") +
      # ggforce::geom_mark_ellipse(expand = unit(0.2,"mm"),linewidth = 0) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
      labs(
        x = paste0("Primera Componente Principal (", percent(prop_pca[1]), ")", sep = ""),
        y = paste0("Segunda Componente Principal (", percent(prop_pca[2]), ")", sep = ""),
        title = paste0("Permanova R² = ", round(test[[3]][1], 2), ", valor-p = ", round(test[[5]][1], 5)),
        subtitle = paste0("Factor Permanova : ", str_to_sentence(col_grupo))
      ) +
      theme_bw() +
      scale_fill_manual(name = str_to_sentence(col_grupo), values = alpha(col_pca, 0.4), labels = names(col_pca)) +
      geom_segment(
        data = directions,
        mapping = aes(x = 0, y = 0, xend = xvar, yend = yvar, color = cats_pars),
        arrow = arrow(length = unit(1 / 2, "picas"))
      ) +
      geom_text(
        data = directions,
        mapping = aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust, color = cats_pars),
        size = 3
      ) +
      scale_color_manual(name = "Tipo parámetro", values = cols_type, breaks = names(cols_type)) +
      guides(color = guide_legend(override.aes = list(label = " "))) +
      geom_point(data = scores, mapping = aes(x = xvar, y = yvar), show.legend = F) +
      geom_text(data = scores, mapping = aes(x = xvar, y = yvar, label = col_sitio), nudge_y = 0.1, size = 2.5) +
      theme(text = element_text(family = "Arial"))
    #+
    # xlim(-3,3)+
    # ylim(-3,3)
    ggsave(filename = paste0("fig_pca_", matriz, "_", col_grupo, ".png"), plot = plot, device = "png", width = width, height = height)
  }
}

#' @title fn_plot_granulometria
#' @description función para graficar fracciones granulométricas. Solo tiene sentido para datos de Sedimentos.
#' @param data archivo entrada que tiene que tener, al menos, las columnas: sitio, parámetros (nombre o sigla), valor de parámetros. Tiene que estar en formato "long". Se recomida usar csv o tsv como formatos.
#' @param col_pars string que indica el nombre de la columna en data que tiene los parámetros (su sigla).
#' @param col_sitio string que indica el nombre de la columna que tiene los sitios de muestreo.
#' @param ord_sitio string que indica el orden en que se quiere que aparezcan los sitios en el gráfico (de forma ascendente o descendente de acuerdo al número de estación o punto de muestreo).
#' @param code_sitio string que indica el código (generalmente una letra seguida un guión) que utilizada para nombras las estaciones (eg. "E-", "P-", "D-"). Se esperan puntos de muestreos asignados de forma consecutiva y sin gaps.
#' @param col_valor string que indica el nombre de la columna que tiene el valor de los parámetros.
#' @param col_grupo string que indica el nombre del de la columna que contiene una variable de agrupamiento para el plot (e.g. zonas, campañas, etc.). Por defecto, Nulo.
#' @param ord_grupo string que indica el orden en el que se quiera que aparezan en las leyendas y/o ejes los iteml del grupo. Solo tiene sentido si se usa col_grupo. Por defecto, Nulo.
#' @param width ancho del gráfico. Por defecto, toma valor 8
#' @param height alto del gráfico. Por defecto, toma valor 6
#' @param aspect_ratio relación alto-ancho. Por defecto, toma valor 1.
#' @import tidyverse
#' @import RColorBrewer
#' @import grDevices
#' @return gráfico de granulometría
#' @export fn_plot_granulometria
#' @examples
#' \dontrun{
#' # figura sin grupos
#' data <- readr::read_tsv("data_sedimento_RDLP.tsv")
#' col_pars <- "Param"
#' col_sitio <- "Estacion"
#' col_valor <- "Resultado"
#' code_sitio <- "P-"
#' ord_sitio <- "asc"# "desc o asc"
#' width <- 6
#' height <- 4
#' fn_plot_granulometria(data = data,col_pars = col_pars,col_sitio = col_sitio, col_valor = col_valor,ord_sitio = ord_sitio, width = 8,height = 6, aspect_ratio = 1, code_sitio = code_sitio)
#' }
fn_plot_granulometria <- function(data, col_pars, col_sitio, col_valor, code_sitio, ord_sitio = "asc", col_grupo = NULL, ord_grupo = NULL, width = 8, height = 6, aspect_ratio = 1) {
  # setting vars#
  pars_gran <- c("LIM", "AMF", "AF", "AM", "AG", "AMG", "GRAN")
  vars <- c(col_sitio, col_pars, col_valor)
  data_plot <- data %>% select(all_of(vars))
  patterns <- c("(?i)sitio|(?i)estacion","(?i)param", "(?i)resultado|(?i)valor")
  new_names <- c( "col_sitio", "col_pars", "col_valor")
  data_plot <- data_plot %>%
    dplyr::select(matches(patterns)) %>%
    dplyr::rename_at(vars(matches(patterns)), ~ new_names)
  if (!is.null(col_grupo)) {
    grupo <- data %>% pull({{ col_grupo }})
    data_plot <- data_plot %>% mutate(col_grupo = grupo)
  }
  all_pars <- data_plot %>% pull(col_pars) %>% unique()
  if (length(intersect(all_pars, pars_gran)) == 0) {
    stop("Error, no hay variables de granos en el dataframe")
  }
  sitios_tmp <- str_extract_all(data_plot[["col_sitio"]], "\\d+",simplify = T) %>%
    as.numeric() %>%
    unique() %>%
    sort()
  sitios_ord <- dplyr::case_when(
    ord_sitio == "asc" ~ paste0(code_sitio, sitios_tmp),
    ord_sitio == "desc" ~ paste0(code_sitio, rev(sitios_tmp)),
    TRUE ~ NA_character_
  )
  col_grano <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(pars_gran))
  names(col_grano) <- pars_gran
  # setting df#
  data_plot <- data_plot %>% dplyr::filter(col_pars %in% pars_gran)
  if (is.null(col_grupo)) {
    data_plot <- data_plot %>% dplyr::mutate(
      col_pars = factor(col_pars, levels = pars_gran),
      col_sitio = factor(col_sitio, levels = sitios_ord),
      col_pars = factor(col_pars, levels = pars_gran)
    )
    plot <- ggplot() +
      geom_col(data = data_plot, aes(x = as.factor(col_sitio), y = col_valor, fill = col_pars)) +
      # facet_grid(~ zona, scales = "free") +
      labs(x = "Estación", y = "Tamaño de grano (%)") +
      theme_bw() +
      scale_fill_manual("Tamaño", values = col_grano) +
      theme(text = element_text(family = "Arial"))
    ggsave(filename = "fig_granulometria.png", plot = plot, width = width, height = height, dpi = 300)
  }
  if (!is.null(col_grupo) && length(col_grupo) == 1) {
    if (!is.null(ord_grupo)) {
      order_grupo <- ord_grupo
    } else {
      order_grupo <- data_plot %>%
        dplyr::pull(col_grupo) %>%
        unique() %>%
        sort()
    }
    data_plot <- data_plot %>% dplyr::mutate(col_pars = factor(col_pars, levels = pars_gran),
                                             col_sitio = factor(col_sitio, levels = sitios_ord),
                                             col_grupo = factor(col_grupo, levels = order_grupo)
    )
    plot <- ggplot(data = data_plot, aes(x = as.factor(col_sitio), y = col_valor, fill = col_pars)) +
      geom_col(position = "stack") +
      facet_grid(~col_grupo, scales = "free") +
      labs(x = "Sitio",
           y = "Tamaño de grano (%)") +
      scale_fill_manual("Tamaño", values = col_grano) +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.position = "right") +
      guides(fill = guide_legend(ncol = 1, byrow = TRUE)) +
      theme_bw() +
      theme(strip.placement = "outside",
            strip.background = element_rect(fill = "gray95"),
            text = element_text(size = 10, family = "Arial"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            aspect.ratio = aspect_ratio,
            legend.key.size = unit(1 / 2, "picas"))
    ggsave(filename = paste0("fig_granulometria_", col_grupo, ".png"), plot = plot, width = 10, height = 5, dpi = 300)
  }
  #if (!is.null(col_grupo) && length(col_grupo) == 2) {
  #  grupo1 <- sym(col_grupo[1])
  #  grupo2 <- sym(col_grupo[2])
  #  grupo1_ord <- data %>%
  #    dplyr::pull(!!grupo1) %>%
  #    unique() %>%
  #    sort(decreasing = F)
  #  grupo2_ord <- data %>%
  #    dplyr::pull(!!grupo2) %>%
  #    unique()
  #  data_plot <- data_plot %>% dplyr::mutate(!!pars := factor(!!pars, levels = pars_gran),
  #    !!sitio := factor(!!sitio, levels = sitios_ord),
  #    grupo1 = factor(!!grupo1, levels = grupo1_ord),
  #    grupo2 = factor(!!grupo2, levels = grupo2_ord)
  #  )
  #  df_list <- data_plot %>%
  #    dplyr::group_by(!!grupo2) %>%
  #    dplyr::group_split() %>%
  #    setNames(purrr::map(., ~ paste0(unique(.[[grupo2]]))))
  #  # aux fn plot#
  #  fn_plot_aux <- function(data) {
  #    plot <- ggplot() +
  #      geom_col(data = data, aes(x = as.factor(!!sitio), y = !!valor, fill = !!pars)) +
  #      facet_grid(~grupo1, scales = "free") +
  #      labs(
  #        x = NULL,
  #        y = "Tamaño de grano (%)"
  #      ) +
  #      theme_bw() +
  #      scale_fill_manual("Tamaño", values = col_grano) +
  #      theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.position = "right") +
  #      guides(fill = guide_legend(ncol = 1, byrow = TRUE)) +
  #      theme(
  #        strip.placement = "outside",
  #        aspect.ratio = aspect_ratio,
  #        strip.background = element_rect(fill = "gray95"),
  #        text = element_text(family = "Arial")
  #      )
  #  }
  #  list_plot <- purrr::map(df_list, ~ fn_plot_aux(.))
  #  plot <- list_plot[[1]] / list_plot[[2]] +
  #    patchwork::plot_layout(
  #      guides = "collect",
  #      axis_titles = "collect_y"
  #    ) +
  #    plot_annotation(
  #      tag_levels = "a",
  #      tag_suffix = ")"
  #    ) + xlab("Sitios") +
  #    theme(
  #      plot.tag = element_text(hjust = 0),
  #      text = element_text(size = 10, family = "Arial"),
  #      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  #    )
  #  ggsave(filename = paste0("fig_granulometria_", col_grupo[1], "_", col_grupo[2], ".png"), plot = plot, width = 10, height = 5, dpi = 300)
  #}
}
