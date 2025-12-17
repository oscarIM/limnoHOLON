#' @title Resumen estadístico por parámetro
#' @description Calcula estadísticos descriptivos por parámetro
#' @param data Tabla de datos de entrada.
#' @param col_pars Nombre de la columna (como cadena) que identifica el parámetro.
#' @param col_value Nombre de la columna (como cadena) con los valores numéricos.
#' @param col_bld Nombre de la columna (como cadena) que indica registros bajo el límite de detección (BLD).
#' @param matrix Cadena que indica la matrix (por ejemplo, "agua", "sedimento") y controla el filtrado de parámetros granulométricos.
#' @param round Número de decimales para redondear los estadísticos (por defecto 2).
#' @param output_name Nombre de archivo de salida (CSV) donde se guardará la tabla de resultados (se genera también un TSV adicional).
#' @importFrom dplyr select rename filter group_by summarise mutate mutate_at all_of case_when vars n
#' @importFrom rlang sym
#' @importFrom scales percent
#' @importFrom knitr kable
#' @importFrom utils write.table
#' @importFrom stringr str_replace
#' @importFrom readr write_tsv
#' @export get_summ_stats
#' @examples
#' \dontrun{
#' data <- readr::read_tsv("data_sedimento_inv_2025_clean.tsv")
#' col_pars <- "Sigla"
#' col_value <- "Valor"
#' matrix <- "sedimento"
#' round <- 4
#' fn_stats(data = data, col_pars = col_pars, col_value = col_value, data_pars = data_pars, matrix = matrix, round = round)
#' }
get_summ_stats <- function(data,
                           col_pars,
                           col_value,
                           col_bld,
                           matrix,
                           round = 2,
                           output_name) {
  op <- options(scipen = 999)
  on.exit(options(op))
  # setting vars and aux functions#
  pars_gran <- c(
    "LIM", "AMF", "AF", "AM", "AG", "AMG", "GRAN", "GUIJ",
    "Arena Fina", "Arena Gruesa", "Arena Media", "Arena Muy Fina",
    "Arena Muy Gruesa", "Fango", "Grava Fina", "Grava Muy Fina",
    "KURTOSIS", "SKEWNESS", "SORTING", "Tamaño medio", "Tipo Grano"
  )
  vars <- c(col_pars, col_value, col_bld)
  data_plot <- data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::rename(
      col_pars = !!sym(col_pars),
      col_value = !!sym(col_value),
      col_bld = !!sym(col_bld)
    )
  pars_no_LC <- c("Dens", "Transp", "SST", "DO%")
  if (stringr::str_detect(string = matrix, pattern = "(?i)sedimentos?")) {
    data_plot <- data_plot %>% dplyr::filter(!col_pars %in% pars_gran)
    pars_no_LC <- "MOT"
  }
  summ_pars <- data_plot %>%
    dplyr::group_by(col_pars) %>%
    dplyr::summarise(
      Nobs = dplyr::n(),
      min = min(col_value, na.rm = TRUE),
      max = max(col_value, na.rm = TRUE),
      prom = mean(col_value, na.rm = TRUE),
      desvest = sd(col_value, na.rm = TRUE),
      cv_num = (desvest / prom),
      "cv%" = scales::percent(desvest / prom, accuracy = 0.01),
      n_bld = sum(col_bld, na.rm = TRUE),
      prop_bld = round(n_bld / Nobs, 2)
    ) %>%
    dplyr::mutate(
      prop_bld = dplyr::case_when(col_pars %in% pars_no_LC ~ NA,
        .default = prop_bld
      ),
      "BLD%" = scales::percent(prop_bld, accuracy = 1)
    ) %>%
    dplyr::select(-c(cv_num, n_bld, prop_bld)) %>%
    dplyr::rename(Sigla = col_pars) %>%
    dplyr::mutate_at(dplyr::vars(3:6), list(~ round(., round)))
  print(knitr::kable(summ_pars, format = "markdown", align = "c", caption = "Tabla parámetros"))
  write.table(summ_pars, file = output_name, sep = ";", na = "NA", dec = ",", row.names = F, col.names = T)
  file <- stringr::str_replace(string = output_name, pattern = ".csv", replacement = ".tsv")
  readr::write_tsv(x = summ_pars, file = file)
}
###############################################################################
#' @title Gráficos de barras de parámetros abióticos con normas
#' @description Variante de `plot_parameter_levels` que, cuando se entrega una tabla de normas (`ref_data`), filtra y dibuja solo aquellas referencias que se sexcedan
#' @param data Tabla de datos de entrada.
#' @param col_pars Nombre de la columna (cadena) que identifica el parámetro.
#' @param col_site Nombre de la columna (cadena) que identifica el sitio.
#' @param col_value Nombre de la columna (cadena) con los valores numéricos.
#' @param col_facet Nombre de la columna (cadena) usada para facetear (opcional).
#' @param ord_facet Vector con el orden deseado de los niveles de la faceta (opcional).
#' @param col_units Nombre de la columna (cadena) con la unidad del parámetro.
#' @param ord_site Vector con el orden deseado de los sitios en el eje x.
#' @param matrix Cadena que describe la matrix (por ejemplo, "agua", "sedimento").
#' @param type_par Nombre de la columna (cadena) que clasifica los parámetros por tipo.
#' @param col_group Nombre de la columna (cadena) con el grupo (estrato, réplica, etc.) para colorear barras (opcional). En caso de ser estratos, tienen que ser ,"Supreficie", "Medio" y/o "Fondo".
#' @param legend_name Título de la leyenda asociada a `col_group` (opcional).
#' @param data_source Cadena que describe la fuente de datos (por ejemplo, "in situ").
#' @param ref_data Tabla con normas o valores de referencia para los parámetros (incluyendo columnas `Sigla`, `nombre_norma`, `limite`, `tipo_limite`).
#' @importFrom dplyr rename group_by summarise mutate filter pull arrange case_when group_split distinct left_join
#' @importFrom dplyr select
#' @importFrom rlang sym
#' @importFrom stringr str_detect str_replace
#' @importFrom ggplot2 ggplot aes geom_bar geom_hline geom_text facet_grid labs theme_bw theme element_text element_rect scale_linetype_manual
#' @importFrom ggplot2 scale_fill_manual scale_color_manual guides guide_legend position_dodge2 scale_y_continuous ggsave
#' @importFrom ggsci pal_nejm pal_jco pal_npg
#' @importFrom ggh4x facetted_pos_scales
#' @importFrom glue glue
#' @importFrom purrr map walk walk2
#' @export plot_parameter_levels
plot_parameter_levels <- function(data,
                                  col_pars,
                                  col_site,
                                  ord_site,
                                  col_value,
                                  col_facet = NULL,
                                  ord_facet = NULL,
                                  col_units,
                                  matrix,
                                  type_par,
                                  col_group = NULL,
                                  legend_name = NULL,
                                  data_source,
                                  ref_data = NULL) {
  op <- options(scipen = 999)
  on.exit(options(op))
  #### función auxiliar####
  fn_plot_aux <- function(data) {
    n_pars <- length(unique(data$col_pars))
    type_data <- as.character(unique(data$type_par))
    title_par <- ifelse(type_data == "Metales", "metálicos", stringr::str_to_lower(type_data))

    subt <- if (data_source == "in situ") {
      bquote("Parámetros medidos " * italic("in situ"))
    } else {
      paste("Parámetros medidos", data_source)
    }

    # ---- generador de gráficos ----
    make_plot <- function(data_sub, idx = NULL) {
      n_pars_sub <- length(unique(data_sub$col_pars))

      # tamaño dinámico
      if (n_pars_sub <= 2) {
        w <- 9
        h <- 5
      } else if (n_pars_sub <= 5) {
        w <- 9
        h <- 10
      } else if (n_pars_sub <= 7) {
        w <- 9
        h <- 11
      } else {
        w <- 9
        h <- 18
      }
      if (is.null(col_group) && is.null(col_facet)) {
        # Sin agrupación ni facetas
        plot <- ggplot2::ggplot(
          data = data_sub,
          ggplot2::aes(x = factor(col_site), y = col_value)
        ) +
          ggplot2::geom_bar(
            stat = "identity",
            position = ggplot2::position_dodge2(preserve = "single"),
            show.legend = FALSE,
            colour = "#80796BFF"
          ) +
          ggplot2::facet_grid(~label,
            scales = "free",
            switch = "y",
            space = "free_x"
          ) +
          ggh4x::facetted_pos_scales(y = list(label == "pH" ~ gglot2::scale_y_continuous(limits = c(5, 9))))
      } else if (is.null(col_group) && !is.null(col_facet)) {
        plot <- ggplot2::ggplot(data = data_sub, ggplot2::aes(
          x = factor(col_site),
          y = col_value,
          fill = type_par
        )) +
          ggplot2::geom_bar(
            stat = "identity",
            position = ggplot2::position_dodge2(preserve = "single"),
            show.legend = FALSE
          ) +
          ggplot2::scale_fill_manual(values = cols_type) +
          ggplot2::facet_grid(label ~ col_facet,
            scales = "free",
            switch = "y",
            space = "free_x"
          ) +
          ggh4x::facetted_pos_scales(y = list(label == "pH" ~ ggplot2::scale_y_continuous(limits = c(5, 9))))
      } else if (!is.null(col_group) && is.null(col_facet)) {
        # Con agrupación, sin facetas
        plot <- ggplot2::ggplot(data = data_sub, ggplot2::aes(
          x = factor(col_site),
          y = col_value,
          fill = col_group
        )) +
          ggplot2::geom_bar(
            stat = "identity",
            position = position_dodge2(preserve = "single"),
            show.legend = TRUE
          ) +
          ggplot2::scale_fill_manual(values = cols_type, name = legend_name) +
          ggplot2::facet_grid(
            rows = vars(label),
            scales = "free",
            switch = "y",
            space = "free_x"
          ) +
          ggh4x::facetted_pos_scales(y = list(label == "pH" ~ scale_y_continuous(limits = c(5, 9))))
      } else if (!is.null(col_group) && !is.null(col_facet)) {
        # Con agrupación y facetas adicionales
        plot <- ggplot2::ggplot(data = data_sub, ggplot2::aes(
          x = factor(col_site),
          y = col_value,
          fill = col_group
        )) +
          ggplot2::geom_bar(
            stat = "identity",
            position = position_dodge2(preserve = "single"),
            show.legend = TRUE
          ) +
          ggplot2::scale_fill_manual(values = cols_type, name = legend_name) +
          ggplot2::facet_grid(label ~ col_facet,
            scales = "free",
            switch = "y",
            space = "free_x"
          ) +
          ggh4x::facetted_pos_scales(y = list(label == "pH" ~ scale_y_continuous(limits = c(5, 9))))
      }
      plot <- plot +
        ggplot2::labs(
          title = glue::glue("Concentración o nivel de parámetros {title_par}"),
          subtitle = subt,
          x = "Sitios",
          y = "Concentración o nivel"
        ) +
        ggplot2::theme_bw(base_size = 10) +
        ggplot2::theme(
          strip.text = element_text(face = "bold", colour = "white"),
          strip.background = element_rect(fill = "#2c3e50"),
          text = element_text(size = 10, family = "Arial"),
          plot.title = element_text(size = 12, face = "bold", hjust = 0),
          axis.text.x = element_text(angle = angle, vjust = 0.5, hjust = 0.5)
        )

      if (!is.null(ref_data)) {
        ### ACA HAY QUE FILTRAR
        normas_tbl <- ref_data %>%
          dplyr::filter(.data$Sigla %in% unique(data_sub$col_pars))
        normas_tbl <- dplyr::left_join(data_sub, normas_tbl, by = c("col_pars" = "Sigla"), relationship = "many-to-many")
        df_long <- normas_tbl %>%
          dplyr::filter(!is.na(tipo_limite))
        df_excede <- df_long %>%
          dplyr::mutate(
            excede = dplyr::case_when(
              col_pars == "pH" & tipo_limite == "Rango inferior" & col_value < limite ~ 1, # pH bajo el límite inferior
              col_pars == "pH" & tipo_limite == "Rango superior" & col_value > limite ~ 1, # pH sobre el límite superior
              col_pars != "pH" & col_value > limite ~ 1, # otros parámetros: excede si supera el límite
              TRUE ~ 0
            )
          )
        selected_norm <- df_excede %>%
          dplyr::filter(excede == 1) %>%
          dplyr::distinct(nombre_norma) %>%
          dplyr::pull(nombre_norma)
        selected_pars_norm <- df_excede %>%
          dplyr::filter(excede == 1) %>%
          dplyr::distinct(col_pars) %>%
          dplyr::pull(col_pars)
        data_ref <- data_ref %>%
          dplyr::filter(nombre_norma %in% selected_norm) %>%
          dplyr::filter(Sigla %in% selected_pars_norm)

        data_sub <- dplyr::left_join(data_sub, data_ref, by = c("col_pars" = "Sigla"), relationship = "many-to-many")

        colores_ref <- c("#FF8C00", "#FF3030", "#912CEE", "#0072B2", "#009E73", "#000000", "#CC79A7", "gray50")
        names(colores_ref) <- c(
          "Referencia norma ANZECC", "Referencia norma CCME", "Valor característico", "Referencia norma DS 144",
          "Referencia norma EPA",
          "Referencia norma NOAA",
          "Referencia norma NSCA Quintero",
          "Referencia norma UE 2023"
        )

        # names(colores_ref) <- unique(na.omit(data_sub$nombre_norma))

        plot <- plot +
          ggplot2::geom_hline(
            data = data_sub, na.rm = TRUE,
            ggplot2::aes(
              yintercept = limite,
              color = nombre_norma,
              group = col_pars,
              linetype = tipo_limite
            ),
            # linetype = "dashed",
            linewidth = 0.7,
            show.legend = TRUE
          ) +
          ggplot2::scale_color_manual(
            name = "Normas de contraste",
            values = colores_ref,
            na.translate = F
          ) +
          ggplot2::scale_linetype_manual(
            name = "Tipo de valor",
            values = c(
              "Valor máximo" = "solid",
              "Valor agudo" = "dashed",
              "Valor crónico" = "dotted",
              "Rango superior" = "dotdash",
              "Rango superior" = "dotdash",
              "PEL" = "dashed",
              "TEL" = "dotted"
            ), # vector nombrado por niveles de `nombre_norma`
            na.translate = FALSE
          ) +
          ggplot2::guides(
            fill = guide_legend(override.aes = list(linetype = NULL), order = 3),
            color = guide_legend(override.aes = list(fill = NA), order = 1),
            linetype = guide_legend(order = 2, override.aes = list(fill = NA))
          )
      }

      # Si no hay col_group ni facetas, o si col_group es NULL y col_facet no lo es (casos sin "fill" relevante):
      if (is.null(col_group)) {
        plot <- plot + ggplot2::guides(fill = "none")
      }

      # Si no hay ref_data, quitamos también la de color:
      if (is.null(ref_data)) {
        plot <- plot + ggplot2::guides(color = "none")
      }
      data_source_clean <- stringr::str_replace(string = data_source, pattern = " ", replacement = "_")
      sufijo <- if (!is.null(idx)) paste0("_", idx) else ""

      file_name <- if (is.null(col_facet)) {
        glue::glue("bar_pars_{unique(data_sub$type_par)}{sufijo}_{matrix}_{data_source_clean}.png")
      } else {
        glue::glue("bar_pars_{unique(data_sub$type_par)}{sufijo}_{col_facet}_{matrix}_{data_source_clean}.png")
      }
      ggplot2::ggsave(
        filename = file_name,
        plot = plot,
        width = w,
        height = h,
        dpi = 300
      )
    }

    if (n_pars > 8) {
      grupos_pars <- split(
        unique(data$col_pars),
        rep(1:ceiling(n_pars / 5), each = 5, length.out = n_pars)
      )
      purrr::walk2(grupos_pars, seq_along(grupos_pars), function(pnames, idx) {
        data_sub <- dplyr::filter(data, col_pars %in% pnames)
        make_plot(data_sub, idx = idx)
      })
    } else {
      make_plot(data)
    }
  }
  #### variables base####
  pars_gran <- c(
    "LIM", "AMF", "AF", "AM", "AG", "AMG", "GRAN", "GUIJ",
    "Arena Fina", "Arena Gruesa", "Arena Media", "Arena Muy Fina",
    "Arena Muy Gruesa", "Fango", "Grava Fina", "Grava Muy Fina",
    "KURTOSIS", "SKEWNESS", "SORTING", "Tamaño medio", "Tipo Grano"
  )

  vars <- c(col_site, col_pars, col_units, col_value, type_par, col_group, col_facet)
  data_plot <- data %>%
    dplyr::rename(
      col_pars = !!sym(col_pars),
      col_value = !!sym(col_value),
      col_site = !!sym(col_site),
      col_units = !!sym(col_units),
      type_par = !!sym(type_par),
      !!!if (!is.null(col_facet)) setNames(col_facet, "col_facet") else NULL,
      !!!if (!is.null(col_group)) setNames(col_group, "col_group") else NULL
    )
  ##### selección de parámetros####
  selected_pars <- data_plot %>%
    dplyr::group_by(col_pars) %>%
    dplyr::summarise(
      prom = abs(mean(col_value, na.rm = TRUE)),
      desvest = sd(col_value, na.rm = TRUE),
      cv_num = desvest / prom
    ) %>%
    dplyr::filter(cv_num > 0) %>%
    dplyr::pull(col_pars)

  if (stringr::str_detect(string = matrix, pattern = "(?i)sedimento?")) {
    selected_pars <- setdiff(selected_pars, pars_gran)
  }

  data_plot <- data_plot %>%
    dplyr::filter(col_pars %in% selected_pars)
  order_type <- c("Fisicoquímicos", "Nutrientes", "Metales", "Orgánicos", "Bacteriológicos", "Bioquímicos", "TCLP inorgánicos")
  cols_type <- ggsci::pal_nejm("default")(length(order_type) + 1)
  cols_type[7] <- "#727272"
  names(cols_type) <- order_type
  #### colores por tipo de parámetro ####
  n_types <- length(unique(data_plot$type_par))
  if (is.null(col_group)) {
    type_par <- unique(data_plot$type_par)
    cols_type <- cols_type[type_par]
  }
  #### colores por grupo : Estratos, replicas, etc ####
  if (!is.null(col_group)) {
    if (stringr::str_detect(string = col_group, pattern = "(?i)estratos?")) {
      cols_type <- c("#87CEFF", "#00868B", "#1874CD")
      names(cols_type) <- c("Superficie", "Medio", "Fondo")
    } else if (stringr::str_detect(string = col_group, pattern = "(?i)r[eé]plicas?")) {
      cols_type <- ggsci::pal_jco("default")(length(unique(data_plot$col_group)))
      names(cols_type) <- sort(unique(data_plot$col_group))
    } else {
      cols_type <- ggsci::pal_npg("nrc")(length(unique(data_plot$col_group)))
      names(cols_type) <- unique(data_plot$col_group)
    }
  }

  #### ángulo etiquetas####
  angle <- if (length(ord_site) <= 13) 0 else 90
  data_plot <- data_plot %>%
    dplyr::mutate(
      col_site = factor(col_site, levels = ord_site),
      type_par = factor(type_par, levels = order_type),
      col_facet = if ("col_facet" %in% names(.)) factor(col_facet, levels = ord_facet) else col_facet
    ) %>%
    dplyr::arrange(type_par) %>%
    dplyr::mutate(
      label = glue::glue("{col_pars} {col_units}"),
      label = dplyr::case_when(col_pars == "pH" ~ "pH", .default = label)
    )
  if (!is.null(col_group)) {
    data_plot <- data_plot %>% dplyr::mutate(col_group = factor(col_group, levels = unique(data_plot$col_group)))
  }

  df_list <- data_plot %>%
    dplyr::group_by(type_par) %>%
    dplyr::group_split() %>%
    setNames(purrr::map(., ~ paste0(unique(.[["type_par"]]))))

  purrr::walk(df_list, ~ fn_plot_aux(data = .))
}
###############################################################################
#' @title  Correlograma de Spearman para Parámetros Ambientales
#' @description Esta función procesa un data frame de parámetros ambientales, calcula correlaciones de Spearman y genera un gráfico de tipo "heatmap" (triángulo inferior)
#' @param data Un data.frame o tibble que contiene los datos.
#' @param col_pars String. Nombre de la columna que contiene los nombres de los parámetros (ej. "Parametro").
#' @param col_site String. Nombre de la columna que contiene los sitios/estaciones.
#' @param col_value String. Nombre de la columna que contiene los valores numéricos.
#' @param col_group String (Opcional). Nombre de la columna para agrupación adicional. Por defecto NULL.
#' @param col_factor String (Opcional). Nombre de una columna de factor extra. Por defecto NULL.
#' @param matrix String. Tipo de matrix (ej. "Agua", "Sedimento"). Si contiene "sedimento", se excluyen parámetros granulométricos.
#' @param output_name String. Nombre archivo de salida.
#' @param width Numérico. Ancho de la imagen guardada en pulgadas. Por defecto 6.
#' @param height Numérico. Alto de la imagen guardada en pulgadas. Por defecto 6.
#' @param save_rds Lógico. Si es TRUE, guarda también el objeto R (.rds). Por defecto FALSE.
#' @return Un objeto ggplot2 con el correlograma. También guarda un archivo PNG en el directorio de trabajo.
#' @export plot_correlogram
#' @importFrom dplyr select rename group_by summarise filter pull distinct mutate if_else
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_detect
#' @importFrom rstatix cor_mat cor_pmat
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradientn labs theme_minimal theme element_text element_blank coord_fixed ggsave
#' @importFrom glue glue
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats sd median setNames
#' @importFrom rlang sym "!!" "!!!"
#' @examples
#' \dontrun{
#' plot_correlogram(
#'   data = mi_data,
#'   col_pars = "PARAMETRO",
#'   col_site = "ESTACION",
#'   col_value = "RESULTADO",
#'   matrix = "Agua de Mar",
#'   output_name = "correlograma_spearman.png"
#' )
#' }
plot_correlogram <- function(data,
                             col_pars,
                             col_site,
                             col_value,
                             matrix,
                             col_group = NULL,
                             col_factor = NULL,
                             width = 6,
                             height = 6,
                             output_name = "correlograma_spearman.png",
                             save_rds = FALSE) {
  op <- options(scipen = 999)
  on.exit(options(op))

  # 1. Definición de parámetros estáticos (Hardcoded parameters)
  pars_gran <- c(
    "LIM", "AMF", "AF", "AM", "AG", "AMG", "GRAN", "GUIJ",
    "Arena Fina", "Arena Gruesa", "Arena Media", "Arena Muy Fina",
    "Arena Muy Gruesa", "Fango", "Grava Fina", "Grava Muy Fina",
    "KURTOSIS", "SKEWNESS", "SORTING", "Tamaño medio", "Tipo Grano"
  )

  # 2. Selección y renombrado de columnas usando tidy evaluation
  # Creamos una lista de columnas a seleccionar
  vars_select <- c(col_site, col_pars, col_value)
  if (!is.null(col_group)) vars_select <- c(vars_select, col_group)
  if (!is.null(col_factor)) vars_select <- c(vars_select, col_factor)

  data_clean <- data %>%
    dplyr::select(dplyr::all_of(vars_select)) %>%
    dplyr::rename(
      col_site = !!rlang::sym(col_site),
      col_pars = !!rlang::sym(col_pars),
      col_value = !!rlang::sym(col_value),
      !!!if (!is.null(col_factor)) stats::setNames(col_factor, "col_factor") else NULL,
      !!!if (!is.null(col_group)) stats::setNames(col_group, "col_group") else NULL
    )

  # 3. Filtrado de parámetros (CV > 0 y exclusión de granulometría)
  # Calculamos estadísticas básicas para filtrar variables sin varianza
  stats_summary <- data_clean %>%
    dplyr::group_by(col_pars) %>%
    dplyr::summarise(
      prom = abs(mean(col_value, na.rm = TRUE)),
      desvest = stats::sd(col_value, na.rm = TRUE),
      cv_num = dplyr::if_else(prom == 0, 0, desvest / prom),
      .groups = "drop"
    )

  selected_pars <- stats_summary %>%
    dplyr::filter(cv_num > 0) %>%
    dplyr::pull(col_pars)

  # Lógica específica para sedimentos
  if (stringr::str_detect(string = matrix, pattern = "(?i)sedimento?")) {
    selected_pars <- setdiff(selected_pars, pars_gran)
  }

  # 4. Preparación de datos para correlación
  data_plot <- data_clean %>%
    dplyr::filter(col_pars %in% selected_pars) %>%
    tidyr::pivot_wider(names_from = col_pars, values_from = col_value)

  # Extraer matrix numérica y escalar
  # Identificamos columnas que NO son numéricas (metadata)
  # En este punto, 'col_site' es la única columna no parámetro seguro
  cols_metadata <- "col_site"
  if ("col_group" %in% names(data_plot)) cols_metadata <- c(cols_metadata, "col_group")
  if ("col_factor" %in% names(data_plot)) cols_metadata <- c(cols_metadata, "col_factor")

  data_corr <- data_plot %>%
    dplyr::select(!dplyr::any_of(cols_metadata)) %>%
    scale() %>%
    as.data.frame()

  # Advertencia por pocos sitios
  n_sitios <- nrow(data_corr)
  if (n_sitios <= 4) {
    warning(glue::glue(
      "¡Atención!, hay solo n = {n_sitios} puntos. ",
      "Interpretar coeficientes de Spearman y p-values con extremo cuidado."
    ))
  }

  # 5. Cálculo de matrices de correlación (r y p)
  coor_r <- rstatix::cor_mat(data_corr,
    method = "spearman",
    alternative = "two.sided",
    conf.level = 0.95
  ) %>%
    dplyr::select(-rowname) %>%
    as.matrix()
  rownames(coor_r) <- colnames(coor_r)

  coor_p <- rstatix::cor_pmat(data_corr,
    method = "spearman",
    alternative = "two.sided",
    conf.level = 0.95
  ) %>%
    dplyr::select(-rowname) %>%
    as.matrix()
  rownames(coor_p) <- colnames(coor_p)

  # 6. Formateo para ggplot (Triángulo inferior)
  plot_df <- as.data.frame(as.table(coor_r)) %>%
    dplyr::rename(Var1 = Var1, Var2 = Var2, Cor = Freq) %>%
    dplyr::mutate(Pval = as.data.frame(as.table(coor_p))$Freq)

  # Filtrar para mantener triángulo inferior
  # Nota: as.table convierte factores. Nos aseguramos de usar los niveles para filtrar.
  plot_df <- plot_df %>%
    dplyr::filter(as.integer(Var1) < as.integer(Var2)) %>%
    dplyr::mutate(sig = dplyr::if_else(Pval < 0.05, "p<0.05", "ns"))

  # 7. Gráfico
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = Var1, y = Var2)
  ) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = dplyr::if_else(sig == "p<0.05", Cor, NA_real_)),
      color = "gray40"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = dplyr::if_else(sig == "p<0.05", sprintf("%.2f", Cor), "")),
      size = 3,
      color = "white",
      fontface = "bold"
    ) +
    ggplot2::scale_fill_gradientn(
      colors = RColorBrewer::brewer.pal(11, "RdBu"),
      limits = c(-1, 1),
      name = expression("Correlación (" * rho * ")"),
      na.value = "white"
    ) +
    ggplot2::labs(
      x = "",
      y = "",
      title = "Correlación de Spearman entre parámetros",
      subtitle = glue::glue("matriz: {matrix}")
    ) +
    ggplot2::theme_minimal(base_family = "Arial") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 11)
    ) +
    ggplot2::coord_fixed()

  # 8. Guardado y Retorno
  tryCatch(
    {
      ggplot2::ggsave(filename = output_name, plot = p, width = width, height = height, dpi = 300)

      if (save_rds) {
        rds_name <- stringr::str_replace(output_name, "\\.(png|jpg|jpeg|pdf|tiff)$", ".rds")
        if (rds_name == output_name) rds_name <- paste0(output_name, ".rds")
        saveRDS(p, file = rds_name)
        message(paste("Objeto con correlograma (ggplot) guardado en:", rds_name))
      }
    },
    error = function(e) warning("Error guardando imagen/objeto: ", e$message)
  )

  return(p)
}
###############################################################################
#' @title Biplot de PCA con PERMANOVA Opcional
#' @description Realiza un Análisis de Componentes Principales (PCA) y genera un biplot.
#' Si se proporciona un factor (`col_factor`), calcula PERMANOVA y añade elipses.
#'
#' @param data Data frame con los datos.
#' @param col_pars String. Columna con los nombres de los parámetros.
#' @param col_site String. Columna con los identificadores de sitio.
#' @param col_value String. Columna con los valores numéricos.
#' @param type_par String. Columna que clasifica el tipo de parámetro.
#' @param matrix String. Tipo de matrix (ej. "Agua", "Sedimento").
#' @param col_rep String (Opcional). Columna de réplicas.
#' @param col_factor String (Opcional). Columna para agrupar y calcular PERMANOVA.
#' @param levels_factor Vector (Opcional). Orden específico para los niveles de `col_factor`.
#' @param width Numérico. Ancho de la imagen.
#' @param height Numérico. Alto de la imagen.
#' @param dist String. Método de distancia para PERMANOVA (por defecto "euclidean").
#' @param output_name String. Nombre del archivo de salida (png).
#' @param save_rds Lógico. Si es TRUE, guarda también el objeto R (.rds). Por defecto FALSE.
#'
#' @return Un objeto ggplot (invisible).
#' @export plot_pca
#' @importFrom dplyr select rename group_by summarise filter pull distinct mutate left_join bind_cols starts_with any_of arrange
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_detect str_to_lower str_to_sentence str_replace
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline labs geom_segment geom_text geom_point scale_color_manual scale_fill_manual guides guide_legend theme_bw theme element_text stat_ellipse scale_x_continuous scale_y_continuous arrow unit xlim ylim coord_fixed
#' @importFrom glue glue
#' @importFrom stats prcomp sd setNames qchisq
#' @importFrom scales percent
#' @importFrom ggsci pal_nejm pal_d3
#' @importFrom grDevices colorRampPalette
#' @importFrom rlang sym "!!" "!!!"
#' @importFrom vegan adonis2
plot_pca <- function(data,
                     col_pars,
                     col_site,
                     col_value,
                     type_par,
                     matrix,
                     col_rep = NULL,
                     col_factor = NULL,
                     levels_factor = NULL,
                     width = 6,
                     height = 6,
                     dist = "euclidean",
                     output_name = "pca.png",
                     save_rds = FALSE) {
  # Manejo de notación científica seguro
  op <- options(scipen = 999)
  on.exit(options(op))

  # --- 2. Configuración de Parámetros ---
  pars_gran <- c(
    "LIM", "AMF", "AF", "AM", "AG", "AMG", "GRAN", "GUIJ",
    "Arena Fina", "Arena Gruesa", "Arena Media", "Arena Muy Fina", "Arena Muy Gruesa", "Fango",
    "Grava Fina", "Grava Muy Fina", "KURTOSIS", "SKEWNESS", "SORTING", "Tamaño medio", "Tipo Grano"
  )

  vars <- c(col_site, col_pars, col_value, col_rep, type_par, col_factor)

  # Renombrado y Selección (Harmonizado)
  data_plot <- data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::rename(
      col_pars = !!rlang::sym(col_pars),
      col_site = !!rlang::sym(col_site),
      col_value = !!rlang::sym(col_value),
      type_par = !!rlang::sym(type_par),
      !!!if (!is.null(col_factor)) stats::setNames(col_factor, "col_factor") else NULL,
      !!!if (!is.null(col_rep)) stats::setNames(col_rep, "col_rep") else NULL
    )

  # Filtrado CV > 0
  selected_pars <- data_plot %>%
    dplyr::group_by(col_pars) %>%
    dplyr::summarise(
      prom = abs(mean(col_value, na.rm = TRUE)),
      desvest = stats::sd(col_value, na.rm = TRUE),
      cv_num = desvest / prom
    ) %>%
    dplyr::filter(cv_num > 0) %>%
    dplyr::pull(col_pars)

  if (stringr::str_detect(string = matrix, pattern = "(?i)sedimento?")) {
    selected_pars <- setdiff(selected_pars, pars_gran)
  }

  data_pca <- data_plot %>%
    dplyr::filter(col_pars %in% selected_pars)

  # Configuración de Colores
  order_type <- c("Fisicoquímicos", "Nutrientes", "Metales", "Orgánicos", "Bacteriológicos", "Bioquímicos", "TCLP inorgánicos")
  cols_type <- ggsci::pal_nejm("default")(length(order_type) + 1)
  cols_type <- cols_type[-7]
  # Ajuste manual TCLP
  cols_type_final <- stats::setNames(cols_type, order_type[1:length(cols_type)])
  if (length(cols_type) < length(order_type)) {
    # Fallback simple si el vector no calza exacto
    cols_type_final <- stats::setNames(rep("grey", length(order_type)), order_type)
  }
  # Reasignar el gris específico si existe el nombre
  # Nota: Tu código original hacía cols_type[7] <- "#727272" asumiendo longitud.
  # Para robustez en paquete:
  cols_type <- ggsci::pal_nejm("default")(length(order_type) + 1)
  cols_type <- cols_type[1:length(order_type)]
  names(cols_type) <- order_type
  cols_type["TCLP inorgánicos"] <- "#727272"

  type_par_vec <- unique(data_plot$type_par)
  cols_type <- cols_type[names(cols_type) %in% type_par_vec]

  # --- 3. Generación del Gráfico (Estructura Original Bifurcada) ---

  if (is.null(col_factor)) {
    # === CASO A: SIN FACTOR ===
    to_pca <- data_pca %>%
      dplyr::select(dplyr::any_of(c("col_site", "col_rep", "col_pars", "col_value"))) %>%
      tidyr::pivot_wider(names_from = col_pars, values_from = col_value) %>%
      dplyr::select(dplyr::where(~ !any(is.na(.))))

    metadata <- to_pca %>% dplyr::select(dplyr::starts_with("col_"))
    to_pca_num <- to_pca %>% dplyr::select(!dplyr::starts_with("col_"))

    pca <- stats::prcomp(to_pca_num, scale. = TRUE)
    prop_pca <- pca$sdev^2 / sum(pca$sdev^2)

    tmp_data <- get_data_pca_plot(pca_obj = pca)
    directions <- tmp_data$directions

    data_pars <- data_pca %>%
      dplyr::select(col_pars, type_par) %>%
      dplyr::distinct()

    directions <- dplyr::left_join(directions, data_pars, by = c("varname" = "col_pars"))
    scores <- tmp_data$scores
    scores <- dplyr::bind_cols(scores, metadata)

    xmax <- ceiling(max(directions$xvar, scores$xvar)) + 0.5
    xmin <- floor(min(directions$xvar, scores$xvar)) - 0.5
    ymax <- ceiling(max(directions$yvar, scores$yvar)) + 0.5
    ymin <- floor(min(directions$yvar, scores$yvar)) - 0.5

    plot <- ggplot2::ggplot() +
      ggplot2::geom_hline(yintercept = 0, linetype = 3, color = "gray0") +
      ggplot2::geom_vline(xintercept = 0, linetype = 3, color = "gray0") +
      ggplot2::xlim(xmin + 0.25, xmax - 0.25) +
      ggplot2::ylim(ymin + 0.25, ymax - 0.25) +
      ggplot2::labs(
        title = glue::glue("Análisis multivariado (PCA) para parámetros medidos en {matrix}"),
        x = glue::glue("Primera Componente Principal ({scales::percent(prop_pca[1])})"),
        y = glue::glue("Segunda Componente Principal ({scales::percent(prop_pca[2])})")
      ) +
      ggplot2::geom_segment(
        data = directions,
        mapping = ggplot2::aes(x = 0, y = 0, xend = xvar, yend = yvar, color = type_par),
        arrow = ggplot2::arrow(length = ggplot2::unit(1 / 2, "picas"))
      ) +
      ggplot2::geom_text(
        data = directions,
        mapping = ggplot2::aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust, color = type_par),
        size = 3, family = "sans"
      ) +
      ggplot2::geom_text(
        data = scores,
        mapping = ggplot2::aes(x = xvar, y = yvar, label = col_site),
        nudge_y = 0.1,
        family = "sans",
        size = 3
      ) +
      ggplot2::scale_color_manual(name = "Tipo parámetro", values = cols_type, breaks = names(cols_type)) +
      ggplot2::guides(color = ggplot2::guide_legend(title.position = "top", override.aes = list(label = " "))) +
      ggplot2::geom_point(data = scores, mapping = ggplot2::aes(x = xvar, y = yvar), size = 1) +
      ggplot2::scale_x_continuous(expand = c(0.1, 0.1)) +
      ggplot2::scale_y_continuous(expand = c(0.1, 0.1)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        text = ggplot2::element_text(size = 8, family = "sans"),
        plot.title = ggplot2::element_text(face = "bold", family = "sans", size = 10)
      )
  } else {
    # === CASO B: CON FACTOR (PERMANOVA) ===
    to_pca <- data_pca %>%
      dplyr::select(dplyr::starts_with("col_")) %>%
      tidyr::pivot_wider(names_from = col_pars, values_from = col_value) %>%
      dplyr::select(dplyr::where(~ !any(is.na(.))))

    # Calcular índice de inicio de columnas numéricas
    # Esto asume estructura, pero es como estaba en tu código original
    # Usamos tryCatch para vegan
    if (!requireNamespace("vegan", quietly = TRUE)) stop("Paquete vegan requerido")

    begin <- data_pca %>%
      dplyr::select(dplyr::any_of(c("col_site", "col_factor", "col_rep"))) %>%
      ncol(.) + 1

    pca <- stats::prcomp(to_pca[begin:ncol(to_pca)], scale = TRUE)

    test <- vegan::adonis2(to_pca[begin:ncol(to_pca)] ~ as.factor(to_pca$col_factor), method = dist)

    prop_pca <- pca$sdev^2 / sum(pca$sdev^2)
    tmp_data <- get_data_pca_plot(pca_obj = pca)
    directions <- tmp_data$directions

    data_pars <- data_pca %>%
      dplyr::select(col_pars, type_par) %>%
      dplyr::distinct()

    directions <- dplyr::left_join(directions, data_pars, by = c("varname" = "col_pars"))
    scores_tmp <- tmp_data$scores
    scores <- dplyr::bind_cols(to_pca, scores_tmp)

    directions$gr <- "constante"

    # Colores dinámicos para el factor
    n_factor <- length(unique(scores$col_factor))
    if (n_factor <= 20) {
      col_pca <- ggsci::pal_d3("category20")(n_factor)
    } else {
      base_colors <- ggsci::pal_d3("category20")(20)
      col_pca <- grDevices::colorRampPalette(base_colors)(n_factor)
    }
    names(col_pca) <- unique(scores$col_factor)

    if (!is.null(levels_factor)) {
      order_grupo <- levels_factor
    } else {
      order_grupo <- scores %>%
        dplyr::pull(col_factor) %>%
        unique() %>%
        sort()
    }

    # Reordenar colores
    names(col_pca) <- order_grupo
    col_pca <- col_pca[order_grupo[order_grupo %in% names(col_pca)]]
    scores$gr <- factor(scores$col_factor, levels = order_grupo)
    R2 <- round(test$R2[1] * 100, 1)
    pval <- format.pval(test$`Pr(>F)`[1], digits = 3, eps = 0.001)
    subtitle <- glue::glue("PERMANOVA por {stringr::str_to_lower(col_factor)}: R² = {R2}%, valor-p = {pval}")

    p <- ggplot2::ggplot(data = scores, mapping = ggplot2::aes(x = xvar, y = yvar, fill = gr, group = gr)) +
      ggplot2::stat_ellipse(level = 0.68, geom = "polygon") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
      ggplot2::labs(
        x = glue::glue("Primera Componente Principal ({scales::percent(prop_pca[1])})"),
        y = glue::glue("Segunda Componente Principal ({scales::percent(prop_pca[2])})"),
        title = glue::glue("Análisis multivariado (PCA) para parámetros medidos en {matrix}"),
        subtitle = subtitle
      ) +
      ggplot2::scale_fill_manual(name = stringr::str_to_sentence(col_factor), values = ggplot2::alpha(col_pca, 0.2), labels = names(col_pca)) +
      ggplot2::geom_segment(
        data = directions,
        mapping = ggplot2::aes(x = 0, y = 0, xend = xvar, yend = yvar, color = type_par),
        arrow = ggplot2::arrow(length = ggplot2::unit(1 / 2, "picas")),
        inherit.aes = FALSE # Importante para no heredar groups
      ) +
      ggplot2::geom_text(
        data = directions,
        mapping = ggplot2::aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust, color = type_par),
        size = 3, inherit.aes = FALSE
      ) +
      ggplot2::scale_color_manual(name = "Tipo parámetro", values = cols_type, breaks = names(cols_type)) +
      ggplot2::geom_point(data = scores, mapping = ggplot2::aes(x = xvar, y = yvar), show.legend = FALSE) +
      ggplot2::geom_text(data = scores, mapping = ggplot2::aes(x = xvar, y = yvar, label = col_site), nudge_y = 0.1, size = 2.5) +
      ggplot2::scale_x_continuous(expand = c(0.1, 0.1)) +
      ggplot2::scale_y_continuous(expand = c(0.1, 0.1)) +
      ggplot2::theme_bw(base_family = "Arial") +
      ggplot2::guides(
        fill = ggplot2::guide_legend(order = 1),
        color = ggplot2::guide_legend(order = 2, override.aes = list(label = " "))
      ) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }


  tryCatch(
    {
      ggplot2::ggsave(filename = output_name, plot = p, device = "png", width = width, height = height, dpi = 300)

      if (save_rds) {
        rds_name <- stringr::str_replace(output_name, "\\.(png|jpg|jpeg|pdf|tiff)$", ".rds")
        if (rds_name == output_name) rds_name <- paste0(output_name, ".rds")
        saveRDS(plot, file = rds_name)
        message(paste("Objeto R con el pca guardado en:", rds_name))
      }
    },
    error = function(e) warning("Error guardando imagen: ", e$message)
  )

  return(p)
}

###############################################################################
#' @title Modelos Lineales Generalizados (GLM) por Grupos
#' @description Ajusta GLMs para múltiples parámetros simultáneamente, calcula el análisis de deviance (ANOVA) y genera tablas resumen con el porcentaje de deviance explicada (%DEM) y significancia.
#' @param data Data frame con los datos.
#' @param col_pars String. Nombre de la columna que identifica el parámetro (variable de agrupación).
#' @param col_value String. Nombre de la columna con la variable respuesta (numérica).
#' @param col_factor Vector de caracteres. Nombres de las columnas que actuarán como predictores (factores).
#' @param interaction Vector de caracteres (Opcional). Interacciones explícitas (ej. "FactorA:FactorB").
#' @param output_name. Ruta/Nombre base para guardar los archivos de salida.
#' @param family String. Familia del GLM. Opciones: "Gamma", "Gaussian", "Quasipoisson".
#' @param dec Entero. Número de decimales para el redondeo. Por defecto 2.
#' @return Una lista con dos data frames: `summary_dem` (% Deviance Explicada) y `summary_raw` (Tabla completa con valores p).
#' @export get_glm
#' @importFrom dplyr select rename mutate group_by group_split filter relocate case_when rowwise ungroup pull all_of
#' @importFrom purrr map set_names imap_dfr
#' @importFrom stats as.formula glm anova gaussian Gamma quasipoisson na.omit
#' @importFrom janitor clean_names
#' @importFrom tidyr pivot_wider
#' @importFrom readr write_csv
#' @importFrom stringr str_remove
#' @importFrom glue glue
#' @importFrom rlang sym "!!" "!!!"
get_glm <- function(data,
                    col_pars,
                    col_value,
                    col_factor,
                    interaction = NULL,
                    output_name = NULL,
                    family = c("Gaussian", "Gamma", "Quasipoisson"),
                    dec = 2) {
  op <- options(scipen = 999)
  on.exit(options(op))

  # 1. Validación y Configuración
  family <- match.arg(family) # Valida que family sea una de las opciones permitidas

  # Selección y renombrado seguro
  # Mantenemos los nombres originales de los factores, pero estandarizamos respuesta y ID
  data_clean <- data %>%
    dplyr::select(dplyr::all_of(c(col_pars, col_value, col_factor))) %>%
    dplyr::rename(
      col_pars_internal = !!rlang::sym(col_pars),
      col_value_internal = !!rlang::sym(col_value)
    )

  # Ajuste para Gamma (valores deben ser > 0)
  if (family == "Gamma" && any(data_clean$col_value_internal <= 0, na.rm = TRUE)) {
    warning("Se encontraron valores <= 0 para familia Gamma. Se ajustarán sumando 1e-6.")
    data_clean <- data_clean %>%
      dplyr::mutate(col_value_internal = ifelse(col_value_internal <= 0, col_value_internal + 1e-6, col_value_internal))
  }

  # 2. Construcción de la Fórmula
  # Usamos los nombres originales de los factores que están ahora en data_clean
  predictors <- paste(col_factor, collapse = " + ")

  if (!is.null(interaction)) {
    # Validar que los términos de interacción existan en los factores
    inter_terms <- unlist(strsplit(interaction, ":"))
    if (!all(inter_terms %in% col_factor)) {
      stop("Todos los términos en `interaction` deben estar presentes en `col_factor`.")
    }
    interaction_str <- paste(interaction, collapse = " + ")
    formula_str <- paste("col_value_internal ~", predictors, "+", interaction_str)
  } else {
    formula_str <- paste("col_value_internal ~", predictors)
  }

  formula_obj <- stats::as.formula(formula_str)

  # 3. Ajuste de Modelos (Iteración por parámetro)
  # Dividir datos
  data_split <- data_clean %>%
    dplyr::group_by(col_pars_internal) %>%
    dplyr::group_split() %>%
    purrr::set_names(purrr::map(., ~ unique(.x$col_pars_internal)))

  # Definir función de familia
  glm_family <- switch(family,
    "Gaussian" = stats::gaussian(link = "identity"),
    "Gamma" = stats::Gamma(link = "log"),
    "Quasipoisson" = stats::quasipoisson(link = "log")
  )

  # Ajustar modelos con manejo de errores (tryCatch)
  models_fit <- purrr::map(data_split, function(df) {
    tryCatch(
      {
        stats::glm(formula = formula_obj, data = df, na.action = stats::na.omit, family = glm_family)
      },
      error = function(e) {
        warning(paste("Falló el ajuste para:", unique(df$col_pars_internal), "-", e$message))
        return(NULL)
      }
    )
  })

  # Filtrar modelos nulos (los que fallaron)
  models_fit <- purrr::keep(models_fit, ~ !is.null(.x))

  if (length(models_fit) == 0) stop("Ningún modelo pudo ser ajustado.")

  # 4. Extracción de ANOVA y Tabla Resumen
  # Pre-calcular null deviances para eficiencia
  null_deviances <- purrr::map(models_fit, ~ .x$null.deviance)

  anovas <- purrr::map(models_fit, ~ stats::anova(.x, test = "Chisq"))

  glm_table <- purrr::imap_dfr(anovas, function(anova_obj, par_name) {
    df <- as.data.frame(anova_obj)
    df$Parametros <- par_name
    df$efecto <- rownames(df)
    df$Null_deviance <- null_deviances[[par_name]]
    return(df)
  })

  if (nrow(glm_table) == 0) stop("No se pudo generar la tabla ANOVA.")

  # Limpieza y formateo de tabla RAW (Significancia)
  glm_table_processed <- glm_table %>%
    dplyr::relocate(efecto) %>%
    dplyr::filter(!is.na(efecto), efecto != "NULL") %>%
    dplyr::mutate(
      significancia = dplyr::case_when(
        is.na(`Pr(>Chi)`) ~ NA_character_,
        `Pr(>Chi)` <= 0.001 ~ "***",
        `Pr(>Chi)` <= 0.01 ~ "**",
        `Pr(>Chi)` <= 0.05 ~ "*",
        TRUE ~ "ns"
      )
    )

  # Limpieza de nombres con janitor
  glm_table_raw <- glm_table_processed %>%
    janitor::clean_names()

  # 5. Cálculo de % Deviance Explained (%DEM)
  glm_table_clean <- glm_table_raw %>%
    dplyr::mutate(expl_percent = round((deviance / null_deviance) * 100, dec)) %>%
    dplyr::select(efecto, parametros, expl_percent) %>%
    tidyr::pivot_wider(names_from = "efecto", values_from = "expl_percent") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(`%DEM` = sum(dplyr::c_across(2:ncol(.)), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::rename("Parámetros" = parametros)

  # 6. Guardado de Archivos (Opcional)
  if (!is.null(output_name)) {
    # Asegurar extensión csv
    if (!grepl("\\.csv$", output_name)) output_name <- paste0(output_name, ".csv")

    readr::write_csv(x = glm_table_clean, file = output_name)

    out_raw_name <- stringr::str_replace(output_name, "\\.csv$", "_signtable.csv")
    readr::write_csv(x = glm_table_raw, file = out_raw_name)

    message(glue::glue("Archivos guardados: {output_name} y {out_raw_name}"))
  }

  # Retornar lista para uso programático
  return(list(
    summary_dem = glm_table_clean,
    summary_raw = glm_table_raw
  ))
}
###############################################################################
#' @title Generar Boxplots con GLM Externo (Campos Específicos)
#' @description Crea boxplots paginados. Usa una tabla externa (`stats_df`) con campos `parametros` y `pr_chi` para etiquetar el GLM.
#' @param data Data frame con los datos crudos.
#' @param stats_df Data frame con los resultados del GLM. Debe contener las columnas **`parametros`** y **`pr_chi`**.
#' @param col_x String. Columna eje X.
#' @param col_y String. Columna eje Y.
#' @param col_pars String. Columna de parámetros.
#' @param col_units String. Columna de unidades.
#' @param n_col Entero. Columnas facet.
#' @param output_name String. Archivo salida.
#' @param width Numérico. Ancho.
#' @param height Numérico. Alto.
#' @param levels_factor Vector. Orden factor X.
#' @param title String. Título.
#' @param label_x String. Etiqueta X.
#' @param label_y String. Etiqueta Y.
#' @param add_test Lógico. Si TRUE, activa subtítulo GLM, etiquetas GLM y test de Dunn (condicional).
#' @param save_rds Lógico. Guardar RDS.
#' @export plot_boxplot
#' @importFrom dplyr select rename mutate group_by summarise left_join filter pull distinct group_modify ungroup inner_join
#' @importFrom ggplot2 ggplot aes geom_boxplot facet_wrap labs theme_linedraw theme element_text geom_text scale_y_continuous ggsave element_rect expansion scale_x_discrete
#' @importFrom rlang sym "!!"
#' @importFrom stats quantile
#' @importFrom glue glue
#' @importFrom tools file_path_sans_ext
#' @importFrom scales pvalue
#' @importFrom rstatix dunn_test add_xy_position
#' @importFrom ggpubr stat_pvalue_manual
#' @importFrom tibble tibble
plot_boxplot <- function(data,
                         col_x,
                         col_y,
                         col_pars,
                         col_units,
                         stats_df = NULL,
                         n_col = 3,
                         output_name = "boxplot.png",
                         width = 10,
                         height = 8,
                         levels_factor = NULL,
                         title = "",
                         label_x = "Sitios",
                         label_y = "Valor",
                         add_test = FALSE,
                         save_rds = FALSE) {
  # --- 1. Preparación de Datos ---
  vars <- c(col_x, col_y, col_pars, col_units)

  data_plot <- data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::rename(
      col_x = !!rlang::sym(col_x),
      col_y = !!rlang::sym(col_y),
      col_pars = !!rlang::sym(col_pars),
      col_units = !!rlang::sym(col_units)
    ) %>%
    dplyr::mutate(
      col_label = dplyr::if_else(
        is.na(col_units),
        as.character(col_pars),
        glue::glue("{col_pars} {col_units}")
      )
    )

  if (!is.null(levels_factor)) {
    data_plot$col_x <- factor(data_plot$col_x, levels = levels_factor)
  } else {
    data_plot$col_x <- as.factor(data_plot$col_x)
  }

  # Recorte visual (95%)
  limites <- data_plot %>%
    dplyr::group_by(col_label) %>%
    dplyr::summarise(
      lim_superior = stats::quantile(col_y, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  data_plot <- data_plot %>%
    dplyr::left_join(limites, by = "col_label") %>%
    dplyr::mutate(col_y_plot = pmin(col_y, lim_superior))

  # --- 2. Preparar Etiquetas GLM (Desde stats_df) ---
  stats_ready <- NULL
  subtitle_text <- "Parámetros con efecto significativo del factor considerado (GLM)" # Subtítulo fijo

  if (add_test && !is.null(stats_df)) {
    # Validar que existan las columnas requeridas
    if (!all(c("parametros", "pr_chi") %in% names(stats_df))) {
      stop("La tabla 'stats_df' debe contener las columnas 'parametros' y 'pr_chi'.")
    }

    # Mapear etiquetas completas (param + unidad) para el join
    mapping_labels <- data_plot %>%
      dplyr::select(col_pars, col_label) %>%
      dplyr::distinct()

    # Procesar la tabla externa
    stats_ready <- stats_df %>%
      dplyr::select(col_pars = parametros, pr_chi) %>% # Renombrar para el join y seleccionar p-value
      dplyr::inner_join(mapping_labels, by = "col_pars") %>%
      dplyr::mutate(
        # Crear la etiqueta con el formato solicitado
        label_test = glue::glue("Modelo GLM: valor-p = {scales::pvalue(pr_chi, accuracy = 0.001)}")
      )
  }

  # --- 3. Función Constructora del Gráfico ---
  build_plot <- function(d_plot, d_stats_glm) {
    n_factor <- length(unique(d_plot$col_x))

    # Base Plot (Estilo Original)
    p <- ggplot2::ggplot(d_plot, ggplot2::aes(x = col_x, y = col_y_plot)) +
      ggplot2::geom_boxplot(
        na.rm = TRUE,
        outlier.colour = "black",
        outlier.shape = 19,
        outlier.alpha = 0.4,
        fill = "#E69F00"
      ) +
      ggplot2::facet_wrap(~col_label, scales = "free_y", ncol = n_col) +
      ggplot2::labs(x = label_x, y = label_y, title = title, subtitle = subtitle_text) +
      ggplot2::theme_linedraw(base_family = "Arial") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
        strip.text = ggplot2::element_text(face = "bold", colour = "white"),
        strip.background = ggplot2::element_rect(fill = "#2c3e50")
      )

    # --- LOGICA CONDICIONAL DE TEST ---
    if (add_test) {
      # 1. DUNN TEST (Interno y Condicional: <= 4 niveles)
      if (n_factor <= 4) {
        dunn_res <- d_plot %>%
          dplyr::group_by(col_label) %>%
          dplyr::group_modify(~ tryCatch(
            rstatix::dunn_test(col_y ~ col_x, data = .x, p.adjust.method = "bonferroni"),
            error = function(e) tibble::tibble()
          )) %>%
          dplyr::ungroup()

        if (nrow(dunn_res) > 0) {
          # Corrección visual de altura
          max_vis <- d_plot %>%
            dplyr::group_by(col_label) %>%
            dplyr::summarise(mx = max(col_y_plot, na.rm = TRUE), .groups = "drop")

          dunn_res <- dunn_res %>%
            rstatix::add_xy_position(x = "col_x", formula = col_y ~ col_x, data = d_plot, step.increase = 0) %>%
            dplyr::left_join(max_vis, by = "col_label") %>%
            dplyr::group_by(col_label) %>%
            dplyr::mutate(
              step = dplyr::row_number(),
              y.position = mx + (mx * 0.05) + (step * 0.08 * mx)
            ) %>%
            dplyr::ungroup()

          p <- p + ggpubr::stat_pvalue_manual(dunn_res, label = "p.adj.signif", hide.ns = TRUE, size = 3)
          p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.25)))
          p <- p + ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0.2, 0.2)))
        }
      }

      # 2. TEXTO GLM (Desde Tabla Externa stats_df)
      if (!is.null(d_stats_glm) && nrow(d_stats_glm) > 0) {
        if (n_factor <= 4) {
          # Izquierda (si hay barras Dunn)
          p <- p + ggplot2::geom_text(
            data = d_stats_glm,
            ggplot2::aes(x = -Inf, y = Inf, label = label_test),
            hjust = -0.1, vjust = 1.5, size = 3, fontface = "italic", inherit.aes = FALSE
          )
        } else {
          # Derecha (si NO hay barras Dunn)
          p <- p + ggplot2::geom_text(
            data = d_stats_glm,
            ggplot2::aes(x = Inf, y = Inf, label = label_test),
            hjust = 1.1, vjust = 1.1, size = 3.5, color = "black", fontface = "italic", inherit.aes = FALSE
          )
        }
      }
    }

    return(p)
  }

  # --- 4. Lógica de Paginación ---
  labels_unicos <- unique(data_plot$col_label)
  n_pars <- length(labels_unicos)

  if (n_pars > 10) {
    n_grupos <- ceiling(n_pars / 10)
    grupos <- split(labels_unicos, sort(rep(1:n_grupos, length.out = n_pars)))

    for (i in seq_along(grupos)) {
      sub_data <- data_plot %>% dplyr::filter(col_label %in% grupos[[i]])
      sub_stats <- if (!is.null(stats_ready)) stats_ready %>% dplyr::filter(col_label %in% grupos[[i]]) else NULL

      p_page <- build_plot(sub_data, sub_stats)

      fname <- glue::glue("{tools::file_path_sans_ext(output_name)}_{i}.png")
      tryCatch(
        {
          ggplot2::ggsave(filename = fname, plot = p_page, width = width, height = height, dpi = 300)
          if (save_rds) {
            rds_name <- glue::glue("{tools::file_path_sans_ext(output_name)}_{i}.rds")
            saveRDS(p_page, file = rds_name)
          }
        },
        error = function(e) warning(paste("Error página", i, ":", e$message))
      )
    }
    return(invisible(NULL))
  } else {
    # Caso Único
    p_single <- build_plot(data_plot, stats_ready)

    tryCatch(
      {
        ggplot2::ggsave(filename = output_name, plot = p_single, width = width, height = height, dpi = 300)
        if (save_rds) saveRDS(p_single, stringr::str_replace(output_name, "\\.[a-z]+$", ".rds"))
      },
      error = function(e) warning(e$message)
    )

    return(invisible(p_single))
  }
}

###############################################################################
#' @title Graficar Distribución de Granulometría (Stacked Barplot)
#' @description Genera un gráfico de barras apiladas para visualizar la composición granulométrica por sitio. Permite agrupar por factores (ej. Zonas) y separar por réplicas.
#' @param data Data frame con los datos.
#' @param col_pars String. Nombre de la columna con los tipos de grano (ej. "Parametro").
#' @param col_site String. Nombre de la columna con los sitios.
#' @param col_value String. Nombre de la columna con el porcentaje/valor.
#' @param col_factor String (Opcional). Columna para agrupar en facetas (columnas).
#' @param col_rep String (Opcional). Columna para agrupar en facetas (filas).
#' @param ord_site Vector (Opcional). Orden específico para los sitios.
#' @param levels_factor Vector (Opcional). Orden específico para el factor de agrupación.
#' @param width Numérico. Ancho de la imagen guardada.
#' @param height Numérico. Alto de la imagen guardada.
#' @param output_name String. Nombre del archivo de salida.
#' @param save_rds Lógico. Si es TRUE, guarda también el objeto R (.rds). Por defecto FALSE.
#' @return Un objeto ggplot.
#' @export plot_granulometria
#' @importFrom dplyr select rename mutate filter all_of
#' @importFrom ggplot2 ggplot geom_col labs scale_fill_manual guides guide_legend theme_bw theme element_text element_rect facet_grid ggsave
#' @importFrom ggsci pal_npg
#' @importFrom rlang sym "!!" "!!!"
#' @importFrom stats setNames
plot_granulometria <- function(data,
                               col_pars,
                               col_site,
                               col_value,
                               col_factor = NULL,
                               col_rep = NULL,
                               ord_site = NULL,
                               levels_factor = NULL,
                               width = 8,
                               height = 6,
                               output_name = "plot_granulometria.png",
                               save_rds = FALSE) {
  # 1. Definición de Parámetros Estándar
  pars_gran_all <- c("LIM", "AMF", "AF", "AM", "AG", "AMG", "GRAN", "GUIJ")

  # 2. Selección y Renombrado
  vars_select <- c(col_site, col_pars, col_value, col_factor, col_rep)

  data_clean <- data %>%
    dplyr::select(dplyr::all_of(vars_select)) %>%
    dplyr::rename(
      col_site = !!rlang::sym(col_site),
      col_pars = !!rlang::sym(col_pars),
      col_value = !!rlang::sym(col_value),
      !!!if (!is.null(col_factor)) stats::setNames(col_factor, "col_factor") else NULL,
      !!!if (!is.null(col_rep)) stats::setNames(col_rep, "col_rep") else NULL
    )

  # 3. Filtrado y Ordenamiento de Datos
  # Intersección: Solo mantenemos los parámetros que están en la lista oficial de granulometría
  pars_present <- intersect(pars_gran_all, unique(data_clean$col_pars))

  if (length(pars_present) == 0) {
    warning("No se encontraron parámetros de granulometría estándar (LIM, AMF, etc.) en los datos.")
  }

  data_plot <- data_clean %>%
    dplyr::filter(col_pars %in% pars_present) %>%
    dplyr::mutate(
      col_pars = factor(col_pars, levels = pars_present)
    )

  # Aplicar orden a Sitios
  if (!is.null(ord_site)) {
    # Validar que los niveles existan
    valid_sites <- intersect(ord_site, unique(data_plot$col_site))
    data_plot$col_site <- factor(data_plot$col_site, levels = valid_sites)
  } else {
    data_plot$col_site <- as.factor(data_plot$col_site)
  }

  # Aplicar orden a Factor (si existe)
  if (!is.null(col_factor)) {
    if (!is.null(levels_factor)) {
      data_plot$col_factor <- factor(data_plot$col_factor, levels = levels_factor)
    } else {
      data_plot$col_factor <- as.factor(data_plot$col_factor)
    }
  }

  # 4. Configuración Visual
  # Rotación de etiquetas eje X según cantidad de sitios
  n_sites <- length(unique(data_plot$col_site))
  angle_x <- if (n_sites <= 13) 0 else 90
  hjust_x <- if (n_sites <= 13) 0.5 else 1
  vjust_x <- if (n_sites <= 13) 0 else 0.5

  # Paleta de colores (NPG - Nature Publishing Group)
  cols_grano <- ggsci::pal_npg("nrc")(length(pars_present))
  names(cols_grano) <- pars_present

  # 5. Generación del Gráfico
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = col_site, y = col_value, fill = col_pars)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::labs(
      x = "Sitios",
      y = "Tamaño de grano (%)",
      title = "Distribución espacial de las fracciones granulométricas"
    ) +
    ggplot2::scale_fill_manual("Fracción granulométrica", values = cols_grano) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      title.position = "left",
      title = "Fracción granulométrica"
    )) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Arial"), # Arial suele dar problemas en Linux/Docker, sans es seguro
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      strip.text = ggplot2::element_text(face = "bold", colour = "white"),
      strip.background = ggplot2::element_rect(fill = "#2c3e50"),
      axis.text.x = ggplot2::element_text(angle = angle_x, hjust = hjust_x, vjust = vjust_x),
      legend.title = ggplot2::element_text(angle = 90, hjust = 0.5, vjust = 0.5)
    )

  # 6. Faceting Condicional
  if (!is.null(col_factor)) {
    if (is.null(col_rep)) {
      # Solo factor (columnas)
      p <- p + ggplot2::facet_grid(~col_factor, scales = "free_x", space = "free_x")
    } else {
      # Factor (columnas) y Replicas (filas)
      p <- p + ggplot2::facet_grid(col_rep ~ col_factor, scales = "free", space = "free", switch = "y")
    }
  } else if (!is.null(col_rep)) {
    # Solo replicas (filas) - Caso raro pero posible
    p <- p + ggplot2::facet_grid(col_rep ~ ., scales = "free", space = "free")
  }

  # --- Bloque de Guardado Actualizado ---
  tryCatch(
    {
      # Guardar PNG
      ggplot2::ggsave(filename = output_name, plot = p, width = width, height = height, dpi = 300)

      # Guardar RDS si se solicita
      if (save_rds) {
        rds_name <- stringr::str_replace(output_name, "\\.(png|jpg|jpeg|pdf|tiff)$", ".rds")
        # Si el regex falla (ej. no tiene extension), agrega .rds
        if (rds_name == output_name) rds_name <- paste0(output_name, ".rds")

        saveRDS(p, file = rds_name)
        message(paste("Objeto R guardado en:", rds_name))
      }
    },
    error = function(e) warning("Error guardando archivos: ", e$message)
  )

  return(p)
}
#######################################################################################
#' @title Gráfico de Series Temporales con Detección de Outliers (DBSCAN)
#' @description Genera gráficos de línea por sitio/campaña, detectando anomalías mediante DBSCAN y el método "Kneedle" para epsilon óptimo. Permite resaltar valores bajo límite de detección (BLD).
#' @param data Data frame con los datos.
#' @param col_value String. Nombre de la columna con los valores numéricos.
#' @param col_pars String. Nombre de la columna del parámetro.
#' @param col_x String. Nombre de la columna eje X (ej. "Campaña" numérico o factor).
#' @param col_site String. Nombre de la columna de sitios.
#' @param col_year String. Nombre de la columna de año.
#' @param col_units String. Nombre de la columna de unidades.
#' @param matrix String. Nombre de la matrix (para títulos).
#' @param col_group String (Opcional). Columna para agrupación adicional (ej. Estrato).
#' @param ord_site Vector (Opcional). Orden de los sitios.
#' @param output_name String. Nombre del archivo de salida.
#' @param width Numérico. Ancho imagen.
#' @param height Numérico. Alto imagen.
#' @param legend_label String. Título de la leyenda de color.
#' @param xlabel String. Título del eje x.
#' @param ylabel String. Título del eje y.
#' @param pars_annot Vector (Opcional). Parámetros a excluir de la anotación de asteriscos BLD.
#' @param n_col Entero. Columnas en facet_wrap.
#' @param type_plot String. "annotated" (muestra outliers y BLD) o "clean" (filtra outliers y BLD).
#' @param save_rds Lógico. Guardar objeto ggplot.
#' @export line_plot_by_time
#' @importFrom dplyr select rename mutate filter group_split bind_rows left_join row_number any_of all_of distinct case_when
#' @importFrom ggplot2 ggplot geom_line geom_point geom_text facet_wrap labs theme_linedraw theme element_text scale_x_continuous scale_color_manual coord_cartesian
#' @importFrom dbscan kNN dbscan
#' @importFrom ggsci scale_color_d3
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom readr write_csv
#' @importFrom rlang sym "!!" "!!!" set_names
line_plot_by_time <- function(data,
                              col_value,
                              col_pars,
                              col_x,
                              col_site,
                              col_year,
                              matrix,
                              col_units,
                              col_group = NULL,
                              ord_site = NULL,
                              output_name,
                              width = 8,
                              height = 8,
                              legend_label = "Sitio",
                              pars_annot = NULL,
                              n_col = 3,
                              type_plot = "annotated",
                              save_rds = FALSE) {
  op <- options(scipen = 999)
  on.exit(options(op))

  # 1. Funciones Auxiliares Internas ------------------------------------------

  # Algoritmo Kneedle para encontrar el "codo" en la curva de distancias
  find_eps_kneedle <- function(distancias) {
    y <- sort(distancias)
    x <- seq_along(y)
    if (length(y) < 2) {
      return(0.1)
    } # Fallback seguro

    base_line <- data.frame(x = c(1, length(y)), y = c(y[1], y[length(y)]))
    a <- (base_line$y[2] - base_line$y[1]) / (base_line$x[2] - base_line$x[1])
    b <- base_line$y[1] - a * base_line$x[1]

    dist_to_line <- abs(a * x - y + b) / sqrt(a^2 + 1)
    y[which.max(dist_to_line)]
  }

  # Wrapper para detección de outliers por parámetro
  fn_get_ol <- function(df_subset) {
    # Requiere al menos unos pocos puntos para DBSCAN
    if (nrow(df_subset) < 5) {
      df_subset$ol_dbscan <- FALSE
      return(df_subset %>% dplyr::select(inx_row, ol_dbscan))
    }

    data_input <- scale(df_subset[, "col_y"])

    # Calcular distancias kNN (k=4 para minPts=5)
    knn_res <- dbscan::kNN(data_input, k = 4)
    d_k <- apply(knn_res$dist, 1, max)

    eps_objetivo <- find_eps_kneedle(d_k)

    # Evitar epsilon 0 si todos los valores son iguales
    if (eps_objetivo == 0) eps_objetivo <- 1e-6

    modelo <- dbscan::dbscan(data_input, eps = eps_objetivo, minPts = 5)

    # Cluster 0 es ruido (outlier)
    df_subset$ol_dbscan <- modelo$cluster == 0
    df_subset %>% dplyr::select(inx_row, ol_dbscan)
  }

  # 2. Preparación de Datos ---------------------------------------------------

  # Normalizar type_plot
  tp <- if (type_plot %in% c("annotated", "annonated")) "annotated" else "clean"

  # Manejo de is_BLD1 (Si no existe, asumimos que nada es BLD)
  if (!"is_BLD1" %in% names(data)) {
    data$is_BLD1 <- FALSE
  }

  vars_select <- c(col_site, col_x, col_pars, col_value, col_units, col_year, "is_BLD1")
  if (!is.null(col_group)) vars_select <- c(vars_select, col_group)

  # Renombrado estándar y creación de índice
  data_plot <- data %>%
    dplyr::select(dplyr::all_of(vars_select)) %>%
    dplyr::rename(
      col_site = !!rlang::sym(col_site),
      col_x = !!rlang::sym(col_x),
      col_pars = !!rlang::sym(col_pars),
      col_y = !!rlang::sym(col_value), # Usamos col_y internamente como valor
      col_year = !!rlang::sym(col_year),
      col_units = !!rlang::sym(col_units),
      !!!if (!is.null(col_group)) rlang::set_names(col_group, "col_group") else NULL
    ) %>%
    dplyr::mutate(
      inx_row = dplyr::row_number(),
      col_label = dplyr::if_else(is.na(col_units), as.character(col_pars), glue::glue("{col_pars} {col_units}"))
    ) %>%
    dplyr::filter(!is.na(col_y))

  # Aplicar orden de sitios si existe
  if (!is.null(ord_site)) {
    # Intersección para evitar niveles huerfanos
    valid_sites <- intersect(ord_site, unique(data_plot$col_site))
    data_plot$col_site <- factor(data_plot$col_site, levels = valid_sites)
  } else {
    data_plot$col_site <- as.factor(data_plot$col_site)
  }

  # 3. Detección de Outliers --------------------------------------------------
  # Se calculan siempre sobre el dataset completo antes de filtrar para graficar
  data_outliers <- data_plot %>%
    dplyr::select(col_y, dplyr::any_of("col_group"), col_pars, inx_row) %>%
    dplyr::group_split(col_pars) %>%
    purrr::map(fn_get_ol) %>%
    dplyr::bind_rows()

  data_plot <- dplyr::left_join(data_plot, data_outliers, by = "inx_row")

  # Títulos dinámicos
  min_y <- min(data_plot$col_year, na.rm = TRUE)
  max_y <- max(data_plot$col_year, na.rm = TRUE)
  title_main <- glue::glue("Concentración de parámetros en {matrix}")
  subtitle_main <- glue::glue("Periodo: {min_y}–{max_y}")

  # 4. Lógica de Graficado (Ramificación) -------------------------------------

  if (tp == "clean") {
    # --- MODO CLEAN: Filtramos outliers y BLD ---
    data_viz <- data_plot %>%
      dplyr::filter(!is_BLD1, !ol_dbscan) %>%
      dplyr::mutate(
        group_aes = if ("col_group" %in% names(.)) interaction(col_site, col_group, drop = TRUE) else col_site
      )

    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = data_viz,
        ggplot2::aes(x = col_x, y = col_y, group = group_aes, color = col_site)
      )
  } else {
    # --- MODO ANNOTATED: Mostramos todo y exportamos ---

    # Exportación CSV (Datos crudos + etiquetas outliers)
    # Filtramos parámetros excluidos SOLO para la exportación si se desea,
    # aunque generalmente en annotated se quiere ver todo.
    data_export <- data_plot

    # Revertir nombres para el CSV
    cols_orig <- c("Sitio", "Campaña", "Sigla", "Valor", "Unidades", "Año", "is_BLD1")
    cols_curr <- c("col_site", "col_x", "col_pars", "col_y", "col_units", "col_year", "is_BLD1")

    if ("col_group" %in% names(data_export)) {
      cols_orig <- c(cols_orig, "Estrato/Grupo")
      cols_curr <- c(cols_curr, "col_group")
    }
    cols_curr <- c(cols_curr, "ol_dbscan")
    cols_orig <- c(cols_orig, "ol_dbscan") # Añadimos flag outlier al csv

    # Crear dataset para guardar
    csv_out <- data_export %>%
      dplyr::select(dplyr::all_of(cols_curr)) %>%
      rlang::set_names(cols_orig)

    matrix_safe <- stringr::str_replace_all(matrix, " ", "_")
    file_annot <- glue::glue("data_{matrix_safe}_PROMNA_annotated.csv")

    # Guardamos CSV auxiliar
    tryCatch(readr::write_csv(csv_out, file_annot), error = function(e) warning("No se pudo guardar CSV anotado"))

    # Preparar datos para ggplot
    data_viz <- data_plot %>%
      dplyr::mutate(
        group_aes = if ("col_group" %in% names(.)) interaction(col_site, col_group, drop = TRUE) else col_site
      )

    p <- ggplot2::ggplot() +
      # Puntos Rojos (Outliers)
      ggplot2::geom_point(
        data = data_viz %>% dplyr::filter(ol_dbscan, is.null(pars_annot) | !(col_pars %in% pars_annot)),
        ggplot2::aes(x = col_x, y = col_y),
        color = "#B22222", size = 1.5
      ) +
      # Líneas Conectoras (Serie completa)
      ggplot2::geom_line(
        data = data_viz,
        ggplot2::aes(x = col_x, y = col_y, group = group_aes, color = col_site)
      ) +
      ggplot2::labs(
        caption = paste0(
          "\u2022 Puntos rojos: Valores anómalos (DBSCAN)\n",
          "\u2022 Asteriscos (*): Parámetros mayoritariamente BLD (>90%)"
        )
      )

    # Asteriscos Gigantes para BLD masivo
    if (!is.null(pars_annot) && length(pars_annot) > 0) {
      anotaciones <- data_viz %>%
        dplyr::filter(col_pars %in% pars_annot) %>%
        dplyr::distinct(col_label) %>%
        dplyr::mutate(x = Inf, y = Inf, label = "*")

      p <- p +
        ggplot2::geom_text(
          data = anotaciones,
          ggplot2::aes(x = x, y = y, label = label),
          inherit.aes = FALSE,
          color = "#225E7C",
          hjust = 1.1, vjust = 1.1,
          size = 10, fontface = "bold"
        )
    }
  }

  # 5. Estilo Común y Guardado ------------------------------------------------

  # Definir breaks enteros para campañas/años si son numéricos
  x_breaks <- if (is.numeric(data_plot$col_x)) {
    seq(min(data_plot$col_x, na.rm = TRUE), max(data_plot$col_x, na.rm = TRUE), length.out = 5) %>%
      round() %>%
      unique()
  } else {
    ggplot2::waiver()
  }

  p <- p +
    ggplot2::facet_wrap(~col_label, scales = "free_y", ncol = n_col) +
    ggplot2::labs(
      colour = legend_label,
      x = xlabel,
      y = ylabel,
      title = title_main,
      subtitle = subtitle_main
    ) +
    ggsci::scale_color_d3() +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::theme_linedraw(base_family = "Arial") +
    ggplot2::theme(
      # aspect.ratio = 0.6, # A veces conflictivo con facet_wrap libre
      plot.title = ggplot2::element_text(hjust = 0, face = "bold"),
      strip.text.x = ggplot2::element_text(face = "bold", size = 9),
      legend.position = "right",
      legend.box = "horizontal",
      legend.text = ggplot2::element_text(size = 8),
      plot.caption = ggplot2::element_text(hjust = 0, face = "italic", size = 9)
    )

  # Guardado
  tryCatch(
    {
      ggplot2::ggsave(filename = output_name, plot = p, dpi = 300, width = width, height = height)

      if (save_rds) {
        rds_name <- stringr::str_replace(output_name, "\\.[a-z]+$", ".rds")
        if (rds_name == output_name) rds_name <- paste0(output_name, ".rds")
        saveRDS(p, file = rds_name)
        message(paste("Objeto R guardado en:", rds_name))
      }
    },
    error = function(e) warning("Error al guardar gráfico: ", e$message)
  )

  return(p)
}
################################################################################
#' @title Calcular Excedencias de Normas Ambientales
#' @description Evalúa los datos frente a una tabla de normas, calculando frecuencias y porcentajes de excedencia. Maneja lógica específica para rangos (ej. pH) y límites superiores.
#'
#' @param data Data frame con los datos de mediciones.
#' @param norm_data Data frame con las normas. Debe contener columnas `limite_1` y `limite_2`.
#' @param col_pars String. Nombre de la columna de parámetros en `data` y `norm_data`.
#' @param col_value String. Nombre de la columna de valores en `data`.
#' @param col_group String (Opcional). Columna de agrupación (ej. "Zona", "Sitio").
#' @param output_name String. Nombre del archivo de salida (.tsv o .csv).
#'
#' @return Un data frame con el resumen de excedencias (invisible).
#' @export get_norm_exceedance
#' @importFrom dplyr select rename group_by summarise filter pull distinct mutate left_join case_when relocate n
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_detect
#' @importFrom scales percent
#' @importFrom readr write_tsv write_csv
#' @importFrom rlang sym "!!" "!!!"
get_norm_exceedance <- function(data,
                                norm_data,
                                col_pars,
                                col_value,
                                col_group = NULL,
                                output_name = "tabla_excedencias.tsv") {
  # 1. Preparación de Datos ---------------------------------------------------

  # Renombrado estándar para manipulación interna
  # Nota: Asumimos que col_pars existe en ambas tablas con el mismo nombre o el usuario ya lo gestionó.
  # Aquí estandarizamos 'data'
  vars_select <- c(col_pars, col_value, col_group)

  data_clean <- data %>%
    dplyr::select(dplyr::all_of(vars_select)) %>%
    dplyr::rename(
      col_pars_int = !!rlang::sym(col_pars),
      col_val_int = !!rlang::sym(col_value),
      col_group_int = !!rlang::sym(col_group)
    )

  # 2. Filtrado por Varianza (CV > 0) -----------------------------------------
  # Eliminar parámetros constantes o vacíos antes de cruzar
  selected_pars <- data_clean %>%
    dplyr::group_by(col_pars_int) %>%
    dplyr::summarise(
      prom = mean(col_val_int, na.rm = TRUE),
      desvest = stats::sd(col_val_int, na.rm = TRUE),
      cv_num = dplyr::if_else(prom == 0, 0, desvest / prom),
      .groups = "drop"
    ) %>%
    dplyr::filter(cv_num > 0) %>%
    dplyr::pull(col_pars_int)

  data_filt <- data_clean %>%
    dplyr::filter(col_pars_int %in% selected_pars)

  # 3. Cruce con Normas -------------------------------------------------------

  # Validar que la tabla de normas tenga la estructura esperada
  if (!all(c("limite_1", "limite_2") %in% names(norm_data))) {
    stop("La tabla 'norm_data' debe contener las columnas 'limite_1' y 'limite_2'.")
  }

  # Asegurar que el nombre del parámetro en norma coincida con el argumento col_pars
  # Si la norma usa otro nombre, el usuario debe renombrarlo antes o usar el mismo.
  # Asumimos que `col_pars` es la llave. Renombramos en norma temporalmente si es necesario,
  # pero lo más seguro es hacer el join dinámico.

  # Preparamos norma para el join
  norm_prep <- norm_data %>%
    dplyr::rename(col_pars_int = !!rlang::sym(col_pars))

  # Filtrar datos que tengan norma
  data_to_table <- data_filt %>%
    dplyr::filter(col_pars_int %in% unique(norm_prep$col_pars_int))

  data_final <- dplyr::left_join(data_to_table, norm_prep, by = "col_pars_int", relationship = "many-to-many")

  # 4. Evaluación de Excedencias ----------------------------------------------

  df_long <- data_final %>%
    tidyr::pivot_longer(
      cols = c("limite_1", "limite_2"),
      names_to = "tipo_limite_int",
      values_to = "limite_val"
    ) %>%
    dplyr::filter(!is.na(limite_val)) # Eliminar normas sin valor

  df_excede <- df_long %>%
    dplyr::mutate(
      excede = dplyr::case_when(
        # Lógica pH: Rango (Bajo inferior o Sobre superior)
        col_pars_int == "pH" & tipo_limite_int == "limite_1" & col_val_int < limite_val ~ 1,
        col_pars_int == "pH" & tipo_limite_int == "limite_2" & col_val_int > limite_val ~ 1,
        # Lógica General: Supera límite superior
        col_pars_int != "pH" & col_val_int > limite_val ~ 1,
        TRUE ~ 0
      )
    )

  # 5. Resumen y Etiquetado ---------------------------------------------------

  # Verificar si existe columna 'norma' en norm_data, sino crear dummy
  if (!"norma" %in% names(df_excede)) df_excede$norma <- "Norma"

  tabla_excedencia <- df_excede %>%
    dplyr::group_by(col_pars_int, norma, tipo_limite_int, col_group_int) %>%
    dplyr::summarise(
      N_muestras = dplyr::n(),
      N_excedencias = sum(excede),
      Porcentaje_excedencia = scales::percent(mean(excede), accuracy = 1),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Limite_desc = dplyr::case_when(
        # --- CASO ESPECIAL: pH ---
        col_pars_int == "pH" & stringr::str_detect(norma, "DS 144") & tipo_limite_int == "limite_1" ~ "Valor mínimo",
        col_pars_int == "pH" & stringr::str_detect(norma, "DS 144") & tipo_limite_int == "limite_2" ~ "Valor máximo",

        # --- REGLAS GENERALES ---
        stringr::str_detect(norma, "NSCA Quintero|DS 144") & tipo_limite_int == "limite_1" ~ "Valor máximo",
        stringr::str_detect(norma, "EPA") & tipo_limite_int == "limite_1" ~ "Agudo",
        stringr::str_detect(norma, "EPA") & tipo_limite_int == "limite_2" ~ "Crónico",
        stringr::str_detect(norma, "NOAA") & tipo_limite_int == "limite_1" ~ "Agudo",
        stringr::str_detect(norma, "NOAA") & tipo_limite_int == "limite_2" ~ "Crónico",
        TRUE ~ tipo_limite_int
      )
    ) %>%
    dplyr::relocate(Limite_desc, .after = norma) %>%
    dplyr::select(-tipo_limite_int) %>%
    dplyr::rename(
      Parámetro = col_pars_int,
      Norma = norma,
      Límite = Limite_desc,
      !!rlang::sym(col_group) := col_group_int, # Restaurar nombre original del grupo
      "N° de muestras" = N_muestras,
      "N° de excedencias" = N_excedencias,
      "Excedencias %" = Porcentaje_excedencia
    )

  # 6. Guardado y Retorno -----------------------------------------------------

  tryCatch(
    {
      # Detectar formato por extensión
      if (stringr::str_detect(output_name, "\\.csv$")) {
        readr::write_csv(tabla_excedencia, output_name)
      } else {
        # Por defecto TSV si no es CSV
        if (!stringr::str_detect(output_name, "\\.tsv$")) output_name <- paste0(output_name, ".tsv")
        readr::write_tsv(tabla_excedencia, output_name)
      }
    },
    error = function(e) warning("Error guardando tabla: ", e$message)
  )

  return(tabla_excedencia)
}
