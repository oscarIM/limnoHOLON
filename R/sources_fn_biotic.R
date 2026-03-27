# ---- 1. plot_treemap ---------------------------------------------------------
#' @title Generar Treemap de Composición Taxonómica
#' @description Crea un gráfico Treemap basado en abundancias. Soporta
#'   facetado, agrupación por sitio/zona y formateo específico de etiquetas.
#' @param data Data frame con los datos.
#' @param col_site String (Opcional). Nombre de la columna de sitios.
#' @param ord_site Vector (Opcional). Orden de los niveles de sitios.
#' @param col_N String. Nombre de la columna de abundancia o conteo.
#' @param col_facet String (Opcional). Nombre de la columna para facetar.
#' @param ord_facet Vector (Opcional). Orden de los niveles del facet.
#' @param col_zone String (Opcional). Nombre de la columna de zonas.
#' @param ord_zone Vector (Opcional). Orden de los niveles de zonas.
#' @param taxa_id String. Texto identificador del taxón superior para el título.
#' @param taxa_group String. Nombre de la columna con el grupo taxonómico.
#' @param width Numérico. Ancho de la imagen. Default 8.
#' @param height Numérico. Alto de la imagen. Default 10.
#' @param output_name String. Nombre del archivo de salida.
#' @param output_dir String. Directorio de salida. Default "outputs".
#' @return Un objeto ggplot (invisible).
#' @export
plot_treemap <- function(data,
                         col_site = NULL,
                         ord_site = NULL,
                         col_N,
                         col_facet = NULL,
                         ord_facet = NULL,
                         col_zone = NULL,
                         ord_zone = NULL,
                         taxa_id,
                         taxa_group,
                         width = 8,
                         height = 10,
                         output_name = "plot_treemap.png",
                         output_dir = "outputs") {
  # --- 1.1 Configuración inicial ---------------------------------------------
  op <- options(scipen = 999)
  on.exit(options(op))

  vars <- c(taxa_group, col_N, col_facet, col_zone, col_site)

  # --- 1.2 Selección y renombrado dinámico ------------------------------------
  data_plot <- data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::rename(
      taxa_group = !!rlang::sym(taxa_group),
      col_N      = !!rlang::sym(col_N),
      !!!if (!is.null(col_zone)) stats::setNames(col_zone, "col_zone") else NULL,
      !!!if (!is.null(col_facet)) stats::setNames(col_facet, "col_facet") else NULL,
      !!!if (!is.null(col_site)) stats::setNames(col_site, "col_site") else NULL
    )

  all_vars <- names(data_plot)
  gr_vars <- setdiff(all_vars, "col_N")

  # --- 1.3 Agregación de datos ------------------------------------------------
  data_plot <- data_plot %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(gr_vars))) %>%
    dplyr::summarise(col_N = sum(col_N), .groups = "drop")

  # --- 1.4 Cálculo de porcentajes (labels) ------------------------------------
  if (!is.null(col_facet)) {
    data_plot <- data_plot %>%
      dplyr::group_by(col_facet) %>%
      dplyr::mutate(
        label = scales::percent(col_N / sum(col_N), accuracy = 0.01)
      ) %>%
      dplyr::ungroup()
  } else {
    data_plot <- data_plot %>%
      dplyr::mutate(
        label = scales::percent(col_N / sum(col_N), accuracy = 0.01)
      )
  }

  # --- 1.5 Ordenamiento taxonómico --------------------------------------------
  ord_taxonomy <- data_plot %>%
    dplyr::group_by(taxa_group) %>%
    dplyr::summarise(total = sum(col_N), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(total)) %>%
    dplyr::pull(taxa_group)

  data_plot <- data_plot %>%
    dplyr::mutate(taxa_group = factor(taxa_group, levels = ord_taxonomy))

  # --- 1.6 Manejo de factores (facet, sitio, zona) ----------------------------
  if (!is.null(col_facet)) {
    if (!is.null(ord_facet)) {
      data_plot <- data_plot %>%
        dplyr::mutate(col_facet = factor(col_facet, levels = ord_facet))
    } else {
      data_plot <- data_plot %>%
        dplyr::mutate(col_facet = as.factor(col_facet))
    }
  }

  if (!is.null(col_site)) {
    if (!is.null(ord_site)) {
      data_plot <- data_plot %>%
        dplyr::mutate(col_site = factor(col_site, levels = ord_site))
    } else {
      data_plot <- data_plot %>%
        dplyr::mutate(col_site = factor(col_site, levels = sort(unique(col_site))))
    }
  }

  if (!is.null(col_zone)) {
    if (!is.null(ord_zone)) {
      data_plot <- data_plot %>%
        dplyr::mutate(col_zone = factor(col_zone, levels = ord_zone))
    } else {
      data_plot <- data_plot %>%
        dplyr::mutate(col_zone = factor(col_zone, levels = sort(unique(col_zone))))
    }
  }

  # --- 1.7 Configuración de colores ------------------------------------------
  col <- ggsci::pal_d3("category20c")(length(ord_taxonomy))
  names(col) <- ord_taxonomy

  # --- 1.8 Creación de etiquetas compuestas -----------------------------------
  if (!is.null(col_site) || !is.null(col_zone)) {
    data_plot <- data_plot %>%
      dplyr::mutate(
        label_lugar = if (!is.null(col_site)) col_site else col_zone,
        label       = glue::glue("{label_lugar} ({label})")
      )
  }

  # --- 1.9 Generación del gráfico ---------------------------------------------
  plot <- ggplot2::ggplot(
    data    = data_plot,
    mapping = ggplot2::aes(area = col_N, fill = taxa_group, label = label)
  ) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(
      grow   = FALSE,
      reflow = TRUE,
      colour = "white",
      place  = "centre",
      size   = 12
    ) +
    ggplot2::scale_fill_manual(
      values = col,
      name   = stringr::str_to_sentence(taxa_group)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::labs(
      title = glue::glue(
        "Composici\u00f3n taxon\u00f3mica por {stringr::str_to_lower(taxa_group)} de {taxa_id}"
      ),
      subtitle = "Composici\u00f3n derivada de la abundancia total de taxones"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text             = ggplot2::element_text(family = "Arial"),
      plot.title       = ggplot2::element_text(face = "bold", size = 12),
      strip.background = ggplot2::element_rect(fill = "gray20"),
      strip.text       = ggplot2::element_text(face = "bold", colour = "white"),
      plot.subtitle    = ggplot2::element_text(margin = ggplot2::margin(b = 12))
    )

  if (!is.null(col_facet)) {
    plot <- plot +
      ggplot2::facet_wrap(~col_facet, scales = "free", ncol = 1)
  }

  if (stringr::str_detect(string = taxa_group, pattern = "(?i)(especies?|organismos?)")) {
    plot <- plot +
      ggplot2::theme(legend.text = ggplot2::element_text(face = "italic"))
  }

  # --- 1.10 Guardado y retorno ------------------------------------------------
  output_path <- ensure_output_path(output_name, output_dir)

  tryCatch(
    {
      ggplot2::ggsave(
        filename = output_path,
        plot     = plot,
        width    = width,
        height   = height,
        dpi      = 300
      )
    },
    error = function(e) warning("Error guardando imagen: ", e$message)
  )

  return(invisible(plot))
}


# ---- 2. plot_index_by_taxa ---------------------------------------------------
#' @title Graficar Índices de Diversidad por Taxón
#' @description Genera y guarda dos gráficos de barras apiladas: uno de
#'   Abundancia Total y otro de Riqueza de Taxones, desglosados por sitio y
#'   grupo taxonómico.
#' @param data Data frame con los datos.
#' @param col_site String. Nombre de la columna de sitios.
#' @param ord_site Vector (Opcional). Orden de los niveles de sitios.
#' @param col_N String. Nombre de la columna de abundancia.
#' @param col_taxa String. Nombre de la columna de taxones individuales.
#' @param taxon_group String. Nombre de la columna de agrupación taxonómica.
#' @param col_facet String (Opcional). Nombre de la columna para facetar.
#' @param ord_facet Vector (Opcional). Orden de los niveles del facet.
#' @param taxa_id String. Identificador del grupo taxonómico general.
#' @param width Numérico. Ancho de la imagen.
#' @param height Numérico. Alto de la imagen.
#' @param xlabel String (Opcional). Etiqueta para el eje X. Default "Sitios".
#' @param output_dir String. Directorio de salida. Default "outputs".
#' @return Invisible NULL. Guarda dos archivos PNG.
#' @export
plot_index_by_taxa <- function(data,
                               col_site,
                               ord_site = NULL,
                               col_N,
                               col_taxa,
                               taxon_group,
                               col_facet = NULL,
                               ord_facet = NULL,
                               taxa_id,
                               width = 10,
                               height = 6,
                               xlabel = NULL,
                               output_dir = "outputs") {
  # --- 2.1 Configuración inicial ---------------------------------------------
  op <- options(scipen = 999)
  on.exit(options(op))

  vars <- c(col_site, col_N, col_taxa, taxon_group, col_facet)

  # --- 2.2 Selección y renombrado dinámico ------------------------------------
  data_plot <- data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::rename(
      col_site    = !!rlang::sym(col_site),
      col_N       = !!rlang::sym(col_N),
      col_taxa    = !!rlang::sym(col_taxa),
      taxon_group = !!rlang::sym(taxon_group),
      !!!if (!is.null(col_facet)) stats::setNames(col_facet, "col_facet") else NULL
    )

  gr_by_cols <- c("col_site", "taxon_group")
  if ("col_facet" %in% names(data_plot)) gr_by_cols <- c(gr_by_cols, "col_facet")

  ord_site <- if (is.null(ord_site)) sort(unique(data_plot$col_site)) else ord_site

  # --- 2.3 Cálculo de abundancia (N) -----------------------------------------
  res_abund <- data_plot %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(gr_by_cols))) %>%
    dplyr::summarise(N = sum(col_N, na.rm = TRUE), .groups = "drop") %>%
    {
      if (!is.null(col_facet) && !is.null(ord_facet)) {
        dplyr::mutate(., col_facet = factor(col_facet, levels = ord_facet))
      } else if (!is.null(col_facet) && is.null(ord_facet)) {
        dplyr::mutate(., col_facet = as.factor(col_facet))
      } else {
        .
      }
    } %>%
    dplyr::mutate(col_site = factor(col_site, levels = ord_site))

  # --- 2.4 Cálculo de riqueza (S) --------------------------------------------
  res_riqueza <- data_plot %>%
    dplyr::filter(!is.na(col_N), col_N > 0) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(gr_by_cols))) %>%
    dplyr::summarise(S = dplyr::n_distinct(col_taxa), .groups = "drop") %>%
    {
      if (!is.null(col_facet) && !is.null(ord_facet)) {
        dplyr::mutate(., col_facet = factor(col_facet, levels = ord_facet))
      } else if (!is.null(col_facet) && is.null(ord_facet)) {
        dplyr::mutate(., col_facet = as.factor(col_facet))
      } else {
        .
      }
    } %>%
    dplyr::mutate(col_site = factor(col_site, levels = ord_site))

  # --- 2.5 Orden de clases y paleta de colores --------------------------------
  ord_abund <- res_abund %>%
    dplyr::group_by(taxon_group) %>%
    dplyr::summarise(total_N = sum(N), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(total_N)) %>%
    dplyr::pull(taxon_group)

  ord_rich <- res_riqueza %>%
    dplyr::group_by(taxon_group) %>%
    dplyr::summarise(total_S = sum(S), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(total_S)) %>%
    dplyr::pull(taxon_group)

  all_taxon_group <- unique(c(as.character(ord_abund), as.character(ord_rich)))
  n_taxa_total <- length(all_taxon_group)

  col <- ggsci::pal_d3("category20c")(n_taxa_total)
  names(col) <- all_taxon_group

  res_abund <- res_abund %>%
    dplyr::mutate(taxon_group = factor(taxon_group, levels = ord_abund))
  res_riqueza <- res_riqueza %>%
    dplyr::mutate(taxon_group = factor(taxon_group, levels = ord_rich))

  xlab <- ifelse(is.null(xlabel), "Sitios", xlabel)

  # --- 2.6 Gráfico de abundancia ---------------------------------------------
  title_facet <- if (!is.null(col_facet)) {
    glue::glue(" por {stringr::str_to_lower(col_facet)}")
  } else {
    ""
  }

  p_abund <- ggplot2::ggplot(
    res_abund,
    ggplot2::aes(x = col_site, y = N, fill = taxon_group)
  ) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_y_continuous(name = "Abundancia total [N]", n.breaks = 6) +
    ggplot2::scale_fill_manual(
      values = col,
      name   = stringr::str_to_sentence(taxon_group)
    ) +
    ggplot2::labs(
      title = glue::glue(
        "Abundancia de taxones de {stringr::str_to_lower(taxa_id)}{title_facet}"
      ),
      x = xlab
    ) +
    ggplot2::theme_linedraw(base_size = 10, base_family = "Arial") +
    ggplot2::theme(
      legend.position  = "right",
      plot.title       = ggplot2::element_text(face = "bold", size = 12),
      strip.text       = ggplot2::element_text(face = "bold", colour = "white"),
      strip.background = ggplot2::element_rect(fill = "#2c3e50"),
      plot.tag         = ggplot2::element_text(face = "bold")
    )

  if (!is.null(col_facet)) {
    p_abund <- p_abund +
      ggplot2::facet_grid(~col_facet, scales = "free_x", space = "free_x")
  }

  if (stringr::str_detect(string = taxon_group, pattern = "(?i)(especies?|organismos?)")) {
    p_abund <- p_abund +
      ggplot2::theme(legend.text = ggplot2::element_text(face = "italic"))
  }

  # --- 2.7 Gráfico de riqueza ------------------------------------------------
  p_rich <- ggplot2::ggplot(
    res_riqueza,
    ggplot2::aes(x = col_site, y = S, fill = taxon_group)
  ) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_y_continuous(
      name = "Riqueza de taxones [S]",
      n.breaks = 6,
      labels = scales::label_number(accuracy = 1)
    ) +
    ggplot2::scale_fill_manual(
      values = col,
      name   = stringr::str_to_sentence(taxon_group)
    ) +
    ggplot2::labs(
      title = glue::glue(
        "Riqueza de taxones de {stringr::str_to_lower(taxa_id)}{title_facet}"
      ),
      x = xlab
    ) +
    ggplot2::theme_linedraw(base_size = 10, base_family = "Arial") +
    ggplot2::theme(
      legend.position  = "right",
      plot.title       = ggplot2::element_text(face = "bold", size = 12),
      strip.text       = ggplot2::element_text(face = "bold", colour = "white"),
      strip.background = ggplot2::element_rect(fill = "#2c3e50"),
      plot.tag         = ggplot2::element_text(face = "bold")
    )

  if (!is.null(col_facet)) {
    p_rich <- p_rich +
      ggplot2::facet_grid(~col_facet, scales = "free_x", space = "free_x")
  }

  if (stringr::str_detect(string = taxon_group, pattern = "(?i)(especies?|organismos?)")) {
    p_rich <- p_rich +
      ggplot2::theme(legend.text = ggplot2::element_text(face = "italic"))
  }

  # --- 2.8 Guardado -----------------------------------------------------------
  path_rich <- ensure_output_path(
    glue::glue("plot_riqueza_sitio_{taxa_id}.png"), output_dir
  )
  path_abund <- ensure_output_path(
    glue::glue("plot_abundancia_sitio_{taxa_id}.png"), output_dir
  )

  tryCatch(
    {
      ggplot2::ggsave(
        filename = path_rich, plot = p_rich,
        width = width, height = height, dpi = 300
      )
      ggplot2::ggsave(
        filename = path_abund, plot = p_abund,
        width = width, height = height, dpi = 300
      )
    },
    error = function(e) warning("Error guardando gr\u00e1ficos: ", e$message)
  )

  return(invisible(NULL))
}


# ---- 3. get_index_df --------------------------------------------------------
#' @title Calcular Índices de Diversidad (N, S, H)
#' @description Agrupa datos, calcula abundancia (N), riqueza (S) y Shannon (H)
#'   usando \code{vegan}, y guarda el resultado en CSV.
#' @param data Data frame con los datos.
#' @param col_value String. Nombre de la columna de abundancia o valor.
#' @param col_site String. Nombre de la columna de sitios.
#' @param col_sampling String (Opcional). Nombre de la columna de muestreo.
#' @param col_taxa String. Nombre de la columna de taxones.
#' @param col_zone String (Opcional). Nombre de la columna de zonas.
#' @param col_rep String (Opcional). Nombre de la columna de réplicas.
#' @param output_name String. Nombre del archivo de salida (.csv).
#' @param output_dir String. Directorio de salida. Default "outputs".
#' @param fun Función o nombre de función para agregar datos. Default sum.
#' @return Un data frame con los índices calculados (invisible).
#' @export
get_index_df <- function(data,
                         col_value,
                         col_site,
                         col_sampling = NULL,
                         col_taxa,
                         col_zone = NULL,
                         col_rep = NULL,
                         output_name,
                         output_dir = "outputs",
                         fun = sum) {
  # --- 3.1 Configuración inicial ---------------------------------------------
  op <- options(scipen = 999)
  on.exit(options(op))

  fun <- rlang::as_function(fun)

  # --- 3.2 Selección y renombrado dinámico ------------------------------------
  vars_to_select <- c(col_value, col_sampling, col_site, col_taxa, col_zone, col_rep)

  data_plot <- data %>%
    dplyr::select(dplyr::all_of(vars_to_select)) %>%
    dplyr::rename(
      col_value = !!rlang::sym(col_value),
      col_site  = !!rlang::sym(col_site),
      col_taxa  = !!rlang::sym(col_taxa),
      !!!if (!is.null(col_zone)) stats::setNames(col_zone, "col_zone") else NULL,
      !!!if (!is.null(col_sampling)) stats::setNames(col_sampling, "col_sampling") else NULL,
      !!!if (!is.null(col_rep)) stats::setNames(col_rep, "col_rep") else NULL
    ) %>%
    dplyr::filter(!is.na(col_taxa))

  # --- 3.3 Agregación de datos ------------------------------------------------
  grouping_cols <- setdiff(names(data_plot), "col_value")

  data_index <- data_plot %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
    dplyr::summarise(col_value = fun(col_value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from  = "col_taxa",
      values_from = "col_value",
      values_fill = 0
    )

  # --- 3.4 Cálculo de índices con vegan ---------------------------------------
  metadata <- data_index %>% dplyr::select(dplyr::starts_with("col_"))
  matrix_comm <- data_index %>% dplyr::select(-dplyr::starts_with("col_"))

  summ_index <- matrix_comm %>%
    dplyr::mutate(
      N = rowSums(.),
      S = vegan::specnumber(.),
      H = vegan::diversity(., index = "shannon")
    ) %>%
    dplyr::select(N, S, H)

  summ_index <- dplyr::bind_cols(metadata, summ_index)

  # --- 3.5 Guardado y retorno ------------------------------------------------
  if (!stringr::str_detect(output_name, "\\.csv$")) {
    output_name <- glue::glue("{output_name}.csv")
  }

  output_path <- ensure_output_path(output_name, output_dir)

  tryCatch(
    {
      readr::write_csv(x = summ_index, file = output_path)
    },
    error = function(e) warning("Error guardando archivo CSV: ", e$message)
  )

  return(invisible(summ_index))
}


# ---- 4. abbreviate_taxa -----------------------------------------------------
#' @title Abreviar Nombres Taxonómicos con Desambiguación
#' @description Genera códigos abreviados de nombres científicos. Si detecta
#'   colisiones (mismo código para especies distintas), aumenta la longitud del
#'   género para diferenciarlas.
#' @param taxa_name Vector de caracteres con los nombres científicos.
#' @return Un vector de caracteres con las abreviaciones.
#' @export
abbreviate_taxa <- function(taxa_name) {
  # --- 4.1 Limpieza y extracción de componentes -------------------------------
  df <- dplyr::tibble(original = taxa_name) %>%
    dplyr::mutate(
      clean = stringr::str_squish(
        stringr::str_trim(
          stringr::str_replace_all(original, "\\.", "")
        )
      ),
      w1 = stringr::word(clean, 1),
      w2 = stringr::word(clean, 2),
      w3 = stringr::word(clean, 3),

      # --- 4.2 Detección de patrones -----------------------------------------
      w2_sp = !is.na(w2) & stringr::str_detect(stringr::str_to_lower(w2), "^spp?$"),
      w2_cf = !is.na(w2) & stringr::str_detect(stringr::str_to_lower(w2), "^cf$"),
      w2_num = !is.na(w2) & stringr::str_detect(w2, "^\\d+$"),
      w3_num = !is.na(w3) & stringr::str_detect(w3, "^\\d+$"),
      solo_una = is.na(w2) | w2_sp,

      # --- 4.3 Generación de sigla base (1G + 4S) ----------------------------
      sigla_base = dplyr::case_when(
        w2_sp & w3_num ~ stringr::str_c(stringr::str_sub(w1, 1, 5), w3),
        w2_cf & !is.na(w3) ~ stringr::str_c(stringr::str_sub(w1, 1, 1), stringr::str_sub(w3, 1, 4)),
        solo_una ~ stringr::str_sub(w1, 1, 5),
        w2_num ~ stringr::str_c(stringr::str_sub(w1, 1, 4), w2),
        TRUE ~ stringr::str_c(stringr::str_sub(w1, 1, 1), stringr::str_sub(w2, 1, 4))
      )
    )

  # --- 4.4 Detección y resolución de colisiones (3G + 3S) ---------------------
  df_final <- df %>%
    dplyr::group_by(sigla_base) %>%
    dplyr::mutate(
      n_distinct_originals = dplyr::n_distinct(original),
      sigla_ext = dplyr::case_when(
        w2_sp | w2_num | (w2_cf & is.na(w3)) ~ sigla_base,
        w2_cf & !is.na(w3) ~ stringr::str_c(stringr::str_sub(w1, 1, 3), stringr::str_sub(w3, 1, 3)),
        TRUE ~ stringr::str_c(stringr::str_sub(w1, 1, 3), stringr::str_sub(w2, 1, 3))
      ),
      sigla_final = dplyr::if_else(n_distinct_originals > 1, sigla_ext, sigla_base)
    ) %>%
    dplyr::ungroup()

  # --- 4.5 Retorno formateado ------------------------------------------------
  return(stringr::str_to_sentence(df_final$sigla_final))
}


# ---- 5. plot_nmds -----------------------------------------------------------
#' @title Plot NMDS y PERMANOVA
#' @description Realiza un análisis NMDS con transformación de Hellinger. Si hay
#'   factor, dibuja elipses y calcula PERMANOVA. Si no, colorea por sitios.
#' @param data Data frame con los datos.
#' @param col_site String. Nombre de la columna de sitios.
#' @param col_N String. Nombre de la columna de abundancia.
#' @param col_taxa String. Nombre de la columna de taxones.
#' @param taxa_id String. Nombre del grupo taxonómico para el título.
#' @param col_factor String (Opcional). Nombre de la columna de agrupación.
#' @param legend_factor String (Opcional). Título personalizado para la leyenda.
#' @param levels_factor Vector (Opcional). Orden de los niveles del factor.
#' @param height Numérico. Alto de la imagen. Default 6.
#' @param width Numérico. Ancho de la imagen. Default 6.
#' @param output_name String. Nombre del archivo de salida.
#' @param output_dir String. Directorio de salida. Default "outputs".
#' @param save_rds Lógico. Si TRUE, guarda el objeto ggplot como .rds.
#' @return Un objeto ggplot con el NMDS.
#' @export
plot_nmds <- function(data,
                      col_site,
                      col_N,
                      col_taxa,
                      taxa_id,
                      col_factor = NULL,
                      legend_factor = NULL,
                      levels_factor = NULL,
                      height = 6,
                      width = 6,
                      output_name = "plot_nmds.png",
                      output_dir = "outputs",
                      save_rds = FALSE) {
  # --- 5.1 Configuración inicial ---------------------------------------------
  op <- options(scipen = 999)
  on.exit(options(op))

  # --- 5.2 Selección y renombrado dinámico ------------------------------------
  vars_to_select <- c(col_taxa, col_N, col_site, col_factor)

  data_plot <- data %>%
    dplyr::select(dplyr::all_of(vars_to_select)) %>%
    dplyr::rename(
      col_taxa = !!rlang::sym(col_taxa),
      col_N    = !!rlang::sym(col_N),
      col_site = !!rlang::sym(col_site),
      !!!if (!is.null(col_factor)) stats::setNames(col_factor, "col_factor") else NULL
    )

  run_stats <- !is.null(col_factor)

  # --- 5.3 Manejo de colores (factor vs sitios) -------------------------------
  if (run_stats) {
    levels_fac <- if (!is.null(levels_factor)) levels_factor else sort(unique(data_plot$col_factor))
    data_plot <- data_plot %>%
      dplyr::mutate(col_factor = factor(col_factor, levels = levels_fac))
  } else {
    data_plot$col_factor <- as.factor(data_plot$col_site)
  }

  # --- 5.4 Transformación a matriz de comunidad -------------------------------
  matriz_comunidad <- data_plot %>%
    dplyr::group_by(col_site, col_taxa, col_factor) %>%
    dplyr::summarise(abundancia = sum(col_N, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from  = "col_taxa",
      values_from = "abundancia",
      values_fill = 0
    )

  metadata <- matriz_comunidad %>% dplyr::select(col_site, col_factor)
  matriz_especies <- matriz_comunidad %>% dplyr::select(-col_site, -col_factor)

  # --- 5.5 Transformación Hellinger y NMDS ------------------------------------
  matriz_hel <- vegan::decostand(matriz_especies, method = "hellinger")

  NMDS1 <- vegan::metaMDS(
    matriz_hel,
    distance      = "euclid",
    k             = 2,
    trymax        = 1000,
    autotransform = FALSE,
    trace         = 0
  )

  data_nmds <- as.data.frame(vegan::scores(NMDS1)$sites)
  data_nmds <- dplyr::bind_cols(metadata, data_nmds)

  species_nmds <- as.data.frame(vegan::scores(NMDS1, display = "species"))
  species_nmds$species <- rownames(species_nmds)

  # --- 5.6 Estadísticas (PERMANOVA) ------------------------------------------
  subtitle_text <- NULL

  if (run_stats && length(unique(data_nmds$col_factor)) > 1) {
    permanova <- vegan::adonis2(
      matriz_hel ~ col_factor,
      data         = metadata,
      method       = "euclid",
      permutations = 999
    )

    Fval <- round(permanova$F[1], 2)
    pval <- format.pval(permanova$`Pr(>F)`[1], digits = 3, eps = 0.001)
    R2 <- round(permanova$R2[1] * 100, 1)

    orig_factor_name <- col_factor
    label_factor_txt <- dplyr::case_when(
      stringr::str_detect(orig_factor_name, stringr::regex("sector", ignore_case = TRUE)) ~ "Sector",
      stringr::str_detect(orig_factor_name, stringr::regex("zona", ignore_case = TRUE)) ~ "Zonas",
      stringr::str_detect(orig_factor_name, stringr::regex("sitio?s?", ignore_case = TRUE)) ~ "Sitio",
      stringr::str_detect(orig_factor_name, stringr::regex("[e\u00e9]pocas?$", ignore_case = TRUE)) ~ "\u00c9poca",
      TRUE ~ stringr::str_to_sentence(orig_factor_name)
    )

    subtitle_text <- glue::glue(
      "PERMANOVA por {stringr::str_to_lower(label_factor_txt)}: F = {Fval}, R\u00b2 = {R2}%, p = {pval}"
    )
  }

  # --- 5.7 Configuración visual -----------------------------------------------
  n_groups <- length(unique(data_nmds$col_factor))

  if (n_groups <= 20) {
    cols <- ggsci::pal_d3("category20")(n_groups)
  } else {
    base_colors <- ggsci::pal_d3("category20")(20)
    cols <- grDevices::colorRampPalette(base_colors)(n_groups)
  }
  names(cols) <- unique(data_nmds$col_factor)

  legend_title <- if (run_stats) {
    if (!is.null(legend_factor)) legend_factor else stringr::str_to_sentence(col_factor)
  } else {
    "Sitios"
  }

  # --- 5.8 Construcción del gráfico ------------------------------------------
  title_text <- glue::glue("An\u00e1lisis multivariado (NMDS) para {taxa_id}")

  plot <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linetype = 3, color = "gray0") +
    ggplot2::geom_vline(xintercept = 0, linetype = 3, color = "gray0") +
    ggplot2::geom_text(
      data = species_nmds,
      ggplot2::aes(x = NMDS1, y = NMDS2, label = species),
      fontface = "italic", alpha = 0.9, size = 2
    ) +
    ggplot2::geom_point(
      data = data_nmds,
      ggplot2::aes(x = NMDS1, y = NMDS2, color = col_factor),
      size = 1, show.legend = TRUE
    ) +
    ggplot2::annotate(
      "label",
      x = -Inf, y = -Inf, hjust = -0.1, vjust = -0.5,
      size = 3, fill = "lightgrey",
      label = paste("Stress:", round(NMDS1$stress, 3))
    ) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::labs(
      x        = "Primera dimensi\u00f3n NMDS",
      y        = "Segunda dimensi\u00f3n NMDS",
      colour   = legend_title,
      title    = title_text,
      subtitle = subtitle_text
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      text               = ggplot2::element_text(family = "Arial"),
      legend.position    = "right",
      legend.box         = "vertical",
      plot.title         = ggplot2::element_text(face = "bold", size = 12),
      legend.key.height  = grid::unit(0.5, "cm"),
      legend.text        = ggplot2::element_text(size = 9),
      legend.spacing.y   = grid::unit(0.1, "cm"),
      plot.caption       = ggplot2::element_text(face = "bold.italic", size = 9, hjust = 0)
    )

  if (run_stats && n_groups > 1) {
    plot <- plot +
      ggplot2::stat_ellipse(
        data = data_nmds,
        ggplot2::aes(x = NMDS1, y = NMDS2, color = col_factor),
        type = "norm", linetype = 1, linewidth = 0.8, alpha = 0.5, level = 0.95
      )
  }

  # --- 5.9 Guardado y retorno ------------------------------------------------
  output_path <- ensure_output_path(output_name, output_dir)

  tryCatch(
    {
      ggplot2::ggsave(
        filename = output_path,
        plot     = plot,
        height   = height,
        width    = width,
        dpi      = 300
      )

      if (save_rds) {
        rds_name <- stringr::str_replace(output_path, "\\.png$", ".rds")
        saveRDS(plot, file = rds_name)
        message(glue::glue("Objeto R guardado en: {rds_name}"))
      }
    },
    error = function(e) warning("Error guardando plot: ", e$message)
  )

  return(plot)
}


# ---- 6. plot_alluvial_taxa ---------------------------------------------------
#' @title Plot Alluvial de Composición (Abundancia o Riqueza por Grupo)
#' @description Crea un gráfico aluvial. Ordena la leyenda por magnitud y
#'   respeta la lógica de riqueza total sin filtrar si se solicita.
#' @param data Data frame con los datos.
#' @param col_site String. Columna eje X.
#' @param ord_site Vector (Opcional). Orden de los sitios.
#' @param col_N String. Columna de abundancia.
#' @param col_taxa_group String. Grupo mayor que forma las cintas.
#' @param col_taxa String (Opcional). Unidad menor a contar para riqueza.
#' @param taxa_id String. Nombre del grupo general para título.
#' @param col_facet String (Opcional). Columna para facetar.
#' @param ord_facet Vector (Opcional). Orden de los niveles del facet.
#' @param metric String. "abundance" o "richness".
#' @param legend_label String (Opcional). Título leyenda.
#' @param xlabel String. Título eje X. Default "Sitios".
#' @param top_n Entero. Top N grupos (solo para metric = "abundance").
#' @param output_name String. Nombre del archivo de salida.
#' @param output_dir String. Directorio de salida. Default "outputs".
#' @param width Numérico. Ancho imagen. Default 10.
#' @param height Numérico. Alto imagen. Default 6.
#' @param save_rds Lógico. Guardar RDS. Default FALSE.
#' @return Un objeto ggplot.
#' @export
plot_alluvial_taxa <- function(data,
                               col_site,
                               ord_site = NULL,
                               col_N,
                               col_taxa_group,
                               col_taxa = NULL,
                               taxa_id,
                               col_facet = NULL,
                               ord_facet = NULL,
                               metric = "abundance",
                               legend_label = NULL,
                               xlabel = "Sitios",
                               top_n = 10,
                               output_name,
                               output_dir = "outputs",
                               width = 10,
                               height = 6,
                               save_rds = FALSE) {
  # --- 6.1 Configuración inicial ---------------------------------------------
  op <- options(scipen = 999)
  on.exit(options(op))

  if (metric == "richness" && is.null(col_taxa)) {
    stop("Error: Para calcular 'richness', debes especificar 'col_taxa'.")
  }

  # --- 6.2 Selección y renombrado --------------------------------------------
  vars_select <- list(
    col_site       = rlang::sym(col_site),
    col_N          = rlang::sym(col_N),
    col_taxa_group = rlang::sym(col_taxa_group)
  )
  if (!is.null(col_taxa)) vars_select$col_taxa <- rlang::sym(col_taxa)
  if (!is.null(col_facet)) vars_select$col_facet <- rlang::sym(col_facet)

  data_plot <- data %>% dplyr::select(!!!vars_select)

  # --- 6.3 Cálculo (abundancia vs riqueza) ------------------------------------
  grp_base <- c("col_site", "col_taxa_group")
  if (!is.null(col_facet)) grp_base <- c(grp_base, "col_facet")

  if (metric == "richness") {
    data_calc <- data_plot %>%
      dplyr::filter(col_N > 0) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_base))) %>%
      dplyr::summarise(final_value = dplyr::n_distinct(col_taxa), .groups = "drop")

    y_label <- "Riqueza de taxa [S]"
    text_string <- "la riqueza de taxa"
    if (is.null(legend_label)) legend_label <- "Grupo"
  } else {
    data_calc <- data_plot %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_base))) %>%
      dplyr::summarise(final_value = sum(col_N, na.rm = TRUE), .groups = "drop")

    y_label <- "Abundancia [N]"
    text_string <- "la abundancia"
    if (is.null(legend_label)) legend_label <- "Tax\u00f3n"
  }

  # --- 6.4 Filtrado y etiquetado (Top N / Others) -----------------------------
  if (metric == "richness") {
    top_groups <- unique(data_calc$col_taxa_group)
  } else {
    ranking <- data_calc %>%
      dplyr::group_by(col_taxa_group) %>%
      dplyr::summarise(total = sum(final_value, na.rm = TRUE)) %>%
      dplyr::arrange(dplyr::desc(total))

    top_groups <- ranking %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::pull(col_taxa_group)
  }

  grp_final <- c("col_site", "taxa_label")
  if (!is.null(col_facet)) grp_final <- c(grp_final, "col_facet")

  data_alluvial <- data_calc %>%
    dplyr::mutate(
      taxa_label = dplyr::if_else(
        col_taxa_group %in% top_groups, as.character(col_taxa_group), "Others"
      )
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_final))) %>%
    dplyr::summarise(final_value = sum(final_value, na.rm = TRUE), .groups = "drop")

  # --- 6.5 Ordenamiento de leyenda por magnitud -------------------------------
  total_per_label <- data_alluvial %>%
    dplyr::group_by(taxa_label) %>%
    dplyr::summarise(total = sum(final_value, na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(total))

  ordered_levels <- total_per_label$taxa_label
  if ("Others" %in% ordered_levels) {
    ordered_levels <- c(setdiff(ordered_levels, "Others"), "Others")
  }

  data_alluvial <- data_alluvial %>%
    dplyr::mutate(taxa_label = factor(taxa_label, levels = ordered_levels))

  # --- 6.6 Ordenamiento de ejes (sitios y facets) -----------------------------
  if (!is.null(ord_site)) {
    data_alluvial <- data_alluvial %>%
      dplyr::mutate(col_site = factor(col_site, levels = ord_site))
  }

  if (!is.null(col_facet)) {
    fac_levels <- if (!is.null(ord_facet)) ord_facet else sort(unique(data_alluvial$col_facet))
    data_alluvial <- data_alluvial %>%
      dplyr::mutate(col_facet = factor(col_facet, levels = fac_levels))
  }

  # --- 6.7 Paleta de colores -------------------------------------------------
  n_colors <- length(unique(data_alluvial$taxa_label))

  if (n_colors <= 20) {
    cols <- ggsci::pal_d3("category20c")(n_colors)
  } else {
    cols <- scales::hue_pal()(n_colors)
  }
  names(cols) <- levels(data_alluvial$taxa_label)
  if ("Others" %in% names(cols)) cols["Others"] <- "grey80"

  # --- 6.8 Construcción del gráfico ------------------------------------------
  plot <- ggplot2::ggplot(
    data_alluvial,
    ggplot2::aes(x = col_site, y = final_value, alluvium = taxa_label)
  ) +
    ggalluvial::geom_alluvium(
      ggplot2::aes(fill = taxa_label),
      alpha = 0.5, width = 1 / 5, color = NA
    ) +
    ggalluvial::geom_stratum(
      ggplot2::aes(stratum = taxa_label, fill = taxa_label),
      width = 1 / 5, alpha = 1, color = "grey20"
    ) +
    ggplot2::labs(
      title = glue::glue(
        "Din\u00e1mica espacial de {stringr::str_to_lower(text_string)} de {taxa_id}"
      ),
      y = y_label,
      x = xlabel,
      fill = legend_label
    ) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_y_continuous(n.breaks = 10) +
    ggplot2::theme_bw(base_size = 12, base_family = "Arial") +
    ggplot2::theme(
      strip.text       = ggplot2::element_text(face = "bold", colour = "white"),
      strip.background = ggplot2::element_rect(fill = "#2c3e50"),
      plot.title       = ggplot2::element_text(face = "bold"),
      axis.text.x      = ggplot2::element_text(face = "bold"),
      legend.position  = "right"
    )

  if (!is.null(col_facet)) {
    plot <- plot +
      ggplot2::facet_grid(~col_facet, scales = "free_x", space = "free")
  }

  # --- 6.9 Guardado y retorno ------------------------------------------------
  output_path <- ensure_output_path(output_name, output_dir)

  tryCatch(
    {
      ggplot2::ggsave(
        filename = output_path,
        plot     = plot,
        width    = width,
        height   = height,
        dpi      = 300
      )

      if (save_rds) {
        rds_name <- stringr::str_replace(output_path, "\\.(png|jpg|jpeg|pdf|tiff)$", ".rds")
        if (rds_name == output_path) rds_name <- paste0(output_path, ".rds")
        saveRDS(plot, file = rds_name)
        message(paste("Objeto aluvial guardado en:", rds_name))
      }
    },
    error = function(e) warning("Error guardando imagen/objeto: ", e$message)
  )

  return(plot)
}


# ---- 7. plot_index ----------------------------------------------------------
#' @title Graficar Variación Espacial de Índices Comunitarios
#' @description Crea un gráfico de líneas y puntos facetado por tipo de índice
#'   (N, S, H). Asigna paletas de colores específicas según el nombre de la
#'   leyenda.
#' @param data Data frame con los datos.
#' @param col_value String. Nombre de la columna con el valor del índice.
#' @param col_index String. Nombre de la columna que indica el tipo de índice.
#' @param col_site String. Nombre de la columna de sitios (Eje X).
#' @param ord_site Vector (Opcional). Orden de los niveles de sitios.
#' @param col_rep String (Opcional). Columna para agrupar líneas/colores.
#' @param col_facet String (Opcional). Columna para facetar horizontalmente.
#' @param ord_facet Vector (Opcional). Orden de los niveles del facet.
#' @param taxa_id String. Identificador del taxón para el título.
#' @param legend_name String. Título de la leyenda.
#' @param width Numérico. Ancho de la imagen. Default 8.
#' @param height Numérico. Alto de la imagen. Default 6.
#' @param xlabel String. Etiqueta eje X. Default "Sitios".
#' @param output_name String. Nombre del archivo de salida.
#' @param output_dir String. Directorio de salida. Default "outputs".
#' @param save_rds Lógico. Guardar .rds. Default FALSE.
#' @return Un objeto ggplot (invisible).
#' @export
plot_index <- function(data,
                       col_value,
                       col_index,
                       col_site,
                       ord_site = NULL,
                       col_rep = NULL,
                       col_facet = NULL,
                       ord_facet = NULL,
                       taxa_id,
                       legend_name,
                       width = 8,
                       height = 6,
                       output_name = "plot_index.png",
                       output_dir = "outputs",
                       xlabel = "Sitios",
                       save_rds = FALSE) {
  # --- 7.1 Configuración inicial ---------------------------------------------
  op <- options(scipen = 999)
  on.exit(options(op))

  # --- 7.2 Selección y renombrado dinámico ------------------------------------
  data_plot <- data %>%
    dplyr::select(
      col_value = !!rlang::sym(col_value),
      col_site  = !!rlang::sym(col_site),
      col_index = !!rlang::sym(col_index),
      !!!if (!is.null(col_facet)) stats::setNames(col_facet, "col_facet") else NULL,
      !!!if (!is.null(col_rep)) stats::setNames(col_rep, "col_rep") else NULL
    ) %>%
    dplyr::mutate(
      col_index = factor(
        col_index,
        levels = c("N", "S", "H"),
        labels = c(
          "Abundancia total [N]",
          "Riqueza de taxones [S]",
          "Diversidad de Shannon [H']"
        )
      )
    )

  # --- 7.3 Manejo de facet (ordenamiento) -------------------------------------
  if (!is.null(col_facet)) {
    lvls <- if (!is.null(ord_facet)) ord_facet else sort(unique(data_plot$col_facet))
    data_plot <- data_plot %>%
      dplyr::mutate(col_facet = factor(col_facet, levels = lvls))
  }

  # --- 7.4 Manejo de colores y agrupación (col_rep) ---------------------------
  cols <- NULL

  if (!is.null(col_rep)) {
    data_plot <- data_plot %>%
      dplyr::mutate(col_rep = factor(col_rep, levels = sort(unique(col_rep))))

    if (stringr::str_detect(string = legend_name, pattern = "(?i)estratos?")) {
      cols <- c("#87CEFF", "#00868B", "#1874CD")
      names(cols) <- c("Superficie", "Medio", "Fondo")
      data_plot <- data_plot %>%
        dplyr::mutate(col_rep = factor(col_rep, levels = names(cols)))
    } else if (stringr::str_detect(string = legend_name, pattern = "(?i)r[e\u00e9]plicas?")) {
      n_cols <- length(unique(data_plot$col_rep))
      cols <- ggsci::pal_jco("default")(n_cols)
      names(cols) <- unique(data_plot$col_rep)
    } else {
      n_cols <- length(unique(data_plot$col_rep))
      cols <- ggsci::pal_npg("nrc")(n_cols)
      names(cols) <- unique(data_plot$col_rep)
    }
  }

  # --- 7.5 Orden de sitios ----------------------------------------------------
  ord_site_final <- if (is.null(ord_site)) {
    sort(unique(data_plot$col_site))
  } else {
    ord_site
  }

  data_plot <- data_plot %>%
    dplyr::mutate(col_site = factor(col_site, levels = ord_site_final))

  # --- 7.6 Construcción del gráfico ------------------------------------------
  plot <- ggplot2::ggplot(
    data = data_plot,
    mapping = ggplot2::aes(
      x      = col_site,
      y      = col_value,
      colour = col_rep,
      group  = col_rep
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = xlabel,
      y = "Valor \u00edndice",
      title = glue::glue(
        "Variaci\u00f3n espacial de \u00edndices comunitarios para {taxa_id}"
      )
    ) +
    ggplot2::scale_color_manual(values = cols, name = legend_name) +
    ggplot2::facet_grid(
      col_index ~ col_facet,
      scales = "free", space = "free_x", switch = "y"
    ) +
    ggplot2::theme_linedraw(base_size = 10, base_family = "Arial") +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 12),
      strip.text       = ggplot2::element_text(face = "bold", colour = "white"),
      strip.background = ggplot2::element_rect(fill = "#2c3e50")
    )

  # --- 7.7 Guardado y retorno ------------------------------------------------
  output_path <- ensure_output_path(output_name, output_dir)

  tryCatch(
    {
      ggplot2::ggsave(
        filename = output_path,
        plot     = plot,
        width    = width,
        height   = height,
        dpi      = 300
      )

      if (save_rds) {
        rds_name <- stringr::str_replace(output_path, "\\.png$", ".rds")
        saveRDS(plot, file = rds_name)
        message(glue::glue("Objeto R guardado en: {rds_name}"))
      }
    },
    error = function(e) warning("Error guardando plot: ", e$message)
  )

  return(plot)
}


# ---- 8. get_taxo_worms ------------------------------------------------------
#' @title Taxonomía según WoRMS
#' @description Toma un vector de nombres científicos y consulta la base de
#'   datos de WoRMS para obtener su clasificación taxonómica completa.
#' @param taxa_vector Vector de caracteres con los nombres de las especies.
#' @param max_dist Distancia máxima para match difuso. Default 3.
#' @param dist_method Método de cálculo de distancia. Default "lv".
#' @return Un tibble con la jerarquía taxonómica y metadatos de búsqueda.
#' @export
get_taxo_worms <- function(taxa_vector, max_dist = 3, dist_method = "lv") {
  # --- 8.1 Helper interno: limpiar nombres taxonómicos ------------------------
  clean_taxon_name <- function(name) {
    name_clean <- stringr::str_replace_all(
      name,
      ",?\\s*\\(?[A-Z][a-z]+\\)?\\s*,?\\s*\\d{4}.*",
      ""
    )
    name_clean <- stringr::str_replace_all(
      name_clean,
      "(?i)\\b(cf\\.|sp\\.|spp\\.|aff\\.|gr\\.|v\\.|var\\.)\\b",
      " "
    )
    return(stringr::str_squish(name_clean))
  }

  # --- 8.2 Función por taxón individual ---------------------------------------
  get_single_taxo <- function(taxon_raw) {
    # --- 8.2.1 Detección de genéricos y limpieza ---
    es_generico <- stringr::str_detect(
      taxon_raw,
      stringr::regex("\\b(sp|spp)\\b|\\d+", ignore_case = TRUE)
    )
    es_generico <- ifelse(is.na(es_generico), FALSE, es_generico)
    taxon_clean <- clean_taxon_name(taxon_raw)

    res <- NULL
    attempt <- NA_real_

    # --- 8.2.2 Paso 1: Búsqueda exacta ---
    res_exact <- tryCatch(
      worrms::wm_records_names(taxon_clean, fuzzy = FALSE, marine_only = FALSE),
      error = function(e) NULL
    )

    if (!is.null(res_exact) && length(res_exact) > 0) {
      res <- dplyr::bind_rows(res_exact) %>%
        dplyr::arrange(
          dplyr::desc(stringr::str_detect(kingdom, "Chromista|Plantae")),
          status == "unaccepted"
        ) %>%
        dplyr::slice(1)
      attempt <- 1
    }

    # --- 8.2.3 Paso 2: TAXAMATCH (fuzzy) ---
    if (is.na(attempt) && !es_generico) {
      res_taxa <- tryCatch(
        worrms::wm_records_taxamatch(taxon_clean, marine_only = FALSE),
        error = function(e) NULL
      )

      if (!is.null(res_taxa) && length(res_taxa) > 0) {
        res <- dplyr::bind_rows(res_taxa) %>%
          dplyr::mutate(
            d = stringdist::stringdist(taxon_clean, scientificname, method = dist_method)
          ) %>%
          dplyr::filter(d <= max_dist + 1) %>%
          dplyr::arrange(
            dplyr::desc(stringr::str_detect(kingdom, "Chromista|Plantae")),
            d,
            status == "unaccepted"
          ) %>%
          dplyr::slice(1)

        if (nrow(res) > 0) attempt <- 2
      }
    }

    # --- 8.2.4 Paso 3: Fallback a género ---
    if (is.na(attempt)) {
      higher_rank <- stringr::str_split_i(taxon_clean, "\\s+", 1)

      res_gen <- tryCatch(
        worrms::wm_records_names(higher_rank, fuzzy = TRUE, marine_only = FALSE),
        error = function(e) NULL
      )

      if (!is.null(res_gen) && length(res_gen) > 0) {
        res <- dplyr::bind_rows(res_gen) %>%
          dplyr::arrange(
            dplyr::desc(stringr::str_detect(kingdom, "Chromista|Plantae")),
            status == "unaccepted"
          ) %>%
          dplyr::slice(1)
        attempt <- 3
      }
    }

    # --- 8.2.5 Manejo de NAs / sin registros ---
    if (is.na(attempt)) {
      return(tibble::tibble(
        taxa_input = taxon_raw,
        taxa_searched = taxon_clean,
        match_attempt = NA_real_,
        valid_name = NA_character_,
        status = "not found",
        AphiaID = NA_integer_,
        verificacion_taxonomica = "taxa no encontrado",
        notas = "Sin registros"
      ))
    }

    # --- 8.2.6 Procesamiento de resultados ---
    best <- res

    classif <- tryCatch(
      worrms::wm_classification(best$AphiaID),
      error = function(e) NULL
    )

    classif_flat <- if (!is.null(classif)) {
      classif %>%
        dplyr::filter(
          rank %in% c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus")
        ) %>%
        dplyr::group_by(rank) %>%
        dplyr::summarise(scientificname = dplyr::first(scientificname), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = rank, values_from = scientificname)
    } else {
      NULL
    }

    # --- 8.2.7 Verificación y notas diagnósticas ---
    is_valid <- stringr::str_equal(
      stringr::str_squish(taxon_clean),
      stringr::str_squish(best$valid_name),
      ignore_case = TRUE
    )
    is_valid <- ifelse(is_valid %in% TRUE, TRUE, FALSE)
    verificacion <- ifelse(is_valid, "taxa encontrado", "taxa inv\u00e1lido")

    nota <- dplyr::case_when(
      es_generico ~ "Identificaci\u00f3n a nivel de g\u00e9nero (sp./spp. detectado)",
      attempt == 2 ~ paste("Sugerido v\u00eda Taxamatch (Fuzzy):", best$scientificname),
      best$status == "unaccepted" ~ paste("Sin\u00f3nimo de:", best$valid_name),
      attempt == 3 ~ "Match logrado solo a nivel de g\u00e9nero",
      TRUE ~ "Nombre coincide con registro v\u00e1lido"
    )

    tibble::tibble(
      taxa_input = taxon_raw,
      taxa_searched = taxon_clean,
      match_attempt = attempt,
      valid_name = best$valid_name,
      status = best$status,
      AphiaID = best$AphiaID,
      matched_rank = best$rank,
      Kingdom = purrr::pluck(classif_flat, "Kingdom", .default = NA_character_),
      Phylum = purrr::pluck(classif_flat, "Phylum", .default = NA_character_),
      Class = purrr::pluck(classif_flat, "Class", .default = NA_character_),
      Order = purrr::pluck(classif_flat, "Order", .default = NA_character_),
      Family = purrr::pluck(classif_flat, "Family", .default = NA_character_),
      Genus = purrr::pluck(classif_flat, "Genus", .default = NA_character_),
      verificacion_taxonomica = verificacion,
      notas = nota
    )
  }

  # --- 8.3 Iteración sobre el vector completo ---------------------------------
  purrr::map(taxa_vector, get_single_taxo) %>%
    purrr::list_rbind()
}
get_taxo_worms2 <- function(taxa_vector,
                            max_dist = 3,
                            dist_method = "lv",
                            priority_kingdoms = "Chromista|Plantae",
                            sleep = 0.1,
                            verbose = TRUE) {
  # --- Helper: limpiar nombres taxonómicos ---

  clean_taxon_name <- function(name) {
    # 1. Eliminar autores, años y paréntesis
    name_clean <- stringr::str_replace_all(name, ",?\\s*\\(?[A-Z][a-z]+\\)?\\s*,?\\s*\\d{4}.*", "")
    # 2. Eliminar calificadores + número opcional (sp. 1, spp. 2, cf., etc.)
    name_clean <- stringr::str_replace_all(name_clean, "(?i)\\b(cf\\.|sp\\.|spp\\.|aff\\.|gr\\.|v\\.|var\\.)\\s*\\d*", " ")
    return(stringr::str_squish(name_clean))
  }

  # --- Cache para wm_classification ---
  classif_cache <- new.env(parent = emptyenv())

  get_classification_cached <- function(aphia_id) {
    key <- as.character(aphia_id)
    if (exists(key, envir = classif_cache)) {
      return(get(key, envir = classif_cache))
    }
    result <- tryCatch(worrms::wm_classification(aphia_id), error = function(e) NULL)
    assign(key, result, envir = classif_cache)
    return(result)
  }

  # --- Helper: priorizar resultados por reino ---
  prioritize_result <- function(df, taxon_clean = NULL, dist_method = "lv") {
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }

    result <- df %>%
      dplyr::arrange(
        dplyr::desc(stringr::str_detect(kingdom, priority_kingdoms)),
        status == "unaccepted"
      )

    if (!is.null(taxon_clean)) {
      result <- result %>%
        dplyr::mutate(d = stringdist::stringdist(taxon_clean, scientificname, method = dist_method)) %>%
        dplyr::filter(d <= max_dist) %>%
        dplyr::arrange(
          dplyr::desc(stringr::str_detect(kingdom, priority_kingdoms)),
          d,
          status == "unaccepted"
        )
    }

    if (nrow(result) == 0) {
      return(NULL)
    }
    dplyr::slice(result, 1)
  }

  # --- Función por taxón individual ---
  get_single_taxo <- function(taxon_raw, idx, total) {
    if (verbose) message(sprintf("[%d/%d] %s", idx, total, taxon_raw))

    es_generico <- stringr::str_detect(taxon_raw, stringr::regex("\\b(sp|spp)\\b|\\d+", ignore_case = TRUE))
    es_generico <- ifelse(is.na(es_generico), FALSE, es_generico)
    taxon_clean <- clean_taxon_name(taxon_raw)

    res <- NULL
    attempt <- NA_real_

    # Paso 1: Exacto
    res_exact <- tryCatch(
      worrms::wm_records_names(taxon_clean, fuzzy = FALSE, marine_only = FALSE),
      error = function(e) NULL
    )
    if (!is.null(res_exact) && length(res_exact) > 0) {
      res <- prioritize_result(dplyr::bind_rows(res_exact))
      if (!is.null(res)) attempt <- 1
    }

    # Paso 2: Taxamatch (solo si no es genérico)
    if (is.na(attempt) && !es_generico) {
      Sys.sleep(sleep)
      res_taxa <- tryCatch(
        worrms::wm_records_taxamatch(taxon_clean, marine_only = FALSE),
        error = function(e) NULL
      )
      if (!is.null(res_taxa) && length(res_taxa) > 0) {
        res <- prioritize_result(dplyr::bind_rows(res_taxa), taxon_clean, dist_method)
        if (!is.null(res)) attempt <- 2
      }
    }

    # Paso 3: Género
    if (is.na(attempt)) {
      Sys.sleep(sleep)
      higher_rank <- stringr::str_split_i(taxon_clean, "\\s+", 1)
      res_gen <- tryCatch(
        worrms::wm_records_names(higher_rank, fuzzy = TRUE, marine_only = FALSE),
        error = function(e) NULL
      )
      if (!is.null(res_gen) && length(res_gen) > 0) {
        res <- prioritize_result(dplyr::bind_rows(res_gen))
        if (!is.null(res)) attempt <- 3
      }
    }

    # Sin resultados
    if (is.na(attempt)) {
      return(tibble::tibble(
        taxa_input = taxon_raw, taxa_searched = taxon_clean,
        match_attempt = NA_real_, valid_name = NA_character_,
        status = "not found", AphiaID = NA_integer_,
        verificacion_taxonomica = "taxa no encontrado",
        notas = "Sin registros"
      ))
    }

    # Clasificación (cacheada)
    classif <- get_classification_cached(res$AphiaID)

    classif_flat <- if (!is.null(classif)) {
      classif %>%
        dplyr::filter(rank %in% c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus")) %>%
        dplyr::group_by(rank) %>%
        dplyr::summarise(scientificname = dplyr::first(scientificname), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = rank, values_from = scientificname)
    } else {
      NULL
    }

    is_valid <- isTRUE(stringr::str_equal(
      stringr::str_squish(taxon_clean),
      stringr::str_squish(res$valid_name),
      ignore_case = TRUE
    ))

    verificacion <- ifelse(is_valid, "taxa encontrado", "taxa inválido")

    nota <- dplyr::case_when(
      es_generico ~ "Identificación a nivel de género (sp./spp. detectado)",
      attempt == 2 ~ paste("Sugerido vía Taxamatch (Fuzzy):", res$scientificname),
      res$status == "unaccepted" ~ paste("Sinónimo de:", res$valid_name),
      attempt == 3 ~ "Match logrado solo a nivel de género",
      TRUE ~ "Nombre coincide con registro válido"
    )

    tibble::tibble(
      taxa_input = taxon_raw, taxa_searched = taxon_clean,
      match_attempt = attempt, valid_name = res$valid_name,
      status = res$status, AphiaID = res$AphiaID,
      matched_rank = res$rank,
      Kingdom = purrr::pluck(classif_flat, "Kingdom", .default = NA_character_),
      Phylum = purrr::pluck(classif_flat, "Phylum", .default = NA_character_),
      Class = purrr::pluck(classif_flat, "Class", .default = NA_character_),
      Order = purrr::pluck(classif_flat, "Order", .default = NA_character_),
      Family = purrr::pluck(classif_flat, "Family", .default = NA_character_),
      Genus = purrr::pluck(classif_flat, "Genus", .default = NA_character_),
      verificacion_taxonomica = verificacion, notas = nota
    )
  }

  # --- Iteración con índice ---
  n <- length(taxa_vector)
  purrr::imap(taxa_vector, ~ get_single_taxo(.x, .y, n)) %>%
    purrr::list_rbind()
}
# ---- Fin del archivo ---------------------------------------------------------
