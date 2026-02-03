#' @title Generar Treemap de Composición Taxonómica
#' @description Crea un gráfico Treemap basado en abundancias. Soporta facetado, agrupación por sitio/zona y formateo específico de etiquetas.
#' @param data Data frame con los datos.
#' @param col_site String (Opcional). Nombre de la columna de sitios. (Anteriormente `col_sitio`).
#' @param ord_site Vector (Opcional). Orden de los niveles de sitios.
#' @param col_N String. Nombre de la columna de abundancia o conteo. (Anteriormente `col_N`).
#' @param col_facet String (Opcional). Nombre de la columna para facetar.
#' @param ord_facet Vector (Opcional). Orden de los niveles del facet.
#' @param col_zone String (Opcional). Nombre de la columna de zonas. (Anteriormente `col_zona`).
#' @param ord_zone Vector (Opcional). Orden de los niveles de zonas.
#' @param taxa_id String. Texto identificador del taxón superior (ej. "Familia X") para el título.
#' @param taxa_group String. Nombre de la columna con el grupo taxonómico a graficar.
#' @param width Numérico. Ancho de la imagen. Default 8.
#' @param height Numérico. Alto de la imagen. Default 10.
#' @param output_name String. Nombre del archivo de salida.
#' @return Un objeto ggplot (invisible).
#' @export plot_treemap
#' @importFrom dplyr select rename group_by summarise mutate arrange pull across desc filter ungroup all_of
#' @importFrom ggplot2 ggplot aes scale_fill_manual guides guide_legend labs theme_bw theme element_text element_rect margin facet_wrap ggsave margin
#' @importFrom treemapify geom_treemap geom_treemap_text
#' @importFrom ggsci pal_d3
#' @importFrom glue glue
#' @importFrom stringr str_to_sentence str_to_lower str_detect
#' @importFrom stats setNames
#' @importFrom rlang sym "!!" "!!!"
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
                         output_name = "plot_treemap.png") {
  # 1. Configuración Inicial
  op <- options(scipen = 999)
  on.exit(options(op))

  # Armonización de variables internas
  vars <- c(taxa_group, col_N, col_facet, col_zone, col_site)

  # 2. Selección y Renombrado Dinámico
  # Mapeamos los nombres originales a nombres internos estándar (col_site, col_N, etc.)
  data_plot <- data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::rename(
      taxa_group = !!rlang::sym(taxa_group),
      col_N = !!rlang::sym(col_N),
      !!!if (!is.null(col_zone)) stats::setNames(col_zone, "col_zone") else NULL,
      !!!if (!is.null(col_facet)) stats::setNames(col_facet, "col_facet") else NULL,
      !!!if (!is.null(col_site)) stats::setNames(col_site, "col_site") else NULL
    )

  all_vars <- names(data_plot)
  # Excluir la columna de valores para agrupar por todo lo demás
  gr_vars <- setdiff(all_vars, c("col_N"))

  # 3. Agregación de Datos
  data_plot <- data_plot %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(gr_vars))) %>%
    dplyr::summarise(col_N = sum(col_N), .groups = "drop")

  # 4. Cálculo de Porcentajes (Labels)
  if (!is.null(col_facet)) {
    # Porcentaje dentro del panel
    data_plot <- data_plot %>%
      dplyr::group_by(col_facet) %>%
      dplyr::mutate(
        label = scales::percent(col_N / sum(col_N), accuracy = 0.01)
      ) %>%
      dplyr::ungroup()
  } else {
    # Porcentaje global
    data_plot <- data_plot %>%
      dplyr::mutate(
        label = scales::percent(col_N / sum(col_N), accuracy = 0.01)
      )
  }

  # 5. Ordenamiento Taxonómico
  ord_taxonomy <- data_plot %>%
    dplyr::group_by(taxa_group) %>%
    dplyr::summarise(total = sum(col_N), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(total)) %>%
    dplyr::pull(taxa_group)

  data_plot <- data_plot %>%
    dplyr::mutate(taxa_group = factor(taxa_group, levels = ord_taxonomy))

  # 6. Manejo de Factores (Facet, Sitio, Zona)

  # Facet
  if (!is.null(col_facet)) {
    if (!is.null(ord_facet)) {
      data_plot <- data_plot %>% dplyr::mutate(col_facet = factor(col_facet, levels = ord_facet))
    } else {
      data_plot <- data_plot %>% dplyr::mutate(col_facet = as.factor(col_facet))
    }
  }

  # Sitio (Antes col_sitio)
  if (!is.null(col_site)) {
    if (!is.null(ord_site)) {
      data_plot <- data_plot %>% dplyr::mutate(col_site = factor(col_site, levels = ord_site))
    } else {
      data_plot <- data_plot %>% dplyr::mutate(col_site = factor(col_site, levels = sort(unique(col_site))))
    }
  }

  # Zona (Antes col_zona)
  if (!is.null(col_zone)) {
    if (!is.null(ord_zone)) {
      data_plot <- data_plot %>% dplyr::mutate(col_zone = factor(col_zone, levels = ord_zone))
    } else {
      data_plot <- data_plot %>% dplyr::mutate(col_zone = factor(col_zone, levels = sort(unique(col_zone))))
    }
  }

  # 7. Configuración de Colores
  col <- ggsci::pal_d3("category20c")(length(ord_taxonomy))
  names(col) <- ord_taxonomy

  # 8. Creación de Etiquetas Compuestas
  if (!is.null(col_site) || !is.null(col_zone)) {
    data_plot <- data_plot %>%
      dplyr::mutate(
        # Usamos nombres internos ya renombrados
        label_lugar = if (!is.null(col_site)) col_site else col_zone,
        label = glue::glue("{label_lugar} ({label})")
      )
  }

  # 9. Generación del Gráfico
  plot <- ggplot2::ggplot(data = data_plot, mapping = ggplot2::aes(area = col_N, fill = taxa_group, label = label)) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(
      grow = FALSE,
      reflow = TRUE,
      colour = "white",
      place = "centre",
      size = 12
    ) +
    ggplot2::scale_fill_manual(values = col, name = stringr::str_to_sentence(taxa_group)) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::labs(
      title = glue::glue("Composición taxonómica por {stringr::str_to_lower(taxa_group)} de {taxa_id}"),
      subtitle = "Composición derivada de la abundancia total de taxones"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Arial"),
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      strip.background = ggplot2::element_rect(fill = "gray20"),
      strip.text = ggplot2::element_text(face = "bold", colour = "white"),
      plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 12))
    )

  if (!is.null(col_facet)) {
    plot <- plot + ggplot2::facet_wrap(~col_facet, scales = "free", ncol = 1)
  }

  # Formato Itálico Condicional (Especies)
  if (stringr::str_detect(string = taxa_group, pattern = "(?i)(especies?|organismos?)")) {
    plot <- plot +
      ggplot2::theme(legend.text = ggplot2::element_text(face = "italic"))
  }

  # 10. Guardado y Retorno
  tryCatch(
    {
      ggplot2::ggsave(
        filename = output_name,
        plot = plot, width = width, height = height, dpi = 300
      )
    },
    error = function(e) warning("Error guardando imagen: ", e$message)
  )

  return(plot)
}
################################################################################
#' @title Graficar Índices de Diversidad por Taxón
#' @description Genera y guarda dos gráficos de barras apiladas: uno de Abundancia Total y otro de Riqueza de Taxones, desglosados por sitio y grupo taxonómico.
#' @param data Data frame con los datos.
#' @param col_site String. Nombre de la columna de sitios.
#' @param ord_site Vector (Opcional). Orden de los niveles de sitios.
#' @param col_N String. Nombre de la columna de abundancia.
#' @param col_taxa String. Nombre de la columna de taxones individuales (para conteo de riqueza).
#' @param taxon_group String. Nombre de la columna de agrupación taxonómica (ej. "Phylum", "Clase") para el relleno del gráfico.
#' @param col_facet String (Opcional). Nombre de la columna para facetar (ej. "Zona").
#' @param ord_facet Vector (Opcional). Orden de los niveles del facet.
#' @param taxa_id String. Identificador del grupo taxonómico general (ej. "Macroinvertebrados") para títulos y nombres de archivo.
#' @param width Numérico. Ancho de la imagen.
#' @param height Numérico. Alto de la imagen.
#' @param xlabel String (Opcional). Etiqueta personalizada para el eje X. Por defecto "Sitios".
#'
#' @return Invisible NULL. Guarda dos archivos PNG.
#' @export plot_index_by_taxa
#' @importFrom dplyr select rename group_by summarise mutate filter arrange pull n_distinct across desc all_of
#' @importFrom ggplot2 ggplot aes geom_bar scale_y_continuous scale_fill_manual labs theme_linedraw theme element_text element_rect facet_grid ggsave expansion
#' @importFrom ggsci pal_d3
#' @importFrom glue glue
#' @importFrom stringr str_to_sentence str_to_lower str_detect
#' @importFrom stats setNames
#' @importFrom scales label_number
#' @importFrom rlang sym "!!" "!!!"
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
                               xlabel = NULL) {
  # 1. Configuración Inicial
  op <- options(scipen = 999)
  on.exit(options(op))

  # Variables internas
  vars <- c(col_site, col_N, col_taxa, taxon_group, col_facet)

  # Renombrado dinámico para uso interno
  data_plot <- data %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::rename(
      col_site = !!rlang::sym(col_site),
      col_N = !!rlang::sym(col_N),
      col_taxa = !!rlang::sym(col_taxa),
      taxon_group = !!rlang::sym(taxon_group),
      !!!if (!is.null(col_facet)) stats::setNames(col_facet, "col_facet") else NULL
    )

  # Configuración de agrupación
  gr_by_cols <- c("col_site", "taxon_group")
  if ("col_facet" %in% names(data_plot)) gr_by_cols <- c(gr_by_cols, "col_facet")

  # Orden de Sitios
  ord_site <- if (is.null(ord_site)) {
    sort(unique(data_plot$col_site))
  } else {
    ord_site
  }

  # --- 2. Cálculo de Abundancia (N) ---
  res_abund <- data_plot %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(gr_by_cols))) %>%
    dplyr::summarise(
      N = sum(col_N, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Manejo de Facet como factor
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

  # --- 3. Cálculo de Riqueza (S) ---
  res_riqueza <- data_plot %>%
    dplyr::filter(!is.na(col_N), col_N > 0) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(gr_by_cols))) %>%
    dplyr::summarise(S = dplyr::n_distinct(col_taxa), .groups = "drop") %>%
    # Manejo de Facet como factor
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

  # --- 4. Definición de Orden de Clases (Taxones) ---
  # Ordenar taxones por abundancia total
  ord_abund <- res_abund %>%
    dplyr::group_by(taxon_group) %>%
    dplyr::summarise(total_N = sum(N), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(total_N)) %>%
    dplyr::pull(taxon_group)

  # Ordenar taxones por riqueza total
  ord_rich <- res_riqueza %>%
    dplyr::group_by(taxon_group) %>%
    dplyr::summarise(total_S = sum(S), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(total_S)) %>%
    dplyr::pull(taxon_group)

  # --- 5. Paleta de Colores Consistente ---
  # Unimos todos los taxones presentes en ambos datasets para que el color sea fijo
  all_taxon_group <- unique(c(as.character(ord_abund), as.character(ord_rich)))
  n_taxa_total <- length(all_taxon_group)

  col <- ggsci::pal_d3("category20c")(n_taxa_total)
  # Si hay más de 20 taxones, pal_d3 recicla o da error dependiendo de la versión,
  # pero tu código original confiaba en esto. Mantenemos lógica.
  # Asignamos nombres para scale_fill_manual
  # Nota: Si n_taxa_total > 20, ggsci podría quedarse corto.
  # Si necesitas más colores, considera colorRampPalette.
  # Mantenemos tu original por instrucción.
  names(col) <- all_taxon_group

  # Asignar niveles a los factores
  res_abund <- res_abund %>%
    dplyr::mutate(taxon_group = factor(taxon_group, levels = ord_abund))

  res_riqueza <- res_riqueza %>%
    dplyr::mutate(taxon_group = factor(taxon_group, levels = ord_rich))

  # Configuración etiqueta X
  xlab <- ifelse(is.null(xlabel), "Sitios", xlabel)

  # --- 6. Gráfico de Abundancia ---
  title_facet <- if (!is.null(col_facet)) glue::glue(" por {stringr::str_to_lower(col_facet)}") else ""

  p_abund <- ggplot2::ggplot(res_abund, ggplot2::aes(x = col_site, y = N, fill = taxon_group)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_y_continuous(
      name = "Abundancia total [N]",
      n.breaks = 6
    ) +
    ggplot2::scale_fill_manual(values = col, name = stringr::str_to_sentence(taxon_group)) +
    ggplot2::labs(
      title = glue::glue("Abundancia de taxones de {stringr::str_to_lower(taxa_id)}{title_facet}"),
      x = xlab
    ) +
    ggplot2::theme_linedraw(base_size = 10, base_family = "Arial") +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      strip.text = ggplot2::element_text(face = "bold", colour = "white"),
      strip.background = ggplot2::element_rect(fill = "#2c3e50"),
      plot.tag = ggplot2::element_text(face = "bold")
    )

  if (!is.null(col_facet)) {
    p_abund <- p_abund +
      ggplot2::facet_grid(~col_facet, scales = "free_x", space = "free_x")
  }

  if (stringr::str_detect(string = taxon_group, pattern = "(?i)(especies?|organismos?)")) {
    p_abund <- p_abund +
      ggplot2::theme(legend.text = ggplot2::element_text(face = "italic"))
  }

  # --- 7. Gráfico de Riqueza ---
  p_rich <- ggplot2::ggplot(res_riqueza, ggplot2::aes(x = col_site, y = S, fill = taxon_group)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_y_continuous(
      name = "Riqueza de taxones [S]",
      n.breaks = 6,
      labels = scales::label_number(accuracy = 1)
    ) +
    ggplot2::scale_fill_manual(values = col, name = stringr::str_to_sentence(taxon_group)) +
    ggplot2::labs(
      title = glue::glue("Riqueza de taxones de {stringr::str_to_lower(taxa_id)}{title_facet}"),
      x = xlab
    ) +
    ggplot2::theme_linedraw(base_size = 10, base_family = "Arial") +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      strip.text = ggplot2::element_text(face = "bold", colour = "white"),
      strip.background = ggplot2::element_rect(fill = "#2c3e50"),
      plot.tag = ggplot2::element_text(face = "bold")
    )

  if (!is.null(col_facet)) {
    p_rich <- p_rich +
      ggplot2::facet_grid(~col_facet, scales = "free_x", space = "free_x")
  }

  if (stringr::str_detect(string = taxon_group, pattern = "(?i)(especies?|organismos?)")) {
    p_rich <- p_rich +
      ggplot2::theme(legend.text = ggplot2::element_text(face = "italic"))
  }

  # --- 8. Guardado ---
  tryCatch(
    {
      ggplot2::ggsave(filename = glue::glue("plot_riqueza_sitio_{taxa_id}.png"), plot = p_rich, width = width, height = height, dpi = 300)
      ggplot2::ggsave(filename = glue::glue("plot_abundancia_sitio_{taxa_id}.png"), plot = p_abund, width = width, height = height, dpi = 300)
    },
    error = function(e) warning("Error guardando gráficos: ", e$message)
  )

  return(invisible(NULL))
}
################################################################################
#' @title Calcular Índices de Diversidad (N, S, H)
#' @description Agrupa datos, calcula abundancia (N), riqueza (S) y Shannon (H) usando `vegan`, y guarda el resultado.
#' @param data Data frame con los datos.
#' @param col_N String. Nombre de la columna de abundancia o valor.
#' @param col_site String. Nombre de la columna de sitios.
#' @param col_sampling String (Opcional). Nombre de la columna de muestreo/campaña.
#' @param col_taxa String. Nombre de la columna de taxones.
#' @param col_zone String (Opcional). Nombre de la columna de zonas.
#' @param col_rep String (Opcional). Nombre de la columna de réplicas.
#' @param output_name String. Nombre del archivo de salida (.csv).
#' @param fun Función o nombre de función (ej. "sum", "mean") para agregar los datos antes de calcular índices.
#' @return Un data frame con los índices calculados (invisible).
#' @export get_index_df
#' @importFrom dplyr select rename filter group_by summarise mutate bind_cols starts_with all_of across
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym "!!" "!!!" as_function
#' @importFrom stats setNames
#' @importFrom stringr str_detect str_replace
#' @importFrom vegan specnumber diversity
#' @importFrom readr write_csv
#' @title Calcular Índices de Diversidad (N, S, H)
#' @description Agrupa datos, calcula abundancia (N), riqueza (S) y Shannon (H) usando `vegan`, y guarda el resultado en CSV.
#' @param data Data frame con los datos.
#' @param col_value String. Nombre de la columna de abundancia o valor.
#' @param col_site String. Nombre de la columna de sitios.
#' @param col_sampling String (Opcional). Nombre de la columna de muestreo/campaña.
#' @param col_taxa String. Nombre de la columna de taxones.
#' @param col_zone String (Opcional). Nombre de la columna de zonas.
#' @param col_rep String (Opcional). Nombre de la columna de réplicas.
#' @param output_name String. Nombre del archivo de salida (.csv).
#' @param fun Función o nombre de función (ej. "sum", "mean") para agregar los datos antes de calcular índices.
#' @return Un data frame con los índices calculados (invisible).
#' @export get_index_df
#' @importFrom dplyr select rename filter group_by summarise mutate bind_cols starts_with all_of across
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym "!!" "!!!" as_function
#' @importFrom stats setNames
#' @importFrom stringr str_detect
#' @importFrom vegan specnumber diversity
#' @importFrom readr write_csv
#' @importFrom glue glue
get_index_df <- function(data,
                         col_value,
                         col_site,
                         col_sampling = NULL,
                         col_taxa,
                         col_zone = NULL,
                         col_rep = NULL,
                         output_name,
                         fun = sum) {
  # 1. Configuración
  op <- options(scipen = 999)
  on.exit(options(op))

  # Convertir argumento 'fun' a función ejecutable
  fun <- rlang::as_function(fun)

  # 2. Selección y Renombrado Dinámico
  vars_to_select <- c(col_value, col_sampling, col_site, col_taxa, col_zone, col_rep)

  data_plot <- data %>%
    dplyr::select(dplyr::all_of(vars_to_select)) %>%
    dplyr::rename(
      col_value = !!rlang::sym(col_value),
      col_site = !!rlang::sym(col_site),
      col_taxa = !!rlang::sym(col_taxa),
      !!!if (!is.null(col_zone)) stats::setNames(col_zone, "col_zone") else NULL,
      !!!if (!is.null(col_sampling)) stats::setNames(col_sampling, "col_sampling") else NULL,
      !!!if (!is.null(col_rep)) stats::setNames(col_rep, "col_rep") else NULL
    ) %>%
    dplyr::filter(!is.na(col_taxa))

  # 3. Agregación de Datos
  grouping_cols <- setdiff(names(data_plot), "col_value")

  data_index <- data_plot %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
    dplyr::summarise(col_value = fun(col_value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "col_taxa", values_from = "col_value", values_fill = 0)

  # 4. Cálculo de Índices con Vegan
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

  # 5. Guardado y Retorno
  tryCatch(
    {
      # Asegurar extensión csv usando glue
      if (!stringr::str_detect(output_name, "\\.csv$")) {
        output_name <- glue::glue("{output_name}.csv")
      }

      readr::write_csv(x = summ_index, file = output_name)
    },
    error = function(e) warning("Error guardando archivo CSV: ", e$message)
  )

  return(invisible(summ_index))
}
################################################################################

################################################################################
#' @title Abreviar Nombres Taxonómicos con Desambiguación
#' @description Genera códigos abreviados. Si detecta colisiones (mismo código para especies distintas), aumenta la longitud del género para diferenciarlas.
#'
#' @param taxa_name Vector de caracteres con los nombres científicos.
#'
#' @return Un vector de caracteres con las abreviaciones.
#' @export abbreviate_taxa
#' @importFrom stringr str_squish str_trim str_replace_all word str_detect str_to_lower str_c str_sub str_to_sentence
#' @importFrom dplyr case_when tibble mutate group_by add_count ungroup pull if_else n
#' @importFrom rlang .data
abbreviate_taxa <- function(taxa_name) {
  # Creamos un tibble temporal para manejar el vector y detectar duplicados fácilmente
  df <- dplyr::tibble(original = taxa_name) %>%
    dplyr::mutate(
      # 1. Limpieza inicial
      clean = stringr::str_squish(
        stringr::str_trim(
          stringr::str_replace_all(original, "\\.", "")
        )
      ),

      # 2. Extracción de palabras
      w1 = stringr::word(clean, 1),
      w2 = stringr::word(clean, 2),
      w3 = stringr::word(clean, 3),

      # 3. Detección de patrones
      w2_sp = !is.na(w2) & stringr::str_detect(stringr::str_to_lower(w2), "^spp?$"),
      w2_cf = !is.na(w2) & stringr::str_detect(stringr::str_to_lower(w2), "^cf$"),
      w2_num = !is.na(w2) & stringr::str_detect(w2, "^\\d+$"),
      w3_num = !is.na(w3) & stringr::str_detect(w3, "^\\d+$"),
      solo_una = is.na(w2) | w2_sp,

      # 4. Generación de Sigla Base (Intento 1: Estándar)
      # Generalmente: 1 letra Genero + 4 letras Especie
      sigla_base = dplyr::case_when(
        # Caso: "sp" + número (Chaetoceros sp 1 -> Chaet1)
        w2_sp & w3_num ~ stringr::str_c(stringr::str_sub(w1, 1, 5), w3),

        # Caso: "cf." (Genus cf. species -> Gspec)
        w2_cf & !is.na(w3) ~ stringr::str_c(stringr::str_sub(w1, 1, 1), stringr::str_sub(w3, 1, 4)),

        # Caso: Solo género o sp sin numero -> Genus (5 letras)
        solo_una ~ stringr::str_sub(w1, 1, 5),

        # Caso: Genero + numero (Genus 123 -> Genu123)
        w2_num ~ stringr::str_c(stringr::str_sub(w1, 1, 4), w2),

        # Caso General: Genus species -> Gspec (1G + 4S)
        TRUE ~ stringr::str_c(stringr::str_sub(w1, 1, 1), stringr::str_sub(w2, 1, 4))
      )
    )

  # 5. Detección y Resolución de Colisiones
  # Agrupamos por la sigla generada. Si hay más de un nombre original diferente para la misma sigla, hay colisión.

  df_final <- df %>%
    dplyr::group_by(sigla_base) %>%
    dplyr::mutate(
      n_distinct_originals = dplyr::n_distinct(original), # Cuántas especies distintas cayeron en esta sigla

      # Generación Sigla Extendida (Intento 2: Resolución)
      # Estrategia: 3 letras Genero + 3 letras Especie (Chaetoceros curvisetus -> Chacur)
      sigla_ext = dplyr::case_when(
        # Si es un caso especial (sp, cf, num), mantenemos la base porque suelen ser únicos por el número
        w2_sp | w2_num | (w2_cf & is.na(w3)) ~ sigla_base,

        # Si es el caso general (Genus species), aplicamos 3G + 3S
        w2_cf & !is.na(w3) ~ stringr::str_c(stringr::str_sub(w1, 1, 3), stringr::str_sub(w3, 1, 3)),
        TRUE ~ stringr::str_c(stringr::str_sub(w1, 1, 3), stringr::str_sub(w2, 1, 3))
      ),

      # Decisión final: Si hay colisión en la base, usa la extendida
      sigla_final = dplyr::if_else(n_distinct_originals > 1, sigla_ext, sigla_base)
    ) %>%
    dplyr::ungroup()

  # 6. Retorno formateado
  return(stringr::str_to_sentence(df_final$sigla_final))
}
################################################################################
#' @title Plot NMDS and PERMANOVA
#' @description Realiza un análisis NMDS con transformación de Hellinger. Si hay factor, dibuja elipses y calcula PERMANOVA. Si no, colorea por sitios.
#' @param data Data frame con los datos.
#' @param col_site String. Nombre de la columna de sitios.
#' @param col_N String. Nombre de la columna de abundancia.
#' @param col_taxa String. Nombre de la columna de taxones.
#' @param taxa_id String. Nombre del grupo taxonómico para el título (ej. "Fitoplancton").
#' @param col_factor String (Opcional). Nombre de la columna de agrupación. Si es NULL, se usa `col_site` para colorear.
#' @param legend_factor String (Opcional). Título personalizado para la leyenda del gráfico cuando se usa `col_factor`.
#' @param levels_factor Vector (Opcional). Orden de los niveles del factor.
#' @param height Numérico. Alto de la imagen. Default 6.
#' @param width Numérico. Ancho de la imagen. Default 6.
#' @param output_name String. Nombre del archivo de salida.
#' @param save_rds Lógico. Si es TRUE, guarda el objeto ggplot como .rds.
#' @return Un objeto ggplot con el NMDS.
#' @export plot_nmds
#' @importFrom dplyr select rename group_by summarise mutate bind_cols case_when rename_with all_of
#' @importFrom tidyr pivot_wider
#' @importFrom vegan decostand metaMDS adonis2 scores
#' @importFrom ggplot2 ggplot geom_hline geom_vline geom_text geom_point stat_ellipse scale_color_manual annotate guides labs coord_equal theme_bw theme element_text guide_legend aes ggsave
#' @importFrom ggsci pal_d3
#' @importFrom grDevices colorRampPalette
#' @importFrom glue glue
#' @importFrom stringr str_detect str_to_lower str_subset str_to_sentence regex str_replace
#' @importFrom rlang sym "!!"
#' @importFrom stats var setNames
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
                      save_rds = FALSE) {
  # 1. Configuración
  op <- options(scipen = 999)
  on.exit(options(op))

  # 2. Selección y Renombrado Dinámico
  vars_to_select <- c(col_taxa, col_N, col_site, col_factor)

  data_plot <- data %>%
    dplyr::select(dplyr::all_of(vars_to_select)) %>%
    dplyr::rename(
      col_taxa = !!rlang::sym(col_taxa),
      col_N = !!rlang::sym(col_N),
      col_site = !!rlang::sym(col_site),
      !!!if (!is.null(col_factor)) stats::setNames(col_factor, "col_factor") else NULL
    )

  # Flag de estadísticas
  run_stats <- !is.null(col_factor)

  # 3. Manejo de Colores (Factor vs Sitios)
  if (run_stats) {
    # Caso A: Factor explícito
    levels_fac <- if (!is.null(levels_factor)) levels_factor else sort(unique(data_plot$col_factor))
    data_plot <- data_plot %>%
      dplyr::mutate(col_factor = factor(col_factor, levels = levels_fac))
  } else {
    # Caso B: Sin factor -> Colorear por Sitios
    data_plot$col_factor <- as.factor(data_plot$col_site)
  }

  # 4. Transformación a Matriz de Comunidad
  matriz_comunidad <- data_plot %>%
    dplyr::group_by(col_site, col_taxa, col_factor) %>%
    dplyr::summarise(abundancia = sum(col_N, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "col_taxa", values_from = "abundancia", values_fill = 0)

  # Separar Metadata y Matriz
  metadata <- matriz_comunidad %>% dplyr::select(col_site, col_factor)
  matriz_especies <- matriz_comunidad %>% dplyr::select(-col_site, -col_factor)

  # 5. Transformación Hellinger y NMDS
  matriz_hel <- vegan::decostand(matriz_especies, method = "hellinger")
  NMDS1 <- vegan::metaMDS(matriz_hel, distance = "euclid", k = 2, trymax = 1000, autotransform = FALSE, trace = 0)

  # Scores
  data_nmds <- as.data.frame(vegan::scores(NMDS1)$sites)
  data_nmds <- dplyr::bind_cols(metadata, data_nmds)

  species_nmds <- as.data.frame(vegan::scores(NMDS1, display = "species"))
  species_nmds$species <- rownames(species_nmds)

  # 6. Estadísticas (PERMANOVA) - Solo si hay factor real
  subtitle_text <- NULL

  if (run_stats) {
    if (length(unique(data_nmds$col_factor)) > 1) {
      permanova <- vegan::adonis2(matriz_hel ~ col_factor, data = metadata, method = "euclid", permutations = 999)

      Fval <- round(permanova$F[1], 2)
      pval <- format.pval(permanova$`Pr(>F)`[1], digits = 3, eps = 0.001)
      R2 <- round(permanova$R2[1] * 100, 1)

      orig_factor_name <- col_factor
      label_factor_txt <- dplyr::case_when(
        stringr::str_detect(orig_factor_name, stringr::regex("sector", ignore_case = TRUE)) ~ "Sector",
        stringr::str_detect(orig_factor_name, stringr::regex("zona", ignore_case = TRUE)) ~ "Zonas",
        stringr::str_detect(orig_factor_name, stringr::regex("sitio?s?", ignore_case = TRUE)) ~ "Sitio",
        stringr::str_detect(orig_factor_name, stringr::regex("[ée]pocas?$", ignore_case = TRUE)) ~ "Época",
        TRUE ~ stringr::str_to_sentence(orig_factor_name)
      )

      subtitle_text <- glue::glue("PERMANOVA por {stringr::str_to_lower(label_factor_txt)}: F = {Fval}, R² = {R2}%, p = {pval}")
    }
  }

  # 7. Configuración Visual
  n_groups <- length(unique(data_nmds$col_factor))

  if (n_groups <= 20) {
    cols <- ggsci::pal_d3("category20")(n_groups)
  } else {
    base_colors <- ggsci::pal_d3("category20")(20)
    cols <- grDevices::colorRampPalette(base_colors)(n_groups)
  }
  names(cols) <- unique(data_nmds$col_factor)

  # --- LOGICA DEL TÍTULO DE LA LEYENDA ---
  legend_title <- if (run_stats) {
    # Si hay factor, usar legend_factor si existe, o el nombre del factor
    if (!is.null(legend_factor)) legend_factor else stringr::str_to_sentence(col_factor)
  } else {
    # Si no hay factor, siempre es "Sitios"
    "Sitios"
  }

  # 8. Plot
  title_text <- glue::glue("Análisis multivariado (NMDS) para {taxa_id}")

  plot <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linetype = 3, color = "gray0") +
    ggplot2::geom_vline(xintercept = 0, linetype = 3, color = "gray0") +

    # Especies
    ggplot2::geom_text(
      data = species_nmds,
      ggplot2::aes(x = NMDS1, y = NMDS2, label = species),
      fontface = "italic", alpha = 0.9, size = 2
    ) +

    # Puntos
    ggplot2::geom_point(
      data = data_nmds,
      ggplot2::aes(x = NMDS1, y = NMDS2, color = col_factor),
      size = 1, show.legend = TRUE
    ) +

    # Stress
    ggplot2::annotate("label",
      x = -Inf, y = -Inf, hjust = -0.1, vjust = -0.5,
      size = 3, fill = "lightgrey",
      label = paste("Stress:", round(NMDS1$stress, 3))
    ) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::labs(
      x = "Primera dimensión NMDS",
      y = "Segunda dimensión NMDS",
      colour = legend_title,
      title = title_text,
      subtitle = subtitle_text
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Arial"),
      legend.position = "right",
      legend.box = "vertical",
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      legend.key.height = grid::unit(0.5, "cm"),
      legend.text = ggplot2::element_text(size = 9),
      legend.spacing.y = grid::unit(0.1, "cm"),
      plot.caption = ggplot2::element_text(face = "bold.italic", size = 9, hjust = 0)
    )

  # Elipses (SIEMPRE que haya factor real y suficientes grupos)
  if (run_stats && n_groups > 1) {
    plot <- plot +
      ggplot2::stat_ellipse(
        data = data_nmds,
        ggplot2::aes(x = NMDS1, y = NMDS2, color = col_factor),
        type = "norm", linetype = 1, linewidth = 0.8, alpha = 0.5, level = 0.95
      )
  }

  # 9. Guardado
  tryCatch(
    {
      ggplot2::ggsave(filename = output_name, plot = plot, height = height, width = width, dpi = 300)

      if (save_rds) {
        rds_name <- stringr::str_replace(output_name, "\\.png$", ".rds")
        saveRDS(plot, file = rds_name)
        message(glue::glue("Objeto R guardado en: {rds_name}"))
      }
    },
    error = function(e) warning("Error guardando plot: ", e$message)
  )

  return(plot)
}
################################################################################
#' @title Plot Alluvial de Composición (Abundancia o Riqueza por Grupo)
#' @description Crea un gráfico aluvial. Ordena la leyenda por magnitud y respeta la lógica de riqueza total sin filtrar si se solicita.
#' @param data Data frame con los datos.
#' @param col_site String. Columna eje X.
#' @param ord_site Vector (Opcional). Orden de los sitios.
#' @param col_N String. Columna de abundancia.
#' @param col_taxa_group String. El grupo mayor que formará las cintas (ej. Clase, Familia).
#' @param col_taxa String (Opcional). La unidad menor a contar para riqueza (ej. Especie). Requerido si metric="richness".
#' @param taxa_id String. Nombre del grupo general para título.
#' @param col_facet String (Opcional). Columna para facetar.
#' @param ord_facet Vector (Opcional). Orden de los niveles del facet.
#' @param metric String. "abundance" (aplica Top N) o "richness" (Muestra TODOS los grupos, sin Top N).
#' @param legend_label String (Opcional). Título leyenda.
#' @param xlabel String. Título eje X.
#' @param top_n Entero. Filtra los N grupos principales (Solo aplica si metric="abundance").
#' @param output_name String. Nombre del archivo de salida (ej. "grafico.png").
#' @param width Numérico. Ancho imagen.
#' @param height Numérico. Alto imagen.
#' @param save_rds Lógico. Guardar RDS.
#' @return Un objeto ggplot.
#' @export plot_alluvial_taxa
#' @importFrom dplyr select group_by summarise mutate arrange slice_head pull if_else all_of filter n_distinct ungroup
#' @importFrom ggplot2 ggplot aes labs scale_fill_manual scale_y_continuous theme_bw theme element_text element_rect ggsave facet_grid
#' @importFrom ggalluvial geom_alluvium geom_stratum
#' @importFrom rlang sym "!!" "!!!"
#' @importFrom glue glue
#' @importFrom ggsci pal_d3
#' @importFrom forcats fct_relevel fct_reorder
#' @importFrom scales hue_pal
#' @importFrom stringr str_replace str_to_lower
plot_alluvial_taxa <- function(data,
                               col_site,
                               ord_site = NULL,
                               col_N,
                               col_taxa_group, # El grupo (Cinta)
                               col_taxa = NULL, # Lo que se cuenta (Especie)
                               taxa_id,
                               col_facet = NULL,
                               ord_facet = NULL,
                               metric = "abundance",
                               legend_label = NULL,
                               xlabel = "Sitios",
                               top_n = 10,
                               output_name, # Agregado porque se usa en ggsave
                               width = 10,
                               height = 6,
                               save_rds = FALSE) {
  # 1. Configuración
  op <- options(scipen = 999)
  on.exit(options(op))

  # Validación estricta para Riqueza
  if (metric == "richness" && is.null(col_taxa)) {
    stop("Error: Para calcular 'richness', debes especificar 'col_taxa' (la columna de especies a contar).")
  }

  # 2. Selección y Renombrado
  vars_select <- list(
    col_site = rlang::sym(col_site),
    col_N = rlang::sym(col_N),
    col_taxa_group = rlang::sym(col_taxa_group)
  )

  if (!is.null(col_taxa)) vars_select$col_taxa <- rlang::sym(col_taxa)
  if (!is.null(col_facet)) vars_select$col_facet <- rlang::sym(col_facet)

  data_plot <- data %>%
    dplyr::select(!!!vars_select)

  # 3. Lógica de Cálculo (Abundancia vs Riqueza)
  grp_base <- c("col_site", "col_taxa_group")
  if (!is.null(col_facet)) grp_base <- c(grp_base, "col_facet")

  if (metric == "richness") {
    # --- MODO RIQUEZA ---
    # Se cuenta la riqueza de 'col_taxa' dentro de 'col_taxa_group'
    data_calc <- data_plot %>%
      dplyr::filter(col_N > 0) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_base))) %>%
      dplyr::summarise(final_value = dplyr::n_distinct(col_taxa), .groups = "drop")

    y_label <- "Riqueza de taxa [S]"
    text_string <- "la riqueza de taxa"
    if (is.null(legend_label)) legend_label <- "Grupo"
  } else {
    # --- MODO ABUNDANCIA ---
    data_calc <- data_plot %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_base))) %>%
      dplyr::summarise(final_value = sum(col_N, na.rm = TRUE), .groups = "drop")

    y_label <- "Abundancia [N]"
    text_string <- "la abundancia"
    if (is.null(legend_label)) legend_label <- "Taxón"
  }

  # 4. Lógica de Filtrado y Etiquetado (Corregida según tu instrucción)

  if (metric == "richness") {
    # Si es riqueza, NO usamos Top N. Usamos TODOS los grupos.
    top_groups <- unique(data_calc$col_taxa_group)
  } else {
    # Si es abundancia, calculamos ranking y cortamos por Top N
    ranking <- data_calc %>%
      dplyr::group_by(col_taxa_group) %>%
      dplyr::summarise(total = sum(final_value, na.rm = TRUE)) %>%
      dplyr::arrange(dplyr::desc(total))

    top_groups <- ranking %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::pull(col_taxa_group)
  }

  # Re-etiquetar (Top N vs Others) y Sumarizar Final
  grp_final <- c("col_site", "taxa_label")
  if (!is.null(col_facet)) grp_final <- c(grp_final, "col_facet")

  data_alluvial <- data_calc %>%
    dplyr::mutate(
      taxa_label = dplyr::if_else(col_taxa_group %in% top_groups, as.character(col_taxa_group), "Others")
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_final))) %>%
    dplyr::summarise(final_value = sum(final_value, na.rm = TRUE), .groups = "drop")

  # --- CORRECCIÓN 2: ORDENAR LEYENDA POR MAGNITUD ---
  # Calculamos el total por etiqueta para ordenar el factor
  total_per_label <- data_alluvial %>%
    dplyr::group_by(taxa_label) %>%
    dplyr::summarise(total = sum(final_value, na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(total))

  # Definimos los niveles ordenados por total descendente
  ordered_levels <- total_per_label$taxa_label

  # Si existe "Others", forzamos que vaya al final
  if ("Others" %in% ordered_levels) {
    ordered_levels <- c(setdiff(ordered_levels, "Others"), "Others")
  }

  # Aplicamos el factor ordenado
  data_alluvial <- data_alluvial %>%
    dplyr::mutate(taxa_label = factor(taxa_label, levels = ordered_levels))

  # 5. Ordenamiento de Ejes (Sitios y Facets)
  if (!is.null(ord_site)) {
    data_alluvial <- data_alluvial %>%
      dplyr::mutate(col_site = factor(col_site, levels = ord_site))
  }

  if (!is.null(col_facet)) {
    fac_levels <- if (!is.null(ord_facet)) ord_facet else sort(unique(data_alluvial$col_facet))
    data_alluvial <- data_alluvial %>%
      dplyr::mutate(col_facet = factor(col_facet, levels = fac_levels))
  }

  # 6. Colores
  n_colors <- length(unique(data_alluvial$taxa_label))

  if (n_colors <= 20) {
    cols <- ggsci::pal_d3("category20c")(n_colors)
  } else {
    cols <- scales::hue_pal()(n_colors)
  }

  names(cols) <- levels(data_alluvial$taxa_label) # Asegurar match con niveles ordenados
  if ("Others" %in% names(cols)) cols["Others"] <- "grey80"

  # 7. Plot
  plot <- ggplot2::ggplot(
    data_alluvial,
    ggplot2::aes(x = col_site, y = final_value, alluvium = taxa_label)
  ) +
    ggalluvial::geom_alluvium(
      ggplot2::aes(fill = taxa_label),
      alpha = 0.5,
      width = 1 / 5,
      color = NA
    ) +
    ggalluvial::geom_stratum(
      ggplot2::aes(stratum = taxa_label, fill = taxa_label),
      width = 1 / 5,
      alpha = 1,
      color = "grey20"
    ) +
    ggplot2::labs(
      title = glue::glue("Dinámica espacial de {stringr::str_to_lower(text_string)} de {taxa_id}"),
      y = y_label,
      x = xlabel,
      fill = legend_label
    ) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_y_continuous(n.breaks = 10) +
    ggplot2::theme_bw(base_size = 12, base_family = "Arial") +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", colour = "white"),
      strip.background = ggplot2::element_rect(fill = "#2c3e50"),
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )

  if (!is.null(col_facet)) {
    plot <- plot + ggplot2::facet_grid(~col_facet, scales = "free_x", space = "free")
  }

  # 8. Guardado
  tryCatch(
    {
      ggplot2::ggsave(filename = output_name, plot = plot, width = width, height = height, dpi = 300)

      if (save_rds) {
        rds_name <- stringr::str_replace(output_name, "\\.(png|jpg|jpeg|pdf|tiff)$", ".rds")
        if (rds_name == output_name) rds_name <- paste0(output_name, ".rds")
        # Corregido: guardar 'plot', no 'p'
        saveRDS(plot, file = rds_name)
        message(paste("Objeto con grafico aluvial (ggplot) guardado en:", rds_name))
      }
    },
    error = function(e) warning("Error guardando imagen/objeto: ", e$message)
  )

  return(plot)
}
#' @title Graficar Variación Espacial de Índices Comunitarios
#' @description Crea un gráfico de líneas y puntos facetado por tipo de índice (N, S, H). Asigna paletas de colores específicas según el nombre de la leyenda.
#' @param data Data frame con los datos.
#' @param col_value String. Nombre de la columna con el valor del índice.
#' @param col_index String. Nombre de la columna que indica el tipo de índice (N, S, H).
#' @param col_site String. Nombre de la columna de sitios (Eje X).
#' @param ord_site Vector (Opcional). Orden de los niveles de sitios.
#' @param col_rep String (Opcional). Nombre de la columna para agrupar líneas/colores (ej. Réplica, Campaña, Estrato).
#' @param col_facet String (Opcional). Nombre de la columna para facetar horizontalmente (ej. Zona).
#' @param ord_facet Vector (Opcional). Orden de los niveles del facet.
#' @param taxa_id String. Identificador del taxón para el título.
#' @param legend_name String. Título de la leyenda. Determina la paleta de colores ("estratos" activa paleta azul fija).
#' @param width Numérico. Ancho de la imagen. Default 8.
#' @param height Numérico. Alto de la imagen. Default 6.
#' @param xlabel String. Etiqueta para el eje X. Default "Sitios".
#' @param output_name String. Nombre del archivo de salida.
#' @param save_rds Lógico. Si es TRUE, guarda también el objeto R (.rds). Por defecto FALSE.
#' @return Un objeto ggplot (invisible).
#' @export plot_index
#' @importFrom dplyr select rename mutate all_of
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs scale_color_manual facet_grid theme_linedraw theme element_text element_rect ggsave
#' @importFrom rlang sym "!!" "!!!"
#' @importFrom stats setNames
#' @importFrom stringr str_detect str_replace
#' @importFrom ggsci pal_jco pal_npg
#' @importFrom glue glue
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
                       xlabel = "Sitios",
                       save_rds = FALSE) {
  # 1. Configuración
  op <- options(scipen = 999)
  on.exit(options(op))

  # 2. Selección y Renombrado Dinámico
  data_plot <- data %>%
    dplyr::select(
      col_value = !!rlang::sym(col_value),
      col_site = !!rlang::sym(col_site),
      col_index = !!rlang::sym(col_index),
      !!!if (!is.null(col_facet)) stats::setNames(col_facet, "col_facet") else NULL,
      !!!if (!is.null(col_rep)) stats::setNames(col_rep, "col_rep") else NULL
    ) %>%
    dplyr::mutate(
      col_index = factor(col_index,
        levels = c("N", "S", "H"),
        labels = c(
          "Abundancia total [N]",
          "Riqueza de taxones [S]",
          "Diversidad de Shannon [H']"
        )
      )
    )

  # 3. Manejo de Facet (Ordenamiento)
  if (!is.null(col_facet)) {
    lvls <- if (!is.null(ord_facet)) ord_facet else sort(unique(data_plot$col_facet))
    data_plot <- data_plot %>%
      dplyr::mutate(col_facet = factor(col_facet, levels = lvls))
  }

  # 4. Manejo de Colores y Agrupación (col_rep)
  cols <- NULL # Inicializar

  if (!is.null(col_rep)) {
    # Ordenamiento base
    data_plot <- data_plot %>%
      dplyr::mutate(col_rep = factor(col_rep, levels = sort(unique(col_rep))))

    # Lógica de Paletas según 'legend_name'
    if (stringr::str_detect(string = legend_name, pattern = "(?i)estratos?")) {
      # Caso Estratos: Paleta Azul Fija
      cols <- c("#87CEFF", "#00868B", "#1874CD")
      names(cols) <- c("Superficie", "Medio", "Fondo")

      # Forzar niveles para coincidir con la paleta fija
      data_plot <- data_plot %>%
        dplyr::mutate(col_rep = factor(col_rep, levels = names(cols)))
    } else if (stringr::str_detect(string = legend_name, pattern = "(?i)r[eé]plicas?")) {
      # Caso Réplicas: Paleta JCO
      n_cols <- length(unique(data_plot$col_rep))
      cols <- ggsci::pal_jco("default")(n_cols)
      names(cols) <- unique(data_plot$col_rep)
    } else {
      # Caso Default: Paleta NPG
      n_cols <- length(unique(data_plot$col_rep))
      cols <- ggsci::pal_npg("nrc")(n_cols)
      names(cols) <- unique(data_plot$col_rep)
    }
  }

  # 5. Orden de Sitios
  ord_site_final <- if (is.null(ord_site)) {
    sort(unique(data_plot$col_site))
  } else {
    ord_site
  }

  data_plot <- data_plot %>%
    dplyr::mutate(col_site = factor(col_site, levels = ord_site_final))

  # 6. Construcción del Gráfico
  plot <- ggplot2::ggplot(
    data = data_plot,
    mapping = ggplot2::aes(
      x = col_site,
      y = col_value,
      colour = col_rep,
      group = col_rep
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = xlabel,
      y = "Valor índice",
      title = glue::glue("Variación espacial de índices comunitarios para {taxa_id}")
    ) +
    ggplot2::scale_color_manual(
      values = cols,
      name = legend_name
    ) +
    ggplot2::facet_grid(col_index ~ col_facet, scales = "free", space = "free_x", switch = "y") +
    ggplot2::theme_linedraw(base_size = 10, base_family = "Arial") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      strip.text = ggplot2::element_text(face = "bold", colour = "white"),
      strip.background = ggplot2::element_rect(fill = "#2c3e50")
    )

  # 7. Guardado
  tryCatch(
    {
      ggplot2::ggsave(
        filename = output_name,
        plot = plot,
        width = width,
        height = height,
        dpi = 300
      )

      if (save_rds) {
        rds_name <- stringr::str_replace(output_name, "\\.png$", ".rds")
        saveRDS(plot, file = rds_name)
        message(glue::glue("Objeto R guardado en: {rds_name}"))
      }
    },
    error = function(e) warning("Error guardando plot: ", e$message)
  )

  return(plot)
}
