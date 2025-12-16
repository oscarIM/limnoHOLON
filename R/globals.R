#' @importFrom utils globalVariables
utils::globalVariables(c(
  # --- Generales (Argumentos comunes y columnas estándar) ---
  "col_pars", "col_value", "col_site", "col_units",
  "col_year", "col_factor", "col_group", "col_rep", "col_x", "col_y",
  "col_facet", "type_par", "matriz", "Sigla", "Valor", "Unidades",

  # --- get_summ_stats ---
  "col_bld", "Nobs", "prom", "desvest", "cv_num", "n_bld", "prop_bld",
  "cv%", "BLD%", "min", "max",

  # --- plot_correlogram ---
  "Var1", "Var2", "Cor", "Freq", "Pval", "sig", "rowname",

  # --- plot_pca ---
  "xvar", "yvar", "varname", "angle", "hjust", "where", # where es usado en select

  # --- get_glm ---
  "col_pars_internal", "col_value_internal",
  "efecto", "Parametros", "Null_deviance", "Pr(>Chi)", # Salida anova
  "deviance", "null_deviance", "parametros", "expl_percent", "%DEM", # Salida janitor/calculada

  # --- plot_boxplot ---
  "col_label", "col_y_plot", "lim_superior", "upper_whisker",
  "p_value", "statistic", "label_test", "lbl", "q3", "iqr",
  "p.adj.signif", # Generado por rstatix
  "step", "y.position",

  # --- plot_parameter_levels ---
  "label", "nombre_norma", "tipo_limite", "limite", "excede",
  
  # --- ine_plot_all_ol ---
  "inx_row", "is_BLD1", "ol_dbscan", "group_aes"
))