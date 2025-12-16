#' Calcular coordenadas para Biplot PCA (Función Interna)
#' @keywords internal
utils_get_pca_biplot <- function(pca_obj) {
  choices <- 1:2
  scale <- 1
  obs.scale <- 1 - scale
  var.scale <- scale
  varname.adjust <- 1.5

  # Extract components
  nobs.factor <- sqrt(nrow(pca_obj$x) - 1)
  d <- pca_obj$sdev
  u <- sweep(pca_obj$x, 2, 1 / (d * nobs.factor), FUN = "*")
  v <- pca_obj$rotation

  # Scores
  choices <- pmin(choices, ncol(u))
  df_u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale, FUN = "*"))

  # Directions
  v <- sweep(v, 2, d^var.scale, FUN = "*")
  df_v <- as.data.frame(v[, choices])

  names(df_u) <- c("xvar", "yvar")
  names(df_v) <- names(df_u)

  df_u <- df_u * nobs.factor
  df_u <- as.data.frame(df_u)

  # Scale directions based on chi-square
  r <- sqrt(stats::qchisq(0.69, df = 2)) * prod(colMeans(df_u^2))^(1 / 4)
  v_scale <- rowSums(v^2)
  df_v <- r * df_v / sqrt(max(v_scale))
  df_v$varname <- rownames(v)

  # Variables for text label placement
  df_v$angle <- (180 / pi) * atan(df_v$yvar / df_v$xvar)
  df_v$hjust <- (1 - varname.adjust * sign(df_v$xvar)) / 2

  return(list(scores = df_u, directions = df_v))
}