#' Calcular coordenadas para Biplot PCA (Función Interna)
#' @keywords internal
get_data_pca_plot <- function(pca_obj) {
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
  df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale, FUN = "*"))
  # Directions
  v <- sweep(v, 2, d^var.scale, FUN = "*")
  df.v <- as.data.frame(v[, choices])
  names(df.u) <- c("xvar", "yvar")
  names(df.v) <- names(df.u)
  df.u <- df.u * nobs.factor
  df.u <- as.data.frame(df.u)

  r <- sqrt(stats::qchisq(0.69, df = 2)) * prod(colMeans(df.u^2))^(1 / 4)
  # Scale directions
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))
  df.v$varname <- rownames(v)
  # Variables for text label placement
  df.v$angle <- with(df.v, (180 / pi) * atan(yvar / xvar))
  df.v$hjust <- with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
  return(list(scores = df.u, directions = df.v))
}
