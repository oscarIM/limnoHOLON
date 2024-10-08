% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sources_fn_abiotic.R
\name{fn_plot_pca}
\alias{fn_plot_pca}
\title{fn_plot_pca}
\usage{
fn_plot_pca(
  data,
  col_pars,
  col_sitio,
  col_valor,
  col_factor = NULL,
  ord_factor = NULL,
  matriz,
  data_pars,
  width = 6,
  height = 6,
  dist = "euc"
)
}
\arguments{
\item{data}{archivo entrada que tiene que tener, al menos, las columnas: sitio, parámetros (nombre o sigla), valor de parámetros. Tiene que estar en formato "long". Se recomida usar csv o tsv como formatos.}

\item{col_pars}{string que indica el nombre de la columna en data que tiene los parámetros (su sigla).}

\item{col_sitio}{string que indica el nombre de la columna que tiene los sitios de muestreo.}

\item{col_valor}{string que indica el nombre de la columna que tiene el valor de los parámetros.}

\item{ord_factor}{string que indica el orden en el que se quiera que aparezan en las leyendas y/o ejes los iteml del grupo. Solo tiene sentido si se usa col_grupo. Por defecto, Nulo.}

\item{matriz}{string que indica el nombre de la "matriz" de datos que se esta analizando. Sirve principalmente como ID para las salidas y para diferenciar entre sedimentos y aguas.}

\item{data_pars}{"dataframe" que contenga los parámetros (sus siglas) tal como aparecen en el "dataframe" de entrada, junto a un columna que indique a que tipo de parámetro pertenece (i.e. Físico, Químico, Nutriente, Metal, Orgánico e inorgánico y Biológico).Las columnas se tienen que llamar: Param y cat_pars}

\item{width}{ancho del gráfico. Por defecto, toma valor 6}

\item{height}{alto del gráfico. Por defecto, toma valor 6}

\item{dist}{cadena de texto que indica la distancia a usar para el calculo de la disimilitud para Permanova ("euc", "bray"). Por defecto, toma valor "euc".}

\item{code_sitio}{string que indica el código (generalmente una letra seguida un guión) que utilizada para nombras las estaciones (eg. "E-", "P-", "D-"). Se esperan puntos de muestreos asignados de forma consecutiva y sin gaps.}

\item{col_grupo}{string que indica el nombre del de la columna que contiene una variable de agrupamiento para el plot (e.g. zonas, campañas, etc.). Por defecto, Nulo.}
}
\value{
gráfico pca
}
\description{
función para graficar resultados de análisis de PCA.
}
\examples{
\dontrun{
#figura sin factor de agrupamiento
data <- readr::read_tsv("data_agua_RDLP.tsv")
col_pars <- "Param"
col_sitio <- "Sitio"
col_valor <- "Resultado"
matriz <- "agua"
data_pars <- readr::read_tsv("tabla_pars_master.tsv")
width <- 7
height <- 7
dist<- "euc"
fn_plot_pca(data = data,col_pars = col_pars,col_sitio = col_sitio, col_valor = col_valor, data_pars = data_pars,width = width, height = height, matriz = matriz)
#figura con 1 factor de agrupamiento#
data <- readr::read_tsv("data_agua_VEN.tsv")
col_pars <- "Param"
col_sitio <- "Sitio"
col_valor <- "Valor"
matriz <- "agua"
code_sitio <- "P-"
data_pars <- readr::read_tsv("tabla_pars_master.tsv")
col_grupo <- "zonas"
ord_factor <- c("Q5","Q3","Q2","Q1","L","Q4")
width <- 9
height <- 8
fn_plot_pca(data = data,col_pars = col_pars,col_sitio = col_sitio, col_valor = col_valor, data_pars = data_pars,width = width, height = height,matriz = matriz,col_grupo = col_grupo,ord_factor = ord_factor)
}
}
