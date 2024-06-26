% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sources_fn_biotic.R
\name{fn_plot_div_index}
\alias{fn_plot_div_index}
\title{fn_plot_div_index}
\usage{
fn_plot_div_index(
  data,
  col_N,
  col_sitio,
  code_sitio,
  col_taxa,
  col_factor = NULL,
  ord_sitio = NULL,
  ord_factor = NULL,
  taxa_id,
  fun = "sum",
  width = 8,
  height = 8
)
}
\arguments{
\item{data}{archivo entrada que tiene que tener, al menos, las columnas que contengan a los taxones, N y sitios de muestreo. Tiene que estar en formato "long". Se recomida usar csv o tsv como formatos.}

\item{col_N}{cadena de texto que indica el nombre de la columna que contiene el N (abundancia) en la base de datos de entrada.}

\item{col_sitio}{cadena de texto que indica el nombre de la columna que contiene los sitios de muestreo en la base de datos de entrada.}

\item{code_sitio}{cadena de texto que indica el código (generalmente una letra seguida un guión) que utilizada para nombras las estaciones (eg. "E-", "P-", "D-").}

\item{col_taxa}{cadena de texto que indica el nombre de la columna que contiene los taxones (generalmente se llama especie, taxa, sigla) en la base de datos de entrada.}

\item{col_factor}{cadena de texto que indica el nombre de la columna que contiene al factor de agrupamiento (zonas, campañas, etc) en la base de datos de entrada. Por defecto, Nulo.}

\item{ord_sitio}{cadena de texto que indica el orden en el que se quiere aparezcan los sitios en el gráfico (de forma ascendente o descendente de acuerdo al número de estaciones o puntos de muestreo). Por defecto, toma valor "asc" (orden ascendente).}

\item{ord_factor}{cadena de texto que indica el orden en el cual se quiere que aparezcan los niveles del factor en el leyenda. Solo tiene sentido al usarlo con col_factor. Por defecto, Nulo.}

\item{taxa_id}{cadena de texto que indica el nombre del "grupo funcional" al cual pertenecen los taxones ("fitoplancton", "zooplancton", "macrofitas", "perifiton", "ictiofauna",etc.).}

\item{fun}{cadena de texto que indica la función aritmética para el calculo de los indices a través de los sitios y/o factores. Por defecto, toma valor "sum".}

\item{width}{ancho del gráfico. Por defecto, toma valor 8.}

\item{height}{alto del gráfico. Por defecto, toma valor 8.}
}
\value{
gráfico  con los indices de diversidad calculados por sitios y/o factor.
}
\description{
función para graficar indices de diversidad a lo largo de estaciones o sitios de muestreo
}
\examples{
\dontrun{
# plot indices diversidad sin factor de agrupamiento, ni orden de
data <- read_tsv("data_fitoplancton.tsv")
col_N <- "N"
col_sitio <- "Sitio"
code_sitio <- "P-"
col_taxa <- "Sigla"
fun = "sum"
ord_sitio <- "asc"
taxa_id <-  "fitoplancton"
height <- 6
width <- 6
fn_plot_div_index(data = data, col_sitio = col_sitio, code_sitio = code_sitio,col_taxa = col_taxa,fun = fun, ord_sitio = ord_sitio, col_N = col_N, taxa_id = taxa_id,height = height, width = width, )
# plot indices diversidad con factor de agrupamiento y en orden alfanumérico.
data <- read_tsv("data_fitoplancton.tsv")
fake data
col_N <- "N"
col_sitio <- "Sitio"
code_sitio <- "P-"
col_taxa <- "Sigla"
fun <- "sum"
ord_sitio <- "asc"
taxa_id <- "fitoplancton"
col_factor <- "Grupo"
height <- 10
width <- 10
fn_plot_div_index(data = data, col_sitio = col_sitio, code_sitio = code_sitio,col_taxa = col_taxa,col_factor = col_factor,fun = fun, ord_sitio = ord_sitio, col_N = col_N, taxa_id = taxa_id, height = height, width = width)
}
}
