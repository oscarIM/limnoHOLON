% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sources_fn_biotic.R
\name{fn_plot_nmds}
\alias{fn_plot_nmds}
\title{fn_plot_nmds}
\usage{
fn_plot_nmds(
  data,
  col_sitio,
  col_taxa,
  col_N,
  dist = "bray",
  col_replica = NULL,
  col_factor = NULL,
  ord_factor = NULL,
  taxa_id,
  height = 6,
  width = 7,
  title_factor = NULL
)
}
\arguments{
\item{data}{archivo entrada que tiene que tener, al menos, las columnas que representen al taxón, grupo taxonómico, taxones y N. Tiene que estar en formato "long". Se recomida usar csv o tsv como formatos.}

\item{col_sitio}{cadena de texto que indica el nombre de la columna que tiene los sitios de muestreo en la base de datos de entrada.}

\item{col_taxa}{cadena de texto que indica el nombre de la columna que contiene los taxones (generalmente se llama especie, taxa, sigla) en la base de datos de entrada.}

\item{col_N}{cadena de texto que indica el nombre de la columna que contiene el N (abundancia) en la base de datos de entrada.}

\item{dist}{cadena de texto que indica la distancia a usar para el calculo de la disimilitud en el NMDS ("euc", "bray"). Por defecto, toma valor "bray".}

\item{col_replica}{cadena de texto que indica el nombre de la columna que contiene las réplicas (zonas, campañas, etc) en la base de datos de entrada. Por defecto, Nulo.}

\item{col_factor}{cadena de texto que indica el nombre de la columna que contiene al factor de agrupamiento (zonas, campañas, etc) en la base de datos de entrada. Por defecto, Nulo.}

\item{ord_factor}{cadena de texto qeu indica el orden en el cual se quiere que aprazcan los niveles del factor en el leyenda.}

\item{taxa_id}{cadena de texto que indica el nombre del "grupo funcional" al cual pertenecen los taxones ("fitoplancton", "zooplancton", "macrofitas", "perifiton", "ictiofauna",etc.).}

\item{height}{alto del gráfico. Por defecto, toma valor 6.}

\item{width}{ancho del gráfico. Por defecto, toma valor 7.}

\item{title_factor}{cadena de texto que indica el nombre que se prefiere en la leyenda y subtititulo del gráfico. Por defecto, NULL.}
}
\value{
gráfico NMDS para la composición de especies.
}
\description{
función para graficar la composición de especies en un NMDS.
}
\examples{
\dontrun{
# plot sin factor de agrupamiento
data <- read_tsv("data_fitoplancton.tsv")
col_sitio <- "Sitio"
col_taxa <- "Sigla"
col_N <- "N"
dist <- "euc"
taxa_id <-  "fitoplancton"
height <- 6
width <- 6
fn_plot_nmds(data = data,col_sitio = col_sitio, col_taxa = col_taxa, col_N = col_N, dist = dist, taxa_id = taxa_id, height = height, width = width)
# plot con factor de agrupamiento: hace permanova y colorea elipses#
# fake data
data <- read_tsv("data_fitoplancton.tsv")
data <- data \%>\% mutate(grupo = if_else(Sitio \%in\% c("P-1", "P-2", "P3"), "area1", "area2"))
col_sitio <- "Sitio"
col_taxa <- "Sigla"
col_N <- "N"
col_factor <- "grupo"
dist <- "euc"
taxa_id <-  "fitoplancton"
height <- 6
width <- 6
title_factor <- "Area"
fn_plot_nmds(data = data,col_sitio = col_sitio, col_taxa = col_taxa,col_factor = col_factor, col_N = col_N, dist = dist, taxa_id = taxa_id, height = height, width = width,title_factor = title_factor)
# plot con factor de agrupamiento: hace permanova y colorea elipses, con orden en la leyenda según orden preferido y con réplica#
fake data
data <- read_tsv("data_fitoplancton.tsv")
data <- data \%>\% mutate(grupo = if_else(Sitio \%in\% c("P-1", "P-2", "P3"), "area1", "area2"))
col_sitio <- "Sitio"
col_taxa <- "Sigla"
col_N <- "N"
col_factor <- "grupo"
col_replica <- "Replica"
dist <- "euc"
taxa_id <-  "fitoplancton"
title_factor <- "Area"
ord_factor <- c("area2", "area1")
height <- 6
width <- 6
fn_plot_nmds(data = data,col_sitio = col_sitio, col_taxa = col_taxa,col_factor = col_factor, col_N = col_N,col_replica = col_replica,ord_factor = ord_factor, dist = dist, taxa_id = taxa_id, height = height, width = width,title_factor = title_factor)
}
}
