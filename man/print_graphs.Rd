% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_graphs.R
\name{print_graphs}
\alias{print_graphs}
\title{Printing graphs to system}
\usage{
print_graphs(
  data,
  path,
  output_type = "jpeg",
  height = 5,
  width = 5,
  res = 600,
  units = "in",
  pdf_filename,
  ...
)
}
\arguments{
\item{data}{List of graphs}

\item{path}{File path for printing our graphs. Use "./" to set to current
working directory}

\item{output_type}{Type of output file, jpeg or pdf}

\item{height}{Height of jpegs}

\item{width}{Width of jpegs}

\item{res}{Resolution of jpegs}

\item{units}{Units of height and width}

\item{pdf_filename}{Filename for pdf option}

\item{...}{Further arguments for jpeg() and pdf()}
}
\value{
print_graphs creates graph files in current working directory
from a list of graphs
}
\description{
Printing graphs to system
}
\examples{
\donttest{
# Read in your data
# Note that this data is coming from data supplied by the package
# hence the complicated argument in read.csv()
# This dataset is a CO2 by light response curve for a single sunflower
data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv",
  package = "photosynthesis"
))

# Fit many AQ curves
# Set your grouping variable
# Here we are grouping by CO2_s and individual
data$C_s <- (round(data$CO2_s, digits = 0))

# For this example we need to round sequentially due to CO2_s setpoints
data$C_s <- as.factor(round(data$C_s, digits = -1))

# To fit one AQ curve
fit <- fit_aq_response(data[data$C_s == 600, ],
  varnames = list(
    A_net = "A",
    PPFD = "Qin"
  )
)

# Print model summary
summary(fit[[1]])

# Print fitted parameters
fit[[2]]

# Print graph
fit[[3]]

# Fit many curves
fits <- fit_many(
  data = data,
  varnames = list(
    A_net = "A",
    PPFD = "Qin",
    group = "C_s"
  ),
  funct = fit_aq_response,
  group = "C_s"
)

# Look at model summary for a given fit
# First set of double parentheses selects an individual group value
# Second set selects an element of the sublist
summary(fits[[3]][[1]])

# Print the parameters
fits[[3]][[2]]

# Print the graph
fits[[3]][[3]]

# Compile graphs into a list for plotting
fits_graphs <- compile_data(fits,
  list_element = 3
)

# Print graphs to pdf
# Uncomment to run
# print_graphs(data = fits_graphs,
#            output_type = "pdf",
#            path = tempdir(),
#            pdf_filename = "mygraphs.pdf")
}
}
