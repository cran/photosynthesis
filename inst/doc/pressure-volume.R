## ---- message = FALSE---------------------------------------------------------

library(dplyr)
library(photosynthesis)

# Read in data
dat = system.file("extdata", "PV_curve.csv", package = "photosynthesis") |>
  read.csv()

# Fit one PV curve
fit = fit_PV_curve(filter(dat, ID == "L2"))

# See fitted parameters
fit[[1]]

# Plot water mass graph
# fit[[2]]

# Plot PV Curve
# fit[[3]]

#Fit all PV curves in a file
fits = fit_many(
  dat,
  group = "ID",
  funct = fit_PV_curve,
  progress = FALSE
)

# See parameters
fits[[1]][[1]]

# See water mass - water potential graph
# fits[[1]][[2]]

# See PV curve
# fits[[1]][[3]]

# Compile parameter outputs
pars = compile_data(data = fits, output_type = "dataframe", list_element = 1)

# Compile the water mass - water potential graphs
graphs1 = compile_data(data = fits, output_type = "list", list_element = 2)

# Compile the PV graphs
graphs2 = compile_data(data = fits, output_type = "list", list_element = 3)


