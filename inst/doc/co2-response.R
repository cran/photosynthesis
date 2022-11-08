## ---- message = FALSE---------------------------------------------------------

library(dplyr)
library(photosynthesis)

# Read in your data
dat = system.file("extdata", "A_Ci_Q_data_1.csv", package = "photosynthesis") |>
  read.csv() |>
  mutate(
    # Set grouping variable
    group = as.factor(round(Qin, digits = 0)),
    # Convert data temperature to K
    T_leaf = Tleaf + 273.15
  ) |>
  rename(A_net = A, PPFD = Qin, C_i = Ci)

# Fit ACi curve. 
# Note that we are filtering the data.frame to fit for a single light value
fit = fit_aci_response(filter(dat, group == 1500))

# View fitted parameters
fit[[1]]

# View graph
fit[[2]]

# View data with modeled parameters attached
# fit[[3]]

# Fit many curves
fits = fit_many(
  data = dat, 
  funct = fit_aci_response, 
  group = "group", 
  progress = FALSE
)

# Print the parameters
# First set of double parentheses selects an individual group value
# Second set selects an element of the sublist
fits[[3]][[1]]

# Print the graph
fits[[3]][[2]]

# Compile graphs into a list for plotting
fits_graphs = compile_data(fits, list_element = 2)

# Print graphs to jpeg
# print_graphs(data = fits_graphs, path = tempdir(), output_type = "jpeg")

# Compile parameters into data.frame for analysis
fits_pars = compile_data(fits, output_type = "dataframe", list_element = 1)


