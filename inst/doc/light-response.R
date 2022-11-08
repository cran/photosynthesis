## ---- message = FALSE---------------------------------------------------------

library(dplyr)
library(photosynthesis)
# Read in your data
dat = system.file("extdata", "A_Ci_Q_data_1.csv", package = "photosynthesis") |>
  read.csv() |>
  # Set grouping variable
  mutate(group = round(CO2_s, digits = 0)) |>
  # For this example, round sequentially due to CO2_s setpoints
  mutate(group = as.factor(round(group, digits = -1))) |>
  rename(A_net = A, PPFD = Qin)

# To fit one AQ curve
fit = fit_aq_response(filter(dat, group == 600))

# Print model summary
summary(fit[[1]])

# Print fitted parameters
fit[[2]]

# Print graph
fit[[3]]

# Fit many curves
fits = fit_many(
  data = dat,
  funct = fit_aq_response,
  group = "group",
  progress = FALSE
)

# Look at model summary for a given fit
# First set of double parentheses selects an individual group value
# Second set selects an element of the sublist
summary(fits[[3]][[1]])

# Print the parameters
fits[[2]][[2]]

# Print the graph
fits[[3]][[3]]

#Compile graphs into a list for plotting
fits_graphs = compile_data(fits, list_element = 3)

# Print graphs to jpeg
# print_graphs(data = fits_graphs, path = tempdir(), output_type = "jpeg")

#Compile parameters into data.frame for analysis
fits_pars = compile_data(fits, output_type = "dataframe", list_element = 2)

