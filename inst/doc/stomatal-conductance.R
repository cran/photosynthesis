## ---- message = FALSE---------------------------------------------------------

library(dplyr)
library(photosynthesis)

round_to_nearest = function(x, values) {
  sapply(x, function(y, values) {
    values[which.min(abs(y - values))]
  }, values = values)
}

# Read in data
dat = system.file("extdata", "A_Ci_Q_data_1.csv", package = "photosynthesis") |>
  read.csv() |>
  # Convert RH to a proportion
  mutate(
    RH = RHcham / 100,
    PPFD = round_to_nearest(Qin, c(25, 50, 75, 100, 125, 375, 750, 1500))
  ) |>
  rename(A_net = A, C_air = Ca, g_sw = gsw, VPD = VPDleaf)

# Fit the Ball-Berry stomatal conductance models
fit = fit_gs_model(
  data = filter(dat, PPFD == 750),
  model = c("BallBerry")
  )

# Look at BallBerry model summary:
summary(fit[["BallBerry"]][["Model"]])

# Look at BallBerry parameters
fit[["BallBerry"]][["Parameters"]]

# Look at BallBerry plot
# fit[["BallBerry"]][["Graph"]]

# Fit many g_sw models
fits = fit_many(dat, funct = fit_gs_model, group = "PPFD", progress = FALSE)

# Look at the Medlyn_partial outputs at 750 PAR
# Model summary
summary(fits[["750"]][["Medlyn_partial"]][["Model"]])

# Model parameters
fits[["750"]][["Medlyn_partial"]][["Parameters"]]

# Graph
# fits[["750"]][["Medlyn_partial"]][["Graph"]]

# Compile parameter outputs for BallBerry model
# Note that it's the first element for each PAR value
# First compile list of BallBerry fits
bbmods = compile_data(data = fits, output_type = "list", list_element = 1)

# Now compile the parameters (2nd element) into a dataframe
bbpars = compile_data(data = bbmods, output_type = "dataframe", list_element = 2)

#Compile graphs
graphs = compile_data(data = bbmods, output_type = "list", list_element = 3)

# Look at 3rd graph
# graphs[[3]]

