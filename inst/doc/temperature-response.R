## ---- message = FALSE---------------------------------------------------------

library(dplyr)
library(photosynthesis)
library(tidyr)

round_to_nearest = function(x, values) {
  sapply(x, function(y, values) {
    values[which.min(abs(y - values))]
  }, values = values)
}

# Read in data
dat = system.file("extdata", "A_Ci_T_data.csv", package = "photosynthesis") |>
  read.csv() |>
  mutate(
    group = as.factor(round_to_nearest(Tleaf, seq(17.5, 40, by = 2.5))),
    # Convert data temperature to K
    T_leaf = Tleaf + 273.15
  ) %>%
  # Calculate mean temperature for group so temperature
  full_join(
    . |>
      group_by(ID, group) |>
      summarize(Curve_Tleaf = round(mean(Tleaf), 1), .groups = "drop"),
    by = c("ID", "group")
  ) |>
  # Create ID column to curve fit by ID and temperature
  mutate(ID2 = paste(ID, Curve_Tleaf, sep = "_")) |>
  rename(A_net = A, C_i = Ci, PPFD = Qin)
   
# Fit many CO2 response curves
fits = fit_many(
  data = dat,
  group = "ID2",
  funct = fit_aci_response,
  alphag = 0,
  progress = FALSE
)
    
# Extract ACi parameters
pars = compile_data(fits, output_type = "dataframe", list_element = 1)
    
# Extract ACi graphs
graphs = compile_data(fits, output_type = "list", list_element = 2)
    
# Parse the ID variable
pars = separate(pars, col = "ID", into = c("ID", "Curve_Tleaf"), sep = "_") |>
  mutate(
    # Make sure curve leaf temperature is numeric and convert to K
    T_leaf = as.numeric(Curve_Tleaf) + 273.15
  )
    
fit = fit_t_response(
  data = filter(pars, ID == "S2"),
  varnames = list(Par = "V_cmax", T_leaf = "T_leaf"),
  setvar = "Hd"
)
    
# Graphs
# fit[["Arrhenius"]][["Graph"]]
# fit[["Heskel"]][["Graph"]]
# fit[["Kruse"]][["Graph"]]
# fit[["Medlyn"]][["Graph"]]
# fit[["MMRT"]][["Graph"]]
# fit[["Quadratic"]][["Graph"]]
# fit[["Topt"]][["Graph"]]

