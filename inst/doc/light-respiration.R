## ---- message = FALSE---------------------------------------------------------
library(dplyr)
library(photosynthesis)

round_to_nearest = function(x, values) {
  sapply(x, function(y, values) {
    values[which.min(abs(y - values))]
  }, values = values)
}

# Read in your data
dat = system.file("extdata", "A_Ci_Q_data_1.csv", package = "photosynthesis") |> 
  read.csv() |>
  mutate(group = round_to_nearest(CO2_s, values = c(50, 100, 200, 300, 410, 600, 820, 1200, 1600))) |>
  rename(A_net = A, PPFD = Qin, phi_PSII = PhiPS2, C_i = Ci)

# Fit light respiration with Yin method
r_light = fit_r_light_yin(data = dat, PPFD_lower = 20, PPFD_upper = 250)

# Fit light respiration with Kok method
r_light = fit_r_light_kok(data = dat, PPFD_lower = 20, PPFD_upper = 150)

# Fit light respiration across groups with Yin method
r_lights = fit_many(
  data = dat,
  funct = fit_r_light_yin,
  group = "group",
  PPFD_lower = 20, PPFD_upper = 250,
  progress = FALSE
)

#Fit the Walker-Ort method for GammaStar and light respiration
walker_ort = fit_r_light_WalkerOrt(dat)

# View model output
summary(walker_ort[[1]])

# View graph
# walker_ort[[2]]

# View coefficients
walker_ort[[3]]


