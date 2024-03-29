## ---- message = FALSE---------------------------------------------------------

library(broom)
library(dplyr)
library(photosynthesis)

# Read in your data
dat = system.file("extdata", "A_Ci_Q_data_1.csv", package = "photosynthesis") |>
  read.csv() |>
  # Set grouping variable
  mutate(group = round(CO2_s, digits = 0)) |>
  # For this example, round sequentially due to CO2_s set points
  mutate(group = as.factor(round(group, digits = -1)))

# Fit one light-response curve
fit = fit_photosynthesis(
  .data = filter(dat, group == 600),
  .photo_fun = "aq_response",
  .vars = list(.A = A, .Q = Qabs),
)

# The 'fit' object inherits class 'nls' and many methods can be used

## Model summary:
summary(fit)

## Estimated parameters:
coef(fit)

## 95% confidence intervals:
confint(fit)

## Tidy summary table using 'broom::tidy()'
tidy(fit, conf.int = TRUE, conf.level = 0.95)

## Calculate light compensation point
coef(fit) |>
  t() |>
  as.data.frame() |>
  mutate(LCP = ((Rd) * (Rd * theta_J - k_sat) / (phi_J * (Rd - k_sat)))) |>

## Calculate residual sum-of-squares
sum(resid(fit) ^ 2)


## -----------------------------------------------------------------------------

library(ggplot2)

b = coef(fit)

df_predict = data.frame(Qabs = seq(0, 0.84 * 1500, length.out = 100)) |>
  mutate(
    A = marshall_biscoe_1980(
      Q_abs = Qabs,
      k_sat = b["k_sat"],
      b["phi_J"],
      b["theta_J"]
    ) - b["Rd"]
  )

ggplot(mapping = aes(Qabs, A)) +
  geom_line(data = df_predict) +
  geom_point(data = filter(dat, group == 600)) +
  labs(
    x = expression("Irradiance (" * mu * mol ~ m^{-2} ~ s^{-1} * ")"),
    y = expression(A[net] ~ "(" * mu * mol ~ m^{-2} ~ s^{-1} * ")")
  ) +
  theme_bw()


## -----------------------------------------------------------------------------
library(purrr)

fits = dat |>
  split(~ group) |>
  map(fit_photosynthesis, .photo_fun = "aq_response", .vars = list(.A = A, .Q = Qabs))

## Estimated parameters:
fits |>
  map(coef) |>
  map(t) |>
  map(as.data.frame) |>
  imap_dfr(~ mutate(.x, CO2_s = .y))


## ---- eval = FALSE------------------------------------------------------------
#  
#  fit = fit_photosynthesis(
#    .data = filter(dat, group == 600),
#    .photo_fun = "aq_response",
#    .vars = list(.A = A, .Q = Qabs),
#    .method = "brms",
#    brm_options = list(chains = 1)
#  )
#  
#  # The 'fit' object inherits class 'brmsfit' and many methods can be used
#  summary(fit)
#  

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


