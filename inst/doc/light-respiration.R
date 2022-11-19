## ---- message = FALSE---------------------------------------------------------

library(broom)
library(dplyr)
library(photosynthesis)

acq_data = system.file("extdata", "A_Ci_Q_data_1.csv", package = "photosynthesis") |> 
  read.csv()

fit = fit_photosynthesis(
  .data = acq_data,
  .photo_fun = "r_light",
  .model = "walker_ort_2015",
  .vars = list(.A = A, .Q = Qin, .C = Ci),
  C_upper = 300,
  # Irradiance levels used in experiment
  Q_levels =  c(1500, 750, 375, 125, 100, 75, 50, 25),
)

# The 'fit' object inherits class 'lm' and many methods can be used

## Model summary:
summary(fit)

## Estimated parameters:
coef(fit)

## 95% confidence intervals:
## n.b. these confidence intervals are not correct because the regression is fit 
## sequentially. It ignores the underlying data and uncertainty in estimates of 
## slopes and intercepts with each A-C curve. Use '.method = "brms"' to properly 
## calculate uncertainty. 
confint(fit)

## Tidy summary table using 'broom::tidy()'
tidy(fit, conf.int = TRUE, conf.level = 0.95)

## Calculate residual sum-of-squares
sum(resid(fit)^2)


## ---- message = FALSE---------------------------------------------------------

library(broom)
library(dplyr)
library(photosynthesis)

acq_data = system.file("extdata", "A_Ci_Q_data_1.csv", package = "photosynthesis") |> 
  read.csv()

fit = fit_photosynthesis(
  .data = acq_data,
  .photo_fun = "r_light",
  .model = "yin_etal_2011",
  .vars = list(.A = A, .phiPSII = PhiPS2, .Q = Qin),
  Q_lower = 20,
  Q_upper = 250
)

# The 'fit' object inherits class 'lm' and many methods can be used

## Model summary:
summary(fit)

## Estimated parameters:
coef(fit)

## 95% confidence intervals:
confint(fit)

## Tidy summary table using 'broom::tidy()'
tidy(fit, conf.int = TRUE, conf.level = 0.95)

## Calculate residual sum-of-squares
sum(resid(fit)^2)


## ---- message = FALSE---------------------------------------------------------

library(broom)
library(dplyr)
library(photosynthesis)

acq_data = system.file("extdata", "A_Ci_Q_data_1.csv", package = "photosynthesis") |> 
  read.csv()

fit = fit_photosynthesis(
  .data = acq_data,
  .photo_fun = "r_light",
  .model = "kok_1956",
  .vars = list(.A = A, .Q = Qin),
  Q_lower = 20,
  Q_upper = 150
)

# The 'fit' object inherits class 'lm' and many methods can be used

## Model summary:
summary(fit)

## Estimated parameters:
coef(fit)

## 95% confidence intervals:
confint(fit)

## Tidy summary table using 'broom::tidy()'
tidy(fit, conf.int = TRUE, conf.level = 0.95)

## Calculate residual sum-of-squares
sum(resid(fit)^2)


## ---- message = FALSE, warning=FALSE, results='hide', eval = FALSE------------
#  
#  library(dplyr)
#  library(photosynthesis)
#  library(tidyr)
#  
#  acq_data = system.file("extdata", "A_Ci_Q_data_1.csv", package = "photosynthesis") |>
#    read.csv()
#  
#  fit = fit_photosynthesis(
#    .data = acq_data,
#    .photo_fun = "r_light",
#    .model = "walker_ort_2015",
#    .method = "brms",
#    .vars = list(.A = A, .Q = Qin, .C = Ci),
#    C_upper = 300,
#    # Irradiance levels used in experiment
#    Q_levels =  c(1500, 750, 375, 125, 100, 75, 50, 25),
#    brm_options = list(chains = 1, seed = 20221118)
#  )
#  
#  # The 'fit' object inherits class 'brmsfit' and many methods can be used
#  
#  # The following code will extract parameter estimates and 95% CIs from the
#  # posterior distribution
#  ests = fit |>
#    as.data.frame() |>
#    dplyr::select(
#      r_xy = `cor_.Q_level__Intercept__.C`,
#      sd_x = `sd_.Q_level__Intercept`,
#      sd_y = `sd_.Q_level__.C`,
#      mu_x = b_Intercept,
#      mu_y = b_.C
#    ) |>
#    dplyr::mutate(
#      gamma_star = -r_xy * sd_x / sd_y,
#      Rd = mu_x + gamma_star * mu_y
#    ) |>
#    dplyr::summarise(dplyr::across(gamma_star:Rd, list(
#      estimate = median,
#      lower = ~quantile(., probs = 0.25),
#      upper = ~quantile(., probs = 0.975)
#    ))) |>
#    pivot_longer(
#      everything(),
#      names_to = c("parameter", ".value"),
#      names_pattern = "(gamma_star|Rd)_(estimate|lower|upper)"
#    )
#  

## ---- message = FALSE, eval = FALSE-------------------------------------------
#  
#  library(photosynthesis)
#  
#  acq_data = system.file("extdata", "A_Ci_Q_data_1.csv", package = "photosynthesis") |>
#    read.csv()
#  
#  fit = fit_photosynthesis(
#    .data = acq_data,
#    .photo_fun = "r_light",
#    .model = "yin_etal_2011",
#    .method = "brms",
#    .vars = list(.A = A, .phiPSII = PhiPS2, .Q = Qin),
#    Q_lower = 20,
#    Q_upper = 250,
#    brm_options = list(chains = 1)
#  )
#  
#  # The 'fit' object inherits class 'brmsfit' and many methods can be used
#  summary(fit)
#  

## ---- message = FALSE, eval = FALSE-------------------------------------------
#  
#  library(photosynthesis)
#  
#  acq_data = system.file("extdata", "A_Ci_Q_data_1.csv", package = "photosynthesis") |>
#    read.csv()
#  
#  fit = fit_photosynthesis(
#    .data = acq_data,
#    .photo_fun = "r_light",
#    .model = "kok_1956",
#    .method = "brms",
#    .vars = list(.A = A, .Q = Qin),
#    Q_lower = 20,
#    Q_upper = 150,
#    brm_options = list(chains = 1)
#  )
#  
#  # The 'fit' object inherits class 'brmsfit' and many methods can be used
#  summary(fit)
#  

## ---- message = FALSE, warning=FALSE------------------------------------------
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
  mutate(group = photosynthesis:::round_to_nearest(CO2_s, values = c(50, 100, 200, 300, 410, 600, 820, 1200, 1600))) |>
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


