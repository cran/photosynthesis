## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  eval = TRUE
)

## ---- mwe---------------------------------------------------------------------

library(dplyr)
library(magrittr)
library(photosynthesis)

# Leaving the make_* functions empty will automatically default to defaults
# parameters.
bake_par   <- make_bakepar()                       # temperature response parameters
constants  <- make_constants(use_tealeaves = FALSE) # physical constants
leaf_par   <- make_leafpar(use_tealeaves = FALSE)   # leaf parameters
enviro_par <- make_enviropar(use_tealeaves = FALSE) # environmental parameters

photo(leaf_par, enviro_par, bake_par, constants, quiet = TRUE,
      use_tealeaves = FALSE)


## ---- replace-defaults--------------------------------------------------------

# Use the `replace` argument to replace defaults. This must be a named list, and
# each named element must have the proper units specified. See `?make_parameters`
# for all parameter names and proper units.

# Temperature response parameters can be updated (but we won't do that here)
bake_par <- make_bakepar()

# Physical constants probably do not need to be replaced in most cases,
# that's why we call them 'constants'!
constants  <- make_constants(use_tealeaves = FALSE)

# First, we'll change photosynthetic photon flux density to 1000 umol / (m^2 s)
enviro_par <- make_enviropar(
  replace = list(
    PPFD = set_units(1000, "umol/m^2/s")
    ), use_tealeaves = FALSE
  )

# Next, we'll change stomatal conductance to 3 umol / (m^2 s Pa)
leaf_par <- make_leafpar(
  replace = list(
    g_sc = set_units(3, "umol/m^2/s/Pa")
    ), use_tealeaves = FALSE
  )

photo <- photo(leaf_par, enviro_par, bake_par, constants, quiet = TRUE,
               use_tealeaves = FALSE)

photo %>%
  select(PPFD, C_chl, A) %>%
  knitr::kable()


## ---- environmental-gradients-------------------------------------------------

# As before, use the `replace` argument to replace defaults, but this time we
# enter multiple values

bake_par <- make_bakepar()
constants <- make_constants(use_tealeaves = FALSE)

# First, we'll change the PPFD to 1000 and 1500 umol / (m^2 s)
enviro_par <- make_enviropar(
  replace = list(
    PPFD = set_units(c(1000, 1500), "umol/m^2/s")
    ), use_tealeaves = FALSE
  )

# Next, we'll change stomatal conductance to to 2 and 4 umol / (m^2 s Pa)
leaf_par  <- make_leafpar(
  replace = list(
    g_sc = set_units(c(2, 4), "umol/m^2/s/Pa")
    ), use_tealeaves = FALSE
  )

# Now there should be 4 combinations (high and low g_sc crossed with high and low PPFD)
ph <- photosynthesis(leaf_par, enviro_par, bake_par, constants, 
                     use_tealeaves = FALSE, progress = FALSE, quiet = TRUE)

ph %>% 
  select(g_sc, PPFD, A) %>%
  knitr::kable()


## ---- parallel-example--------------------------------------------------------

# We'll use the `replace` argument to enter multiple atmospheric CO2 concentrations

bake_par <- make_bakepar()
constants  <- make_constants(use_tealeaves = FALSE)

enviro_par <- make_enviropar(
  replace = list(
    C_air = set_units(seq(1, 200, length.out = 20), "Pa")
    ), use_tealeaves = FALSE
  )

leaf_par  <- make_leafpar(use_tealeaves = FALSE)

ph <- photosynthesis(leaf_par, enviro_par, bake_par, constants, 
                     use_tealeaves = FALSE, progress = FALSE, 
                     quiet = TRUE, parallel = TRUE)

# Plot C_c versus A
library(ggplot2)

## Drop units for plotting
ph %<>% mutate_if(~ is(.x, "units"), drop_units)
ggplot(ph, aes(C_chl, A)) +
  geom_line(size = 2) +
  xlab(expression(paste(C[chl], " [Pa]"))) +
  ylab(expression(paste("A [", mu, "mol ", m^-2~s^-1, "]"))) +
  theme_bw() +
  NULL


## ---- use-tealeaves-example---------------------------------------------------

# You will need to set use_tealeaves = TRUE when making parameters because additional parameters are needed for tealeaves.

bake_par <- make_bakepar()
constants  <- make_constants(use_tealeaves = TRUE)

enviro_par <- make_enviropar(
  replace = list(
    T_air = set_units(seq(288.15, 313.15, 1), K)
    ), use_tealeaves = TRUE
  )

leaf_par <- make_leafpar(replace = list(
    g_sc = set_units(c(2, 4), umol/m^2/s/Pa)
    ), use_tealeaves = TRUE
  )

ph <- photosynthesis(leaf_par, enviro_par, bake_par, constants, 
                     use_tealeaves = TRUE, progress = FALSE, 
                   quiet = TRUE, parallel = TRUE)

# Plot temperature and photosynthesis
library(ggplot2)

## Drop units for plotting
ph %<>% 
  mutate_if(~ is(.x, "units"), drop_units) %>%
  mutate(`g[s]` = ifelse(g_sc == 2, "low", "high"))

ggplot(ph, aes(T_air, T_leaf, color = `g[s]`)) +
  geom_line(size = 2, lineend = "round") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  scale_color_discrete(name = expression(g[s])) +
  xlab(expression(paste(T[air], " [K]"))) +
  ylab(expression(paste(T[leaf], " [K]"))) +
  theme_bw() +
  NULL

ggplot(ph, aes(T_air, A, color = `g[s]`)) +
  geom_line(size = 2, lineend = "round") +
  scale_color_discrete(name = expression(g[s])) +
  xlab(expression(paste(T[leaf], " [K]"))) +
  ylab(expression(paste("A [", mu, "mol ", m^-2~s^-1, "]"))) +
  theme_bw() +
  NULL


