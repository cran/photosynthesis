## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(photosynthesis)

## ---- simple leaf, message = FALSE, eval=TRUE---------------------------------
library(photosynthesis)
bake_par   = make_bakepar()                       # temperature response parameters
constants  = make_constants(use_tealeaves = FALSE) # physical constants
# leaf parameters
leaf_par   = make_leafpar(
  replace = list(
    # Set cuticular conductance to 0
    g_uc = set_units(0, mol / m^2 / s),
    # All conductance through lower stomata and mesophyll
    k_mc = set_units(0, 1),
    k_sc = set_units(0, 1)
  ),
  use_tealeaves = FALSE
  )   
enviro_par = make_enviropar(use_tealeaves = FALSE) # environmental parameters

photo(leaf_par, enviro_par, bake_par, constants, use_tealeaves = FALSE) |>
  dplyr::select(g_sc, A) |>
  knitr::kable(caption = "Stomatal conductance to CO2 (g_sc) and net photosynthetic carbon assimilation (A) from C3 photosynthesis model.")

## ---- amphi leaf, message = FALSE, eval=TRUE----------------------------------
library(photosynthesis)
bake_par   = make_bakepar()                       # temperature response parameters
constants  = make_constants(use_tealeaves = FALSE) # physical constants
# leaf parameters
leaf_par   = make_leafpar(
  replace = list(
    # Set cuticular conductance to 0
    g_uc = set_units(0, mol / m^2 / s),
    # Half of conductance through each surface
    k_mc = set_units(0, 1),
    # airspace conductance: define effective distance through airspace
    # See Nobel (2020) pg. 431
    delta_ias_lower = set_units(100, um),
    delta_ias_upper = set_units(100, um),
    # liquid conductance: sum of cell wall, plasma membrane, and cytosol resistance
    # We are implicitly ignoring chloroplast resistance
    # See Nobel (2020) pg. 448-452
    A_mes_A = set_units(20, 1),
    g_liqc25 = set_units(0.02, mol / m^2 / s),
    k_sc = set_units(1, 1)
  ),
  use_tealeaves = FALSE
  )   
enviro_par = make_enviropar(use_tealeaves = FALSE) # environmental parameters

photo(leaf_par, enviro_par, bake_par, constants, use_tealeaves = FALSE) |>
  dplyr::select(g_sc, g_iasc_lower, g_iasc_upper, g_liqc, A) |>
  knitr::kable(caption = "Stomatal conductance to CO2 (g_sc), internal airspace resistance through lower and upper surfaces (g_iasc_x), liquid-phase conductance (g_liqc), and net photosynthetic carbon assimilation (A) from C3 photosynthesis model.")

