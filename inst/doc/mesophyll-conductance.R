## ---- message = FALSE---------------------------------------------------------

library(dplyr)
library(photosynthesis)

# Read in your data
dat = system.file("extdata", "A_Ci_Q_data_1.csv", package = "photosynthesis") |> 
  read.csv() |>
  rename(A_net = A, J_etr = ETR, C_i = Ci, PPFD = Qin, phi_PSII = PhiPS2)

# Note: there will be issues here if the alpha value used for calculating ETR is 
# off, if GammaStar is incorrect, if Rd is incorrect.
fit = fit_g_mc_variableJ(dat, gamma_star = 46, R_d = 0.153,  usealpha_Q = TRUE,
                         alpha_Q = 0.84, beta_Q = 0.5, P = 84)

# Note that many g_mc values from this method can be unreliable
# ggplot(fit, aes(x = CO2_s, y = g_mc, colour = reliable)) +
#   labs(x = expression(CO[2]~"("*mu*mol~mol^{-1}*")"),
#        y = expression(g[m]~"(mol"~m^{-2}~s^{-1}~Pa^{-1}*")")) +
#   geom_point(size = 2) +
#   theme_bw() +
#   theme(legend.position = 'bottom')

# Plot QAQC graph according to Harley et al. 1992
# ggplot(fit, aes(x = CO2_s, y = dCcdA, colour = reliable)) +
#   labs(x = expression(CO[2]~"("*mu*mol~mol^{-1}*")"),
#        y = expression(delta*C[c]*"/"*delta*A)) +
#   geom_hline(yintercept = 10) +
#   geom_point(size = 2) +
#   theme_bw() +
#   theme(legend.position = 'bottom')

# ggplot(fit, aes(x = dCcdA, y = g_mc, colour = reliable)) +
#   labs(x = expression(delta*C[c]*"/"*delta*A),
#        y = expression(g[m]~"(mol"~m^{-2}~s^{-1}~Pa^{-1}*")")) +
#   geom_point(size = 2) +
#   theme_bw() +
#   theme(legend.position = 'bottom')

