## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  
#  #Read in your data
#  #Note that this data is coming from data supplied by the package
#  #hence the complicated argument in read.csv()
#  #This dataset is a CO2 by light response curve for a single sunflower
#  data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv",
#                               package = "photosynthesis"))
#  
#  #Define a grouping factor based on light intensity to split the ACi
#  #curves
#  data$Q_2 <- as.factor((round(data$Qin, digits = 0)))
#  
#  #Convert data temperature to K
#  data$T_leaf <- data$Tleaf + 273.15
#  
#  #Run a sensitivity analysis on GammaStar and mesophyll conductance
#  #at 25 Celsius for one individual curve
#  pars <- analyze_sensitivity(data = data[data$Q_2 == 1500, ],
#                                    funct = fit_aci_response,
#                                     varnames = list(A_net = "A",
#                                        T_leaf = "T_leaf",
#                                        C_i = "Ci",
#                                        PPFD = "Qin"),
#                                     useg_mct = TRUE,
#                                     test1 = "gamma_star25",
#                                     element_out = 1,
#                                     test2 = "g_mc25",
#                                     fitTPU = TRUE,
#                                     Ea_gamma_star = 0,
#                                     Ea_g_mc = 0,
#                                     values1 = seq(from = 20,
#                                                 to = 60,
#                                                 by = 4),
#                                     values2 = seq(from = 0.2,
#                                                 to = 2,
#                                                 by = 0.1))
#  
#  #Compute measures of sensitivity
#  par2 <- compute_sensitivity(data = pars,
#                              varnames = list(Par = "V_cmax",
#                                              test1 = "gamma_star25",
#                                              test2 = "g_mc25"),
#                              test1_ref = 40,
#                              test2_ref = 1)
#  #Plot sensitivity
#  ggplot(par2, aes(y = CE_gamma_star25, x = CE_g_mc25, colour = V_cmax))+
#    labs(x = expression(g_mc[25]~"Control Coefficient"),
#         y = expression(Gamma[25]~"Control Coefficient")) +
#    geom_point() +
#    theme_bw()
#  
#  ggplot(par2, aes(y = CE_gamma_star25, x = V_cmax, colour = gamma_star25))+
#    geom_point() +
#    theme_bw()
#  #Note that in this case a missing point appears due to an infinity
#  #Can also plot sensitivity measures in 2 dimensions
#  ggplot(par2, aes(x = gamma_star25, y = g_mc25, z = CE_gamma_star25))+
#  geom_tile(aes(fill = CE_gamma_star25)) +
#  labs(x = expression(Gamma*"*"[25]~"("*mu*mol~mol^{-1}*")"),
#       y = expression(g[m][25]~"("*mu*mol~m^{-2}~s^{-1}~Pa^{-1}*")"))+
#  scale_fill_distiller(palette = "Greys") +
#  geom_contour(colour = "Black", size = 1) +
#  theme_bw()
#  
#  plot(PE_gamma_star25 ~ gamma_star25, par2)

