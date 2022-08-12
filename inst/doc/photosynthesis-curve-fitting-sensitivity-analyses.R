## -----------------------------------------------------------------------------
#To install, run the following without comments
#library(devtools)
#install_github("jstinzi/photosynthesis")

#Load package
library(photosynthesis)

#To cite, use:
citation("photosynthesis")

#Load tidyr - needed for vignette manipulations
library(tidyr)


## -----------------------------------------------------------------------------
#library(devtools)
#install_github("erikerhardt/RLicor")
#library(RLicor)

#The following will detect and read Li-Cor 6400 and 6800 files
#?read_Licor

#To cite, use:
#citation("RLicor")

## -----------------------------------------------------------------------------
#Read in your data
#Note that this data is coming from data supplied by the package
#hence the complicated argument in read.csv()
#This dataset is a CO2 by light response curve for a single sunflower
#Note that to read in your own data, you will need to delete the
#system.file() function, otherwise you will get an error
data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv",
                             package = "photosynthesis"))

#Fit many AQ curves
#Set your grouping variable
#Here we are grouping by CO2_s and individual
data$C_s <-(round(data$CO2_s, digits = 0))

#For this example we need to round sequentially due to CO2_s setpoints
data$C_s <- as.factor(round(data$C_s, digits = -1))

#To fit one AQ curve
fit <- fit_aq_response(data[data$C_s == 600,],
                       varnames = list(A_net = "A",
                                         PPFD = "Qin",
                                       Q_cut = 250))

#Print model summary
summary(fit[[1]])

#Print fitted parameters
fit[[2]]

#Print graph
fit[[3]]

#Fit many curves
fits <- fit_many(data = data,
                 varnames = list(A_net = "A",
                                         PPFD = "Qin",
                                         group = "C_s"),
                 funct = fit_aq_response,
                 group = "C_s")

#Look at model summary for a given fit
#First set of double parentheses selects an individual group value
#Second set selects an element of the sublist
summary(fits[[3]][[1]])

#Print the parameters
fits[[2]][[2]]

#Print the graph
fits[[3]][[3]]

#Compile graphs into a list for plotting
fits_graphs <- compile_data(fits,
                            list_element = 3)

#Print graphs to jpeg
# print_graphs(data = fits_graphs,
#             path = tempdir(),
#             output_type = "jpeg")

#Compile parameters into dataframe for analysis
fits_pars <- compile_data(fits,
                          output_type = "dataframe",
                          list_element = 2)


## -----------------------------------------------------------------------------
#Read in your data
#Note that this data is coming from data supplied by the package
#hence the complicated argument in read.csv()
#This dataset is a CO2 by light response curve for a single sunflower
data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv", 
                             package = "photosynthesis"))

#Define a grouping factor based on light intensity to split the ACi
#curves
data$Q_2 <- as.factor((round(data$Qin, digits = 0)))

#Convert data temperature to K
data$T_leaf <- data$Tleaf + 273.15

#Fit ACi curve. Note that we are subsetting the dataframe
#here to fit for a single value of Q_2
fit <- fit_aci_response(data[data$Q_2 == 1500, ],
                        varnames = list(A_net = "A",
                                      T_leaf = "T_leaf",
                                      C_i = "Ci",
                                      PPFD = "Qin"))

#View fitted parameters
fit[[1]]

#View graph
fit[[2]]

#View data with modeled parameters attached
#fit[[3]]

#Fit many curves
fits <- fit_many(data = data,
                 varnames = list(A_net = "A",
                                      T_leaf = "T_leaf",
                                      C_i = "Ci",
                                      PPFD = "Qin"),
                 funct = fit_aci_response,
                 group = "Q_2")

#Print the parameters
#First set of double parentheses selects an individual group value
#Second set selects an element of the sublist
fits[[3]][[1]]

#Print the graph
fits[[3]][[2]]

#Compile graphs into a list for plotting
fits_graphs <- compile_data(fits,
                            list_element = 2)

#Print graphs to jpeg
# print_graphs(data = fits_graphs,
#             path = tempdir(),
#             output_type = "jpeg")

#Compile parameters into dataframe for analysis
fits_pars <- compile_data(fits,
                          output_type = "dataframe",
                          list_element = 1)


## -----------------------------------------------------------------------------
#Read in data
data <- read.csv(system.file("extdata", "A_Ci_T_data.csv", 
                             package = "photosynthesis"),
                 stringsAsFactors = FALSE)

#Round temperatures to group them appropriately
#Use sequential rounding
data$T2 <- round(data$Tleaf, 1)
data$T2 <- round(data$Tleaf, 0)

#Look at unique values to detect rounding issues
unique(data$T2)

#Some still did not round correctly,
#manually correct
for(i in 1:nrow(data)){
  if(data$T2[i] == 18){
    data$T2[i] <- 17
  }
  if(data$T2[i] == 23){
    data$T2[i] <- 22
  }
  if(data$T2[i] == 28){
    data$T2[i] <- 27
  }
  if(data$T2[i] == 33){
    data$T2[i] <- 32
  }
  if(data$T2[i] == 38){
    data$T2[i] <- 37
  }
}

#Make sure it is a character string for grouping
data$T2 <- as.character(data$T2)

#Create grouping variable by ID and measurement temperature
data <- unite(data, col = "ID2", c("ID", "T2"),
                 sep = "_")

#Split by temperature group
data <- split(data, data$ID2)

#Obtain mean temperature for group so temperature
#response fitting is acceptable later, round to
#2 decimal places
for(i in 1:length(data)){
  data[[i]]$Curve_Tleaf <- round(mean(data[[i]]$Tleaf), 2)
}

#Convert from list back to dataframe
data <- do.call("rbind", data)

#Parse grouping variable by ID and measurement temperature
data <- separate(data, col = "ID2", into = c("ID", "T2"),
                 sep = "_")

#Make sure number of values matches number of measurement
#temperatures. May vary slightly if plants had slightly
#different leaf temperatures during the measurements
unique(data$Curve_Tleaf)

#Create ID column to curve fit by ID and temperature
data <- unite(data, col = "ID2", c("ID", "Curve_Tleaf"),
                 sep = "_")

#Convert data temperature to K
data$T_leaf <- data$Tleaf + 273.15

#Fit many CO2 response curves
fits2 <- fit_many(data = data,
                 group = "ID2",
                 varnames = list(A_net = "A",
                                 C_i = "Ci",
                                 T_leaf = "T_leaf",
                                 PPFD = "Qin",
                                 g_mc = "g_mc"),
                 funct = fit_aci_response,
                 alphag = 0)

#Extract ACi parameters
pars <- compile_data(fits2, output_type = "dataframe",
                     list_element = 1)

#Extract ACi graphs
graphs <- compile_data(fits2, output_type = "list",
                     list_element = 2)

#Parse the ID variable
pars <- separate(pars, col = "ID", into = c("ID", "Curve_Tleaf"), sep = "_")

#Make sure curve leaf temperature is numeric
pars$Curve_Tleaf <- as.numeric(pars$Curve_Tleaf)
pars$T_leaf <- pars$Curve_Tleaf + 273.15
out <- fit_t_response(data = pars[pars$ID == "S2",],
                      varnames = list(Par = "V_cmax",
                                      T_leaf = "T_leaf"),
                      setvar = "Hd")

out[["Arrhenius"]][["Graph"]]
out[["Heskel"]][["Graph"]]
out[["Kruse"]][["Graph"]]
out[["Medlyn"]][["Graph"]]
out[["MMRT"]][["Graph"]]
out[["Quadratic"]][["Graph"]]
out[["Topt"]][["Graph"]]

## -----------------------------------------------------------------------------
#Read in your data
#Note that this data is coming from data supplied by the package
#hence the complicated argument in read.csv()
#This dataset is a CO2 by light response curve for a single sunflower
data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv", 
                             package = "photosynthesis"))

#Convert RH to a proportion
data$RH <- data$RHcham / 100

#Fit stomatal conductance models
#Can specify a single model, or all as below
fits <- fit_gs_model(data = data,
                     varnames = list(A_net = "A",
                                     C_air = "Ca",
                                     g_sw = "gsw",
                                     RH = "RH",
                                     VPD = "VPDleaf"),
                     model = c("BallBerry",
                               "Leuning",
                               "Medlyn_partial",
                               "Medlyn_full"),
                         D0 = 3)

#Look at BallBerry model summary:
summary(fits[["BallBerry"]][["Model"]])

#Look at BallBerry parameters
fits[["BallBerry"]][["Parameters"]]

#Look at BallBerry plot
fits[["BallBerry"]][["Graph"]]


#Fit many g_sw models
#Set your grouping variable
#Here we are grouping by Qin and individual
data$Q_2 <- as.factor((round(data$Qin, digits = 0)))

fits <- fit_many(data,
                 varnames = list(A_net = "A",
                                 C_air = "Ca",
                                 g_sw = "gsw",
                                 RH = "RH",
                                 VPD = "VPDleaf"),
                 funct = fit_gs_model,
                 group = "Q_2")

#Look at the Medlyn_partial outputs at 750 PAR
#Model summary
summary(fits[["750"]][["Medlyn_partial"]][["Model"]])

#Model parameters
fits[["750"]][["Medlyn_partial"]][["Parameters"]]

#Graph
fits[["750"]][["Medlyn_partial"]][["Graph"]]

#Compile parameter outputs for BallBerry model
#Note that it's the first element for each PAR value
#First compile list of BallBerry fits
bbmods <- compile_data(data = fits,
                     output_type = "list",
                     list_element = 1)
#Now compile the parameters (2nd element) into a dataframe
bbpars <- compile_data(data = bbmods,
                     output_type = "dataframe",
                     list_element = 2)

#Convert group variable back to numeric
bbpars$ID <- as.numeric(bbpars$ID)

#Take quick look at light response of intercept parameters
plot(g0 ~ ID, bbpars)

#Compile graphs
graphs <- compile_data(data = bbmods,
                       output_type = "list",
                       list_element = 3)

#Look at 3rd graph
graphs[[3]]

## -----------------------------------------------------------------------------
#Read in your data
#Note that this data is coming from data supplied by the package
#hence the complicated argument in read.csv()
#This dataset is a CO2 by light response curve for a single sunflower
data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv", 
                             package = "photosynthesis"))

#Fit light respiration with Yin method
r_light <- fit_r_light_yin(data = data,
                           varnames = list(A_net = "A",
                                           PPFD = "Qin",
                                           phi_PSII = "PhiPS2"),
                           PPFD_lower = 20,
                           PPFD_upper = 250)

#Fit light respiration with Kok method
r_light <- fit_r_light_kok(data = data,
                           varnames = list(A_net = "A",
                                           PPFD = "Qin"),
                           PPFD_lower = 20,
                           PPFD_upper = 150)

#Set your grouping variable
#Here we are grouping by CO2_s and individual
data$C_s <-(round(data$CO2_s, digits = 0))

#For this example we need to round sequentially due to CO2_s setpoints
data$C_s <- as.factor(round(data$C_s, digits = -1))

#Fit light respiration across groups with Yin method
r_lights <- fit_many(data = data,
                     funct = fit_r_light_yin,
                     group = "C_s",
                     varnames = list(A_net = "A",
                                           PPFD = "Qin",
                                           phi_PSII = "PhiPS2"),
                           PPFD_lower = 20,
                           PPFD_upper = 250)

#Compile the outputs - note this is slightly more complex because the
#output of the fit_many above is a list of atomic vectors, not dataframes.
group <- names(r_lights)
r_lights <- do.call("c", r_lights)
r_light_yin <- data.frame(x = group, y = r_lights, stringsAsFactors = FALSE)
r_light_yin$x <- as.numeric(r_light_yin$x)
colnames(r_light_yin) <- c("C_s", "r_light")

#Fit the Walker-Ort method for GammaStar and light respiration
walker_ort <- fit_r_light_WalkerOrt(data,
                      varnames = list(A_net = "A",
                                      C_i = "Ci",
                                      PPFD = "Qin"))

#View model output
summary(walker_ort[[1]])

#View graph
walker_ort[[2]]

#View coeffients
walker_ort[[3]]


## -----------------------------------------------------------------------------
#Read in your data
#Note that this data is coming from data supplied by the package
#hence the complicated argument in read.csv()
#This dataset is a CO2 by light response curve for a single sunflower
data <- read.csv(system.file("extdata", "A_Ci_Q_data_1.csv", 
                             package = "photosynthesis"))

#Note: there will be issues here if the alpha value used
#for calculating ETR is off, if GammaStar is incorrect,
#if Rd is incorrect.
data <- fit_g_mc_variableJ(data,
                         varnames = list(A_net = "A",
                                         J_etr = "ETR",
                                         C_i = "Ci",
                                         PPFD = "Qin",
                                         phi_PSII = "PhiPS2"),
                                         gamma_star = 46,
                                         R_d = 0.153,
                         usealpha_Q = TRUE,
                         alpha_Q = 0.84,
                         beta_Q = 0.5,
                         P = 84)

#Note that many g_mc values from this method can be unreliable
ggplot(data, aes(x = CO2_s, y = g_mc, colour = reliable)) +
  labs(x = expression(CO[2]~"("*mu*mol~mol^{-1}*")"),
       y = expression(g[m]~"(mol"~m^{-2}~s^{-1}~Pa^{-1}*")")) +
  geom_point(size = 2) +
  theme_bw() +
  theme(legend.position = 'bottom')

#Plot QAQC graph according to Harley et al. 1992
ggplot(data, aes(x = CO2_s, y = dCcdA, colour = reliable)) +
  labs(x = expression(CO[2]~"("*mu*mol~mol^{-1}*")"),
       y = expression(delta*C[c]*"/"*delta*A)) +
  geom_hline(yintercept = 10) +
  geom_point(size = 2) +
  theme_bw() +
  theme(legend.position = 'bottom')

ggplot(data, aes(x = dCcdA, y = g_mc, colour = reliable)) +
  labs(x = expression(delta*C[c]*"/"*delta*A),
       y = expression(g[m]~"(mol"~m^{-2}~s^{-1}~Pa^{-1}*")")) +
  geom_point(size = 2) +
  theme_bw() +
  theme(legend.position = 'bottom')

## -----------------------------------------------------------------------------
#Read in data
data <- read.csv(system.file("extdata", "PV_curve.csv", 
                             package = "photosynthesis"))

#Fit one PV curve
fit <- fit_PV_curve(data[data$ID == "L2", ],
                    varnames = list(psi = "psi", 
                                    mass = "mass", 
                                    leaf_mass = "leaf_mass", 
                                    bag_mass = "bag_mass", 
                                    leaf_area = "leaf_area"))

#See fitted parameters
fit[[1]]

#Plot water mass graph
fit[[2]]

#Plot PV Curve
fit[[3]]

#Fit all PV curves in a file
fits <- fit_many(data,
                 group = "ID",
                 funct = fit_PV_curve,
                 varnames = list(psi = "psi", 
                                    mass = "mass", 
                                    leaf_mass = "leaf_mass", 
                                    bag_mass = "bag_mass", 
                                    leaf_area = "leaf_area"))

#See parameters
fits[[1]][[1]]

#See water mass - water potential graph
fits[[1]][[2]]

#See PV curve
fits[[1]][[3]]

#Compile parameter outputs
pars <- compile_data(data = fits,
                     output_type = "dataframe",
                     list_element = 1)

#Compile the water mass - water potential graphs
graphs1 <- compile_data(data = fits,
                     output_type = "list",
                     list_element = 2)

#Compile the PV graphs
graphs2 <- compile_data(data = fits,
                     output_type = "list",
                     list_element = 3)

## -----------------------------------------------------------------------------
#Read in data
data <- read.csv(system.file("extdata", "hydraulic_vulnerability.csv", 
                             package = "photosynthesis"))

#Fit hydraulic vulnerability curve
fit <- fit_hydra_vuln_curve(data[data$Tree == 5 & data$Plot == "Irrigation",],
                            varnames = list(psi = "P",
                                            PLC = "PLC"),
                            start_weibull = list(a = 2, b = 1),
                            title = "Irrigation 5")

#Return Sigmoidal model summary
summary(fit[[1]]) 

#Return Weibull model summary
summary(fit[[4]]) #expecting a = 4.99, b = 3.22

#Return model parameters with 95% confidence intervals
fit[[2]] 

#Return hydraulic parameters
fit[[3]] 

#Return graph
#fit[[5]] 

data <- unite(data, col = "ID", c("Plot", "Tree"), sep = "_")
#fit many function check to make sure it works for weibull
#Fit many curves
fits <- fit_many(data = data,
                  varnames = list(psi = "P",
                                            PLC = "PLC"),
                 group = "ID",
                 start_weibull = list(a = 4, b = 2),
                  #group = "Tree",
                  funct = fit_hydra_vuln_curve)

#To select individuals from the many fits
#Return model summary
summary(fits[[1]][[1]]) #Returns model summary

#Return sigmoidal model output
fits[[1]][[2]] 

#Return hydraulic parameters
fits[[1]][[3]] 

#Return graph
fits[[1]][[5]] 

#Compile parameter outputs
pars <- compile_data(data = fits,
                     output_type = "dataframe",
                     list_element = 3)

#Compile graphs
graphs <- compile_data(data = fits,
                     output_type = "list",
                     list_element = 5)


## -----------------------------------------------------------------------------
#check_dependencies()

