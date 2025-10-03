# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Set working directory (only works in RStudio)
setwd(normalizePath("C:/Users/wzj545/University of Copenhagen/KU/Research/WP4/WP3 - Cross culture coffee DCE/cross-culture-DCE"))
apollo_setWorkDir()

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL",
  modelDescr      = "MNL model, in WTP space",
  indivID         = "id", 
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
### the code would be: database = read.csv("data.csv",header=TRUE)
database <- read.csv("C:/Users/wzj545/University of Copenhagen/KU/Research/WP4/WP3 - Cross culture coffee DCE/cross-culture-DCE/input_MNL.csv")
### for data dictionary, use ?apollo_modeChoiceData


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_1                 = 0,
              asc_2                 = 0,
              asc_3                 = 0,
              b_price               = 0,
              b_arabica             = 0,
              b_full_milk           = 0,
              b_pb_milk             = 0,
              b_sugar               = 0,
              b_fairtrade           = 0,
              b_organic             = 0
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_1")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["choice1"]]  = asc_1 + b_price  * alt1_price + b_arabica * alt1_arabica + b_full_milk * alt1_full_milk + b_pb_milk * alt1_pb_milk + b_sugar * alt1_with_sugar + b_fairtrade * alt1_fairtrade + b_organic * alt1_organic   
  V[["choice2"]]  = asc_2 + b_price  * alt2_price + b_arabica * alt2_arabica + b_full_milk * alt2_full_milk + b_pb_milk * alt2_pb_milk + b_sugar * alt2_with_sugar + b_fairtrade * alt2_fairtrade + b_organic * alt2_organic 
  V[["choice3"]]  = asc_3 + b_price  * alt3_price + b_arabica * alt3_arabica + b_full_milk * alt3_full_milk + b_pb_milk * alt3_pb_milk + b_sugar * alt3_with_sugar + b_fairtrade * alt3_fairtrade + b_organic * alt3_organic
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(choice1=1, choice2=2, choice3=3), 
    avail         = list(choice1=1, choice2=1, choice3=1), 
    choiceVar     = choice, 
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)
