# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

apollo_control = list(
  modelDescr      = "2-class LC model with no covariates on cross-culture survey data",
  modelName       = "2-class LC model no cov",
  indivID         = "id",
  nCores          = 3, 
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
### the code would be: database = read.csv("data.csv",header=TRUE)
database <- read.csv("C:/Users/wzj545/University of Copenhagen/KU/Research/WP4/WP3 - Cross culture coffee DCE/cross-culture-DCE/input_MNL.csv")

### for data dictionary, use ?apollo_swissRouteChoiceData

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_1                 = 0,
              asc_2                 = 0.1,
              asc_3                 = 0.2,
              b_price_1               = 0.1,
              b_arabica_1             = 0.1,
              b_full_milk_1           = 0.3,
              b_pb_milk_1             = 0.1,
              b_sugar_1               = 0.15,
              b_fairtrade_1           = 0.1,
              b_organic_1             = 0.1,
              b_price_2               = 0.05,
              b_arabica_2             = 0.2,
              b_full_milk_2           = 0.1,
              b_pb_milk_2             = 0.05,
              b_sugar_2               = 0,
              b_fairtrade_2           = 0.1,
              b_organic_2             = 0,
              delta_1                 = 0.05,
              delta_2                 = 0
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_1")

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_price"]] = list(b_price_1, b_price_2)
  lcpars[["beta_arabica"]] = list(b_arabica_1, b_arabica_2)
  lcpars[["beta_full_milk"]] = list(b_full_milk_1, b_full_milk_2)
  lcpars[["beta_pb_milk"]] = list(b_pb_milk_1, b_pb_milk_2)
  lcpars[["beta_sugar"]] = list(b_sugar_1, b_sugar_2)
  lcpars[["beta_fairtrade"]] = list(b_fairtrade_1, b_fairtrade_2)
  lcpars[["beta_organic"]] = list(b_organic_1, b_organic_2)
  
  ### Utilities of class allocation model
  V=list()
  V[["class_a"]] = delta_1
  V[["class_b"]] = delta_2
  
  ### Settings for class allocation models
  classAlloc_settings = list(
    classes      = c(class_a=1, class_b=2), 
    utilities    = V  
  )
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  
  return(lcpars)
}

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
  
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2, alt3=3),
    avail        = list(alt1=1, alt2=1, alt3=1),
    choiceVar    = choice
  )
  
  ### Loop over classes
  for(s in 1:2){
    
    ### Compute class-specific utilities
    V=list()
    V[["alt1"]]  = asc_1 + beta_price[[s]] * alt1_price + beta_arabica[[s]] * alt1_arabica + beta_full_milk[[s]] * alt1_full_milk + beta_pb_milk[[s]] * alt1_pb_milk + beta_sugar[[s]] * alt1_with_sugar + beta_fairtrade[[s]] * alt1_fairtrade + beta_organic[[s]] * alt1_organic
    V[["alt2"]]  = asc_2 + beta_price[[s]] * alt2_price + beta_arabica[[s]] * alt2_arabica + beta_full_milk[[s]] * alt2_full_milk + beta_pb_milk[[s]] * alt2_pb_milk + beta_sugar[[s]] * alt2_with_sugar + beta_fairtrade[[s]] * alt2_fairtrade + beta_organic[[s]] * alt2_organic
    V[["alt3"]]  = asc_3 + beta_price[[s]] * alt3_price + beta_arabica[[s]] * alt3_arabica + beta_full_milk[[s]] * alt3_full_milk + beta_pb_milk[[s]] * alt3_pb_milk + beta_sugar[[s]] * alt3_with_sugar + beta_fairtrade[[s]] * alt3_fairtrade + beta_organic[[s]] * alt3_organic
    
    mnl_settings$utilities     = V
    mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
  }
  
  ### Compute latent class model probabilities
  lc_settings  = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, apollo_inputs)

### Show output in screen
apollo_modelOutput(model)

### Save output to file(s)
apollo_saveOutput(model)

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

conditionals <- apollo_conditionals(model, apollo_probabilities, apollo_inputs)
order <- conditionals[order(conditionals$X1), ]
conditionals$Class <- apply(conditionals[, c("X1","X2","X3")], 1, which.max)
conditionals$Class <- paste0("Class_", conditionals$Class)
write.csv(conditionals, "classified_individuals.csv", row.names = FALSE)


### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
apollo_sink()

# ----------------------------------------------------------------- #
#---- OUT OF SAMPLE TESTING                                     ----
# ----------------------------------------------------------------- #

apollo_outOfSample(apollo_beta, apollo_fixed,
                   apollo_probabilities, apollo_inputs)


# ----------------------------------------------------------------- #
#---- BOOTSTRAP ESTIMATION                                       ----
# ----------------------------------------------------------------- #

apollo_bootstrap(apollo_beta, apollo_fixed,apollo_probabilities, 
                 apollo_inputs, bootstrap_settings = list(nRep=15))

# ----------------------------------------------------------------- #
#---- POSTERIOR ANALYSIS                                     ----
# ----------------------------------------------------------------- #

### Compute unconditionals
unconditionals = apollo_unconditionals(model,apollo_probabilities,apollo_inputs)

vtt_class_a = unconditionals[["beta_tt"]][[1]]/unconditionals[["beta_tc"]][[1]]
vtt_class_b = unconditionals[["beta_tt"]][[2]]/unconditionals[["beta_tc"]][[2]]
vtt_unconditional = unconditionals[["pi_values"]][[1]]*vtt_class_a + 
  unconditionals[["pi_values"]][[2]]*vtt_class_b

### Compute conditionals
conditionals = apollo_conditionals(model,apollo_probabilities, apollo_inputs)

summary(conditionals)
summary(as.data.frame(unconditionals[["pi_values"]]))

vtt_conditional=conditionals[,2]*vtt_class_a+conditionals[,3]*vtt_class_b

summary(vtt_unconditional)
summary(vtt_conditional)

### Take first value of covariates for each person
commute_n          = apollo_firstRow(database$commute, apollo_inputs)
car_availability_n = apollo_firstRow(database$car_availability, apollo_inputs)

### Compute posterior values for covariates
post_commute=colSums(commute_n*conditionals)/colSums(conditionals)
post_car_availability=colSums(car_availability_n*conditionals)/colSums(conditionals)

post_commute[2:3]
post_car_availability[2:3]

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

apollo_sink()
