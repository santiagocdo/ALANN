# Script created by Santiago Castiello and Andy Delamater (26/06/2023)
# this code contain the model used in Castiello et al (2022) - NLM which is 
# based on Delamater (2012) - L&B.

# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))

# call created functions
source("functions.R")
# fun load libraries function
f_loadLibraries()

# select and read training and parameters csv file
train <- read.csv(file.choose())

# prepare data
dataReady <- f_prepData(train)
# parameter
par <- dataReady$par
# training phases
trPh <- dataReady$trPh

# # # inputs for the user # # #
# select model type: 
  #mod1: Back-Propagation (BP); Delamater (2012) - Learning & Behaviour
  #mod2: BP dynamic alpha;
  #mod3: BP; Xie & Seung (2003) - Neural Computation
  #mod4: Contrast Hebbian Learning (CHL); Xie & Seung (2003) - Neural Computation
  #mod5: CHL with random feedback; Detorakis, et al. (2019) - Neural Networks

mod_type <- "mod3" # If model 2 then must have a gamma value

# run sanity check function (warnings provided)
f_sanityCheck()



# # # # Parameters # # # #
## ## ## Mod 1 ## ## ##
if (mod_type == "mod1") {
  # alpha (learning rate) and beta (momentum; Delamater, 2012)
  par$alpha <- 0.3
  par$beta <- 0.9
}

## ## ## Mod 2 ## ## ##
if (mod_type == "mod2") {
  # gamma and eta free parameters (smooth learning rate change; Kaye & Pearce, 1984)
  par$rho <- 0.05 # rho (p) is for weights between input to hidden 
  par$mu <- 0.01 # mu (m) is for weights between hidden to output
}

## ## ## Mod 3 ## ## ##
if (mod_type == "mod3") {
  # alpha (learning rate) and beta (momentum; Delamater, 2012)
  par$alpha <- 0.3
  par$beta <- 0.9
  par$adaptBias <- 0
}

## ## ## Mod 4 and 5 ## ## ##
if (mod_type == "mod4" | mod_type == "mod5") {
  par$tf <- 10 # dynamic equation time (30)
  par$dt <- 0.6 # time step (0.08)
  par$adaptBias <- 0 
  par$gamma <- 0.3 # feedback gain factor (0.05)
  par$eta <- 0.3 # learning rate (0.1)
}



# weights per subj and layers (figures and csv; 1 = yes, 0 = no)
print_weights <- 1
# how many simulated subjects?
nSim <- 2

# for loop for subjects
message(paste("Starting ",nSim," simulations..."))
for (s in 1:nSim) {
  # run all phases for one subject 
  temp <- f_runSim(par, trPh, subj = s, print_weights, mod_type)
  # progress bar
  setTxtProgressBar(txtProgressBar(min = 0, max = nSim, style = 3,
                                   width = 50, char = "="), s); Sys.sleep(0.01)
  # combine subjects
  if (s == 1) { # subject 1
    exp <- data.frame(nSubj=s, temp$exp)
    # only when test is not null
    if (!is.null(temp$test)){test <- data.frame(nSubj=s, temp$test)}
    # only when chl_error is not null
    if (!is.null(temp$chl_error)){chl_error <- data.frame(nSubj=s, temp$chl_error)}
  } else { # subject >1
    exp <- rbind(exp,data.frame(nSubj=s, temp$exp))
    # only when test is not null
    if (!is.null(temp$test)){test <- rbind(test,data.frame(nSubj=s, temp$test))}
    # only when chl_error is not null
    if (!is.null(temp$chl_error)){chl_error <- rbind(chl_error,data.frame(nSubj=s, temp$chl_error))}
  }
} # end s loop

# visualize simulations conditional to if test was required or not
if (!exists("test")) {test <- NULL}
if (!exists("chl_error")) {chl_error <- NULL}

# if you do not want individual plots (N < 12) then change doIndPart to 0
plots <- f_plotSims(exp,test,chl_error,par,nSim,doIndPart=1,mod_type)

# display plots
# average activation
plots$pMean
# individual participants
plots$pInd
# test trials
plots$pTest
# LR input to hidden
plots$pLR.IH
# LR hidden to output
plots$pLR.HO
# chl errors
plots$pChl.error

# print "exp" data frame, containing activation in long format
write.csv(exp,paste0("output/exp_",mod_type,"_n",nSim,".csv"),row.names = F)

