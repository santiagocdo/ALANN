# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # ALAAN: main.R # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # Associative # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Learning# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Artificial# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Neural# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Network # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# usage of function structure in main.R:
#   1. f_loadLibraries()
#   2. f_prepData()
#   3. f_sanityCheck()
#   4. f_runSim()
#     4.1. f_mod1(), f_mod2(), f_mod3(), f_mod4(), and f_mod5()
#       4.1.1. f_derAct(), and f_sigAct()
#     4.2 f_printWeights2Layers(), or f_printWeightsNLayers()
#   5. f_plotSims()

# Script created by Santiago Castiello and Andy Delamater (20/01/2024)
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
  #mod0: Rescorla-Wagner (RW); Rescorla & Wagner (1972)
  #mod1: Back-Propagation (BP); Delamater (2012) - Learning & Behaviour
  #mod2: BP dynamic alpha;
  #mod3: BP; Xie & Seung (2003) - Neural Computation
  #mod4: Contrast Hebbian Learning (CHL); Xie & Seung (2003) - Neural Computation
  #mod5: CHL with random feedback; Detorakis, et al. (2019) - Neural Networks

mod_type <- "mod4"

# run sanity check function (warnings provided)
f_sanityCheck()



# # # # Parameters # # # #
## ## ## Mod 0 ## ## ##
if (mod_type == "mod0") { # Rescorla & Wagner (1972)
  par$alpha <- 0.2
  par$beta <- 0.9
  par$betaNR <- 0.45
  label_output <- paste0("alpha",par$alpha,"_betaR",par$beta,
                         "_betaNR",par$betaNR)
}

## ## ## Mod 1 and 3 ## ## ##
if (mod_type == "mod1" | mod_type == "mod3") { # alpha 0.2, beta 0.9
  # alpha (learning rate) and beta (momentum; (Delamater, 2012 - L&B; Xie & Seung, 2003 - Neural Computation)
  par$alpha <- 0.2
  par$beta <- 0.9
  par$adaptBias <- 0
  label_output <- paste0("alpha",par$alpha,"_beta",par$beta)
}

## ## ## Mod 2 ## ## ##
if (mod_type == "mod2") { # beta 0.9, rho = 0.05, mu = 0.01
  par$beta <- 0.9
  # rho and mu free parameters (smooth learning rate change; Kaye & Pearce, 1984)
  par$rho <- 0.05 # rho (p) is for weights between input to hidden 
  par$mu <- 0.01 # mu (m) is for weights between hidden to output
  label_output <- paste0("beta",par$beta,"_rho",par$rho,"_mu",par$mu)
}

## ## ## Mod 4 and 5 ## ## ##
if (mod_type == "mod4" | mod_type == "mod5" | mod_type == "mod6") {
  par$tf <- 15 #        dynamic equation time = 30
  par$dt <- 0.4 #       time step = 0.08
  par$adaptBias <- 0 #  
  par$gamma <- 0.2 #    feedback gain factor = 0.05
  par$eta <- 0.4 #      learning rate = 0.1
  label_output <- paste0("tf",par$tf,"_dt",par$dt,"_gamma",par$gamma,
                         "_eta",par$eta,"_L",nrow(par$nHidden))
}



# weights per subj and layers (figures and csv; TRUE = yes, FALSE = no)
print_weights <- F
# how many simulated subjects?
nSim <- 12

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

# if you want to print individual plots (N < 12) then change doIndPart to T
plots <- f_plotSims(exp, test, chl_error, par, nSim,
                    doIndPart = F, mod_type, label_output)

# display plots from the list() called "plot"
# plot means (pMean) average output activation
plots$pMean
# plot individuals (pInd) output activation
plots$pInd
# plot tests (pTest) average output activation in test trials
plots$pTest
test %>% group_by(trialType) %>% summarize(actOT=mean(actOT))
# plot learning rates from input to hidden (pLR.IH)
plots$pLR.IH
# plot learning rates from hidden to output (pLR.HO)
plots$pLR.HO
# plot ch errors (pChl.error)
plots$pChl.error



# save a csv from "exp" data frame, containing output activations in long format
# write.csv(exp,paste0("output/exp_",mod_type,"_n",nSim,".csv"),row.names = F)

# # save plots
# ggsave(paste0("figures/pMean_",mod_type,"_",label_output,"_out",
#               length(unique(exp$out)),".png"), dpi = 300, limitsize = TRUE,
#        plot = plots$pMean, 
#        units = "px", # "cm", "in"
#        width = 1200,
#        height = 800)
# # save plots
# ggsave(paste0("figures/pInd_",mod_type,"_",label_output,".png"), dpi = 300, limitsize = TRUE,
#        plot = plots$pInd, 
#        units = "px",
#        width = 1200, 
#        height = 800)
# # save plots
# ggsave(paste0("figures/pLR.IH_",mod_type,"_",label_output,".png"), dpi = 300, limitsize = TRUE,
#        plot = plots$pLR.IH, 
#        units = "px",
#        width = 1200, 
#        height = 800)
# # save plots
# ggsave(paste0("figures/pLR.HO",mod_type,"_",label_output,".png"), dpi = 300, limitsize = TRUE,
#        plot = plots$pLR.HO, 
#        units = "px",
#        width = 1200, 
#        height = 800)
# # save plots
# ggsave(paste0("figures/pChl.error_",mod_type,"_",label_output,".png"), dpi = 300, limitsize = TRUE,
#        plot = plots$pChl.error, 
#        units = "px",
#        width = 1200, 
#        height = 800)
