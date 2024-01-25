# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))

# call created functions
source("functions.R")
# fun load libraries function
f_loadLibraries()

# new function for this script only
runSimulations <- function (par, trPh, print_weights, mod_type) {
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
  # output
  return(list(exp=exp,test=test))
}



# model
mod_type <- "mod0"
# weights per subj and layers (figures and csv; TRUE = yes, FALSE = no)
print_weights <- F
# how many simulated subjects?
nSim <- 80

# beta for non reinforcement vector
betaNR <- seq(0,1,by=0.1)



# select and read training and parameters csv file
trainTD1 <- read.csv("relative_validity_TD1.csv")
# prepare training data
dataReady <- f_prepData(trainTD1)
# parameter
par_TD1 <- dataReady$par
# training phases
trPh_TD1 <- dataReady$trPh



# select and read training and parameters csv file
trainTD2 <- read.csv("relative_validity_TD2.csv")
# prepare training data
dataReady <- f_prepData(trainTD2)
# parameter
par_TD2 <- dataReady$par
# training phases
trPh_TD2 <- dataReady$trPh



# select and read training and parameters csv file
trainPD <- read.csv("relative_validity_PD.csv")
# prepare training data
dataReady <- f_prepData(trainPD)
# parameter
par_PD <- dataReady$par
# training phases
trPh_PD <- dataReady$trPh



par_TD1$nBlock <- par_TD2$nBlock <- par_PD$nBlock <- 40
par_TD1$alpha <- par_TD2$alpha <- par_PD$alpha <- 0.2
par_TD1$beta <- par_TD2$beta <- par_PD$beta <- 1



for (i in 1:length(betaNR)) {
  par_TD1$betaNR <- betaNR[i]
  temp <- runSimulations(par_TD1, trPh_TD1, print_weights, mod_type)
  if (i == 1) {
    exp_TD1 <- data.frame(temp$exp,betaNR=betaNR[i])
    test_TD1 <- data.frame(temp$test,betaNR=betaNR[i])
  } else {
    exp_TD1 <- rbind(exp_TD1, data.frame(temp$exp,betaNR=betaNR[i]))
    test_TD1 <- rbind(test_TD1, data.frame(temp$test,betaNR=betaNR[i]))
  }
  
  par_TD2$betaNR <- betaNR[i]
  temp <- runSimulations(par_TD2, trPh_TD2, print_weights, mod_type)
  if (i == 1) {
    exp_TD2 <- data.frame(temp$exp,betaNR=betaNR[i])
    test_TD2 <- data.frame(temp$test,betaNR=betaNR[i])
  } else {
    exp_TD2 <- rbind(exp_TD2, data.frame(temp$exp,betaNR=betaNR[i]))
    test_TD2 <- rbind(test_TD2, data.frame(temp$test,betaNR=betaNR[i]))
  }
  
  par_PD$betaNR <- betaNR[i]
  temp <- runSimulations(par_PD, trPh_PD, print_weights, mod_type)
  if (i == 1) {
    exp_PD <- data.frame(temp$exp,betaNR=betaNR[i])
    test_PD <- data.frame(temp$test,betaNR=betaNR[i])
  } else {
    exp_PD <- rbind(exp_PD, data.frame(temp$exp,betaNR=betaNR[i]))
    test_PD <- rbind(test_PD, data.frame(temp$test,betaNR=betaNR[i]))
  }
}



exp_TD1$group <- "TD1 (AX-  ; BX-  ; 2CX+)"
exp_TD2$group <- "TD2 (AX+  ; BX+  ; 2CX-)"
exp_PD$group <-  "PD  (AX+/-; BX+/-; 2CX+/-)"

exp <- rbind(exp_TD1,exp_TD2,exp_PD)
exp$betaDif <- 1-exp$betaNR

test_TD1$group <- "TD1 (AX-  ; BX-  ; 2CX+)"
test_TD2$group <- "TD2 (AX+  ; BX+  ; 2CX-)"
test_PD$group <-  "PD  (AX+/-; BX+/-; 2CX+/-)"

test <- rbind(test_TD1,test_TD2,test_PD)
test$betaDif <- 1-test$betaNR



# ggplot(exp, aes(x=nBlock,y=actO,col=group)) + 
#   labs(x = "beta_R (0.9) - beta_NR (X)",
#        y = "activation X at test") +
#   stat_summary() +
#   stat_summary(geom = "line", fun = "mean") +
#   coord_cartesian(ylim = c(0,1)) +
#   facet_wrap(.~betaDif) +
#   theme_classic()

ggplot(test, aes(x=betaDif,y=actOT,col=group)) + 
  labs(x = "beta_R (1) - beta_NR ([0, 0.1, ..., 1])",
       y = "activation X at test") +
  stat_summary() +
  stat_summary(geom = "line", fun = "mean") +
  scale_x_continuous(breaks = seq(0,1,by=0.1)) +
  scale_y_continuous(breaks = c(0,0.5,1)) +
  coord_cartesian(ylim = c(0,1)) +
  theme_classic()
