# Script created by Santiago Castiello and Andy Delamater (28/03/2022)
# this code contain the model used in Castiello et al (2022) - NLM which is 
# based on Delamater (2012) - L&B.

# # # # # INSTRUCTION: # # # # #
## -Line 36: add gamma (for dynamic learning rate), otherwise if 
# par$gamma <- NULL then normal backprop.
## -Line 54: if you want to print each subject weights, use "print_weights=s", 
# otherwise "=NULL".
## -Line 54: dynModType == "v1" (one LR for IH and one for HO) or "v2" (PH LR for each 
# trial type, global reciprocol HO does not work for compounds) or "v3" (PH LR IH, global Mackintosh for HO).

# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))

# libraries
if (!require(rstudioapi)) {install.packages("rstudioapi")}; library(rstudioapi)
if (!require(reshape2)) {install.packages("reshape2")}; library(reshape2)
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)

# set wording directory
setwd(dirname(getActiveDocumentContext()$path))

# call created functions
source("functions_v8old.R")

# read training and parameters csv file
train <- read.csv("Category Reversal.csv")

# prepare data
dataReady <- f_prepData(train)
par <- dataReady$par
trPh <- dataReady$trPh
# # # SEE INSTRUCTION # # #
par$a <- 0.3
par$b <- 0.9
par$gamma <- 0.05
par$eta <- 0.01

# how many outputs?
nOut <- ncol(as.matrix(trPh[[1]]$OUTPUT))

## ## ## ## ## ## Loop nSims ## ## ## ## ## ##
## Number of Simulated Subjects
nSim <- 16

# for loop for subjects
message(paste("Starting ",nSim," simulations..."))
for (s in 1:nSim) {
  setTxtProgressBar(txtProgressBar(min = 0, max = nSim, style = 3,
                                   width = 50, char = "="), s); Sys.sleep(0.01)
  # run all phases for one subject 
  # # # SEE INSTRUCTION # # #
  dynModType <- "v3"
  temp <- f_runSim(par, trPh, print_weights=s, dynModType)
  # combine subjects
  if (s == 1) {
    exp <- data.frame(nSubj=s, temp$exp)
    if (!is.null(temp$test)){
      test <- data.frame(nSubj=s, temp$test)
    }
  } else {
    exp <- rbind(exp,data.frame(nSubj=s, temp$exp))
    if (!is.null(temp$test)){
      test <- rbind(test,data.frame(nSubj=s, temp$test))
    }
  }
} # end s loop

# visualize simulations conditional to if test was required or not
if (exists("test")) { # plot pMean and pTest (pInd if nSim <= 24)
  plots <- f_plotSims(exp,test,nSim,nOut,doIndPart=1,doTest=1,dynModType) 
  # to see the actual means from test 
  # meanTest <- test %>% group_by(ph,trialType,out) %>%
  #   summarize(mActOT = mean(actOT)); meanTest
} else { # only plot pMean (pInd if nSim <= 24)
  plots <- f_plotSims(exp,test=NULL,nSim,nOut,doIndPart=1,doTest=0,dynModType) 
}

# display plots
plots$pMean
ggsave("TotalReversal_v2.pdf", plot = plots$pMean, 
       width = 12, height = 12, units = "cm")
# plots$pInd
plots$pTest
plots$pLR
ggplot(exp, aes(x=nBlock,y=lrHO,col=trialType)) + 
  stat_summary() + theme_classic()


write.csv(exp,paste0("exp_nSim",nSim,"_a",par$a,"_b",par$b,".csv"))




######################### Multiple Hidden Layers ###############################
############################################################################## #
############################################################################## #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
