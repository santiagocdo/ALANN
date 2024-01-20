# Script created by Santiago Castiello and Andy Delamater (28/03/2022)
# this code contain the model used in Castiello et al (2022) - NLM which is 
# based on Delamater (2012) - L&B.

# functions:
  # f_backProp: backpropagation function for one phase, received initial weights
  # f_derAct: activation's derivative
  # f_backProp_dynLR: backprop with dynamic LR, received initial weights as f_backProp
  # f_prepData: prepare read csv to lists to feed backprops functions
  # f_runSim: use the result of f_prepData and run one subject with multiple phases
  # f_plotSims: plot simulations mean, individuals, and tests
  # f_printWeights:
  # f_hiddenRepresen:
  # f_plotCorrels: 

# back propagation (Neural Net Model); run one phase
f_backProp <- function (par, training, nKO_MM = 0, 
                        preW.IH = NULL, preW.HO = NULL) {
  ### extract parameters ###
  a <- par$a
  b <- par$b
  nHid <- par$nHidden$nHV + par$nHidden$nHMM + par$nHidden$nHA
  nBlock <- par$nBlock
  actFun <- par$actFun
  INPUT <- as.matrix(training$INPUT)
  OUTPUT <- as.matrix(training$OUTPUT)
  TEST <- as.matrix(training$TEST)
  trialType <- training$trialType
  trialTypeTest <- training$trialTypeTest
  
  ### create relevant matrices and scalars ###
  nOut <- ncol(OUTPUT) # number of outcomes
  nStim <- ncol(INPUT) # number of stimuli
  nTrialType <- nrow(INPUT) # number of trial types
  nTrialTypeTest <- length(trialTypeTest)
  nTest <- nrow(TEST) # number of test trials
  nTrial <- nTrialType*nBlock # number of total trials
  
  ### creation of matrices and add initial values ###
  actOT <- matrix(NA,nrow = nTrialTypeTest, ncol = nOut) # act. output for test
  actH <- matrix(NA,nrow = nTrial, ncol = nHid) # act. hidden layer
  actO <- matrix(NA,nrow = nTrial, ncol = nOut) # act. output layer
  deltaH <- matrix(NA,nrow = nTrial, ncol = nHid) # delta hidden
  deltaO <- matrix(NA,nrow = nTrial, ncol = nOut) # delta output
  dwIH <- matrix(0, nrow = nStim, ncol = nHid) # change input-hidden weights
  dwHO <- matrix(0, nrow = nHid, ncol = nOut) # change hidden-output weights
  # initial values (random or previous phases)
  if (is.null(preW.IH)) {
    wIH <- matrix(runif(nStim*nHid)-0.5,nrow = nStim, ncol = nHid) # rows input units (stim), cols hidden units
  } else {wIH <- preW.IH}
  if (is.null(preW.HO)) {
    wHO <- matrix(runif(nHid*nOut)-0.5, nrow = nHid) # rows hidden units, cols output units or options
  } else {wHO <- preW.HO}
  
  ### remove connections for hidden auditory and visual paths ###
  conIH <- matrix(1, nrow = nStim, ncol = nHid) # C matrix in Castiello et al (2022) - NLM
  # block visual input with auditory hidden
  if (par$nHidden$nHA > 0) {
    conIH[(par$nInput$ctx+1):(par$nInput$ctx + par$nInput$vis),
          (nHid-par$nHidden$nHA + 1):nHid] <- 0}
  # block auditory input with visual hidden
  if (par$nHidden$nHV > 0) {
    conIH[(par$nInput$ctx + par$nInput$vis + 1):nStim,
          1:par$nHidden$nHV] <- 0}
  wIH <- wIH * conIH
  
  #### backpropagation algorithm ###
  # see also: https://web.stanford.edu/group/pdplab/pdphandbook/handbookch6.html
  for (nB in 1:nBlock) {
    # randomized trial types within a block
    randT <- sample(1:nTrialType)
    INPUT <- as.matrix(INPUT[randT,])
    OUTPUT <- as.matrix(OUTPUT[randT,])
    trialType <- trialType[randT]
    for (t in 1:nTrialType) {
      ### ### ### activation ### ### ### 
      # forward activation # chain rule: act_fun(act_fun(trialBlock %*% wIH) %*% wHO)
      trial <- t+((nB-1)*nTrialType)
      actH[trial,] <- actFun(INPUT[t,] %*% wIH) # INPUT[t,] assumed to be a 1 x nStim matrix
      actO[trial,] <- actFun(actH[trial,] %*% wHO) # actH[t,] assumed to be a 1 x nOut matrix
      
      ### ### ### learning (weight changes) ### ######
      
      ### delta (error) for output
      deltaO[trial,] <- (OUTPUT[t,] - actO[trial,]) * f_derAct(actO[trial,])
      
      ### delta (error) for hidden
      deltaH[trial,] <- (deltaO[trial,] %*% t(wHO)) * f_derAct(actH[trial,])
      
      # weight change Hidden-Output
      dwHO[,] <- t(a * (as.matrix(deltaO[trial,]) %*% actH[trial,])) + b*dwHO[,]
      # update weights Hidden-Output
      wHO[,] <- wHO[,] + dwHO[,]
      # weight change Input-Hidden
      dwIH[,] <- t(a * (as.matrix(deltaH[trial,]) %*% INPUT[t,])) + b*dwIH[,]
      # update weights Input-Hidden
      wIH[,] <- wIH[,] + (dwIH[,] * conIH)
    } # end trials per block cycle # t
    if (nB == 1) {
      trialTypeLong <- as.character(trialType)
    } else {
      trialTypeLong <- c(trialTypeLong,as.character(trialType))
    }
  } # end all blocks cycle # nB
  
  ### test input patterns ###
  if (nTest != 0){  # KO mm units when necessary
    if (nKO_MM == 0) {
      actOT <- actFun(actFun(TEST %*% wIH) %*% wHO)
    } else {
      if (nKO_MM > par$nHidden$nHMM) {
        warning("nKO_MM must be smaller than nHMM")
      } else {
        KO_units <- sample((par$nHidden$nHV + 1):(nHid-par$nHidden$nHA))[1:nKO_MM]
        wIH_KO <- wIH; wIH_KO[,KO_units] <- 0
        wHO_KO <- wHO; wHO_KO[KO_units,] <- 0
        actOT <- actFun(actFun(TEST %*% wIH_KO) %*% wHO_KO)
      } # if error message
    } # if KO mm units
  }
  # outputs' activations data frame
  db <- data.frame(nTrial=1:nTrial,nBlock=rep(1:nBlock,each=nTrialType),
                   trialType=trialTypeLong,actO=actO)
  # create output list object
  colnames(actOT) <- paste0(rep("actOT.",nOut),1:nOut)
  output <- list(db=db,actO=actO, wIH=wIH, wHO=wHO, 
                 actOT=data.frame(trialType=trialTypeTest,actOT))
  return(output)
}

# derivative of logistic activation function
f_derAct <- function(act) {act * (1 - act)}

# softmax function applied for each column of a matrix
f_softmax <- function(x,invT) {
  x <- as.matrix(x)
  output <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
  for (i in 1:ncol(x)) {
    output[,i] <- exp(invT*x[,i])/sum(exp(invT*x[,i]))
  }
  return(output)
}

# back propagation (Neural Net Model); run one phase
f_backProp_dynLR_v3 <- function (par,training, nKO_MM = 0,
                              preW.IH = NULL, preW.HO = NULL,
                              preLR = NULL, preLrHO = NULL) {
  ### extract parameters ###
  # a <- par$a
  b <- par$b
  gamma <- par$gamma
  eta <- par$eta
  nHid <- par$nHidden$nHV + par$nHidden$nHMM + par$nHidden$nHA
  nBlock <- par$nBlock
  multiPhase <- par$multiPhase
  actFun <- par$actFun
  INPUT <- as.matrix(training$INPUT)
  OUTPUT <- as.matrix(training$OUTPUT)
  TEST <- as.matrix(training$TEST)
  trialType <- training$trialType
  trialTypeTest <- training$trialTypeTest
  
  ### create relevant matrices and scalars ###
  nOut <- ncol(OUTPUT) # number of outcomes
  nStim <- ncol(INPUT) # number of stimuli
  nTrialType <- nrow(INPUT) # number of trial types
  nTrialTypeTest <- length(trialTypeTest)
  nTest <- nrow(TEST) # number of test trials
  nTrial <- nTrialType*nBlock # number of total trials
  
  ### creation of matrices and add initial values ###
  actOT <- matrix(NA,nrow = nTrialTypeTest, ncol = nOut) # act. output for test
  actH <- matrix(NA,nrow = nTrial, ncol = nHid) # act. hidden layer
  actO <- matrix(NA,nrow = nTrial, ncol = nOut) # act. output layer
  deltaH <- matrix(NA,nrow = nTrial, ncol = nHid) # delta hidden
  deltaO <- matrix(NA,nrow = nTrial, ncol = nOut) # delta output
  dwIH <- matrix(0, nrow = nStim, ncol = nHid) # change input-hidden weights
  dwHO <- matrix(0, nrow = nHid, ncol = nOut) # change hidden-output weights
  outDis <- matrix(NA, nrow = nTrial) # volatile learning rate
  lrIH <- matrix(NA, nrow = nTrial, ncol = nStim) # volatile learning rate I to H
  colnames(lrIH) <- colnames(INPUT)
  lrHO <- matrix(NA, nrow = nTrial) # volatile learning rate
  
  # initial values (random or previous phases)
  if (is.null(preW.IH)) {
    wIH <- matrix(runif(nStim*nHid)-0.5,nrow = nStim, ncol = nHid) # rows input units (stim), cols hidden units
  } else {wIH <- preW.IH}
  if (is.null(preW.HO)) {
    wHO <- matrix(runif(nHid*nOut)-0.5, nrow = nHid) # rows hidden units, cols output units or options
  } else {wHO <- preW.HO}
  
  ### remove connections for hidden auditory and visual paths ###
  conIH <- matrix(1, nrow = nStim, ncol = nHid) # C matrix in Castiello et al (2022) - NLM
  # block visual input with auditory hidden
  if (par$nHidden$nHA > 0) {
    conIH[(par$nInput$ctx+1):(par$nInput$ctx + par$nInput$vis),
          (nHid-par$nHidden$nHA + 1):nHid] <- 0}
  # block auditory input with visual hidden
  if (par$nHidden$nHV > 0) {
    conIH[(par$nInput$ctx + par$nInput$vis + 1):nStim,
          1:par$nHidden$nHV] <- 0}
  wIH <- wIH * conIH
  
  #### backpropagation algorithm ###
  # see also: https://web.stanford.edu/group/pdplab/pdphandbook/handbookch6.html
  for (nB in 1:nBlock) {
    # randomized trial types within a block
    randT <- sample(1:nTrialType)
    INPUT <- as.matrix(INPUT[randT,])
    OUTPUT <- as.matrix(OUTPUT[randT,])
    trialType <- trialType[randT]
    for (t in 1:nTrialType) {
      ### ### ### activation ### ### ### 
      # which trial
      trial <- t+((nB-1)*nTrialType)
      # forward activation # chain rule: act_fun(act_fun(trialBlock %*% wIH) %*% wHO)
      actH[trial,] <- actFun(INPUT[t,] %*% wIH) # INPUT[t,] assumed to be a 1 x nStim matrix
      actO[trial,] <- actFun(actH[trial,] %*% wHO) # actH[t,] assumed to be a 1 x nOut matrix
     
      ### ### ### learning (weight changes) ### ### ###
      
      ### delta (error) for output
      deltaO[trial,] <- (OUTPUT[t,] - actO[trial,]) * f_derAct(actO[trial,])
      
      ### delta (error) for hidden
      deltaH[trial,] <- (deltaO[trial,] %*% t(wHO)) * f_derAct(actH[trial,])
      
      ### alpha ###
      outDis[trial] <- mean(abs(OUTPUT[t,] - actO[trial,]))
      if (trial == 1) {
        # Learning Rates (LR)
        if (is.null(preLR)) { # phase 1
          lrIH[trial,] <- gamma * outDis[trial] * INPUT[t,]
          lrHO[trial] <- eta*(1-outDis[trial])
        } else { # phase > 1
          temp <- gamma*outDis[trial]*INPUT[t,] + (1-gamma)*preLR[nrow(preLR),]*INPUT[t,]
          # carrying over previous learning rates of absent stimuli
          lrIH[trial,] <- temp + preLR[nrow(preLR),]*(1-INPUT[t,])
          lrHO[trial] <- eta*(1-outDis[trial])+(1-eta)*preLrHO[length(preLrHO)]
        }
      } else { # trial > 1
        # Learning Rates (LR)
        temp <- gamma*outDis[trial]*INPUT[t,] + (1-gamma)*lrIH[trial-1,]*INPUT[t,]
        # carrying over previous learning rates of absent stimuli
        lrIH[trial,] <- temp + lrIH[trial-1,]*(1-INPUT[t,])
        lrHO[trial] <- eta*(1-outDis[trial])+(1-eta)*lrHO[trial-1]
      }
      # lrHO <- 1 - outDis[trial]
      # lrHO_hi[trial,] <- f_softmax(rowSums(actH[trial,]*wHO),10)
      
      # weight change Hidden-Output
      dwHO[,] <- t(lrHO[trial]*(as.matrix(deltaO[trial,]) %*% actH[trial,])) + b*dwHO[,]
      # dwHO[,] <- t(lrHO_hi[trial,]*(as.matrix(deltaO[trial,]) %*% actH[trial,])) + b*dwHO[,]
      # update weights Hidden-Output
      wHO[,] <- wHO[,] + dwHO[,]
      
      # weight change Input-Hidden
      dwIH[,] <- (lrIH[trial,]*t(as.matrix(deltaH[trial,]) %*% INPUT[t,])) + b*dwIH[,]
      # update weights Input-Hidden
      wIH[,] <- wIH[,] + (dwIH[,] * conIH)
    } # end trials per block cycle # t
    
    # trial type vector
    if (nB == 1) {
      trialTypeLong <- as.character(trialType)
    } else {
      trialTypeLong <- c(trialTypeLong,as.character(trialType))
    }
  } # end all blocks cycle # nB
  
  ### test input patterns ###
  if (nTest!=0){  # KO mm units when necessary
    if (nKO_MM == 0) {
      for (t in 1:nTest) {
        actOT[t,] <- actFun(actFun(TEST[t,] %*% wIH) %*% wHO)
      }
    } else {
      if (nKO_MM > par$nHidden$nHMM) {
        warning("nKO_MM must be smaller than nHMM")
      } else {
        KO_units <- sample((par$nHidden$nHV + 1):(nHid-par$nHidden$nHA))[1:nKO_MM]
        wIH_KO <- wIH; wIH_KO[,KO_units] <- 0
        wHO_KO <- wHO; wHO_KO[KO_units,] <- 0
        actOT <- actFun(actFun(TEST %*% wIH_KO) %*% wHO_KO)
      } # if error message
    } # if KO mm units
  }
  # outputs' activations data frame
  db <- data.frame(nTrial=1:nTrial,nBlock=rep(1:nBlock,each=nTrialType),
                   trialType=trialTypeLong,outDis=outDis,
                   actO=actO)
  # create output list object
  colnames(actOT) <- paste0(rep("actOT.",nOut),1:nOut)
  output <- list(db=db, actO=actO, wIH=wIH, wHO=wHO, lrIH=lrIH, lrHO=lrHO,
                 actOT=data.frame(trialType=trialTypeTest,actOT))
  return(output)
}

# back propagation (Neural Net Model); run one phase
f_backProp_dynLR_v2 <- function (par, training, nKO_MM = 0,
                                 preW.IH = NULL, preW.HO = NULL,
                                 preLR = NULL) {
  ### extract parameters ###
  a <- par$a
  b <- par$b
  gamma <- par$gamma
  nHid <- par$nHidden$nHV + par$nHidden$nHMM + par$nHidden$nHA
  nBlock <- par$nBlock
  multiPhase <- par$multiPhase
  actFun <- par$actFun
  INPUT <- as.matrix(training$INPUT)
  OUTPUT <- as.matrix(training$OUTPUT)
  TEST <- as.matrix(training$TEST)
  trialType <- training$trialType
  trialTypeTest <- training$trialTypeTest
  
  ### create relevant matrices and scalars ###
  nOut <- ncol(OUTPUT) # number of outcomes
  nStim <- ncol(INPUT) # number of stimuli
  nTrialType <- nrow(INPUT) # number of trial types
  nTrialTypeTest <- length(trialTypeTest)
  nTest <- nrow(TEST) # number of test trials
  nTrial <- nTrialType*nBlock # number of total trials
  
  ### creation of matrices and add initial values ###
  actOT <- matrix(NA,nrow = nTrialTypeTest, ncol = nOut) # act. output for test
  actH <- matrix(NA,nrow = nTrial, ncol = nHid) # act. hidden layer
  actO <- matrix(NA,nrow = nTrial, ncol = nOut) # act. output layer
  deltaH <- matrix(NA,nrow = nTrial, ncol = nHid) # delta hidden
  deltaO <- matrix(NA,nrow = nTrial, ncol = nOut) # delta output
  dwIH <- matrix(0, nrow = nStim, ncol = nHid) # change input-hidden weights
  dwHO <- matrix(0, nrow = nHid, ncol = nOut) # change hidden-output weights
  lrIH <- matrix(NA, nrow = nTrial) # volatile learning rate
  lrIH_trialType <- matrix(NA, nrow = nBlock, ncol = nTrialType)
  colnames(lrIH_trialType) <- trialType
  
  # initial values (random or previous phases)
  if (is.null(preW.IH)) {
    wIH <- matrix(runif(nStim*nHid)-0.5,nrow = nStim, ncol = nHid) # rows input units (stim), cols hidden units
  } else {wIH <- preW.IH}
  if (is.null(preW.HO)) {
    wHO <- matrix(runif(nHid*nOut)-0.5, nrow = nHid) # rows hidden units, cols output units or options
  } else {wHO <- preW.HO}
  
  ### remove connections for hidden auditory and visual paths ###
  conIH <- matrix(1, nrow = nStim, ncol = nHid) # C matrix in Castiello et al (2022) - NLM
  # block visual input with auditory hidden
  if (par$nHidden$nHA > 0) {
    conIH[(par$nInput$ctx+1):(par$nInput$ctx + par$nInput$vis),
          (nHid-par$nHidden$nHA + 1):nHid] <- 0}
  # block auditory input with visual hidden
  if (par$nHidden$nHV > 0) {
    conIH[(par$nInput$ctx + par$nInput$vis + 1):nStim,
          1:par$nHidden$nHV] <- 0}
  wIH <- wIH * conIH
  
  #### backpropagation algorithm ###
  # see also: https://web.stanford.edu/group/pdplab/pdphandbook/handbookch6.html
  for (nB in 1:nBlock) {
    # randomized trial types within a block
    randT <- sample(1:nTrialType)
    INPUT <- as.matrix(INPUT[randT,])
    OUTPUT <- as.matrix(OUTPUT[randT,])
    trialType <- trialType[randT]
    for (t in 1:nTrialType) {
      ### ### ### activation ### ### ### 
      trial <- t+((nB-1)*nTrialType)
      # forward activation # chain rule: act_fun(act_fun(trialBlock %*% wIH) %*% wHO)
      actH[trial,] <- actFun(INPUT[t,] %*% wIH) # INPUT[t,] assumed to be a 1 x nStim matrix
      actO[trial,] <- actFun(actH[trial,] %*% wHO) # actH[t,] assumed to be a 1 x nOut matrix
      
      ### ### ### learning (weight changes) ### ######
      
      ### delta (error) for output
      deltaO[trial,] <- (OUTPUT[t,] - actO[trial,]) * f_derAct(actO[trial,])
      
      ### delta (error) for hidden
      deltaH[trial,] <- (deltaO[trial,] %*% t(wHO)) * f_derAct(actH[trial,]) #rowSums(as.matrix(deltaO[t,] * wHO[,]))
      
      ### alpha ###
      if (nB == 1) {
        lrIH[trial] <- mean(abs(OUTPUT[t,] - actO[trial,]))
        if (is.null(preLR)) {
          lrIH_trialType[nB,trialType[t]] <- lrIH[trial]
        } else {
          lrIH[trial] <- gamma * lrIH[trial] + (1 - gamma) * preLR[nrow(preLR),trialType[t]]
          lrIH_trialType[nB,trialType[t]] <- lrIH[trial]
        }
      } else {
        lrIH[trial] <- mean(abs(OUTPUT[t,] - actO[trial,]))
        lrIH[trial] <- gamma * lrIH[trial] + (1 - gamma) * lrIH_trialType[nB-1,trialType[t]]
        lrIH_trialType[nB,trialType[t]] <- lrIH[trial]
      }
      lrHO <- 1 - lrIH[trial]
      
      # weight change Hidden-Output
      dwHO[,] <- t(lrHO*(as.matrix(deltaO[trial,]) %*% actH[trial,])) + b*dwHO[,]
      # update weights Hidden-Output
      wHO[,] <- wHO[,] + dwHO[,]
      
      # weight change Input-Hidden
      dwIH[,] <- t(lrIH[trial]*(as.matrix(deltaH[trial,]) %*% INPUT[t,])) + b*dwIH[,]
      # update weights Input-Hidden
      wIH[,] <- wIH[,] + (dwIH[,] * conIH)
    } # end trials per block cycle # t
    if (nB == 1) {
      trialTypeLong <- as.character(trialType)
    } else {
      trialTypeLong <- c(trialTypeLong,as.character(trialType))
    }
  } # end all blocks cycle # nB
  
  ### test input patterns ###
  if (nTest!=0){  # KO mm units when necessary
    if (nKO_MM == 0) {
      for (t in 1:nTest) {
        actOT[t,] <- actFun(actFun(TEST[t,] %*% wIH) %*% wHO)
      }
    } else {
      if (nKO_MM > par$nHidden$nHMM) {
        warning("nKO_MM must be smaller than nHMM")
      } else {
        KO_units <- sample((par$nHidden$nHV + 1):(nHid-par$nHidden$nHA))[1:nKO_MM]
        wIH_KO <- wIH; wIH_KO[,KO_units] <- 0
        wHO_KO <- wHO; wHO_KO[KO_units,] <- 0
        actOT <- actFun(actFun(TEST %*% wIH_KO) %*% wHO_KO)
      } # if error message
    } # if KO mm units
  }
  # outputs' activations data frame
  db <- data.frame(nTrial=1:nTrial,nBlock=rep(1:nBlock,each=nTrialType),
                   trialType=trialTypeLong,lrIH=lrIH,actO=actO)
  # create output list object
  colnames(actOT) <- paste0(rep("actOT.",nOut),1:nOut)
  output <- list(db=db, actO=actO, wIH=wIH, wHO=wHO, lrTt=lrIH_trialType,
                 actOT=data.frame(trialType=trialTypeTest,actOT))
  return(output)
}

# back propagation (Neural Net Model); run one phase
f_backProp_dynLR_v1 <- function (par, training, nKO_MM = 0, 
                                 preW.IH = NULL, preW.HO = NULL,
                                 preLR = NULL) {
  ### extract parameters ###
  a <- par$a
  b <- par$b
  gamma <- par$gamma
  nHid <- par$nHidden$nHV + par$nHidden$nHMM + par$nHidden$nHA
  nBlock <- par$nBlock
  multiPhase <- par$multiPhase
  actFun <- par$actFun
  INPUT <- as.matrix(training$INPUT)
  OUTPUT <- as.matrix(training$OUTPUT)
  TEST <- as.matrix(training$TEST)
  trialType <- training$trialType
  trialTypeTest <- training$trialTypeTest
  
  ### create relevant matrices and scalars ###
  nOut <- ncol(OUTPUT) # number of outcomes
  nStim <- ncol(INPUT) # number of stimuli
  nTrialType <- nrow(INPUT) # number of trial types
  nTrialTypeTest <- length(trialTypeTest)
  nTest <- nrow(TEST) # number of test trials
  nTrial <- nTrialType*nBlock # number of total trials
  
  ### creation of matrices and add initial values ###
  actOT <- matrix(NA,nrow = nTrialTypeTest, ncol = nOut) # act. output for test
  actH <- matrix(NA,nrow = nTrial, ncol = nHid) # act. hidden layer
  actO <- matrix(NA,nrow = nTrial, ncol = nOut) # act. output layer
  deltaH <- matrix(NA,nrow = nTrial, ncol = nHid) # delta hidden
  deltaO <- matrix(NA,nrow = nTrial, ncol = nOut) # delta output
  dwIH <- matrix(0, nrow = nStim, ncol = nHid) # change input-hidden weights
  dwHO <- matrix(0, nrow = nHid, ncol = nOut) # change hidden-output weights
  lrIH <- matrix(NA, nrow = nTrial) # volatile learning rate
  
  # initial values (random or previous phases)
  if (is.null(preW.IH)) {
    wIH <- matrix(runif(nStim*nHid)-0.5,nrow = nStim, ncol = nHid) # rows input units (stim), cols hidden units
  } else {wIH <- preW.IH}
  if (is.null(preW.HO)) {
    wHO <- matrix(runif(nHid*nOut)-0.5, nrow = nHid) # rows hidden units, cols output units or options
  } else {wHO <- preW.HO}
  
  ### remove connections for hidden auditory and visual paths ###
  conIH <- matrix(1, nrow = nStim, ncol = nHid) # C matrix in Castiello et al (2022) - NLM
  # block visual input with auditory hidden
  if (par$nHidden$nHA > 0) {
    conIH[(par$nInput$ctx+1):(par$nInput$ctx + par$nInput$vis),
          (nHid-par$nHidden$nHA + 1):nHid] <- 0}
  # block auditory input with visual hidden
  if (par$nHidden$nHV > 0) {
    conIH[(par$nInput$ctx + par$nInput$vis + 1):nStim,
          1:par$nHidden$nHV] <- 0}
  wIH <- wIH * conIH
  
  #### backpropagation algorithm ###
  # see also: https://web.stanford.edu/group/pdplab/pdphandbook/handbookch6.html
  for (nB in 1:nBlock) {
    # randomized trial types within a block
    randT <- sample(1:nTrialType)
    INPUT <- as.matrix(INPUT[randT,])
    OUTPUT <- as.matrix(OUTPUT[randT,])
    trialType <- trialType[randT]
    for (t in 1:nTrialType) {
      ### ### ### activation ### ### ### 
      trial <- t+((nB-1)*nTrialType)
      # forward activation # chain rule: act_fun(act_fun(trialBlock %*% wIH) %*% wHO)
      actH[trial,] <- actFun(INPUT[t,] %*% wIH) # INPUT[t,] assumed to be a 1 x nStim matrix
      actO[trial,] <- actFun(actH[trial,] %*% wHO) # actH[t,] assumed to be a 1 x nOut matrix
      
      ### ### ### learning (weight changes) ### ######
      
      ### delta (error) for output
      deltaO[trial,] <- (OUTPUT[t,] - actO[trial,]) * f_derAct(actO[trial,])
      
      ### delta (error) for hidden
      deltaH[trial,] <- (deltaO[trial,] %*% t(wHO)) * f_derAct(actH[trial,]) #rowSums(as.matrix(deltaO[t,] * wHO[,]))
      
      ### alpha ###
      if (trial == 1) {
        lrIH[trial] <- mean(abs(OUTPUT[t,] - actO[trial,]))
        if (!is.null(preLR)) {
          lrIH[trial] <- gamma * lrIH[trial] + (1 - gamma) * preLR[length(preLR)]
        }
      } else {
        lrIH[trial] <- mean(abs(OUTPUT[t,] - actO[trial,]))
        lrIH[trial] <- gamma * lrIH[trial] + (1 - gamma) * lrIH[trial-1]
      }
      lrHO <- 1 - lrIH[trial]
      
      # weight change Hidden-Output
      dwHO[,] <- t(lrHO*(as.matrix(deltaO[trial,]) %*% actH[trial,])) + b*dwHO[,]
      # update weights Hidden-Output
      wHO[,] <- wHO[,] + dwHO[,]
      
      # weight change Input-Hidden
      dwIH[,] <- t(lrIH[trial]*(as.matrix(deltaH[trial,]) %*% INPUT[t,])) + b*dwIH[,]
      # update weights Input-Hidden
      wIH[,] <- wIH[,] + (dwIH[,] * conIH)
    } # end trials per block cycle # t
    if (nB == 1) {
      trialTypeLong <- as.character(trialType)
    } else {
      trialTypeLong <- c(trialTypeLong,as.character(trialType))
    }
  } # end all blocks cycle # nB
  
  ### test input patterns ###
  if (nTest!=0){  # KO mm units when necessary
    if (nKO_MM == 0) {
      for (t in 1:nTest) {
        actOT[t,] <- actFun(actFun(TEST[t,] %*% wIH) %*% wHO)
      }
    } else {
      if (nKO_MM > par$nHidden$nHMM) {
        warning("nKO_MM must be smaller than nHMM")
      } else {
        KO_units <- sample((par$nHidden$nHV + 1):(nHid-par$nHidden$nHA))[1:nKO_MM]
        wIH_KO <- wIH; wIH_KO[,KO_units] <- 0
        wHO_KO <- wHO; wHO_KO[KO_units,] <- 0
        actOT <- actFun(actFun(TEST %*% wIH_KO) %*% wHO_KO)
      } # if error message
    } # if KO mm units
  }
  # outputs' activations data frame
  db <- data.frame(nTrial=1:nTrial,nBlock=rep(1:nBlock,each=nTrialType),
                   trialType=trialTypeLong,lrIH=lrIH,actO=actO)
  # create output list object
  colnames(actOT) <- paste0(rep("actOT.",nOut),1:nOut)
  output <- list(db=db, actO=actO, wIH=wIH, wHO=wHO,
                 actOT=data.frame(trialType=trialTypeTest,actOT))
  return(output)
}


# setting up input data in the format that f_backProp can read it 
f_prepData <- function (train) {
  # create model parameters list
  par <- train[,grepl("par.",colnames(train))]
  par <- list(a=par$par.a[1],b=par$par.b[1],nBlock=par$par.nBlock[!is.na(par$par.nBlock)],
              nHidden=data.frame(nHV=par$par.nH.nHV[1],nHMM=par$par.nH.nHMM[1],nHA=par$par.nH.nHA[1]),
              nInput=data.frame(ctx=par$par.nI.ctx[1],vis=par$par.nI.vis[1],aud=par$par.nI.aud[1]),
              actFun=function (netin) {1/(1+exp(-(netin-2.2)))}
  )
  if (length(par$nBlock) != length(unique(train$phase))) {
    warning("Missmatch between number of phases and number of nBlock values.")
  }
  # create training blocks
  trPh <- list()
  for (ph in 1:length(unique(train$phase))) {
    temp <- train[train$phase == unique(train$phase)[ph],!grepl("par",colnames(train))]
    trPh[[ph]] <- list(INPUT=temp[temp$matType=="INPUT",grepl("in",colnames(temp))],
                       TEST=temp[temp$matType=="TEST",grepl("in",colnames(temp))],
                       OUTPUT=temp[temp$matType=="OUTPUT",grepl("out",colnames(temp))],
                       trialType=temp$trialType[temp$matType=="INPUT"],
                       trialTypeTest=temp$trialType[temp$matType=="TEST"])
  }
  return(list(par=par,trPh=trPh))
}

# run simulation for one subject all phases (uses f_backProp)
f_runSim <- function (par, trPh, print_weights = NULL, dynModType = "v1") {
  phs <- list()
  weights <- list()
  for (ph in 1:length(trPh)) {
    
    
    # # # # Phase 1 # # # #
    if (ph == 1) {
      par$multiPhase <- 0
      # use the ph th nBlock value
      tempPar <- par
      tempPar$nBlock <- par$nBlock[ph]
      # run backpropagation
      if (is.null(par$gamma)) {
        phs[[ph]] <- f_backProp(tempPar,training=trPh[[ph]])
      } else {
        if (dynModType == "v1") {
          phs[[ph]] <- f_backProp_dynLR_v1(tempPar,training=trPh[[ph]])
        } else if (dynModType == "v2") {
          message("only works with individual cues, trial type are one cue trials")
          phs[[ph]] <- f_backProp_dynLR_v2(tempPar,training=trPh[[ph]])
        } else if (dynModType == "v3") {
          # warning("not working yet...")
          phs[[ph]] <- f_backProp_dynLR_v3(tempPar,training=trPh[[ph]])
        } else {warning("not model type specified [see f_runSim()]")}
      } # end gamma
      # add Learning Rates to the main database
      if (dynModType == "v3") {
        colnames(phs[[ph]]$lrIH) <- paste0(substr(colnames(phs[[ph]]$lrIH),4,
                                                  nchar(colnames(phs[[ph]]$lrIH))),"_lrIH")
        # colnames(phs[[ph]]$lrHO) <- paste0("h",1:sum(par$nHidden),"_lrHO")
        exp <- data.frame(ph=ph,phs[[ph]]$db,phs[[ph]]$lrIH,lrHO=phs[[ph]]$lrHO)
      } else {
        exp <- data.frame(ph=ph,phs[[ph]]$db)
      } # end dynModType
      # if statement to avoid errors when input file does not have TEST trials
      if (nrow(phs[[ph]]$actOT) != 0){
        test <- data.frame(ph=ph,phs[[ph]]$actOT)
      }
      # save weights
      weights[[ph]] <- list(wIH=phs[[ph]]$wIH,wHO=phs[[ph]]$wHO)
      # print weights
      if (!is.null(print_weights)) {
        f_printWeights(weights,ph,trPh,tempPar,print_weights)
      }
    
      
    # # # # Phase > 1 # # # #
    } else {
      par$multiPhase <- 1
      # use the ph th nBlock value
      tempPar <- par
      tempPar$nBlock <- par$nBlock[ph]
      # run backpropagation
      if (is.null(par$gamma)) {
        phs[[ph]] <- f_backProp(tempPar,training=trPh[[ph]],
                                preW.IH=phs[[ph-1]]$wIH,preW.HO=phs[[ph-1]]$wHO)
      } else {
        if (dynModType == "v1") {
          phs[[ph]] <- f_backProp_dynLR_v1(tempPar,training=trPh[[ph]],
                                           preW.IH=phs[[ph-1]]$wIH,preW.HO=phs[[ph-1]]$wHO,
                                           preLR=phs[[ph-1]]$db$lrIH) 
        } else if (dynModType == "v2") {
          message("only works with individual cues, trial type are one cue trials")
          phs[[ph]] <- f_backProp_dynLR_v2(tempPar,training=trPh[[ph]],
                                           preW.IH=phs[[ph-1]]$wIH,preW.HO=phs[[ph-1]]$wHO,
                                           preLR=phs[[ph-1]]$lrIH)
        } else if (dynModType == "v3") {
          # warning("not working yet...")
          phs[[ph]] <- f_backProp_dynLR_v3(tempPar,training=trPh[[ph]],
                                           preW.IH=phs[[ph-1]]$wIH,preW.HO=phs[[ph-1]]$wHO,
                                           preLR=phs[[ph-1]]$lrIH,preLrHO=phs[[ph-1]]$lrHO)
        }
      } # end gamma
      phs[[ph]]$db$nBlock <- phs[[ph]]$db$nBlock + max(exp$nBlock)
      # add Learning Rates to the main database
      if (dynModType == "v3") {
        colnames(phs[[ph]]$lrIH) <- paste0(substr(colnames(phs[[ph]]$lrIH),4,
                                                  nchar(colnames(phs[[ph]]$lrIH))),"_lrIH")
        # colnames(phs[[ph]]$lrHO) <- paste0("h",1:sum(par$nHidden),"_lrHO")
        exp <- rbind(exp,data.frame(ph=ph,phs[[ph]]$db,phs[[ph]]$lrIH,lrHO=phs[[ph]]$lrHO))
      } else {
        exp <- rbind(exp,data.frame(ph=ph,phs[[ph]]$db))
      } # end dynModType
      # if statement to avoid errors when input file does not have TEST trials
      if (nrow(phs[[ph]]$actOT) != 0){
        test <- rbind(test,data.frame(ph=ph,phs[[ph]]$actOT))
      }
      # save weights
      weights[[ph]] <- list(wIH=phs[[ph]]$wIH,wHO=phs[[ph]]$wHO)
      # print weights
      if (!is.null(print_weights)) {
        f_printWeights(weights,ph,trPh,tempPar,print_weights)
      }
    } # end if phase 1
  } # end Phase
  # melt for better visualization
  if (ncol(as.matrix(trPh[[1]]$OUTPUT)) > 1) {
    exp <- reshape2::melt(exp,measure.vars = colnames(exp)[grepl("act",colnames(exp))])
    colnames(exp)[(ncol(exp)-1):ncol(exp)] <- c("out","actO")
    if (exists("test")) {
      test <- melt(test, measure.vars = colnames(test)[3:4])
      colnames(test)[3:4] <- c("out","actOT")
    }
  }
  # create test if it was not required
  if (!exists("test")) {test <- NULL}
  return(list(exp=exp,test=test,weights=weights))
}



# visualization (exp data, test data, number of outputs, 
# plot individual participants, plot test data)
f_plotSims <- function(exp, test, nSim, nOut,
                       doIndPart = 0, doTest = 0, dynModType = NULL) {
  # plot depending on number of output units
  pMean <- ggplot(exp,aes(x=nBlock,y=actO,col=trialType)) + 
    labs(title = paste0("N = ",nSim,"; a = ",par$a,"; b = ",par$b),
         x = "Blocks (1 block = n trial types)",
         y = "Mean Output Activation") + 
    geom_vline(xintercept = cumsum(par$nBlock)) +
    stat_summary(geom="line") + stat_summary() +
    coord_cartesian(ylim = c(0,1)) +
    scale_y_continuous(breaks = c(0.0,0.5,1.0)) +
    scale_x_continuous(breaks = cumsum(par$nBlock)) +
    theme_bw()
  # create individual network plots
  if (doIndPart == 1 & nSim <= 24) {
    pInd <- ggplot(exp,aes(x=nBlock,y=actO,col=trialType)) + 
      labs(title = paste0("N = ",nSim,"; a = ",par$a,"; b = ",par$b),
           x = "Blocks (1 block = n trial types)",
           y = "Mean Output Activation") + 
      geom_vline(xintercept = cumsum(par$nBlock)) +
      geom_line() + 
      coord_cartesian(ylim = c(0,1)) +
      scale_y_continuous(breaks = c(0.0,0.5,1.0)) +
      scale_x_continuous(breaks = cumsum(par$nBlock)) +
      facet_grid(nSubj ~ .) +
      theme_bw()
  } else {pInd <- NULL}
  # create test plot
  if (doTest == 1 & !is.null(test)) {
    colnames(test)[grepl("actOT",colnames(test))] <- "actOT"
    pTest <- ggplot(test, aes(x=ph,y=actOT,col=trialType)) + 
      labs(title="Test",x="Phase",y="Mean Output Activation") +
      scale_x_continuous(breaks = unique(test$ph)) +
      stat_summary(geom="line") + stat_summary() + 
      theme_bw()
  } else {pTest <- NULL} 
   
  # plot learning rates
  if (!is.null(exp$outDis)) {
    if (nOut > 1) {
      temp <- exp[exp$out == unique(exp$out)[1],]; temp$out <- NULL
    } else {
      temp <- exp
    }
    
    
    # # # # figure for model variable LR verion 3 # # # #
    if (dynModType == "v3") {
      templr <- melt(temp,measure.vars = colnames(temp)[grepl("_lrIH",colnames(temp))])
      pLR <- ggplot(templr, aes(x=nBlock,y=value,col=variable)) +
        labs(title="Learning Rates (LR)",x="Blocks (1 block = n trial types)",
             y="LR",col="Trial Type",shape="Layer LR") +
        geom_vline(xintercept = cumsum(par$nBlock)) +
        stat_summary(geom="line") + stat_summary() +
        scale_x_continuous(breaks = cumsum(par$nBlock)) + 
        theme_bw()
      
    # # # # figure for model variable LR version 3 # # # #
    } else {
      temp$lrHO <- 1 - temp$outDis
      templr <- reshape2::melt(temp, measure.vars = c("outDis","lrHO"))
      pLR <- ggplot(templr,aes(x=nBlock,y=value,col=trialType,shape=variable)) + 
        labs(title="Learning Rates (LR)",x="Blocks (1 block = n trial types)",
             y="LR",col="Trial Type",shape="Layer LR") +
        geom_vline(xintercept = cumsum(par$nBlock)) +
        stat_summary(geom="line") + stat_summary() +  
        coord_cartesian(ylim = c(0,1)) +
        scale_y_continuous(breaks = c(0.0,0.5,1.0)) +
        scale_x_continuous(breaks = cumsum(par$nBlock)) + 
        theme_bw()
    }
  } else {pLR <- NULL}
  
  if (nOut > 1) {
    pMean <- pMean + facet_grid(. ~ out)
    pInd <- pInd + facet_grid(nSubj ~ out)
    pTest <- pTest + facet_grid(. ~ out)
  }
  return(list(pMean=pMean,pInd=pInd,pTest=pTest,pLR=pLR))
}

# if his function is run, then will print csv and png files of the weights
f_printWeights <- function(weights,ph,trPh,tempPar,print_weights) {
  # create weights matrices
  wIH <- data.frame(weights[[ph]]$wIH)
  wHO <- data.frame(weights[[ph]]$wHO)
  
  # name weights
  rownames(wIH) <- colnames(trPh[[ph]]$INPUT)
  colnames(wIH) <- paste0(rep(colnames(tempPar$nHidden),tempPar$nHidden),".",
                          1:sum(tempPar$nHidden))
  rownames(wHO) <- colnames(wIH)
  colnames(wHO) <- paste0("out.",1:ncol(data.frame(trPh[[ph]]$OUTPUT)))
  
  # combine layers
  w <- rbind(wIH,"",t(wHO))
  rownames(w)[ncol(trPh[[ph]]$INPUT)+1] <- ""
  
  # print csv
  write.csv(w,paste0(getwd(),"/weights/s",print_weights,"_ph",ph,".csv"))
  
  # visualize weights (Input-Hidden)
  datWIH <- reshape2::melt(data.frame(input=rownames(wIH),wIH),id.vars="input")
  datWIH$value[datWIH$value==0] <- NA
  datWIH$input <- factor(datWIH$input,levels = rownames(wIH))
  datWIH$variable <- factor(datWIH$variable,levels = colnames(wIH))
  pWih <- ggplot2::ggplot(datWIH,aes(x=input,y=variable,fill=value)) + 
    labs(title = "L = 1",x = "Input", y = "Hidden", fill="Weight") +
    geom_tile() + theme_minimal() +
    scale_fill_gradient2(low="red",mid="white",high="green") +
    theme(axis.text.x = element_text(hjust=1,angle=90))
  
  # visualize weights (Hidden-Output)
  datWHO <- reshape2::melt(data.frame(hidden=rownames(wHO),wHO),id.vars="hidden")
  datWHO$value[datWHO$value==0] <- NA
  datWHO$hidden <- factor(datWHO$hidden, levels = rownames(wHO))
  pWho <- ggplot2::ggplot(datWHO,aes(x=variable,y=hidden,fill=value)) + 
    labs(title = "L = 2",x = "Output", y = "Hidden", fill="Weight") +
    geom_tile() + theme_minimal() +
    scale_fill_gradient2(low="red",mid="white",high="green") +
    theme(axis.text.x = element_text(hjust=1,angle=90))
  
  if ((nrow(wIH)/ncol(wHO)) > 2) {
    pWidths <- c(2,1)
  } else if ((nrow(wIH)/ncol(wHO)) < 0.5) { 
    pWidths <- c(1,2)
  } else {
    pWidths <- c(1,1)
  }
  # combine plots
  p <- ggpubr::ggarrange(pWih,pWho,align="hv",common.legend = T,
                         widths = pWidths)
  
  # print plots
  ggplot2::ggsave(paste0(getwd(),"/weights/p_s",print_weights,"_ph",ph,".png"),
         plot=p,width=14,height=14,dpi=600,units="cm",limitsize=T)
}

# to get the hidden layer representation given weights and inputs (trials)
f_hiddenRepresen <- function(weigths,inputs,par) {
  actFun <- par$actFun
  actH <- matrix(NA,nrow=ncol(weigths$wIH),ncol=nrow(inputs))
  diffActO <- as.vector(rep(NA,nrow(inputs)))
  for (t in 1:nrow(inputs)) {
    actH[,t] <- actFun(inputs[t,] %*% weigths$wIH) 
    temp <- actFun(actFun(inputs[t,] %*% weigths$wIH) %*% weigths$wHO)
    if (ncol(weigths$wHO)!=1) { # for one or multiple outcomes
      diffActO[t] <- abs(temp[1] - temp[2])
    } else {
      diffActO[t] <- abs(temp)
    }
  }
  # correlations between trial types
  corrV_A <- cor(actH[,1],actH[,2])
  corrV_VA <- cor(actH[,1],actH[,3])
  corrA_VA <- cor(actH[,2],actH[,3])
  # discrimination rate
  elemComp <- ((diffActO[1]+diffActO[2])/2)-diffActO[3]
  temp2 <- (par$nHidden$nHV+1):(par$nHidden$nHV+par$nHidden$nHMM)
  # correlation between weights
  corMM <- cor(weigths$wIH[1,temp2],weigths$wIH[2,temp2])
  return(data.frame(corrV_A,corrV_VA,corrA_VA,corMM,elemComp))
}

# visualize distribution of the weights and hidden activation correlations
f_plotCorrels <- function(correls,binSize,nSim,titLab) {
  correls <- correls[,c("nSubj","corrV_A","corMM","elemComp")]
  temp <- reshape2::melt(correls,id.vars="nSubj")
  temp$variable <- factor(temp$variable, levels = unique(colnames(correls)[-1]))
  levels(temp$variable) <- c("act(Hid.): V vs A","W(Mul.Mod.): V vs A",
                             "Discrimination Rate")
  # plot 1
  p1 <- ggplot2::ggplot(correls,aes(x=corrV_A,y=elemComp)) +
    labs(y="Discrimination Rate",
         subtitle = paste("# of simulations:",nSim)) +
    geom_vline(xintercept = 0, col="red", alpha=0.5) +
    geom_point() +
    coord_cartesian(xlim = c(-1,1)) +
    theme_minimal() + theme(axis.title.x = element_blank())
  # plot 2
  p2 <- ggplot2::ggplot(temp, aes(x=value)) + 
    labs(y="Count",subtitle = paste("# of simulations:",nSim)) +
    geom_vline(xintercept = 0, col="red", alpha=0.5) +
    geom_histogram(bins = binSize) + 
    coord_cartesian(xlim = c(-1,1)) +
    facet_grid(variable~.) + 
    theme_minimal() + theme(axis.title.x = element_blank())
  
  p <- annotate_figure(ggpubr::ggarrange(p1,p2),
                       top = text_grob(titLab, face="bold", size=16),
                       bottom = text_grob("Correation (r)", face="bold", size=12)
                       )
  return(p)
}
