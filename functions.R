# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # ALAAN: functions.R# # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # Associative # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Learning# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Artificial# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Neural# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # Network # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Script created by Santiago Castiello and Andy Delamater (28/03/2022)
# this code contain the model used in Castiello, et al (2022) - NLM which is 
# based on Delamater (2012) - L&B.

# functions:
  # f_loadLibraries: load R libraries
  # f_prepData: prepare read csv to lists to feed backpropagation functions
  # f_sanityCheck: check that .csv input are correct
  # f_runSim: use the result of f_prepData and run one subject with multiple phases
  # f_plotSims: plot simulations mean, individuals, and tests
  # f_printWeights2Layers: each layer weights figures and csv for mod1 and mod2
  # f_printWeightsNLayers: each layer weights figures and csv for mod3 and mod4
  # f_derAct: activation's derivative 
  # f_sigAct: sigmoidal activation function
  # f_mod0: Rescorla-Wagner model
  # f_mod1: Backpropagation function for one phase, received initial weights. Delamater (2012) - L&B
  # f_mod2: Backpropagation dynamic LR, received initial weights as f_mod1
  # f_mod3: Backpropagation. Xie & Seung (2003) - Neural Computation
  # f_mod4: Contrast Hebbian Learning (CHL) based on Detorakis, et al. (2019) - Neural Networks
  # f_mod5: CHL with random feedback from Detorakis, et al. (2019) - Neural Networks
  # f_mod6: same as model 4 but with dynamic learning rate

# install or load required libraries
f_loadLibraries <- function () {
  # libraries
  if (!require(rstudioapi)) {install.packages("rstudioapi")}; library(rstudioapi)
  if (!require(reshape2)) {install.packages("reshape2")}; library(reshape2)
  if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
  if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
  if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
} 

# setting up input data in the format that f_mod1 can read it 
f_prepData <- function (train) {
  # create model parameters list
  par <- train[,grepl("par.",colnames(train))]
  par <- list(nBlock=par$par.nBlock[!is.na(par$par.nBlock)],
              nHidden=data.frame(nHV=par$par.nH.nHV[!is.na(par$par.nH.nHV)],
                                 nHMM=par$par.nH.nHMM[!is.na(par$par.nH.nHMM)],
                                 nHA=par$par.nH.nHA[!is.na(par$par.nH.nHA)]),
              nInput=data.frame(ctx=par$par.nI.ctx[1],vis=par$par.nI.vis[1],aud=par$par.nI.aud[1])
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

# sanity check function
f_sanityCheck <- function() {
  if (sum(par$nInput) != ncol(trPh[[1]]$INPUT)) {
    warning("# of input units (matrix) and # of input unit type (ctx, aud, vis) does not match")
  }
  if (nrow(par$nHidden)>1 & mod_type != "mod3") {
    warning("training file have more than 1 hidden layer, please use mod3, mod4, or mod5")
  }
  if (mod_type == "mod2") {
    warning("make sure you declare gamma and eta values")
  }
  if (mod_type == "mod3") {
    warning("make sure you declare the bias term (i.e., adaptBias)")
  }
  if (mod_type == "mod4" | mod_type == "mod5") {
    warning("running CHL, this will be slow")
  }
} 

# run simulation for one subject all phases (uses f_mod1)
f_runSim <- function (par, trPh, subj, print_weights = F, mod_type = "mod1") {
  # create phases list to receive all information from models functions
  phs <- list()
  # create weights list to accumulate weights from phases
  weights <- list()
  for (ph in 1:length(trPh)) {
    
    
    # # # # Phase 1 # # # #
    if (ph == 1) {
      # use the ph th nBlock value
      tempPar <- par
      tempPar$nBlock <- par$nBlock[ph]
      # run neural network models
      if (mod_type == "mod0") {
        phs[[ph]] <- f_mod0(par=tempPar,training=trPh[[ph]])
        exp <- data.frame(ph=ph,phs[[ph]]$db)
        Vs <- data.frame(ph=ph,phs[[ph]]$Vs)
      } else if (mod_type == "mod1") {
        phs[[ph]] <- f_mod1(par=tempPar,training=trPh[[ph]])
        exp <- data.frame(ph=ph,phs[[ph]]$db)
      } else if (mod_type == "mod2") {
        phs[[ph]] <- f_mod2(par=tempPar,training=trPh[[ph]])
        colnames(phs[[ph]]$lrIH) <- paste0(substr(colnames(phs[[ph]]$lrIH),4,
                                                  nchar(colnames(phs[[ph]]$lrIH))),"_lrIH")
        exp <- data.frame(ph=ph,phs[[ph]]$db,phs[[ph]]$lrIH,lrHO=phs[[ph]]$lrHO)
      } else if (mod_type == "mod3") {
        phs[[ph]] <- f_mod3(par=tempPar,training=trPh[[ph]])
        exp <- data.frame(ph=ph,phs[[ph]]$db)
      } else if (mod_type == "mod4") {
        phs[[ph]] <- f_mod4(par=tempPar,training=trPh[[ph]])
        exp <- data.frame(ph=ph,phs[[ph]]$db)
        chl_error <- data.frame(ph=ph,phs[[ph]]$chl_error)
      } else if (mod_type == "mod5") {
        phs[[ph]] <- f_mod5(par=tempPar,training=trPh[[ph]])
        exp <- data.frame(ph=ph,phs[[ph]]$db)
        if (nrow(par$nHidden) == 1) {
          chl_error <- data.frame(ph=ph,phs[[ph]]$chl_error)
        }
      } else if (mod_type == "mod6") {
        phs[[ph]] <- f_mod6(par=tempPar,training=trPh[[ph]])
        colnames(phs[[ph]]$lrIH) <- paste0(substr(colnames(phs[[ph]]$lrIH),4,
                                                  nchar(colnames(phs[[ph]]$lrIH))),"_lrIH")
        exp <- data.frame(ph=ph,phs[[ph]]$db,phs[[ph]]$lrIH,lrHO=phs[[ph]]$lrHO)
        if (nrow(par$nHidden) == 1) {
          chl_error <- data.frame(ph=ph,phs[[ph]]$chl_error)          
        }
      } else {warning("not model type specified [see mod_type]")}
      
      # if statement to avoid errors when input file does not have TEST trials
      # else then test = NULL
      if (!is.null(phs[[ph]]$actOT)) {
        test <- data.frame(ph=ph,phs[[ph]]$actOT)
      } else {test <- NULL}
      
      
      
      # # # # Phase > 1 # # # #
    } else {
      # use the ph th nBlock value
      tempPar <- par
      tempPar$nBlock <- par$nBlock[ph]
      # run neural network models
      if (mod_type == "mod0") {
        phs[[ph]] <- f_mod0(par=tempPar,training=trPh[[ph]],preW=phs[[ph-1]]$W)
        # add correct number of blocks
        phs[[ph]]$db$nBlock <- phs[[ph]]$db$nBlock + max(exp$nBlock)
        exp <- rbind(exp,data.frame(ph=ph,phs[[ph]]$db))
        # add correct number of blocks 
        phs[[ph]]$Vs$nBlock <- phs[[ph]]$Vs$nBlock + max(Vs$nBlock)
        Vs <- rbind(Vs,data.frame(ph=ph,phs[[ph]]$Vs))
      } else if(mod_type == "mod1") {
        phs[[ph]] <- f_mod1(par=tempPar,training=trPh[[ph]],
                            preW.IH=phs[[ph-1]]$wIH,preW.HO=phs[[ph-1]]$wHO)
        # add correct number of blocks
        phs[[ph]]$db$nBlock <- phs[[ph]]$db$nBlock + max(exp$nBlock)
        exp <- rbind(exp,data.frame(ph=ph,phs[[ph]]$db))
      } else if (mod_type == "mod2") {
        phs[[ph]] <- f_mod2(par=tempPar,training=trPh[[ph]],
                            preW.IH=phs[[ph-1]]$wIH,preW.HO=phs[[ph-1]]$wHO,
                            preLR=phs[[ph-1]]$lrIH,preLrHO=phs[[ph-1]]$lrHO)
        colnames(phs[[ph]]$lrIH) <- paste0(substr(colnames(phs[[ph]]$lrIH),4,
                                                  nchar(colnames(phs[[ph]]$lrIH))),"_lrIH")
        # add correct number of blocks
        phs[[ph]]$db$nBlock <- phs[[ph]]$db$nBlock + max(exp$nBlock)
        exp <- rbind(exp,data.frame(ph=ph,phs[[ph]]$db,phs[[ph]]$lrIH,lrHO=phs[[ph]]$lrHO))
      } else if (mod_type == "mod3") {
        phs[[ph]] <- f_mod3(par=tempPar,training=trPh[[ph]],
                            preW=list(W=phs[[ph-1]]$W,
                                      C=phs[[ph-1]]$C))
        # add correct number of blocks
        phs[[ph]]$db$nBlock <- phs[[ph]]$db$nBlock + max(exp$nBlock)
        exp <- rbind(exp,data.frame(ph=ph,phs[[ph]]$db))
      } else if (mod_type == "mod4") {
        phs[[ph]] <- f_mod4(par=tempPar,training=trPh[[ph]],
                            preW=list(W=phs[[ph-1]]$W,
                                      C=phs[[ph-1]]$C))
        # add correct number of blocks
        phs[[ph]]$db$nBlock <- phs[[ph]]$db$nBlock + max(exp$nBlock)
        exp <- rbind(exp,data.frame(ph=ph,phs[[ph]]$db))
        chl_error <- rbind(chl_error,data.frame(ph=ph,phs[[ph]]$chl_error))
      } else if (mod_type == "mod5") {
        phs[[ph]] <- f_mod5(par=tempPar,training=trPh[[ph]],
                            preW=list(W=phs[[ph-1]]$W,
                                      G=phs[[ph-1]]$G,
                                      C=phs[[ph-1]]$C))
        # add correct number of blocks
        phs[[ph]]$db$nBlock <- phs[[ph]]$db$nBlock + max(exp$nBlock)
        exp <- rbind(exp,data.frame(ph=ph,phs[[ph]]$db))
        if (nrow(par$nHidden) == 1) {
          chl_error <- rbind(chl_error,data.frame(ph=ph,phs[[ph]]$chl_error))        
        }
      } else if (mod_type == "mod6") {
        phs[[ph]] <- f_mod6(par=tempPar,training=trPh[[ph]],
                            preW=list(W=phs[[ph-1]]$W,
                                      G=phs[[ph-1]]$G,
                                      C=phs[[ph-1]]$C),
                            preLR=phs[[ph-1]]$lrIH,preLrHO=phs[[ph-1]]$lrHO)
        colnames(phs[[ph]]$lrIH) <- paste0(substr(colnames(phs[[ph]]$lrIH),4,
                                                  nchar(colnames(phs[[ph]]$lrIH))),"_lrIH")
        # add correct number of blocks
        phs[[ph]]$db$nBlock <- phs[[ph]]$db$nBlock + max(exp$nBlock)
        exp <- rbind(exp,data.frame(ph=ph,phs[[ph]]$db,phs[[ph]]$lrIH,lrHO=phs[[ph]]$lrHO))
        if (nrow(par$nHidden) == 1) {
          chl_error <- rbind(chl_error,data.frame(ph=ph,phs[[ph]]$chl_error))        
        }
      } else {warning("not model type specified [see mod_type]")}
      
      # if statement to avoid errors when input file does not have TEST trials
      # else then test = NULL
      if (!is.null(phs[[ph]]$actOT)) {
        test <- rbind(test,data.frame(ph=ph,phs[[ph]]$actOT))
      } else {test <- NULL}
      
    } # end if phase 1
    
    
    
    # store weights
    if (mod_type == "mod1" | mod_type == "mod2") {
      weights[[ph]] <- list(wIH=phs[[ph]]$wIH,wHO=phs[[ph]]$wHO)
    } else if (mod_type == "mod6") {
      weights[[ph]] <- list(wIH=phs[[ph]]$W[[1]],wHO=phs[[ph]]$W[[2]])
    } else {
      weights[[ph]] <- phs[[ph]]$W
    }
    
    # print weights
    if (print_weights) {
      if (mod_type != "mod0") {
        # if (nrow(par$nHidden)==1) { # if 3 layers (input, hidden, output)
        if (mod_type == "mod1" | mod_type == "mod2" | 
            mod_type == "mod6") { # if 3 layers (input, hidden, output)
          f_printWeights2Layers(weights,ph,trPh,tempPar,subj)
        } else if (mod_type == "mod3" | mod_type == "mod4" | 
                   mod_type == "mod5") { # L > 2
          f_printWeightsNLayers(weights,ph,trPh,tempPar,subj)
        }
      } # end if mod_type
    } # end if print_weights
  } # end for phase
  
  # melt (i.e., create long format data.frames) for better visualization
  if (ncol(as.matrix(trPh[[1]]$OUTPUT)) > 1) {
    exp <- reshape2::melt(exp,measure.vars = colnames(exp)[grepl("act",colnames(exp))])
    colnames(exp)[(ncol(exp)-1):ncol(exp)] <- c("out","actO")
    if (!is.null(test)) {
      test <- melt(test, measure.vars = colnames(test)[-(1:2)])
      colnames(test)[-(1:2)] <- c("out","actOT")
    }
  } else {
    exp <- data.frame(exp,out="actO.1")
    if (!is.null(test)) {
      colnames(test)[ncol(test)] <- "actOT"
      test <- data.frame(test,out="actO.1")
    }
  }
  
  # if no mod4, mod5, or mod6 was conducted then no chl errors, thus NULL
  if (!exists("chl_error")) {chl_error <- NULL}
  if (!exists("Vs")) {Vs <- NULL}
  
  # create test and chl_error if not required 
  return(list(exp=exp,test=test,chl_error=chl_error,weights=weights,Vs=Vs))
}



# # # # # # # # # # Visualization and having weights# # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# visualization (exp data, test data, number of outputs, plot individual participants, plot test data)
f_plotSims <- function(exp, test, chl_error, Vs, par, nSim, doIndPart = F, 
                       mod_type, label_output = "") {
  # how many outputs?
  nOut <- length(unique(exp$out))
  
  # y-axis for mod0 can be also negative
  if (mod_type == "mod0") {
    ylim=c(-1,1)
    Vs <- reshape2::melt(Vs,measure.vars = colnames(Vs)[grepl("_",colnames(Vs))])
    Vs$variable <- as.character(Vs$variable)
    temp <- t(matrix(unlist(strsplit(Vs$variable,"_")),nrow=2))
    colnames(temp) <- c("input","output")
    Vs <- cbind(Vs,temp)
    pMeanMod0 <- ggplot(Vs, aes(x=nBlock, y=value, col=input)) +
      labs(title = paste0(mod_type,"; N = ",nSim),
           subtitle = label_output,
           x = "Blocks (1 block = number of trial types)",
           y = "Mean Inputs Weights",
           col = "inputs:") + 
      geom_vline(xintercept = cumsum(par$nBlock)) +
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0, alpha = 0.1) +
      stat_summary(fun = "mean", geom="line") +
      coord_cartesian(ylim = ylim) +
      scale_y_continuous(breaks = c(ylim[1],median(ylim),ylim[2])) +
      scale_x_continuous(breaks = cumsum(par$nBlock)) +
      theme_bw() + facet_grid(. ~ output)
  } else {
    ylim=c(0,1)
    pMeanMod0 <- "no individual weights, only works with mod0"
  }
  ybreaks=c(ylim[1],median(ylim),ylim[2])
  
  # plot depending on number of output units
  pMean <- ggplot(exp,aes(x=nBlock,y=actO,col=trialType)) + 
    labs(title = paste0(mod_type,"; N = ",nSim),
         subtitle = label_output,
         x = "Blocks (1 block = number of trial types)",
         y = "Mean Output Activation",
         col = "trial types:") + 
    geom_vline(xintercept = cumsum(par$nBlock)) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0, alpha = 0.1) +
    stat_summary(fun = "mean", geom="line") +
    coord_cartesian(ylim = ylim) +
    scale_y_continuous(breaks = ybreaks) +
    scale_x_continuous(breaks = cumsum(par$nBlock)) +
    theme_bw() + facet_grid(. ~ out)
  # if (nOut > 1) {pMean <- pMean + facet_grid(. ~ out)}
  
  # create individual network plots
  if (doIndPart) { # always display only the first 12 networks
    pInd <- ggplot(exp[exp$nSubj < 13,],aes(x=nBlock,y=actO,col=trialType)) + 
      labs(title = paste0(mod_type,"; N = ",nSim),
           subtitle = label_output,
           x = "Blocks (1 block = number of trial types)",
           y = "Mean Output Activation",
           col = "Trial Type:") + 
      geom_vline(xintercept = cumsum(par$nBlock)) +
      geom_line() + 
      coord_cartesian(ylim = ylim) +
      scale_y_continuous(breaks = ybreaks) +
      scale_x_continuous(breaks = cumsum(par$nBlock)) +
      facet_grid(nSubj ~ .) +
      theme_bw() + facet_grid(nSubj ~ out)
    # if (nOut > 1) {pInd <- pInd + facet_grid(nSubj ~ out)}
  } else {pInd <- "no individual network plot"}
  
  # create test plot
  if (!is.null(test)) {
    colnames(test)[grepl("actOT",colnames(test))] <- "actOT"
    pTest <- ggplot(test, aes(x=ph,y=actOT,col=trialType)) + 
      labs(title = paste0(mod_type,"; N = ",nSim),
           subtitle = label_output,
           x="Phase",y="Mean Output Activation",
           col = "Trial Type:") +
      scale_x_continuous(breaks = unique(test$ph)) +
      stat_summary(fun.data = mean_se,geom = "errorbar", width=0,
                   position = position_dodge(0.05)) +
      stat_summary(fun = "mean", geom = "point",position = position_dodge(0.05)) +
      theme_bw() +  facet_grid(. ~ out)
    # if (nOut > 1) {pTest <- pTest + facet_grid(. ~ out)}
  } else {pTest <- "no test available plot"} 
  
  # create chl_error plot
  if (!is.null(chl_error)) { # only one subject
    chl <- reshape2::melt(chl_error, measure.vars = colnames(chl_error)[grepl("out",colnames(chl_error))])
    colnames(chl)[grepl("va",colnames(chl))] <- c("out","error")
    pChl.error <- ggplot(chl[chl$nSubj == 1,],aes(x=nBlock,y=error,col=as.factor(nHid))) +
      labs(title="CHL error",subtitle = "Network 1",x="Phase",y="Error") +
      geom_line() + # facet_grid(trialType ~ .) + 
      theme_bw() + facet_grid(trialType ~ out)
    # if (nOut > 1) {pChl.error <- pChl.error + facet_grid(trialType ~ out)}
  } else {pChl.error <- "no model 4 nor 5 used"} 
  
  # plot learning rates
  if (mod_type == "mod2" | mod_type == "mod6") {
    if (nOut > 1) {temp <- exp[exp$out == unique(exp$out)[1],]; temp$out <- NULL} else {temp <- exp}
    # # # # figure for model type "mod2" (lrIH and lrHO) # # # #
    templr <- melt(temp,measure.vars = colnames(temp)[grepl("_lrIH",colnames(temp))])
    pLR.IH <- ggplot(templr, aes(x=nBlock,y=value,col=variable)) +
      labs(title="Learning Rates (LR)",
           x = "Blocks (1 block = number of trial types)",
           y="LR.IH",col="Trial Type:") +
      geom_vline(xintercept = cumsum(par$nBlock)) +
      stat_summary(geom="line") + stat_summary() +
      scale_x_continuous(breaks = cumsum(par$nBlock)) + 
      theme_bw()
    pLR.HO <- ggplot(temp, aes(x=nBlock,y=lrHO,col=trialType)) +
      labs(title="Learning Rates (LR)",x="Blocks (1 block = n trial types)",
           y="LR.HO",col="Trial Type",shape="Layer LR") +
      geom_vline(xintercept = cumsum(par$nBlock)) +
      stat_summary(geom="line") + stat_summary() +
      scale_x_continuous(breaks = cumsum(par$nBlock)) + 
      theme_bw()
  } else {pLR.IH <- "no dynamic learning rates"; pLR.HO <- "no dynamic learning rates"}
  
  # get all the plots in the return list
  return(list(pMean=pMean,pInd=pInd,pTest=pTest,pLR.IH=pLR.IH,pLR.HO=pLR.HO,
              pChl.error=pChl.error,pMeanMod0=pMeanMod0))
}

# print csv and png files of the weights for mod1 and mod2
f_printWeights2Layers <- function(weights,ph,trPh,tempPar,subj) {
  # create weights matrices
  wIH <- data.frame(weights[[ph]][[1]])
  wHO <- data.frame(weights[[ph]][[2]])
  
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
  write.csv(w,paste0(getwd(),"/weights/s",subj,"_ph",ph,".csv"))
  
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
  ggplot2::ggsave(paste0(getwd(),"/figures/s",subj,"_ph",ph,".png"),
                  plot=p,width=14,height=14,dpi=600,units="cm",limitsize=T)
}

# print csv and png files of the weights for mod1 and mod2
f_printWeightsNLayers <- function(weights,ph,trPh,tempPar,subj) {
  # simplified relevant variables
  wPh <- weights[[ph]]
  L <- length(wPh)
  # create weights matrices and name matrices
  for (k in 1:L) {
    if (k == 1) {
      rownames(wPh[[k]]) <- colnames(trPh[[ph]]$INPUT)
      colnames(wPh[[k]]) <- paste0(rep(colnames(tempPar$nHidden[k,]),tempPar$nHidden[k,]),".",
                                   1:sum(tempPar$nHidden[k,]))
      labX <- "Input"; labY <- paste("Hidden",k)
    } else if (k == L) {
      rownames(wPh[[k]]) <- colnames(wPh[[k-1]])
      colnames(wPh[[k]]) <- paste0("out.",1:ncol(data.frame(trPh[[ph]]$OUTPUT)))
      labX <- paste("Hidden",k-1); labY <- "Output"
    } else {
      rownames(wPh[[k]]) <- colnames(wPh[[k-1]])
      colnames(wPh[[k]]) <- paste0(rep(colnames(tempPar$nHidden[k,]),tempPar$nHidden[k,]),".",
                                   1:sum(tempPar$nHidden[k,]))
      labX <- paste("Hidden",k-1); labY <- paste("Hidden",k)
    }
    # print csv
    write.csv(wPh[[k]],paste0("weights/s",subj,"_ph",ph,"_k",k,".csv"))
    
    # visualize weights (Input-Hidden)
    dat <- reshape2::melt(data.frame(input=rownames(wPh[[k]]),wPh[[k]]),id.vars="input")
    dat$value[dat$value==0] <- NA
    dat$input <- factor(dat$input,levels = rownames(wPh[[k]]))
    dat$variable <- factor(dat$variable,levels = colnames(wPh[[k]]))
    pWeight <- ggplot2::ggplot(dat,aes(x=input,y=variable,fill=value)) + 
      labs(title = paste("L =",k),x = labX, y = labY, fill="Weight") +
      geom_tile() + theme_minimal() +
      scale_fill_gradient2(low="red",mid="white",high="green") +
      theme(axis.text.x = element_text(hjust=1,angle=90))
    # print plots
    ggplot2::ggsave(paste0("figures/s",subj,"_ph",ph,"_k",k,".png"),
                    plot=pWeight,width=14,height=14,dpi=600,units="cm",limitsize=T)
  } # end k loop
}

# # # # # # # # # # Neural networks models# # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# derivative of logistic activation function
f_derAct <- function(act) {act * (1 - act)}

# sigmoidal activation function
f_sigAct <- function(netin, bias = -2.2) {1 / (1 + exp(-(netin + bias)))}

# # # # # # # # # # Delamater (2004) - Learning & Behaviour # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Rescorla-Wagner Model; run one phase
f_mod0 <- function (par, training, preW = NULL) {
  ### extract parameters ###
  alpha <- par$alpha
  beta <- par$beta
  betaNR <- par$betaNR
  nBlock <- par$nBlock
  INPUT <- as.matrix(training$INPUT)
  OUTPUT <- as.matrix(training$OUTPUT)
  TEST <- as.matrix(training$TEST)
  trialType <- training$trialType
  trialTypeTest <- training$trialTypeTest
  
  ### create relevant matrices and scalars ###
  nOut <- ncol(OUTPUT) # number of outcomes
  nStim <- ncol(INPUT) # number of stimuli
  nTrialType <- nrow(INPUT) # number of trial types
  nTrialTypeTest <- nrow(TEST) # number of test trials
  nTrial <- nTrialType*nBlock # number of total trials
  
  ### creation of matrices and add initial values ###
  actOT <- matrix(NA,nrow = nTrialTypeTest, ncol = nOut) # act. output for test
  actO <- matrix(NA,nrow = nTrial, ncol = nOut) # act. output layer
  deltaO <- matrix(NA,nrow = nTrial, ncol = nOut) # delta output
  # initial values (random or previous phases)
  if (is.null(preW)) {
    W <- matrix(0, nrow=nStim, ncol=nOut)
  } else {W <- preW}
  # create matrix with all weights
  Wall <- matrix(NA, ncol=length(as.vector(W)), nrow=nTrial)
  colnames(Wall) <- paste0(rep(colnames(INPUT),nOut),"_out",
                           rep(1:nOut,each=nStim))
  
  #### learning algorithm ###
  # see also: https://web.stanford.edu/group/pdplab/pdphandbook/handbookch6.html
  for (nB in 1:nBlock) {
    # randomized trial types within a block
    randT <- sample(1:nTrialType)
    INPUT <- as.matrix(INPUT[randT,])
    OUTPUT <- as.matrix(OUTPUT[randT,])
    trialType <- trialType[randT]
    
    for (t in 1:nTrialType) {
      ### ### ### activation ### ### ### 
      # trial number
      trial <- t+((nB-1)*nTrialType)
      
      # net input as activation (no logistic transformation as backprop)
      actO[trial,] <- INPUT[t,] %*% W[,] # INPUT[t,] assumed to be a 1 x nStim matrix
      
      ### ### ### learning (weight changes) ### ######
      
      ### vector of prediction error, each element is for one output
      deltaO[trial,] <- OUTPUT[t,] - actO[trial,]
      
      # store all weights
      Wall[trial,] <- as.vector(W)
      
      # update weights input-output
      for (o in 1:nOut) { 
        if (OUTPUT[t,o] == 1) {
          W[,o] <- W[,o] + alpha * beta * (INPUT[t,] %*% t(deltaO[trial,o]))
        } else {
          W[,o] <- W[,o] + alpha * betaNR * (INPUT[t,] %*% t(deltaO[trial,o]))
        }
      }
      
    } # end trials per block cycle # t
    
    if (nB == 1) {
      trialTypeLong <- as.character(trialType)
    } else {
      trialTypeLong <- c(trialTypeLong,as.character(trialType))
    }
  } # end all blocks cycle # nB
  
  ### test input patterns ###
  if (nTrialTypeTest != 0){  # KO mm units when necessary
    actOT <- TEST %*% W[,]
    # create output list object
    colnames(actOT) <- paste0(rep("actOT.",nOut),1:nOut)
    actOT <- data.frame(trialType=trialTypeTest,actOT)
  } else {actOT <- NULL}
  
  # outputs' activations data.frame
  db <- data.frame(nTrial=1:nTrial,nBlock=rep(1:nBlock,each=nTrialType),
                   trialType=trialTypeLong,actO=actO)
  # weights valeus (associative strength; V) data.frame
  Vs <- data.frame(nTrial=1:nTrial,nBlock=rep(1:nBlock,each=nTrialType),
                   trialType=trialTypeLong,Wall)
  
  output <- list(db=db, actO=actO, W=W, actOT=actOT, Vs=Vs)
  return(output)
}

# # # # # # # # # # Delamater (2004) - Learning & Behaviour # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Backpropagation (Neural Net Model); run one phase
f_mod1 <- function (par, training, nKO_MM = 0,
                    preW.IH = NULL, preW.HO = NULL) {
  ### extract parameters ###
  alpha <- par$alpha
  beta <- par$beta
  nHid <- par$nHidden$nHV + par$nHidden$nHMM + par$nHidden$nHA
  nBlock <- par$nBlock
  INPUT <- as.matrix(training$INPUT)
  OUTPUT <- as.matrix(training$OUTPUT)
  TEST <- as.matrix(training$TEST)
  trialType <- training$trialType
  trialTypeTest <- training$trialTypeTest
  
  ### create relevant matrices and scalars ###
  nOut <- ncol(OUTPUT) # number of outcomes
  nStim <- ncol(INPUT) # number of stimuli
  nTrialType <- nrow(INPUT) # number of trial types
  nTrialTypeTest <- nrow(TEST) # number of test trials
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
  if (par$nHidden$nHA > 0 & par$nInput$vis > 0) {
    conIH[(par$nInput$ctx+1):(par$nInput$ctx + par$nInput$vis),
          (nHid-par$nHidden$nHA + 1):nHid] <- 0}
  # block auditory input with visual hidden
  if (par$nHidden$nHV > 0 & par$nInput$aud > 0) {
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
      actH[trial,] <- f_sigAct(INPUT[t,] %*% wIH) # INPUT[t,] assumed to be a 1 x nStim matrix
      actO[trial,] <- f_sigAct(actH[trial,] %*% wHO) # actH[t,] assumed to be a 1 x nOut matrix
      
      ### ### ### learning (weight changes) ### ######
      
      ### delta (error) for output
      deltaO[trial,] <- (OUTPUT[t,] - actO[trial,]) * f_derAct(actO[trial,])
      
      ### delta (error) for hidden
      deltaH[trial,] <- (deltaO[trial,] %*% t(wHO)) * f_derAct(actH[trial,])
      
      # weight change Hidden-Output
      dwHO[,] <- t(alpha * (as.matrix(deltaO[trial,]) %*% actH[trial,])) + beta*dwHO[,]
      # update weights Hidden-Output
      wHO[,] <- wHO[,] + dwHO[,]
      # weight change Input-Hidden
      dwIH[,] <- t(alpha * (as.matrix(deltaH[trial,]) %*% INPUT[t,])) + beta*dwIH[,]
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
  if (nTrialTypeTest != 0){  # KO mm units when necessary
    if (nKO_MM == 0) {
      actOT <- f_sigAct(f_sigAct(TEST %*% wIH) %*% wHO)
    } else {
      if (nKO_MM > par$nHidden$nHMM) {
        warning("nKO_MM must be smaller than nHMM")
      } else {
        KO_units <- sample((par$nHidden$nHV + 1):(nHid-par$nHidden$nHA))[1:nKO_MM]
        wIH_KO <- wIH; wIH_KO[,KO_units] <- 0
        wHO_KO <- wHO; wHO_KO[KO_units,] <- 0
        actOT <- f_sigAct(f_sigAct(TEST %*% wIH_KO) %*% wHO_KO)
      } # if error message
    } # if KO mm units
    # create output list object
    colnames(actOT) <- paste0(rep("actOT.",nOut),1:nOut)
    actOT <- data.frame(trialType=trialTypeTest,actOT)
  } else {actOT <- NULL}
  
  # outputs' activations data frame
  db <- data.frame(nTrial=1:nTrial,nBlock=rep(1:nBlock,each=nTrialType),
                   trialType=trialTypeLong,actO=actO)
  
  output <- list(db=db,actO=actO, wIH=wIH, wHO=wHO, actOT=actOT)
  return(output)
}

# # # # # # # # # # Delamater & Castiello (in preparation) # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Backpropagation dynamic alphas (Neural Net Model); run one phase
f_mod2 <- function (par,training, nKO_MM = 0,
                    preW.IH = NULL, preW.HO = NULL,
                    preLR = NULL, preLrHO = NULL) {
  ### extract parameters ###
  beta <- par$beta
  rho <- par$rho
  mu <- par$mu
  kappa <- par$kappa
  nHid <- par$nHidden$nHV + par$nHidden$nHMM + par$nHidden$nHA
  nBlock <- par$nBlock
  INPUT <- as.matrix(training$INPUT)
  OUTPUT <- as.matrix(training$OUTPUT)
  TEST <- as.matrix(training$TEST)
  trialType <- training$trialType
  trialTypeTest <- training$trialTypeTest
  
  ### create relevant matrices and scalars ###
  nOut <- ncol(OUTPUT) # number of outcomes
  nStim <- ncol(INPUT) # number of stimuli
  nTrialType <- nrow(INPUT) # number of trial types
  nTrialTypeTest <- nrow(TEST) # number of test trials
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
  if (par$nHidden$nHA > 0 & par$nInput$vis > 0) {
    conIH[(par$nInput$ctx+1):(par$nInput$ctx + par$nInput$vis),
          (nHid-par$nHidden$nHA + 1):nHid] <- 0}
  # block auditory input with visual hidden
  if (par$nHidden$nHV > 0 & par$nInput$aud > 0) {
    conIH[(par$nInput$ctx + par$nInput$vis + 1):nStim,
          1:par$nHidden$nHV] <- 0}
  wIH <- wIH * conIH
  
  #### backpropagation algorithm ###
  # see also: https://web.stanford.edu/group/pdplab/pdphandbook/handbookch6.html
  for (nB in 1:nBlock) {
    # randomized trial types within a block
    randT <-  sample(1:nTrialType)
    INPUT <- as.matrix(INPUT[randT,])
    OUTPUT <- as.matrix(OUTPUT[randT,])
    trialType <- trialType[randT]
    for (t in 1:nTrialType) {
      ### ### ### activation ### ### ### 
      # which trial
      trial <- t+((nB-1)*nTrialType)
      # forward activation # chain rule: act_fun(act_fun(trialBlock %*% wIH) %*% wHO)
      actH[trial,] <- f_sigAct(INPUT[t,] %*% wIH) # INPUT[t,] assumed to be a 1 x nStim matrix
      actO[trial,] <- f_sigAct(actH[trial,] %*% wHO) # actH[t,] assumed to be a 1 x nOut matrix
      
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
          lrIH[trial,] <- rho * outDis[trial] * INPUT[t,]
          lrHO[trial] <- mu*(1-outDis[trial])
        } else { # phase > 1
          temp <- rho*outDis[trial]*INPUT[t,] + (1-rho)*preLR[nrow(preLR),]*INPUT[t,]
          # carrying over previous learning rates of absent stimuli
          lrIH[trial,] <- temp + preLR[nrow(preLR),]*(1-INPUT[t,])
          lrHO[trial] <- mu*(1-outDis[trial])+(1-mu)*preLrHO[length(preLrHO)]
        }
      } else { # trial > 1
        # Learning Rates (LR)
        temp <- rho*outDis[trial]*INPUT[t,] + (1-rho)*lrIH[trial-1,]*INPUT[t,]
        # carrying over previous learning rates of absent stimuli
        lrIH[trial,] <- temp + lrIH[trial-1,]*(1-INPUT[t,])
        lrHO[trial] <- mu*(1-outDis[trial])+(1-mu)*lrHO[trial-1]
      }
      
      # weight change Hidden-Output
      dwHO[,] <- t(kappa*lrHO[trial]*(as.matrix(deltaO[trial,]) %*% actH[trial,])) + beta*dwHO[,]
      # update weights Hidden-Output
      wHO[,] <- wHO[,] + dwHO[,]
      
      # weight change Input-Hidden
      dwIH[,] <- (kappa*lrIH[trial,]*t(as.matrix(deltaH[trial,]) %*% INPUT[t,])) + beta*dwIH[,]
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
  if (nTrialTypeTest!=0){ # KO mm units when necessary
    if (nKO_MM == 0) {
      actOT <- f_sigAct(f_sigAct(TEST %*% wIH) %*% wHO)
    } else {
      if (nKO_MM > par$nHidden$nHMM) {
        warning("nKO_MM must be smaller than nHMM")
      } else {
        KO_units <- sample((par$nHidden$nHV + 1):(nHid-par$nHidden$nHA))[1:nKO_MM]
        wIH_KO <- wIH; wIH_KO[,KO_units] <- 0
        wHO_KO <- wHO; wHO_KO[KO_units,] <- 0
        actOT <- f_sigAct(f_sigAct(TEST %*% wIH_KO) %*% wHO_KO)
      } # if error message
    } # if KO mm units
    # create output list object
    colnames(actOT) <- paste0(rep("actOT.",nOut),1:nOut)
    actOT <- data.frame(trialType=trialTypeTest,actOT)
  } else {actOT <- NULL}
  
  # outputs' activations data frame
  db <- data.frame(nTrial=1:nTrial,nBlock=rep(1:nBlock,each=nTrialType),
                   trialType=trialTypeLong,outDis=outDis,
                   actO=actO)
  
  output <- list(db=db, actO=actO, wIH=wIH, wHO=wHO, lrIH=lrIH, lrHO=lrHO, actOT=actOT)
  return(output)
}

# # # # # # # # # # Xie & Seung (2003) - Neural Computations# # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Backpropagation (Xie & Seung, 2003)
f_mod3 <- function (par, training, preW = NULL) {
  ### extract parameters ###
  alpha <- par$alpha
  beta <- par$beta
  nHid <- rowSums(par$nHidden)
  nBlock <- par$nBlock
  L <- nrow(par$nHidden) + 1
  adaptBias <- par$adaptBias
  INPUT <- as.matrix(training$INPUT)
  OUTPUT <- as.matrix(training$OUTPUT)
  TEST <- as.matrix(training$TEST)
  trialType <- training$trialType
  trialTypeTest <- training$trialTypeTest
  
  ### create relevant matrices and scalars ###
  nOut <- ncol(OUTPUT) # number of outcomes
  nStim <- ncol(INPUT) # number of stimuli
  nTrialType <- nrow(INPUT) # number of trial types
  nTrialTypeTest <- nrow(TEST) # number of test trials
  nTrial <- nTrialType*nBlock # number of total trials
  
  ### vector with number of units ###
  nUnits <- c(nStim,nHid,nOut)
  
  ### list with weights matrices per layer k ###
  if (is.null(preW)) {
    W <- C <- list()
    for (k in 1:L) {
      W[[k]] <- matrix(runif(nUnits[k]*nUnits[k+1])-0.5,nrow = nUnits[k], ncol = nUnits[k+1])
      ### remove connections for hidden auditory and visual paths ###
      C[[k]] <- matrix(1, nrow = nUnits[k], ncol = nUnits[k+1]) # C matrix in Castiello et al (2022) - NLM
      if (k < L) {
        if (k == 1) {
          # block visual input with auditory hidden
          if (par$nHidden$nHA[k] > 0 & par$nInput$vis > 0) {
            C[[k]][(par$nInput$ctx+1):(par$nInput$ctx + par$nInput$vis),
                   (nUnits[k+1]-par$nHidden$nHA[k] + 1):nUnits[k+1]] <- 0}
          # block auditory input with visual hidden
          if (par$nHidden$nHV[k] > 0 & par$nInput$aud > 0) {
            C[[k]][(par$nInput$ctx + par$nInput$vis + 1):nUnits[k],
                   1:par$nHidden$nHV[k]] <- 0}
        } else {
          if (par$nHidden$nHA[k] > 0) { # block visual input with auditory hidden
            C[[k]][1:par$nHidden$nHV[k-1],
                   (nUnits[k+1]-par$nHidden$nHA[k] + 1):nUnits[k+1]] <- 0}
          if (par$nHidden$nHV[k] > 0) { # block auditory input with visual hidden
            C[[k]][(nUnits[k]-par$nHidden$nHV[k-1]+1):nUnits[k],
                   1:par$nHidden$nHV[k]] <- 0}
        }
      }
      W[[k]] <- W[[k]] * C[[k]]
    }
  } else {W <- preW$W; C <- preW$C}
  
  ### list with activation (act) and biases (bias) per layer k ###
  # note: inputs does not have a space in act 
  act <- bias <- D <- delta <- dW <- list()
  for (k in 1:L) {
    act[[k]] <- matrix(rep(NA,nUnits[k+1]),nrow = nUnits[k+1])
    bias[[k]] <- matrix(adaptBias,nrow=nUnits[k+1])
    dW[[k]] <- matrix(0, nrow = nUnits[k], ncol = nUnits[k+1])
  }
  
  ### run epochs or blocks ###
  for (nB in 1:nBlock) {
    # randomized trial types within a block
    randT <- sample(1:nTrialType)
    INPUT <- as.matrix(INPUT[randT,])
    OUTPUT <- as.matrix(OUTPUT[randT,])
    trialType <- trialType[randT]
    
    ### run trials within a block ###
    for (j in 1:nTrialType) {
      ## 1. in the forward pass ##
      act0 <- as.vector(INPUT[j,])
      
      ## Activation and Diagonal matrices ##
      for (k in 1:L) {
        if (k == 1) {
          act[[k]] <- f_sigAct(t(W[[k]]) %*% act0 + bias[[k]])
          D[[k]] <- diag(as.vector(f_derAct(f_sigAct(t(W[[k]]) %*% act0 + bias[[k]]))))
        } else {
          act[[k]] <- f_sigAct(t(W[[k]]) %*% act[[k-1]] + bias[[k]])
          if (nOut > 1) {
            D[[k]] <- diag(as.vector(f_derAct(f_sigAct(t(W[[k]]) %*% act[[k-1]] + bias[[k]]))))
          } else {
            D[[k]] <- diag(as.matrix(f_derAct(f_sigAct(t(W[[k]]) %*% act[[k-1]] + bias[[k]]))))
          }
        }
      } # end k
      
      ## 2. desired output d ## 
      d <- as.vector(OUTPUT[j,])
      
      ## 3. error signal (delta per layer k) ##
      # output activation
      delta[[L]] <- D[[L]] %*% (d - act[[L]])
      for (k in L:2) {
        delta[[k-1]] <- D[[k-1]] %*% W[[k]] %*% delta[[k]]
      }
      
      # 4. the weight update
      for (k in 1:L) {
        if (k == 1) { 
          dW[[k]] <- t(alpha * delta[[k]] %*% t(act0)) + beta * dW[[k]]
        } else {
          dW[[k]] <- t(alpha * delta[[k]] %*% t(act[[k-1]])) + beta * dW[[k]]
        }
        W[[k]] <- W[[k]] + dW[[k]] * C[[k]]
      } # end k layer
      
      ### accumulate activations per trial ###
      if (j == 1) {
        inpEp <- t(act0)
        outEp <- t(d)
        actOEp <- t(act[[L]])
      } else {
        inpEp <- rbind(inpEp,t(act0))
        outEp <- rbind(outEp,t(d))
        actOEp <- rbind(actOEp,t(act[[L]]))
      }
    } # end j (nTrialType)
    
    ### accumulate activations per blocks ###
    if (nB == 1) {
      inp <- inpEp
      out <- outEp
      actO <- actOEp
      trialTypeLong <- as.character(trialType)
    } else {
      inp <- rbind(inp,inpEp)
      out <- rbind(out,outEp)
      actO <- rbind(actO,actOEp)
      trialTypeLong <- c(trialTypeLong,as.character(trialType))
    }
  } # end nB blocks (epochs)
  
  ### test input patterns ###
  if (nTrialTypeTest!=0) {
    for (k in 1:L) {
      if (k == 1) {
        actOT <- f_sigAct(TEST %*% W[[k]])
      } else {
        actOT <- f_sigAct(actOT %*% W[[k]])
      }
    } # end k layer
    # create output list object
    colnames(actOT) <- paste0(rep("actOT.",nOut),1:nOut)
    actOT <- data.frame(trialType=trialTypeTest,actOT)
  } else {actOT <- NULL}
  
  # outputs' activations data frame
  db <- data.frame(nTrial=1:nTrial,nBlock=rep(1:nBlock,each=nTrialType),
                   trialType=trialTypeLong,actO=actO)
  
  output <- list(db=db, actO=actO, W=W, C=C, actOT=actOT)
  return(output)
}

# Contrastive Hebbian learning (CHL; Xie & Seung, 2003)
f_mod4 <- function (par, training, preW = NULL) {
  ### extract parameters ###
  tf <- par$tf
  dt <- par$dt
  gamma <- par$gamma
  eta <- par$eta
  nHid <- rowSums(par$nHidden)
  nBlock <- par$nBlock
  L <- nrow(par$nHidden)+1
  adaptBias <- par$adaptBias
  INPUT <- as.matrix(training$INPUT)
  OUTPUT <- as.matrix(training$OUTPUT)
  TEST <- as.matrix(training$TEST)
  trialType <- training$trialType
  trialTypeTest <- training$trialTypeTest
  
  ### create relevant matrices and scalars ###
  nOut <- ncol(OUTPUT) # number of outcomes
  nStim <- ncol(INPUT) # number of stimuli
  nTrialType <- nrow(INPUT) # number of trial types
  nTrialTypeTest <- nrow(TEST) # number of test trials
  nTrial <- nTrialType*nBlock # number of total trials
  
  ### vector with number of units ###
  nUnits <- c(nStim,nHid,nOut)
  
  ### list with weights matrices per layer k ###
  if (is.null(preW)) {
    W <- C <- list()
    for (k in 1:L) {
      W[[k]] <- matrix(runif(nUnits[k]*nUnits[k+1])-0.5,nrow = nUnits[k], ncol = nUnits[k+1])
      ### remove connections for hidden auditory and visual paths ###
      C[[k]] <- matrix(1, nrow = nUnits[k], ncol = nUnits[k+1]) # C matrix in Castiello et al (2022) - NLM
      if (k < L) {
        if (k == 1) {
          # block visual input with auditory hidden
          if (par$nHidden$nHA[k] > 0 & par$nInput$vis > 0) {
            C[[k]][(par$nInput$ctx+1):(par$nInput$ctx + par$nInput$vis),
                   (nUnits[k+1]-par$nHidden$nHA[k] + 1):nUnits[k+1]] <- 0}
          # block auditory input with visual hidden
          if (par$nHidden$nHV[k] > 0 & par$nInput$aud > 0) {
            C[[k]][(par$nInput$ctx + par$nInput$vis + 1):nUnits[k],
                   1:par$nHidden$nHV[k]] <- 0}
        } else {
          if (par$nHidden$nHA[k] > 0) { # block visual input with auditory hidden
            C[[k]][1:par$nHidden$nHV[k-1],
                   (nUnits[k+1]-par$nHidden$nHA[k] + 1):nUnits[k+1]] <- 0}
          if (par$nHidden$nHV[k] > 0) { # block auditory input with visual hidden
            C[[k]][(nUnits[k]-par$nHidden$nHV[k-1]+1):nUnits[k],
                   1:par$nHidden$nHV[k]] <- 0}
        }
      }
      W[[k]] <- W[[k]] * C[[k]]
    }
  } else {W <- preW$W; C <- preW$C}
  
  ### list with activation (act) and biases (bias) per layer k ###
  # note: inputs does not have a space in act 
  xUp <- xDo <- bias <- dW <- list()
  for (k in 1:L) {
    xUp[[k]] <- matrix(rep(f_sigAct(0),nUnits[k+1]),nrow = nUnits[k+1])
    xDo[[k]] <- matrix(rep(f_sigAct(0),nUnits[k+1]),nrow = nUnits[k+1])
    bias[[k]] <- matrix(adaptBias,nrow=nUnits[k+1])
    dW[[k]] <- matrix(0, nrow = nUnits[k], ncol = nUnits[k+1])
  }
  
  ### run epochs or blocks ###
  for (nB in 1:nBlock) {
    # randomized trial types within a block
    randT <- sample(1:nTrialType)
    INPUT <- as.matrix(INPUT[randT,])
    OUTPUT <- as.matrix(OUTPUT[randT,])
    trialType <- trialType[randT]
    
    ### run trials within a block ###
    for (j in 1:nTrialType) {
      xDo0 <- as.vector(INPUT[j,])
      xUp[[L]] <- as.vector(OUTPUT[j,])
      xUp0 <- as.vector(INPUT[j,])
      
      ## ## Contrastive Hebbian Learning ## ##
      # 1. dynamic equations # # # # # # # # # # # # # # # # # # # #
      # # # # Clamped Phase  # # # # # # # # # # # # # # # # # # # #
      # create xUp2 for visualization purposes
      xUp2 <- xUp
      for (ts in 1:tf) {
        for (k in (L-1):1) {
          if (k == 1) { # k-1 should be xUp 0
            temp <- f_sigAct( t(W[[k]])%*%xUp0 + gamma*W[[k+1]]%*%xUp[[k+1]] + bias[[k]] )
          } else { # regular k layer
            temp <- f_sigAct( t(W[[k]])%*%xUp[[k-1]] + gamma*W[[k+1]]%*%xUp[[k+1]] + bias[[k]] )
          }
          xUp[[k]] <- xUp[[k]] + dt * (-xUp[[k]] + temp)
          # accumulate activation for visualization purposes
          # xUp2[[k]] <- cbind(xUp2[[k]],xUp[[k]])
        }
      }
      # ggplot(melt(t(xUp2[[1]])),aes(x=Var1,y=value,col=as.factor(Var2))) + geom_line()
      
      # # # # Free Phase # # # # # # # # # # # # # # # # # # # #
      # create xD02 for visualization purposes
      xDo2 <- xDo
      for (ts in 1:tf) {
        for (k in 1:L) {
          if (k == 1) { # k-1 should be xDown 0
            temp <- f_sigAct( t(W[[k]])%*%xDo0 + gamma*W[[k+1]]%*%xDo[[k+1]] + bias[[k]])
          } else if (k == L) { # k+1 does not exist
            temp <- f_sigAct( t(W[[k]])%*%xDo[[k-1]] + bias[[k]])
          } else { # regular k layer
            temp <- f_sigAct( t(W[[k]])%*%xDo[[k-1]] + gamma*W[[k+1]]%*%xDo[[k+1]] + bias[[k]])
          }
          xDo[[k]] <- xDo[[k]] + dt * (-xDo[[k]] + temp)
          # accumulate activation for visualization purposes
          # xDo2[[k]] <- cbind(xDo2[[k]],xDo[[k]])
        }
      }
      # ggplot(melt(t(xDo2[[1]])),aes(x=Var1,y=value,col=as.factor(Var2))) + geom_line()
      # ggplot(melt(t(xDo2[[2]])),aes(x=Var1,y=value,col=as.factor(Var2))) + geom_line()
      
      # 4. Hebbian update # # # # # # # # # # # # # # # # # # # #
      for (k in 1:L) {
        if (k == 1) {
          dW <- eta*gamma^(k-L) * t(xUp[[k]]%*%t(xUp0) - xDo[[k]]%*%t(xDo0))
        } else {
          error <- t(xUp[[k]]%*%t(xUp[[k-1]]) - xDo[[k]]%*%t(xDo[[k-1]]))
          dW <- eta*gamma^(k-L) * error
        }
        W[[k]] <- W[[k]] + dW * C[[k]]
      }
      
      ### accumulate activations per trial ###
      if (j == 1) {
        inpEp <- t(xDo0)
        outEp <- t(xUp[[L]])
        actOEp <- t(xDo[[L]])
        # record error activation
        chl <- data.frame(trial=j,trialType=trialType[j],
                          nHid=1:nHid,error)
      } else {
        inpEp <- rbind(inpEp,t(xDo0))
        outEp <- rbind(outEp,t(xUp[[L]]))
        actOEp <- rbind(actOEp,t(xDo[[L]]))
        chl <- rbind(chl,data.frame(trial=j,trialType=trialType[j],
                                    nHid=1:nHid,error))
      }
    } # end j (nTrialType)
    
    ### accumulate activations per blocks ###
    if (nB == 1) {
      inp <- inpEp
      out <- outEp
      actO <- actOEp
      trialTypeLong <- as.character(trialType)
      chl_error <- data.frame(nBlock=nB,chl)
    } else {
      inp <- rbind(inp,inpEp)
      out <- rbind(out,outEp)
      actO <- rbind(actO,actOEp)
      trialTypeLong <- c(trialTypeLong,as.character(trialType))
      chl_error <- rbind(chl_error,data.frame(nBlock=nB,chl))
    }
  } # end nB blocks (epochs)
  
  ### test input patterns ###
  if (nTrialTypeTest!=0) {
    for (k in 1:L) {
      if (k == 1) {
        actOT <- f_sigAct(TEST %*% W[[k]])
      } else {
        actOT <- f_sigAct(actOT %*% W[[k]])
      }
    } # end k layer
    # create output list object
    colnames(actOT) <- paste0(rep("actOT.",nOut),1:nOut)
    actOT <- data.frame(trialType=trialTypeTest,actOT)
  } else {actOT <- NULL}
  # data.frame(actOT,trialTypeTest) # see if learning took place
  
  # outputs' activations data frame
  db <- data.frame(nTrial=1:nTrial,nBlock=rep(1:nBlock,each=nTrialType),
                   trialType=trialTypeLong,actO=actO)
  
  # rename output-hidden errors columns
  colnames(chl_error)[-(1:4)] <- paste0(rep("out.",nOut),1:nOut)
  output <- list(db=db, actO=actO, W=W, C=C, chl_error=chl_error,actOT=actOT)
  return(output)
}

# # # # # # # # # # Detorakis, et al. (2019) - Neural Networks# # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Contrastive Hebbian learning (CHL) with random feedback (Detorakis, et al., 2019)
f_mod5 <- function (par, training, preW = NULL) {
  ### extract parameters ###
  tf <- par$tf
  dt <- par$dt
  gamma <- par$gamma
  eta <- par$eta
  nHid <- rowSums(par$nHidden)
  nBlock <- par$nBlock
  L <- nrow(par$nHidden)+1
  adaptBias <- par$adaptBias
  INPUT <- as.matrix(training$INPUT)
  OUTPUT <- as.matrix(training$OUTPUT)
  TEST <- as.matrix(training$TEST)
  trialType <- training$trialType
  trialTypeTest <- training$trialTypeTest
  
  ### create relevant matrices and scalars ###
  nOut <- ncol(OUTPUT) # number of outcomes
  nStim <- ncol(INPUT) # number of stimuli
  nTrialType <- nrow(INPUT) # number of trial types
  nTrialTypeTest <- nrow(TEST) # number of test trials
  nTrial <- nTrialType*nBlock # number of total trials
  
  ### vector with number of units ###
  nUnits <- c(nStim,nHid,nOut)
  
  ### list with weights matrices per layer k ###
  if (is.null(preW)) {
    W <- G <- C <- list()
    for (k in 1:L) {
      # random weights matrices
      W[[k]] <- matrix(runif(nUnits[k]*nUnits[k+1])-0.5,nrow = nUnits[k], ncol = nUnits[k+1])
      # random feedback gain
      G[[k]] <- matrix(runif(nUnits[k]*nUnits[k+1])-0.5,nrow = nUnits[k], ncol = nUnits[k+1])
      ### remove connections for hidden auditory and visual paths ###
      C[[k]] <- matrix(1, nrow = nUnits[k], ncol = nUnits[k+1]) # C matrix in Castiello et al (2022) - NLM
      if (k < L) {
        if (k == 1) {
          # block visual input with auditory hidden
          if (par$nHidden$nHA[k] > 0 & par$nInput$vis > 0) {
            C[[k]][(par$nInput$ctx+1):(par$nInput$ctx + par$nInput$vis),
                   (nUnits[k+1]-par$nHidden$nHA[k] + 1):nUnits[k+1]] <- 0}
          # block auditory input with visual hidden
          if (par$nHidden$nHV[k] > 0 & par$nInput$aud > 0) {
            C[[k]][(par$nInput$ctx + par$nInput$vis + 1):nUnits[k],
                   1:par$nHidden$nHV[k]] <- 0}
        } else {
          if (par$nHidden$nHA[k] > 0) { # block visual input with auditory hidden
            C[[k]][1:par$nHidden$nHV[k-1],
                   (nUnits[k+1]-par$nHidden$nHA[k] + 1):nUnits[k+1]] <- 0}
          if (par$nHidden$nHV[k] > 0) { # block auditory input with visual hidden
            C[[k]][(nUnits[k]-par$nHidden$nHV[k-1]+1):nUnits[k],
                   1:par$nHidden$nHV[k]] <- 0}
        }
      }
      W[[k]] <- W[[k]] * C[[k]]
      G[[k]] <- G[[k]] * C[[k]]
    }
  } else {W <- preW$W; G <- preW$G; C <- preW$C}
  
  ### list with activation (xUp and xDown) and biases (bias) per layer k ###
  # note: inputs does not have a space in act 
  xUp <- xDo <- bias <- dW <- list()
  for (k in 1:L) {
    xUp[[k]] <- matrix(rep(f_sigAct(0),nUnits[k+1]),nrow = nUnits[k+1])
    xDo[[k]] <- matrix(rep(f_sigAct(0),nUnits[k+1]),nrow = nUnits[k+1])
    bias[[k]] <- matrix(adaptBias,nrow=nUnits[k+1])
    dW[[k]] <- matrix(0, nrow = nUnits[k], ncol = nUnits[k+1])
  }
  
  ### run epochs or blocks ###
  for (nB in 1:nBlock) {
    # randomized trial types within a block
    randT <- sample(1:nTrialType)
    INPUT <- as.matrix(INPUT[randT,])
    OUTPUT <- as.matrix(OUTPUT[randT,])
    trialType <- trialType[randT]
    
    ### run trials within a block ###
    for (j in 1:nTrialType) {
      xDo0 <- as.vector(INPUT[j,])
      xUp[[L]] <- as.vector(OUTPUT[j,])
      xUp0 <- as.vector(INPUT[j,])
      
      ## ## Contrastive Hebbian Learning ## ##
      # 1. dynamic equations # # # # # # # # # # # # # # # # # # # #
      # # # # Clamped Phase  # # # # # # # # # # # # # # # # # # # #
      # create xUp2 for visualization purposes
      xUp2 <- xUp # Up is clamped phase
      for (ts in 1:tf) {
        for (k in (L-1):1) {
          if (k == 1) { # k-1 should be xUp 0
            temp <- f_sigAct( t(W[[k]])%*%xUp0 + gamma*G[[k+1]]%*%xUp[[k+1]] + bias[[k]] )
          } else { # regular k layer
            temp <- f_sigAct( t(W[[k]])%*%xUp[[k-1]] + gamma*G[[k+1]]%*%xUp[[k+1]] + bias[[k]] )
          }
          xUp[[k]] <- xUp[[k]] + dt * (-xUp[[k]] + temp)
          # accumulate activation for visualization purposes
          # xUp2[[k]] <- cbind(xUp2[[k]], xUp[[k]])
        }
      }
      # ggplot(melt(t(xUp2[[1]])),aes(x=Var1,y=value,col=as.factor(Var2))) + geom_line()
      
      # # # # Free Phase # # # # # # # # # # # # # # # # # # # #
      # create xD02 for visualization purposes
      xDo2 <- xDo # Do(wn) is free phase
      for (ts in 1:tf) {
        for (k in 1:L) {
          if (k == 1) { # k-1 should be xDown 0
            temp <- f_sigAct( t(W[[k]])%*%xDo0 + gamma*G[[k+1]]%*%xDo[[k+1]] + bias[[k]])
          } else if (k == L) { # k+1 does not exist
            temp <- f_sigAct( t(W[[k]])%*%xDo[[k-1]] + bias[[k]])
          } else { # regular k layer
            temp <- f_sigAct( t(W[[k]])%*%xDo[[k-1]] + gamma*G[[k+1]]%*%xDo[[k+1]] + bias[[k]])
          }
          xDo[[k]] <- xDo[[k]] + dt * (-xDo[[k]] + temp)
          # accumulate activation for visualization purposes
          # xDo2[[k]] <- cbind(xDo2[[k]],xDo[[k]])
        }
      }
      # ggplot(melt(t(xDo2[[1]])),aes(x=Var1,y=value,col=as.factor(Var2))) + geom_line()
      # ggplot(melt(t(xDo2[[2]])),aes(x=Var1,y=value,col=as.factor(Var2))) + geom_line()
      
      # 4. Hebbian update # # # # # # # # # # # # # # # # # # # #
      for (k in 1:L) {
        if (k == 1) {
          dW <- eta*gamma^(k-L) * t(xUp[[k]]%*%t(xUp0) - xDo[[k]]%*%t(xDo0))
        } else {
          error <- t(xUp[[k]]%*%t(xUp[[k-1]]) - xDo[[k]]%*%t(xDo[[k-1]]))
          dW <- eta*gamma^(k-L) * error
        }
        W[[k]] <- W[[k]] + dW * C[[k]]
      }
      
      ### accumulate activations per trial ###
      if (j == 1) {
        inpEp <- t(xDo0)
        outEp <- t(xUp[[L]])
        actOEp <- t(xDo[[L]])
        # record error activation
        if (length(nHid) < 2) {
          chl <- data.frame(trial=j,trialType=trialType[j],
                            nHid=1:nHid,error)
        }
      } else {
        inpEp <- rbind(inpEp,t(xDo0))
        outEp <- rbind(outEp,t(xUp[[L]]))
        actOEp <- rbind(actOEp,t(xDo[[L]]))
        if (length(nHid) < 2) {
          chl <- rbind(chl,data.frame(trial=j,trialType=trialType[j],
                                      nHid=1:nHid,error))
        }
      }
    } # end j (nTrialType)
    
    ### accumulate activations per blocks ###
    if (nB == 1) {
      inp <- inpEp
      out <- outEp
      actO <- actOEp
      trialTypeLong <- as.character(trialType)
      if (length(nHid) < 2) {
        chl_error <- data.frame(nBlock=nB,chl)
      }
    } else {
      inp <- rbind(inp,inpEp)
      out <- rbind(out,outEp)
      actO <- rbind(actO,actOEp)
      trialTypeLong <- c(trialTypeLong,as.character(trialType))
      if (length(nHid) < 2) {
        chl_error <- rbind(chl_error,data.frame(nBlock=nB,chl))
      }
    }
  } # end nB blocks (epochs)
  # visualize chl errors
  # ggplot(chl,aes(x=nBlock,y=error,col=as.factor(nHid))) +
  #   geom_line() + facet_wrap(trialType~.)
  
  ### test input patterns ###
  if (nTrialTypeTest!=0) {
    for (k in 1:L) {
      if (k == 1) {
        actOT <- f_sigAct(TEST %*% W[[k]])
      } else {
        actOT <- f_sigAct(actOT %*% W[[k]])
      }
    } # end k layer
    # create output list object
    colnames(actOT) <- paste0(rep("actOT.",nOut),1:nOut)
    actOT <- data.frame(trialType=trialTypeTest,actOT)
  } else {actOT <- NULL}
  # data.frame(actOT,trialTypeTest) # see if learning took place
  
  # outputs' activations data frame
  db <- data.frame(nTrial=1:nTrial,nBlock=rep(1:nBlock,each=nTrialType),
                   trialType=trialTypeLong,actO=actO)
  
  # rename output-hidden errors columns
  if (exists("chl_error")) {
    colnames(chl_error)[-(1:4)] <- paste0(rep("out.",nOut),1:nOut)
  } else {chl_error <- NULL}

  output <- list(db=db, actO=actO, W=W, G=G, C=C, chl_error=chl_error,actOT=actOT)
  return(output)
}

# # # # # # # # # # Detorakis with adaptive (Pearce) learning rate# # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# CHL with random feedback (Detorakis, et al., 2019) and adaptive lLR  (mod2 and mod5)
f_mod6 <- function (par, training, preW = NULL,
                    preLR = NULL, preLrHO = NULL) {
  ### extract parameters ###
  tf <- par$tf
  dt <- par$dt
  gamma <- par$gamma
  eta <- par$eta
  nHid <- rowSums(par$nHidden)
  nBlock <- par$nBlock
  L <- nrow(par$nHidden)+1
  adaptBias <- par$adaptBias
  INPUT <- as.matrix(training$INPUT)
  OUTPUT <- as.matrix(training$OUTPUT)
  TEST <- as.matrix(training$TEST)
  trialType <- training$trialType
  trialTypeTest <- training$trialTypeTest
  # dynamic learning rate
  rho <- par$rho
  mu <- par$mu
  
  ### create relevant matrices and scalars ###
  nOut <- ncol(OUTPUT) # number of outcomes
  nStim <- ncol(INPUT) # number of stimuli
  nTrialType <- nrow(INPUT) # number of trial types
  nTrialTypeTest <- nrow(TEST) # number of test trials
  nTrial <- nTrialType*nBlock # number of total trials
  
  ### vector with number of units ###
  nUnits <- c(nStim,nHid,nOut)
  
  ### list with weights matrices per layer k ###
  if (is.null(preW)) {
    W <- G <- C <- list()
    for (k in 1:L) {
      # random weights matrices
      W[[k]] <- matrix(runif(nUnits[k]*nUnits[k+1])-0.5,nrow = nUnits[k], ncol = nUnits[k+1])
      # random feedback gain
      G[[k]] <- matrix(runif(nUnits[k]*nUnits[k+1])-0.5,nrow = nUnits[k], ncol = nUnits[k+1])
      ### remove connections for hidden auditory and visual paths ###
      C[[k]] <- matrix(1, nrow = nUnits[k], ncol = nUnits[k+1]) # C matrix in Castiello et al (2022) - NLM
      if (k < L) {
        if (k == 1) {
          # block visual input with auditory hidden
          if (par$nHidden$nHA[k] > 0 & par$nInput$vis > 0) {
            C[[k]][(par$nInput$ctx+1):(par$nInput$ctx + par$nInput$vis),
                   (nUnits[k+1]-par$nHidden$nHA[k] + 1):nUnits[k+1]] <- 0}
          # block auditory input with visual hidden
          if (par$nHidden$nHV[k] > 0 & par$nInput$aud > 0) {
            C[[k]][(par$nInput$ctx + par$nInput$vis + 1):nUnits[k],
                   1:par$nHidden$nHV[k]] <- 0}
        } else {
          if (par$nHidden$nHA[k] > 0) { # block visual input with auditory hidden
            C[[k]][1:par$nHidden$nHV[k-1],
                   (nUnits[k+1]-par$nHidden$nHA[k] + 1):nUnits[k+1]] <- 0}
          if (par$nHidden$nHV[k] > 0) { # block auditory input with visual hidden
            C[[k]][(nUnits[k]-par$nHidden$nHV[k-1]+1):nUnits[k],
                   1:par$nHidden$nHV[k]] <- 0}
        }
      }
      W[[k]] <- W[[k]] * C[[k]]
      G[[k]] <- G[[k]] * C[[k]]
    }
  } else {W <- preW$W; G <- preW$G; C <- preW$C}
  
  
  ### creation of matrices and add initial values ###
  outDis <- matrix(NA, nrow = nTrial) # volatile learning rate
  lrIH <- matrix(NA, nrow = nTrial, ncol = nStim) # volatile learning rate I to H
  colnames(lrIH) <- colnames(INPUT)
  lrHO <- matrix(NA, nrow = nTrial) # volatile learning rate
  
  
  ### list with activation (xUp and xDown) and biases (bias) per layer k ###
  # note: inputs does not have a space in act 
  xUp <- xDo <- bias <- dW <- list()
  for (k in 1:L) {
    xUp[[k]] <- matrix(rep(f_sigAct(0),nUnits[k+1]),nrow = nUnits[k+1])
    xDo[[k]] <- matrix(rep(f_sigAct(0),nUnits[k+1]),nrow = nUnits[k+1])
    bias[[k]] <- matrix(adaptBias,nrow=nUnits[k+1])
    dW[[k]] <- matrix(0, nrow = nUnits[k], ncol = nUnits[k+1])
  }
  
  ### run epochs or blocks ###
  for (nB in 1:nBlock) {
    # randomized trial types within a block
    randT <- sample(1:nTrialType)
    INPUT <- as.matrix(INPUT[randT,])
    OUTPUT <- as.matrix(OUTPUT[randT,])
    trialType <- trialType[randT]
    
    ### run trials within a block ###
    for (j in 1:nTrialType) {
      # xDown (free phase) at layer 0 is the same as input
      xDo0 <- as.vector(INPUT[j,])
      # xUp (clamped phase) at layer L is the same as output
      xUp[[L]] <- as.vector(OUTPUT[j,])
      # xUp (clamped phase) at layer 0 is the same as input
      xUp0 <- as.vector(INPUT[j,])
      
      # which trial
      trial <- j+((nB-1)*nTrialType)
      
      ## ## Contrastive Hebbian Learning ## ##
      # 1. dynamic equations # # # # # # # # # # # # # # # # # # # #
      
      # create xUp2 for visualization purposes
      # xUp2 <- xUp # Up is clamped phase
      # create xD02 for visualization purposes
      # xDo2 <- xDo # Do(wn) is free phase
      for (ts in 1:tf) {
        # 2. Clamped Phase # # # # # # # # # # # # # # # # # # # # #
        for (k in (L-1):1) {
          if (k == 1) { # k-1 should be xUp 0
            temp <- f_sigAct( t(W[[k]])%*%xUp0 + gamma*G[[k+1]]%*%xUp[[k+1]] + bias[[k]] )
          } else { # regular k layer
            temp <- f_sigAct( t(W[[k]])%*%xUp[[k-1]] + gamma*G[[k+1]]%*%xUp[[k+1]] + bias[[k]] )
          }
          xUp[[k]] <- xUp[[k]] + dt * (-xUp[[k]] + temp)
          # accumulate activation for visualization purposes
          # xUp2[[k]] <- cbind(xUp2[[k]], xUp[[k]])
        }
        # 3. Free Phase# # # # # # # # # # # # # # # # # # # # # # #
        for (k in 1:L) {
          if (k == 1) { # k-1 should be xDown 0
            temp <- f_sigAct( t(W[[k]])%*%xDo0 + gamma*G[[k+1]]%*%xDo[[k+1]] + bias[[k]])
          } else if (k == L) { # k+1 does not exist
            temp <- f_sigAct( t(W[[k]])%*%xDo[[k-1]] + bias[[k]])
          } else { # regular k layer
            temp <- f_sigAct( t(W[[k]])%*%xDo[[k-1]] + gamma*G[[k+1]]%*%xDo[[k+1]] + bias[[k]])
          }
          xDo[[k]] <- xDo[[k]] + dt * (-xDo[[k]] + temp)
          # accumulate activation for visualization purposes
          # xDo2[[k]] <- cbind(xDo2[[k]],xDo[[k]])
        }
      }
      # ggplot(melt(t(xUp2[[1]])),aes(x=Var1,y=value,col=as.factor(Var2))) + geom_line() +
      #   labs(subtitle = "Trial = 3; dt = 0.08; time-steps = 30; gamma = 0.02",
      #        y = "Activations", x = "Time Steps", col = "Hidden Units") + theme_classic()
      # ggplot(melt(t(xDo2[[1]])),aes(x=Var1,y=value,col=as.factor(Var2))) + geom_line() +
      #   labs(subtitle = "Trial = 1; dt = 0.08; time-steps = 30; gamma = 0.02",
      #        y = "Activations", x = "Time Steps", col = "Hidden Units") + theme_classic()
      # ggplot(melt(t(xDo2[[2]])),aes(x=Var1,y=value,col=as.factor(Var2))) + geom_line()
      
      ### alpha ###
      outDis[trial] <- mean(abs(OUTPUT[j,] - xDo[[L]]))
      if (trial == 1) {
        # Learning Rates (LR)
        if (is.null(preLR)) { # phase 1
          lrIH[trial,] <- rho * outDis[trial] * INPUT[j,]
          lrHO[trial] <- mu*(1-outDis[trial])
        } else { # phase > 1
          temp <- rho*outDis[trial]*INPUT[j,] + (1-rho)*preLR[nrow(preLR),]*INPUT[j,]
          # carrying over previous learning rates of absent stimuli
          lrIH[trial,] <- temp + preLR[nrow(preLR),]*(1-INPUT[j,])
          lrHO[trial] <- mu*(1-outDis[trial])+(1-mu)*preLrHO[length(preLrHO)]
        }
      } else { # trial > 1
        # Learning Rates (LR)
        temp <- rho*outDis[trial]*INPUT[j,] + (1-rho)*lrIH[trial-1,]*INPUT[j,]
        # carrying over previous learning rates of absent stimuli
        lrIH[trial,] <- temp + lrIH[trial-1,]*(1-INPUT[j,])
        lrHO[trial] <- mu*(1-outDis[trial])+(1-mu)*lrHO[trial-1]
      }
      
      # 4. Hebbian update # # # # # # # # # # # # # # # # # # # #
      for (k in 1:L) {
        if (k == 1) {
          dW <- lrIH[trial,]*gamma^(k-L) * t(xUp[[k]]%*%t(xUp0) - xDo[[k]]%*%t(xDo0))
          # dW <- eta * gamma^(k-L) * t(xUp[[k]]%*%t(xUp0) - xDo[[k]]%*%t(xDo0))
        } else {
          error <- t(xUp[[k]]%*%t(xUp[[k-1]]) - xDo[[k]]%*%t(xDo[[k-1]]))
          dW <- lrHO[trial,]*gamma^(k-L) * error
          # dW <- eta*gamma^(k-L) * error
        }
        W[[k]] <- W[[k]] + dW * C[[k]]
      }
      
      ### accumulate activations per trial ###
      if (j == 1) {
        inpEp <- t(xDo0)
        outEp <- t(xUp[[L]])
        actOEp <- t(xDo[[L]])
        # record error activation
        chl <- data.frame(trial=j,trialType=trialType[j],
                          nHid=1:nHid,error)
      } else {
        inpEp <- rbind(inpEp,t(xDo0))
        outEp <- rbind(outEp,t(xUp[[L]]))
        actOEp <- rbind(actOEp,t(xDo[[L]]))
        chl <- rbind(chl,data.frame(trial=j,trialType=trialType[j],
                                    nHid=1:nHid,error))
      }
    } # end j (nTrialType)
    
    ### accumulate activations per blocks ###
    if (nB == 1) {
      inp <- inpEp
      out <- outEp
      actO <- actOEp
      trialTypeLong <- as.character(trialType)
      chl_error <- data.frame(nBlock=nB,chl)
    } else {
      inp <- rbind(inp,inpEp)
      out <- rbind(out,outEp)
      actO <- rbind(actO,actOEp)
      trialTypeLong <- c(trialTypeLong,as.character(trialType))
      chl_error <- rbind(chl_error,data.frame(nBlock=nB,chl))
    }
  } # end nB blocks (epochs)
  # visualize chl errors
  # ggplot(chl,aes(x=nBlock,y=error,col=as.factor(nHid))) +
  #   geom_line() + facet_wrap(trialType~.)
  
  ### test input patterns ###
  if (nTrialTypeTest!=0) {
    for (k in 1:L) {
      if (k == 1) {
        actOT <- f_sigAct(TEST %*% W[[k]])
      } else {
        actOT <- f_sigAct(actOT %*% W[[k]])
      }
    } # end k layer
    # create output list object
    colnames(actOT) <- paste0(rep("actOT.",nOut),1:nOut)
    actOT <- data.frame(trialType=trialTypeTest,actOT)
  } else {actOT <- NULL}
  # data.frame(actOT,trialTypeTest) # see if learning took place
  
  # outputs' activations data frame
  db <- data.frame(nTrial=1:nTrial,nBlock=rep(1:nBlock,each=nTrialType),
                   trialType=trialTypeLong,actO=actO)
  
  # rename output-hidden errors columns
  colnames(chl_error)[-(1:4)] <- paste0(rep("out.",nOut),1:nOut)
  output <- list(db=db, actO=actO, W=W, G=G, C=C, chl_error=chl_error,
                 lrIH=lrIH, lrHO=lrHO, actOT=actOT)
  return(output)
}





# # # # # # # # # # For Gregynog 2024 # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
f_combine_sims <- function(par,tot) {
  # ggplot(tot, aes(x=nBlock,y=actO,col=trialType)) +
  #   stat_summary(geom = "line") +
  #   facet_grid(.~out) +
  #   theme_classic()
  # ggplot(par, aes(x=nBlock,y=actO,col=trialType)) +
  #   stat_summary(geom = "line") +
  #   facet_grid(.~out) +
  #   theme_classic()
  
  # prepare phase 2 partial reversal only reversed stimuli
  # par <- par[par$ph == 2,]
  actO.1 <- par[par$out == "actO.1",]
  actO.1$actO.1 <- actO.1$actO; actO.1$out <- NULL
  actO.2 <- par[par$out == "actO.2",]
  par <- data.frame(actO.1,actO.2=actO.2$actO)
  
  par$discScore[par$ph==1] <- ifelse(par$trialType[par$ph==1] == "A" | par$trialType[par$ph==1] == "B" |
                                       par$trialType[par$ph==1] == "C" | par$trialType[par$ph==1] == "D",
                                     par$actO.1[par$ph==1]/(par$actO.1[par$ph==1]+par$actO.2[par$ph==1]),
                                     par$actO.2[par$ph==1]/(par$actO.1[par$ph==1]+par$actO.2[par$ph==1]))
  par$discScore[par$ph==2] <- ifelse(par$trialType[par$ph==2] == "A" | par$trialType[par$ph==2] == "B" |
                                       par$trialType[par$ph==2] == "G" | par$trialType[par$ph==2] == "H",
                                     par$actO.1[par$ph==2]/(par$actO.1[par$ph==2]+par$actO.2[par$ph==2]),
                                     par$actO.2[par$ph==2]/(par$actO.1[par$ph==2]+par$actO.2[par$ph==2]))
  par$condition <- ifelse(par$trialType == "C" | par$trialType == "D" |
                            par$trialType == "G" | par$trialType == "H",
                          "reversed","nonreversed")
  # ggplot(par, aes(x=nBlock,y=actO.1/(actO.2+actO.1),col=trialType)) +
  #   stat_summary(fun.data = mean_se, geom = "errorbar", width = 0, alpha = 0.1) +
  #   stat_summary(fun = "mean", geom="line")
  # par$condition[par$ph==1] <- ifelse(par$trialType[par$ph==1] == "C" | par$trialType[par$ph==1] == "D" |
  #                                      par$trialType[par$ph==1] == "G" | par$trialType[par$ph==1] == "H",
  #                                    "reversal","non-reversal")
  # par$condition[par$ph==2] <- ifelse(par$trialType[par$ph==2] == "C" | par$trialType[par$ph==2] == "D" |
  #                                      par$trialType[par$ph==2] == "G" | par$trialType[par$ph==2] == "H",
  #                                    "reversal","non-reversal")
  
  # relevant <- t(matrix(c("C","actO.2",
  #                        "D","actO.2",
  #                        "G","actO.1",
  #                        "H","actO.1"),ncol=4)); par$relevant <- F
  # for (i in 1:nrow(relevant)) {
  #   par$relevant[par$trialType == relevant[i,1] & par$out == relevant[i,2]] <- T
  # }
  # par <- par[par$relevant == T,]; par$relevant <- NULL
  
  
  
  # prepare phase 2 total reversal
  # tot <- tot[tot$ph == 2,]
  actO.1 <- tot[tot$out == "actO.1",]
  actO.1$actO.1 <- actO.1$actO; actO.1$out <- NULL
  actO.2 <- tot[tot$out == "actO.2",]
  tot <- data.frame(actO.1,actO.2=actO.2$actO)
  
  tot$discScore[tot$ph==1] <- ifelse(tot$trialType[tot$ph==1] == "E" | tot$trialType[tot$ph==1] == "F" |
                                       tot$trialType[tot$ph==1] == "G" | tot$trialType[tot$ph==1] == "H",
                                     tot$actO.2[tot$ph==1]/(tot$actO.1[tot$ph==1]+tot$actO.2[tot$ph==1]),
                                     tot$actO.1[tot$ph==1]/(tot$actO.1[tot$ph==1]+tot$actO.2[tot$ph==1]))
  tot$discScore[tot$ph==2] <- ifelse(tot$trialType[tot$ph==2] == "E" | tot$trialType[tot$ph==2] == "F" |
                                       tot$trialType[tot$ph==2] == "G" | tot$trialType[tot$ph==2] == "H",
                                     tot$actO.1[tot$ph==2]/(tot$actO.1[tot$ph==2]+tot$actO.2[tot$ph==2]),
                                     tot$actO.2[tot$ph==2]/(tot$actO.1[tot$ph==2]+tot$actO.2[tot$ph==2]))
  tot$condition <- "reversed"
  
  # relevant <- t(matrix(c("A","actO.2",
  #                        "B","actO.2",
  #                        "C","actO.2",
  #                        "D","actO.2",
  #                        "E","actO.1",
  #                        "F","actO.1",
  #                        "G","actO.1",
  #                        "H","actO.1"),ncol=8)); tot$relevant <- F
  # for (i in 1:nrow(relevant)) {
  #   tot$relevant[tot$trialType == relevant[i,1] & tot$out == relevant[i,2]] <- T
  # }
  # tot <- tot[tot$relevant == T,]; tot$relevant <- NULL
  
  
  
  # combine both databases
  db <- rbind(par,tot)
  return(db)
}

