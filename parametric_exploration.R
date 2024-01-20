# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))

# call created functions
source("functions.R")
# fun load libraries function
f_loadLibraries()
# functions
oneCsvAnd2ndPhase <- function (trPh,par,mod_type,nSim,group="partial") {
  # weights per subj and layers (figures and csv; 1 = yes, 0 = no)
  print_weights <- 0
  
  # for loop for subjects
  for (s in 1:nSim) {
    # run all phases for one subject 
    temp <- f_runSim(par, trPh, subj = s, print_weights, mod_type)
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
  
  
  
  temp <- exp[exp$ph == 2,]
  if (group == "total") {
    # temp$condition <- "different"
    temp$category <- recode(temp$trialType, "A"="cat2", "B"="cat2", "C"="cat2", "D"="cat2",
                                            "E"="cat1", "F"="cat1", "G"="cat1", "H"="cat1")
    temp$condition <- factor("reversed",levels = c("reversed","non-reversed"))
    # ggplot(temp,aes(x=nBlock,y=actO,col=condition)) + 
    #   stat_summary(geom="line") +
    #   facet_grid(.~out)
  } else if (group == "partial") {
    temp$category <- recode(temp$trialType, "A"="cat1", "B"="cat1", "C"="cat2", "D"="cat2",
                                            "E"="cat2", "F"="cat2", "G"="cat1", "H"="cat1")
    temp$condition <- recode(temp$trialType, "A"="same", "B"="same", "C"="diff", "D"="diff",
                                             "E"="same", "F"="same", "G"="diff", "H"="diff")
    temp$condition <- factor(ifelse(temp$condition == "same","non-reversed","reversed"),
                             levels = c("reversed","non-reversed"))
    # ggplot(temp,aes(x=nBlock,y=actO,col=category,linetype=condition)) + 
    #   stat_summary(geom="line") +
    #   facet_grid(.~out)
  }
  # add temp to data.frame
  temp$group <- group
  # function output
  return(temp)
}
categoriesComparison <- function (temp) {
  # this is used to estimate differences and discrimination ratio between output units
  temp <- temp %>% group_by(group,nBlock,category,condition,out) %>%
    summarise(actO=mean(actO))
  act1 <- temp[temp$out == "actO.1",]
  act2 <- temp[temp$out == "actO.2",]
  acts <- cbind(act1[,-(5:6)],actO1=act1$actO,actO2=act2$actO)
  acts$sum <- acts$actO1 + acts$actO2
  acts$discRatio <- ifelse(acts$category=="cat1",acts$actO1/acts$sum,acts$actO2/acts$sum)
  acts$diffCorMinInc <- ifelse(acts$category=="cat1",acts$actO1-acts$actO2,acts$actO2-acts$actO1)
  acts$condition <- factor(acts$condition,levels = c("reversed","non-reversed"))
  return(acts)
}

# select and read training and parameters csv file
train <- read.csv("Category Reversal.csv")
# prepare data
dataReady <- f_prepData(train)
# parameter
par_total <- dataReady$par
# training phases
trPh_total <- dataReady$trPh

# select and read training and parameters csv file
train <- read.csv("Category Partial Reversal.csv")
# prepare data
dataReady <- f_prepData(train)
# parameter
par_partial <- dataReady$par
# training phases
trPh_partial <- dataReady$trPh
remove(dataReady)

# model
mod_type <- "mod4"
# how many simulated subjects?
nSim <- 16

# # # # Parameters # # # #

## ## ## Mod 2 ## ## ##
if (mod_type == "mod2") { # beta 0.9, rho = 0.05, mu = 0.01
  beta <- 0.9
  rho <- c(0.01,0.05,0.25,1)
  mu <- c(0.01,0.05,0.25,1)
  label_output <- paste0("beta",par_total$beta,"_rho",par_total$rho,"_mu",par_total$mu)
  
  for (i in 1:length(beta)) {
    for (j in 1:length(rho)) {
      for (k in 1:length(mu)) {
        par_partial$beta <- par_total$beta <- beta[i]
        # rho and mu free parameters (smooth learning rate change; Kaye & Pearce, 1984)
        par_partial$rho <- par_total$rho <- rho[j] # rho (p) is for weights between input to hidden 
        par_partial$mu <- par_total$mu <- mu[k]# mu (m) is for weights between hidden to output
        label_output <- paste0("beta",beta[i],"_rho",rho[j],"_mu",mu[k])
        
        
        # simulate total
        total <- oneCsvAnd2ndPhase(trPh = trPh_total, par = par_total, mod_type,
                                   nSim, group = "total")
        # ggplot(total,aes(x=nBlock,y=actO,col=category,linetype=condition)) +
        #   stat_summary(geom = "line") + facet_grid(group~out) + theme_classic()
        total2 <- categoriesComparison(total)
        
        
        # simulate partial
        partial <- oneCsvAnd2ndPhase(trPh = trPh_partial, par = par_partial, mod_type,
                                     nSim, group = "partial")
        # ggplot(partial,aes(x=nBlock,y=actO,col=category,linetype=condition)) +
        #   stat_summary(geom = "line") + facet_grid(group~out) + theme_classic()
        partial2 <- categoriesComparison(partial)
        
        
        # pool groups
        pooled <- rbind(total,partial)
        pooled2 <- rbind(total2,partial2)
        
        # save RData
        # save(pooled,file = paste0("simulations/pooled_",mod_type,"_n",nSim,"_",
        #                           label_output,".RData"))
        # get plot
        # plot1 <- ggplot(pooled ,aes(x=nBlock,y=actO,col=group,linetype=condition)) + 
        #   labs(subtitle = paste0(mod_type,"_n",nSim,"_",label_output),
        #        y="Correct Output Activation", x = "Number of Blocks") +
        #   geom_hline(yintercept = 0.5, alpha = 0.5, col = "grey") +
        #   scale_y_continuous(breaks = c(0.1,0.5,0.9)) + 
        #   stat_summary(geom = "line") +
        #   facet_grid(out~category) +
        #   theme_classic()
        # # plot1
        # plot2 <- ggplot(pooled2 ,aes(x=nBlock,y=discRatio,col=group,linetype=condition)) + 
        #   labs(subtitle = paste0(mod_type,"_n",nSim,"_",label_output),
        #        y = expression(act[correct]/(act[correct]+act[incorrect])), 
        #        x = "Number of Blocks") +
        #   geom_hline(yintercept = 0.5, alpha = 0.5, col = "grey") +
        #   scale_y_continuous(breaks = c(0.1,0.5,0.9)) + 
        #   stat_summary(geom = "line") +
        #   facet_grid(.~category) +
        #   theme_classic()
        # # plot2
        # plot3 <- ggplot(pooled2 ,aes(x=nBlock,y=diffCorMinInc,col=group,linetype=condition)) + 
        #   labs(subtitle = paste0(mod_type,"_n",nSim,"_",label_output),
        #        y = expression(act[correct]-act[incorrect]), 
        #        x = "Number of Blocks") +
        #   geom_hline(yintercept = 0, alpha = 0.5, col = "grey") +
        #   scale_y_continuous(breaks = c(-0.9,0,0.9),limits = c(-1,1)) + 
        #   stat_summary(geom = "line") +
        #   facet_grid(.~category) +
        #   theme_classic()
        # plot3
        plot4 <- ggplot(pooled2 ,aes(x=nBlock,y=diffCorMinInc,col=group,linetype=condition)) + 
          labs(subtitle = paste0(mod_type,"_n",nSim,"_",label_output),
               y = expression(act[correct]-act[incorrect]), 
               x = "Number of Blocks") +
          geom_hline(yintercept = 0, alpha = 0.5, col = "grey") +
          scale_y_continuous(breaks = c(-0.9,0,0.9),limits = c(-1,1)) + 
          stat_summary(geom = "line") +
          theme_classic()
        # plot4
        
        # save plot
        # ggsave(paste0("figures/explore1_",mod_type,"_n",nSim,"_",label_output,"_out",
        #               length(unique(pooled$out)),".png"), dpi = 300, limitsize = TRUE,
        #        plot = plot1,
        #        units = "px", # "cm", "in"
        #        width = 1600,
        #        height = 1200)
        # ggsave(paste0("figures/explore2_",mod_type,"_n",nSim,"_",label_output,"_out",
        #               length(unique(pooled$out)),".png"), dpi = 300, limitsize = TRUE,
        #        plot = plot2,
        #        units = "px", # "cm", "in"
        #        width = 1600,
        #        height = 1200)
        # ggsave(paste0("figures/explore3_",mod_type,"_n",nSim,"_",label_output,"_out",
        #               length(unique(pooled$out)),".png"), dpi = 300, limitsize = TRUE,
        #        plot = plot3,
        #        units = "px", # "cm", "in"
        #        width = 1600,
        #        height = 1200)
        ggsave(paste0("figures/explore4_",mod_type,"_n",nSim,"_",label_output,"_out",
                      length(unique(pooled$out)),".png"), dpi = 300, limitsize = TRUE,
               plot = plot4,
               units = "px", # "cm", "in"
               width = 1200,
               height = 800)
      }# end mu
    }# end rho
  }# end beta
}

## ## ## Mod 4 and 5 ## ## ##
if (mod_type == "mod4" | mod_type == "mod5" | mod_type == "mod6") {
  tf <- 20 #        dynamic equation time = 30
  dt <- 0.3 #       time step = 0.08
  adaptBias <- 0 #  
  gamma <- c(0.05,0.5,0.95) #    feedback gain factor = 0.05
  eta <- c(0.1,0.5,0.9) #      learning rate = 0.1
  
  for (i in 1:length(dt)) {
    for (j in 1:length(gamma)) {
      for (k in 1:length(eta)) {
        par_partial$tf <- par_total$tf <- tf
        par_partial$dt <- par_total$dt <- dt[i]
        par_partial$adaptBias <- par_total$adaptBias <- adaptBias
        par_partial$gamma <- par_total$gamma <- gamma[j]
        par_partial$eta <- par_total$eta <- eta[k]
        label_output <- paste0("tf",tf,"_dt",dt[i],"_gamma",gamma[j],
                               "_eta",eta[k],"_L",nrow(par_partial$nHidden))
        
        
        # simulate total
        total <- oneCsvAnd2ndPhase(trPh = trPh_total, par = par_total, mod_type,
                                   nSim, group = "total")
        # ggplot(total,aes(x=nBlock,y=actO,col=category,linetype=condition)) +
        #   stat_summary(geom = "line") + facet_grid(group~out) + theme_classic()
        total2 <- categoriesComparison(total)
        
        
        # simulate partial
        partial <- oneCsvAnd2ndPhase(trPh = trPh_partial, par = par_partial, mod_type,
                                     nSim, group = "partial")
        # ggplot(partial,aes(x=nBlock,y=actO,col=category,linetype=condition)) +
        #   stat_summary(geom = "line") + facet_grid(group~out) + theme_classic()
        partial2 <- categoriesComparison(partial)
        
        
        # pool groups
        pooled <- rbind(total,partial)
        pooled2 <- rbind(total2,partial2)
    
        # save RData
        # save(pooled,file = paste0("simulations/pooled_",mod_type,"_n",nSim,"_",
        #                           label_output,".RData"))
        # get plot
        # plot1 <- ggplot(pooled ,aes(x=nBlock,y=actO,col=group,linetype=condition)) + 
        #   labs(subtitle = paste0(mod_type,"_n",nSim,"_",label_output),
        #        y="Correct Output Activation", x = "Number of Blocks") +
        #   geom_hline(yintercept = 0.5, alpha = 0.5, col = "grey") +
        #   scale_y_continuous(breaks = c(0.1,0.5,0.9)) + 
        #   stat_summary(geom = "line") +
        #   facet_grid(out~category) +
        #   theme_classic()
        # # plot1
        # plot2 <- ggplot(pooled2 ,aes(x=nBlock,y=discRatio,col=group,linetype=condition)) + 
        #   labs(subtitle = paste0(mod_type,"_n",nSim,"_",label_output),
        #        y = expression(act[correct]/(act[correct]+act[incorrect])), 
        #        x = "Number of Blocks") +
        #   geom_hline(yintercept = 0.5, alpha = 0.5, col = "grey") +
        #   scale_y_continuous(breaks = c(0.1,0.5,0.9)) + 
        #   stat_summary(geom = "line") +
        #   facet_grid(.~category) +
        #   theme_classic()
        # # plot2
        # plot3 <- ggplot(pooled2 ,aes(x=nBlock,y=diffCorMinInc,col=group,linetype=condition)) + 
        #   labs(subtitle = paste0(mod_type,"_n",nSim,"_",label_output),
        #        y = expression(act[correct]-act[incorrect]), 
        #        x = "Number of Blocks") +
        #   geom_hline(yintercept = 0, alpha = 0.5, col = "grey") +
        #   scale_y_continuous(breaks = c(-0.9,0,0.9),limits = c(-1,1)) + 
        #   stat_summary(geom = "line") +
        #   facet_grid(.~category) +
        #   theme_classic()
        # plot3
        plot4 <- ggplot(pooled2 ,aes(x=nBlock,y=diffCorMinInc,col=group,linetype=condition)) + 
          labs(subtitle = paste0(mod_type,"_n",nSim,"_",label_output),
               y = expression(act[correct]-act[incorrect]), 
               x = "Number of Blocks") +
          geom_hline(yintercept = 0, alpha = 0.5, col = "grey") +
          scale_y_continuous(breaks = c(-0.9,0,0.9),limits = c(-1,1)) + 
          stat_summary(geom = "line") +
          theme_classic()
        # plot4
        
        # save plot
        # ggsave(paste0("figures/explore1_",mod_type,"_n",nSim,"_",label_output,"_out",
        #               length(unique(pooled$out)),".png"), dpi = 300, limitsize = TRUE,
        #        plot = plot1,
        #        units = "px", # "cm", "in"
        #        width = 1600,
        #        height = 1200)
        # ggsave(paste0("figures/explore2_",mod_type,"_n",nSim,"_",label_output,"_out",
        #               length(unique(pooled$out)),".png"), dpi = 300, limitsize = TRUE,
        #        plot = plot2,
        #        units = "px", # "cm", "in"
        #        width = 1600,
        #        height = 1200)
        # ggsave(paste0("figures/explore3_",mod_type,"_n",nSim,"_",label_output,"_out",
        #               length(unique(pooled$out)),".png"), dpi = 300, limitsize = TRUE,
        #        plot = plot3,
        #        units = "px", # "cm", "in"
        #        width = 1600,
        #        height = 1200)
        ggsave(paste0("figures/explore4_",mod_type,"_n",nSim,"_",label_output,"_out",
                      length(unique(pooled$out)),".png"), dpi = 300, limitsize = TRUE,
               plot = plot4,
               units = "px", # "cm", "in"
               width = 1200,
               height = 800)
      }# end mu
    }# end rho
  }# end beta
}



