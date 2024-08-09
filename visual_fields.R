library(MASS)
# stimulus building

nTrials <- 200


# # # # RW (width) # # # #
# category A
# tilt and width
mu <- c(30,50)
sigma <- t(matrix(c(2.5^2,  0,
                    0, 20^2),nrow=2)) 
trialsA <- data.frame(mvrnorm(n = nTrials, mu, sigma),#, tol = 1e-6, empirical = FALSE, EISPACK = FALSE),
                      rep("A",nTrials))
colnames(trialsA) <- c("tilt","width","cat")

# category B
# tilt and width
mu <- c(70,50)
sigma <- t(matrix(c(2.5^2,  0,
                    0, 20^2),nrow=2)) 
trialsB <- data.frame(mvrnorm(n = nTrials, mu, sigma),
                      rep("B",nTrials))
colnames(trialsB) <- c("tilt","width","cat")

# combine categories
trials <- rbind(trialsA,trialsB)
trials <- trials[sample(1:nrow(trials)),]

library(ggplot2)
p_rb <- ggplot(trials, aes(x=tilt,y=width,col=cat)) + 
  labs(subtitle = "Rule-Based (RB) tilt") +
  geom_point() +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  theme_bw()



# # # # II (positive) # # # #
# category A
# tilt and width
mu <- c(70,70)
sigma <- t(matrix(c(10^2, -7^2,
                    -7^2, 10^2),nrow=2)) 
trialsA <- data.frame(mvrnorm(n = nTrials, mu, sigma),#, tol = 1e-6, empirical = FALSE, EISPACK = FALSE),
                      rep("A",nTrials))
colnames(trialsA) <- c("tilt","width","cat")

# category B
# tilt and width
mu <- c(30,30)
sigma <- t(matrix(c(8^2, -7^2,
                    -7^2, 8^2),nrow=2)) 
trialsB <- data.frame(mvrnorm(n = nTrials, mu, sigma),
                      rep("B",nTrials))
colnames(trialsB) <- c("tilt","width","cat")

# combine categories
trials <- rbind(trialsA,trialsB)
trials <- trials[sample(1:nrow(trials)),]


library(ggplot2)
p_ii <- ggplot(trials, aes(x=tilt,y=width,col=cat)) + 
  labs(subtitle = "Integration-Information (II) negative") +
  geom_point() +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  theme_bw()

ggarrange(p_rb, p_ii, common.legend = T)



# build the ann input as Turner and Wasserman (2024 - iScience)
bins <- seq(5,100,by=5)

# visual dimensions
tilt <- rep(bins,20)
width <- rep(bins,each=20)
X <- data.frame(tilt,width)

# input names
inputNames <- paste0("in.t",X$tilt,"_w",X$width)



# create csv for ALANN
OUTPUT <- trials
OUTPUT$phase <- 1
OUTPUT$matType <- "OUTPUT"
OUTPUT$trialType <- paste0(OUTPUT$cat,"_t",OUTPUT$tilt,"_w",OUTPUT$width)
INPUT <- matrix(NA,nrow = nTrials, ncol = length(inputNames))
colnames(INPUT) <- inputNames
OUTPUT <- cbind(OUTPUT,INPUT)
OUTPUT$out.1 <- ifelse(OUTPUT$cat=="A",1,0)
OUTPUT$out.2 <- 1-OUTPUT$out.1

INPUT <- OUTPUT
INPUT$matType <- "INPUT"
INPUT$out.2 <- INPUT$out.1 <- NA

# perceptual field precision
delta <- 0.8

for (t in 1:nrow(trials)) {
  # stim for one trial
  stim <- as.matrix(OUTPUT[t, c("tilt","width")])
  # stim for one trial in matrix form
  stim <- t(matrix(rep(stim, nrow(X)), ncol = nrow(X)))
  
  # estimate inputs
  K <- exp(-delta*rowSums(abs(X - stim)))
  
  # fill input trials
  INPUT[t,grepl("in.",colnames(OUTPUT))] <- K
  
  # perceptual fields
  # image(matrix(K,20))
  # library(reshape2)
  # for_plot <- melt(matrix(K,20))
  # pB <- ggplot(for_plot, aes(x = Var2,y = Var1,fill = value)) +
  #   labs(subtitle = delta, x = "width", y = "tilt") +
  #   geom_tile()
  # pB
  # library(ggpubr)
  # ggarrange(pA, pB)#, legend = "none")
}

# combine input and output, read to print
csv <- rbind(INPUT,OUTPUT)

# add nBlock
csv$par.nBlock <- c(10, rep(NA,nrow(csv)-1))
# add number of hidden units
csv$par.nH.nHV <- c(0, rep(NA,nrow(csv)-1))	
csv$par.nH.nHMM	<- c(8, rep(NA,nrow(csv)-1))
csv$par.nH.nHA <- c(0, rep(NA,nrow(csv)-1))
# add number of inputs
csv$par.nI.ctx <- c(0, rep(NA,nrow(csv)-1))
csv$par.nI.vis <- c(400, rep(NA,nrow(csv)-1))
csv$par.nI.aud <- c(0, rep(NA,nrow(csv)-1))



write.csv(csv[,4:ncol(csv)], paste0("training_files/turner&wasserman_II_positive_delta", delta, ".csv"), 
          row.names = F, na = "")



ii0.2 <- read.csv("output/exp_II_positive_delta0.2_mod1_n16_alpha0.2_beta0.9.csv")
ii0.8 <- read.csv("output/exp_II_positive_delta0.8_mod1_n16_alpha0.2_beta0.9.csv")
ii0.2$delta <- "0.2"
ii0.8$delta <- "0.8"
ii0.2$task <- ii0.8$task <- "II"

rb0.2 <- read.csv("output/exp_RB_width_delta0.2_mod1_n16_alpha0.2_beta0.9.csv")
rb0.8 <- read.csv("output/exp_RB_width_delta0.8_mod1_n16_alpha0.2_beta0.9.csv")
rb0.2$delta <- "0.2"
rb0.8$delta <- "0.8"
rb0.2$task <- rb0.8$task <- "RB"

# ii0.2$nTrial <- ii0.8$nTrial <- rb0.2$nTrial <- rb0.8$nTrial <- rep(rep(1:80,each=50*2),16)

ii0.2$cat <- substr(ii0.2$trialType,1,1)
ii0.8$cat <- substr(ii0.8$trialType,1,1)

rb0.2$cat <- substr(rb0.2$trialType,1,1)
rb0.8$cat <- substr(rb0.8$trialType,1,1)

delta <- rbind(rb0.2,rb0.8,ii0.2,ii0.8)
# delta$nTrial <- delta$nTrial * (delta$nBlock-1)

delta$correct <- ifelse(delta$cat == "A" & delta$out == "actO.1", T,
                        ifelse(delta$cat == "B" & delta$out == "actO.2", T, F))
delta <- delta[delta$correct == T,]; delta$correct <- NULL

ggplot(delta[delta$nBlock==1,], aes(x=nTrial, y=actO, col=delta)) + 
  stat_summary(geom = "line") +
  facet_grid(.~ task) +
  theme_bw()

