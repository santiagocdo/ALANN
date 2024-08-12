# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))

# libraries
library(MASS)
library(ggplot2)
library(ggpubr)

# number of trials per category
nTrials <- 200

# multivariate normal distribution parameters

# mean category A (tilt and width)
muA <- c(30,50)
# mean category B (tilt and width)
muB <- c(70,50)
# variances (or SD)
sigma <- t(matrix(c(2.5^2,    0,
                        0, 20^2),nrow=2)) 

# category A
trialsA <- mvrnorm(n = nTrials, muA, sigma)#, tol = 1e-6, empirical = FALSE, EISPACK = FALSE),

# category B
trialsB <- mvrnorm(n = nTrials, muB, sigma)


# # # # # # # # RB (tilt) # # # # # # # # ####
# combine categories
trials_rb_t <- data.frame(rbind(trialsA,trialsB),c(rep("A",nTrials),rep("B",nTrials)))

# column names
colnames(trials_rb_t) <- c("tilt","width","cat")

# create test labels only for tilt and positive
test_t <- rep(NA, nrow(trials_rb_t))
# interference
test_t <- ifelse((trials_rb_t$tilt > 60 & trials_rb_t$tilt < 80 &
                  trials_rb_t$width > 20 & trials_rb_t$width < 40) |
                 (trials_rb_t$tilt > 20 & trials_rb_t$tilt < 40 &
                    trials_rb_t$width > 60 & trials_rb_t$width < 80), "Interference", test_t)
# new
test_t <- ifelse((trials_rb_t$tilt > 60 & trials_rb_t$tilt < 80 &
                  trials_rb_t$width > 40 & trials_rb_t$width < 60) |
                 (trials_rb_t$tilt > 20 & trials_rb_t$tilt < 40 &
                    trials_rb_t$width > 40 & trials_rb_t$width < 60), "New", test_t)
# transfer
test_t <- ifelse((trials_rb_t$tilt > 60 & trials_rb_t$tilt < 80 &
                  trials_rb_t$width > 60 & trials_rb_t$width < 80) |
                 (trials_rb_t$tilt > 20 & trials_rb_t$tilt < 40 &
                    trials_rb_t$width > 20 & trials_rb_t$width < 40), "Transfer", test_t)

# visualization
p_rb_t <- ggplot(trials_rb_t, aes(x=width,y=tilt,col=cat)) + 
  labs(subtitle = "Rule-Based (RB) - tilt") +
  geom_point() +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  theme_bw()
# p_rb_t



# # # # # # # # RB (width) # # # # # # # # ####
# Define the rotation angle (90 degrees), theta is in radians
theta <- pi / 2

# Create the rotation matrix
rotation_matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), ncol = 2)

# combine categories
trials_rb_w <- rbind(trialsA,trialsB)

# subtract 50 to all (center to 0)
trials_rb_w <- trials_rb_w - 50

# multiply by rotation matrix
trials_rb_w <- trials_rb_w %*% rotation_matrix

# recenter to 50
trials_rb_w <- trials_rb_w + 50

# add category
trials_rb_w <- data.frame(trials_rb_w,c(rep("A",nTrials),rep("B",nTrials)))

# column names
colnames(trials_rb_w) <- c("tilt","width","cat")

# visualization
p_rb_w <- ggplot(trials_rb_w, aes(x=width,y=tilt,col=cat)) + 
  labs(subtitle = "Rule-Based (RB) - width") +
  geom_point() +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  theme_bw()
# p_rb_w



# # # # # # # # II (positive) # # # # # # # # ####
# Define the rotation angle (45 degrees), theta is in radians
theta <- 3*pi / 4

# Create the rotation matrix
rotation_matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), ncol = 2)

# combine categories
trials_ii_p <- rbind(trialsA,trialsB)

# subtract 50 to all (center to 0)
trials_ii_p <- trials_ii_p - 50

# multiply by rotation matrix
trials_ii_p <- trials_ii_p %*% rotation_matrix

# recenter to 50
trials_ii_p <- trials_ii_p + 50

# add category
trials_ii_p <- data.frame(trials_ii_p,c(rep("A",nTrials),rep("B",nTrials)))

# create test labels for positive
test_p <- test_t 
test_p[test_p=="Transfer"] <- "Int"
test_p[test_p=="Interference"] <- "Transfer"
test_p[test_p=="Int"] <- "Interference"

# column names
colnames(trials_ii_p) <- c("tilt","width","cat")

# visualization
p_ii_p <- ggplot(trials_ii_p, aes(x=width,y=tilt,col=cat)) + 
  labs(subtitle = "Integrated-Information (II) - positive") +
  geom_point() +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  theme_bw()
# p_ii_p



# # # # # # # # II (negative) # # # # # # # # ####
# plot squares must to be declared here, because we must rotate it: plot_squares_rot
plot_squares_rot <- data.frame(
  #   interference, new, transfer
  x = c(30, 70, 50, 50, 70, 30),
  y = c(70, 30, 70, 30, 70, 30),
  width = c(20, 20, 20, 20, 20, 20),
  height = c(20, 20, 20, 20, 20, 20),
  fill_color = c("purple", "purple", "yellow","yellow","green","green")
)
# Define the rotation angle (45 degrees), theta is in radians
theta <- pi / 4

# Create the rotation matrix
rotation_matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), ncol = 2)

# combine categories
trials_ii_n <- rbind(trialsA,trialsB)

# subtract 50 to all (center to 0)
trials_ii_n <- trials_ii_n - 50
plot_squares_rot[,1:2] <- plot_squares_rot[,1:2] - 50

# multiply by rotation matrix
trials_ii_n <- trials_ii_n %*% rotation_matrix
plot_squares_rot[,1:2] <- as.matrix(plot_squares_rot[,1:2]) %*% rotation_matrix

# recenter to 50
trials_ii_n <- trials_ii_n + 50
plot_squares_rot[,1:2] <- plot_squares_rot[,1:2] + 50

# add category
trials_ii_n <- data.frame(trials_ii_n,c(rep("A",nTrials),rep("B",nTrials)))

# column names
colnames(trials_ii_n) <- c("tilt","width","cat")

# visualization
p_ii_n <- ggplot(trials_ii_n, aes(x=width,y=tilt,col=cat)) + 
  labs(subtitle = "Integrated-Information (II) - negative") +
  geom_point() +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  theme_bw()
# p_ii_n



# # # # # # # # Combine 4 tasks # # # # # # # # ####
# ggarrange(p_rb_t, p_rb_w, p_ii_p, p_ii_n, 
#           labels = letters[1:4], common.legend = T)



# # # # # # # # randomize categories within task # # # # # # # # ####
# add test label
trials_rb_t$test <- test_t
# randomize
trials_rb_t <- trials_rb_t[sample(1:nrow(trials_rb_t)),]
# add trial count per category
trials_rb_t$trial_count <- NA 
trials_rb_t$trial_count[trials_rb_t$cat == "A"] <- 
  trials_rb_t$trial_count[trials_rb_t$cat == "B"] <- 1:nTrials
# which task
trials_rb_t$task <- "rbt"
trials_rb_t$phase <- 2

# add test label
trials_rb_w$test <- NA
# randomize
trials_rb_w <- trials_rb_w[sample(1:nrow(trials_rb_w)),]
# add trial count per category
trials_rb_w$trial_count <- NA 
trials_rb_w$trial_count[trials_rb_w$cat == "A"] <- 
  trials_rb_w$trial_count[trials_rb_w$cat == "B"] <- 1:nTrials
# which task
trials_rb_w$task <- "rbw"
trials_rb_w$phase <- 1

# add test label
trials_ii_p$test <- test_p
# randomize
trials_ii_p <- trials_ii_p[sample(1:nrow(trials_ii_p)),]
# add trial count per category
trials_ii_p$trial_count <- NA 
trials_ii_p$trial_count[trials_ii_p$cat == "A"] <- 
  trials_ii_p$trial_count[trials_ii_p$cat == "B"] <- 1:nTrials
# which task
trials_ii_p$task <- "iip"
trials_ii_p$phase <- 2

# add test label
trials_ii_n$test <- NA
# randomize
trials_ii_n <- trials_ii_n[sample(1:nrow(trials_ii_n)),]
# add trial count per category
trials_ii_n$trial_count <- NA 
trials_ii_n$trial_count[trials_ii_n$cat == "A"] <- 
  trials_ii_n$trial_count[trials_ii_n$cat == "B"] <- 1:nTrials
# which task
trials_ii_n$task <- "iin"
trials_ii_n$phase <- 1


# combine II and RB
trials_rb <- rbind(trials_rb_w, trials_rb_t)
trials_ii <- rbind(trials_ii_n, trials_ii_p)





# trials_rb$labels <- as.factor(paste0(trials_rb$cat,"_",trials_rb$task))
plot_squares <- data.frame(
  #   interference, new, transfer
  x = c(30, 70, 50, 50, 70, 30),
  y = c(70, 30, 70, 30, 70, 30),
  width = c(20, 20, 20, 20, 20, 20),
  height = c(20, 20, 20, 20, 20, 20),
  fill_color = c("green","green","yellow","yellow","purple", "purple")
)

# visualization
trials_rb$labels <- factor(paste(trials_rb$phase,trials_rb$cat),
                           levels = c("1 A", "2 A", "1 B", "2 B"))
levels(trials_rb$labels) <- c("A - Training", "A - Test","B - Training", "B - Test")
p_rb <- ggplot() + 
  labs(title = "Rule-Based (RB)", col = "Category", alpha = "Phase",
       x = "Width OR Frequency", y="Tilt OR Orientation") +
  geom_tile(data = plot_squares, aes(x = x, y = y, width = width, 
                                     height = height, fill = fill_color),
            alpha = 0.2, color = "black", size = 0.5) +
  scale_fill_identity() +
  geom_point(data = trials_rb, aes(x=width, y=tilt, col = labels),
             shape = 16, size = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("red","salmon","blue","skyblue")) +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  theme_bw()
# p_rb

# Function to calculate the coordinates of a rotated square
get_rotated_square <- function(x_center, y_center, size, angle) {
  # Convert angle to radians
  angle_rad <- angle * pi / 180
  
  # Calculate the corners of the square without rotation
  x_offsets <- c(-size/2, size/2, size/2, -size/2)
  y_offsets <- c(-size/2, -size/2, size/2, size/2)
  
  # Apply the rotation matrix
  x_rotated <- x_center + x_offsets * cos(angle_rad) - y_offsets * sin(angle_rad)
  y_rotated <- y_center + x_offsets * sin(angle_rad) + y_offsets * cos(angle_rad)
  
  data.frame(x = x_rotated, y = y_rotated)
}
# for looped the 6 squares
for (i in 1:nrow(plot_squares_rot)) {
  # Example data: create two rotated squares
  square <- get_rotated_square(x_center = plot_squares_rot$x[i], y_center = plot_squares_rot$y[i], 
                               size = 20, angle = 45)
  square$id <- i  # ID to group corners of the first square
  square$fill_color <- plot_squares_rot$fill_color[i]
  if (i == 1) {
    squares <- square
  } else {
    squares <- rbind(squares,square)
  }
}

trials_ii$labels <- factor(paste(trials_ii$phase,trials_ii$cat),
                           levels = c("1 A", "2 A", "1 B", "2 B"))
levels(trials_ii$labels) <- c("A - Training", "A - Test","B - Training", "B - Test")
p_ii <- ggplot() + 
  labs(title = "Integrated-Information (II)", col = "Category", alpha = "Phase",
       x = "Width OR Frequency", y="Tilt OR Orientation") +
  geom_polygon(data = squares, aes(x = x, y = y, group = id, fill = fill_color), 
               color = "black", alpha = 0.2, size = 0.5) +
  scale_fill_identity() +
  geom_point(data = trials_ii, aes(x=width, y=tilt, col=labels),
             shape = 16, size = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("red","salmon","blue","skyblue")) +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  theme_bw()
# p_ii
ggarrange(p_rb, p_ii, common.legend = T)


# ggarrange(ggarrange(p_rb_t, p_rb_w, p_ii_p, p_ii_n, 
#                     labels = letters[1:4], common.legend = T),
#           ggarrange(p_rb,p_ii, labels = letters[5:6], common.legend = T),nrow=2)



####










# build the ann input as Turner and Wasserman (2024 - iScience)
bins <- seq(5,100,by=5)

# visual dimensions
tilt <- rep(bins,20)
width <- rep(bins,each=20)
X <- data.frame(tilt,width)

# input names
inputNames <- paste0("in.t",X$tilt,"_w",X$width)

# which task?
trials <- trials_rb # trials_rb OR trials_ii


# create csv for ALANN
OUTPUT <- trials
OUTPUT$phase <- rep(1:2, each=nTrials*2)
OUTPUT$matType <- "OUTPUT"
OUTPUT$trialType <- paste0(OUTPUT$cat,"_",OUTPUT$task,"_",OUTPUT$trial_count,
                           "_",OUTPUT$test,"_t",OUTPUT$tilt,"_w",OUTPUT$width)
INPUT <- matrix(NA,nrow = nTrials, ncol = length(inputNames))
colnames(INPUT) <- inputNames
OUTPUT <- cbind(OUTPUT,INPUT)
OUTPUT$out.1 <- ifelse(OUTPUT$cat=="A",1,0)
OUTPUT$out.2 <- 1-OUTPUT$out.1

INPUT <- OUTPUT
INPUT$matType <- "INPUT"
INPUT$out.2 <- INPUT$out.1 <- NA

# perceptual field precision
delta <- 0.2

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
}

# combine input and output, read to print
csv <- rbind(INPUT,OUTPUT)

# add nBlock
csv$par.nBlock <- c(1,1, rep(NA,nrow(csv)-2))
# add number of hidden units
csv$par.nH.nHV <- c(0, rep(NA,nrow(csv)-1))	
csv$par.nH.nHMM	<- c(8, rep(NA,nrow(csv)-1))
csv$par.nH.nHA <- c(0, rep(NA,nrow(csv)-1))
# add number of inputs
csv$par.nI.ctx <- c(0, rep(NA,nrow(csv)-1))
csv$par.nI.vis <- c(400, rep(NA,nrow(csv)-1))
csv$par.nI.aud <- c(0, rep(NA,nrow(csv)-1))

# print csv
write.csv(csv[,7:ncol(csv)], paste0("training_files/turner&wasserman_RB_delta", delta, ".csv"), 
          row.names = F, na = "")








# read simulations' activations
ii <- read.csv("output/exp_II_delta0.2_mod1_n16_alpha0.2_beta0.9.csv")
rb <- read.csv("output/exp_RB_delta0.2_mod1_n16_alpha0.2_beta0.9.csv")

# split trialType, but is a list per row
ii_names <- strsplit(ii$trialType, split = "_")
rb_names <- strsplit(rb$trialType, split = "_")

# how many splits per list space
max_splits <- max(sapply(ii_names, length))

# transform that list into a column
ii_split <- do.call(rbind, lapply(ii_names, function(x) c(x, rep(NA, max_splits - length(x)))))
rb_split <- do.call(rbind, lapply(rb_names, function(x) c(x, rep(NA, max_splits - length(x)))))

# column names of the new matrices
colnames(ii_split) <- colnames(rb_split) <- c("cat","task","trial_count","test","tilt","width")

# Add split columns to the original data frame
ii <- cbind(ii, ii_split) 
rb <- cbind(rb, rb_split) 

# trial count to integers
ii$trial_count <- as.integer(ii$trial_count)
rb$trial_count <- as.integer(rb$trial_count)

# adjust nTrial and trial_count by phase
ii$trial_count <- ii$trial_count + (ii$ph-1)*200 
rb$trial_count <- rb$trial_count + (rb$ph-1)*200
ii$nTrial <- ii$nTrial + (ii$ph-1)*400
rb$nTrial <- rb$nTrial + (rb$ph-1)*400

# create bins for x-axis
bin <- 20
ii$nBlockArt <- rep(rep(rep(1:(max(ii$nTrial)/bin),each=bin),2),length(unique(ii$nSubj)))
rb$nBlockArt <- rep(rep(rep(1:(max(rb$nTrial)/bin),each=bin),2),length(unique(rb$nSubj)))

# change order of factors 
ii$test <- factor(ii$test, levels = c("NA","Interference","New","Transfer"))
rb$test <- factor(rb$test, levels = c("NA","Interference","New","Transfer"))

# raw visualization
p_ii_2 <- ggplot(ii, aes(x=nBlockArt, y=actO, group=test, fill=test)) +
  geom_vline(xintercept = 40.5, col = "black") +
  labs(title = "II: Negative --> Positive",
       x = "10-Trial Blocks", y = "Activations") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point", shape = 21, col = "black", size = 3,  alpha = 0.5) +
  facet_grid(cat ~ out) + 
  scale_fill_manual(values = c("black","green","yellow","blue")) +
  theme_bw()
p_rb_2 <- ggplot(rb, aes(x=nBlockArt, y=actO, group=test, fill=test)) +
  geom_vline(xintercept = 40.5, col = "black") +
  labs(title = "RB: Width --> Tilt",
       x = "10-Trial Blocks", y = "Activations") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point", shape = 21, col = "black", size = 3, alpha = 0.5) +
  facet_grid(cat ~ out) + 
  scale_fill_manual(values = c("black","green","yellow","blue")) +
  theme_bw()
ggarrange(p_ii_2, p_rb_2, common.legend = T)



# create correct index for II
temp <- ii[ii$out == "actO.1",]
colnames(temp)[grepl("actO",colnames(temp))] <- "actO.1"
ii <- data.frame(temp,actO.2=ii$actO[ii$out == "actO.2"])
ii$actO <- ii$actO.1 / (ii$actO.1 + ii$actO.2)
ii$actO <- ifelse(ii$cat == "A", ii$actO, 1-ii$actO)
# create correct index for RB
temp <- rb[rb$out == "actO.1",]
colnames(temp)[grepl("actO",colnames(temp))] <- "actO.1"
rb <- data.frame(temp,actO.2=rb$actO[rb$out == "actO.2"])
rb$actO <- rb$actO.1 / (rb$actO.1 + rb$actO.2)
rb$actO <- ifelse(rb$cat == "A", rb$actO, 1-rb$actO)

p_ii_2 <- ggplot(ii[ii$test != "NA",], aes(x=nBlockArt, y=actO, group=test, fill=test)) +
  geom_vline(xintercept = 20.5, col = "black") +
  labs(title = "II: Negative --> Positive", fill = "Test Type:",
       x = "10-Trial Blocks at Test", y = "Accuracy") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point", shape = 21, col = "black", size = 3,  alpha = 0.5) + 
  scale_fill_manual(values = c("green","yellow","purple")) +
  theme_bw()
p_rb_2 <- ggplot(rb[rb$test != "NA",], aes(x=nBlockArt, y=actO, group=test, fill=test)) +
  geom_vline(xintercept = 20.5, col = "black") +
  labs(title = "RB: Width --> Tilt", fill = "Test Type:",
       x = "10-Trial Blocks at Test", y = "Activations") +
  stat_summary(geom = "line") +
  stat_summary(geom = "point", shape = 21, col = "black", size = 3, alpha = 0.5) +
  scale_fill_manual(values = c("green","yellow","purple")) +
  theme_bw()

fig1 <- ggarrange(ggarrange(p_ii, p_rb, common.legend = T,labels = c("A","B")),
                  ggarrange(p_ii_2, p_rb_2, common.legend = T,labels = c("C","D")),nrow=2)
fig1
ggsave(paste0("figures/figure1.pdf"), dpi = 2000, limitsize = TRUE,
                     plot = fig1,
                     units = "cm", # "px", "cm", "in"
                     width = 16,
                     height = 16)
