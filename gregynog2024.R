# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))



# read output data
# par <- read.csv("figures/mod6_600t_n16_rho0.01_mu0.01_gamma0.2_partial/exp_partial_mod6_n16.csv")
# par <- read.csv("output/exp_partial_mod6_n16_tf15_dt0.4_gamma0.2_eta_rho0.01_mu0.01_L2.csv")
# par <- read.csv("output/exp_partial_mod1_n16_alpha0.2_beta0.9.csv")
par <- read.csv("output/exp_partial_mod2_n16_beta0.9_rho0.05_mu5e-04.csv")
par$task <- "partial"
# tot <- read.csv("figures/mod6_600t_n16_rho0.01_mu0.01_gamma0.2_total/exp_total_mod6_n16.csv")
# tot <- read.csv("output/exp_total_mod1_n16_alpha0.2_beta0.9.csv")
tot <- read.csv("output/exp_total_mod2_n16_beta0.9_rho0.05_mu5e-04.csv")
tot$task <- "total"


source("functions.R")
db <- f_combine_sims(par,tot) 



# visualize
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
db$condition <- factor(db$condition, levels = c("reversed","nonreversed"))
db$condition2 <- paste0(db$task,"-",db$condition)
db$condition2 <- factor(db$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
plot2 <- ggplot(db[,], aes(x=nBlock,y=discScore,
                           col=condition2,linetype=condition2)) +
  labs(title = expression(rho==0.05*`;`~mu==0.0005),
       #title = expression(alpha==0.2*`;`~beta==0.9),
       x="blocks",col="Task and Condition",
       linetype="Task and Condition",#y=expression(act[target]*`/(`*act[target]+act[other]*`)`)
       y="correct score") +
  coord_cartesian(ylim = c(0,1)) +
  geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
  geom_vline(xintercept = 125.5, col="black", alpha=0.5) +
  # scale_x_continuous(breaks = seq(0,1000,by=250)) +
  scale_x_continuous(breaks = c(125,175)) +
  scale_y_continuous(breaks = seq(0,1,by=0.5)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  stat_summary(geom="line",position = position_dodge(0.2),size=1.2) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  theme_bw() 
# plot2






# read output data
# par <- read.csv("figures/mod6_600t_n16_rho0.01_mu0.0001_gamma0.2_partial/exp_partial_mod6_n16.csv")
# par <- read.csv("output/exp_partial_mod6_n16_tf15_dt0.4_gamma0.2_eta_rho0.01_mu1e-04_L2.csv")
par <- read.csv("output/exp_partial_mod1_n16_alpha0.2_beta0.9.csv")
par$task <- "partial"
# tot <- read.csv("figures/mod6_600t_n16_rho0.01_mu0.0001_gamma0.2_total/exp_total_mod6_n16.csv")
# tot <- read.csv("output/exp_total_mod6_n16_tf15_dt0.4_gamma0.2_eta_rho0.01_mu1e-04_L2.csv")
tot <- read.csv("output/exp_total_mod1_n16_alpha0.2_beta0.9.csv")
tot$task <- "total"


db <- f_combine_sims(par,tot) 



# visualize
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
db$condition <- factor(db$condition, levels = c("reversed","nonreversed"))
db$condition2 <- paste0(db$task,"-",db$condition)
db$condition2 <- factor(db$condition2, levels = c("total-reversed","partial-reversed","partial-nonreversed"))
plot1 <- ggplot(db[,], aes(x=nBlock,y=discScore,
                           col=condition2,linetype=condition2)) +
  labs(title = expression(alpha==0.2*`;`~beta==0.9),
       # title = expression(rho==0.05*`;`~mu==0.05),
       x="blocks",col="Task and Condition",
       linetype="Task and Condition",#y=expression(act[target]*`/(`*act[target]+act[other]*`)`)
       y="correct score") +
  coord_cartesian(ylim = c(0,1)) +
  geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
  geom_vline(xintercept = 125.5, col="black", alpha=0.5) +
  # scale_x_continuous(breaks = seq(0,1000,by=250)) +
  scale_x_continuous(breaks = c(125,175)) +
  scale_y_continuous(breaks = seq(0,1,by=0.5)) +
  scale_color_manual(values = c("blue","orange","orange")) +
  scale_linetype_manual(values = c("solid","solid","dotted")) +
  stat_summary(geom="line",position = position_dodge(0.2),size=1.2) +
  stat_summary(geom="errorbar",position = position_dodge(0.2)) +
  theme_bw()
# plot1




library(ggpubr)
plot <- ggarrange(plot1, plot2, ncol=2, common.legend = T, labels = c("A","B"))
plot


ggsave(paste0("figures/figureCO3.png"), dpi = 2400, limitsize = TRUE,
       plot = plot,
       units = "in",
       width = 7.5,
       height = 2.5)
