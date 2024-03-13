# Remove all of the elements currently loaded in R
rm(list=ls(all=TRUE))

# read output data
par <- read.csv("figures/mod6_200t_n16_partial_rho0.05_mu0.05/exp_partial_mod6_n16.csv")
par$task <- "partial"
tot <- read.csv("figures/mod6_200t_n16_total_rho0.05_mu0.05/exp_total_mod6_n16.csv")
tot$task <- "total"

# ggplot(tot, aes(x=nBlock,y=actO,col=trialType)) +
#   stat_summary(geom = "line") +
#   facet_grid(.~out) +
#   theme_classic()
# ggplot(par, aes(x=nBlock,y=actO,col=trialType)) +
#   stat_summary(geom = "line") +
#   facet_grid(.~out) +
#   theme_classic()

# prepare phase 2 partial reversal only reversed stimuli
par <- par[par$ph == 2,]
actO.1 <- par[par$out == "actO.1",]
actO.1$actO.1 <- actO.1$actO; actO.1$out <- NULL
actO.2 <- par[par$out == "actO.2",]
par <- data.frame(actO.1,actO.2=actO.2$actO)

par$discScore <- ifelse(par$trialType == "A" | par$trialType == "B" |
                          par$trialType == "G" | par$trialType == "H",
                        par$actO.1/(par$actO.1+par$actO.2),
                        par$actO.2/(par$actO.1+par$actO.2))
par$condition <- ifelse(par$trialType == "C" | par$trialType == "D" |
                          par$trialType == "G" | par$trialType == "H",
                        "reversal","non-reversal")

# relevant <- t(matrix(c("C","actO.2",
#                        "D","actO.2",
#                        "G","actO.1",
#                        "H","actO.1"),ncol=4)); par$relevant <- F
# for (i in 1:nrow(relevant)) {
#   par$relevant[par$trialType == relevant[i,1] & par$out == relevant[i,2]] <- T
# }
# par <- par[par$relevant == T,]; par$relevant <- NULL



# prepare phase 2 total reversal
tot <- tot[tot$ph == 2,]
actO.1 <- tot[tot$out == "actO.1",]
actO.1$actO.1 <- actO.1$actO; actO.1$out <- NULL
actO.2 <- tot[tot$out == "actO.2",]
tot <- data.frame(actO.1,actO.2=actO.2$actO)

tot$discScore <- ifelse(tot$trialType == "E" | tot$trialType == "F" |
                          tot$trialType == "G" | tot$trialType == "H",
                        tot$actO.1/(tot$actO.1+tot$actO.2),
                        tot$actO.2/(tot$actO.1+tot$actO.2))
tot$condition <- "reversal"

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

# visualize
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
plot<-ggplot(db[db$condition=="reversal",], aes(x=nBlock,y=discScore,col=task)) +
  labs(title = "rho = 0.05; mu = 0.01",
    y=expression(act[target]*`/(`*act[target]+act[other]*`)`)) +
  coord_cartesian(ylim = c(0,1)) +
  geom_hline(yintercept = 0.5, col="black", alpha=0.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0, alpha = 0.1) +
  stat_summary(fun = "mean", geom="line") +
  theme_classic()
plot
ggsave(paste0("figures/CRL_200t_rho0.05_mu0.05.png"), dpi = 300, limitsize = TRUE,
       plot = plot,
       units = "px",
       width = 1200,
       height = 800)
