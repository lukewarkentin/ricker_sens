# See how alpha and beta effect SMSY and Sgen values

library(ggplot2)
library(purrr)
library(here)
library(gsl) # need for lambert_W() function
options(scipen = 10, digits=9)
# Source function code
walk(list.files(here("R"), full.names = TRUE), source)

# Make series of alpha and beta values
alphas <- c(seq(1,2,0.1),seq(2,7,by=0.05)) # alphas values, higher resolution below 2. uniroot function used to get Sgen gives error if you include alpha =1
betas <- 1/(10000 * seq(0.5, 1.5, 0.1)) # betas centered at 1/10000 (carrying capacity =10000) with variation in carrying capacity +/- 50%
# make into a data frame
df <- expand.grid(alphas, betas)
names(df) <- c("alpha", "beta")
# generate SMSY based on alpha and beta
#df$SMSY = log(df$alpha)*(0.5-0.07*log(df$alpha))/df$beta # Hilborn and Walters equation to estimate SMSY


# get SMSY and Sgen in new data frame
ndf <- pmap_dfr(list(df$alpha, df$beta), get_SMSY_Sgen)
# Check with lambert method

SMSY_lam <- pmap(list(a=df$alpha, b=df$beta), SMSY_lambert)
plot(ndf$SMSY, SMSY_lam)
abline(a=0,b=1, col="orange")

# bind SMSY and Sgen to alpha and beta
df1 <- cbind(df, ndf)
# function to check whether Sgen is correct. Should be the spawner number 
#   that gives recruits=SMSY in one generation, no fishing
check_Sgen <- function(a,b,S) {
  a*S*exp(-b*S)
}
# yes, correctly gets to SMSY
plot(cbind(check_Sgen(a=df1$alpha, b=df1$beta, S=df1$Sgen), df1$SMSY))
abline(a=0,b=1, col="orange")


# make data frame with alpha and beta from Nicola, Kitsumkalum 
pops <- read.csv(here("data-in/values-real-populations.csv"))
npops <- pmap_dfr(list(pops$alpha, pops$beta), get_SMSY_Sgen)
pops1 <- cbind(pops, npops)

plot(cbind(check_Sgen(a=pops1$alpha, b=pops1$beta, S=pops1$Sgen), pops1$SMSY))
abline(a=0,b=1, col="orange")


# Plot how SMSY and Sgen change with alpha and beta
if(!dir.exists("figures"))
dir.create("figures")

# SMSY
png("figures/fig_SMSY~alpha.png", width=8,height=6, units="in", res=300)
ggplot(df1, aes(x=alpha, y=SMSY, colour=beta)) +
  geom_point() +
  scale_colour_viridis_c() +
  geom_point(data=pops1, aes(x=alpha, y=SMSY), size=2, colour='black' ) +
  geom_text(data=pops1, aes(x=alpha, y=SMSY, label=population), nudge_y=300, colour="black") +
  theme_bw()
dev.off()

# Sgen
png("figures/fig_Sgen~alpha.png", width=8,height=6, units="in", res=300)
ggplot(df1, aes(x=alpha, y=Sgen, colour=beta)) +
  geom_point() +
  scale_colour_viridis_c() +
  geom_point(data=pops1, aes(x=alpha, y=Sgen), size=2, colour='black' ) +
  geom_text(data=pops1, aes(x=alpha, y=Sgen, label=population), nudge_y=100, colour="black") +
  theme_bw()
dev.off()

# Sgen no pops.
png("figures/fig_Sgen~alpha-basic.png", width=6,height=4, units="in", res=300)
ggplot(df1, aes(x=alpha, y=Sgen, colour=beta)) +
  #geom_point(size=1) +
  geom_line(aes(group=beta), linewidth=0.5) +
  scale_colour_viridis_c() +
  ylab(expression(S[gen])) +
  xlab(expression(alpha)) +
  labs(col = expression(beta)) +
  scale_x_continuous(breaks = 1:7) +
  coord_cartesian(expand = FALSE,ylim = c(0, max(df1$Sgen +100)), xlim = c(0.8,7.2), clip = "off") +
  # geom_point(data=pops1, aes(x=alpha, y=Sgen), size=2, colour='black' ) +
  # geom_text(data=pops1, aes(x=alpha, y=Sgen, label=population), nudge_y=100, colour="black") +
  theme_classic()
dev.off()


# Both are very sensitive to beta. SMSY is more sensitive to alpha. Sgen not very sensitive to alpha.

# All, alpha and beta axes
png("figures/fig_beta~alpha.png", width=8,height=6, units="in", res=300)
ggplot(df1, aes(x=alpha, y=beta, color=SMSY, size=Sgen)) +
  geom_point() +
  scale_colour_viridis_c() +
  theme_bw()
dev.off()

# All, Sgen and SMSY axes
png("figures/fig_Sgen~SMSY.png", width=8,height=6, units="in", res=300)
ggplot(df1, aes(x=SMSY, y=Sgen, color=alpha, fill=alpha, size=beta)) +
  geom_point(shape=21) +
  scale_colour_viridis_c() +
  scale_fill_viridis_c(alpha=0.5) +
  theme_bw()
dev.off()

# SMSY and Sgen very sensitive to beta, alpha is secondary. 
# For given value of beta, Sgen is highest at intermediate alpha. 




# # Plot ricker curves with Sgen and SMSY
# plot(type="n", bty="l", x=0, y=0, xlim=c(0,20000), ylim=c(0,20000)) 
# abline(a=0, b=1, lty=3, col="gray")
# #for(i in sample(1:nrow(df), size=5, replace=FALSE)) {
# for(i in 1:nrow(df1)) {
#   curve(df1$alpha[i]*x * exp(-df1$beta[i]*x), add=TRUE, col=i)
#   abline(v=df1$Sgen[i], col=i, lty=2)
#   abline(v=df1$SMSY[i], col=i)
#   #  text(x=1000+i*10000,y=30000, col=i, label=df$beta[i])
# }

