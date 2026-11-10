# =============================================================================
# Linear Models in R · Session 10: Generalized linear models (GLM)
# Worked examples: puzzle box (logistic regression) and grunts (Poisson / NB)
# Simon Stephan · CBB Göttingen
# =============================================================================
#
# Both data sets are simulated; each monkey contributes ONE observation, so the
# observations are independent (repeated observations: session 11).

library(ggplot2)
library(MASS)        # glm.nb()
# install.packages("DHARMa")   # simulated residuals (recommended)

# =============================================================================
# Example 1: who solves the puzzle box?  (binary response)
# =============================================================================

set.seed(10)
puzzle <- data.frame(age = round(runif(120, 2, 22), 1))
puzzle$age_c  <- puzzle$age - 10                     # centred at 10 years
puzzle$solved <- rbinom(120, 1, plogis(0.4 - 0.18 * puzzle$age_c))

# --- What goes wrong with a linear model? -----------------------------------
lm1 <- lm(solved ~ age_c, data = puzzle)
predict(lm1, newdata = data.frame(age_c = c(-10, 15)))   # ages 0 and 25: < 0% / > 100%?
par(mfrow = c(2, 2)); plot(lm1); par(mfrow = c(1, 1))  # residual plots make no sense

# --- Logistic regression -----------------------------------------------------
glm1 <- glm(solved ~ age_c, family = binomial, data = puzzle)
summary(glm1)

plogis(coef(glm1)[1])         # P(solved) for a 10-year-old
exp(coef(glm1))               # odds ratios
exp(confint(glm1))            # profile CIs of the odds ratios
coef(glm1)[2] / 4             # rule of thumb: max. change in probability per year

# --- Predictions on the response scale (CI on the link scale!) --------------
nd <- data.frame(age = seq(2, 22, by = 0.2)); nd$age_c <- nd$age - 10
pr <- predict(glm1, newdata = nd, type = "link", se.fit = TRUE)
nd$p  <- plogis(pr$fit)
nd$lo <- plogis(pr$fit - 1.96 * pr$se.fit)
nd$hi <- plogis(pr$fit + 1.96 * pr$se.fit)

ggplot(nd, aes(age, p)) +
  geom_jitter(data = puzzle, aes(age, solved), height = .03, width = 0, alpha = .5) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .2) +
  geom_line(linewidth = 1.2) +
  labs(x = "Age (years)", y = "P(solved)") +
  theme_minimal()

# --- Inference: Wald (summary) vs. likelihood-ratio test --------------------
glm0 <- glm(solved ~ 1, family = binomial, data = puzzle)
anova(glm0, glm1, test = "Chisq")
drop1(glm1, test = "Chisq")
AIC(glm0, glm1)

# --- One framework: a binary predictor = a 2 x 2 table ----------------------
puzzle$sex <- factor(sample(rep(c("female", "male"), 60)))
tab <- table(puzzle$sex, puzzle$solved); tab
(tab[2, 2] / tab[2, 1]) / (tab[1, 2] / tab[1, 1])                  # odds ratio by hand
exp(coef(glm(solved ~ sex, family = binomial, data = puzzle))[2])  # identical

# --- Simulate first: power for a smaller effect (odds ratio 0.90 per year) --
power_logistic <- function(n, b = -0.1, nsim = 400) {
  mean(replicate(nsim, {
    age_c <- runif(n, 2, 22) - 10
    y <- rbinom(n, 1, plogis(0.4 + b * age_c))
    coef(summary(glm(y ~ age_c, family = binomial)))[2, 4] < .05
  }))
}
sapply(c(30, 60, 120, 200), power_logistic)

# =============================================================================
# Example 2: grunts during focal observations  (counts, different durations)
# =============================================================================

set.seed(3)
calls <- data.frame(rank_z = round(rnorm(90), 2), minutes = round(runif(90, 10, 60)))
calls$grunts <- rnbinom(90, mu = calls$minutes * exp(log(0.15) + 0.35 * calls$rank_z), size = 2)

# --- Poisson regression with an offset ---------------------------------------
pois <- glm(grunts ~ rank_z + offset(log(minutes)), family = poisson, data = calls)
summary(pois)
exp(coef(pois))               # grunts per minute (intercept), rate ratio (rank)

# --- Overdispersion ----------------------------------------------------------
sum(residuals(pois, type = "pearson")^2) / df.residual(pois)   # should be ~ 1
# library(DHARMa); plot(simulateResiduals(pois))

# --- Negative binomial model -------------------------------------------------
nb <- glm.nb(grunts ~ rank_z + offset(log(minutes)), data = calls)
summary(nb)
nb$theta                      # extra variance: var = mu + mu^2 / theta
round(cbind(Poisson = sqrt(diag(vcov(pois))), NegBin = sqrt(diag(vcov(nb)))), 3)

# --- Why it matters: false positives under overdispersion -------------------
set.seed(7)
fp <- t(replicate(1000, {
  d <- data.frame(x = rnorm(90), minutes = runif(90, 10, 60))
  d$y <- rnbinom(90, mu = d$minutes * 0.15, size = 2)              # x has NO effect
  c(pois = coef(summary(glm(y ~ x + offset(log(minutes)), family = poisson, data = d)))["x", 4],
    nb   = coef(summary(glm.nb(y ~ x + offset(log(minutes)), data = d)))["x", 4])
}))
colMeans(fp < .05)            # nominal: 0.05

# =============================================================================
# Beyond counts: durations with a Gamma GLM
# =============================================================================

set.seed(4)
lat <- data.frame(group = factor(rep(c("familiar", "novel"), each = 40)))
lat$latency <- rgamma(80, shape = 2, rate = 2 / ifelse(lat$group == "novel", 30, 15))
gm <- glm(latency ~ group, family = Gamma(link = "log"), data = lat)
exp(coef(gm))                 # ratio of mean latencies
