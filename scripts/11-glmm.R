# =============================================================================
# Linear Models in R · Session 11: From GLMs to GLMMs
# Worked example: the Safe-vs-Risky box study (simulated data)
# Simon Stephan · CBB Göttingen
# =============================================================================
#
# Chimpanzees choose into which of two boxes a grape is dropped. The Safe box
# clearly guarantees delivery; in the Risky box, the grape seems to get stuck.
# In fact, both boxes always deliver.
#   RQ1: Do chimps prefer the Safe box on trial 1?
#   RQ2: Does this preference change over trials?
#
# The study (with the MPI EVA, Leipzig) is preregistered; the data do not
# exist yet. We use simulated data of what we expect, so we know the truth.

library(lme4)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1. Simulate the data
# -----------------------------------------------------------------------------

TRUE.INT   <- qlogis(0.70)  # log-odds of choosing Safe on trial 1 (Side = left)
TRUE.TRIAL <- 0             # average change per trial (log-odds)
TRUE.SIDE  <- 0.3           # small bias towards the right side
SD.INT     <- 0.8           # between-chimp SD of the intercept
SD.SLOPE   <- 0.05          # between-chimp SD of the trial slope (per trial)

side_sequence <- function() {           # 5 x right, 5 x left; max. 2 in a row
  repeat {
    s <- sample(rep(0:1, 5))
    if (max(rle(s)$lengths) <= 2) return(s)
  }
}

sim_chimps <- function(n = 15, b0 = TRUE.INT, b.trial = TRUE.TRIAL, b.side = TRUE.SIDE,
                       sd.int = SD.INT, sd.slope = SD.SLOPE) {
  ids <- sprintf("C%02d", 1:n)
  u0 <- setNames(rnorm(n, 0, sd.int), ids); u1 <- setNames(rnorm(n, 0, sd.slope), ids)
  d <- do.call(rbind, lapply(ids, function(id)
    data.frame(subject = id, session = rep(1:4, each = 10), trial = 1:40,
               side_safe = unlist(replicate(4, side_sequence(), simplify = FALSE)))))
  d$trial_c <- d$trial - 1                       # first-trial centred (as preregistered)
  eta <- b0 + u0[d$subject] + (b.trial + u1[d$subject]) * d$trial_c + b.side * d$side_safe
  d$safe <- rbinom(nrow(d), 1, plogis(eta))
  d
}

set.seed(120)
xdata <- sim_chimps()        # 15 chimps x 4 sessions x 10 trials = 600 choices
str(xdata)

# -----------------------------------------------------------------------------
# 2. Look at the data: every chimpanzee is different
# -----------------------------------------------------------------------------

ps <- aggregate(safe ~ subject + session, data = xdata, FUN = mean)
ggplot(ps, aes(session, safe)) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  geom_line() + geom_point() +
  facet_wrap(~ subject, nrow = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Session", y = "Proportion of Safe choices") +
  theme_minimal()

# -----------------------------------------------------------------------------
# 3. The model we already know: a logistic GLM (session 10)
# -----------------------------------------------------------------------------

glm.mod <- glm(safe ~ trial_c + side_safe, family = binomial, data = xdata)
summary(glm.mod)
plogis(coef(glm.mod)[1])     # P(Safe) on trial 1 (Side = left)

# Problem: 600 choices, but only 15 chimpanzees -> pseudoreplication

# -----------------------------------------------------------------------------
# 4. Option A: chimp as a fixed factor (31 coefficients, conditional on these chimps)
# -----------------------------------------------------------------------------

fix.mod <- glm(safe ~ trial_c * subject + side_safe, family = binomial, data = xdata)
length(coef(fix.mod))

# -----------------------------------------------------------------------------
# 5. Option B: random effects
# -----------------------------------------------------------------------------

ctl <- glmerControl(optimizer = "bobyqa")

# Random intercepts: chimps differ in their overall preference
ri.mod <- glmer(safe ~ trial_c + side_safe + (1 | subject),
                family = binomial, data = xdata, control = ctl)
summary(ri.mod)

# Random intercepts + random slopes: chimps also differ in how their preference changes
rs.mod <- glmer(safe ~ trial_c + side_safe + (1 + trial_c | subject),
                family = binomial, data = xdata, control = ctl)
summary(rs.mod)
VarCorr(rs.mod)              # the between-chimp SDs
ranef(rs.mod)$subject        # each chimp's deviation from the average

# Likelihood-ratio test of the trial effect (keeping the random slope)
rs.null <- glmer(safe ~ side_safe + (1 + trial_c | subject),
                 family = binomial, data = xdata, control = ctl)
anova(rs.null, rs.mod)

# -----------------------------------------------------------------------------
# 6. What changes? Compare the three models with the truth
# -----------------------------------------------------------------------------

get_trial <- function(m) summary(m)$coefficients["trial_c", c(1, 2, 4)]
cmp <- rbind(GLM            = get_trial(glm.mod),
             `GLMM (1 | s)` = get_trial(ri.mod),
             `GLMM (1 + trial_c | s)` = get_trial(rs.mod))
round(cmp, 4)                # true average trial effect = 0

# Predicted lines for each chimp
nd <- expand.grid(trial_c = 0:39, subject = unique(xdata$subject), side_safe = 0.5)
nd$p <- predict(rs.mod, newdata = nd, type = "response")
ggplot(nd, aes(trial_c + 1, p, group = subject)) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  geom_line(colour = "#1b6f8a", alpha = 0.7) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Trial", y = "Predicted P(Safe)") +
  theme_minimal()

# -----------------------------------------------------------------------------
# 7. Was this just bad luck? Repeat the whole study 1000 times
# -----------------------------------------------------------------------------

fit_chimps <- function(d) {
  ctl <- glmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
  g  <- glm(safe ~ trial_c + side_safe, family = binomial, data = d)
  ri <- glmer(safe ~ trial_c + side_safe + (1 | subject), family = binomial, data = d, control = ctl)
  rs <- glmer(safe ~ trial_c + side_safe + (1 + trial_c | subject), family = binomial, data = d, control = ctl)
  c(glm.p = summary(g)$coef["trial_c", 4],
    ri.p  = summary(ri)$coef["trial_c", 4],
    rs.p  = summary(rs)$coef["trial_c", 4])
}

# Takes a few minutes; reduce n for a quick check
set.seed(2026)
sims <- as.data.frame(t(replicate(1000, suppressMessages(fit_chimps(sim_chimps())))))
colMeans(sims < 0.05)        # false-positive rates; nominal = 0.05
