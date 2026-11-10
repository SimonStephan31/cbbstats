# =============================================================================
# Linear Models in R · Session 11: From GLMs to GLMMs
# Worked example: the Safe-vs-Risky box study (made-up data)
# Simon Stephan · CBB Göttingen
# =============================================================================
#
# Chimpanzees choose into which of two boxes a grape is dropped. The Safe box
# clearly guarantees delivery; in the Risky box, the grape seems to get stuck.
# In fact, both boxes always deliver.
#   RQ1: Do chimps prefer the Safe box in session 1?
#   RQ2: Does this preference change over sessions?
#
# The study (with the MPI EVA, Leipzig) is preregistered; the data do not
# exist yet. The numbers below are made up to reflect what we expect.

library(lme4)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1. The data: 10 chimps x 4 sessions x 10 trials
# -----------------------------------------------------------------------------

# Safe choices (out of 10) of each chimp (rows) in sessions 1-4 (columns)
K <- rbind(c( 9, 10, 10, 10),  # chimp 1
           c(10,  9,  9,  8),  # chimp 2
           c(10, 10,  9,  7),  # chimp 3
           c(10, 10,  6,  8),  # chimp 4
           c( 5,  8,  8,  8),  # chimp 5
           c( 8,  6,  6,  3),  # chimp 6
           c( 5,  3,  2,  4),  # chimp 7
           c( 5,  2,  3,  3),  # chimp 8
           c( 1,  0,  1,  0),  # chimp 9
           c( 1,  0,  0,  0))  # chimp 10
cd <- data.frame(chimp = factor(rep(1:10, 4)), session = rep(1:4, each = 10), safe = c(K))

# Trial-level data (what the models are fitted to): one row per choice,
# Safe trials at random positions within each session
set.seed(11)
trials <- do.call(rbind, lapply(seq_len(nrow(cd)), function(i)
  data.frame(chimp = cd$chimp[i], session = cd$session[i],
             trial = (cd$session[i] - 1) * 10 + 1:10,
             choice = sample(rep(c(1, 0), c(cd$safe[i], 10 - cd$safe[i]))))))
trials$session_c <- trials$session - 1   # session 1 = 0
str(trials)

# -----------------------------------------------------------------------------
# 2. Plot the data: every chimp is different
# -----------------------------------------------------------------------------

cd$p <- cd$safe / 10
ggplot(cd, aes(session, p, group = chimp, colour = chimp)) +
  geom_line() + geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Session", y = "Proportion of Safe choices") +
  theme_minimal()

# -----------------------------------------------------------------------------
# 3. The model we already know: a logistic GLM (session 10)
# -----------------------------------------------------------------------------

glm.mod <- glm(choice ~ session_c, family = binomial, data = trials)
summary(glm.mod)
plogis(coef(glm.mod)[1])     # P(Safe) in session 1

# Problem: 400 choices, but only 10 chimpanzees -> pseudo-replication

# -----------------------------------------------------------------------------
# 4. Two solutions
# -----------------------------------------------------------------------------

# Standard solution: chimp as a dummy-coded factor (9 extra coefficients;
# results hold only for these 10 chimps)
fix.mod <- glm(choice ~ session_c + chimp, family = binomial, data = trials)
summary(fix.mod)

# GLMM solution: chimp as a random effect (one extra parameter: the SD)
ri.mod <- glmer(choice ~ session_c + (1 | chimp), family = binomial, data = trials)
summary(ri.mod)
VarCorr(ri.mod)              # between-chimp SD of the intercepts
coef(ri.mod)$chimp           # each chimp's intercept (and the shared slope)

# -----------------------------------------------------------------------------
# 5. Random slopes: each chimp its own change over sessions
# -----------------------------------------------------------------------------

rs.mod <- glmer(choice ~ session_c + (1 + session_c | chimp),
                family = binomial, data = trials,
                control = glmerControl(optimizer = "bobyqa"))
summary(rs.mod)
VarCorr(rs.mod)              # SDs of intercepts and slopes, and their correlation
coef(rs.mod)$chimp           # each chimp's intercept and slope

# -----------------------------------------------------------------------------
# 6. What changes? Compare the three models
# -----------------------------------------------------------------------------

get_coefs <- function(m) summary(m)$coefficients[, c(1, 2, 4)]
round(get_coefs(glm.mod), 3)
round(get_coefs(ri.mod),  3)
round(get_coefs(rs.mod),  3)

# Predicted P(Safe) for each chimp under the random-slope model
nd <- expand.grid(session_c = seq(0, 3, by = 0.05), chimp = levels(trials$chimp))
nd$p <- predict(rs.mod, newdata = nd, type = "response")
ggplot(nd, aes(session_c + 1, p, group = chimp)) +
  geom_point(data = cd, aes(session, p), colour = "grey60") +
  geom_line(colour = "#1b6f8a") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Session", y = "Predicted P(Safe)") +
  theme_minimal()
