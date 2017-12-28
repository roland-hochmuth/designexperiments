library(tidyverse)
library(car)
library(emmeans)
library(multcomp)
library(lme4)
library(MASS)
library(ordinal)
library(RVAideMemoire)

setwd("~/Documents/repos/roland-hochmuth/designexperiments")

# Questions 1-6
df = read_csv("websearch3.csv")
head(df)

# Determine the number of unique subjects.
n_distinct(df$Subject)

df$Subject <- factor(df$Subject)
df$Engine <- factor(df$Engine)
df$Order <- factor(df$Order)
df$Searches <- as.integer(df$Searches)
df$Effort <- order(df$Effort)

# Determine the mean of the number of searches by search engine.
df %>% group_by(Engine) %>% summarise(Searches = mean(Searches))

# Conduct a linear mixed model (LMM) analysis of variance on Searches by Engine.
contrasts(df$Engine) <- contr.sum
m <- lmer(Searches ~ Engine + (1|Subject), data=df)
Anova(m, type=3, test.statistic="F")

summary(glht(m, mcp(Engine="Tukey")), test=adjusted(type="holm"))

# Questions 7-10
df = read_csv("socialvalue.csv")
head(df)

# Determine the number of unique subjects.
n_distinct(df$Subject)

df %>% group_by(Clip, Social) %>% summarise(mean = mean(Valued), n = n())

df$Subject <- factor(df$Subject)
df$Clip <- factor(df$Clip)
df$ClipOrder <- factor(df$ClipOrder)
df$Social <- factor(df$Social)
df$SocialOrder <- factor(df$SocialOrder)
df$Valued <- as.integer(df$Valued)

contrasts(df$Social) <- contr.sum
contrasts(df$Clip) <- contr.sum

# Conduct a linear mixed model (LMM) analysis of variance on Valued by Social and Clip.
m <- lmer(Valued ~ (Social*Clip) + (1|Subject), data=df)
Anova(m, type=3, test.statistic="F")

# Conduct two planned pairwise comparisons of how the film clips may have influenced judgments about the value of social media.
summary(glht(m, lsm(pairwise ~ Social * Clip)), test=adjusted(type="none"))

# Adjust and compare the pvalues for positive on Facebook and positive on Twitter
p.adjust(c(0.00017, 0.59374), method="holm")

# Questions 11-16
df = read_csv("teaser.csv")
head(df)

# Determine the number of unique subjects.
n_distinct(df$Subject)

df$Subject = factor(df$Subject)
df$Teaser = factor(df$Teaser)
df$Order = factor(df$Order)
df$Liked = factor(df$Liked)

# Display the number of Liked by Teaser.
ggplot(df) +
  aes(x = Teaser, y = Liked) +
  geom_bar(stat = "identity")

# Using a generalized linear mixed model (GLMM), conduct a test of order effects on Liked to ensure counterbalancing worked.
contrasts(df$Order) <- contr.sum
m = glmer("Liked ~ Order + (1|Subject)", data=df, family="binomial")
Anova(m, type=3)

# Using a generalized linear mixed model (GLMM), conduct a test of Liked by Teaser.
contrasts(df$Teaser) <- contr.sum
m = glmer("Liked ~ Teaser + (1|Subject)", data=df, family="binomial")
Anova(m, type=3)

# Conduct simultaneous post hoc pairwise comparisons among levels of Teaser. Be sure to use Holm's sequential Bonferroni procedure.
summary(glht(m, lsm(pairwise ~ Teaser), data=df, df=FALSE), test=adjusted(type="holm"))

# Questions 17-23
df = read_csv("vocab.csv")
head(df)

# Determine the number of unique subjects.
n_distinct(df$Subject)

df$Subject <- factor(df$Subject)
df$Sex <- factor(df$Sex)
df$Social <- factor(df$Social)
df$Order <- factor(df$Order)
df$Vocab <- as.integer(df$Vocab)

df %>% group_by(Social, Sex) %>% summarise(Vocab = mean(Vocab)) -> groups
groups

# Display an interaction plot with Social on the x-axis and Sex as the traces
ggplot(groups) +
  aes(x=Social, y=Vocab, group=Sex, color=Sex) +
  geom_line() +
  geom_point()

# Perform three Kolmogorov-Smirnov goodness-of-fit tests on Vocab for each level of Social using exponential distributions.
x <- df[df$Social == "Facebook",]$Vocab
fit <- fitdistr(x, densfun="exponential")
ks.test(x=x, y="pexp", rate=0.0102424, exact=TRUE)

x <- df[df$Social == "Twitter",]$Vocab
fit <- fitdistr(x, densfun="exponential")
ks.test(x=x, y="pexp", rate=0.01052632, exact=TRUE)

x <- df[df$Social == "Gplus",]$Vocab
fit <- fitdistr(x, densfun="exponential")
ks.test(x=x, y="pexp", rate=0.004405286, exact=TRUE)

# Use a generalized linear mixed model (GLMM) to conduct a test of order effects on Vocab to ensure counterbalancing worked.
contrasts(df$Sex) <- contr.sum
contrasts(df$Order) <- contr.sum
m = glmer("Vocab ~ (Sex*Order) + (1|Subject)", data=df, family=Gamma(link="log"))
Anova(m, type=3)

# Use a generalized linear mixed model (GLMM) to conduct a test of Vocab by Sex and Social. 
contrasts(df$Sex) <- contr.sum
contrasts(df$Social) <- contr.sum
m = glmer("Vocab ~ (Sex*Social) + (1|Subject)", data=df, family=Gamma(link="log"))
Anova(m, type=3)

summary(glht(m, lsm(pairwise ~ Social), data=df, df=FALSE), test=adjusted(type="holm"))

# Questions 24 and 25.
df = read_csv("websearch3.csv")
head(df)

# Display a boxplot of Effort by Engine
ggplot(df) +
  aes(x = Engine, y = Effort) +
  geom_boxplot()

df$Effort <- as.factor(df$Effort)
df2 <- as.data.frame(df) # quirk

contrasts(df2$Engine) <- "contr.sum"
m = clmm(Effort ~ Engine + (1|Subject), data=df2)
Anova(m, type=3) # type ignored

m = lmer(as.numeric(Effort) ~ Engine + (1|Subject), data=df2)
summary(glht(m, mcp(Engine="Tukey")), test=adjusted(type="holm"))

# References
# http://www.tfrec.wsu.edu/ANOVA/index.html
# https://sebastiansauer.github.io/vis_interaction_effects/
# https://cran.r-project.org/web/packages/emmeans/index.html
# https://www.tidyverse.org/
# http://rcompanion.org/handbook/G_12.html
# https://cran.r-project.org/web/packages/ordinal/vignettes/clmm2_tutorial.pdf
# https://cran.r-project.org/web/packages/ordinal/ordinal.pdf
