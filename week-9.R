library(car)
library(lsmeans)
library(multcomp)
library(lme4)
library(lmerTest)
library(MASS)
library(ggplot2)
library(ordinal)
library(RVAideMemoire)
library(plyr)

setwd("~/Documents/repos/roland-hochmuth/designexperiments")

# Questions 1-6
df = read.csv("websearch3.csv")
head(df)
length(unique(df$Subject))

df$Subject <- factor(df$Subject)
df$Engine <- factor(df$Engine)
df$Order <- factor(df$Order)
df$Searches <- as.integer(df$Searches)
df$Effort <- order(df$Effort)

df %>%group_by(Engine) %>% summarise(Searches = mean(Searches)) -> groups

contrasts(df$Engine) <- contr.sum

m <- lmer(Searches ~ Engine + (1|Subject), data=df)
Anova(m, type=3, test.statistic="F")

summary(glht(m, mcp(Engine="Tukey")), test=adjusted(type="holm"))

# Questions 7-10
df = read.csv("socialvalue.csv")
head(df)
length(unique(df$Subject))

ddply(df, ~ Clip * Social, summarise, Valued.mean=mean(Valued), Valued.sd=sd(Valued))

df$Subject <- factor(df$Subject)
df$Clip <- factor(df$Clip)
df$ClipOrder <- factor(df$ClipOrder)
df$Social <- factor(df$Social)
df$SocialOrder <- factor(df$SocialOrder)
df$Valued <- as.integer(df$Valued)

contrasts(df$Social) <- contr.sum
contrasts(df$Clip) <- contr.sum

m <- lmer(Valued ~ (Social*Clip) + (1|Subject), data=df)
Anova(m, type=3, test.statistic="F")

summary(glht(m, lsm(pairwise ~ Social * Clip)), test=adjusted(type="none"))
p.adjust(c(0.00017, 0.59374), method="holm")

# Questions 11-16
df = read.csv("teaser.csv")
head(df)
length(unique(df$Subject))

df$Subject = factor(df$Subject)
df$Teaser = factor(df$Teaser)
df$Order = factor(df$Order)
df$Liked = factor(df$Liked)

# http://www.tfrec.wsu.edu/ANOVA/index.html

plot(df$Liked ~ df$Teaser)

contrasts(df$Order) <- contr.sum
m = glmer("Liked ~ Order + (1|Subject)", data=df, family="binomial")
Anova(m, type=3)

contrasts(df$Teaser) <- contr.sum
m = glmer("Liked ~ Teaser + (1|Subject)", data=df, family="binomial")
Anova(m, type=3)

summary(glht(m, lsm(pairwise ~ Teaser), data=df), test=adjusted(type="holm"))

# Questions 17-23
df = read.csv("vocab.csv")
head(df)
length(unique(df$Subject))

df %>% group_by(Social, Sex) %>% summarise(Vocab = mean(Vocab)) -> groups

ggplot() +
  aes(x = groups$Social, y = groups$Vocab, group = groups$Sex, color = groups$Sex) +
  geom_line() +
  geom_point()

x <- df[df$Social == "Facebook",]$Vocab
fit <- fitdistr(x, densfun="exponential")
ks.test(x=x, y="pexp", rate=0.0102424, exact=TRUE)

x <- df[df$Social == "Twitter",]$Vocab
fit <- fitdistr(x, densfun="exponential")
ks.test(x=x, y="pexp", rate=0.01052632, exact=TRUE)

x <- df[df$Social == "Gplus",]$Vocab
fit <- fitdistr(x, densfun="exponential")
ks.test(x=x, y="pexp", rate=0.004405286, exact=TRUE)

contrasts(df$Sex) <- contr.sum
contrasts(df$Order) <- contr.sum
m = glmer("Vocab ~ Order + (1|Subject)", data=df, family=Gamma(link="log"))
Anova(m, type=3)

contrasts(df$Sex) <- contr.sum
contrasts(df$Social) <- contr.sum
m = glmer("Vocab ~ (Sex*Social) + (1|Subject)", data=df, family=Gamma(link="log"))
Anova(m, type=3)

summary(glht(m, lsm(pairwise ~ Social), data=df), test=adjusted(type="holm"))

# Questions 24 and 25.
df = read.csv("websearch3.csv")
df$Effort <- as.factor(df$Effort)
df2 <- as.data.frame(df) # quirk
contrasts(df2$Engine) <- "contr.sum"
m = clmm(Effort ~ Engine + (1|Subject), data=df2)
Anova(m, type=3) # type ignored

# assuming code continuing from Q24
plot(as.numeric(Effort) ~ Engine, data=df2)
m = lmer(as.numeric(Effort) ~ Engine + (1|Subject), data=df2)
summary(glht(m, mcp(Engine="Tukey")), test=adjusted(type="holm"))

# Answers
# 1. 30
# 2. 152.67
# 3. 0.0564
# 4. No
# 5. Incorrect, 0.0516, 0.0454
# 6. True
# 7. 16
# 8. Incorrect, 163, 10
# 9. .0179
# 10. Incorrect, 0.0020, 0.0003
# 11. 20
# 12. Incorrect, Latin Square, Balanced Latin Square
# 13. Incorrect, horror
# 14. 0.4169
# 15. Incorrect, 0
# 16. 5
# 17. 30
# 18. 0
# 19. .2734
# 20. Incorrect, 0.8885
# 21. 0.8407
# 22. 0.578
# 23. 7
# 24. 0.0174
# 25. 0.9381

# References
# http://www.tfrec.wsu.edu/ANOVA/index.html
# https://sebastiansauer.github.io/vis_interaction_effects/