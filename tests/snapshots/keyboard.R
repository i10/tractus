###
## Comparing keyboard layouts: QWERTY, Dvorak, and Colemak
##

## One-way ANOVA

# read in a data file with task completion Speeds (min) now from 3 tools
kbd = read.csv("./2-kbd-study.csv")
View(kbd)
kbd$ParticipantID = factor(kbd$ParticipantID) # convert to nominal factor
summary(kbd)

# view descriptive statistics by Speed
library(plyr)
ddply(kbd, ~ Layout, function(data) summary(data$Speed))
ddply(kbd, ~ Layout, summarise, Speed.mean=mean(Speed), Speed.sd=sd(Speed))

# explore new response distribution
hist(kbd[kbd$Layout == "QWERTY",]$Speed)
hist(kbd[kbd$Layout == "Dvorak",]$Speed)
hist(kbd[kbd$Layout == "Colemak",]$Speed) # new one
plot(Speed ~ Layout, data=kbd) # boxplot

# test normality for new Layout
shapiro.test(kbd[kbd$Layout == "QWERTY",]$Speed)
shapiro.test(kbd[kbd$Layout == "Dvorak",]$Speed)
shapiro.test(kbd[kbd$Layout == "Colemak",]$Speed)
m = aov(Speed ~ Layout, data=kbd) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

# test log-normality of new Layout
library(MASS)
fit = fitdistr(kbd[kbd$Layout == "QWERTY",]$Speed, "lognormal")$estimate
ks.test(kbd[kbd$Layout == "QWERTY",]$Speed, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE) # lognormality

# compute new log(Speed) column and re-test
kbd$logSpeed = log(kbd$Speed) # add new column
View(kbd) # verify
shapiro.test(kbd[kbd$Layout == "QWERTY",]$logSpeed)
m = aov(logSpeed ~ Layout, data=kbd) # fit model
shapiro.test(residuals(m)) # test residuals
qqnorm(residuals(m)); qqline(residuals(m)) # plot residuals

# test homoscedasticity
library(car)
leveneTest(Speed ~ Layout, data=kbd, center=median) # Brown-Forsythe test

# one-way ANOVA, suitable now to logSpeed
m = aov(Speed ~ Layout, data=kbd) # fit model
anova(m) # report anova

# post hoc independent-samples t-tests
plot(Speed ~ Layout, data=kbd) # for convenience
library(multcomp)
summary(glht(m, mcp(Layout="Tukey")), test=adjusted(type="holm")) # Tukey means compare all pairs
# note: equivalent to this using lsm instead of mcp
library(lsmeans)
summary(glht(m, lsm(pairwise ~ Layout)), test=adjusted(type="holm"))


## Nonparametric equivalent of one-way ANOVA

# Kruskal-Wallis test
library(coin)
kruskal_test(Speed ~ Layout, data=kbd, distribution="asymptotic") # can't do exact with 3 levels
kruskal_test(logSpeed ~ Layout, data=kbd, distribution="asymptotic") # note: same result
# for reporting Kruskal-Wallis as chi-square, we can get N with nrow(kbd)

# manual post hoc Mann-Whitney U pairwise comparisons
# note: wilcox_test we used above doesn't take two data vectors, so use wilcox.test
vs.ec = wilcox.test(kbd[kbd$Layout == "Dvorak",]$Speed, kbd[kbd$Layout == "QWERTY",]$Speed, exact=FALSE)
vs.py = wilcox.test(kbd[kbd$Layout == "Colemak",]$Speed, kbd[kbd$Layout == "QWERTY",]$Speed, exact=FALSE)
ec.py = wilcox.test(kbd[kbd$Layout == "Colemak",]$Speed, kbd[kbd$Layout == "Dvorak",]$Speed, exact=FALSE)
p.adjust(c(vs.ec$p.value, vs.py$p.value, ec.py$p.value), method="holm")

# alternative approach is using PMCMR for nonparam pairwise comparisons
library(PMCMR)
posthoc.kruskal.conover.test(Speed ~ Layout, data=kbd, p.adjust.method="holm") # Conover & Iman (1979)
