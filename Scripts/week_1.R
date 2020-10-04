d.cats <- read.csv("Data/Cats.csv", header = T, stringsAsFactors = T)

# First impression of the data
head(d.cats)
str(d.cats)

plot(Hwt ~ Bwt, data = d.cats)

# 1.1 Question - Linear models 
lm.cats <- lm(Hwt ~ Bwt, data = d.cats)
coef(lm.cats)

mean.Bwt <- mean(d.cats$Bwt)
d.cats$Bwt.centered <- d.cats$Bwt - mean.Bwt
plot(Hwt ~ Bwt.centered, data = d.cats)

lm.cats.BIS <- lm(Hwt ~ Bwt.centered, data = d.cats)
coef(lm.cats.BIS)["(Intercept)"]
summary(lm.cats.BIS)


# 1.2 Question - Consider gender and use "males" as reference

lm.cats2 <- lm(Hwt ~ Bwt + Sex, data = d.cats)
coef(lm.cats2)

d.cats$Sex.relevel <- relevel(d.cats$Sex, ref = "M")
levels(d.cats$Sex)
levels(d.cats$Sex.relevel)

lm.cats2.BIS <- lm(Hwt ~ Bwt + Sex.relevel, data = d.cats)
coef(lm.cats2.BIS)


# 1.3 Question - Meaning of coefficients

# coef of Bwt in lm.cats: the average weight of cat increases about 4 
# when the body weight of cat increases by 1 unit.

# coef of Bwt in lm.cats2: the average weight of cat increases about 4
# when the body weight of cat increases by 1 unit and other predictors are fixed.


# 1.4 Question - Create Bwt as categorical variable
quantiles.Bwt <- quantile(d.cats$Bwt)
quantiles.Bwt

d.cats$Bwt.Class <- cut(d.cats$Bwt, breaks = quantiles.Bwt, include.lowest = T)

# Check how many observations are present in each class
table(d.cats$Bwt.Class)

lm.cats3 <- lm(Hwt ~ Sex + Bwt.Class, data = d.cats)
coef(lm.cats3)

drop1(lm.cats3, test = "F")

summary(lm.cats3)


# 1.5 Question - A post-hoc test

# library(multcomp, mask.ok = geyser)
require(multcomp)
glht.1 <- glht(lm.cats3, linfct = mcp(Bwt.Class = "Tukey"))
summary(glht.1)
