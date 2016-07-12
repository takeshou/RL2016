dat <- read.csv("data/survey.csv", na.strings = "")
dat <- read.csv("data/survey.csv", na.strings = "")
head(dat)
tail(dat)
names(dat)
summary(dat)
dat$sex[1]
dat$weight
summary(dat$height)
dat[1, 5]
dat[, 4]
summary(dat[, 3])

plot(dat$village)
hist(dat$income)
boxplot(dat$income)

hist(dat[, 2])
boxplot(dat[, 2])
plot(dat[, 5])

# 2変量の図示

plot(dat$village, dat$sex)
plot(dat$income ~ dat$village)
plot(dat$height, dat$weight)

plot(dat[, 6], dat[, 5])
plot(dat[, 7] ~ dat[, 6])
plot(dat[, 3], dat[, 4])

# カテゴリカル同士
xtabs(~ sex + village, data = dat)
chisq.test(dat$sex, dat$village)
fisher.test(dat$sex, dat$village)

# カテゴリカルと連続
plot(weight ~ sex, data = dat)
t.test(weight ~ sex, data = dat)
hist(dat$weight[dat$sex == "m"])
hist(dat$weight[dat$sex == "f"])

plot(height ~ village, data = dat)
oneway.test(height ~ village, data = dat)
pairwise.t.test(dat$height, dat$village)

# ノンパラメトリック
plot(income ~ sex, data = dat)
wilcox.test(income ~ sex, data = dat)

plot(income ~ village, data = dat)
kruskal.test(income ~ village, data = dat)
pairwise.wilcox.test(dat$income, dat$village)

# 連続同士
plot(dat$height, dat$weight)
cor.test(dat$height, dat$weight)
cor.test(~ height + weight, data = dat)

plot(dat$age, dat$income)
cor.test(dat$age, dat$income, method = "spearman")
cor.test(dat$age, dat$income, method = "kendall")
