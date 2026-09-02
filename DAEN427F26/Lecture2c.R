
pi <- sample(c(.2,.5,.8), 10000, replace=TRUE, prob=c(.10,.25,.65))
y <- rbinom(10000, size=6, prob=pi)
prop.table(table(pi[y == 1]))


pi <- sample(c(.2,.5,.8), 10000, replace=TRUE, prob=c(.10,.25,.65))
y <- rbinom(10000, size=6, prob=pi)
prop.table(table(pi[y == 5]))
