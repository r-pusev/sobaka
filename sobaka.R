t <- read.csv2("F:\\t.csv")
statistic <- numeric(length(names(t))-3)
pvalue <- numeric(length(names(t))-3)
for(i in 4:(length(names(t)))){
	test <- chisq.test(t[,4], t[,i])
	print(test)
	statistic[i-3] <- test$statistic
	pvalue[i-3] <- test$p.value
}
statistic <- statistic
pvalue <- pvalue
mean <- colMeans(t[,4:(length(names(t)))])
sd <- apply(t[,4:(length(names(t)))], 2, sd)
t <- rbind(t[,4:(length(names(t)))], statistic, pvalue, mean, sd)
write.csv2(t, file="F:\\out.csv")
