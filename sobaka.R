list <- list.files(path="D:\\Table", full.names=TRUE, recursive=TRUE)

for(j in 1:length(list)){
	if(substr(list[j], nchar(list[j])-7, nchar(list[j])) != "stat.csv"){
		t <- read.csv2(list[j])
		statistic1 <- numeric(length(names(t))-3)
		pvalue1 <- numeric(length(names(t))-3)
		for(i in 4:(length(names(t)))){
			test <- chisq.test(t[,4], t[,i])
			statistic1[i-3] <- test$statistic
			pvalue1[i-3] <- test$p.value
		}
		statistic2 <- numeric(length(names(t))-3)
		pvalue2 <- numeric(length(names(t))-3)
		for(i in 4:(length(names(t)))){
			if(i==4){
				test <- chisq.test(t[,i], t[,i])
			}else{
				test <- chisq.test(t[,i-1], t[,i])
			}
		statistic2[i-3] <- test$statistic
		pvalue2[i-3] <- test$p.value
		}
		mean <- colMeans(t[,4:(length(names(t)))])
		mad <- function(x){
  			mean(abs(x-mean(x)))
		}
		m <- apply(t[,4:(length(names(t)))], 2, mad)
		sd <- apply(t[,4:(length(names(t)))], 2, sd)
		time <- as.numeric(substr(names(t)[4:length(names(t))], 2, nchar(names(t)[4:length(names(t))])))
		s <- c("", "", "", "", "correlation", round(cor(time, mean, method="spearman"), digits=3), round(cor.test(time, mean, method="spearman")$p.value, digits=3), "", "", "", "", "", "", "", "statistic", "p-value-1", "statistic", "p-value-2", "mean", "m", "sd")
		s <- matrix(s, nrow=7, ncol=3)
		s <- as.data.frame(s)
		names(s) <- names(t[, 1:3])
		s <- rbind(t[, 1:3], s)
		t <- cbind(s, rbind(t[,4:(length(names(t)))], statistic1, round(pvalue1, digits=3), statistic2, round(pvalue2, digits=3), mean, m, round(sd, digits=3)))
		write.csv2(t, file=paste0(substr(list[j], 1, nchar(list[j])-4), " stat.csv"))
	}
}
