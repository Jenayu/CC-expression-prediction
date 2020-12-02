# chromosome info

chr=data.frame(start=c(1,  554, 1108, 1561, 2005, 2452, 2874, 3329, 3717, 4107, 4490,
                       4928, 5285, 5654, 5980, 6283, 6575, 6880, 7158, 7401),
             stop=c(554, 1108, 1561, 2005, 2452, 2874, 3329, 3717, 4107, 4490,
                    4928, 5285, 5654, 5980, 6283, 6575, 6880, 7158, 7401,7641), 
             t=c('a','b','a','b', 'a','b', 'a','b', 'a','b','a','b','a','b', 'a','b', 'a','b', 'a','b'))


# genome scan

y=yAST
nfeat <- 7641
rsqs <- numeric(nfeat)
logP <- numeric(nfeat)
for (i in 1:nfeat){
  fit <- lm(y~x[, 8*(i-1) + 1:8])
  rsqs[i] <- summary(fit)$r.squared
  logP[i] <- -log10(anova(fit)$Pr[1])
}

# selected predictors

coef <- coef(cv_gglasso, cv_gglasso$lambda.1se)
coef_grp = coef[seq(2, nrow(coef), 8)]!=0

pdf(filename="C:/Users/Jennifer Chen/Desktop/honors project/plots/gscan_ast.pdf", width=1200, height=500)
idx <- 1:7641
idx <- idx[coef_grp]
graph <- ggplot() + 
  geom_rect(data=chr, mapping=aes(xmin=start, xmax=stop, ymin=-Inf, ymax=+Inf, fill=t),alpha = 1) + 
  scale_fill_manual(values = c("grey90", "white")) + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks=chr$start,labels=seq(1,20),name = "Chromosome number")
graph + geom_line(data.frame(markers=1:length(logP), logP=logP), mapping=aes(x= markers, y =logP)) +
  geom_vline(data.frame(markers = 1:length(idx), selected = idx),
             mapping=aes(xintercept=selected), linetype="dashed", color = 'tomato1') +
  labs(y="-log10(P)", x = "markers") 
dev.off()
