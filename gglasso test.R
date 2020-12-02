lambdas = 10^seq(-2,1,length=20)
y <- yAST
z <- scale(y, scale = FALSE)


# ridge

cvridge <- cv.glmnet(x,z,alpha=0,lambda=lambdas,nfolds=45)

df=data.frame(lambdas=cvridge$lambda, cvm=cvridge$cvm, 
              scaled=-log10((cvridge$cvm)/(cvridge$cvm[1])), rsqs=1-cvridge$cvm/var(z))


png(filename="~/plots/ast_ridge.png", width=400, height=250)
ggplot(data=df,aes(x=round(log10(lambdas),2), y=cvm)) + # lambdas vs. errors 
  geom_point(mapping=aes(size=0.1, color='skyblue3')) +  # resize & recolor the dots
  theme(text = element_text(size=12),legend.position="none") +  # remove the legend
  scale_color_manual(values = c("skyblue3")) + # set color
  labs(y="CV(ridge,lambdas)", x = "log10(lambdas)") + # set label
  scale_x_continuous(breaks=round(log10(lambdas)[seq(1,length(lambdas), 2)],digits=1), 
                   sec.axis = dup_axis(labels=cvridge$nzero[seq(1,length(lambdas), 2)],name = "Number of selected predictors")) + 
  # round lambdas & display number of selected predictors
  geom_vline(data.frame(selected=c(log10(cvridge$lambda.min),log10(cvridge$lambda.1se))),
             mapping=aes(xintercept=selected),linetype="dashed",color = 'skyblue4') # lambda.min and lambda.1se
dev.off()


# lasso

cvlasso <- cv.glmnet(x,z,alpha=1,lambda=lambdas, nfolds=45)
df=data.frame(lambdas=cvlasso$lambda, cvm=cvlasso$cvm, 
              scaled=-log10((cvlasso$cvm)/cvlasso$cvm[1]), rsqs=1-cvlasso$cvm/var(z))

png(filename="~/plots/ast_lasso.png", width=400, height=250)
ggplot(data=df,aes(x=log10(lambdas),y=cvm)) + 
  geom_point(mapping=aes(size=0.1, color='skyblue3')) + 
  theme(text = element_text(size=12),legend.position="none") + 
  scale_color_manual(values = c("skyblue3")) +
  labs(y="CV(lasso,lambdas)", x = "log10(lambdas)") +
  scale_x_continuous(breaks=round(log10(lambdas)[seq(1,length(lambdas), 2)],digits=1), 
                     sec.axis = dup_axis(labels=rev(cvlasso$nzero[seq(1,length(lambdas), 2)]),name = "Number of selected predictors")) +
  geom_vline(data.frame(selected=c(log10(cvlasso$lambda.min),log10(cvlasso$lambda.1se))),
             mapping=aes(xintercept=selected),linetype="dashed",color = 'skyblue4')
dev.off()


# gglasso

# define group index
group <- rep(1:7641,each=8)

cv_gglasso <- cv.gglasso(x,z,group=group,lambda = lambdas, 
                 pred.loss = c("misclass","loss","L1", "L2"), nfolds=45)
df=data.frame(lambdas=cv_gglasso$lambda, cvm=cv_gglasso$cvm, 
              scaled=-log10((cv_gglasso$cvm)/cv_gglasso$cvm[1]), rsqs=1-cv_gglasso$cvm/var(z))

nzeros <- numeric(length(lambdas))
for (i in 1:length(lambdas)){
  nzeros[i] <- sum(coef(cv_gglasso, s=lambdas[i])!=0)-1
}

png(filename="~/plots/ast_gglasso.png", width=400, height=250)
ggplot(data=df,aes(x=log10(lambdas),y=cvm)) + 
  geom_point(mapping=aes(size=0.1, color='skyblue3')) + 
  theme(text = element_text(size=12),legend.position="none") + 
  scale_color_manual(values = c("skyblue3")) +
  labs(y="CV(group lasso,lambdas)", x = "log10(lambdas)") +
  scale_x_continuous(breaks=round(log10(lambdas)[seq(1,length(lambdas), 2)],digits=1), 
                     sec.axis = dup_axis(labels=nzeros[seq(1,length(lambdas), 2)],name = "Number of selected predictors")) +
  geom_vline(data.frame(selected=c(log10(cv_gglasso$lambda.min),log10(cv_gglasso$lambda.1se))),
             mapping=aes(xintercept=selected),linetype="dashed",color = 'skyblue4')
dev.off()


###

coefgglasso <- coef(cv_gglasso)

ind <- which(coef(cv_gglasso, s="lambda.min")!= 0)
selected<-rownames(coefgglasso)[ind]
selected[seq(2, length(selected), 8)]

###

fit <- glmnet(x,z,alpha=1,lambda=0.1)
