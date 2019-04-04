load("clean_epistasis.rda")

gene1only <- subset(wings, gene2 == "wt")

#variance covariance matrix 

cov(gene1only[ ,8:11])

#visual variance covariance matrix
pairs(gene1only[, 8:11],
      pch = ".", gap = 0)


scatterplotMatrix( ~ L2s + L3s + L4s + L5s | interaction(gene1, background), 
                   ellipse = TRUE, data = gene1only, gap = 0,
                   plot.points = T, pch = 20, cex  = 0.5)

#examine the eigenvalues of the covariance matri - you don't want really small values
#check to see if the covariance matrix was not of full rank

eig_vals <- svd(cov(gene1only[, 8:11]))$d

det(cov(gene1only[, 8:11]))

mlm_fit1 <- lm(as.matrix(gene1only[,8:11]) ~ background*gene1*sex, data = gene1only)
class(mlm_fit1)

summary(manova(mlm_fit1))

summary(manova(mlm_fit1), 
        test = "Wilks")

Anova(mlm_fit1)

coef(mlm_fit1)


