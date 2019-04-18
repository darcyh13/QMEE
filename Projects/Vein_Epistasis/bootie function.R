bootie <- function(dat=gene1only, genename = "Egfr" ) {
  new_dat <- gene1only[sample(nrow(gene1only), nrow(gene1only), replace = T),  ]
  
  mod_new <- lm(L3s ~ background*sex, 
                  data = new_dat,
                  subset = gene1only$gene1 == genename)
  return(coef(mod_new)[1:2])
}


coef_out <- replicate(n = 100, bootie())
dim(coef_out)
ratio_coef <- coef_out[1,]/coef_out[2,]
quantile(ratio_coef, probs = c(0.025, 0.5, 0.975))
