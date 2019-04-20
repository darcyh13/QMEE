bootie <- function(dat=gene1only, genename = "Egfr", vein = gene1only$L2s) {
  new_dat <- gene1only[sample(nrow(gene1only), nrow(gene1only), replace = T),  ]
  mod_new <- lm(vein ~ background*sex, 
                  data = new_dat,
                  subset = gene1only$gene1 == genename)
  return(coef(mod_new))
}


coef_out <- replicate(n = 100, bootie())
dim(coef_out)
ratio_coef_f <- coef_out[2,]/coef_out[1,]
quantile(ratio_coef_f, probs = c(0.5, 0.5, 0.95))
ratio_coef_m <- coef_out[4,]/coef_out[3,]
quantile(ratio_coef_m, probs = c(0.05, 0.5, 0.95))

