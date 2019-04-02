#I talked to Ian about the problems with that file and it was all kinds of fucked up. He gave me a new version that reads in clean 
#This also has those NAs replaced with wts so we dont need that line of code. 

wings_raw <- read_csv("RhomboidEpistasisWingMeasures_Fixes_2019.csv")

#Making the cols factors and releveling so everything is relitive to wt
columns_to_factors <- c("background", "gene1", "gene2", "rhomboid", "sex", "individual")
wings_raw[,columns_to_factors] <- lapply(wings_raw[,columns_to_factors], as.factor)

wings_raw$gene1 <- relevel(wings_raw$gene1, ref = "wt")
wings_raw$gene2 <- relevel(wings_raw$gene2, ref = "wt")


#omit NAs - indicates dead flies 
wings_raw <- na.omit(wings_raw)

#make list of what occurs with what and how many times (Checks for duplicates)
print(wings_raw
      %>% group_by(background, individual,rep,gene1, gene2, L2, L3, L4, L5,WL, sex)
      %>% summarize(count = n())
      %>% filter(count>1)
)

#looks like no duplicates

#want to make a new column with genotype 
wings_raw <- mutate(wings_raw,
                         genotype=interaction(gene1, gene2,rhomboid))
#checking for any obvious outliers

#hist
wings_raw %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#might be outliers in L5 and WL 

#boxplot for L5
(ggplot(wings_raw,aes(genotype, L5))
  + geom_boxplot()
  + stat_sum(colour="darkgray",alpha=0.5)
  + scale_size(breaks=1:2, range=c(3,6))
)

#remove outlier 
wings_raw<- subset(wings_raw, wings_raw[11]<5)

#double check outlier has been removed 
(ggplot(wings_raw,aes(genotype, L5))
  + geom_boxplot()
  + stat_sum(colour="darkgray",alpha=0.5)
  + scale_size(breaks=1:2, range=c(3,6))
)

(ggplot(wings_raw,aes(genotype, WL))
  + geom_boxplot()
  + stat_sum(colour="darkgray",alpha=0.5)
  + scale_size(breaks=1:2, range=c(3,6))
)

#remove outlier 

wings_raw<- subset(wings_raw, wings_raw[12]>4)

#double check outlier has been removed 

(ggplot(wings_raw,aes(genotype, WL))
  + geom_boxplot()
  + stat_sum(colour="darkgray",alpha=0.5)
  + scale_size(breaks=1:2, range=c(3,6))
)


#going to resale the veins relitive to wing lenght 
wings_scale <- (wings_raw
                         %>% mutate(L2s = L2/WL)
                         %>% mutate(L3s = L3/WL)
                         %>% mutate(L4s = L4/WL)
                         %>% mutate(L5s = L5/WL)
)


wings <- wings_scale

save(wings, 
     file = "clean_epistasis.rda")
