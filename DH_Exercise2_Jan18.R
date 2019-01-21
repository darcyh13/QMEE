library("tidyverse")

#setwd("~/Data")

GeneticBackground_Interactions <- read_csv("GeneticBackgroundVeinMutantInteractionsFinal.csv")
#112 parsing failures - Due to expected vs actual 
#When I use read.csv it is adding rows(ask about this)

#str(GeneticBackground_Interactions)
#head(GeneticBackground_Interactions)
#tail(GeneticBackground_Interactions)

#need to change to factors
columns_to_factors <- c(1:3,6:7)
GeneticBackground_Interactions[,columns_to_factors] <- lapply(GeneticBackground_Interactions[,columns_to_factors], as.factor)


#make list of what occurs with what and how many times (Checks for replicates)
print(GeneticBackground_Interactions
      %>% group_by(background, individual,rep,gene1, gene2, L2, L3, L4, L5,WL, sex)
      %>% summarize(count = n())
      %>% filter(count>1)
)
#looks like no replicates

#make a histogram with wing length and vein measurements
#Check to see abnormalities

GeneticBackground_Interactions %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#looks reasonable



#save(GeneticBackground_Interactions, file = "GeneticBackground_Interactions.csv")
#save script
#For future...
#load("GeneticBackground_Interactions.csv")
#source("DH_exercise2_Jan18.R")

