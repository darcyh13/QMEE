library("tidyverse")


GeneticBackground_Interactions <-read_csv("https://raw.githubusercontent.com/darcyh13/QMEE/master/GeneticBackgroundVeinMutantInteractionsFinal.csv")
#112 parsing failures - Due to expected vs actual 
#When I use read.csv it is adding rows(ask about this)

#str(GeneticBackground_Interactions)
#head(GeneticBackground_Interactions)
#tail(GeneticBackground_Interactions)


#make list of what occurs with what and how many times (Checks for replicates)
print(GeneticBackground_Interactions
      %>% group_by(background, individual,rep,gene1, gene2, L2, L3, L4, L5,WL, sex)
      %>% summarize(count = n())
      %>% filter(count>1)
)
#looks like no replicates


#make a histogram with wing length and vein measurements
#Check to see abnormalities
hist(GeneticBackground_Interactions$WL)

hist(GeneticBackground_Interactions$L2)

hist(GeneticBackground_Interactions$L3)

hist(GeneticBackground_Interactions$L4)

hist(GeneticBackground_Interactions$L5)

#looks reasonable

#save(GeneticBackground_Interactions, file = "GeneticBackground_Interactions.csv")
#save script

#For future...
#load("GeneticBackground_Interactions.csv")
#source("DH_exercise2_Jan18.R")

