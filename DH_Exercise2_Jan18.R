library("tidyverse")

#setwd("~/Data")

GeneticBackground_Interactions <- read_csv("GeneticBackgroundVeinMutantInteractionsFinal.csv")
#112 parsing failures - Due to expected vs actual 
#When I use read.csv it is adding rows(ask about this)

# JD: How many rows do you think you should have?
# The .csv has most rows matching the header, but many rows with 
# hundreds or thousands of copies of row information inside.
# We can work on this programatically, but better if you can go upstream and get a clean .csv

#str(GeneticBackground_Interactions)
#head(GeneticBackground_Interactions)
#tail(GeneticBackground_Interactions)

#need to change to factors
columns_to_factors <- c(1:3,6:7)
GeneticBackground_Interactions[,columns_to_factors] <- lapply(GeneticBackground_Interactions[,columns_to_factors], as.factor)

# JD: This would be great code if you had dozens (although even though I might use names instead of numbers
# With just a few, I would prefer to do this with tidy verbs:
# mutate(background=as.factor(background)), etc.
# Always best to avoid numbers as they are not readable, and somebody may change the input file format

#make list of what occurs with what and how many times (Checks for replicates)
print(GeneticBackground_Interactions
      %>% group_by(background, individual,rep,gene1, gene2, L2, L3, L4, L5,WL, sex)
      %>% summarize(count = n())
      %>% filter(count>1)
)
#looks like no replicates
## JD: You're looking at the wrong scale. You should group only by the 
## key variables. 

#make a histogram with wing length and vein measurements
#Check to see abnormalities

GeneticBackground_Interactions %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#looks reasonable
# JD: Looks like there's a value of L5 that you should be worried about, no?

## JD: save and load make .RData files, not .csv
#save(GeneticBackground_Interactions, file = "GeneticBackground_Interactions.csv")
#save script
#For future...
#load("GeneticBackground_Interactions.csv")
#source("DH_exercise2_Jan18.R")

## JD: score 2. (1=poor, 2=fine, 3=excellent)

