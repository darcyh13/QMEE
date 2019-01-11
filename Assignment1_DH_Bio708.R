


library("tidyverse")


download.file(url="https://raw.githubusercontent.com/darcyh13/QMEE/master/GeneticBackgroundVeinMutantInteractionsFinal.csv",
              destfile = "data/GeneticBackgroundVeinMutantInteractionsFinal.csv")

data <- read_csv("data/GeneticBackgroundVeinMutantInteractionsFinal.csv")

#make sure data looks like it got fully imported in
head(data)
str(data)


#omit NAs for simplicity for now
data <- na.omit(data)

#find mean of Wing Lengths of Ore 
data %>%
  filter(background == "Ore") %>%
  pull(WL) %>%
  mean()
  
#find mean of Wing Lengths of Sam 
data %>%
  filter(background == "Sam") %>%
  pull(WL) %>%
  mean()



