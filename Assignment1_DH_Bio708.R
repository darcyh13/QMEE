library("tidyverse")

## BMB: this is *not* reproducible (you don't have a data/ directory)
## should comment this out (don't want to run it every time)
## download.file(url="https://raw.githubusercontent.com/darcyh13/QMEE/master/GeneticBackgroundVeinMutantInteractionsFinal.csv",
##              destfile = "data/GeneticBackgroundVeinMutantInteractionsFinal.csv")

## BMB: also not reproducible - there's no data/ in your repo directory
## data <- read_csv("data/GeneticBackgroundVeinMutantInteractionsFinal.csv")
data <- read_csv("GeneticBackgroundVeinMutantInteractionsFinal.csv")

## don't use data as a variable name
## don't leave head()/str() in your code
## BMB: what are all the "parsing failures"?
#make sure data looks like it got fully imported in
head(data)
str(data)


#omit NAs for simplicity for now
data <- na.omit(data)

#find mean wing lengths 
data %>%
  pull(WL) %>%
    mean()

## BMB: IMO this is "extreme tidyverse": could be
mean(data$WL)

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

## BMB: why not
data %>% group_by(background) %>% summarise(WL=mean(WL))
## ???

## BMB: this is fine: 2 (score 1=poor, 2=fine, 3=excellent).
## In future you *will* get dinged for submitting code that
##  can't immediately be reproduced (see data/ directory issue
##  above)


