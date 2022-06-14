library(tidyverse)
library(ggplot2)


results <- read.csv("experiment-data.csv")

pruned_results <- results %>%
  select(-numberOfByzantineRobots, -byzantineSwarmStyle, -numberOfRobots, -numberOfBlacks) %>%
  mutate(approach = case_when(
    isClassical == 1 ~ 0,
    consensusAlgorithm == "pow" ~ 1,
    consensusAlgorithm == "poi" ~ 2
  ), difficulty= round(percentageOfBlackTiles/(100-percentageOfBlackTiles), 2)) %>%
  select(-consensusAlgorithm, -isClassical, -percentageOfBlackTiles)

summary <- pruned_results %>%
  group_by(id) %>%
  summarise(average_time_taken=mean(secondsTaken[numberOfWhites == 10]), exit_probability=sum(numberOfWhites), approach=mean(approach), decisionRule=mean(decisionRule), difficulty=mean(difficulty))

time_taken <- pruned_results %>%
  filter(numberOfWhites == 10)

relabeled <- summary %>%
  mutate(Approach = case_when(
    approach == 0 ~ "Classical",
    approach == 1 ~ "PoW",
    approach == 2 ~ "PoI"
  ), decisionRule = case_when(
    decisionRule == 1 ~ "DMVD",
    decisionRule == 2 ~ "DC",
    decisionRule == 3 ~ "DMMD"
  ))

time_taken <- time_taken %>%
  mutate(Approach = case_when(
  approach == 0 ~ "Classical",
  approach == 1 ~ "PoW",
  approach == 2 ~ "PoI"
), decisionRule = case_when(
  decisionRule == 1 ~ "DMVD",
  decisionRule == 2 ~ "DC",
  decisionRule == 3 ~ "DMMD"
))

bar_chart <- ggplot(data=relabeled, aes(fill=Approach, x=difficulty, y=exit_probability)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Exit Probability vs. Difficulty for different Decision Rules ", x="Difficulty", y="Exit Probability") +
  facet_wrap(~decisionRule)

bar_chart

box_plot <- ggplot(data=time_taken, aes(fill=Approach, x=as.factor(difficulty), y=secondsTaken)) +
  geom_boxplot() +
  labs(title="Consensus Time vs. Difficulty for different Decision Rules ", x="Difficulty", y="Consensus Time") +
  facet_wrap(~decisionRule)
box_plot
