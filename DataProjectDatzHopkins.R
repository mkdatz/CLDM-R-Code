# Created by Michael Datz
library(tidyverse)
filePart1 <- read.csv("~/Downloads/general_AbsTaskData(1).csv")
filePart2 <- read.csv("~/Downloads/general_AbsTaskData(2).csv")
filePart3 <- read.csv("~/Downloads/general_AbsTaskData(3).csv")
filePart4 <- read.csv("~/Downloads/general_AbsTaskData(4).csv")
filePart5 <- read.csv("~/Downloads/general_AbsTaskData(5).csv")
filePart6 <- read.csv("~/Downloads/general_AbsTaskData(6).csv")
filePart7 <- read.csv("~/Downloads/general_AbsTaskData(7).csv")
allFiles <- rbind(filePart1, filePart2, filePart3, filePart4, filePart5, filePart6, filePart7)
# Creating lists to take out of the data
admins = c(190, 316, 307, 308, 305, 310, 314, 334)
drops = c(47, 136, 154, 194, 183, 249, 281, 340, 382, 406, 480, 557, 69, 76, 105, 
          160, 203, 208, 256, 271, 311, 478, 490, 532, 543, 298, 299, 236, 138, 
          165, 3, 27, 111, 24, 84, 128, 134, 118, 228, 214, 226, 235, 321, 417, 
          371, 412, 415, 448, 451, 461, 467, 486, 487, 494, 511, 571)
# Cutting out users that are admins or drops
# subset(allFiles, !(usernum %in% admins) & !(usernum %in% drops) & finished_study != 1) -> allFiles_Trimmed
# Recoding variables to proper values
allFiles %>%
  subset(!(usernum %in% admins) & !(usernum %in% drops) & (finished_study == 1)) %>%
  select(-prior_strength, -prior_direction) %>%
  mutate(
    prior_strength_direction = ifelse(prior_strength_direction == -11, NA, prior_strength_direction), 
    final_strength_direction = ifelse(final_strength_direction == -11, NA, final_strength_direction),
    rt_final_strength = ifelse(rt_final_strength == -9, NA, rt_final_strength),
    final_cell_a = ifelse(final_cell_a == -9, NA, final_cell_a),
    final_cell_b = ifelse(final_cell_b == -9, NA, final_cell_b),
    final_cell_c = ifelse(final_cell_c == -9, NA, final_cell_c),
    final_cell_d = ifelse(final_cell_d == -9, NA, final_cell_d),
    rt_final_frequency = ifelse(rt_final_frequency == -9, NA, rt_final_frequency)
  ) -> allFiles_Trimmed
# OUR CODE
# Creating a subset where condition and longterm_condition match
 # subset(allFiles_Trimmed) -> dataProjectDF
# Use ifelse to go down the columns and recode values according to the number
# final_cell_a,b,c,d for the final estimates
# group by condition on new dataframe
allFiles_Trimmed %>%
  mutate(
    actualCellA = ifelse(condition == "preventive", 3, 
                         ifelse(condition == "generative", 9, 
                                ifelse(condition == "outcomeDensity", 9, 10))),
    
    actualCellB = ifelse(condition == "preventive", 9,
                         ifelse(condition == "generative", 3,
                                ifelse(condition == "outcomeDensity", 3, 6))),
    actualCellC = ifelse(condition == "preventive", 9,
                         ifelse(condition == "generative", 3,
                                ifelse(condition == "outcomeDensity", 3, 5))),
    actualCellD = ifelse(condition == "preventive", 3,
                         ifelse(condition == "generative", 9,
                                ifelse(condition == "outcomeDensity", 9, 3)))
  ) -> dataProjectDF
subset(dataProjectDF, condition == longterm_condition) -> dataProjectDF
dataProjectDF %>%
  mutate(
    estimationA = ((final_cell_a - actualCellA)/actualCellA) * 100,
    estimationB = ((final_cell_b - actualCellB)/actualCellB) * 100,
    estimationC = ((final_cell_c - actualCellC)/actualCellC) * 100,
    estimationD = ((final_cell_d - actualCellD)/actualCellD) * 100
  ) -> dataProjectDF

dataProjectDF %>%
  group_by(condition, task_length) %>%
  summarize(
    A = mean(estimationA),
    B = mean(estimationB),
    C = mean(estimationC),
    D = mean(estimationD)
  ) %>% mutate(
    actualCellA = ifelse(condition == "preventive", 3, 
                         ifelse(condition == "generative", 9, 
                                ifelse(condition == "outcomeDensity", 9, 10))),
    
    actualCellB = ifelse(condition == "preventive", 9,
                         ifelse(condition == "generative", 3,
                                ifelse(condition == "outcomeDensity", 3, 6))),
    actualCellC = ifelse(condition == "preventive", 9,
                         ifelse(condition == "generative", 3,
                                ifelse(condition == "outcomeDensity", 3, 5))),
    actualCellD = ifelse(condition == "preventive", 3,
                         ifelse(condition == "generative", 9,
                                ifelse(condition == "outcomeDensity", 9, 3)))
  ) %>% gather(
    key="cell_type", value="estimate", A, B, C, D
  ) %>% mutate(
    actual_val = ifelse(cell_type == "A", actualCellA, 
                        ifelse(cell_type == "B", actualCellB, 
                               ifelse(cell_type == "C", actualCellC, actualCellD)))
  ) %>% select(
    -actualCellA, -actualCellB, -actualCellC, -actualCellD
  ) -> estimation_means

# Scatterplot
# "lm" should change in the code
ggplot(data = estimation_means, mapping = aes(x = actual_val, y = estimate, color = condition)) +
  geom_point() +
  geom_smooth(method = "lm",  se=FALSE, color="black", formula = y ~ log(x)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ylim(-200, 200) +
  scale_color_manual(values=c("#000000", "#7D3C98", "#2E86C1", "#C71585")) +
  labs(title="Estimated vs. Actual Cell Frequencies by Condition",x="Actual Cell Frequency",y="Percent Overestimation")
#Add horizontal line and line of fit?
# Subsetting data to long term
subset(estimation_means, condition == "generative" & task_length == "long") -> generativeTableLong
subset(estimation_means, condition == "aCellBias" & task_length == "long") -> aCellBiasTableLong
subset(estimation_means, condition == "preventive" & task_length == "long") -> preventiveTableLong
subset(estimation_means, condition == "outcomeDensity" & task_length == "long") -> outcomeDensityTableLong
# Subsetting data to short term
subset(estimation_means, condition == "generative" & task_length == "short") -> generativeTableShort
subset(estimation_means, condition == "aCellBias" & task_length == "short") -> aCellBiasTableShort
subset(estimation_means, condition == "preventive" & task_length == "short") -> preventiveTableShort
subset(estimation_means, condition == "outcomeDensity" & task_length == "short") -> outcomeDensityTableShort

subset(dataProjectDF, condition == "generative") -> generativeTable
subset(dataProjectDF, condition == "aCellBias") -> aCellBiasTable
subset(dataProjectDF, condition == "preventive") -> preventiveTable
subset(dataProjectDF, condition == "outcomeDensity") -> outcomeDensityTable

# T-test data for long term
t.test(generativeTableLong$estimate, mu = 0, alternative = "two.sided")
t.test(aCellBiasTableLong$estimate, mu = 0, alternative = "two.sided")
t.test(preventiveTableLong$estimate, mu = 0, alternative = "two.sided")
t.test(outcomeDensityTableLong$estimate, mu = 0, alternative = "two.sided")
# T-test data to short term
t.test(generativeTableShort$estimate, mu = 0, alternative = "two.sided")
t.test(aCellBiasTableShort$estimate, mu = 0, alternative = "two.sided")
t.test(preventiveTableShort$estimate, mu = 0, alternative = "two.sided")
t.test(outcomeDensityTableShort$estimate, mu = 0, alternative = "two.sided")

# Generative
t.test(generativeTable$estimationA, mu = 0, alternative = "two.sided")
t.test(generativeTable$estimationB, mu = 0, alternative = "two.sided")
t.test(generativeTable$estimationC, mu = 0, alternative = "two.sided")
t.test(generativeTable$estimationD, mu = 0, alternative = "two.sided")
# A Cell Bias
t.test(aCellBiasTable$estimationA, mu = 0, alternative = "two.sided")
t.test(aCellBiasTable$estimationB, mu = 0, alternative = "two.sided")
t.test(aCellBiasTable$estimationC, mu = 0, alternative = "two.sided")
t.test(aCellBiasTable$estimationD, mu = 0, alternative = "two.sided")
# Preventive
t.test(preventiveTable$estimationA, mu = 0, alternative = "two.sided")
t.test(preventiveTable$estimationB, mu = 0, alternative = "two.sided")
t.test(preventiveTable$estimationC, mu = 0, alternative = "two.sided")
t.test(preventiveTable$estimationD, mu = 0, alternative = "two.sided")
# Outcome Density
t.test(outcomeDensityTable$estimationA, mu = 0, alternative = "two.sided")
t.test(outcomeDensityTable$estimationB, mu = 0, alternative = "two.sided")
t.test(outcomeDensityTable$estimationC, mu = 0, alternative = "two.sided")
t.test(outcomeDensityTable$estimationD, mu = 0, alternative = "two.sided")

# NEW CODE
# Subset data by what the actual cell values are equal to
# Maybe use different dataframe? Need both the actual_val and final_cell_a
subset(estimation_means, actual_val == 3) -> valThree
subset(estimation_means, actual_val == 5) -> valFive
subset(estimation_means, actual_val == 6) -> valSix
subset(estimation_means, actual_val == 9) -> valNine
subset(estimation_means, actual_val == 10) -> valTen

t.test(valThree$estimate, mu = 0, alternative = "two.sided")
t.test(valFive$estimate, mu = 0, alternative = "two.sided")
t.test(valSix$estimate, mu = 0, alternative = "two.sided")
t.test(valNine$estimate, mu = 0, alternative = "two.sided")
t.test(valTen$estimate, mu = 0, alternative = "two.sided")

# T-test data for long term
t.test(generativeTableLong$estimate, mu = 0, alternative = "two.sided")
t.test(aCellBiasTableLong$estimate, mu = 0, alternative = "two.sided")
t.test(preventiveTableLong$estimate, mu = 0, alternative = "two.sided")
t.test(outcomeDensityTableLong$estimate, mu = 0, alternative = "two.sided")
# T-test data to short term
t.test(generativeTableShort$estimate, mu = 0, alternative = "two.sided")
t.test(aCellBiasTableShort$estimate, mu = 0, alternative = "two.sided")
t.test(preventiveTableShort$estimate, mu = 0, alternative = "two.sided")
t.test(outcomeDensityTableShort$estimate, mu = 0, alternative = "two.sided")
  
t.test(estimation_means$estimate, estimation_means$actual_val)
# t test for estimation against the actual values of the cells

# T test for the participant's raw value
# Preventive
t.test(preventiveTable$final_cell_a, mu = 3, alternative = "two.sided")
t.test(preventiveTable$final_cell_b, mu = 9, alternative = "two.sided")
t.test(preventiveTable$final_cell_c, mu = 9, alternative = "two.sided")
t.test(preventiveTable$final_cell_d, mu = 3, alternative = "two.sided")
# Outcome Density
t.test(outcomeDensityTable$final_cell_a, mu = 9, alternative = "two.sided")
t.test(outcomeDensityTable$final_cell_b, mu = 3, alternative = "two.sided")
t.test(outcomeDensityTable$final_cell_c, mu = 9, alternative = "two.sided")
t.test(outcomeDensityTable$final_cell_d, mu = 3, alternative = "two.sided")
# Generative
t.test(generativeTable$final_cell_a, mu = 9, alternative = "two.sided")
t.test(generativeTable$final_cell_b, mu = 3, alternative = "two.sided")
t.test(generativeTable$final_cell_c, mu = 3, alternative = "two.sided")
t.test(generativeTable$final_cell_d, mu = 9, alternative = "two.sided")
# ACellBias
t.test(aCellBiasTable$final_cell_a, mu = 10, alternative = "two.sided")
t.test(aCellBiasTable$final_cell_b, mu = 6, alternative = "two.sided")
t.test(aCellBiasTable$final_cell_c, mu = 5, alternative = "two.sided")
t.test(aCellBiasTable$final_cell_d, mu = 3, alternative = "two.sided")