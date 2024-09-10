#!/usr/local/bin/Rscript
#
# Jonas Grossmann <jg@fgcz.ethz.ch>
# 2024
# 
#

# Here we want to plot growth curves depending on different conditions and different populations

# data structure:
# Time	OD600_blank_corrected	Strain	Growth_Medium	Replicate	Experiment
# 0	-0.000449997	Cowan	TSB	1	exp1
# 0.500083333	-0.000549998	Cowan	TSB	1	exp1
# 1.000138889	-0.000474999	Cowan	TSB	1	exp1
# 1.500222222	-0.00045	Cowan	TSB	1	exp1
# 2.000277778	-0.00065	Cowan	TSB	1	exp1
# 2.500333333	-5.00E-05	Cowan	TSB	1	exp1
# 3.000416667	0.000375001	Cowan	TSB	1	exp1

# chatgpt: -> cracy! (pdf pasted!)
# 

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

data <- read_tsv("JE2_growth.txt")
str(data)

# minimal time differences for different replica.. round it first

data$Time <- round(data$Time, 2)
# data <- data |> filter(Experiment == "exp2" & Growth_Medium == "SN" | Experiment == "exp4" & Growth_Medium == "SN")
# data <- data |> filter(Experiment == "exp2")
# data <- data |> filter(Replicate == 1)

# write new input for students
write_tsv(x = data, file = "JE_growth_final.txt")

# aggregate with sd
aggregated_data2 <- data%>%
  group_by(Time, Growth_Medium) %>%
  summarize(
    mean_OD600 = mean(OD600_blank_corrected, na.rm = TRUE),
    sd_OD600 = sd(OD600_blank_corrected, na.rm = TRUE)
  )


ggplot(aggregated_data2, aes(x = Time, y = mean_OD600, group = Growth_Medium, color = Growth_Medium)) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_OD600 - sd_OD600, ymax = mean_OD600 + sd_OD600), width = 0.2) +
  labs(title = "Aggregated Growth Curve", x = "Time (h)", y = "Mean OD600 (Blank Corrected)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("blue", "red", "green"))






