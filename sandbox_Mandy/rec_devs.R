
rm(list = ls())

plot.new()
dev.off()

library(plotTimeSeries)
library(ggplot2)
library(tidyr)
library(dplyr)
library(pals)

d <- read.csv("../../data/intermediate/combined_recdev_trends.csv")

sedars <- as.numeric(gsub("[^0-9]", "", names(d)))
table(sedars)

dim(d)
names(d)

datdata <- as.integer(d$indicator)
d1 <- d[, 2:ncol(d)]
dim(d1)
cols <- glasbey(15)

matplot(datdata, d1, type = "h", col = cols)
abline(h = 0)

for (i in 1:ncol(d1)) { 
  clean_name <- gsub("\\.", " ", sub("_.*", "", names(d1)[i]))
  names(d1)[i] <- clean_name
  }
clean_name

# 1. Combine years with d1 and reshape to long format
df_long <- as.data.frame(d1) %>%
  mutate(Year = datdata) %>%
  pivot_longer(
    cols = -Year,
    names_to = "Species_Assessment",
    values_to = "RecDev"
  ) %>%
  filter(!is.na(RecDev))

# 2. Generate the stacked barplot
ggplot(df_long, aes(x = factor(Year), y = RecDev, fill = Species_Assessment)) +
  geom_col(position = "stack", width = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  labs(
    x = "Year",
    y = "Recruitment Deviations",
    fill = "Stock Assessment"
  ) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
    panel.grid = element_blank(),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1), color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

shapes <- setNames(rep(15:18, length.out = length(cols)), names(cols))



ggplot(df_long, aes(x = factor(Year), y = RecDev)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5, linetype = 2) +
  stat_summary(
    aes(group = 1),
    fun.data = mean_se, # or mean_sdl for +/- 1 SD
    geom = "ribbon",
    fill = "gray50",
    alpha = 0.25,
    color = NA
  ) +
  geom_point(
    aes(color = Species_Assessment, shape = Species_Assessment),
    size = 2.5,
    alpha = 0.8,
    position = position_jitter(width = 0.2, height = 0, seed = 42)
  ) +
  labs(
    x = "Year",
    y = "Recruitment Deviations",
    color = "Stock Assessment",
    shape = "Stock Assessment"
  ) +
  scale_color_manual(values = cols) +
  scale_shape_manual(values = shapes) +
  theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
    panel.grid = element_blank(),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(1), color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
