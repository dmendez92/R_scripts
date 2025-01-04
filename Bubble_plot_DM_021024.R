# R script to generate bubble plots based on bacterial species count.

# Set working directory.
path <- "your path (if using MS Windows, remember to separate file path with /)"
setwd(path)

# Load libraries.
library(tidyverse)
library(reshape2)
library(ggtext)
library(glue)

# Loading your data to RStudio.
my_data = read.csv("data_bubble_plot.csv", header = TRUE)

# Convert data frame from a "wide" format to a "long" format
converted_data = melt(my_data, id = c("Genus"))

# Mark the first word of 'Genus' for italics.
converted_data_alt <- converted_data %>% 
  mutate(Genus_name = word(Genus, 1,1, sep=" "),
         SP = word(Genus, -1,-1, sep=" ")) %>% 
  mutate(Bacteria_new = glue("<i>{Genus_name }</i> {SP}"))

# Make bubble plot.
bubble_plot = ggplot(converted_data_alt, aes(x = variable, y = reorder(Bacteria_new,value), size = value, fill = variable)) + 
  geom_point(shape = 21) + 
  scale_size(limits = c(0.1, 40), range = c(1,15), breaks = c(1,10,20,40)) + 
  scale_fill_manual(values=c("#398f8b", "#430f53"), guide = "none") +
  labs(x= "Source", y = "Genus", size = "Abundance")  +
  theme_bw() + # Comment this line if you don't want lines in the background
  theme(legend.key=element_blank(), 
        axis.text.x = element_text(colour = "black", size = 10),
        axis.title.x = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.y = ggtext::element_markdown(colour = "black", size = 10),
        axis.title.y = element_text(colour = "black", size = 12, face = "bold"), 
        legend.text = element_text(colour = "black", size = 10), 
        legend.title = element_text(size = 12, face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2), # Comment this line if you don't want a white background with black border
        legend.position = "right") + 
  scale_x_discrete(limits = rev(levels(converted_data_alt$variable)))

bubble_plot

# Saving the file as a high resolution PNG.
png('Bubble_plot.png', 
    width = 5 * 300, # multiplied by 300 to get 300 dpi
    height = 6 * 300, # multiplied by 300 to get 300 dpi
    bg = 'white', 
    res = 300)  # Set resolution to 300 dpi

bubble_plot

dev.off()
