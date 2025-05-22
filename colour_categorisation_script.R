

library(reshape2)
library(tidyr)
library(plyr)
library(ggplot2)
library(farver)
library(scales)
library(dplyr)


#loading data
DATA <- read.csv("...") #insert datafile
str(DATA)

#convert RGB to LCh using farver conversions
library(farver)
library(scales)
library(ggrepel)

DATA_rgb <- DATA[,c("r", "g", "b")]
DATA_lch <- convert_colour(DATA_rgb, from = 'rgb', to = 'lch',  white_from = "D65") 
DATA_hex <- encode_colour(DATA_lch, from = 'lch')
DATA <- cbind(DATA,DATA_lch)
DATA <- cbind(DATA,DATA_hex)


#categorising colours
DATA$colour_cat <- "elephant" 


#defining categories; h - hue; l - lightness; c - chroma
DATA$colour_cat[DATA$h > 280 & DATA$h <= 346] <- "purple"
DATA$colour_cat[DATA$h > 346 &  DATA$l > 60] <- "pink"
DATA$colour_cat[DATA$h > 346 &  DATA$l <= 60] <- "red"
DATA$colour_cat[DATA$h <= 45 &  DATA$l <= 60] <- "red"
DATA$colour_cat[DATA$h <= 45 &  DATA$l > 60] <- "pink"
DATA$colour_cat[DATA$h > 45 & DATA$h <= 72 &  DATA$l > 50] <- "orange"
DATA$colour_cat[DATA$h > 72 & DATA$h <= 105 &  DATA$l > 70] <- "yellow" 
DATA$colour_cat[DATA$h > 45 & DATA$h <= 72 &  DATA$l <= 50] <- "brown"
DATA$colour_cat[DATA$h > 72 & DATA$h <= 105 &  DATA$l <= 70] <- "brown" 
DATA$colour_cat[DATA$h > 105 & DATA$h <=166] <- "green"    
DATA$colour_cat[DATA$h > 166 & DATA$h <=220] <- "turquoise"
DATA$colour_cat[DATA$h > 220 & DATA$h <=280] <- "blue"


#if lightness ≤ 10 and chroma ≤10 define as "black"
#if lightness >= 95 and chroma ≤10 define as "white"
#all where chroma ≤ 10 define as "gray"
DATA$colour_cat[DATA$l <= 10 & DATA$c <= 10 ] <- "black"
DATA$colour_cat[DATA$l >= 95 & DATA$c <= 10] <- "white"
DATA$colour_cat[DATA$l < 95 & DATA$l >10 & DATA$c <=10 ] <- "gray"

str(DATA)


# SAVE dataset with colour categories
write.csv(DATA, "preferences_categorised.csv", row.names = FALSE)


### VISUALISATIONS

DATA <- DATA %>%
  group_by(colour_cat) %>%
  mutate(y = row_number()) %>%
  ungroup()

DATA <- DATA %>%
  mutate(colour_cat = factor(colour_cat, levels = c("red", "orange", "yellow", "brown", "green", "turquoise", "blue", "purple", "pink", "gray", "white", "black"))) %>%
  group_by(colour_cat) %>%
  mutate(y = row_number()) %>%
  ungroup()

ggplot(DATA, aes(x = colour_cat, y = y, color = DATA_hex)) +
  geom_point(size = 6) +
  scale_color_identity() +
  labs(title = "Favourite Colors") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) 



