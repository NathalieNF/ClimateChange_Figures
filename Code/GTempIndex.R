#########################################################
## Global Temperature Index Plot

library(tidyverse)  


# Upload data
# Using data from https://data.giss.nasa.gov/gistemp/
# We notice that the first raw contains the title of the database
# Some cells also have "***" instead of values, so we assume they are NA
# Considering that, we upload the data

gtemp <- read_csv("Data/GLB.Ts+dSST.csv",skip=1, na ="***")

# We want the global mean over the whole year.
# So, we select the column with the mean from January to December J-D

temper <- select(gtemp,Year,temp_mean = "J-D") %>% 
  drop_na()


###########


ggplot(data = temper) +
  aes( x = Year, y = temp_mean) +
  geom_line(aes(color = "1"), linewidth = 0.8, show.legend = FALSE) +
  geom_point(fill= "white", aes(color = "1"), shape= 21, show.legend = TRUE) +
  geom_smooth(se = FALSE , aes(color = "2"), size = 0.8, span = 0.18, show.legend = FALSE) +
  scale_x_continuous(breaks= seq(1880,2030,20), expand = c(0,0)) +
  scale_y_continuous(limits = c(-0.5, 1.5), expand = c(0,0)) +
  theme_light() +
  scale_color_manual(name= NULL, 
                     breaks = c(1,2),
                     values = c("dark orange", "dark blue"),
                     labels= c("Annual mean", "Lowes smothing"),
                     guide = guide_legend(override.aes = list(shape = 15, size = 3))) +
  labs( x = "Year",
        y = "Temperature Anomaly (C)",
        title = "GLOBAL LAND-OCEAN TEMPERATURE INDEX",
        subtitle = "Data source : NASA's Goddard Institute for Space Studies(GISS)\nCredit: NASA/GISS") +
  theme(
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(margin = margin(b=8), color="red", family = "Roboto", size = 18, face = "bold"),
    plot.subtitle = element_text(size= 11, margin = margin(b=13), family = "Comic Sans MS", lineheight = 0.98),
    axis.title.x = element_text(family ="Roboto", face = "bold", colour = "black", size = 14,
                                margin = margin(t = 8, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(family ="Roboto", face = "bold", colour = "black", size = 14,
                                margin = margin(t = 0, r = 8, b = 0, l = 0)),
    axis.text.x = element_text(family ="Roboto", face = "bold", colour = "black", size = 9.5,),
    axis.text.y = element_text(family ="Roboto", face = "bold", colour = "black", size = 10),
    legend.position = c(0.2, 0.81),  # position x and y
    legend.title = element_text(size = 1),
    legend.text = element_text(size= 11, family = "Roboto", colour = "black"),
    legend.key.height = unit(13, "pt"),
    legend.background = element_rect(fill = "transparent"),
    legend.margin = margin(0,0,0,0)
  )


ggsave("Figures/Temperature_Index.png", width = 8, height = 5) # for QRG




