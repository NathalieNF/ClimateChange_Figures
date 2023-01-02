#########################################################
## Global Temperature have increased by over 1.2C \ Bars

library(tidyverse)  

# Using data from https://data.giss.nasa.gov/gistemp/
# We want the global mean over the whole year.
# So, we select the column with the mean from January to December J-D

read_csv("Data/GLB.Ts+dSST.csv",skip=1, na ="***") %>% 
  select(year = "Year",temp_mean = "J-D") %>% 
  drop_na() -> temper2

# For labeling only initial and final year we create a data frame with this traits

annotation <- temper2 %>% 
  arrange(year) %>%   
  slice(1,n())  %>% 
  mutate(temp_mean = 0, 
         x = year + c(-8,8))   

library(scales)
library(glue)


# Calculating the increase of degrees since 1880 
# Unicode for degrees temperature \u00B0

# Generate a variable of max and min of the data set 
max_data <- format(round(max(temper2$temp_mean), 2), nsmall=1)

min_year <- min(temper2$year)


ggplot(data = temper2) + 
  aes(x = year, y = temp_mean, fill= temp_mean) + 
  geom_col(show.legend = FALSE) +  
  geom_text(data = annotation, aes(x=x, label=year), 
            colour= "white", fontface = 2) +
  geom_text(x=1880, y=1.05, hjust=0, 
            label= glue("Global Temperatures have increased by over {max_data}\u00B0C since {min_year}"),
            fontface = 3,
            color="white", family = "Comic Sans MS", size = 3.5)  +
  scale_fill_stepsn(colors=c("dark blue","white","dark red"), 
                    values=rescale(c(min(temper2$temp_mean),0, max(temper2$temp_mean))),
                    limits=c(min(temper2$temp_mean),max(temper2$temp_mean)), 
                    n.breaks =8) +   
  theme_void() +
  theme(plot.background = element_rect(fill = "black"),
        legend.text = element_text(colour="white")
  )

ggsave("Figures/TemperatureBar.png", width=7, height=5)# for QRG
