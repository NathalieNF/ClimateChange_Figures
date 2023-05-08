#########################################################
##  Temperature Spirals - Animated

#to make gganimate work with this version of R
install.packages("gifski")
library(gifski)
install.packages("png")
library(png)

library(tidyverse)  
library(glue)
install.packages("gganimate")
library(gganimate)


temp_month <- read_csv("Data/GLB.Ts+dSST_updated_in_april.csv", skip=1, na ="***") %>% 
  select(year = Year, all_of(month.abb)) %>% 
  pivot_longer(-year, names_to = "month", values_to = "mtemp") %>%  
  drop_na() 


# Making temperatures extend beyond current year
nextjan <- temp_month %>% 
  filter(month == "Jan") %>%
  mutate(year = year - 1, month = "next_Jan")


tabla <-  bind_rows( temp_month, nextjan) %>% 
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
         month_number = as.numeric(month)) %>%
  arrange(year,month)  %>% filter(year!= 1879) %>% 
  mutate(step_number = 1:nrow(.))  

# Add year label 
annotation <- tabla %>%
  slice_max(year)  %>%
  slice_max(month_number)


temp_lines <- tibble(
  x = 12, # December
  y = c(1, 1.5, 2.0),
  labels= c("1\u00B0C", "1.5\u00B0C", "2\u00B0C"))

# Month-labels tangential to the spirals
month_labels <- tibble( x = 1:12,
                        labels = month.abb,
                        y= 2.7)



aspiral <- tabla %>% ggplot(aes(x = month_number, y = mtemp, group = year, 
                                color = year)) +
  
  geom_rect(aes(xmin = 1, xmax = 13, ymin = -2, ymax = 2.4), color = "black",
            fill = "black", inherit.aes = FALSE) + 
  
  geom_hline(yintercept = c(1, 1.5, 2.0), color = "red") +
  
  geom_label(data = temp_lines, aes(x=x, y=y, label= labels),
             color= "red", fill= "black", label.size = 0,
             inherit.aes=FALSE) +
  
  geom_text(data = month_labels, aes(x=x , y=y, label = labels),
            color="white", inherit.aes= FALSE,
            angle = seq(360 - 360/12, 0, length.out = 12), size = 5)  +
  
  geom_label(aes(x =1, y = -1.40, label = year),
             size= 7, color = "white" , fill = "black",
             label.padding = unit(35, "pt"),
             label.size = 0) +  
  
  geom_line() + 
  
  scale_x_continuous(breaks =1:12, labels = month.abb, 
                     sec.axis = dup_axis(name = NULL, labels = NULL),
                     expand = c(0,0)) +
  
  scale_y_continuous(breaks = seq(-2,2,0.2),
                     limits = c(-2,2.7), 
                     sec.axis = dup_axis(name= NULL, labels = NULL),
                     expand = c(0,-0.6)) +
  
  scale_color_viridis_c(breaks = seq(1880,2020,20),
                        guide= "none")  +
  
  coord_polar(start = 2*pi/12)  +
  
  labs(x=NULL, 
       y = NULL,
       title =  glue("Global Temperature Change ({min(temp_month$year)}-{max(temp_month$year)})"),
       subtitle = "Data source:NASA's Goddard Institute for Space Studies(GISS)") +
  
  theme(
    panel.background = element_rect(fill = "black", size = 1),
    plot.background = element_rect(fill = "black", color = "black" ),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y =  element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(color = "white", face = "bold", family = "Roboto"),
    plot.title = element_text(color= "white", face = "bold", family = "Roboto",
                              hjust = 0.5),
    plot.subtitle = element_text(color = "white", size= 8, family = "Comic Sans MS",
                                 lineheight = 0.98, hjust = 0.5),
    
  )  + 
  
  transition_manual(frames = year, cumulative = TRUE)


animate(aspiral, width=3.8, height=4.3, unit="in", res=500)
 

anim_save("Figures/TemperatureSpiralsAnim_updated2.gif") # for QRG

# A Short video 
install.packages("av")
library(av)

animate(aspiral, width=3.8, height=4.3, unit="in", res=500,
        renderer = av_renderer("Figures/ClimateSpiralA_updated.mp4")
) 
