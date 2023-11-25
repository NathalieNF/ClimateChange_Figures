#########################################################
## Line plot of annual temperature anomalies - Animated

#to make gganimate work with this version of R
install.packages("gifski")
library(gifski)
install.packages("png")
library(png)

install.packages("tidyverse")
library(tidyverse)  
library(glue)

install.packages("gganimate")
library(gganimate)



temp_month <- read_csv("Data/GLB.Ts+dSST_update_nov.csv", skip=1, na ="***")%>% 
  select(year = "Year",all_of(month.abb)) %>% 
  pivot_longer(-year, names_to = "month", values_to = "mtemp") %>% 
  drop_na() 


lastdec <- temp_month %>%
  filter(month == "Dec") %>%
  mutate(year = year + 1, month = "last_Dec")

nextjan <- temp_month %>% 
  filter(month == "Jan") %>%
  mutate(year = year - 1, month = "next_Jan")


tabla <- bind_rows(lastdec, temp_month, nextjan) %>% 
  mutate(month = factor(month, levels = c("last_Dec", month.abb, "next_Jan")),
         month_number = as.numeric(month) -1,
         this_year = year == 2023) 

annotation <- tabla %>%
  slice_max(year)  %>%
  slice_max(month_number)

LinesA <- tabla %>% ggplot(aes(x= month_number, y = mtemp, group = year, 
                     color= year)) +
  geom_hline(yintercept = 0, color = "white", size = 1) +
  geom_point(size=2, 
             # Create a distinct grouping variable for geom_point 
             aes(group = seq_along(year)) ) +
  geom_line() +
  
  scale_color_viridis_c(breaks = seq(1880,2040,20),
                        guide= guide_colorbar(frame.colour = "white",
                                              frame.linewidth = 0.7))  +
  scale_x_continuous(breaks =1:12, labels = month.abb, sec.axis = dup_axis(name = NULL, labels = NULL)) +
  scale_y_continuous(breaks = seq(-2,2,0.2)) +
  scale_size_manual(breaks = c(FALSE, TRUE),
                    values = c(0.25, 1), guide = "none")  +
  coord_cartesian(xlim = c(1,12)) +
  labs(x=NULL, 
       y = "Temperature change since preindustrial time [\u00B0C]",
       title =  glue("Global Temperature Change ({min(temp_month$year)}-{max(temp_month$year)})"), 
       subtitle = "Data source:NASA's GISS"
    ) +
  theme(
    panel.background = element_rect(fill = "black", color = "white", size = 1),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white", face = "bold", family = "Roboto"),
    axis.ticks = element_line(color = "white"),
    axis.ticks.length = unit(-5, "pt"),
    axis.title = element_text(color = "white", face = "bold", family = "Roboto"),
    plot.title = element_text(color= "white", face = "bold", family = "Roboto",
                              hjust = 0.5),
    plot.subtitle = element_text(color = "white",face = "bold", family = "Roboto",
                                 hjust = 0.5, size = 10),
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA),
    legend.text = element_text(color = "white", face = 'bold'),
    legend.key.height = unit(53, "pt"),
    
  ) +
  transition_manual(frames = year, cumulative = TRUE) 

  
animate(LinesA, width = 5, height = 5, unit = "in", res = 500,
        renderer = gifski_renderer(loop = TRUE),
        nframes = 88)





anim_save("Figures/LinesAnimated.gif") # for QRG

# A Short video 
install.packages("av")
library(av)

animate(LinesA, width= 5, height= 5, unit="in", res=500,
        renderer = av_renderer("Figures/ClimateLinesA.mp4")
        ) 


