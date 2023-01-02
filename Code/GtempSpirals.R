#########################################################
##  Temperature spirals

library(tidyverse)  
library(glue)


temp_month <- read_csv("Data/GLB.Ts+dSST.csv", skip=1, na ="***")%>% 
  select(year = "Year",all_of(month.abb)) %>% 
  pivot_longer(-year, names_to = "month", values_to = "mtemp") %>%  
  drop_na() 


# Temperatures extend beyond the current year
nextjan <- temp_month %>% 
  filter(month == "Jan") %>%
  mutate(year = year - 1, month = "next_Jan")


tabla <- temp_month %>%  bind_rows( temp_month, nextjan) %>% 
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
         month_number = as.numeric(month)) 

# Add year label 
annotation <- tabla %>%
  slice_max(year)  %>%
  slice_max(month_number)


temp_lines <- tibble(
  x = 12, 
  y = c(1.5, 2.0),
  labels= c("1.5\u00B0C", "2\u00B0C"))


# Month-labels tangential to the spirals
month_labels <- tibble( x = 1:12,
                        labels = month.abb,
                        y= 2.7)



tabla %>% ggplot(aes(x= month_number, y = mtemp, group = year, 
                     color= year)) +
  geom_col(data = month_labels, aes(x=x + 0.5, y=2.3), fill="black",
           width =  1  ,inherit.aes = FALSE)  +
  geom_col(data = month_labels, aes(x=x + 0.5, y=-2), fill="black",
           width =  1  ,inherit.aes = FALSE)  +  
  geom_hline(yintercept = c(1.5, 2.0), color = "red") +
  geom_line() +
  geom_point(data = annotation, aes(x=month_number, y= mtemp, color= year),
             size=2, inherit.aes= FALSE) +
  geom_label(data=temp_lines, aes(x=x,y=y, label= labels),
             color= "red", fill= "black", label.size = 0,
             inherit.aes=FALSE) +
  geom_text(data = month_labels, aes(x=x , y=y, label = labels),color="white", inherit.aes= FALSE,
            angle = seq(360 - 360/12, 0, length.out = 12), size = 5)  +
  geom_text(aes(x =1, y = -1.35, label = "2022"), size= 7) +  
  
  scale_color_viridis_c(breaks = seq(1880,2020,20),
                        guide= "none")  +
  scale_x_continuous(breaks =1:12, labels = month.abb, 
                     sec.axis = dup_axis(name = NULL, labels = NULL),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-2,2,0.2),
                     limits = c(-2,2.8), 
                     sec.axis = dup_axis(name= NULL, labels = NULL),
                     expand = c(0,-0.7)) +
  
  coord_polar(start = 2*pi/12)  +
  labs(x=NULL, 
       y = NULL,
       title =  glue("Global Temperature Change ({min(temp_month$year)}-{max(temp_month$year)})")) +
  theme(
    panel.background = element_rect(fill = "#444444", size = 1),
    plot.background = element_rect(fill = "#444444", color = "#444444" ),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y =  element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(color = "white", face = "bold", family = "Roboto"),
    plot.title = element_text(color= "white", face = "bold", family = "Roboto",
                              hjust = 0.5)
  )


ggsave("Figures/Temperature_Spirals.png", height = 8 , width = 8) #for QRG



