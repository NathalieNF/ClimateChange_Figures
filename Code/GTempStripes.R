#########################################################
## Warming stripes visualization of climate change

library(tidyverse)  

read_csv("Data/GLB.Ts+dSST.csv", skip=1, na ="***")%>% 
  select(year = "Year",temp_mean = "J-D") %>% 
  drop_na() -> temper2


library(scales)

ggplot(data = temper2) + 
  aes(x = year, y = 1, fill = temp_mean) + # y=1, so we have same size of bars/ patterns
  geom_tile(show.legend = FALSE) +
  scale_fill_stepsn(colors = c("#08306B","white","#67000D"),
                    values=rescale(c(min(temper2$temp_mean),0, max(temper2$temp_mean))),
                    n.break = 8) +
  coord_cartesian(expand = FALSE) +
  theme_void()  # a completely empty theme, useful for plots with non-standard coordinates or for drawings.

#ggsave("Graphs/Warm_Stripes_Raw.png", height = 4, width =8)


# The labelled version

library(glue)


ggplot(data = temper2) + 
  aes(x = year, y = 1, fill = temp_mean) +
  geom_tile(show.legend = FALSE) +
  scale_fill_stepsn(colors = c("#08306B","white","#67000D"),
                    values=rescale(c(min(temper2$temp_mean),0, max(temper2$temp_mean))),
                    n.break = 8) +
  scale_x_continuous(breaks = seq(1890, 2020, 30)) +
  coord_cartesian(expand = FALSE) +
  labs(title = glue("Global Temperature Change ({min(temper2$year)}-{max(temper2$year)})")) +
  theme_void()  + 
  theme(
    axis.text.x = element_text(color = "white", face = "bold", family= "Roboto",
                               margin = margin(t=5,b=10, unit= "pt")),
    plot.title = element_text(color = "white", face = "bold", family= "Roboto",
                              margin = margin(b=5,t=10, unit= "pt"),
                              hjust = 0.3),
    plot.background = element_rect(fill ="black")
  )


ggsave("Figures/Warm_Stripes_Final.png", height = 4, width =8) #for QRG


