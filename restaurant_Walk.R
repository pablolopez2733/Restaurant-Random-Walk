# Load libraries
library(tidyverse)
library(gganimate)
library(ggimage)
library(dplyr)
library(gifski)

# Number of iterations/steps
n <- 20 

# Random walk for 4 restaurants
df <- tibble(
  ite = 0:n,
  Player1 = c(0, rnorm(n, mean = 1)),
  Player2 = c(0, rnorm(n, mean = 1)),
  Player3 = c(0, rnorm(n, mean = 1)),
  Player4 = c(0, rnorm(n, mean = 1))) %>%
  gather(player, score, Player1:Player4) %>%
  arrange(player, ite) %>%
  group_by(player) %>%
  mutate(totalScore = cumsum(score),
         size = ifelse(ite == n, 7, 0)) # This is used to size the labels

#Dataframe for images
player <- c("Player1","Player2","Player3","Player4")
images <- c("https://upload.wikimedia.org/wikipedia/en/thumb/0/0f/Wingstop_logo.svg/1920px-Wingstop_logo.svg.png",
            "https://upload.wikimedia.org/wikipedia/commons/thumb/0/03/RGB_Blue_Dominos_rechts_5000px.png/1920px-RGB_Blue_Dominos_rechts_5000px.png",
            "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b5/Carl%27s_Jr_logo.svg/1920px-Carl%27s_Jr_logo.svg.png",
            "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Sushi_Roll_Logo.svg/1920px-Sushi_Roll_Logo.svg.png")

img <- data.frame(player,images)

# Add images to generated data
df <- df %>% 
  left_join(img, by = c('player' = 'player'))

# Plot
g <- ggplot(df, aes(x = player, y = totalScore)) +
  #geom_label(aes(label = player, fill = player, size = size)) +
  #geom_label(aes(label = player, fill = player, size = size)) +
  geom_image(aes(image = images), size = 0.15, by = "width", asp = 16/9) +
  theme_bw() +
  theme(legend.position = 'none',
        plot.background = element_rect(fill = "#efe9e6"),
        panel.background = element_rect(fill = "#efe9e6"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "lightgrey"),
        axis.line.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))+
  annotate(geom = 'segment', y = Inf, yend = Inf, color = 'black', x = -Inf, xend = Inf, linetype = 'dashed')+
  #scale_fill_brewer(palette = "Spectral") +
  labs(title = "Random walk race",
       subtitle = "Iteration: {closest_state} of 20",
       x = NULL,
       y = NULL,
       caption = "Random walk simulated for 20 iterations") +
  transition_states(ite, transition_length = 1, state_length = 1, wrap = FALSE) 
  #shadow_wake(wake_length = .05, wrap = FALSE, alpha = 0.02, size = 0.2)
 
animate(g,
        renderer = gifski_renderer(loop = T),
        duration = 8,
        start_pause = 5,
        end_pause = 20,
        fps = 20,
        height = 1000, width = 1000,
        res = 150
        )

anim_save('race_res.gif')