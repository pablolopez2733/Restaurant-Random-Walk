# Load libraries
library(tidyverse)
library(gganimate)
library(ggimage)
library(dplyr)

# Generate data
n <- 20 # Number of iterations/steps

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
         size = ifelse(ite == n, 6, 4)) # This is used to size the labels

#Dataframe for images

player <- c("Player1","Player2","Player3","Player4")
images <- c("https://upload.wikimedia.org/wikipedia/en/d/d6/Samus_Aran.png",
            "https://upload.wikimedia.org/wikipedia/en/d/d2/MGS2_Solid_Snake.png",
            "https://upload.wikimedia.org/wikipedia/en/a/a9/MarioNSMBUDeluxe.png",
            "https://upload.wikimedia.org/wikipedia/en/a/a6/Pok%C3%A9mon_Pikachu_art.png")

img <- data.frame(player,images)

df <- df %>% 
  left_join(img, by = c('player' = 'player'))



# Plot
ggplot(df, aes(x = player, y = totalScore)) +
  geom_hline(yintercept = 0, linetype ="dashed") +
  geom_label(aes(label = player, fill = player, size = size)) +
  geom_image(aes(image = images), size = 0.05, by = "width", asp = 16/9) +
  theme_minimal() +
  theme(legend.position = 'none') +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Random walk race",
       subtitle = "Iteration: {closest_state} of 20",
       y = "Score total",
       x = "Entity",
       caption = "Random walk simulated for 20 iterations") +
  transition_states(ite, transition_length = 1, state_length = 1, wrap = FALSE) +
  shadow_wake(wake_length = .1, wrap = FALSE)

anim_save('C:/Users/pablo/Desktop/GithubRepos/Restaurant-Random-Walk/race_char.gif')