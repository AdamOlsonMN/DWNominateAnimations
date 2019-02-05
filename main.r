#### Test out gganimate on ideology data

library(tidyverse)
library(gganimate)

# Load Data
DWNominate <- read_csv("https://voteview.com/static/data/out/members/HSall_members.csv")

# Clean Data
## This removes the very small parties -- based on having fewer than five members in the post war congresses
DWNominate %>%
  filter(
    congress > 78,
    chamber != "President"
  ) %>%
  mutate(
    party_code = as.factor(party_code)
  ) %>%
  group_by(
    congress,
    party_code
  ) %>%
  filter(
    n() > 5
  ) %>%
  ungroup() -> finaldata

# Draw the first Graph!
finaldata %>%
  ggplot(
    aes(
      x = nominate_dim1,
      group = party_code,
      fill = party_code
      )
    ) +
  geom_density(alpha = .7) +
  facet_wrap(vars(chamber)) +
  labs(
    title = "Ideology in the Post War Congress",
    caption = "DW-Nominate Scores",
    subtitle = 'Congress: {closest_state}',
    x = "",
    y = ""
    ) +
  scale_fill_manual(
    values = c("#003366", "#B22222"), 
    name = "",
    labels = c("Democrat", "Republican")
    ) +
  theme(legend.position="bottom") +
  transition_states(
    congress,
    transition_length = 2,
    state_length = 100
    ) +
  ease_aes('linear')

gganimate::anim_save(filename = "./output/20190119 - DW Nominate Animation Distro Scores.gif")

# Draw the second Graph!
finaldata %>%
  ggplot(
    aes(
      y = nominate_dim1,
      x = nominate_dim2,
      group = party_code,
      color = party_code
    )
  ) +
  geom_jitter() +
  facet_wrap(vars(chamber)) +
  labs(
    title = "Ideology in the Post War Congress",
    caption = "DW-Nominate Scores",
    subtitle = 'Congress: {closest_state}',
    x = "Second Dimension",
    y = "First Dimension"
  ) +
  scale_fill_manual(
    values = c("#003366", "#B22222"), 
    name = "",
    labels = c("Democrat", "Republican")
  ) +
  theme(legend.position="bottom") +
  transition_states(
    congress,
    transition_length = 2,
    state_length = 100
  ) +
  ease_aes('linear')

gganimate::anim_save(filename = "./output/20190119 - DW Nominate Animation Two Dim Scores.gif")