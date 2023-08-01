require(tidyverse)
require(readxl)
tps <- read_excel("posts/three-party-system/tps.xlsx")
tps <- tps |>
  filter(Election <= 1979) |>
  filter(Party %in% c("CON", "LAB", "LD"))

my_colors <- c("blue", "red", "gold3")

tps_graph <- ggplot(tps, aes(x = Election, y = Seats, color = Party)) +
  geom_step() +
  theme_minimal() +
  scale_color_manual(values = my_colors) +
  labs(title = "Seats in UK House of Commons")

tps_graph