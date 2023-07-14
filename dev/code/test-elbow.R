df <- tibble(
  x = c(0, 1),
  y = c(1, 0),
  grp = 1
)

df <- tibble(
  x = 0,
  xend = runif(4),
  y = 1,
  yend = runif(4)
)


df |>
  ggplot() +
  geom_elbow(aes(x = x, xend = xend, y = y, yend = yend, colour = xend),
             arrow = arrow(type = "closed", length = unit(0.03, "npc")))



df <- tibble(
  x = 0,
  xend = 1,
  y = 1,
  yend = seq(0.9, 0.1, length = 10)
)

df |>
  ggplot(aes(x = x, xend = xend, y = y, yend = yend, colour = yend)) +
  geom_elbow() +
  # scale_colour_gradient2() +
  coord_fixed()
