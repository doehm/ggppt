
df <- tibble(
  xmin = 0:3+0.1,
  xmax = 1:4,
  ymin = 0.1,
  ymax = 1
)

df |>
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = 1:4)) +
  geom_cross(angle = 45) +
  scale_fill_gradient2() +
  coord_fixed()




