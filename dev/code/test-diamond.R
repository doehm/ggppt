
df <- tibble(
  xmin = 0:3+0.1,
  xmax = 1:4,
  ymin = 0,
  ymax = 1.5
)

df |>
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = 1:4)) +
  geom_diamond() +
  scale_fill_gradient2() +
  coord_fixed()
