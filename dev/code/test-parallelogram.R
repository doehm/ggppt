
df <- tibble(
  xmin = c(0, 0.5),
  xmax = c(1, 4),
  ymin = 0,
  ymax = 1
)

df |>
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = 1:2)) +
  geom_parallelogram() +
  scale_fill_gradient2() +
  coord_fixed()
