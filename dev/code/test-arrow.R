
df <- tibble(
  xmin = c(0:3, 5),
  xmax = c(1:4, 8),
  ymin = 3,
  ymax = 4
)

df |>
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = 1:5)) +
  geom_arrow(head_length = 0.15) +
  scale_fill_gradient2() +
  coord_fixed()
