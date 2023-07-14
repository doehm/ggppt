
df <- tibble(
  xmin = 0:3,
  xmax = 1:4,
  ymin = 3,
  ymax = 4
)

df |>
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = 1:4)) +
  geom_arrow(angle = seq(0, 270, 90), rotate_at = "center", colour = "black") +
  geom_callout(aes(xmin = xmin+0.1, xmax = xmax-0.1, ymin = ymin+0.2-1, ymax = ymax-0.2-1), df) +
  scale_fill_gradient2() +
  coord_fixed()
