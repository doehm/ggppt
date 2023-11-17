
df <- tibble(
  xmin = c(0, 0.5, 6),
  xmax = c(1, 4, 7),
  ymin = 0,
  ymax = 1
)

df |>
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = 1:3)) +
  geom_parallelogram(angle = 270, rotate_at = "center") +
  scale_fill_gradient2() +
  coord_fixed()



theta <- 0:360
x <- cos(theta*pi/180)
y <- sin(theta*pi/180)

tibble(
  x = x,
  y = y
) |>
  ggplot() +
  geom_point(aes(x, y)) +
  geom_segment(aes(x = 0, xend = cos(b*pi/180), y = 0, yend = sin(b*pi/180)), tibble(b = c(15, 30, 45, 60, 75, 90))) +
  coord_fixed()
