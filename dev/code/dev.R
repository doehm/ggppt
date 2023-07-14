

tibble(
  xmin = 0,
  xmax = 1,
  ymin = 0,
  ymax = 1
) |>
  ggplot() +
  stat_poly_arrow(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), colour = "black")



df2 <- tibble(
  xmin = 0:8,
  xmax = 1:9,
  ymin = 0,
  ymax = 1,
  fill = letters[1:9],
  n = 1:9
)


df2 |>
  ggplot() +
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_arrow(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = n),
                  colour = "black", angle = seq(0, 360, length = 10), alpha = 0.5) +
  scale_fill_gradient2() +
  coord_fixed()

df2 |>
  ggplot() +
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_arrow(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = n),
                  colour = "black", angle = c(0, 45, 90, 135, 180, 225, 270, 315, 360), alpha = 0.5) +
  scale_fill_gradient2() +
  coord_fixed()

df2 |>
  ggplot() +
  geom_hline(yintercept = 0.5, linetype = 2) +
  geom_arrow(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = n),
                  colour = "black", angle = c(0, 45, 90, 135, 180, 225, 270, 315, 360),
                  rotate_at = "center") +
  scale_fill_gradient2() +
  coord_fixed()



# chevron -----------------------------------------------------------------

df3 <- tibble(
  xmin = (0:3)/2,
  xmax = (1:4)/2+0.45,
  ymin = 0,
  ymax = 1,
  fill = letters[1:4],
  n = 1:4
)

df3 |>
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = n)) +
  geom_chevron(colour = "black") +
  geom_chevron(data = tibble(xmin = 0, xmax = 2.45, ymin = 1.1, ymax = 2, n = 0), colour = "black") +
  scale_fill_gradient2() +
  coord_fixed()
