
df <- tibble(
  xmin = c(0, 1.33),
  xmax = c(1, 4),
  ymin = 0,
  ymax = 1
)

df |>
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = 1:2)) +
  geom_pentagon() +
  scale_fill_gradient2() +
  coord_fixed()


make_pentagon(df$xmin, df$xmax, df$ymin, df$ymax) |>
  ggplot() +
  geom_polygon(aes(x, y)) +
  coord_fixed()






r <- 2

y <- round(r*sin(theta*pi/180), 2)
x <- round(r*cos(theta*pi/180), 2)

theta <- seq(30, 330, 60)
x
y

tibble(x, y) |>
  ggplot() +
  geom_polygon(aes(x, y)) +
  coord_fixed()

xmin <- 0
xmax <- 4
ymin <- 0
ymax <- 1
dy <- ymax-ymin
beta <- tan(30*pi/180)*dy
tibble(
  x = c(xmin, beta, xmax, xmax-beta),
  y = c(ymin, ymax, ymax, ymin)
) |>
  ggplot() +
  geom_polygon(aes(x, y))
