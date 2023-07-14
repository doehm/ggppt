
ggplot() +
  geom_arrow_down(aes(xmin = 0, xmax = 1, ymin = 4, ymax = 5), rotate_at = "center") +
  geom_arrow_right(aes(xmin = 1, xmax = 2, ymin = 4, ymax = 5), rotate_at = "center") +
  geom_arrow_up(aes(xmin = 2, xmax = 3, ymin = 4, ymax = 5), rotate_at = "center") +
  geom_arrow_left(aes(xmin = 3, xmax = 4, ymin = 4, ymax = 5), rotate_at = "center") +
  geom_chevron(aes(xmin = 0, xmax = 1, ymin = 3, ymax = 3.9)) +
  geom_parallelogram(aes(xmin = 1.1, xmax = 1.9, ymin = 3, ymax = 3.9)) +
  geom_diamond(aes(xmin = 2.1, xmax = 2.9, ymin = 3, ymax = 3.9)) +
  geom_triangle(aes(xmin = 3.1, xmax = 3.9, ymin = 3, ymax = 3.9)) +
  geom_pentagon(aes(xmin = 0, xmax = 1, ymin = 2, ymax = 2.9)) +
  geom_callout(aes(xmin = 1.1, xmax = 1.9, ymin = 2.2, ymax = 2.8)) +
  geom_elbow(aes(x = 2, xend = 3, y = 2.9, yend = 2.1),
             arrow = arrow(type = "closed", length = unit(0.03, "npc"))) +
  geom_cross(aes(xmin = 3.1, xmax = 4, ymin = 2.05, ymax = 2.95)) +
  coord_fixed() +
  theme_minimal()


df <- tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~geom,
  0.05, 0.95, 4.05, 4.95, "arrow_down",
  1.05, 1.95, 4.05, 4.95, "arrow_right",
  2.05, 2.95, 4.05, 4.95, "arrow_up",
  3.05, 3.95, 4.05, 4.95, "arrow_left",
  0.05, 0.95, 3.05, 3.95, "chevron",
  1.15, 1.85, 3.05, 3.95, "parallelogram",
  2.1, 2.9, 3.05, 3.95, "diamond",
  3.1, 3.9, 3.05, 3.95, "triangle",
  0.05, 0.95, 2.05, 2.95, "pentagon",
  1.1, 1.9, 2.2, 2.8, "callout",
  2.05, 2.95, 2.9, 2.1, "elbow",
  3.05, 3.95, 2.05, 2.95, "cross"
)


df <- tibble(
  xmin = 0.05,
  xmax = 0.95,
  ymin = 0.05,
  ymax = 0.95,
  geom = c("arrow_down", "arrow_right", "arrow_up", "arrow_left",
           "chevron", "parallelogram", "diamond", "triangle",
           "pentagon", "callout", "elbow", "cross")
)

df1 <- map_dfr(df$geom, ~df |> mutate(focus = .x)) |>
  mutate(
    xmin = ifelse(geom == focus, xmin, NA),
    xmax = ifelse(geom == focus, xmax, NA),
    ymin = ifelse(geom == focus, ymin, NA),
    ymax = ifelse(geom == focus, ymax, NA),
  )

df |>
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_arrow_down(data = filter(df, geom == "arrow_down"), rotate_at = "center") +
  geom_arrow_right(data = filter(df, geom == "arrow_right"), rotate_at = "center") +
  geom_arrow_up(data = filter(df, geom == "arrow_up"), rotate_at = "center") +
  geom_arrow_left(data = filter(df, geom == "arrow_left"), rotate_at = "center") +
  geom_chevron(data = filter(df, geom == "chevron")) +
  geom_parallelogram(data = filter(df, geom == "parallelogram")) +
  geom_diamond(data = filter(df, geom == "diamond")) +
  geom_triangle(data = filter(df, geom == "triangle")) +
  geom_pentagon(data = filter(df, geom == "pentagon")) +
  geom_callout(data = filter(df, geom == "callout")) +
  geom_elbow(aes(x = xmin, xend = xmax, y = ymax, yend = ymin),
             arrow = arrow(type = "closed", length = unit(0.03, "npc")),
             data = filter(df, geom == "elbow")) +
  geom_cross(data = filter(df, geom == "cross")) +
  coord_fixed() +
  theme_minimal()

