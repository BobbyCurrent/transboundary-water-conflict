joined %>%
  distinct(basin_name) %>%
  mutate(b = paste("XXX", basin_name, "XXX")) %>%
  write_csv("basins_temp.csv")
