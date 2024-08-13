library(sf)
library(raster)
library(exactextractr)

shapefile_path <- "/Users/u2061312/Downloads/test/pt2/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp"
countries <- st_read(shapefile_path)

raster_path <- "/Users/u2061312/Downloads/test/pt2/STAR_50km.tif"
raster_data <- raster(raster_path)

subregions <- c("Western Africa","Eastern Asia","Central Asia")

countries_filtered <- countries %>%
  filter(SUBREGION %in% subregions)

raster_values <- function(country, raster_data) {
  values <- exact_extract(raster_data, country, c("mean", "min", "max"))
  data.frame(country = country$NAME, 
             mean = values$mean, 
             min = values$min, 
             max = values$max)}

# Apply the function to each country
results <- lapply(split(countries_filtered, countries_filtered$NAME), raster_values, raster_data)
results_df <- do.call(rbind, results)

write_csv(results_df, file="results_df.csv")

#task2
countries_filtered <- st_transform(countries_filtered, crs = st_crs(3395)) # World Mercator projection
countries_filtered$area_m2 <- st_area(countries_filtered)
countries_filtered$area_km2 <- as.numeric(countries_filtered$area_m2) / 1e6

area_results <- data.frame(country = countries_filtered$NAME, area_km2 = countries_filtered$area_km2)

write_csv(area_results, file="area_results.csv")

#task3
task3 <- area_results %>%
  select(country, min, max, mean)

write_csv(task3, file="task3.csv")

#task4
sheet_to_join <- read_csv("sheet_to_join.csv")
colnames(sheet_to_join)[colnames(sheet_to_join) == 'NAME_0'] <- 'country'

joined <- merge(task3,sheet_to_join, by = country) %>%
  dplyr::select(country, min, max, mean,EII_Structure_min, EII_Structure_max, EII_Structure_mean)

write_csv(joined, file="joined.csv")











