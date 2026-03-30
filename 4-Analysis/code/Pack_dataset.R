# Load necessary libraries
library(terra)
library(sf)
library(dplyr)
library(data.table)
library(stringr)
library(tools)

## Function to choose epsg code

lon <- -95.5   # Replace with central longitude of your study area
lat <-  50    # Replace with your latitude

utm_zone <- floor((lon + 180) / 6) + 1
hemisphere <- ifelse(lat >= 0, "north", "south")
epsg_code <- ifelse(hemisphere == "north", 32600, 32700) + utm_zone

print(paste("Recommended EPSG:", epsg_code))

### Habitat Proportion
# Define the function to calculate the proportion of raster values within polygons
calculate_proportion <- function(raster_data, polygons) {
  # Extract raster values within each polygon (returns a list of vectors)
  raster_values <- terra::extract(raster_data, vect(polygons))
  
  # Check if any values were extracted
  if (nrow(raster_values) == 0) {
    return(data.frame(value = NA, count = NA, proportion = NA))
  }
  
  # Drop the ID column and rename raster values column to 'value'
  values_df <- raster_values[, 2, drop = FALSE]
  colnames(values_df) <- "value"
  
  # Count each value
  value_counts <- values_df %>%
    count(value) %>%
    mutate(proportion = n / sum(n)) %>%
    rename(count = n)
  
  return(value_counts)
}

# Load the raster (only one raster file)
raster_directory <- "~/Documents/Masters/Data/GIS/Landclass/Cover Type/Canada 2015"
#raster_directory <- "~/Documents/Masters/Workshops/Mapping_in_R/Input/RMNP"

#r_hab <- rast(file.path(raster_directory, "RMNPlandcover_wgs84.tif"))
r_hab <- rast((file.path(raster_directory, "Landcover_2015.grd")))

# Reproject the raster
#raster_hab <- projectRaster(raster_list, crs = "+init=epsg:32614", method = "ngb")
rhab_proj <- project(r_hab, "EPSG:32614", method = "near")
rhab_proj[is.na(rhab_proj)] <- 100  # Assign a value to NA pixels if necessary

# Load shapefiles
shapefile_directory <- "KDE_HomeRanges/GHAIndividual/95UD"  # Update with the correct path if needed

# List all shapefiles (polygons)
kde.95UD <- list.files(path = shapefile_directory, pattern = '.shp', full.names = TRUE)

# Load shapefiles and calculate proportions for each polygon
results <- lapply(kde.95UD, function(shapefile_path) {
  polygons <- st_read(shapefile_path)
  polygons <- st_transform(polygons, crs = 32614)
  
  # Extract values and calculate proportions for this shapefile
  proportion_data <- calculate_proportion(rhab_proj, polygons)
  
  # Add the shapefile name to the result for reference
  proportion_data$shapefile <- basename(shapefile_path)
  
  return(proportion_data)
})

# Flatten the list of results into a single data frame
final_results <- do.call(rbind, results)

# Print the final results
print(final_results)

fwrite(final_results, "Pack_Dataset/GHA_individual-yearly_habitat-proportion.csv")

### Extract prey density by year
## For RMNP
# Function to read, project shapefiles, and extract average raster values
extract_avg_raster_value <- function(shapefile_paths, raster_stack) {
  
  # Step 1: Read and project shapefiles
  shapefiles <- lapply(shapefile_paths, function(shapefile) {
    shp <- st_read(shapefile, quiet = TRUE)
    shp <- st_transform(shp, crs = 32614)
    vect(shp)
  })
  
  # Step 2: Get the years from the shapefile filenames (assuming 'year' is the 1st 4 digits of filenames)
  shapefile_names <- gsub("^([^_]+_[0-9]+)_.*", "\\1", basename(shapefile_paths))
  shapefile_years <- sub( ".*_", "", shapefile_names)
  #shapefile_years <- regmatches(shapefile_paths, regexpr("\\d{4}", shapefile_paths))
  
  
  # Step 3: Extract average raster values for each shapefile
  results <- lapply(1:length(shapefile_years), function(i) {
    year <- shapefile_years[i]
    
    # Check if the year exists in the names of the raster stack
    matching_raster_name <- grep(year, names(raster_stack), value = TRUE)
    
    # If matching raster exists
    if (length(matching_raster_name) > 0) {
      matching_raster <- raster_stack[[matching_raster_name]]   # Get the matching raster by name
      shapefile <- shapefiles[[i]]   # Get the corresponding shapefile
      
      # Mask the raster with the shapefile and calculate the mean
      masked_raster <- mask(matching_raster, shapefile)
      avg_value <- global(masked_raster, fun = 'mean', na.rm = TRUE)
      min_value <- global(masked_raster, fun = 'min', na.rm = TRUE)
      max_value <- global(masked_raster, fun = 'max', na.rm = TRUE)
      
      # Return results
      return(data.frame(shapefile = basename(shapefile_paths[i]), 
                        raster = matching_raster_name, 
                        avg_value = avg_value,
                        min_value = min_value,
                        max_value = max_value))
    }
    return(NULL)
  })
  
  # Combine the results into a single data frame
  result_df <- do.call(rbind, results)
  return(result_df)
}

# Example usage:
shapefiles <- list.files(path = "KDE_HomeRanges/RMNPPack_Season/95UD", pattern = "*.shp", full.names = TRUE)
rasters.ls <- list.files(path = "~/Documents/Masters/Data/Survey_Density/Output/RMNP/Rasters", pattern = "*.tif$", full.names = TRUE)

rast.set <- rast(rasters.ls)
rast.rmnp <- project(rast.set, "EPSG:32614", method = "near")
names(rast.rmnp) <- tools::file_path_sans_ext(basename(rasters.ls))

avg_values <- extract_avg_raster_value(shapefiles, raster_stack = rast.rmnp)
print(avg_values)

fwrite(avg_values, "Pack_Dataset/Prey_Dens/RMNP_pack-seasonal_prey-density_updated.csv")

## Individual
extract_avg_raster_value <- function(shapefile_paths, raster_stack, id_year_table) {
  
  # Step 1: Read and project shapefiles, extract ID
  shapefiles_info <- lapply(shapefile_paths, function(path) {
    id <- sub("_.*", "", basename(path))  # Extract ID from file name
    shp <- st_read(path, quiet = TRUE)
    shp <- st_transform(shp, crs = 32614)
    list(id = id, path = path, vect = vect(shp))
  })
  
  # Step 2: Extract raster values using deployment year
  results <- lapply(shapefiles_info, function(info) {
    # Match Animal_ID to Deployment_Year from lookup table
    year <- id_year_table$Deployment_Year[id_year_table$Animal_ID == info$id]
    
    if (length(year) == 0 || is.na(year)) {
      warning(paste("No deployment year found for ID:", info$id))
      return(NULL)
    }
    
    # Find matching raster layer
    matching_raster_name <- grep(as.character(year), names(raster_stack), value = TRUE)
    
    if (length(matching_raster_name) == 0) {
      warning(paste("No raster found for year:", year, "for ID:", info$id))
      return(NULL)
    }
    
    matching_raster <- raster_stack[[matching_raster_name]]
    masked <- mask(matching_raster, info$vect)
    
    # Check if masked raster has only NAs
    if (all(is.na(values(masked)))) {
      warning(paste("Masked raster has only NA values for ID:", info$id, "- returning 0s"))
      return(data.frame(
        shapefile = basename(info$path),
        Animal_ID = info$id,
        Deployment_Year = year,
        raster = matching_raster_name,
        avg_value = 0,
        min_value = 0,
        max_value = 0
      ))
    }
    
    # Otherwise, calculate normally
    avg <- global(masked, fun = "mean", na.rm = TRUE)[[1]]
    min_val <- global(masked, fun = "min", na.rm = TRUE)[[1]]
    max_val <- global(masked, fun = "max", na.rm = TRUE)[[1]]
    
    return(data.frame(
      shapefile = basename(info$path),
      Animal_ID = info$id,
      Deployment_Year = year,
      raster = matching_raster_name,
      avg_value = avg,
      min_value = min_val,
      max_value = max_val
    ))
    
  })
  
  
  # Combine and return
  result_df <- do.call(rbind, results)
  return(result_df)
}


# 1. Shapefile list
shapefiles <- list.files(
  path = "KDE_HomeRanges/RMNPIndividual/95UD", 
  pattern = "*.shp", 
  full.names = TRUE
)

# 2. Load rasters and project to UTM
rasters.ls <- list.files(
  path = "~/Documents/Masters/Data/Survey_Density/Output/RMNP/Rasters/Averaged", 
  pattern = "*.tif$", 
  full.names = TRUE
)

rast.set <- rast(rasters.ls)
rast.rmnp <- project(rast.set, "EPSG:32614", method = "near")
names(rast.rmnp) <- tools::file_path_sans_ext(basename(rasters.ls))

# 3. ID-Year lookup table (example)
id_table_rmnp <- fread("Pack_Dataset/Summary/RMNP_individual_deploymentyear.csv")

# 4. Run function
avg_values <- extract_avg_raster_value(shapefiles, raster_stack = rast.rmnp, id_table_rmnp)
print(avg_values)

fwrite(avg_values, "Pack_Dataset/Prey_Dens/RMNP_individual-yearly_prey-density_averaged.csv")

### Eastern MB
emb_avg_raster_value <- function(shapefile_paths, raster_stack) {
  
  # Step 1: Read and project shapefiles
  shapefiles <- lapply(shapefile_paths, function(shapefile) {
    shp <- st_read(shapefile, quiet = TRUE)
    shp <- st_transform(shp, crs = 32614)
    vect(shp)
  })
  
  # Step 2: Extract years from shapefile filenames
  shapefile_names <- gsub("^([^_]+_[0-9]+)_.*", "\\1", basename(shapefile_paths))
  shapefile_years <- sub( ".*_", "", shapefile_names)
  
  # Step 3: Define shapefile-to-raster year mapping
  year_map <- list(
    "2014" = "2013",
    "2015" = "2016",
    "2016" = "2016",
    "2017" = "2018",
    "2018" = "2018",
    "2019" = "2020"
  )
  
  # Step 4: Loop through shapefiles and extract average raster values
  results <- lapply(seq_along(shapefile_years), function(i) {
    shapefile_year <- shapefile_years[i]
    
    # Use mapped year if it exists, else use original shapefile year
    mapped_year <- if (!is.null(year_map[[shapefile_year]])) year_map[[shapefile_year]] else shapefile_year
    
    # Find the raster that matches the mapped year
    matching_raster_name <- grep(mapped_year, names(raster_stack), value = TRUE)
    print(paste("Mapped year:", mapped_year, "| Matching raster:", matching_raster_name))
    
    if (length(matching_raster_name) > 0) {
      matching_raster <- raster_stack[[matching_raster_name]]
      shapefile <- shapefiles[[i]]
      
      masked_raster <- mask(matching_raster, shapefile)
      # Calculate average, min, and max values
      avg_value <- global(masked_raster, fun = 'mean', na.rm = TRUE)
      min_value <- global(masked_raster, fun = 'min', na.rm = TRUE)
      max_value <- global(masked_raster, fun = 'max', na.rm = TRUE)
      
      # Return results
      return(data.frame(
        shapefile = basename(shapefile_paths[i]),
        shapefile_year = shapefile_year,
        raster_year = mapped_year,
        avg_value = avg_value,
        min_value = min_value,
        max_value = max_value
      ))
    }
    
    return(NULL)
  })
  
  # Step 5: Combine results into a single data frame
  result_df <- do.call(rbind, results)
  return(result_df)
}

shapefiles <- list.files(path = "KDE_HomeRanges/GHAPack_Season/95UD", pattern = "*.shp", full.names = TRUE)
rasters.ls <- list.files(path = "~/Documents/Masters/Data/Survey_Density/Output/Raster", pattern = "*.tif$", full.names = TRUE)

# Extract filenames without extensions for use as names
raster_names <- tools::file_path_sans_ext(basename(rasters.ls))

# Stack rasters
rast.set <- rast(rasters.ls)
names(rast.set) <- raster_names
rast.gha <- project(rast.set, "EPSG:32614", method = "near")

# Run function
emb_avg_values <- emb_avg_raster_value(shapefiles, raster_stack = rast.gha)
print(emb_avg_values)

fwrite(emb_avg_values, "Pack_Dataset/Prey_Dens/EMB_pack_seasonal_moose-density.csv")

## Individual
emb_avg_raster_value <- function(shapefile_paths, raster_stack, id_year_table) {
  
  # Step 1: Read shapefiles and extract Animal_ID
  shapefiles_info <- lapply(shapefile_paths, function(path) {
    id <- sub("(_akde.*|_aKDE.*)", "", file_path_sans_ext(basename(path)))
    #id <- sub("(_Snow.*|_Snow.*)", "", file_path_sans_ext(basename(path)))
    shp <- st_read(path, quiet = TRUE)
    shp <- st_transform(shp, crs = 32614)
    list(id = id, path = path, vect = vect(shp))
  })
  
  # Step 2: Year mapping from deployment to raster year
  year_map <- list(
    "2014" = "2015",
    "2015" = "2015",
    "2016" = "2017",
    "2017" = "2017",
    "2018" = "2019",
    "2019" = "2019"
  )
  
  # Step 3: Extract raster values for each shapefile
  results <- lapply(shapefiles_info, function(info) {
    # Match Animal_ID to deployment year
    deployment_year <- id_year_table$Deployment_Year[id_year_table$Animal_ID == info$id]
    
    if (length(deployment_year) == 0 || is.na(deployment_year)) {
      warning(paste("No deployment year found for Animal_ID:", info$id))
      return(NULL)
    }
    
    # Map deployment year to raster year
    deployment_year <- as.character(deployment_year)
    mapped_year <- if (!is.null(year_map[[deployment_year]])) year_map[[deployment_year]] else deployment_year
    
    # Match raster by year
    matching_raster_name <- grep(mapped_year, names(raster_stack), value = TRUE)
    print(paste("Animal_ID:", info$id, "| Deploy Year:", deployment_year, "| Raster Year:", mapped_year, "| Raster:", matching_raster_name))
    
    if (length(matching_raster_name) == 0) {
      warning(paste("No raster found for mapped year:", mapped_year, "for Animal_ID:", info$id))
      return(NULL)
    }
    
    matching_raster <- raster_stack[[matching_raster_name]]
    masked <- mask(matching_raster, info$vect)
    
    # Handle no-overlap case
    if (all(is.na(values(masked)))) {
      warning(paste("No overlap for:", info$id))
      return(data.frame(
        shapefile = basename(info$path),
        Animal_ID = info$id,
        Deployment_Year = deployment_year,
        raster_year = mapped_year,
        avg_value = 0,
        min_value = 0,
        max_value = 0
      ))
    }
    
    # Calculate values
    avg <- global(masked, fun = 'mean', na.rm = TRUE)[[1]]
    min_val <- global(masked, fun = 'min', na.rm = TRUE)[[1]]
    max_val <- global(masked, fun = 'max', na.rm = TRUE)[[1]]
    
    data.frame(
      shapefile = basename(info$path),
      Animal_ID = info$id,
      Deployment_Year = deployment_year,
      raster_year = mapped_year,
      avg_value = avg,
      min_value = min_val,
      max_value = max_val
    )
  })
  
  # Combine results
  result_df <- do.call(rbind, results)
  return(result_df)
}

# 1. Shapefile list
shapefiles <- list.files(
  path = "KDE_HomeRanges/GHAIndividual/95UD", 
  pattern = "*.shp", 
  full.names = TRUE
)

# 2. Load rasters and project to UTM
rasters.ls <- list.files(
  path = "~/Documents/Masters/Data/Survey_Density/Output/Raster/Averaged", 
  pattern = "*.tif$", 
  full.names = TRUE
)

raster_names <- tools::file_path_sans_ext(basename(rasters.ls))

# Stack rasters
rast.set <- rast(rasters.ls)
names(rast.set) <- raster_names
rast.gha <- project(rast.set, "EPSG:32614", method = "near")

# 3. ID-Year lookup table (example)
id_table_gha <- fread("Pack_Dataset/Summary/GHA_individual_deploymentyear.csv")

# Run function
avg_values <- emb_avg_raster_value(shapefiles, raster_stack = rast.gha, id_year_table = id_table_gha)
print(avg_values)

fwrite(avg_values, "Pack_Dataset/Prey_Dens/GHA_individual-yearly_prey-density_averaged.csv")

### Road Density
calculate_road_densities <- function(road_list, homerange_list) {
  # Ensure both road and home range lists are named
  if (is.null(names(road_list))) {
    names(road_list) <- paste0("road_", seq_along(road_list))
  }
  if (is.null(names(homerange_list))) {
    names(homerange_list) <- paste0("range_", seq_along(homerange_list))
  }
  
  # Loop over home ranges and calculate densities
  all_results <- lapply(names(homerange_list), function(hr_name) {
    homerange <- homerange_list[[hr_name]]
    homerange_geom <- st_union(homerange)
    
    if (is.na(st_crs(homerange_geom))) {
      stop(paste("Home range", hr_name, "has no CRS defined."))
    }
    
    area_km2 <- as.numeric(st_area(homerange_geom)) / 1e6
    
    # Road densities
    road_densities <- sapply(names(road_list), function(road_name) {
      road_layer <- road_list[[road_name]]
      clipped_roads <- st_intersection(road_layer, homerange_geom)
      if (nrow(clipped_roads) == 0) return(0)
      length_km <- sum(st_length(clipped_roads)) / 1000
      as.numeric(length_km / area_km2)
    })
    
    # Build data frame with home range name
    result <- data.frame(
      home_range = hr_name,
      road_layer = names(road_densities),
      density_km_per_km2 = road_densities,
      row.names = NULL
    )
    
    # Add total row
    total_row <- data.frame(
      home_range = hr_name,
      road_layer = "Total",
      density_km_per_km2 = sum(road_densities)
    )
    
    rbind(result, total_row)
  })
  
  # Combine all results into a single table
  bind_rows(all_results)
}

# Suppose you read your shapefiles like this:
lf_files <- list.files("Linear_Features/SEMB", pattern = "\\.shp$", full.names = TRUE)
#lf_files <- list.files("Linear_Features/RMNP", pattern = "\\.shp$", full.names = TRUE)
# Extract base names (without extension) to use as identifiers
lf_names <- tools::file_path_sans_ext(basename(lf_files))

akde_files <- list.files("KDE_HomeRanges/GHAIndividual/95UD", pattern = "\\.shp$", full.names = TRUE)
#akde_files <- list.files("KDE_HomeRanges/RMNPIndividual/95UD", pattern = "\\.shp$", full.names = TRUE)
hr_names <- tools::file_path_sans_ext(basename(akde_files))

# Choose a projected CRS (e.g., UTM appropriate for your region)
target_crs <- 32614  # Replace with your desired EPSG code

# Read and transform road layers
lfs <- setNames(
  lapply(lf_files, function(f) st_read(f) %>% st_transform(crs = target_crs)),
  lf_names
)

# Read and transform home range layers
akde_ranges <- setNames(
  lapply(akde_files, function(f) st_read(f) %>% st_transform(target_crs)), 
  hr_names)

# Run the function
road_density_results <- calculate_road_densities(lfs, akde_ranges)
head(road_density_results)

fwrite(road_density_results, file = "Pack_Dataset/LF_Dens/GHA_individual_yearly_lf-dens.csv")

### Step Lengths
## GHA
GHA.tib <- as_tibble(GHA_clean) %>% make_track(.x = Easting, .y = Northing, 
                                               .t = ts, id = Animal_ID,
                                               crs = 32614, all_cols = T)
GHA.trk <- GHA.tib %>% nest(data = -"Animal_ID")

GHA.stp <- GHA.trk |> 
  mutate(steps = map(data, function(x) 
    x |> track_resample(rate = hours(2), tolerance = minutes(5)) |> steps_by_burst()))

GHA.stp |> dplyr::select(Animal_ID, steps) |> unnest(cols = steps) |> 
  ggplot(aes(sl_, fill = factor(Animal_ID))) + geom_density(alpha = 0.4)

GHA.sl <- GHA.stp |> dplyr::select(Animal_ID, steps) |> unnest(cols = steps) |>
  group_by(Animal_ID) %>%
  filter(sl_ < 18000) %>% 
  summarise(mean = mean(sl_), med = median(sl_), min = min(sl_), max = max(sl_),
            sd = sd(sl_), n = n())

#fwrite(GHA.sl, file = "GHA_step-length_summary-by-id.csv")

df <- GHA.stp |> dplyr::select(Animal_ID, steps) |> unnest(cols = steps) |> 
  filter(Animal_ID == "ER_W_16") |>
  mutate(id = row_number())

lines_sf <- df %>%
  rowwise() %>%
  mutate(geometry = list(st_linestring(matrix(c(x1_, y1_, x2_, y2_), ncol = 2, byrow = TRUE)))) %>% 
  st_as_sf() %>%
  st_set_crs(32614) %>%
  st_transform(4326)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolylines(data = lines_sf, color = ~ifelse(df$sl_ > 15000, "red", "blue"),
               popup = ~paste("Distance:", df$sl_))

#Seasonal
GHA.trk.seas <- GHA.tib %>% nest(data = -"IndSeason")

GHA.stp.seas <- GHA.trk.seas |> 
  mutate(steps = map(data, function(x) 
    x |> track_resample(rate = hours(2), tolerance = minutes(5)) |> steps_by_burst()))

GHA.sl.seas <- GHA.stp.seas |> dplyr::select(IndSeason, steps) |> unnest(cols = steps) |>
  group_by(IndSeason) %>%
  filter(sl_ < 18000) %>% 
  summarise(mean = mean(sl_), med = median(sl_), min = min(sl_), max = max(sl_), n = n())
#fwrite(GHA.sl.seas, file = "GHA_seasonal_step-length_summary-by-id.csv")

## RMNP
RMNP.tib <- as_tibble(RMNP_clean) %>% make_track(.x = X, .y = Y, 
                                               .t = ts, id = Animal_ID,
                                               crs = 32614, all_cols = T)
RMNP.trk <- RMNP.tib %>% nest(data = -"Animal_ID")

RMNP.stp <- RMNP.trk |> 
  mutate(steps = map(data, function(x) 
    x |> track_resample(rate = hours(2), tolerance = minutes(5)) |> steps_by_burst()))

RMNP.stp |> dplyr::select(Animal_ID, steps) |> unnest(cols = steps) |> 
  ggplot(aes(sl_, fill = factor(Animal_ID))) + geom_density(alpha = 0.4)

RMNP.df <- RMNP.stp |> dplyr::select(Animal_ID, steps) |> unnest(cols = steps) |> 
  filter(Animal_ID == "W27")

RMNP.sl <- RMNP.stp |> dplyr::select(Animal_ID, steps) |> unnest(cols = steps) |>
  group_by(Animal_ID) %>%
  filter(sl_ < 15000) %>% 
  summarise(mean = mean(sl_), med = median(sl_), min = min(sl_), max = max(sl_), n = n())

#fwrite(RMNP.sl, file = "RMNP_step-length_summary-by-id.csv")

#Seasonal
RMNP.trk.seas <- RMNP.tib %>% nest(data = -"IndSeason")

RMNP.stp.seas <- RMNP.trk.seas |> 
  mutate(steps = map(data, function(x) 
    x |> track_resample(rate = hours(2), tolerance = minutes(5)) |> steps_by_burst()))

df <- RMNP.stp.seas |> dplyr::select(IndSeason, steps) |> unnest(cols = steps) |> 
  filter(IndSeason == "W02_SnowFree") |>
  mutate(id = row_number())

lines_sf <- df %>%
  rowwise() %>%
  mutate(geometry = list(st_linestring(matrix(c(x1_, y1_, x2_, y2_), ncol = 2, byrow = TRUE)))) %>% 
  st_as_sf() %>%
  st_set_crs(32614) %>%
  st_transform(4326)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolylines(data = lines_sf, color = ~ifelse(df$sl_ < 200, "red", "blue"),
               popup = ~paste("Distance:", df$sl_))


RMNP.sl.seas <- RMNP.stp.seas |> dplyr::select(IndSeason, steps) |> unnest(cols = steps) |>
  group_by(IndSeason) %>%
  filter(sl_ < 15000) %>% 
  summarise(mean = mean(sl_), med = median(sl_), min = min(sl_), max = max(sl_),n = n())

#fwrite(RMNP.sl, file = "RMNP_seasonal_step-length_summary-by-id.csv")

## Summarize SL results for both regions
GHA.yr <- GHA.stp |> dplyr::select(Animal_ID, steps) |> unnest(cols = steps) |>
  filter(sl_ < 18000 & !(Animal_ID == "ER_W_44"| Animal_ID == "ER_W_38" | Animal_ID == "ER_W_52")) |>
  mutate(Study_Area = "SEMB")

GHA.merge <- GHA.stp |> dplyr::select(Animal_ID, steps) |> unnest(cols = steps) |>
  filter(sl_ < 18000) |>
  mutate(Study_Area = "SEMB") |>
  left_join(id_table_gha[, c("Animal_ID", "PackID", "Deployment_Year")], by = "Animal_ID")

RMNP.yr <- RMNP.stp |> dplyr::select(Animal_ID, steps) |> unnest(cols = steps) |>
  filter(sl_ < 18000) |>
  mutate(Study_Area = "RMNP")

RMNP.merge <- merge(RMNP.yr, id_table_rmnp[, c("Animal_ID", "PackID", 
                                               "Deployment_Year")], by = "Animal_ID", fill = T)

sl.yr <- rbind(GHA.merge, RMNP.merge) 

sl.summary <- sl.yr |> group_by(PackID, Deployment_Year) %>%
  summarise(mean = mean(sl_), med = median(sl_), min = min(sl_), max = max(sl_), n = n())

#fwrite(sl.summary, file = "Pack_Dataset/Step_Length/Yearly_step-length_summary-by-pack.csv")

col_pal <- c("#21918c", "#440154")

ggplot(sl.yr, aes(sl_, fill = Study_Area)) + 
  geom_density(alpha = 0.4) +
  geom_vline(data = sl.summary, aes(xintercept=mean, color = Study_Area)) +
  scale_colour_manual(values = col_pal,labels=c('RMNP', 'SEMB'), name = "Study Area") +
  scale_fill_manual(values = col_pal,labels=c('RMNP', 'SEMB'), name = "Study Area") +
  xlim(0, 10000) + 
  xlab("Step lengths (m)/2-hours") +
  ylab("Density") +
  theme_bw() + 
  theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=14),
    axis.title = element_text(size=14),
    axis.text.y = element_text(size=14),
    legend.title=element_text(size=14),
    legend.text = element_text(size = 14),
    legend.position=c(0.8, 0.8)) 

#ggsave(filename = "Regional_steplength_density-plot.png", width = 7, height = 5)

# Seasonal
GHA.seas <- GHA.stp.seas |> dplyr::select(IndSeason, steps) |> unnest(cols = steps) |>
  filter(sl_ < 18000 & !(IndSeason == "ER_W_44_Snow"| 
                           IndSeason == "ER_W_38_Snow" | IndSeason == "ER_W_38_SnowFree" | 
                           IndSeason == "ER_W_52_Snow" | IndSeason == "ER_W_52_SnowFree")) |>
  mutate(Study_Area = "SEMB")

#GHA.seas <- GHA.stp.seas |> dplyr::select(IndSeason, steps) |> unnest(cols = steps) |>
#  filter(sl_ < 18000) |>
#  mutate(Study_Area = "SEMB")

RMNP.seas <- RMNP.stp.seas |> dplyr::select(IndSeason, steps) |> unnest(cols = steps) |>
  filter(sl_ < 15000) |>
  mutate(Study_Area = "RMNP")

sl.seas <- rbind(GHA.seas, RMNP.seas) 

id_table <- rbind(id_table_gha, id_table_rmnp)

sl.seas.merge <- sl.seas |> 
  mutate(Season = sub( ".*_", "", IndSeason)) |>
  mutate(Animal_ID = str_extract(IndSeason, "^[^_]+(?:_[^_]+)*?(?=_Snow|_SnowFree|_aKDE)")) |>
  left_join(id_table[, N := NULL], by = "Animal_ID") |>
  group_by(PackID, Deployment_Year, Season) |>
  summarise(mean = mean(sl_), med = median(sl_), min = min(sl_), max = max(sl_), n = n())

#fwrite(sl.seas.merge, file = "Pack_Dataset/Step_Length/Seasonal_step-length_summary-by-pack.csv")

sl.seas.summary <- sl.seas |> 
  mutate(Season = sub( ".*_", "", IndSeason)) |>
  group_by(Study_Area, Season) |>
  summarise(mean = mean(sl_), med = median(sl_), min = min(sl_), max = max(sl_), 
            sd = sd(sl_), n = n())

#fwrite(sl.seas.summary, file = "Seasonal_step-length_summary-by-region_2025-09-19.csv")

ggplot(sl.seas |> 
         mutate(Season = sub( ".*_", "", IndSeason)), aes(sl_, fill = Season)) + 
  geom_density(alpha = 0.4) +
  geom_vline(data = sl.seas.summary, aes(xintercept=mean, color = Season)) +
  scale_colour_manual(values = col_pal,labels=c('Snow', 'Snow free'), name = "Season") +
  scale_fill_manual(values = col_pal,labels=c('Snow', 'Snow free'), name = "Season") +
  facet_wrap(~ Study_Area) +
  xlim(0, 10000) + 
  xlab("Step lengths (m)/2-hours") +
  ylab("Density") +
  theme_bw() + 
  theme(
    panel.background =element_rect(colour = "black", fill=NA, linewidth =1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", size = .1),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14),
    strip.text = element_text(size = 14),
    legend.title=element_text(size=14),
    legend.text = element_text(size = 14)) 

#ggsave(filename = "Regional_steplength_density-plot_by-season.png", width = 7, height = 5)

### Summarize results
## Prey Density
# Create a list of all pack yearly and seasonal datasets
pack.ls <- list.files(path = "Pack_Dataset/Prey_Dens/Pack", 
                       pattern = '.csv', full.names = TRUE)

#pack.out <- lapply(pack.ls, fread)

# Function to read a CSV file, add a new column with the file name, and return the modified data frame
add_filename_column <- function(file_path) {
  # Read the CSV file into a data frame
  df <- fread(file_path)
  
  # Extract the first 3 or 4 letters of the file name before the first "_"
  file_name <- gsub("^([a-zA-Z]{3,4})_.*", "\\1", basename(file_path))  # This regex extracts the first 3 or 4 letters
  parts <- strsplit(basename(file_path), "_")[[1]]
  season <- parts[3]
  survey <- gsub(".*-(\\w+).csv.*", "\\1", basename(file_path))
  
  # Add a new column with the extracted file name part
  df$study_area <- file_name
  df$time_frame <- season
  df$survey <- survey
  
  return(df)
}

# Apply the function to all CSV files in the list using lapply
modified_data <- lapply(pack.ls, add_filename_column)

## Prey
pack.prey <- rbindlist(modified_data, fill=TRUE)
pack.prey[, species := ifelse(study_area == "RMNP", 
                              #sub("_.*", "", raster),
                              tstrsplit(raster, "_")[[2]],
                              "moose")]
pack.prey[, pack := str_extract(shapefile, "^[^_]+")]
pack.prey[, year := tstrsplit(shapefile, "_", keep = 2)]
pack.prey[, season := ifelse(time_frame == "yearly", "full-year", 
                             unlist(tstrsplit(shapefile, "_", keep = 3)))]
pack.prey[, species_survey := paste0(species, "_", survey)]

prey.cast <- dcast(pack.prey, pack + year + season + study_area ~ species_survey, 
                   #fun = sum,
                   value.var = "mean")

fwrite(prey.cast, file = "Regional_seasonal_prey-density_by-pack_updated.csv")

# Individuals
# Create a list of all pack yearly and seasonal datasets
indivs.ls <- list.files(path = "Pack_Dataset/Prey_Dens/Individual", 
                      pattern = '.csv', full.names = TRUE)

# Function to read a CSV file, add a new column with the file name, and return the modified data frame
add_filename_column <- function(file_path) {
  # Read the CSV file into a data frame
  df <- fread(file_path)
  
  # Extract the first 3 or 4 letters of the file name before the first "_"
  file_name <- gsub("^([a-zA-Z]{3,4})_.*", "\\1", basename(file_path))  # This regex extracts the first 3 or 4 letters
  parts <- strsplit(basename(file_path), "_")[[1]]
  season <- parts[3]
  survey <- gsub(".*-(\\w+).csv.*", "\\1", basename(file_path))
  
  # Add a new column with the extracted file name part
  df$study_area <- file_name
  df$time_frame <- season
  df$survey <- survey
  
  return(df)
}

# Apply the function to all CSV files in the list using lapply
modified_data <- lapply(indivs.ls, add_filename_column)

## Prey
indiv.prey <- rbindlist(modified_data, fill=TRUE)
indiv.prey[, species := ifelse(study_area == "RMNP", 
                              #sub("_.*", "", raster),
                              tstrsplit(raster, "_")[[2]],
                              "moose")]
#indiv.prey[, pack := str_extract(shapefile, "^[^_]+")]
indiv.prey[, year := Deployment_Year]
indiv.prey[, season := ifelse(time_frame == "yearly", "full-year", 
                             str_extract(shapefile, "SnowFree|Snow"))]
indiv.prey[, species_survey := paste0(species, "_", survey)]

indiv.cast <- dcast(indiv.prey, Animal_ID + year + season + study_area ~ species_survey, 
                   #fun = sum,
                   value.var = "avg_value")

fwrite(indiv.cast, file = "Regional_seasonal_prey-density_by-individual_updated.csv")

## Habitat
pack.ls <- list.files(path = "Pack_Dataset/Habitat/Pack", 
                      pattern = '.csv', full.names = TRUE)

#pack.out <- lapply(pack.ls, fread)

# Function to read a CSV file, add a new column with the file name, and return the modified data frame
add_filename_column <- function(file_path) {
  # Read the CSV file into a data frame
  df <- fread(file_path)
  
  # Extract the first 3 or 4 letters of the file name before the first "_"
  file_name <- gsub("^([a-zA-Z]{3,4})_.*", "\\1", basename(file_path))
  season <- gsub(".*-(\\w+)_.*", "\\1", basename(file_path))  
  
  # Add a new column with the extracted file name part
  df$study_area <- file_name
  df$time_frame <- season
  return(df)
}

# Apply the function to all CSV files in the list using lapply
modified_data <- lapply(pack.ls, add_filename_column)

pack.hab <- rbindlist(modified_data, fill=TRUE)
legend <- fread("~/Documents/Masters/Data/GIS/Landclass/Cover Type/Canada 2015/CoverType_2015_Legend.csv")
names(legend) <- tolower(names(legend))

pack.hab <- merge(pack.hab, legend, by = "value")
pack.hab[, pack := str_extract(shapefile, "^[^_]+")]
pack.hab[, year := tstrsplit(shapefile, "_", keep = 2)]
pack.hab[, season := ifelse(time_frame == "yearly", "full-year", 
                             unlist(tstrsplit(shapefile, "_", keep = 3)))]

View(pack.hab[time_frame == "yearly", sum(proportion), 
              by = c("cover", "study_area", "pack", "year")])

pack.cast <- dcast(pack.hab, pack + season + year + study_area ~ cover, 
                   fun = sum,
                   value.var = "proportion")

fwrite(pack.cast, file = "Pack_Dataset/Summary/Regional_habitat-proportion_by-pack.csv")

#Individuals
indiv.ls <- list.files(path = "Pack_Dataset/Habitat/Individual", 
                      pattern = '.csv', full.names = TRUE)

#pack.out <- lapply(pack.ls, fread)

# Function to read a CSV file, add a new column with the file name, and return the modified data frame
add_filename_column <- function(file_path) {
  # Read the CSV file into a data frame
  df <- fread(file_path)
  
  # Extract the first 3 or 4 letters of the file name before the first "_"
  file_name <- gsub("^([a-zA-Z]{3,4})_.*", "\\1", basename(file_path))
  season <- gsub(".*-(\\w+)_.*", "\\1", basename(file_path))  
  
  # Add a new column with the extracted file name part
  df$study_area <- file_name
  df$time_frame <- season
  return(df)
}

# Apply the function to all CSV files in the list using lapply
modified_data <- lapply(indiv.ls, add_filename_column)

indiv.hab <- rbindlist(modified_data, fill=TRUE)
legend <- fread("~/Documents/Masters/Data/GIS/Landclass/Cover Type/Canada 2015/CoverType_2015_Legend.csv")
names(legend) <- tolower(names(legend))

indiv.hab <- merge(indiv.hab, legend, by = "value")
indiv.hab[, Animal_ID := str_extract(shapefile, "^[^_]+(?:_[^_]+)*?(?=_Snow|_SnowFree|_aKDE)")]
indiv.hab[, season := ifelse(time_frame == "yearly", "full-year", 
                            str_extract(shapefile, "SnowFree|Snow"))]

indiv.cast <- dcast(indiv.hab, Animal_ID + season + study_area ~ cover, 
                   fun = sum,
                   value.var = "proportion")

#fwrite(indiv.cast, file = "Pack_Dataset/Summary/Regional_habitat-proportion_by-individual.csv")

## LF Density
# Create a list of all pack yearly and seasonal datasets
pack.ls <- list.files(path = "Pack_Dataset/LF_Dens/Pack", 
                      pattern = '.csv', full.names = TRUE)

#pack.out <- lapply(pack.ls, fread)

# Function to read a CSV file, add a new column with the file name, and return the modified data frame
add_filename_column <- function(file_path) {
  # Read the CSV file into a data frame
  df <- fread(file_path)
  
  # Extract the first 3 or 4 letters of the file name before the first "_"
  file_name <- gsub("^([a-zA-Z]{3,4})_.*", "\\1", basename(file_path))  # This regex extracts the first 3 or 4 letters
  parts <- strsplit(basename(file_path), "_")[[1]]
  season <- parts[3]
  
  # Add a new column with the extracted file name part
  df$study_area <- file_name
  df$time_frame <- season
  
  return(df)
}

# Apply the function to all CSV files in the list using lapply
modified_data <- lapply(pack.ls, add_filename_column)

# Pack LF
pack.lf <- rbindlist(modified_data, fill=TRUE)
pack.lf[, pack := str_extract(home_range, "^[^_]+")]
pack.lf[, year := tstrsplit(home_range, "_", keep = 2)]
pack.lf[, season := ifelse(time_frame == "yearly", "full-year", 
                             unlist(tstrsplit(home_range, "_", keep = 3)))]

lf.cast <- dcast(pack.lf[road_layer == "Total"], pack + year + season + study_area ~ road_layer, 
                   value.var = "density_km_per_km2")
setnames(lf.cast, "Total", "lf_dens")

#fwrite(lf.cast, file = "Regional_lf-density_by-pack.csv")

# Individuals
# Create a list of all pack yearly and seasonal datasets
indivs.ls <- list.files(path = "Pack_Dataset/LF_Dens/Individual", 
                        pattern = '.csv', full.names = TRUE)

# Function to read a CSV file, add a new column with the file name, and return the modified data frame
add_filename_column <- function(file_path) {
  # Read the CSV file into a data frame
  df <- fread(file_path)
  
  # Extract the first 3 or 4 letters of the file name before the first "_"
  file_name <- gsub("^([a-zA-Z]{3,4})_.*", "\\1", basename(file_path))  # This regex extracts the first 3 or 4 letters
  parts <- strsplit(basename(file_path), "_")[[1]]
  season <- parts[3]
  #survey <- gsub(".*-(\\w+).csv.*", "\\1", basename(file_path))
  
  # Add a new column with the extracted file name part
  df$study_area <- file_name
  df$time_frame <- season
  #df$survey <- survey
  
  return(df)
}

# Apply the function to all CSV files in the list using lapply
modified_data <- lapply(indivs.ls, add_filename_column)

indiv.lf <- rbindlist(modified_data, fill=TRUE)
#indiv.prey[, pack := str_extract(shapefile, "^[^_]+")]
indiv.lf[, Animal_ID := str_extract(home_range, "^[^_]+(?:_[^_]+)*?(?=_Snow|_SnowFree|_aKDE)")]
indiv.lf[, season := ifelse(time_frame == "yearly", "full-year", 
                             str_extract(home_range, "SnowFree|Snow"))]

indiv.cast <- dcast(indiv.lf[road_layer == "Total"], Animal_ID + season + study_area ~ road_layer, 
                    #fun = sum,
                    value.var = "density_km_per_km2")

setnames(indiv.cast, "Total", "lf_dens")

#fwrite(indiv.cast, file = "Pack_Dataset/Summary/Regional_lf-density_by-individual.csv")

## Step Length
# Create a list of all pack yearly and seasonal datasets
pack.ls <- list.files(path = "Pack_Dataset/Step_Length/Pack", 
                      pattern = '.csv', full.names = TRUE)

# Function to read a CSV file, add a new column with the file name, and return the modified data frame
add_filename_column <- function(file_path) {
  # Read the CSV file into a data frame
  df <- fread(file_path)
  
  # Extract the first 3 or 4 letters of the file name before the first "_"
  parts <- strsplit(basename(file_path), "_")[[1]]
  season <- parts[1]
  
  # Add a new column with the extracted file name part
  df$time_frame <- season
  
  return(df)
}

# Apply the function to all CSV files in the list using lapply
modified_data <- lapply(pack.ls, add_filename_column)

# Pack LF
pack.sl <- rbindlist(modified_data, fill=TRUE)
pack.sl[, pack := PackID]
pack.sl[, year := Deployment_Year]
pack.sl[, season := ifelse(time_frame == "Yearly", "full-year", 
                           Season)]
names.sl <- paste0('sl', sep = "_", names(pack.sl)[4:7])
pack.sl[, (names.sl) := .SD, .SDcols= c(4:7)]

#fwrite(pack.sl[, c("pack", "year", "season", "sl_mean", "sl_med")], file = "Pack_Dataset/Summary/Seasonal_step-length_by-pack.csv")

# Individuals
# Create a list of all pack yearly and seasonal datasets
indivs.ls <- list.files(path = "Pack_Dataset/Step_Length/Individual", 
                        pattern = '.csv', full.names = TRUE)

# Function to read a CSV file, add a new column with the file name, and return the modified data frame
add_filename_column <- function(file_path) {
  # Read the CSV file into a data frame
  df <- fread(file_path)
  
  # Extract the first 3 or 4 letters of the file name before the first "_"
  file_name <- gsub("^([a-zA-Z]{3,4})_.*", "\\1", basename(file_path))  # This regex extracts the first 3 or 4 letters
  parts <- strsplit(basename(file_path), "_")[[1]]
  season <- parts[2]
  #survey <- gsub(".*-(\\w+).csv.*", "\\1", basename(file_path))
  
  # Add a new column with the extracted file name part
  df$study_area <- file_name
  df$time_frame <- season
  #df$survey <- survey
  
  return(df)
}

# Apply the function to all CSV files in the list using lapply
modified_data <- lapply(indivs.ls, add_filename_column)

indiv.sl <- rbindlist(modified_data, fill=TRUE)
#indiv.prey[, pack := str_extract(shapefile, "^[^_]+")]
indiv.sl[, Animal_ID := ifelse(time_frame == "yearly", Animal_ID,
                               str_extract(IndSeason, "^[^_]+(?:_[^_]+)*?(?=_Snow|_SnowFree|_aKDE)"))]
indiv.sl[, season := ifelse(time_frame == "yearly", "full-year", 
                            str_extract(IndSeason, "SnowFree|Snow"))]
names.sl <- paste0('sl', sep = "_", names(indiv.sl)[2:5])
indiv.sl[, (names.sl) := .SD, .SDcols= c(2:5)]

indiv.merge <- merge(indiv.sl, id_table, by = "Animal_ID")

fwrite(indiv.merge[, c("Animal_ID", "PackID", "study_area", "Deployment_Year", "season",
                       "sl_mean", "sl_med")], 
       file = "Pack_Dataset/Summary/Seasonal_step-length_by-individual.csv")

### Merge datasets
## Packs
# Pack aKDEs
akde.full <- fread("Output/Seasonal_akde_area_full.csv")
colnames(akde.full) <- tolower(colnames(akde.full))
akde.full[, study_area := ifelse(study_area == "SEMB", "GHA", "RMNP")]
akde.full[, season := ifelse(season == "Year-round", "full-year", season)]
#akde.full[, year := as.character(year)]

# Population Size
pop.df <- fread("Pack_Dataset/Summary/Yearly_pack-size.csv")
pop.df[, pack := gsub(" ", "-", pack)]

pack.ds <- as.data.table(full_join(akde.full[, !c("columnnames", "values")], 
                               distinct(pop.df[, !c("animal_ID")]), 
                               by = c("pack", "year", "study_area")))

# Prey density
prey.dens <- fread("Pack_Dataset/Summary/Regional_seasonal_prey-density_by-pack_updated.csv")

pack.ds <- merge(pack.ds, prey.dens[, !c("elk_density", "elk_density_averaged")],
                 by = c("pack", "year", "season", "study_area"))

# Habitat density
hab.dens <- fread("Pack_Dataset/Summary/Regional_habitat-proportion_by-pack.csv")

pack.ds <- merge(pack.ds, 
                 hab.dens[, !("Urban")], 
                 by = c("pack", "year", "season", "study_area"))

# LF density
lf.dens <- fread("Pack_Dataset/Summary/Regional_lf-density_by-pack.csv")

pack.ds <- merge(pack.ds, lf.dens, 
                 by = c("pack", "year", "season", "study_area"))

# Step Length
sl.pack <- fread("Pack_Dataset/Summary/Seasonal_step-length_by-pack.csv")
sl.pack[, pack := gsub(" ", "-", pack)]

pack.ds <- merge(pack.ds, sl.pack, 
                 by = c("pack", "year", "season"))

names(pack.ds) <- tolower(names(pack.ds))

# Niche Breadth
pack.niche <- fread("Pack_Dataset/Niche_Breadth/Pack_nichebreadth_by-season.csv")
colnames(pack.niche) <- tolower(colnames(pack.niche))
pack.niche[, pack := gsub(" ", "-", pack)]
pack.niche[, season := gsub( " ", "", season) ]

pack.ds <- merge(pack.ds, unique(pack.niche[, !c("niche_cat", "tot_diet", "perc_diet")]), 
                  by = c("season", "study_area", "pack"), all.x = T)

#fwrite(pack.ds[order(study_area, pack, year, season)], file = "Pack_dataset_full_updated_2025-08-15.csv")

## Individuals
# Individual aKDEs
akde.indiv <- fread("Pack_Dataset/Summary/Seasonal_akde_area_individual.csv")
colnames(akde.indiv) <- tolower(colnames(akde.indiv))

# Population Size
indiv.df <- fread("Pack_Dataset/Summary/Yearly_pack-size.csv")
indiv.df[, pack := gsub(" ", "-", pack)]
colnames(indiv.df) <- tolower(colnames(indiv.df))

indiv.ds <- as.data.table(merge(akde.indiv, indiv.df, 
                                   by = c("animal_id", "study_area"), fill = T))

# Prey density
prey.dens <- fread("Pack_Dataset/Summary/Regional_seasonal_prey-density_by-individual_updated.csv")
colnames(prey.dens) <- tolower(colnames(prey.dens))

indiv.ds <- merge(indiv.ds, prey.dens[, !c("elk_density", "elk_density_averaged")],
                 by = c("animal_id", "year", "season", "study_area"), fill = T)

# Habitat density
hab.dens <- fread("Pack_Dataset/Summary/Regional_habitat-proportion_by-individual.csv")
colnames(hab.dens) <- tolower(colnames(hab.dens))

indiv.ds <- merge(indiv.ds, 
                 hab.dens[, !("urban")], 
                 by = c("animal_id", "season", "study_area"))

# LF density
lf.dens <- fread("Pack_Dataset/Summary/Regional_lf-density_by-individual.csv")
colnames(lf.dens) <- tolower(colnames(lf.dens))

indiv.ds <- merge(indiv.ds, lf.dens, 
                 by = c("animal_id", "season", "study_area"))

# Step Length
sl.indiv <- fread("Pack_Dataset/Summary/Seasonal_step-length_by-individual.csv")
colnames(sl.indiv) <- tolower(colnames(sl.indiv))
setnames(sl.indiv, "packid", "pack")
setnames(sl.indiv, "deployment_year", "year")
sl.indiv[, pack := gsub(" ", "-", pack)]

indiv.ds <- merge(indiv.ds, sl.indiv, 
                 by = c("animal_id", "year", "season", "study_area", "pack"))

# Niche Breadth
indiv.niche <- fread("Pack_Dataset/Niche_Breadth/Individual_nichebreadth_by-season.csv")
colnames(indiv.niche) <- tolower(colnames(indiv.niche))
indiv.niche[, pack := gsub(" ", "-", pack)]
indiv.niche[, season := gsub( " ", "", season) ]

indiv.ds <- merge(indiv.ds, unique(indiv.niche[, !c("niche_cat", "tot_diet", "perc_diet")]), 
                  by = c("animal_id", "season", "study_area", "pack"), all.x = T)

#fwrite(indiv.ds[order(study_area, animal_id, year, season)], file = "Individual_dataset_full_updated_2025-08-15.csv")

## Calculate Home Range Stats
## Read in aKDEs
# Create a list of all individual aKDEs in a given folder using the ".shp" file ending
GHA.95UD <- list.files(path = "KDE_HomeRanges/GHAIndividual_Season/95UD", 
                       pattern = '.shp', full.names = TRUE)

RMNP.95UD <- list.files(path = "KDE_HomeRanges/RMNPIndiv_Season/95UD", 
                        pattern = '.shp', full.names = TRUE)


# Use lapply to read in all aKDE shapefiles using st_read
gha.list.95 <- lapply(GHA.95UD, st_read)

rmnp.list.95 <- lapply(RMNP.95UD, st_read)

# lapply over list again to subset to only the estimate contour - in column 2 - rather than the 95% CI
#list.95 <- lapply(shapefile_df, function(x) {x[2,]})

# Transform to WGS84 crs
gha_df_95 <- map(gha.list.95, ~ st_transform(., 32614))

rmnp_df_95 <- map(rmnp.list.95, ~ st_transform(., 32614))

# Create a new list of just Pack ID
names(gha_df_95) <- sub("^([^_]+_[0-9]+)_.*", "\\1", basename(GHA.95UD))

names(rmnp_df_95) <- sub("^([^_]+_[0-9]+)_.*", "\\1", basename(RMNP.95UD))

# Use rbind to merge all objects in list into a single item - helpful for plotting below
gha_sf_95 = do.call(rbind, gha_df_95)
gha_sf_95$AnimalID <- str_extract(names(gha_df_95), "^[^_]+(?:_[^_]+)*?(?=_Snow|_SnowFree|_aKDE)")
gha_sf_95$Season <- str_extract(names(gha_df_95), "SnowFree|Snow")
gha_sf_95$Area <- st_area(gha_sf_95)

gha_area <- as.data.table(gha_sf_95)
gha_area[, Area_km := set_units(Area, km^2)]
gha_area[!(AnimalID == "ER_W_52" | AnimalID == "ER_W_38" | AnimalID == "ER_W_44"), range(Area_km), by = Season]

rmnp_sf_95 <- do.call(rbind, rmnp_df_95)
rmnp_sf_95$AnimalID <- str_extract(names(rmnp_df_95), "^[^_]+(?:_[^_]+)*?(?=_Snow|_SnowFree|_aKDE)")
rmnp_sf_95$Season <- str_extract(names(rmnp_df_95), "SnowFree|Snow")
rmnp_sf_95$Area <- st_area(rmnp_sf_95)

rmnp_area <- as.data.table(rmnp_sf_95)
rmnp_area[, Area_km := set_units(Area, km^2)]
rmnp_area[, range(Area_km), by = Season]


