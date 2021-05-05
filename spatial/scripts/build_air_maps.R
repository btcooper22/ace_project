# Packages
require(readr)
require(dplyr)
require(ggplot2)
require(foreach)
require(doParallel)
require(tools)
require(tidyr)
require(magrittr)

# Load postcode conversion file
postcodes <- read_csv("spatial/data/grid/postcode_gridref.csv") %>% 
  filter(Description != "postcode not found")

# Load pollution data
no2_dat <- read_csv("spatial/data/air/30-no2-2018.csv", skip = 5)
nox_dat <- read_csv("spatial/data/air/30-nox-2018.csv", skip = 5) %>% 
  select(1:6)
pm10_dat <- read_csv("spatial/data/air/30-pm10-2018.csv", skip = 5) %>% 
  select(1:6)
pm25_dat <- read_csv("spatial/data/air/30-pm25-2018.csv", skip = 5) %>% 
  select(1:6)

# Combine pollution data
pollution <- no2_dat %>% 
  left_join(nox_dat) %>% 
  left_join(pm10_dat) %>% 
  left_join(pm25_dat)

# Plot
pollution %>% 
  pivot_longer(6:9) %>% 
  ggplot(aes(x / 1000, y / 1000,
             fill = value))+
  geom_raster()+
  scale_fill_viridis_c(option = "B")+
  coord_fixed()+
  theme_classic(20)+
  facet_wrap(~name)
  
# postcodes %>% 
#   ggplot(aes(`X (easting)`/ 1000,
#              `Y (northing)` / 1000))+
#   geom_point()+
#   coord_fixed()+
#   theme_classic(20)

# Set up parallel
ptm <- proc.time()
psnice(value = 19)
n_cores <- 8
cl <- makeCluster(ifelse(detectCores() <= n_cores,
                         detectCores() - 1,
                         n_cores))
registerDoParallel(cl)


# For each postcode, find nearest pollution and record
output <- foreach(i = 1:nrow(postcodes),
                      .combine = "rbind",
                      .packages = "dplyr") %dopar%
  {
    # Isolate current postcode
    target <- postcodes[i,]
    
    # Measure distance to pollution measurements
    xdist <- (target$`X (easting)` - pollution$x)^2
    ydist <- (target$`Y (northing)` - pollution$y)^2
    dist <- sqrt(xdist + ydist)
    
    # Find closest and add
    pollution[which.min(dist),] %>% 
      select(6:9) %>% 
      cbind(target, .)
  }

output %>% 
  pivot_longer(8:11) %>% 
  ggplot(aes(`X (easting)`/ 1000,
             `Y (northing)` / 1000,
             colour = value))+
  geom_point()+
  scale_colour_viridis_c(option = "B")+
  coord_fixed()+
  theme_classic(20)+
  facet_wrap(~name)

# Clean
output_pollution <- output %>% 
  select(Postcode, Total_NO2_18,
         Total_NOx_18, Total_PM10_18,
         Total_PM2.5_18) %>% 
  rename(pcds = "Postcode",
         NO2 = "Total_NO2_18",
         NOx = "Total_NOx_18",
         PM10 = "Total_PM10_18",
         PM25 = "Total_PM2.5_18")

# Aggregate to each level----

# Load translation database
translation <- read_csv("spatial/area_stats/postcode_ONS_translation.csv")

# Join to pollution by postcode
pollution_full <- translation %>% 
  left_join(output_pollution) %>% 
  na.omit()

# Prepare postcode-level ouput
output_pcd <- pollution_full %>% 
  pivot_longer(6:9) %>% 
  group_by(pcds, name) %>% 
  summarise(value = median(value)) %>% 
  pivot_wider() %>% 
  ungroup()

# Prepare LSOA output
output_LSOA <- pollution_full %>% 
  pivot_longer(6:9) %>% 
  group_by(lsoa11cd, name) %>% 
  summarise(value = median(value)) %>% 
  pivot_wider() %>% 
  ungroup()

# Prepare MSOA output
output_MSOA <- pollution_full %>% 
  pivot_longer(6:9) %>% 
  group_by(msoa11cd, name) %>% 
  summarise(value = median(value)) %>% 
  pivot_wider() %>% 
  ungroup()

# Prepare ward output
output_ward <- pollution_full %>% 
  pivot_longer(6:9) %>% 
  group_by(WD15CD, name) %>% 
  summarise(value = median(value)) %>% 
  pivot_wider() %>% 
  ungroup()

# Prepare postcode district output
output_pcdhl <- pollution_full %>% 
  pivot_longer(6:9) %>% 
  group_by(pcdhl, name) %>% 
  summarise(value = median(value)) %>% 
  pivot_wider() %>% 
  ungroup()

# Write
write_csv(output_pcd, "spatial/area_stats/bradford_air_postcode.csv")
write_csv(output_LSOA, "spatial/area_stats/bradford_air_LSOA.csv")
write_csv(output_MSOA, "spatial/area_stats/bradford_air_MSOA.csv")
write_csv(output_ward, "spatial/area_stats/bradford_air_ward.csv")
write_csv(output_pcdhl, "spatial/area_stats/bradford_air_district.csv")
