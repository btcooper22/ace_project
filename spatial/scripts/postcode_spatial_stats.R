# Packages
require(readr)
require(dplyr)
require(stringr)
require(magrittr)
require(tidyr)

# Load postcodes database
postcodes <- read_csv("spatial/data/raw/PCD_OA_LSOA_MSOA_LAD_AUG19_UK_LU.csv")

# Filter to bradford
bradford_codes <- postcodes %>% 
  filter(grepl("bradford", ladnm,
               ignore.case = TRUE)) %>% 
  # Add high-level postcode
  mutate(pcdhl = str_split(pcds, " ",
                           simplify = TRUE)[,1])

# Load IMD database
imd <- read_csv("spatial/data/raw/Indices_of_Multiple_Deprivation_(IMD)_2019.csv")

# Join
combined_df <- bradford_codes %>% 
  select(pcds, pcdhl, lsoa11cd,
         lsoa11nm, msoa11cd,
         msoa11nm) %>% 
  left_join(imd) %>% 
  # Remove ranks
  select(!contains("Rank")) %>% 
  # Remove deciles
  select(!contains("Dec")) %>% 
  # Cleanup
  select(-FID, -lsoa11nmw,
         -st_areasha, -st_lengths,
         -LSOA01NM, -LADcd, -LADnm,
         -Shape__Area, -Shape__Length)

# Prepare postcode-level ouput
output_pcd <- combined_df %>% 
  pivot_longer(7:27) %>% 
  group_by(pcds, name) %>% 
  summarise(value = median(value)) %>% 
  pivot_wider() %>% 
  ungroup()

# Prepare LSOA output
output_LSOA <- combined_df %>% 
  pivot_longer(7:27) %>% 
  group_by(lsoa11cd, name) %>% 
  summarise(value = median(value)) %>% 
  pivot_wider() %>% 
  ungroup()

# Prepare MSOA output
output_MSOA <- combined_df %>% 
  pivot_longer(7:27) %>% 
  group_by(msoa11cd, name) %>% 
  summarise(value = median(value)) %>% 
  pivot_wider() %>% 
  ungroup()

# Prepare high-level output
output_pcdhl <- combined_df %>% 
  pivot_longer(7:27) %>% 
  group_by(pcdhl, name) %>% 
  summarise(value = median(value)) %>% 
  pivot_wider() %>% 
  ungroup()

# Prepare translation
output_translation <- combined_df %>% 
  select(pcds, pcdhl, lsoa11cd,
         msoa11cd)

# Write outputs
write_csv(output_translation, "spatial/area_stats/postcode_ONS_translation.csv")
write_csv(output_pcd, "spatial/area_stats/bradford_imd_postcode.csv")
write_csv(output_LSOA, "spatial/area_stats/bradford_imd_LSOA.csv")
write_csv(output_MSOA, "spatial/area_stats/bradford_imd_MSOA.csv")
write_csv(output_pcdhl, "spatial/area_stats/bradford_imd_district.csv")
