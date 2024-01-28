#case3: mapping Mexico kidnapping
#setwd("/Users/eonkim/Dropbox (The University of Manchester)/R/EDI_qunt")

df <- read.csv("Municipal-Delitos-2015-2023_dic2023/Municipal-Delitos-2015-2023_dic2023.csv", fileEncoding = "Latin1")

colnames(df)
table(df$Tipo.de.delito)
table(df$Subtipo.de.delito)

df_secuestro <- subset(df, Subtipo.de.delito == "Secuestro")
table(df_secuestro$Modalidad)
num_Entidad <- length(unique(df_secuestro$Entidad)) #getting 32, matching with sf

#install and load the 'sf' package
install.packages("sf")
library(sf)

sf <- st_read("mex_admbnda_govmex_20210618_SHP/mex_admbnda_adm1_govmex_20210618.shp")

#install and load the 'tmap' and 'dplyr' package
install.packages("tmap")
library(tmap)
install.packages("dplyr")
library(dplyr)

#create a base map 
tm_shape(sf) +
  tm_polygons()

#make a panel
df_agg <- df_secuestro %>%
  group_by(year, Entidad) %>%
  summarise(kidnapping_cnt = sum(Enero + Febrero + Marzo + Abril + Mayo + Junio + Julio + Agosto + Septiembre + Octubre + Noviembre + Diciembre, na.rm = TRUE))

#matching names (two unmathing records)
df_agg$Entidad <- ifelse(df_agg$Entidad == "Querétaro", "Querétaro de Arteaga",
                         ifelse(df_agg$Entidad == "Ciudad de México", "Distrito Federal", df_agg$Entidad))


#merge kidnapping count with the mexico stat file
df_merged <- left_join(df_agg, sf, by = c("Entidad" = "ADM1_ES"))

# Remove rows with empty or invalid geometry
df_merged <- df_merged %>%
  filter(!(is.na(Shape_Area)))

table(df_merged$year)
#2015 2016 2017 2018 2019 
#32   32   32   32   26 

#convert merged_df to an sf object
sf_merged <- st_as_sf(df_merged)

tm_shape(sf_merged) +
  tm_borders() +
  tm_fill(col = "kidnapping_cnt", palette = "Purples") +
  tm_layout(title = "Kidnapping Counts by Mexico Entidad")

tm_shape(sf_merged %>% filter(year == 2018)) +
  tm_borders() +
  tm_fill(col = "kidnapping_cnt", palette = "Purples") +
  tm_layout(title = "Kidnapping Counts in 2018 by Mexico Entidad")

#install and load 'leaflet'
install.packages("leaflet")
library(leaflet)

leaflet(sf_merged) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolygons(fillColor = ~colorQuantile("Purples", kidnapping_cnt)(kidnapping_cnt),
              fillOpacity = 0.7,
              color = "white",
              weight = 1,
              label = ~paste(Entidad, kidnapping_cnt),
              labelOptions = labelOptions(direction = "auto"))


#filter the data for each year, rm-ed 2019 as 6 missing
sf_merged_filtered <- sf_merged %>%
  filter(year %in% c(2015, 2016, 2017, 2018))

#create the map with facets for each year
tm_shape(sf_merged_filtered) +
  tm_borders() +
  tm_fill(col = "kidnapping_cnt", palette = "Purples") +
  tm_layout(title = "Kidnapping Counts by Year (2015-2018)") +
  tm_facets(by = "year")


#spatial autocorrelation
#install and load the 'spData'and 'spdep' package
install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')
library(spDataLarge)
install.packages("spData")
library(spData)
install.packages("spdep")
library(spdep)

#aggregate kidnapping_cnt values for each geometry
sf_merged_filtered_crosssec <- sf_merged_filtered %>%
  group_by(Entidad) %>%
  summarise(kidnapping_cnt = sum(kidnapping_cnt))

#define nbhd polygons and create a spatial weight matrix
nb <- poly2nb(sf, queen=TRUE) #class(nb),summary(nb)
wl <- nb2listw(nb, style='W')
wm <- nb2mat(nb, style='W')
wm_rs <- nb2mat(w, style='W')

global_moran <- moran.test(sf_merged_filtered_crosssec$kidnapping_cnt, wl)
moran.plot(sf_merged_filtered_crosssec$kidnapping_cnt, w)

local_moran <- localmoran(sf_merged_filtered_crosssec$kidnapping_cnt, wl)

sf_merged_filtered_crosssec$normalised_kidnapping_cnt <- scale(sf_merged_filtered_crosssec$kidnapping_cnt) %>% as.vector()
sf_merged_filtered_crosssec$lag_kidnapping_cnt <- lag.listw(wl, sf_merged_filtered_crosssec$normalised_kidnapping_cnt)

# Create a new column 'quad_sig' based on local Moran's I
sf_merged_filtered_crosssec <- st_as_sf(sf_merged_filtered_crosssec) %>% 
  mutate(quad_sig = ifelse(normalised_kidnapping_cnt > 0 & 
                             lag_kidnapping_cnt > 0 & 
                             local_moran[, 5] <= 0.05, 
                           "high-high",
                           ifelse(normalised_kidnapping_cnt <= 0 & 
                                    lag_kidnapping_cnt <= 0 & 
                                    local_moran[, 5] <= 0.05, 
                                  "low-low", 
                                  ifelse(normalised_kidnapping_cnt > 0 & 
                                           lag_kidnapping_cnt <= 0 & 
                                           local_moran[, 5] <= 0.05, 
                                         "high-low",
                                         ifelse(normalised_kidnapping_cnt <= 0 & 
                                                  lag_kidnapping_cnt > 0 & 
                                                  local_moran[, 5] <= 0.05,
                                                "low-high", 
                                                "non-significant")))))
         
qtm(sf_merged_filtered_crosssec, fill="quad_sig", fill.title="LISA")
         

         

#crosssec base map
leaflet(sf_merged_filtered_crosssec) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolygons(fillColor = ~colorQuantile("Purples", kidnapping_cnt)(kidnapping_cnt),
              fillOpacity = 0.7,
              color = "white",
              weight = 1,
              label = ~paste(Entidad, kidnapping_cnt),
              labelOptions = labelOptions(direction = "auto"))
#create a LISA cluster map
leaflet(sf_merged_filtered_crosssec) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolygons(fillColor = ~case_when(
    quad_sig == "high-high" ~ "purple",
    quad_sig == "high-low" ~ "green",
    quad_sig == "low-high" ~ "orange",
    quad_sig == "low-low" ~ "blue",
    TRUE ~ "rgba(128, 128, 128, 0.02)"
  ),
  color = "white",
  stroke = FALSE,
  label = ~paste("Entidad:", Entidad, "<br>Quad Sig:", quad_sig))









##################################
#each year 
##################################
global_moran_results <- lapply(unique(sf_merged_filtered$year), function(year) {
  sf_year <- sf_merged_filtered[sf_merged_filtered$year == year, ]
  moran_global <- moran.test(sf_year$kidnapping_cnt, w)
  return(list(Year = year, Moran_Global = moran_global))
  })


local_moran_results <- lapply(unique(sf_merged_filtered$year), function(year) {
  sf_year <- sf_merged_filtered[sf_merged_filtered$year == year, ]
  local_moran <- localmoran(sf_year$kidnapping_cnt, w)
  return(list(Year = year, Local_Moran = local_moran))
})


