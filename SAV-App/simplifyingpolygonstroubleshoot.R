## Cleaning up the sav2023 files 

#ideally we want to union the files, simplify, then cast to polygon and plot

#cast a small buffer between the shapes to prevent any weird microscopic line generation
#clean_2023 <- st_buffer(sav2023, dist = 0) <- this didn't work, just going to go with extracting the multipolygon
sav2023_union <- st_union(sav2023)
sav2023_unionpoly <- st_collection_extract(sav2023_union, "POLYGON")

sav2022_union <- st_union(sav2022)

sav2023_simpl <- st_simplify(sav2023_unionpoly, preserveTopology = TRUE, dTolerance = 5)
sav2022_simpl <- st_simplify(sav2022_union, preserveTopology = TRUE, dTolerance = 10)

sav2023_simplpolygon <- st_cast(sav2023_simpl, "POLYGON")

#end troubleshoot code
###############################################################################

#### let's test the new map:

sav2023 <- st_read(here("2023shp"))
sav2023 <- st_transform(sav2023, crs = "+proj=longlat +datum=WGS84") #transform to DD
sav2022 <- st_read(here("misc", "2022shp"))
sav2022 <- st_transform(sav2022, crs = "+proj=longlat +datum=WGS84") #transform to DD
sav2021 <- st_read(here("misc", "2021shp_multipolygon"))
sav2021 <- st_transform(sav2021, crs = "+proj=longlat +datum=WGS84") #transform to DD
sav2020 <- st_read(here("misc", "2020shp_multipolygon"))
sav2020 <- st_transform(sav2020, crs = "+proj=longlat +datum=WGS84") #transform to DD
sav2019 <- st_read(here("misc",  "2019shp_multipolygon"))
sav2019 <- st_transform(sav2019, crs = "+proj=longlat +datum=WGS84") #transform to DD

#create union shapes
sav2022_union <- st_union(sav2022)
sav2021_union <- st_union(sav2021)
sav2020_union <- st_union(sav2020)
sav2019_union <- st_union(sav2019)

#2023 needs extra step
sav2023_union <- st_union(sav2023)
sav2023_unionpoly <- st_collection_extract(sav2023_union, "POLYGON")

#simplify
sav2023_simpl <- st_simplify(sav2023_unionpoly, preserveTopology = TRUE, dTolerance = 10)
sav2022_simpl <- st_simplify(sav2022_union, preserveTopology = TRUE, dTolerance = 10)
sav2021_simpl <- st_simplify(sav2021_union, preserveTopology = TRUE, dTolerance = 10)
sav2020_simpl <- st_simplify(sav2020_union, preserveTopology = TRUE, dTolerance = 10)
sav2019_simpl <- st_simplify(sav2019_union, preserveTopology = TRUE, dTolerance = 10)

#cast to polygon for glleaflet
sav2019_simplpolygon <- st_cast(sav2019_simpl, "POLYGON")
sav2020_simplpolygon <- st_cast(sav2020_simpl, "POLYGON")
sav2021_simplpolygon <- st_cast(sav2021_simpl, "POLYGON")
sav2022_simplpolygon <- st_cast(sav2022_simpl, "POLYGON")
sav2023_simplpolygon <- st_cast(sav2023_simpl, "POLYGON")

#map

savmap <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(lng = -76.3, lat = 38.7, zoom = 8) %>%
  # addGlPolygons(data = sav2019_simplpolygon, color = "#67B44B", weight = 2, opacity = 1,
  #               popup = "SAV Coverage 2019",
  #               group = "SAV Coverage 2019") %>%
  # addGlPolygons(data = sav2020_simplpolygon, color = "#67B44B", weight = 2, opacity = 1,
  #               popup = "SAV Coverage 2020",
  #               group = "SAV Coverage 2020") %>%
  # addGlPolygons(data = sav2021_simplpolygon, color = "#67B44B", weight = 2, opacity = 1,
  #               popup = "SAV Coverage 2021",
  #               group = "SAV Coverage 2021") %>%
  # addGlPolygons(data = sav2022_simplpolygon, color = "#67B44B", weight = 2, opacity = 1,
  #               popup = "SAV Coverage 2022",
  #               group = "SAV Coverage 2022") %>%
  addGlPolygons(data = sav2023_simplpolygon, color = "#427330", weight = 2, opacity = 1,
                popup = "SAV Coverage 2023",
                group = "SAV Coverage 2023") %>%
  addLayersControl(
    overlayGroups = c(
      #"SAV Coverage 2019",
      #"SAV Coverage 2020",
      #"SAV Coverage 2021",
      #"SAV Coverage 2022",
      "SAV Coverage 2023"
    ),
    options = layersControlOptions(collapsed = FALSE))
savmap

#just do a little compare to see how much detail we lose:
savmap <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setView(lng = -76.3, lat = 38.7, zoom = 8) %>%
  addGlPolygons(data = sav2023_simplpolygon, color = "orange", weight = 2, opacity = 1,
                popup = "SAV Coverage 2023 Simple",
                group = "SAV Coverage 2023 Simple") %>%
  addGlPolygons(data = sav2023, color = "#67B44B", weight = 2, opacity = 1,
                popup = "SAV Coverage 2023 Full Detail",
                group = "SAV Coverage 2023 Full Detail") %>%
  addLayersControl(
    overlayGroups = c(
      "SAV Coverage 2023 Full Detail",
      "SAV Coverage 2023 Simple"
    ),
    options = layersControlOptions(collapsed = FALSE))
savmap