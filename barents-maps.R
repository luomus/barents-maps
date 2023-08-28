library(bit64)
library(dplyr)
library(gganimate)
library(ggplot2)
library(gifski)
library(glue)
library(rgbif)
library(sf)
library(tidyr)
library(transformr)
library(urltools)

url <- "https://geoserver.laji.fi"

path(url) <- "geoserver/LajiMapData/wms"

url <- param_set(url, "service", "WMS")
url <- param_set(url, "request", "GetMap")
url <- param_set(url, "width", 768)
url <- param_set(url, "height", 365)
url <- param_set(url, "bbox", url_encode("11,61,32,72"))
url <- param_set(url, "layers", url_encode("LajiMapData:barentsRegion2"))
url <- param_set(url, "format", "geojson")

barents_map <- st_read(url)

barents_map <- mutate(
  barents_map,
  shapeName = case_match(
    shapeName,
    "Lapland" ~ "Lappi",
    "Norrbottens län" ~ "Norrbotten",
    "Northern Ostrobothnia" ~ "Pohjois-Pohjanmaa",
    "North Karelia" ~ "Pohjois-Karjala",
    "Västerbottens län" ~ "Västerbotten",
    .default = shapeName
  )
)

bbox <- st_as_text(st_as_sfc(round(st_bbox(barents_map)) - c(1, 1, 0, -1)))

locale <- list(
  c(lang = "en", n = "Individuals"),
  c(lang = "fi", n = "Yksilömäärä"),
  c(lang = "sv", n = "Individer"),
  c(lang = "se", n = "Individuálamearri")
)

# onc_gor <- name_backbone("Oncorhynchus gorbuscha")
#
# onc_gor_key <- onc_gor[[1, "usageKey"]]
#
# onc_gor_dwnld <- occ_download(
#   type="and",
#   pred("taxonKey", onc_gor_key),
#   pred("hasGeospatialIssue", FALSE),
#   pred("hasCoordinate", TRUE),
#   pred("occurrenceStatus","PRESENT"),
#   pred_gte("year", 1900),
#   pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
#   pred_within(bbox),
#   pred_or(
#     pred_lt("coordinateUncertaintyInMeters",10000),
#     pred_isnull("coordinateUncertaintyInMeters")
#   ),
#   format = "SIMPLE_CSV"
# )
#
# occ_download_wait(onc_gor_dwnld)

onc_gor_data <- occ_download_import(occ_download_get("0060246-230530130749713"))

onc_gor_data <- st_as_sf(
  onc_gor_data, coords = c("decimalLongitude", "decimalLatitude"),
  crs = "EPSG:4326"
)

onc_gor_data <- st_filter(onc_gor_data, st_buffer(barents_map, 10000))

onc_gor_data <- mutate(
  onc_gor_data,
  year = factor(year, levels = c(seq.int(min(year), max(year)), "All Years")),
  Individuals = cut(
    replace_na(individualCount, 1),
    c(0, 9, 99, 999, 999999),
    c("<10", "10-99", "100-999", ">999")
  )
)

onc_gor_data <- bind_rows(
  onc_gor_data,
  mutate(onc_gor_data, year = factor("All Years", levels = levels(year)))
)

for (i in locale) {

  onc_gor_gg <-
    ggplot(barents_map) +
    geom_sf(color = "grey25", fill = "grey90") +
    geom_sf_text(aes(label = shapeName)) +
    geom_sf(
      data = onc_gor_data,
      aes(group = year, color = Individuals, size = Individuals)
    ) +
    scale_color_viridis_d() +
    theme_minimal() +
    theme(
      plot.title = element_text(vjust = -25),
      plot.subtitle = element_text(vjust = -15, face = "italic"),
      legend.position = c(.1, .15)
    ) +
    transition_states(year, wrap = FALSE) +
    labs(
      title = "{closest_state}", subtitle = "Oncorhynchus gorbuscha",
      x = NULL, y = NULL, color = i[["n"]], size = i[["n"]]
    )

  anim_save(
    sprintf("onc-gor-%s.webm", i[["lang"]]),
    animate(
      onc_gor_gg, nframes = nlevels(onc_gor_data$year) * 5, fps = 10,
      renderer = av_renderer(), end_pause = nlevels(onc_gor_data$year) * .5
    )
  )

}

# lup_pol <- name_backbone("Lupinus polyphyllus")
#
# lup_pol_key <- lup_pol[[1, "usageKey"]]
#
# lup_pol_dwnld <- occ_download(
#   type="and",
#   pred("taxonKey", lup_pol_key),
#   pred("hasGeospatialIssue", FALSE),
#   pred("hasCoordinate", TRUE),
#   pred("occurrenceStatus","PRESENT"),
#   pred_gte("year", 1900),
#   pred_not(pred_in("basisOfRecord", c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
#   pred_within(bbox),
#   pred_or(
#     pred_lt("coordinateUncertaintyInMeters",10000),
#     pred_isnull("coordinateUncertaintyInMeters")
#   ),
#   format = "SIMPLE_CSV"
# )
#
# occ_download_wait(lup_pol_dwnld)

lup_pol_data <- occ_download_import(occ_download_get("0060596-230530130749713"))

lup_pol_data <- st_as_sf(
  lup_pol_data, coords = c("decimalLongitude", "decimalLatitude"),
  crs = "EPSG:4326"
)

lup_pol_data <- st_filter(lup_pol_data, st_buffer(barents_map, 10000))

lup_pol_data <- mutate(
  lup_pol_data,
  year = factor(year, levels = c(seq.int(min(year), max(year)), "All Years")),
  Individuals = cut(
    replace_na(individualCount, 1),
    c(0, 9, 99, 999, 999999),
    c("<10", "10-99", "100-999", ">999")
  )
)

lup_pol_data <- bind_rows(
  lup_pol_data,
  mutate(lup_pol_data, year = factor("All Years", levels = levels(year)))
)

for (i in locale) {

  lup_pol_gg <-
    ggplot(barents_map) +
    geom_sf(color = "grey25", fill = "grey90") +
    geom_sf_text(aes(label = shapeName)) +
    geom_sf(
      data = lup_pol_data,
      aes(group = year, color = Individuals, size = Individuals)
    ) +
    scale_color_viridis_d() +
    labs() +
    theme_minimal() +
    theme(
      plot.title = element_text(vjust = -25),
      plot.subtitle = element_text(vjust = -15, face = "italic"),
      legend.position = c(.1, .15)
    ) +
    transition_states(year, wrap = FALSE) +
    labs(
      title = "{closest_state}", subtitle = "Lupinus polyphyllus",
      x = NULL, y = NULL, color = i[["n"]], size = i[["n"]]
    )

  anim_save(
    sprintf("lup-pol-%s.webm", i[["lang"]]),
    animate(
      lup_pol_gg, nframes = nlevels(lup_pol_data$year) * 5, fps = 10,
      renderer = av_renderer(), end_pause = nlevels(lup_pol_data$year) * .5
    )
  )

}

# lup_noo <- name_backbone("Lupinus nootkatensis")
#
# lup_noo_key <- lup_noo[[1, "usageKey"]]
#
# lup_noo_dwnld <- occ_download(
#   type="and",
#   pred("taxonKey", lup_noo_key),
#   pred("hasGeospatialIssue", FALSE),
#   pred("hasCoordinate", TRUE),
#   pred("occurrenceStatus","PRESENT"),
#   pred_gte("year", 1900),
#   pred_not(pred_in("basisOfRecord", c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
#   pred_within(bbox),
#   pred_or(
#     pred_lt("coordinateUncertaintyInMeters",10000),
#     pred_isnull("coordinateUncertaintyInMeters")
#   ),
#   format = "SIMPLE_CSV"
# )
#
# occ_download_wait(lup_noo_dwnld)

lup_noo_data <- occ_download_import(occ_download_get("0062518-230530130749713"))

lup_noo_data <- st_as_sf(
  lup_noo_data, coords = c("decimalLongitude", "decimalLatitude"),
  crs = "EPSG:4326"
)

lup_noo_data <- st_filter(lup_noo_data, st_buffer(barents_map, 10000))

lup_noo_data <- mutate(
  lup_noo_data,
  year = factor(year, levels = c(seq.int(min(year), max(year)), "All Years")),
  Individuals = cut(
    replace_na(individualCount, 1),
    c(0, 9, 99, 999, 999999),
    c("<10", "10-99", "100-999", ">999")
  )
)

lup_noo_data <- bind_rows(
  lup_noo_data,
  mutate(lup_noo_data, year = factor("All Years", levels = levels(year)))
)

for (i in locale) {

  lup_noo_gg <-
    ggplot(barents_map) +
    geom_sf(color = "grey25", fill = "grey90") +
    geom_sf_text(aes(label = shapeName)) +
    geom_sf(
      data = lup_noo_data,
      aes(group = year, color = Individuals, size = Individuals)
    ) +
    scale_color_viridis_d() +
    labs() +
    theme_minimal() +
    theme(
      plot.title = element_text(vjust = -25),
      plot.subtitle = element_text(vjust = -15, face = "italic"),
      legend.position = c(.1, .15)
    ) +
    transition_states(year, wrap = FALSE) +
    labs(
      title = "{closest_state}", subtitle = "Lupinus nootkatensis",
      x = NULL, y = NULL, color = i[["n"]], size = i[["n"]]
    )

  anim_save(
    sprintf("lup-noo-%s.webm", i[["lang"]]),
    animate(
      lup_noo_gg, nframes = nlevels(lup_noo_data$year) * 5, fps = 10,
      renderer = av_renderer(), end_pause = nlevels(lup_noo_data$year) * .5
    )
  )

}

# her_per <- name_backbone("Heracleum L.")
#
# her_per_key <- her_per[[1, "usageKey"]]
#
# her_per_dwnld <- occ_download(
#   type="and",
#   pred("taxonKey", her_per_key),
#   pred_or(
#     pred("taxonKey", 3034825),
#     pred("taxonKey", 3628745),
#     pred("taxonKey", 3628608),
#     pred("taxonKey", 10748454),
#     pred("taxonKey", 9919952),
#     pred("taxonKey", 9690856),
#     pred("taxonKey", 10217435),
#     pred_like("verbatimScientificName", "*persicum*"),
#     pred_like("verbatimScientificName", "*ntegazzianum*"),
#     pred_like("verbatimScientificName", "*sosnowskyi*"),
#     pred_like("verbatimScientificName", "*pubescens*")
#   ),
#   pred("hasGeospatialIssue", FALSE),
#   pred("hasCoordinate", TRUE),
#   pred("occurrenceStatus","PRESENT"),
#   pred_gte("year", 1900),
#   pred_not(pred_in("basisOfRecord", c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
#   pred_within(bbox),
#   pred_or(
#     pred_lt("coordinateUncertaintyInMeters",10000),
#     pred_isnull("coordinateUncertaintyInMeters")
#   ),
#   format = "SIMPLE_CSV"
# )
#
# occ_download_wait(her_per_dwnld)

her_per_data <- occ_download_import(occ_download_get("0062695-230530130749713"))

her_per_data <- st_as_sf(
  her_per_data, coords = c("decimalLongitude", "decimalLatitude"),
  crs = "EPSG:4326"
)

her_per_data <- st_filter(her_per_data, st_buffer(barents_map, 10000))

her_per_data <- mutate(
  her_per_data,
  year = factor(year, levels = c(seq.int(min(year), max(year)), "All Years")),
  Individuals = cut(
    replace_na(individualCount, 1),
    c(0, 9, 99, 999, 999999),
    c("<10", "10-99", "100-999", ">999")
  )
)

her_per_data <- bind_rows(
  her_per_data,
  mutate(her_per_data, year = factor("All Years", levels = levels(year)))
)

for (i in locale) {

  her_per_gg <-
    ggplot(barents_map) +
    geom_sf(color = "grey25", fill = "grey90") +
    geom_sf_text(aes(label = shapeName)) +
    geom_sf(
      data = her_per_data,
      aes(group = year, color = Individuals, size = Individuals)
    ) +
    scale_color_viridis_d() +
    labs() +
    theme_minimal() +
    theme(
      plot.title = element_text(vjust = -25),
      plot.subtitle = element_text(vjust = -15, face = "italic"),
      legend.position = c(.1, .15)
    ) +
    transition_states(year, wrap = FALSE) +
    labs(
      title = "{closest_state}", subtitle = "Heracleum persicum agg.",
      x = NULL, y = NULL, color = i[["n"]], size = i[["n"]]
    )

  anim_save(
    sprintf("her-per-%s.webm", i[["lang"]]),
    animate(
      her_per_gg, nframes = nlevels(her_per_data$year) * 5, fps = 10,
      renderer = av_renderer(), end_pause = nlevels(her_per_data$year) * .5
    )
  )

}
