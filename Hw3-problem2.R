library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

dir.create("raw", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# ---------------------------------------------------------------
# 1. DOWNLOAD (raw files are never edited)
# ---------------------------------------------------------------
lib_url  <- "https://data.cityofchicago.org/resource/x8fc-8rcq.csv"
area_url <- "https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON"
lib_raw_f  <- "raw/libraries_raw.csv"
area_raw_f <- "raw/community_areas.geojson"
access_date <- as.character(Sys.Date())

# Pagination: keep requesting pages until a page comes back short
if (!file.exists(lib_raw_f)) {
  page <- 1000; offset <- 0; pages <- list()
  repeat {
    u <- sprintf("%s?$limit=%d&$offset=%d&$order=:id", lib_url, page, offset)
    d <- read_csv(u, col_types = cols(.default = "c"), show_col_types = FALSE)
    pages[[length(pages) + 1]] <- d
    if (nrow(d) < page) break
    offset <- offset + page
  }
  write_csv(bind_rows(pages), lib_raw_f)
}
if (!file.exists(area_raw_f)) download.file(area_url, area_raw_f, mode = "wb")

lib_raw <- read_csv(lib_raw_f, col_types = cols(.default = "c"), show_col_types = FALSE)
names(lib_raw) <- tolower(names(lib_raw))
names(lib_raw)   # CHECK: confirm the name, address and location columns

# ---------------------------------------------------------------
# 2. CLEAN THE FACILITIES
# ---------------------------------------------------------------
# Handles "(lat, lon)" or "POINT (lon lat)" text, or separate lat/lon columns
parse_xy <- function(x) {
  nums <- regmatches(x, gregexpr("-?[0-9]+\\.?[0-9]*", x))
  t(vapply(nums, function(v) {
    if (length(v) < 2) return(c(NA_real_, NA_real_))
    a <- as.numeric(v[1]); b <- as.numeric(v[2])
    if (abs(a) > 60) c(b, a) else c(a, b)      # returns c(lat, lon)
  }, numeric(2)))
}

if (all(c("latitude", "longitude") %in% names(lib_raw))) {
  lat <- as.numeric(lib_raw$latitude); lon <- as.numeric(lib_raw$longitude)
} else {
  loc_col <- if ("location" %in% names(lib_raw)) "location" else grep("loc", names(lib_raw), value = TRUE)[1]
  xy <- parse_xy(lib_raw[[loc_col]]); lat <- xy[, 1]; lon <- xy[, 2]
}
name_col <- intersect(c("name_", "name", "branch_name"), names(lib_raw))[1]
addr_col <- intersect(c("address", "street_address"), names(lib_raw))[1]

lib <- tibble(facility_name = lib_raw[[name_col]],
              address       = lib_raw[[addr_col]],
              lat = lat, lon = lon)

# Data-quality flags
bad_missing <- is.na(lib$lat) | is.na(lib$lon)
bad_range   <- !bad_missing & (abs(lib$lat) > 90 | abs(lib$lon) > 180)
bad_zero    <- !bad_missing & lib$lat == 0 & lib$lon == 0
bad_noname  <- is.na(lib$facility_name) | trimws(lib$facility_name) == ""
dup_exact   <- duplicated(lib[, c("facility_name", "address", "lat", "lon")])
shared_xy   <- !bad_missing & (duplicated(lib[, c("lat", "lon")]) |
                                 duplicated(lib[, c("lat", "lon")], fromLast = TRUE))

removal_log <- tibble(
  reason = c("missing coordinates", "out-of-range coordinates", "(0,0) coordinates",
             "missing facility name", "exact duplicate record"),
  n_flagged = c(sum(bad_missing), sum(bad_range), sum(bad_zero), sum(bad_noname), sum(dup_exact)))
print(removal_log)
cat("Facilities sharing coordinates (KEPT):", sum(shared_xy), "\n")

lib_clean <- lib[!(bad_missing | bad_range | bad_zero | bad_noname | dup_exact), ] %>%
  mutate(site_id = sprintf("LIB%03d", row_number())) %>%
  select(site_id, facility_name, address, lat, lon)

cat(sprintf("Before cleaning: %d | after: %d | removed: %d\n",
            nrow(lib), nrow(lib_clean), nrow(lib) - nrow(lib_clean)))
stopifnot(!anyDuplicated(lib_clean$site_id), nrow(lib_clean) >= 20)
write_csv(lib_clean, "data/libraries_clean.csv")

# ---------------------------------------------------------------
# 3. PROJECT TO METERS
# ---------------------------------------------------------------
crs_m <- 26916   # NAD83 / UTM zone 16N, units = metres
sites_m <- st_as_sf(lib_clean, coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(crs_m)
cat("CRS:", st_crs(sites_m)$input, "| units:", st_crs(sites_m)$units_gdal, "\n")

# Boundaries (study area)
areas_raw <- st_read(area_raw_f, quiet = TRUE)
names(areas_raw) <- tolower(names(areas_raw))
id_col    <- grep("^area_num", names(areas_raw), value = TRUE)[1]
name_col2 <- intersect(c("community", "name", "area_name"), names(areas_raw))[1]

areas_m <- areas_raw %>%
  transmute(district_id = as.character(.data[[id_col]]),
            district_name = .data[[name_col2]]) %>%
  st_transform(crs_m) %>%
  filter(!st_is_empty(geometry)) %>%
  st_make_valid()
stopifnot(nrow(areas_m) >= 5)
study_area <- st_union(areas_m)

# data_sources.csv
data_sources <- tibble(
  dataset = c("Chicago public libraries", "Chicago community areas"),
  publisher = "City of Chicago Data Portal",
  url = c("https://data.cityofchicago.org/resource/x8fc-8rcq",
          "https://data.cityofchicago.org/resource/cauq-8yn6"),
  raw_file = c(lib_raw_f, area_raw_f),
  access_date = access_date,
  license = "City of Chicago open data terms of use (verify on portal)",
  notes = c("Downloaded with $limit/$offset pagination; raw file unchanged.",
            "Used as study-area boundary."))
write_csv(data_sources, "data_sources.csv")

# ---------------------------------------------------------------
# 4. MAPS
# ---------------------------------------------------------------
cap2 <- sprintf("Source: City of Chicago Data Portal (libraries; community areas). Accessed %s. CRS EPSG:%d (m).",
                access_date, crs_m)

p_sites <- ggplot() +
  geom_sf(data = areas_m, aes(fill = "Community area"), color = "grey60", linewidth = 0.2) +
  geom_sf(data = sites_m, aes(color = "Library"), size = 1.8) +
  scale_fill_manual(name = NULL, values = c("Community area" = "grey95")) +
  scale_color_manual(name = NULL, values = c("Library" = "firebrick")) +
  labs(title = sprintf("Public library locations in Chicago (n = %d)", nrow(sites_m)),
       caption = cap2) +
  theme_bw()

# ---------------------------------------------------------------
# 5. BUFFERS: union so overlaps count once; NOT clipped to the
#    boundary, so area outside the study boundary is included
# ---------------------------------------------------------------
buf500  <- st_union(st_buffer(sites_m, 500))
buf1000 <- st_union(st_buffer(sites_m, 1000))

buffer_summary <- tibble(
  radius_m = c(500, 1000),
  union_area_km2 = c(as.numeric(st_area(buf500)), as.numeric(st_area(buf1000))) / 1e6)
stopifnot(buffer_summary$union_area_km2[2] >= buffer_summary$union_area_km2[1])
print(buffer_summary)

# Extra detail: how much of each buffer falls inside vs. outside the city
inside <- c(as.numeric(st_area(st_intersection(buf500,  study_area))),
            as.numeric(st_area(st_intersection(buf1000, study_area)))) / 1e6
buffer_detail <- buffer_summary %>%
  mutate(inside_boundary_km2  = inside,
         outside_boundary_km2 = union_area_km2 - inside,
         pct_of_city_covered  = 100 * inside / (as.numeric(st_area(study_area)) / 1e6))
print(buffer_detail)

buf_sf <- st_sf(label = "500 m buffer (combined)", geometry = buf500)

p_buffers <- ggplot() +
  geom_sf(data = areas_m, fill = "grey97", color = "grey70", linewidth = 0.2) +
  geom_sf(data = buf_sf, aes(fill = label), alpha = 0.45, color = NA) +
  geom_sf(data = sites_m, aes(color = "Library"), size = 1.5) +
  scale_fill_manual(name = NULL, values = c("500 m buffer (combined)" = "steelblue")) +
  scale_color_manual(name = NULL, values = c("Library" = "firebrick")) +
  labs(title = "Area within 500 m of at least one library",
       subtitle = sprintf("Combined buffer = %.1f km\u00B2 (overlaps counted once)",
                          buffer_summary$union_area_km2[1]),
       caption = cap2) +
  theme_bw()

print(p_sites)
print(p_buffers)
ggsave("outputs/p2_sites.png",   p_sites,   width = 7, height = 8, dpi = 150)
ggsave("outputs/p2_buffers.png", p_buffers, width = 7, height = 8, dpi = 150)

# Keep the points and both combined buffer zones
st_write(sites_m, "data/sites_m.gpkg", delete_dsn = TRUE, quiet = TRUE)
st_write(st_sf(radius_m = c(500, 1000), geometry = c(buf500, buf1000)),
         "data/buffers_union.gpkg", delete_dsn = TRUE, quiet = TRUE)
write_csv(buffer_summary, "outputs/buffer_summary.csv")
