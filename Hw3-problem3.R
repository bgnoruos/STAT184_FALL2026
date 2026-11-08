# ---------------------------------------------------------------
# 1. MATCH SITES TO AREAS
# ---------------------------------------------------------------
hits    <- st_intersects(sites_m, areas_m)   # list of matching area rows per site
n_match <- lengths(hits)

site_match_audit <- tibble(
  site_id        = sites_m$site_id,
  facility_name  = sites_m$facility_name,
  n_area_matches = n_match,
  matched_ids    = vapply(hits, function(i) paste(areas_m$district_id[i], collapse = ";"), ""))
write_csv(site_match_audit, "site_match_audit.csv")

# How many sites match zero, one, or multiple areas
audit_summary <- site_match_audit %>%
  mutate(status = case_when(n_area_matches == 0 ~ "zero matches",
                            n_area_matches == 1 ~ "exactly one match",
                            TRUE                ~ "multiple matches")) %>%
  count(status)
print(audit_summary)

# Sites WITHOUT exactly one match
print(site_match_audit %>% filter(n_area_matches != 1))

# Keep only exactly-one-match sites, each counted once
assigned <- site_match_audit %>%
  filter(n_area_matches == 1) %>%
  mutate(district_id = matched_ids)
n_assigned   <- nrow(assigned)
n_unassigned <- nrow(sites_m) - n_assigned

# ---------------------------------------------------------------
# 2. SUMMARY TABLE (zero-count areas kept)
# ---------------------------------------------------------------
counts <- assigned %>% count(district_id, name = "n_sites")

district_counts <- areas_m %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  left_join(counts, by = "district_id") %>%
  mutate(n_sites = replace_na(n_sites, 0L),       # zero, not missing
         sites_per_km2 = n_sites / area_km2) %>%
  select(district_id, district_name, n_sites, area_km2, sites_per_km2)

# Checks
stopifnot(all(district_counts$area_km2 > 0),
          sum(district_counts$n_sites) == n_assigned,
          !anyNA(district_counts$n_sites))
cat(sprintf("Assigned: %d | unassigned: %d | areas: %d | zero-count areas: %d\n",
            n_assigned, n_unassigned, nrow(district_counts), sum(district_counts$n_sites == 0)))

print(district_counts %>% arrange(desc(n_sites)), n = Inf)   # output the table
write_csv(district_counts, "outputs/district_counts.csv")

# ---------------------------------------------------------------
# 3. MAPS: same boundaries, CRS and extent; separate legends with units
# ---------------------------------------------------------------
map_data <- areas_m %>% left_join(district_counts, by = c("district_id", "district_name"))

bb <- st_bbox(areas_m)
same_extent <- coord_sf(crs = st_crs(areas_m),
                        xlim = c(bb["xmin"], bb["xmax"]),
                        ylim = c(bb["ymin"], bb["ymax"]), datum = NA)

cap3 <- sprintf(paste0("Source: City of Chicago Data Portal (libraries; community areas). Accessed %s. ",
                       "%d of %d sites unassigned (not in exactly one area). ",
                       "Areas labeled 0 have zero sites; grey = missing data (none)."),
                access_date, n_unassigned, nrow(sites_m))

# Label zero-count areas "0" so zero is distinct from missing (grey)
zero_areas <- filter(map_data, n_sites == 0)
zero_lab   <- if (nrow(zero_areas) > 0) {
  st_point_on_surface(zero_areas) %>% mutate(lab = "0")
} else NULL

add_zero_labels <- if (!is.null(zero_lab)) {
  geom_sf_text(data = zero_lab, aes(label = lab), size = 2.5, color = "grey20")
} else NULL

p_count <- ggplot(map_data) +
  geom_sf(aes(fill = n_sites), color = "white", linewidth = 0.2) +
  add_zero_labels +
  scale_fill_viridis_c(name = "Libraries\n(count)", option = "C", na.value = "grey70") +
  same_extent +
  labs(title = "Number of public libraries per community area", caption = cap3) +
  theme_bw()

p_density <- ggplot(map_data) +
  geom_sf(aes(fill = sites_per_km2), color = "white", linewidth = 0.2) +
  add_zero_labels +
  scale_fill_viridis_c(name = "Libraries\nper km\u00B2", option = "C", na.value = "grey70") +
  same_extent +
  labs(title = "Public library density per community area", caption = cap3) +
  theme_bw()

print(p_count)
print(p_density)
ggsave("outputs/p3_count.png",   p_count,   width = 7, height = 8, dpi = 150)
ggsave("outputs/p3_density.png", p_density, width = 7, height = 8, dpi = 150)

# Optional: sites over the boundaries
p_overlay <- ggplot() +
  geom_sf(data = areas_m, fill = "grey95", color = "grey50", linewidth = 0.2) +
  geom_sf(data = sites_m, color = "firebrick", size = 1.5) +
  labs(title = "Libraries over community areas", caption = cap3) +
  theme_bw()
print(p_overlay)

# ---------------------------------------------------------------
# 4. TOP AREAS (to help write the interpretation)
# ---------------------------------------------------------------
district_counts %>% arrange(desc(n_sites))      %>% slice_head(n = 5)
district_counts %>% arrange(desc(sites_per_km2)) %>% slice_head(n = 5)