stopifnot(length(COUNTRY_CODE) == 1, nchar(COUNTRY_CODE) == 3)

# Read shapefiles
a_countries <- sf::st_read(PATH_COUNTRIES, quiet = TRUE) %>% 
  sf::st_make_valid()
a_subs      <- sf::st_read(PATH_SUBBASINS, quiet = TRUE) %>% 
  sf::st_make_valid()

# Ensure same CRS
if (sf::st_crs(a_countries) != sf::st_crs(a_subs)) {
  a_subs <- sf::st_transform(a_subs, sf::st_crs(a_countries))
}

# Filter country
country <- a_countries %>% filter(.data$GMI_CNTRY == COUNTRY_CODE)
if (nrow(country) == 0) stop("No country with GMI_CNTRY == ", COUNTRY_CODE)

# Intersections: subbasins partially or fully covered by the country polygon
inter_idx <- sf::st_intersects(a_subs, country, sparse = TRUE)
sel <- lengths(inter_idx) > 0
subs_sel <- a_subs[sel, ]

# Classify as FULL vs PARTIAL coverage (by area ratio of intersection)
inter_geom <- sf::st_intersection(sf::st_make_valid(subs_sel), sf::st_make_valid(country))
area_sub   <- sf::st_area(subs_sel)
area_int   <- sf::st_area(inter_geom)
cover      <- as.numeric(area_int) / as.numeric(area_sub)

subs_sel$coverage_class <- ifelse(cover >= 0.999, "FULL", "PARTIAL")
subs_sel$coverage_ratio <- cover

HYBAS_IDS <- subs_sel$HYBAS_ID
length(HYBAS_IDS)