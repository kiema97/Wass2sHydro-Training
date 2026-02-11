

# Read shapefiles
a_countries <- sf::st_read(PATH_COUNTRIES, quiet = TRUE) %>%
  sf::st_make_valid()

a_subs <- sf::st_read(PATH_SUBBASINS, quiet = TRUE) %>%
  sf::st_make_valid() %>%
  rename(HYBAS_ID = dplyr::all_of(SUBBASINS_ID_COL)) %>%
  mutate(HYBAS_ID = as.factor(HYBAS_ID))

# Ensure same CRS
if (sf::st_crs(a_countries) != sf::st_crs(a_subs)) {
  a_subs <- sf::st_transform(a_subs, sf::st_crs(a_countries))
}

# Filter country
country <- a_countries
if(!is.null(COUNTRY_CODE)){
  stopifnot(nchar(COUNTRY_CODE) == 3)
  country <- a_countries %>% filter(.data$GMI_CNTRY == COUNTRY_CODE)
}


if (nrow(country) == 0) stop("No country with GMI_CNTRY == ", COUNTRY_CODE)

# Intersections: subbasins partially or fully covered by the country polygon
inter_idx <- sf::st_intersects(a_subs, country, sparse = TRUE)
sel <- lengths(inter_idx) > 0
subs_sel <- a_subs[sel, ]

# Classify as FULL vs PARTIAL coverage (by area ratio of intersection)

if(!is.null(COUNTRY_CODE)){
  inter_geom <- sf::st_intersection(sf::st_make_valid(subs_sel), sf::st_make_valid(country))
  
}else{
  inter_geom <- sf::st_intersection(sf::st_make_valid(subs_sel), sf::st_make_valid(subs_sel))
  
}
area_sub   <- sf::st_area(subs_sel)
area_int   <- sf::st_area(inter_geom)
cover      <- as.numeric(area_int) / as.numeric(area_sub)

subs_sel$coverage_class <- ifelse(cover >= 0.999, "FULL", "PARTIAL")
subs_sel$coverage_ratio <- cover

HYBAS_IDS <- as.factor(subs_sel$HYBAS_ID)
length(HYBAS_IDS)
