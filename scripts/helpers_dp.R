# ==== Libraries ==============================================================
# ------------------------------------------------------------------------------
# safeload(): Install and load required R packages (CRAN + GitHub)
#
# Purpose:
#   Ensures that a list of required packages is installed and loaded.
#   - Installs missing CRAN packages from the configured repo.
#   - Installs/updates a GitHub package (WASS2SHydroR) via 'remotes'.
#
# Arguments:
#   pkgs          Character vector of package names to ensure are available.
#   update_github Logical. If TRUE, force re-installation of WASS2SHydroR
#                 from GitHub even if it is already installed. Default: FALSE.
#
# Returns:
#   (Invisibly) the result of loading all requested packages.
#
# Notes:
#   - Uses 'remotes::install_github()' which is lighter than 'devtools'.
#   - Respects GITHUB_PAT if set (recommended for rate limits).
#   - Installs dependencies and parallelizes CRAN compilation when possible.
# ------------------------------------------------------------------------------

required_pkgs <- c(
  "WASS2SHydroR","lubridate","ncdf4","stars",
  "sf","tidyr","dplyr","purrr","ggplot2",
  "ggspatial","readr","stringr","tibble"
)

safeload <- function(pkgs, update_github = FALSE) {
  # Helper: check if a namespace is available without attaching it
  is_installed <- function(x) {
    requireNamespace(x, quietly = TRUE)
  }

  # Report missing packages (before any install attempts)
  missing_pkgs <- pkgs[!vapply(pkgs, is_installed, logical(1))]
  if (length(missing_pkgs)) {
    message("Missing packages detected: ", paste(missing_pkgs, collapse = ", "))
  }

  # Ensure a stable CRAN mirror (optional but recommended for reproducibility)
  if (is.null(getOption("repos")) || isTRUE(getOption("repos")["CRAN"] == "@CRAN@")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  # Ensure 'remotes' is available for GitHub installations
  if (!is_installed("remotes")) {
    install.packages("remotes", dependencies = TRUE)
  }

  # Install or update the GitHub package if requested or missing
  if (update_github || "WASS2SHydroR" %in% missing_pkgs) {
    remotes::install_github(
      "kiema97/AGRHYMET-WASS2SHydroR",
      build_vignettes = FALSE,
      upgrade = "never",
      auth_token = Sys.getenv("GITHUB_PAT", unset = NA),
      dependencies = TRUE,
      quiet = TRUE
    )
  }

  # Remove WASS2SHydroR from the missing list (if it was there)
  missing_pkgs <- setdiff(missing_pkgs, "WASS2SHydroR")

  # Install remaining CRAN packages if needed
  if (length(missing_pkgs) > 0) {
    install.packages(
      missing_pkgs,
      dependencies = TRUE,
      Ncpus = max(1L, parallel::detectCores(logical = TRUE) - 1L)
    )
  }

  # Silently attach all requested packages
  invisible(lapply(
    pkgs,
    function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))
  ))
}

# Execute once to ensure your working session has everything loaded
safeload(required_pkgs,update_github = update_github)


# suppressPackageStartupMessages({
#   library(dplyr)
#   library(readr)
#   library(stringr)
#   library(purrr)
#   library(tidyr)
#   library(sf)
#   library(lubridate)
#   library(leaflet)
#   library(ggplot2)
#   library(ncdf4)
# })


# WASS2SHydroR is expected to be installed; falls back gracefully if missing
has_wass2s <- requireNamespace("WASS2SHydroR", quietly = TRUE)
if (!has_wass2s) {
  message("NOTE: Package 'WASS2SHydroR' not found. You can still run most steps;\n",
          "the function 'wass2s_prepare_data()' will be called only if available.")
}

# Helper: safe parallel plan (base R's parallel via mclapply on Unix; fall back on lapply on Windows)
.parallel_map <- function(X, FUN, ...){
  if (.Platform$OS.type == "windows") {
    # Simple fallback for Windows notebooks to avoid cluster overhead for trainees
    lapply(X, FUN, ...)
  } else {
    parallel::mclapply(X, FUN, mc.cores = N_CORES, ...)
  }
}

# Walk predictor folders and parse filenames like: CanCM3.PRCP.nc, CCSM4.PRCP_f2025.nc, CCSM4.SST.nc
catalog_predictors <- function(base_dir = PATH_PREDICTORS){
  dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  tibble(dir = dirs) %>%
    mutate(var = basename(dir)) %>%
    filter(var %in% c("PRCP","SST")) %>%
    mutate(files = map(dir, ~list.files(.x, pattern = "\\.nc$", full.names = TRUE))) %>%
    tidyr::unnest(files) %>%
    mutate(
      file = files,
      fname = basename(file),
      # Patterns: Model.VAR.nc OR Model.VAR_fYYYY.nc
      model = str_replace(fname, "^([^.]+)\\..*$", "\\1"),
      var_detect = toupper(str_replace(fname, "^[^.]+\\.([A-Za-z]+).*$", "\\1")),
      init_year = ifelse(str_detect(fname, "_f\\d{4}"), as.integer(str_replace(fname, ".*_f(\\d{4}).*", "\\1")), NA_integer_),
      var = ifelse(var_detect %in% c("PRCP","SST"), var_detect, var)
    ) %>%
    select(model, var, init_year, file)
}

catalog_predictors <- function(
    base_dir = PATH_PREDICTORS,
    vars_keep = c("PRCP", "SST"),
    file_regex = "\\.nc(\\.(gz|zip))?$"  # accepte .nc, .nc.gz, .nc.zip
) {
  # --- Dépendances (pas de library() pour rester robuste en package)
  stopifnot(requireNamespace("tibble",  quietly = TRUE))
  stopifnot(requireNamespace("dplyr",   quietly = TRUE))
  stopifnot(requireNamespace("purrr",   quietly = TRUE))
  stopifnot(requireNamespace("stringr", quietly = TRUE))
  stopifnot(requireNamespace("tidyr",   quietly = TRUE))

  # --- Vérifs de base
  if (is.null(base_dir) || !dir.exists(base_dir)) {
    stop("`base_dir` introuvable : ", base_dir)
  }

  # --- Lister les sous-dossiers (1er niveau)
  dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  if (length(dirs) == 0L) {
    return(tibble::tibble(model = character(), var = character(),
                          init_year = integer(), file = character()))
  }

  tibble::tibble(dir = dirs) %>%
    dplyr::mutate(var_dir = basename(dir)) %>%                                  # nom du répertoire = var annoncée
    dplyr::filter(var_dir %in% vars_keep) %>%                                   # ne garder que PRCP/SST (paramétrable)
    dplyr::mutate(files = purrr::map(dir, ~ list.files(.x, pattern = file_regex, full.names = TRUE))) %>%
    tidyr::unnest(files, keep_empty = FALSE) %>%                                 # drop lignes vides
    dplyr::mutate(
      file  = files,
      fname = basename(file),

      # Modèle = tout avant le premier point (ex: "CanSIPSIC4" de "CanSIPSIC4.PRCP_f2025.nc")
      model = stringr::str_replace(fname, "^([^.]+)\\..*$", "\\1"),

      # Variable détectée depuis le nom (entre 1er point et suivant/underscore) → upper
      var_detect = stringr::str_to_upper(
        stringr::str_replace(fname, "^[^.]+[._]([A-Za-z]+).*$", "\\1")
      ),

      # Année d'initialisation si motif "_fYYYY" présent
      init_year = dplyr::if_else(
        stringr::str_detect(fname, "_f\\d{4}"),
        readr::parse_integer(stringr::str_extract(fname, "(?<=_f)\\d{4}")),
        NA_integer_
      ),

      # Choix final de la var : détection valide sinon le nom du dossier
      var = dplyr::if_else(var_detect %in% vars_keep, var_detect, var_dir)
    ) %>%
    dplyr::select(model, var, init_year, file) %>%
    dplyr::distinct() %>%
    dplyr::arrange(var, model, init_year, file)
}



extract_predictors_nested <- function(hybas_ids,
                                      models,
                                      hist_df = NULL,
                                      predictor = c("PRCP","SST"),
                                      predictors_root = "predictors",
                                      subbasins_sf = NULL,      # sf avec colonne HYBAS_ID (optionnel)
                                      bbox_list = NULL,         # nommée par HYBAS_ID: list("123"=c(xmin,ymin,xmax,ymax))
                                      init_year = NULL,         # ex. 2025 pour viser *_f2025.nc
                                      on_empty = c("warn","stop","quiet")) {
  on_empty <- match.arg(on_empty)

  # --- helpers concis ---------------------------------------------------------
  .bbox_for <- function(hid) {
    if (!is.null(bbox_list) && !is.null(bbox_list[[as.character(hid)]])) {
      bb <- bbox_list[[as.character(hid)]]
      stopifnot(is.numeric(bb), length(bb) == 4); return(as.numeric(bb))
    }
    if (!is.null(subbasins_sf)) {
      g <- subbasins_sf[subbasins_sf$HYBAS_ID == hid, , drop = FALSE]
      if (nrow(g)) {
        bb <- sf::st_bbox(sf::st_geometry(g))
        return(as.numeric(c( bb["ymax"],bb["xmin"], bb["ymin"], bb["xmax"])))
      }
    }
    NULL
  }

  .pattern <- function(model, var) {
    m <- stringr::str_replace_all(model, "\\.", "\\\\.")
    v <- stringr::str_replace_all(var,   "\\.", "\\\\.")
    paste0(sprintf("^%s\\.%s\\.nc$", m, v), "|", sprintf("^%s\\.%s.*_f%d\\.nc$", m, v, init_year))
  }

  .nc_dir <- function(var) {
    d <- file.path(predictors_root, toupper(var))
    if (dir.exists(d)) d else predictors_root
  }

  .extract_one <- function(hid, model, var) {
    dirv <- .nc_dir(var)
    pat  <- .pattern(model, toupper(var))

    if(!is.null(hist_df)){
      hid_hist_df <- hist_df %>%
        dplyr::filter(HYBAS_ID == hid) %>%
        dplyr::select(HYBAS_ID,DATE,Q)
    }

    fvec <- sort(list.files(dirv, pat, full.names = TRUE))
    if (!length(fvec)) {
      msg <- sprintf("[HYBAS %s][%s.%s] no files in %s", hid, model, var, dirv)
      if (on_empty == "stop") stop(msg) else if (on_empty == "warn") warning(msg)
      return(NULL)
    }
    bb <- if(PREDICTOR_VARS == "PRCP") .bbox_for(hid) else NULL
    df <- purrr::map(fvec, ~{
        predictors_data <- tryCatch({
          WASS2SHydroR::wass2s_prepare_data(
            x = .x, bbox = bb, spatial_reduce = "none",
            cell_layout = "wide", cell_prefix = tolower(var), verbose = FALSE
          ) |>
            dplyr::mutate(
              DATE     = lubridate::year(as.Date(.data$DATE)),  # année (entier)
              HYBAS_ID = hid
            ) |>
            dplyr::select(HYBAS_ID, DATE, dplyr::everything())
        }, error = function(e) {
          message(paste0("Product ", basename(.x), " not avaible for bassin : ", hid, "\n", e))
          tibble::tibble(HYBAS_ID = integer(), DATE = integer())
        })

        if(is.null(hist_df)){
          hid_hist_df <- data.frame(HYBAS_ID =hid,
                                    DATE =predictors_data$DATE,
                                    Q = NA_integer_)
        }

        hid_hist_df %>% right_join(predictors_data, by = c("DATE","HYBAS_ID")) %>%
        rename(YYYY = DATE) %>%
        dplyr::select(HYBAS_ID,YYYY,Q, everything())

    }) |>
      dplyr::bind_rows()
    df
  }

  # --- cœur : liste imbriquée HYBAS_ID -> MODEL -> DF -------------------------
  out <- vector("list", length(hybas_ids)); names(out) <- as.character(hybas_ids)
  for (hid in hybas_ids) {
      message(paste0("Processing bassin : ", hid))
    per_model <- vector("list", length(models)); names(per_model) <- models
    for (m in models) {
      # concatène PRCP/SST par DATE pour ce modèle
      pieces <- lapply(predictor, function(v) .extract_one(hid, m, v))
      pieces <- pieces[!vapply(pieces, is.null, logical(1))]
      if (length(pieces)) {
        per_model[[m]] <- Reduce(function(x,y) dplyr::full_join(x,y, by="YYYY"), pieces) |>
          dplyr::arrange(YYYY)
      }
    }
    per_model <- per_model[!vapply(per_model, is.null, logical(1))]
    if (length(per_model)) out[[as.character(hid)]] <- per_model
  }
  # retire HYBAS_ID vides
  out[vapply(out, function(x) length(x) > 0, logical(1))]
}


## Generate Maps

make_leaflet_map <- function(
    subs_sel,
    country,
    fill_col = NULL,          # colonne (dans subs_sel) pour la couleur
    id_col   = NULL,          # identifiant pour labels
    popup_cols = NULL,        # colonnes pour le popup
    palette = "viridis",      # viridis, magma, inferno, plasma, Spectral...
    basemaps = c("CartoDB.Positron", "OpenStreetMap", "Esri.WorldImagery"),
    outline_color = "#2b2b2b",
    outline_weight = 1,
    fill_opacity = 0.6,
    add_measure = FALSE       # <- par défaut off ; pas d'appel si TRUE et fonction absente
) {
  stopifnot(requireNamespace("leaflet", quietly = TRUE))
  if (!requireNamespace("sf", quietly = TRUE)) stop("Le package 'sf' est requis.")
  if (!inherits(subs_sel, "sf") || !inherits(country, "sf")) stop("subs_sel et country doivent être des objets 'sf'.")

  # helpers sûrs (n'appellent que si dispo)
  have_extras  <- function(fn) requireNamespace("leaflet.extras", quietly = TRUE)  &&
    fn %in% getNamespaceExports("leaflet.extras")
  have_extras2 <- function(fn) requireNamespace("leaflet.extras2", quietly = TRUE) &&
    fn %in% getNamespaceExports("leaflet.extras2")

  # labels & popups
  mk_label <- function(dat) if (!is.null(id_col) && id_col %in% names(dat)) sprintf("<strong>%s</strong>", dat[[id_col]]) else NULL
  mk_popup <- function(dat) {
    if (is.null(popup_cols)) return(NULL)
    keep <- popup_cols[popup_cols %in% names(dat)]
    if (!length(keep)) return(NULL)
    rows <- lapply(seq_len(nrow(dat)), function(i) {
      cells <- vapply(keep, function(cl) sprintf("<tr><th style='text-align:left;padding-right:8px;'>%s</th><td>%s</td></tr>", cl, as.character(dat[[cl]][i])), "")
      sprintf("<table class='table table-sm' style='font-size:12px;'>%s</table>", paste0(cells, collapse = ""))
    })
    unlist(rows)
  }

  # palette
  pal <- NULL; legend_args <- NULL
  if (!is.null(fill_col) && fill_col %in% names(subs_sel)) {
    vals <- subs_sel[[fill_col]]
    if (is.numeric(vals)) {
      if (requireNamespace("viridisLite", quietly = TRUE)) {
        pal <- leaflet::colorNumeric(palette = palette, domain = vals, na.color = "#cccccc")
      } else {
        pal <- leaflet::colorNumeric(palette = "YlGnBu", domain = vals, na.color = "#cccccc")
      }
      legend_args <- list(pal = pal, values = vals, title = fill_col, labFormat = leaflet::labelFormat())
    } else {
      pal <- leaflet::colorFactor(palette = "Set2", domain = vals, na.color = "#cccccc")
      legend_args <- list(pal = pal, values = vals, title = fill_col)
    }
  }

  # emprise
  bbox <- sf::st_bbox(sf::st_union(sf::st_geometry(subs_sel), sf::st_geometry(country)))
  lng1 <- bbox["xmin"]; lat1 <- bbox["ymin"]; lng2 <- bbox["xmax"]; lat2 <- bbox["ymax"]

  # carte
  m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
  for (b in unique(basemaps)) m <- leaflet::addProviderTiles(m, b, group = b)

  m <- m |>
    leaflet::addPolygons(
      data = country,
      fillColor = "orange", fillOpacity = 0.2,
      color = outline_color, weight = outline_weight,
      group = "Country"
    ) |>
    leaflet::addPolygons(
      data = subs_sel,
      fillColor = if (is.null(pal)) "#4C78A8" else pal(subs_sel[[fill_col]]),
      fillOpacity = fill_opacity,
      color = outline_color, weight = outline_weight,
      label = mk_label(subs_sel),
      labelOptions = leaflet::labelOptions(direction = "auto", textsize = "12px"),
      popup = mk_popup(subs_sel),
      highlightOptions = leaflet::highlightOptions(weight = 2, color = "#000", fillOpacity = min(1, fill_opacity + 0.15)),
      group = "Sub-basins", smoothFactor = 0.3
    ) |>
    leaflet::addLayersControl(
      baseGroups = unique(basemaps),
      overlayGroups = c("Country", "Sub-basins"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addScaleBar(position = "bottomleft") |>
    leaflet::fitBounds(lng1, lat1, lng2, lat2)

  # mini-map si dispo (extras ou extras2)
  if (have_extras("addMiniMap")) {
    m <- leaflet.extras::addMiniMap(m, tiles = basemaps[1], toggleDisplay = TRUE, minimized = TRUE)
  } else if (have_extras2("add_minimap")) {
    m <- leaflet.extras2::add_minimap(m, providerTiles = basemaps[1], toggleDisplay = TRUE, minimized = TRUE)
  }

  # plein écran si dispo
  if (have_extras("addFullscreenControl")) {
    m <- leaflet.extras::addFullscreenControl(m)
  } else if (have_extras2("add_fullscreen")) {
    m <- leaflet.extras2::add_fullscreen(m)
  }

  # outil de mesure (optionnel) — seulement si explicitement demandé ET fonction dispo
  if (isTRUE(add_measure)) {
    if (have_extras("addMeasure")) {
      m <- leaflet.extras::addMeasure(
        m, position = "topleft",
        primaryLengthUnit = "kilometers",
        primaryAreaUnit   = "hectares"
      )
    } else if (have_extras2("add_measure")) {
      m <- leaflet.extras2::add_measure(
        m, position = "topleft",
        primary_length_unit = "kilometers",
        primary_area_unit   = "hectares"
      )
    } else {
      warning("Aucun outil de mesure disponible (ni leaflet.extras::addMeasure ni leaflet.extras2::add_measure).")
    }
  }

  # légende si palette
  if (!is.null(legend_args)) {
    m <- do.call(leaflet::addLegend, c(list(map = m, position = "bottomright", opacity = 0.8), legend_args))
  }

  m
}


plot_subbasins_map <- function(
    subs_sel,                  # sf des sous-bassins à mettre en avant
    country,                   # sf du pays (polygone(s))
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    country_fill = "orange",
    country_alpha = 0.20,
    country_border = "grey35",
    subs_color = "#1f78b4",
    subs_size = 0.7,
    subs_fill = NA,            # ou une couleur si vous voulez remplir les sous-bassins
    expand_ratio = 0.04,       # marge autour de l’emprise des sous-bassins
    base_size = 12             # taille de police de base
) {
  # --- Dépendances
  stopifnot(requireNamespace("sf", quietly = TRUE))
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  stopifnot(requireNamespace("ggspatial", quietly = TRUE))

  # --- Harmoniser les CRS
  if (sf::st_crs(subs_sel) != sf::st_crs(country)) {
    country <- sf::st_transform(country, sf::st_crs(subs_sel))
  }

  # --- Emprise avec petite marge
  bb  <- sf::st_bbox(subs_sel)
  dx  <- as.numeric(bb$xmax - bb$xmin)
  dy  <- as.numeric(bb$ymax - bb$ymin)
  pad <- c(dx, dy) * expand_ratio

  xlim <- c(bb$xmin - pad[1], bb$xmax + pad[1])
  ylim <- c(bb$ymin - pad[2], bb$ymax + pad[2])

  # --- Thème “pro”
  theme_map <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "grey30"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "grey25"),
      plot.caption = ggplot2::element_text(color = "grey40", size = rel(0.9)),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = grid::unit(c(8, 8, 8, 8), "pt")
    )

  # --- Carte
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = country,
                     ggplot2::aes(geometry = geometry),
                     inherit.aes = FALSE,
                     fill = country_fill, color = country_border,
                     linewidth = 0.3, alpha = country_alpha) +
    ggplot2::geom_sf(data = subs_sel,
                     ggplot2::aes(geometry = geometry),
                     inherit.aes = FALSE,
                     fill = subs_fill, color = subs_color,
                     linewidth = subs_size) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.8) +
    ggspatial::annotation_north_arrow(
      location = "bl",
      which_north = "true",
      pad_y = grid::unit(0.6, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    theme_map

  return(p)
}

