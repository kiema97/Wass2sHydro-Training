plot_entropy_map_facet <- function(
    df_entropy,
    sf_bassins,
    id_col = "HYBAS_ID",
    entropy_col = "entropy",
    year_col = "YYYY",
    # Period selection (same format as YYYY, e.g., 19930101)
    period_start = NULL,
    period_end   = NULL,
    # Facet grouping:
    # "YYYY" uses the raw YYYY values; "year" extracts the year (1993, 1994, ...)
    facet_by = c("year", "YYYY"),
    # Optional: aggregate within each facet group if duplicates exist
    # (e.g., multiple runs per basin-year)
    agg = c("none", "mean", "median", "min", "max"),
    # Plot options
    palette = "viridis",
    na_fill = "grey85",
    border_color = NA,
    border_size = 0.1,
    ncol = NULL,
    title = "Entropy maps",
    subtitle = NULL,
    caption = NULL
) {
  # ---- Dependencies ----
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.", call. = FALSE)
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Package 'rlang' is required.", call. = FALSE)
  if (!requireNamespace("viridis", quietly = TRUE)) stop("Package 'viridis' is required.", call. = FALSE)

  facet_by <- match.arg(facet_by)
  agg <- match.arg(agg)

  # ---- Validation ----
  if (!is.data.frame(df_entropy)) stop("`df_entropy` must be a data.frame/tibble.", call. = FALSE)
  if (!inherits(sf_bassins, "sf")) stop("`sf_bassins` must be an sf object.", call. = FALSE)

  miss_df <- setdiff(c(id_col, entropy_col, year_col), names(df_entropy))
  if (length(miss_df)) stop("Missing columns in df_entropy: ", paste(miss_df, collapse = ", "), call. = FALSE)
  if (!id_col %in% names(sf_bassins)) stop("`id_col` not found in sf_bassins.", call. = FALSE)

  id_sym <- rlang::sym(id_col)
  ent_sym <- rlang::sym(entropy_col)
  yr_sym <- rlang::sym(year_col)

  # ---- Filter by period ----
  x <- df_entropy %>%
    dplyr::mutate(
      .yyy = as.numeric(!!yr_sym),
      .entropy_num = as.numeric(!!ent_sym)
    )

  if (!is.null(period_start)) x <- dplyr::filter(x, .yyy >= as.numeric(period_start))
  if (!is.null(period_end))   x <- dplyr::filter(x, .yyy <= as.numeric(period_end))

  if (nrow(x) == 0) stop("No rows left after period filtering.", call. = FALSE)

  # ---- Build facet variable ----
  if (facet_by == "year") {
    # Extract year from YYYY-like integer (19930101 -> 1993)
    x <- x %>% dplyr::mutate(.facet = floor(.yyy / 10000))
  } else {
    x <- x %>% dplyr::mutate(.facet = .yyy)
  }

  # ---- Optional aggregation per basin within facet ----
  # Ensures 1 entropy value per basin per facet (required for a clean join)
  if (agg != "none") {
    fun <- switch(
      agg,
      mean   = function(x) mean(x, na.rm = TRUE),
      median = function(x) stats::median(x, na.rm = TRUE),
      min    = function(x) min(x, na.rm = TRUE),
      max    = function(x) max(x, na.rm = TRUE)
    )

    x <- x %>%
      dplyr::group_by(!!id_sym, .facet) %>%
      dplyr::summarise(entropy_agg = fun(.entropy_num), .groups = "drop")
  } else {
    # keep as-is but rename consistently
    x <- x %>%
      dplyr::transmute(!!id_sym, .facet, entropy_agg = .entropy_num)
  }

  # ---- Expand sf across facets and join ----
  facets_tbl <- dplyr::distinct(x, .facet)

  sf_long <- sf_bassins %>%
    dplyr::mutate(.tmp_join = 1) %>%
    dplyr::left_join(dplyr::mutate(facets_tbl, .tmp_join = 1), by = ".tmp_join") %>%
    dplyr::select(-.tmp_join)

  sf_long <- sf_long %>%
    dplyr::left_join(x, by = c(setNames(id_col, id_col), ".facet" = ".facet"))

  # ---- Subtitle auto ----
  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Period: ",
      ifelse(is.null(period_start), "-Inf", period_start),
      " to ",
      ifelse(is.null(period_end), "Inf", period_end),
      " | Facet by: ", facet_by,
      if (agg != "none") paste0(" | Aggregation: ", agg) else ""
    )
  }

  # ---- Plot ----
  p <- ggplot2::ggplot(sf_long) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = entropy_agg),
      color = border_color,
      linewidth = border_size
    ) +
    scale_fill_gradient(low = RColorBrewer::brewer.pal(9,name="Reds")[2],
                        high =RColorBrewer::brewer.pal(9,name="Reds")[5],
                        name="Entropy"  )+
    ggplot2::facet_wrap(~.facet, ncol = ncol) +
    # ggplot2::labs(
    #   title = title,
    #   subtitle = subtitle,
    #   caption = caption
    # ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      panel.grid.major = ggplot2::element_line(linewidth = 0.1),
      strip.text = ggplot2::element_text(face = "bold")
    )

  list(sf_map = sf_long, plot = p, data = x)
}



plot_basin_metric_map <- function(
    df,
    sf_bassins,
    id_col = "HYBAS_ID",
    value_col,
    # Optional grouping column for faceting (NOT necessarily time)
    group_col = NULL,
    facet = FALSE,
    ncol = NULL,
    # Optional aggregation if multiple rows per basin (and per group)
    agg = c("none", "mean", "median", "min", "max"),
    # Fill scale
    fill_scale = c("gradient", "viridis"),
    legend_title = NULL,
    na_fill = "grey85",
    border_color = NA,
    border_size = 0.1,
    # gradient options
    gradient_palette = c("Reds", 2, 5),
    # viridis options
    viridis_option = "viridis",
    # labels
    title = NULL,
    subtitle = NULL,
    caption = NULL
) {
  # ---- deps ----
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.", call. = FALSE)
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Package 'rlang' is required.", call. = FALSE)

  agg <- match.arg(agg)
  fill_scale <- match.arg(fill_scale)

  # ---- validation ----
  if (!is.data.frame(df)) stop("`df` must be a data.frame/tibble.", call. = FALSE)
  if (!inherits(sf_bassins, "sf")) stop("`sf_bassins` must be an sf object.", call. = FALSE)
  if (!id_col %in% names(df)) stop("`id_col` not found in df.", call. = FALSE)
  if (!id_col %in% names(sf_bassins)) stop("`id_col` not found in sf_bassins.", call. = FALSE)
  if (missing(value_col) || !is.character(value_col) || length(value_col) != 1L) {
    stop("`value_col` must be a single column name (character).", call. = FALSE)
  }
  if (!value_col %in% names(df)) stop("`value_col` not found in df.", call. = FALSE)

  if (!is.null(group_col)) {
    if (!is.character(group_col) || length(group_col) != 1L) stop("`group_col` must be NULL or a single column name.", call. = FALSE)
    if (!group_col %in% names(df)) stop("`group_col` not found in df.", call. = FALSE)
  }

  if (isTRUE(facet) && is.null(group_col)) {
    stop("If `facet = TRUE`, you must provide `group_col`.", call. = FALSE)
  }

  if (is.null(legend_title)) legend_title <- value_col

  id_sym <- rlang::sym(id_col)
  val_sym <- rlang::sym(value_col)
  grp_sym <- if (!is.null(group_col)) rlang::sym(group_col) else NULL

  # ---- prepare ----
  x <- df %>%
    dplyr::mutate(
      .id = as.character(!!id_sym),
      .value = as.numeric(!!val_sym),
      .facet = if (is.null(group_col)) "all" else as.character(!!grp_sym)
    )

  # ---- aggregate if needed ----
  if (agg != "none") {
    fun <- switch(
      agg,
      mean   = function(v) mean(v, na.rm = TRUE),
      median = function(v) stats::median(v, na.rm = TRUE),
      min    = function(v) min(v, na.rm = TRUE),
      max    = function(v) max(v, na.rm = TRUE)
    )

    x <- x %>%
      dplyr::group_by(.id, .facet) %>%
      dplyr::summarise(value_agg = fun(.value), .groups = "drop")
  } else {
    x <- x %>%
      dplyr::transmute(.id, .facet, value_agg = .value)
  }

  # ---- expand sf across facets (only if needed) ----
  facets_tbl <- dplyr::distinct(x, .facet)

  sf_long <- sf_bassins %>%
    dplyr::mutate(.tmp_join = 1) %>%
    dplyr::left_join(dplyr::mutate(facets_tbl, .tmp_join = 1), by = ".tmp_join") %>%
    dplyr::select(-.tmp_join)

  # join using basin id
  sf_long <- sf_long %>%
    dplyr::mutate(.id = as.character(.data[[id_col]])) %>%
    dplyr::left_join(x, by = c(".id" = ".id", ".facet" = ".facet"))

  # ---- auto subtitle ----
  if (is.null(subtitle)) {
    subtitle <- paste0(
      if (is.null(group_col)) "No grouping" else paste0("Group: ", group_col),
      if (agg != "none") paste0(" | Aggregation: ", agg) else ""
    )
  }

  # ---- plot ----
  p <- ggplot2::ggplot(sf_long) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = value_agg),
      color = border_color,
      linewidth = border_size
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      panel.grid.major = ggplot2::element_line(linewidth = 0.1),
      strip.text = ggplot2::element_text(face = "bold")
    )

  if (fill_scale == "viridis") {
    if (!requireNamespace("viridis", quietly = TRUE)) stop("Package 'viridis' is required for fill_scale='viridis'.", call. = FALSE)
    p <- p + viridis::scale_fill_viridis(option = viridis_option, na.value = na_fill, name = legend_title)
  } else {
    if (!requireNamespace("RColorBrewer", quietly = TRUE)) stop("Package 'RColorBrewer' is required for fill_scale='gradient'.", call. = FALSE)
    brewer_name <- gradient_palette[[1]]
    low_idx <- gradient_palette[[2]]
    high_idx <- gradient_palette[[3]]
    pal <- RColorBrewer::brewer.pal(9, name = brewer_name)
    p <- p + ggplot2::scale_fill_gradient(
      low = pal[low_idx],
      high = pal[high_idx],
      na.value = na_fill,
      name = legend_title
    )
  }

  if (!is.null(title) || !is.null(subtitle) || !is.null(caption)) {
    p <- p + ggplot2::labs(title = title, subtitle = subtitle, caption = caption)
  }

  if (isTRUE(facet) && !is.null(group_col)) {
    p <- p + ggplot2::facet_wrap(~.facet, ncol = ncol)
  }

  list(sf_map = sf_long, plot = p, data = x)
}
