reduce_data_by_products_eof <- function(
    data_by_products,
    sst_regex = "^sst_",
    num_comp = 15,
    corr_threshold = 0.99,
    corr_method = "pearson",
    remove_linear_comb = FALSE,
    # Naming policy: the reduced EOF columns MUST match the same SST regex
    # Example: "sst_eof_001", ..., "sst_eof_015" -> still matches "^sst_"
    eof_prefix = "sst_eof_",
    eof_digits = 3,
    verbose = TRUE
) {
  # ---- Dependencies ----
  stopifnot(is.list(data_by_products))
  if (!requireNamespace("recipes", quietly = TRUE)) stop("Package 'recipes' is required.", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.", call. = FALSE)
  if (!requireNamespace("purrr", quietly = TRUE)) stop("Package 'purrr' is required.", call. = FALSE)
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.", call. = FALSE)

  # ---- Validations ----
  if (!is.numeric(num_comp) || length(num_comp) != 1L || num_comp < 1) {
    stop("`num_comp` must be a single positive integer.", call. = FALSE)
  }
  if (!is.numeric(corr_threshold) || corr_threshold <= 0 || corr_threshold >= 1) {
    stop("`corr_threshold` must be in (0,1), e.g. 0.99.", call. = FALSE)
  }
  if (!is.character(eof_prefix) || !nzchar(eof_prefix)) {
    stop("`eof_prefix` must be a non-empty character string.", call. = FALSE)
  }
  # Ensure the new EOF names still match the SST regex (critical requirement)
  test_name <- paste0(eof_prefix, sprintf(paste0("%0", eof_digits, "d"), 1))
  if (!grepl(sst_regex, test_name)) {
    stop(
      sprintf(
        "The generated EOF column name '%s' does NOT match sst_regex '%s'. Please adjust `eof_prefix` or `sst_regex`.",
        test_name, sst_regex
      ),
      call. = FALSE
    )
  }

  basin_ids <- names(data_by_products)
  if (is.null(basin_ids) || !length(basin_ids)) {
    stop("`data_by_products` must be a named list (names = HYBAS_ID).", call. = FALSE)
  }

  # ---- Helper: identify SST predictor columns ----
  .get_sst_cols <- function(df) grep(sst_regex, names(df), value = TRUE)

  # ---- Collect all climate models across basins ----
  all_models <- sort(unique(unlist(purrr::map(data_by_products, names))))
  if (!length(all_models)) stop("No climate models found inside `data_by_products`.", call. = FALSE)

  # ---- Fit one EOF/PCA recipe per climate model using ALL basins (stacked) ----
  eof_recipes <- purrr::map(all_models, function(mdl) {
    dfs <- purrr::map(basin_ids, function(bid) {
      df <- data_by_products[[bid]][[mdl]]
      if (is.null(df) || !is.data.frame(df)) return(NULL)

      # Minimal required columns for alignment and to keep your structure consistent
      needed <- c("HYBAS_ID", "YYYY", "Q")
      miss <- setdiff(needed, names(df))
      if (length(miss)) {
        stop(
          sprintf("Model '%s' basin '%s': missing required columns: %s",
                  mdl, bid, paste(miss, collapse = ", ")),
          call. = FALSE
        )
      }

      sst_cols <- .get_sst_cols(df)
      if (!length(sst_cols)) return(NULL)

      # Keep only modeled columns + minimal ids during training
      df[, c("HYBAS_ID", "YYYY", "Q", sst_cols), drop = FALSE]
    })

    dfs <- purrr::compact(dfs)
    if (!length(dfs)) {
      if (isTRUE(verbose)) message("[EOF] Model '", mdl, "': no usable data found. Skipped.")
      return(NULL)
    }

    stacked <- dplyr::bind_rows(dfs)
    sst_cols <- .get_sst_cols(stacked)
    if (!length(sst_cols)) {
      if (isTRUE(verbose)) message("[EOF] Model '", mdl, "': no SST columns matched regex. Skipped.")
      return(NULL)
    }

    # Recipe: zv -> nzv -> impute -> (optional lincomb) -> corr -> normalize -> PCA
    rec <- recipes::recipe(
      stats::as.formula(paste("Q ~", paste(sst_cols, collapse = " + "))),
      data = stacked
    ) |>
      recipes::step_zv(recipes::all_predictors(), id = "zv") |>
      recipes::step_nzv(recipes::all_predictors(), id = "nzv") |>
      recipes::step_impute_median(recipes::all_numeric_predictors(), id = "imp_num")

    if (isTRUE(remove_linear_comb)) {
      rec <- rec |> recipes::step_lincomb(recipes::all_numeric_predictors(), id = "lincomb")
    }

    rec <- rec |>
      recipes::step_corr(
        recipes::all_numeric_predictors(),
        threshold = corr_threshold,
        method = corr_method,
        id = "corr"
      ) |>
      recipes::step_normalize(recipes::all_numeric_predictors(), id = "norm") |>
      recipes::step_pca(recipes::all_numeric_predictors(), num_comp = as.integer(num_comp), id = "pca")

    rec_prep <- recipes::prep(rec, training = stacked, verbose = FALSE)

    baked_train <- recipes::bake(rec_prep, new_data = stacked)
    pc_cols <- grep("^PC", names(baked_train), value = TRUE)

    if (isTRUE(verbose)) {
      message(
        "[EOF] Model '", mdl, "': stacked rows = ", nrow(stacked),
        " | SST cols (raw) = ", length(sst_cols),
        " | PCs created = ", length(pc_cols)
      )
    }

    list(model = mdl, recipe = rec_prep, pc_cols = pc_cols)
  })

  names(eof_recipes) <- all_models
  eof_recipes <- purrr::compact(eof_recipes)
  if (!length(eof_recipes)) stop("No EOF/PCA recipes could be fitted. Check your data and `sst_regex`.", call. = FALSE)

  # ---- Apply EOF transform to every basin/model dataframe ----
  data_reduced <- purrr::imap(data_by_products, function(basin_list, bid) {
    if (!is.list(basin_list)) return(basin_list)

    purrr::imap(basin_list, function(df, mdl) {
      if (is.null(df) || !is.data.frame(df)) return(df)
      if (!mdl %in% names(eof_recipes)) return(df)

      # Identify SST columns in the original df
      sst_cols <- .get_sst_cols(df)
      if (!length(sst_cols)) return(df) # nothing to reduce

      # Bake PCs (recipes may drop non-modeled columns; we will re-attach metadata)
      rec_prep <- eof_recipes[[mdl]]$recipe
      baked <- recipes::bake(rec_prep, new_data = df)

      pc_cols <- eof_recipes[[mdl]]$pc_cols
      pc_cols <- intersect(pc_cols, names(baked))
      if (!length(pc_cols)) {
        stop(sprintf("Basin '%s' model '%s': no PC columns produced by bake().", bid, mdl), call. = FALSE)
      }

      # Build EOF column names that STILL match the SST regex
      eof_names <- paste0(eof_prefix, sprintf(paste0("%0", eof_digits, "d"), seq_along(pc_cols)))

      pcs <- baked[, pc_cols, drop = FALSE]
      names(pcs) <- eof_names

      # Preserve all non-SST columns from original df (including HYBAS_ID, YYYY, Q, etc.)
      non_sst_df <- df[, setdiff(names(df), sst_cols), drop = FALSE]

      # Final: original non-SST columns + reduced SST (EOF) columns (named as sst_*)
      out <- dplyr::bind_cols(non_sst_df, pcs)
      tibble::as_tibble(out)
    })
  })

  # ---- Return reduced data + fitted EOF objects for reproducibility ----
  list(
    data_reduced = data_reduced,
    eof_objects  = eof_recipes,
    meta = list(
      sst_regex = sst_regex,
      num_comp = as.integer(num_comp),
      corr_threshold = corr_threshold,
      corr_method = corr_method,
      remove_linear_comb = isTRUE(remove_linear_comb),
      eof_prefix = eof_prefix,
      eof_digits = eof_digits,
      fitted_models = names(eof_recipes)
    )
  )
}



safe_readRDS <- function(path) {
  if (is.null(path)) return(NULL)
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  readRDS(path)
}

validate_and_load_predictors <- function(
    PRCP_PATH_INPUTS = NULL,
    SST_PATH_INPUTS  = NULL,
    prcp_regex = "prcp",
    sst_regex  = "^sst_",
    # how many basins/models to inspect (fast sanity check)
    n_check_basins = 1,
    n_check_models = 1,
    verbose = TRUE
) {
  # ---- helper: basic path checks ----
  .check_path <- function(path, label) {
    if (is.null(path)) return(invisible(TRUE))
    if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
      stop(label, " must be NULL or a non-empty character scalar.", call. = FALSE)
    }
    if (!file.exists(path)) {
      stop(label, " file not found: ", path, call. = FALSE)
    }
    invisible(TRUE)
  }

  .check_path(PRCP_PATH_INPUTS, "PRCP_PATH_INPUTS")
  .check_path(SST_PATH_INPUTS,  "SST_PATH_INPUTS")

  if (is.null(PRCP_PATH_INPUTS) && is.null(SST_PATH_INPUTS)) {
    stop("You must provide at least one of PRCP_PATH_INPUTS or SST_PATH_INPUTS.", call. = FALSE)
  }

  # ---- helper: sample a df inside the nested list ----
  .sample_df <- function(x, n_basin = 1, n_model = 1) {
    if (is.null(x)) return(NULL)
    if (!is.list(x) || is.null(names(x)) || !length(x)) return(NULL)

    basin_ids <- names(x)[seq_len(min(n_basin, length(x)))]
    for (bid in basin_ids) {
      basin_list <- x[[bid]]
      if (!is.list(basin_list) || !length(basin_list)) next

      model_ids <- names(basin_list)[seq_len(min(n_model, length(basin_list)))]
      for (mid in model_ids) {
        df <- basin_list[[mid]]
        if (is.data.frame(df) && nrow(df) > 0) return(df)
      }
    }
    NULL
  }

  # ---- helper: detect variable type by column names ----
  .detect_type <- function(df, prcp_regex, sst_regex) {
    if (is.null(df) || !is.data.frame(df)) return(list(is_prcp = FALSE, is_sst = FALSE))

    cols <- names(df)
    is_prcp <- any(grepl(prcp_regex, cols, ignore.case = TRUE))
    is_sst  <- any(grepl(sst_regex, cols, ignore.case = FALSE))
    list(is_prcp = is_prcp, is_sst = is_sst)
  }

  # ---- read files ----
  prcp_data_by_products <- if (!is.null(PRCP_PATH_INPUTS)) safe_readRDS(PRCP_PATH_INPUTS)
  sst_data_by_products  <- if (!is.null(SST_PATH_INPUTS))  safe_readRDS(SST_PATH_INPUTS)

  # ---- validate structure quickly ----
  .validate_list_structure <- function(x, label) {
    if (is.null(x)) return(invisible(TRUE))
    if (!is.list(x) || is.null(names(x)) || !length(x)) {
      stop(label, " must be a named list of basins (HYBAS_ID).", call. = FALSE)
    }
    invisible(TRUE)
  }
  .validate_list_structure(prcp_data_by_products, "PRCP data")
  .validate_list_structure(sst_data_by_products,  "SST data")

  # ---- content validation (guard against swapped paths) ----
  if (!is.null(prcp_data_by_products)) {
    df_chk <- .sample_df(prcp_data_by_products, n_check_basins, n_check_models)
    flags  <- .detect_type(df_chk, prcp_regex, sst_regex)

    if (!flags$is_prcp && flags$is_sst) {
      stop(
        "PRCP_PATH_INPUTS seems to point to SST data (found SST columns, but no PRCP columns). ",
        "Please provide the correct PRCP file.",
        call. = FALSE
      )
    }
    if (!flags$is_prcp) {
      stop(
        "PRCP_PATH_INPUTS does not look like PRCP data: no columns matched regex '", prcp_regex, "'.",
        call. = FALSE
      )
    }
    if (isTRUE(verbose)) message("[OK] PRCP file validated (PRCP columns detected).")
  }

  if (!is.null(sst_data_by_products)) {
    df_chk <- .sample_df(sst_data_by_products, n_check_basins, n_check_models)
    flags  <- .detect_type(df_chk, prcp_regex, sst_regex)

    if (!flags$is_sst && flags$is_prcp) {
      stop(
        "SST_PATH_INPUTS seems to point to PRCP data (found PRCP columns, but no SST columns). ",
        "Please provide the correct SST file.",
        call. = FALSE
      )
    }
    if (!flags$is_sst) {
      stop(
        "SST_PATH_INPUTS does not look like SST data: no columns matched regex '", sst_regex, "'.",
        call. = FALSE
      )
    }
    if (isTRUE(verbose)) message("[OK] SST file validated (SST columns detected).")
  }

  # ---- default preprocessing flags ----
  auto_pca <- TRUE
  apply_corr <- TRUE
  apply_normalize <- TRUE
  apply_impute <- TRUE

  # ---- EOF for SST if present ----
  eof_objects <- NULL
  if (!is.null(sst_data_by_products)) {
    res_eof <- reduce_data_by_products_eof(
      sst_data_by_products,
      sst_regex = sst_regex,
      num_comp = 15,
      corr_threshold = 0.99,
      corr_method = "pearson",
      remove_linear_comb = FALSE,
      eof_prefix = "sst_eof_",
      verbose = TRUE
    )

    sst_data_by_products <- res_eof$data_reduced
    eof_objects <- res_eof$eof_objects

    # If EOF already applied upstream, disable redundant steps downstream
    auto_pca <- FALSE
    apply_corr <- FALSE
    apply_normalize <- FALSE
    apply_impute <- FALSE
  }

  list(
    prcp_data_by_products = prcp_data_by_products,
    sst_data_by_products  = sst_data_by_products,
    eof_objects = eof_objects,
    flags = list(
      auto_pca = auto_pca,
      apply_corr = apply_corr,
      apply_normalize = apply_normalize,
      apply_impute = apply_impute
    )
  )
}
