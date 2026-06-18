.arg_match <- function(arg, choices, arg_name = NULL) {
  if (is.null(arg_name)) {
    arg_name <- deparse(substitute(arg))
  }
  tryCatch(
    match.arg(arg, choices),
    error = function(e) {
      rlang::abort(
        sprintf("`%s` must be one of: %s.", arg_name, paste(choices, collapse = ", ")),
        parent = e
      )
    }
  )
}

.check_bool <- function(x, name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    rlang::abort(sprintf("`%s` must be TRUE or FALSE.", name))
  }
  invisible(x)
}

.check_number <- function(
  x,
  name,
  min = -Inf,
  max = Inf,
  allow_null = FALSE,
  integer = FALSE
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }
  if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
    rlang::abort(sprintf("`%s` must be a single numeric value.", name))
  }
  if (integer && x != as.integer(x)) {
    rlang::abort(sprintf("`%s` must be an integer value.", name))
  }
  if (x < min || x > max) {
    rlang::abort(sprintf("`%s` must be between %s and %s.", name, min, max))
  }
  invisible(x)
}

.check_positive_integer <- function(x, name) {
  .check_number(x, name, min = 1, integer = TRUE)
}

.check_color <- function(x, name, allow_transparent = TRUE) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    rlang::abort(sprintf("`%s` must be a single color string.", name))
  }
  if (allow_transparent && identical(tolower(x), "transparent")) {
    return(invisible(x))
  }
  ok <- tryCatch(
    {
      grDevices::col2rgb(x, alpha = TRUE)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!ok) {
    rlang::abort(sprintf("`%s` is not a valid R color.", name))
  }
  invisible(x)
}

.is_url <- function(x) {
  is.character(x) && length(x) == 1 && grepl("^(https?|ftp)://", x, ignore.case = TRUE)
}

.is_file_url <- function(x) {
  is.character(x) && length(x) == 1 && grepl("^file://", x, ignore.case = TRUE)
}

.file_url_to_path <- function(x) {
  path <- sub("^file://", "", x, ignore.case = TRUE)
  path <- utils::URLdecode(path)
  if (.Platform$OS.type == "windows") {
    path <- sub("^/([A-Za-z]:/)", "\\1", path)
  }
  path
}

.with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(force(expr))
  }
  .check_number(seed, "seed", integer = TRUE)

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }

  on.exit(
    {
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  set.seed(seed)
  force(expr)
}

.check_max_dimension <- function(max_dimension, name = "max_dimension") {
  .check_number(max_dimension, name, min = 10, integer = TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
