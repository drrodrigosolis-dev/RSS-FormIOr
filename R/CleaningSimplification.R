#' Normalize column names into a clean, readable format
#'
#' Designed for non-technical users who want consistent column names after
#' downloading FormIO submissions. Works with either a raw data frame or the
#' list returned by [FlattenSubmissions()].
#'
#' @param x A data frame of responses, or a list from [FlattenSubmissions()].
#' @param style One of `"snake"` (default), `"lower"`, `"upper"`, or `"title"`.
#'   Controls casing and separator behavior.
#' @param make_unique Logical. If `TRUE`, make duplicated names unique.
#' @param transliterate Logical. If `TRUE`, convert accents/special characters
#'   to ASCII when possible.
#' @param return_flat Logical. If `TRUE` and `x` came from [FlattenSubmissions()],
#'   include the updated list as `flat` in the output.
#' @param return_flat Logical. If `TRUE` and `x` came from [FlattenSubmissions()],
#'   include the updated list as `flat` in the output.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Data frame with normalized column names}
#'   \item{name_map}{Data frame with old and new column names}
#'   \item{flat}{If `return_flat = TRUE` and `x` was a FlattenSubmissions list, the updated list}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flat <- FlattenSubmissions(GetResponses(form_id = "123", api_key = "abc"))
#' norm <- NormalizeColumnNames(flat)
#' names(norm$data)
#' norm$name_map
#' }
NormalizeColumnNames <- function(
		x,
		style = c("snake", "lower", "upper", "title"),
		make_unique = TRUE,
		transliterate = TRUE,
		return_flat = FALSE,
		quiet = FALSE
) {
	style <- match.arg(style)

	df <- extract_flat_df(x)
	old_names <- names(df)

	new_names <- normalize_names(
		old_names,
		style = style,
		make_unique = make_unique,
		transliterate = transliterate
	)

	names(df) <- new_names

	name_map <- data.frame(
		OldName = old_names,
		NewName = new_names,
		stringsAsFactors = FALSE
	)

	out <- list(
		data = df,
		name_map = name_map
	)

	if (is_flat_list(x) && isTRUE(return_flat)) {
		x$FlatResponses <- df
		x$ColumnNames <- update_column_names(x$ColumnNames, new_names)
		out$flat <- x
	}

	if (!quiet) {
		changed <- sum(old_names != new_names)
		message("Normalized ", changed, " column name(s).")
	}

	out
}


#' Resolve repeated answers into one row per submission
#'
#' Automatically collapses repeated values within each submission ID using a
#' consistent strategy (or simple heuristics when `strategy = "auto"`).
#' This is a non-interactive alternative to [FixDups()].
#'
#' @param x A data frame of responses, or a list from [FlattenSubmissions()].
#' @param id_col Integer or character. Submission ID column (default 1).
#' @param strategy One of `"auto"`, `"concat"`, `"first"`, `"last"`, `"sum"`,
#'   `"mean"`, `"count"`, or `"count_yes"`.
#' @param sep Separator used for concatenation (default `", "`).
#' @param unique Logical. If `TRUE`, remove duplicate values before concatenating.
#' @param return_flat Logical. If `TRUE` and `x` came from [FlattenSubmissions()],
#'   include the updated list as `flat` in the output.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Collapsed data frame with one row per submission}
#'   \item{summary}{Data frame describing how each column was handled}
#'   \item{flat}{If `return_flat = TRUE` and `x` was a FlattenSubmissions list, the updated list}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flat <- FlattenSubmissions(GetResponses(form_id = "123", api_key = "abc"))
#' resolved <- ResolveRepeats(flat, id_col = "submissionId")
#' head(resolved$data)
#' }
ResolveRepeats <- function(
		x,
		id_col = 1,
		strategy = c("auto", "concat", "first", "last", "sum", "mean", "count", "count_yes"),
		sep = ", ",
		unique = TRUE,
		return_flat = FALSE,
		quiet = FALSE
) {
	strategy <- match.arg(strategy)

	df <- extract_flat_df(x)
	id_col_name <- resolve_id_col(df, id_col)

	ids <- df[[id_col_name]]
	unique_ids <- ids[!duplicated(ids)]

	out_df <- data.frame(
		tmp_id = unique_ids,
		stringsAsFactors = FALSE
	)

	summary <- data.frame(
		column = character(),
		max_distinct = integer(),
		applied_strategy = character(),
		stringsAsFactors = FALSE
	)

	for (col_name in names(df)) {
		values <- df[[col_name]]
		split_vals <- split(values, ids, drop = TRUE)

		max_distinct <- max(vapply(split_vals, function(v) length(unique(v[!is.na(v)])), 1L))

		if (col_name == id_col_name) {
			out_df[[col_name]] <- unique_ids
			next
		}

		if (max_distinct <= 1) {
			out_df[[col_name]] <- vapply(
				split_vals,
				pick_first_non_na,
				na_value_for(values)
			)
			applied <- "first"
		} else {
			applied <- if (strategy == "auto") auto_strategy(values) else strategy
			out_df[[col_name]] <- vapply(
				split_vals,
				apply_repeat_strategy,
				strategy_fun_value(applied, values),
				strategy = applied,
				sep = sep,
				unique = unique
			)
		}

		summary <- rbind(
			summary,
			data.frame(
				column = col_name,
				max_distinct = as.integer(max_distinct),
				applied_strategy = applied,
				stringsAsFactors = FALSE
			)
		)
	}

	out_df <- out_df[, names(df), drop = FALSE]

	out <- list(
		data = out_df,
		summary = summary
	)

	if (is_flat_list(x) && isTRUE(return_flat)) {
		x$FlatResponses <- out_df
		x$ColumnNames <- update_column_names(x$ColumnNames, names(out_df))
		out$flat <- x
	}

	if (!quiet) {
		message("Resolved repeats across ", nrow(summary), " column(s).")
	}

	out
}


#' Deduplicate submissions by submission ID
#'
#' Keeps one row per submission ID, using a timestamp column when available
#' (for example `created` or `modified`), otherwise keeps first/last row.
#'
#' @param x A data frame of responses, or a list from [FlattenSubmissions()].
#' @param id_col Integer or character. Submission ID column (default 1).
#' @param time_col Optional column name to use for ordering. If `NULL`, the
#'   function tries common timestamp names automatically.
#' @param keep One of `"last"` (default) or `"first"`.
#' @param return_flat Logical. If `TRUE` and `x` came from [FlattenSubmissions()],
#'   include the updated list as `flat` in the output.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Deduplicated data frame}
#'   \item{summary}{List with counts and the time column used}
#'   \item{flat}{If `return_flat = TRUE` and `x` was a FlattenSubmissions list, the updated list}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flat <- FlattenSubmissions(GetResponses(form_id = "123", api_key = "abc"))
#' dedup <- DeduplicateSubmissions(flat, id_col = "submissionId")
#' nrow(dedup$data)
#' }
DeduplicateSubmissions <- function(
		x,
		id_col = 1,
		time_col = NULL,
		keep = c("last", "first"),
		return_flat = FALSE,
		quiet = FALSE
) {
	keep <- match.arg(keep)

	df <- extract_flat_df(x)
	id_col_name <- resolve_id_col(df, id_col)

	if (is.null(time_col)) {
		time_col <- guess_time_col(names(df))
	}
	if (!is.null(time_col) && !time_col %in% names(df)) {
		time_col <- NULL
	}

	selected_idx <- pick_dedup_rows(
		df = df,
		id_col = id_col_name,
		time_col = time_col,
		keep = keep
	)

	out_df <- df[selected_idx, , drop = FALSE]

	out <- list(
		data = out_df,
		summary = list(
			original_rows = nrow(df),
			kept_rows = nrow(out_df),
			removed_rows = nrow(df) - nrow(out_df),
			time_col = time_col %||% NA
		)
	)

	if (is_flat_list(x) && isTRUE(return_flat)) {
		x$FlatResponses <- out_df
		x$ColumnNames <- update_column_names(x$ColumnNames, names(out_df))
		out$flat <- x
	}

	if (!quiet) {
		message(
			"Kept ", out$summary$kept_rows,
			" of ", out$summary$original_rows, " rows."
		)
	}

	out
}


#' Compact checkbox/multi-select columns into a single readable column
#'
#' When a question generates multiple TRUE/FALSE columns (e.g., select boxes),
#' this function combines them into a single comma-separated column.
#'
#' @param x A data frame of responses, or a list from [FlattenSubmissions()].
#' @param sep Separator used in column names to split prefix and option.
#'   Default `"-"` (matches [FlattenSubmissions()] naming).
#' @param combine_sep Separator used between selected options.
#' @param drop Logical. If `TRUE`, drop the original checkbox columns.
#' @param keep_empty Logical. If `TRUE`, keep empty strings instead of `NA`
#'   when no options are selected.
#' @param yes_values Values that should count as "selected".
#' @param no_values Values that should count as "not selected" (defaults include
#'   `FALSE`, `"No"`, `0`, and blank strings).
#' @param return_flat Logical. If `TRUE` and `x` came from [FlattenSubmissions()],
#'   include the updated list as `flat` in the output.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Data frame with compacted selection columns}
#'   \item{summary}{Data frame describing which columns were compacted}
#'   \item{flat}{If `return_flat = TRUE` and `x` was a FlattenSubmissions list, the updated list}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' compacted <- CompactSelections(flat)
#' head(compacted$data)
#' }
CompactSelections <- function(
		x,
		sep = "-",
		combine_sep = ", ",
		drop = TRUE,
		keep_empty = FALSE,
		yes_values = c(TRUE, "TRUE", "True", "true", "Yes", "YES", "yes", "Y", "y", 1, "1"),
		no_values = c(FALSE, "FALSE", "False", "false", "No", "NO", "no", "N", "n", 0, "0", "", " ",  "NA"),
		return_flat = FALSE,
		quiet = FALSE
) {
	df <- extract_flat_df(x)
	col_names <- names(df)

	group_keys <- derive_prefix(col_names, sep = sep)
	groups <- split(col_names, group_keys, drop = TRUE)
	groups <- groups[!is.na(names(groups))]

	summary <- data.frame(
		prefix = character(),
		new_column = character(),
		original_columns = character(),
		stringsAsFactors = FALSE
	)

	new_df <- df

	for (prefix in names(groups)) {
		group_cols <- groups[[prefix]]
		if (length(group_cols) < 2) next

		is_checkbox_group <- vapply(
			group_cols,
			function(col_nm) {
				is_checkbox_like(
					df[[col_nm]],
					values_df = df,
					yes_values = yes_values,
					no_values = no_values
				)
			},
			logical(1)
		)
		if (!all(is_checkbox_group)) {
			next
		}

		options <- vapply(group_cols, option_suffix, character(1), sep = sep)

		new_col <- paste0(prefix, sep, "selected")
		if (new_col %in% names(new_df)) {
			new_col <- make.unique(c(names(new_df), new_col))[length(names(new_df)) + 1]
		}

		selected <- apply(
			new_df[, group_cols, drop = FALSE],
			1,
			function(row_vals) {
				row_vals <- normalize_checkbox_values(row_vals)
				keep <- row_vals %in% normalize_checkbox_values(yes_values)
				if (!any(keep)) {
					if (keep_empty) "" else NA_character_
				} else {
					paste(options[keep], collapse = combine_sep)
				}
			}
		)

		new_df[[new_col]] <- selected

		summary <- rbind(
			summary,
			data.frame(
				prefix = prefix,
				new_column = new_col,
				original_columns = paste(group_cols, collapse = ", "),
				stringsAsFactors = FALSE
			)
		)

		if (drop) {
			new_df <- new_df[, setdiff(names(new_df), group_cols), drop = FALSE]
		}
	}

	out <- list(
		data = new_df,
		summary = summary
	)

	if (is_flat_list(x) && isTRUE(return_flat)) {
		x$FlatResponses <- new_df
		x$ColumnNames <- update_column_names(x$ColumnNames, names(new_df))
		out$flat <- x
	}

	if (!quiet) {
		message("Compacted ", nrow(summary), " selection group(s).")
	}

	out
}


# ---- Internal helpers -----------------------------------------------------------

is_flat_list <- function(x) {
	is.list(x) && "FlatResponses" %in% names(x)
}

extract_flat_df <- function(x) {
	if (is_flat_list(x)) return(x$FlatResponses)
	if (!is.data.frame(x)) stop("x must be a data.frame or output from FlattenSubmissions()")
	x
}

resolve_id_col <- function(df, id_col) {
	if (is.numeric(id_col)) {
		id_col_name <- names(df)[id_col]
	} else {
		id_col_name <- as.character(id_col)
	}
	if (is.na(id_col_name) || !id_col_name %in% names(df)) {
		stop("id_col '", id_col, "' not found in data")
	}
	id_col_name
}

normalize_names <- function(names_vec, style, make_unique, transliterate) {
	out <- names_vec
	out <- trimws(out)
	if (transliterate) {
		out <- iconv(out, from = "", to = "ASCII//TRANSLIT")
	}
	out <- gsub("[^A-Za-z0-9]+", "_", out)
	out <- gsub("_+", "_", out)
	out <- gsub("^_+|_+$", "", out)

	if (style == "snake" || style == "lower") {
		out <- tolower(out)
	}
	if (style == "upper") {
		out <- toupper(out)
	}
	if (style == "title") {
		out <- tools::toTitleCase(tolower(gsub("_", " ", out)))
		out <- gsub(" ", "_", out)
	}

	out[out == ""] <- "col"
	if (make_unique) out <- make.unique(out, sep = "_")
	out
}

update_column_names <- function(column_names_df, new_names) {
	if (is.null(column_names_df)) return(column_names_df)
	if (!is.data.frame(column_names_df)) return(column_names_df)
	if ("Name" %in% names(column_names_df)) {
		column_names_df$Name <- new_names
	}
	if ("Names" %in% names(column_names_df)) {
		column_names_df$Names <- new_names
	}
	column_names_df
}

pick_first_non_na <- function(x) {
	x <- x[!is.na(x)]
	if (length(x) == 0) NA else x[1]
}

auto_strategy <- function(values) {
	if (is_numeric_like(values)) return("sum")
	if (is_checkbox_like(values)) return("count_yes")
	"concat"
}

apply_repeat_strategy <- function(values, strategy, sep, unique) {
	values <- values[!is.na(values)]
	if (unique && strategy %in% c("concat", "first", "last")) {
		values <- unique(values)
	}

	if (strategy == "concat") {
		return(if (length(values) == 0) NA_character_ else paste(values, collapse = sep))
	}
	if (strategy == "first") {
		return(if (length(values) == 0) NA else values[1])
	}
	if (strategy == "last") {
		return(if (length(values) == 0) NA else values[length(values)])
	}
	if (strategy == "sum") {
		return(sum(suppressWarnings(as.numeric(values)), na.rm = TRUE))
	}
	if (strategy == "mean") {
		return(mean(suppressWarnings(as.numeric(values)), na.rm = TRUE))
	}
	if (strategy == "count") {
		return(length(values))
	}
	if (strategy == "count_yes") {
		values <- normalize_checkbox_values(values)
		return(sum(values %in% normalize_checkbox_values(default_yes_values()), na.rm = TRUE))
	}

	if (length(values) == 0) NA else values[1]
}

is_numeric_like <- function(values) {
	if (all(is.na(values))) return(FALSE)
	num <- suppressWarnings(as.numeric(values))
	any(!is.na(num)) && sum(is.na(num)) <= sum(is.na(values))
}

default_yes_values <- function() {
	c(TRUE, "TRUE", "True", "true", "Yes", "YES", "yes", "Y", "y", 1, "1")
}

is_checkbox_like <- function(values, values_df = NULL, yes_values = default_yes_values(), no_values = NULL) {
	if (is.logical(values)) return(TRUE)
	if (is.numeric(values)) {
		vals <- values[!is.na(values)]
		if (length(vals) == 0) return(TRUE)
		return(all(vals %in% c(0, 1)))
	}
	if (is.null(no_values)) {
		no_values <- c(FALSE, "FALSE", "False", "false", "No", "NO", "no", "N", "n", 0, "0", "", " ")
	}

	values <- normalize_checkbox_values(values)
	values <- values[!is.na(values)]
	if (length(values) == 0) return(TRUE)

	allowed <- c(normalize_checkbox_values(yes_values), normalize_checkbox_values(no_values))
	all(values %in% allowed)
}

normalize_checkbox_values <- function(values) {
	if (is.factor(values)) values <- as.character(values)
	values <- trimws(as.character(values))
	values[values == "NA"] <- NA_character_
	values
}

derive_prefix <- function(names_vec, sep) {
	pattern <- paste0(sep, "[^", sep, "]+$")
	prefixes <- sub(pattern, "", names_vec)
	prefixes[prefixes == names_vec] <- NA_character_
	prefixes
}

option_suffix <- function(name, sep) {
	pattern <- paste0("^.*", sep)
	sub(pattern, "", name)
}

guess_time_col <- function(names_vec) {
	candidates <- c(
		"modified", "created", "updated", "submitted",
		"submissionDate", "submittedDate", "createdDate", "modifiedDate",
		"_createdAt", "_updatedAt", "lastModified", "timestamp"
	)
	candidates[candidates %in% names_vec][1] %||% NULL
}

pick_dedup_rows <- function(df, id_col, time_col, keep) {
	ids <- df[[id_col]]
	unique_ids <- ids[!duplicated(ids)]

	if (!is.null(time_col)) {
		times <- suppressWarnings(as.POSIXct(df[[time_col]], tz = "UTC"))
		if (all(is.na(times))) {
			time_col <- NULL
		}
	}

	selected <- integer(0)
	for (id in unique_ids) {
		idx <- which(ids == id)
		if (length(idx) == 1) {
			selected <- c(selected, idx)
			next
		}

		if (!is.null(time_col)) {
			times <- suppressWarnings(as.POSIXct(df[[time_col]][idx], tz = "UTC"))
			if (keep == "last") {
				best <- idx[which.max(times)]
			} else {
				best <- idx[which.min(times)]
			}
		} else {
			best <- if (keep == "last") idx[length(idx)] else idx[1]
		}

		selected <- c(selected, best)
	}

	selected
}

na_value_for <- function(values) {
	if (is.numeric(values)) return(NA_real_)
	if (is.logical(values)) return(NA)
	NA_character_
}

strategy_fun_value <- function(strategy, values) {
	if (strategy %in% c("sum", "mean", "count", "count_yes")) return(NA_real_)
	if (strategy == "concat") return(NA_character_)
	na_value_for(values)
}
