#' Normalize column names into a clean, readable format
#'
#' Designed for non-technical users who want consistent column names after
#' downloading FormIO submissions. Works with either a raw data frame or the
#' list returned by [FlattenSubmissions()].
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [FlattenSubmissions()].
#' @param style One of `"snake"` (default), `"lower"`, `"upper"`, or `"title"`.
#'   Controls casing and separator behavior.
#' @param make_unique Logical. If `TRUE`, make duplicated names unique.
#' @param transliterate Logical. If `TRUE`, convert accents/special characters
#'   to ASCII when possible.
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
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

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

	if (audit_depth == 1) {
		maybe_write_audit("NormalizeColumnNames", data = out$data)
	}

	out
}


#' Resolve repeated answers into one row per submission
#'
#' Automatically collapses repeated values within each submission ID using a
#' consistent strategy (or simple heuristics when `strategy = "auto"`).
#' This is a non-interactive alternative to [FixDups()].
#'
#' Note: some FormIO components (for example uploads or address blocks) can
#' produce list-columns or nested data-frame columns even after flattening.
#' These values are converted to readable JSON/text before collapsing so the
#' output remains stable and export-friendly.
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
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
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

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
		values <- coerce_repeat_values(values)
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

	if (audit_depth == 1) {
		maybe_write_audit("ResolveRepeats", data = out$data)
	}

	out
}


#' Deduplicate submissions by submission ID
#'
#' Keeps one row per submission ID, using a timestamp column when available
#' (for example `created` or `modified`), otherwise keeps first/last row.
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
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
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

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

	if (audit_depth == 1) {
		maybe_write_audit("DeduplicateSubmissions", data = out$data)
	}

	out
}


#' Interactively review and resolve duplicate submissions
#'
#' This function walks you through duplicate groups defined by key columns
#' (for example `email`, `username`, `project_id`). For each duplicate group,
#' it shows selected columns so you can compare submissions and choose which
#' ones to keep. You can keep multiple submissions or drop all submissions
#' for a given group if needed.
#'
#' This is designed for non-technical users who want full control over which
#' duplicates are kept. If you want an automatic (non-interactive) approach,
#' use [DeduplicateSubmissions()].
#'
#' If audit logging is active (see [StartAuditLog()]), each decision is logged.
#'
#' @param x A data frame of responses, or a list from [FlattenSubmissions()].
#' @param id_col Integer or character. Submission ID column (default 1).
#' @param key_cols Optional. Columns used to define "duplicate submissions"
#'   (for example `username`, `email`, `project_id`). If `NULL`, you will be
#'   prompted to choose columns interactively (unless `prompt = FALSE`).
#'   The default suggestions are the first three columns that do not start
#'   with `"form-"` or `"form_"`.
#'   Tip: choose columns that are stable within a submission (not multi-row fields).
#' @param compare_cols Optional. Character or integer vector of columns to show
#'   when comparing duplicates. If `NULL` (default), you will be prompted to
#'   choose columns interactively (unless `prompt = FALSE`).
#' @param keep_map Optional. Non-interactive decisions for which rows to keep,
#'   keyed by submission ID. Supported formats:
#'   - Named list: `list(id1 = c(1,3), id2 = "all")`
#'   - Named character vector: `c(id1 = "1,3", id2 = "all")`
#'   - Data frame with columns `id` and `keep`
#'   Values can be row numbers (within each duplicate group), ranges like `"1:3"`,
#'   or keywords `"all"` / `"none"`.
#' @param prompt Logical. If `TRUE` (default), ask interactively. If `FALSE`,
#'   uses `keep_map` and `default_keep` without prompting.
#' @param default_keep Default action when `prompt = FALSE` and no `keep_map`
#'   entry exists for a given ID. Accepts the same formats as `keep_map` values.
#'   Default `"all"`.
#' @param return_flat Logical. If `TRUE` and `x` came from [FlattenSubmissions()],
#'   include the updated list as `flat` in the output.
#' @param quiet Logical. If `FALSE`, prints guidance and summaries.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Data frame after duplicate review}
#'   \item{summary}{List with counts and whether the review stopped early}
#'   \item{decisions}{Data frame describing what was kept/dropped for each ID}
#'   \item{flat}{If `return_flat = TRUE` and `x` was a FlattenSubmissions list, the updated list}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flat <- FlattenSubmissions(GetResponses(form_id = "123", api_key = "abc"))
#' out <- ReviewDuplicateSubmissions(flat, id_col = "form_submissionid")
#' View(out$decisions)
#'
#' # Non-interactive example (scriptable)
#' df <- data.frame(
#'   submissionId = c("a", "a", "b", "b", "b"),
#'   email = c("x@example.com", "x@example.com", "x@example.com", "x@example.com", "x@example.com"),
#'   status = c("draft", "final", "test", "final", "final"),
#'   stringsAsFactors = FALSE
#' )
#' # Group 1 will be the email=x@example.com group (the only group in this example)
#' keep_map <- list(`1` = 2)
#' out2 <- ReviewDuplicateSubmissions(
#'   df,
#'   id_col = "submissionId",
#'   key_cols = "email",
#'   compare_cols = c("submissionId", "email", "status"),
#'   keep_map = keep_map,
#'   prompt = FALSE,
#'   quiet = TRUE
#' )
#' }
ReviewDuplicateSubmissions <- function(
		x,
		id_col = 1,
		key_cols = NULL,
		compare_cols = NULL,
		return_flat = FALSE,
		quiet = FALSE,
		keep_map = NULL,
		prompt = TRUE,
		default_keep = "all"
) {
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)

	df <- extract_flat_df(x)
	df <- as.data.frame(df, stringsAsFactors = FALSE)
	id_col_name <- resolve_id_col(df, id_col)

	ids <- as.character(df[[id_col_name]])

	prompt <- isTRUE(prompt)
	if (!interactive()) prompt <- FALSE

	key_cols <- pick_duplicate_key_columns(df, id_col_name, key_cols, quiet = quiet, prompt = prompt)
	default_compare <- default_non_form_cols(names(df), id_col_name, n = 3)
	compare_default <- unique(c(key_cols, default_compare))
	if (length(compare_default) == 0) compare_default <- id_col_name
	compare_cols <- pick_compare_columns(
		df,
		id_col_name,
		compare_cols,
		quiet = quiet,
		prompt = prompt,
		default_cols = compare_default
	)

	if (is.null(key_cols) || length(key_cols) == 0) {
		stop("key_cols is required to identify duplicates. Provide key_cols or set prompt = TRUE.")
	}

	key_info <- build_duplicate_keys(df, key_cols)
	key_id <- key_info$key_id
	group_keys <- key_info$group_keys

	group_order <- match(unique(key_id), key_id)
	group_ids <- unique(key_id)

	dup_groups <- list()
	for (i in seq_along(group_ids)) {
		rows_idx <- which(key_id == group_ids[i])
		if (length(rows_idx) == 0) next
		group_unique_ids <- unique(ids[rows_idx])
		group_unique_ids <- group_unique_ids[!is.na(group_unique_ids) & group_unique_ids != ""]
		if (length(group_unique_ids) >= 2) {
			dup_groups[[length(dup_groups) + 1]] <- list(
				group_id = length(dup_groups) + 1L,
				key_id = group_ids[i],
				group_key = group_keys[rows_idx[1]],
				rows_idx = rows_idx,
				submission_ids = group_unique_ids
			)
		}
	}

	if (length(dup_groups) == 0) {
		if (!quiet) message("No duplicate submissions detected for the selected key columns.")
		out <- list(
			data = df,
			summary = list(
				original_rows = nrow(df),
				kept_rows = nrow(df),
				removed_rows = 0L,
				duplicate_groups = 0L,
				processed_groups = 0L,
				stopped_early = FALSE
			),
			decisions = data.frame(
				group_id = integer(),
				group_key = character(),
				rows_total = integer(),
				submission_ids = character(),
				kept_ids = character(),
				dropped_ids = character(),
				kept_n = integer(),
				dropped_n = integer(),
				decision = character(),
				stringsAsFactors = FALSE
			)
		)
		if (audit_depth == 1) {
			maybe_write_audit("ReviewDuplicateSubmissions", details = "no duplicates", data = df)
		}
		return(out)
	}

	keep_map <- normalize_keep_map(keep_map)
	if (!prompt && (is.null(keep_map) || length(keep_map) == 0)) {
		if (is.null(default_keep) || length(default_keep) == 0 || !nzchar(as.character(default_keep)[1])) {
			stop("Non-interactive mode requires keep_map or a valid default_keep.")
		}
	}

	if (audit_depth == 1) maybe_prompt_audit_log()
	rows_keep <- rep(TRUE, nrow(df))
	decision_rows <- list()
	stopped <- FALSE

	if (!quiet) {
		message("Duplicate review: you will see each duplicate group and choose which submissions to keep.")
	}

	for (grp in dup_groups) {
		rows_idx <- grp$rows_idx
		group_id <- grp$group_id
		group_key <- grp$group_key
		group_unique_ids <- grp$submission_ids

		if (length(group_unique_ids) <= 1) next

		if (!quiet) {
			cat("\n")
			message("Duplicate group ", group_id, ": ", group_key, " (", length(group_unique_ids), " submission IDs)")
			highlight_cols <- setdiff(compare_cols, id_col_name)
			display_df <- build_compare_frame_by_id(
				df,
				rows_idx,
				ids,
				group_unique_ids,
				compare_cols,
				highlight_cols = highlight_cols
			)
			print_table_left(display_df)
		}

		keep_local <- NULL
		if (!is.null(keep_map)) {
			keep_local <- resolve_keep_for_group(keep_map, group_id = group_id, group_key = group_key, n_rows = length(group_unique_ids))
		}
		if (is.null(keep_local)) {
			if (prompt) {
				keep_local <- ask_keep_rows(length(group_unique_ids))
			} else {
				keep_local <- parse_keep_choice(default_keep, length(group_unique_ids))
			}
		}

		if (identical(keep_local, "quit")) {
			stopped <- TRUE
			if (!quiet) message("Stopping duplicate review early. Rows not yet reviewed will be kept.")
			break
		}

		keep_local <- sort(unique(keep_local))
		keep_local <- keep_local[keep_local >= 1 & keep_local <= length(group_unique_ids)]
		drop_local <- setdiff(seq_along(group_unique_ids), keep_local)

		kept_ids <- group_unique_ids[keep_local]
		dropped_ids <- group_unique_ids[drop_local]

		if (length(dropped_ids) > 0) {
			rows_keep[ids %in% dropped_ids] <- FALSE
		}

		decision_rows[[length(decision_rows) + 1]] <- data.frame(
			group_id = as.integer(group_id),
			group_key = as.character(group_key),
			rows_total = length(rows_idx),
			submission_ids = paste(group_unique_ids, collapse = ", "),
			kept_ids = if (length(kept_ids) > 0) paste(kept_ids, collapse = ", ") else "",
			dropped_ids = if (length(dropped_ids) > 0) paste(dropped_ids, collapse = ", ") else "",
			kept_n = length(kept_ids),
			dropped_n = length(dropped_ids),
			decision = if (length(dropped_ids) == 0) "kept_all" else if (length(kept_ids) == 0) "dropped_all" else "kept_subset",
			stringsAsFactors = FALSE
		)

		if (audit_depth == 1) {
			detail <- paste0(
				"Group ", group_id, " (", group_key, "): kept ",
				length(kept_ids), " of ", length(group_unique_ids),
				if (length(dropped_ids) > 0) paste0(" (dropped IDs ", paste(dropped_ids, collapse = ", "), ")") else ""
			)
			maybe_write_audit("ReviewDuplicateSubmissions", details = detail, data = df[rows_idx, , drop = FALSE])
		}
	}

	out_df <- df[rows_keep, , drop = FALSE]
	decisions <- if (length(decision_rows) == 0) {
		data.frame(
			group_id = integer(),
			group_key = character(),
			rows_total = integer(),
			submission_ids = character(),
			kept_ids = character(),
			dropped_ids = character(),
			kept_n = integer(),
			dropped_n = integer(),
			decision = character(),
			stringsAsFactors = FALSE
		)
	} else {
		do.call(rbind, decision_rows)
	}

	out <- list(
		data = out_df,
		summary = list(
			original_rows = nrow(df),
			kept_rows = nrow(out_df),
			removed_rows = nrow(df) - nrow(out_df),
			duplicate_groups = length(dup_groups),
			processed_groups = nrow(decisions),
			stopped_early = stopped
		),
		decisions = decisions
	)

	if (is_flat_list(x) && isTRUE(return_flat)) {
		x$FlatResponses <- out_df
		x$ColumnNames <- update_column_names(x$ColumnNames, names(out_df))
		out$flat <- x
	}

	if (!quiet) {
		message("Removed ", out$summary$removed_rows, " row(s) across ", out$summary$processed_groups, " duplicate group(s).")
	}

	out
}


#' Compact checkbox/multi-select columns into a single readable column
#'
#' When a question generates multiple TRUE/FALSE columns (e.g., select boxes),
#' this function combines them into a single comma-separated column.
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
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
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

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

	if (audit_depth == 1) {
		maybe_write_audit("CompactSelections", data = out$data)
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

parse_index_input <- function(input, max_val) {
	input <- trimws(input)
	if (!nzchar(input)) return(integer(0))

	# Normalize ranges like "1: 5" -> "1:5"
	input <- gsub("\\s*:\\s*", ":", input)
	parts <- unlist(strsplit(input, "[,;\\s]+"))
	parts <- trimws(parts)
	parts <- parts[parts != ""]
	if (length(parts) == 0) return(integer(0))

	result <- integer(0)
	for (p in parts) {
		if (grepl("^[0-9]+:[0-9]+$", p)) {
			rng <- strsplit(p, ":")[[1]]
			start <- suppressWarnings(as.integer(rng[1]))
			end <- suppressWarnings(as.integer(rng[2]))
			if (!is.na(start) && !is.na(end)) {
				if (start <= end) {
					result <- c(result, seq(start, end))
				} else {
					result <- c(result, seq(start, end, by = -1))
				}
			}
		} else if (grepl("^[0-9]+$", p)) {
			val <- suppressWarnings(as.integer(p))
			if (!is.na(val)) result <- c(result, val)
		}
	}

	result <- unique(result)
	result <- result[result >= 1 & result <= max_val]
	sort(result)
}

default_non_form_cols <- function(df_names, id_col_name, n = 3) {
	non_form <- df_names[!grepl("^form[-_]", df_names)]
	non_form <- setdiff(non_form, id_col_name)
	if (length(non_form) == 0) {
		non_form <- setdiff(df_names, id_col_name)
	}
	head(non_form, n)
}

formior_use_color <- function() {
	opt <- getOption("formior.color")
	if (!is.null(opt)) return(isTRUE(opt))
	if (!interactive()) return(FALSE)
	no_color <- Sys.getenv("NO_COLOR")
	if (nzchar(no_color)) return(FALSE)
	isTRUE(crayon::has_color())
}

strip_style_safe <- function(x) {
	if (is.null(x)) return("")
	x <- as.character(x)
	if (isTRUE(formior_use_color())) {
		return(crayon::strip_style(x))
	}
	x
}

display_width <- function(x) {
	nchar(strip_style_safe(x), type = "width")
}

pad_right <- function(x, width) {
	x <- as.character(x)
	w <- display_width(x)
	if (is.na(w)) w <- 0
	if (w >= width) return(x)
	paste0(x, strrep(" ", width - w))
}

print_table_left <- function(df) {
	if (!is.data.frame(df) || nrow(df) == 0) return(invisible(NULL))
	df <- as.data.frame(df, stringsAsFactors = FALSE)
	cols <- names(df)

	widths <- vapply(seq_along(cols), function(i) {
		col_vals <- c(cols[i], as.character(df[[i]]))
		max(display_width(col_vals), na.rm = TRUE)
	}, numeric(1))

	header <- paste(mapply(pad_right, cols, widths, USE.NAMES = FALSE), collapse = "  ")
	cat(header, "\n")

	for (i in seq_len(nrow(df))) {
		row_vals <- vapply(seq_along(cols), function(j) {
			val <- df[[j]][i]
			if (is.na(val)) val <- ""
			pad_right(val, widths[j])
		}, character(1))
		cat(paste(row_vals, collapse = "  "), "\n")
	}
	invisible(NULL)
}

print_column_selection_table <- function(df_names, right = FALSE, max_rows = 25, color_numbers = TRUE) {
	df_names <- as.character(df_names)
	n <- length(df_names)
	if (n == 0) return(invisible(NULL))

	format_no <- function(x) {
		x <- as.character(x)
		x[is.na(x)] <- ""
		if (isTRUE(color_numbers) && isTRUE(formior_use_color())) {
			return(cyan(x))
		}
		x
	}

	if (n <= max_rows) {
		df <- data.frame(No = format_no(seq_along(df_names)), Column = df_names, stringsAsFactors = FALSE)
		print_table_left(df)
		return(invisible(NULL))
	}

	n_rows <- ceiling(n / 2)
	idx1 <- seq_len(n_rows)
	idx2 <- seq(from = n_rows + 1, length.out = n_rows)
	idx2 <- idx2[idx2 <= n]

	df1 <- data.frame(No = format_no(idx1), Column = df_names[idx1], stringsAsFactors = FALSE)
	df2 <- data.frame(No = format_no(idx2), Column = df_names[idx2], stringsAsFactors = FALSE)

	if (nrow(df2) < nrow(df1)) {
		pad <- nrow(df1) - nrow(df2)
		df2 <- rbind(
			df2,
			data.frame(No = format_no(rep(NA_integer_, pad)), Column = rep("", pad), stringsAsFactors = FALSE)
		)
	}

	out <- data.frame(
		No = df1$No,
		Column = df1$Column,
		No = df2$No,
		Column = df2$Column,
		check.names = FALSE,
		stringsAsFactors = FALSE
	)

	print_table_left(out)
	invisible(NULL)
}

normalize_keep_map <- function(keep_map) {
	if (is.null(keep_map)) return(NULL)

	if (is.list(keep_map) && !is.data.frame(keep_map)) {
		if (!is.null(names(keep_map))) return(keep_map)
	}

	if (is.character(keep_map) && !is.null(names(keep_map))) {
		out <- as.list(keep_map)
		return(out)
	}

	if (inherits(keep_map, "data.frame")) {
		if (!("keep" %in% names(keep_map))) {
			stop("keep_map data.frame must include a 'keep' column.")
		}
		key_col <- NULL
		if ("group_key" %in% names(keep_map)) key_col <- "group_key"
		if (is.null(key_col) && "group_id" %in% names(keep_map)) key_col <- "group_id"
		if (is.null(key_col)) {
			stop("keep_map data.frame must include 'group_key' or 'group_id'.")
		}
		out <- keep_map$keep
		names(out) <- as.character(keep_map[[key_col]])
		return(as.list(out))
	}

	stop("keep_map must be a named list/vector keyed by group_id/group_key, or a data frame with 'group_id'/'group_key' and 'keep'.")
}

parse_keep_choice <- function(choice, n_rows) {
	if (is.null(choice)) return(NULL)

	if (is.logical(choice) && length(choice) == 1) {
		return(if (isTRUE(choice)) seq_len(n_rows) else integer(0))
	}

	if (is.numeric(choice)) {
		idx <- unique(as.integer(choice))
		idx <- idx[!is.na(idx) & idx >= 1 & idx <= n_rows]
		return(idx)
	}

	choice <- as.character(choice)
	if (length(choice) == 0) return(NULL)
	if (length(choice) == 1) {
		val <- tolower(trimws(choice))
		if (val %in% c("all", "a", "keep")) return(seq_len(n_rows))
		if (val %in% c("none", "drop", "delete")) return(integer(0))
	}

	input <- paste(choice, collapse = ",")
	parse_index_input(input, n_rows)
}

resolve_keep_for_group <- function(keep_map, group_id, group_key, n_rows) {
	if (is.null(keep_map) || length(keep_map) == 0) return(NULL)
	if (!is.null(names(keep_map))) {
		if (!is.null(group_key) && group_key %in% names(keep_map)) {
			return(parse_keep_choice(keep_map[[group_key]], n_rows))
		}
		if (as.character(group_id) %in% names(keep_map)) {
			return(parse_keep_choice(keep_map[[as.character(group_id)]], n_rows))
		}
	}
	NULL
}

pick_duplicate_key_columns <- function(df, id_col_name, key_cols, quiet = FALSE, prompt = TRUE) {
	df_names <- names(df)
	if (!is.null(key_cols)) {
		if (is.numeric(key_cols)) {
			key_cols <- df_names[key_cols]
		}
		key_cols <- unique(as.character(key_cols))
		key_cols <- key_cols[key_cols %in% df_names]
		key_cols <- setdiff(key_cols, id_col_name)
		if (length(key_cols) > 0) return(key_cols)
	}

	if (!interactive() || !prompt) {
		return(character(0))
	}

	default_cols <- default_non_form_cols(df_names, id_col_name, n = 3)
	default_label <- if (length(default_cols) > 0) paste(default_cols, collapse = ", ") else "(none)"

	if (!quiet) {
		message("Choose columns that define a duplicate submission (e.g., username, email, project ID).")
		print_column_selection_table(df_names, right = FALSE, max_rows = 25)
		if (length(default_cols) > 0) {
			message("Suggested default columns: ", default_label)
		}
	}

	prompt_txt <- paste0(
		"Duplicate key columns (names or numbers). Press Enter for default: ",
		default_label,
		" (type 'suggest' to reprint defaults)\n> "
	)

	repeat {
		input <- readline(prompt_txt)
		input <- trimws(input)
		if (tolower(input) %in% c("suggest", "suggested", "s")) {
			if (!quiet) message("Suggested default columns: ", default_label)
			next
		}
		if (!nzchar(input)) {
			return(default_cols)
		}

		if (tolower(input) %in% c("all", "everything")) {
			return(setdiff(df_names, id_col_name))
		}

		if (all(grepl("^[0-9,:;\\s]+$", input))) {
			idx <- parse_index_input(input, length(df_names))
			cols <- df_names[idx]
			cols <- setdiff(cols, id_col_name)
			return(unique(cols))
		}

		parts <- unlist(strsplit(input, "[,;]+"))
		parts <- trimws(parts)
		parts <- parts[parts != ""]
		cols <- parts[parts %in% df_names]
		cols <- setdiff(cols, id_col_name)
		return(unique(cols))
	}
}

pick_compare_columns <- function(df, id_col_name, compare_cols, quiet = FALSE, prompt = TRUE, default_cols = NULL) {
	df_names <- names(df)
	if (is.null(default_cols)) {
		default_cols <- unique(c(id_col_name, head(setdiff(df_names, id_col_name), 4)))
	} else {
		default_cols <- unique(default_cols)
	}
	if (length(default_cols) == 0) {
		default_cols <- id_col_name
	}

	if (!is.null(compare_cols)) {
		if (is.numeric(compare_cols)) {
			compare_cols <- df_names[compare_cols]
		}
		compare_cols <- unique(as.character(compare_cols))
		compare_cols <- compare_cols[compare_cols %in% df_names]
		if (length(compare_cols) == 0) compare_cols <- default_cols
		if (!id_col_name %in% compare_cols) compare_cols <- c(id_col_name, compare_cols)
		return(compare_cols)
	}

	if (!interactive() || !prompt) {
		return(default_cols)
	}

	if (!quiet) {
		message("Choose columns to display for duplicate comparisons.")
		print_column_selection_table(df_names, right = FALSE, max_rows = 25)
		if (length(default_cols) > 0) {
			message("Suggested default columns: ", paste(default_cols, collapse = ", "))
		}
	}

	default_label <- paste(default_cols, collapse = ", ")
	prompt <- paste0(
		"Columns to display (names or numbers, comma-separated). Press Enter for default: ",
		default_label,
		" (type 'suggest' to reprint defaults)\n> "
	)

	repeat {
		input <- readline(prompt)
		input <- trimws(input)
		if (tolower(input) %in% c("suggest", "suggested", "s")) {
			if (!quiet) message("Suggested default columns: ", default_label)
			next
		}
		if (!nzchar(input)) {
			return(default_cols)
		}

		if (tolower(input) %in% c("all", "everything")) {
			return(df_names)
		}

		if (all(grepl("^[0-9,:;\\s]+$", input))) {
			idx <- parse_index_input(input, length(df_names))
			cols <- df_names[idx]
			if (length(cols) == 0) cols <- default_cols
			if (!id_col_name %in% cols) cols <- c(id_col_name, cols)
			return(unique(cols))
		}

		parts <- unlist(strsplit(input, "[,;]+"))
		parts <- trimws(parts)
		parts <- parts[parts != ""]
		cols <- parts[parts %in% df_names]
		if (length(cols) == 0) cols <- default_cols
		if (!id_col_name %in% cols) cols <- c(id_col_name, cols)
		return(unique(cols))
	}
}

build_duplicate_keys <- function(df, key_cols) {
	key_vals <- lapply(key_cols, function(col) {
		vals <- coerce_repeat_values(df[[col]])
		vals <- as.character(vals)
		vals[is.na(vals) | vals == ""] <- "<NA>"
		vals
	})
	key_frame <- as.data.frame(key_vals, stringsAsFactors = FALSE)
	names(key_frame) <- key_cols

	key_id <- do.call(paste, c(key_frame, sep = "\r"))
	group_keys <- apply(key_frame, 1, function(row) {
		paste(paste0(key_cols, "=", row), collapse = " | ")
	})

	list(key_id = key_id, group_keys = group_keys)
}

build_compare_frame_by_id <- function(df, rows_idx, ids, unique_ids, compare_cols, highlight_cols = NULL) {
	display_rows <- vapply(unique_ids, function(id) {
		rows_idx[which(ids[rows_idx] == id)[1]]
	}, integer(1))

	out <- df[display_rows, compare_cols, drop = FALSE]
	out <- as.data.frame(out, stringsAsFactors = FALSE)

	for (col in names(out)) {
		out[[col]] <- coerce_repeat_values(out[[col]])
		if (inherits(out[[col]], "POSIXt") || inherits(out[[col]], "Date")) {
			out[[col]] <- as.character(out[[col]])
		}
	}

	if (!is.null(highlight_cols)) {
		highlight_cols <- intersect(highlight_cols, names(out))
		use_color <- formior_use_color()
		for (col in highlight_cols) {
			vals <- as.character(out[[col]])
			non_empty <- which(!is.na(vals) & vals != "")
			if (length(non_empty) == 0) next

			base <- vals[non_empty[1]]
			diff_idx <- which(!is.na(vals) & vals != "" & vals != base)
			if (length(diff_idx) > 0) {
				if (isTRUE(use_color)) {
					vals[diff_idx] <- red(vals[diff_idx])
				}
				out[[col]] <- vals
			}
		}
	}

	row_counts <- vapply(unique_ids, function(id) sum(ids[rows_idx] == id), integer(1))

	data.frame(
		Option = seq_along(unique_ids),
		SubmissionId = unique_ids,
		RowCount = row_counts,
		out,
		check.names = FALSE,
		stringsAsFactors = FALSE
	)
}

ask_keep_rows <- function(n_rows) {
	repeat {
		input <- readline(paste0("Submissions to keep (1-", n_rows, "; e.g., 1,3 or 2:4; 'all', 'none', 'skip', 'q'): "))
		input <- trimws(tolower(input))

		if (!nzchar(input) || input %in% c("all", "a", "skip", "s")) {
			return(seq_len(n_rows))
		}
		if (input %in% c("none", "drop", "delete")) {
			confirm <- readline("Drop all rows for this ID? [y/N]: ")
			if (tolower(trimws(confirm)) %in% c("y", "yes")) {
				return(integer(0))
			}
			next
		}
		if (input %in% c("q", "quit", "exit")) {
			return("quit")
		}

		idx <- parse_index_input(input, n_rows)
		if (length(idx) == 0) {
			message("Please enter one or more valid row numbers.")
			next
		}
		return(idx)
	}
}

pick_first_non_na <- function(x) {
	x <- x[!is.na(x)]
	# Preserve the input type (e.g., character -> NA_character_)
	x[1]
}

coerce_repeat_values <- function(values) {
	if (is.factor(values)) values <- as.character(values)

	# Some tidyverse workflows can produce "data.frame columns" (e.g., nested tibbles).
	# As a column, these have one row per submission, but `split()` can't operate on them.
	# Convert each row to a single JSON/text value so the column behaves like a vector.
	if (inherits(values, "data.frame")) {
		if (nrow(values) == 0) return(character(0))
		return(vapply(seq_len(nrow(values)), function(i) {
			list_element_to_text(values[i, , drop = FALSE])
		}, character(1)))
	}

	# FormIO exports can include "object" fields that remain list-columns after flattening.
	# Convert to a single JSON/text value per row so downstream wrangling and export remain stable.
	if (is.list(values) && !inherits(values, "POSIXt") && !inherits(values, "Date")) {
		return(vapply(values, list_element_to_text, character(1)))
	}

	values
}


list_element_to_text <- function(x) {
	if (is.null(x) || length(x) == 0) return(NA_character_)

	# Common cases: scalar atomic values
	if (is.atomic(x) && length(x) == 1) return(as.character(x))
	if (is.atomic(x) && length(x) > 1) return(paste(as.character(x), collapse = ", "))

	# For nested lists / data.frames, use JSON for a compact, readable representation.
	json <- tryCatch(
		as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null")),
		error = function(e) NA_character_
	)

	json
}

auto_strategy <- function(values) {
	if (is_numeric_like(values)) return("sum")
	if (is_checkbox_like(values)) return("count_yes")
	"concat"
}

apply_repeat_strategy <- function(values, strategy, sep, unique) {
	# Keep factors as text to avoid confusing numeric coercion and vapply type mismatches.
	if (is.factor(values)) values <- as.character(values)

	values <- values[!is.na(values)]
	if (unique && strategy %in% c("concat", "first", "last")) {
		values <- unique(values)
	}

	if (strategy == "concat") {
		return(if (length(values) == 0) NA_character_ else paste(values, collapse = sep))
	}
	if (strategy == "first") {
		# values[1] returns a typed NA when length(values) == 0
		return(values[1])
	}
	if (strategy == "last") {
		if (length(values) == 0) return(values[1])
		return(values[length(values)])
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
	if (is.factor(values)) values <- as.character(values)
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
	if (is.factor(values)) values <- as.character(values)
	if (inherits(values, "Date")) return(as.Date(NA))
	if (inherits(values, "POSIXt")) return(as.POSIXct(NA))
	if (is.numeric(values)) return(NA_real_)
	if (is.logical(values)) return(NA)
	NA_character_
}

strategy_fun_value <- function(strategy, values) {
	if (strategy %in% c("sum", "mean", "count", "count_yes")) return(NA_real_)
	if (strategy == "concat") return(NA_character_)
	na_value_for(values)
}
