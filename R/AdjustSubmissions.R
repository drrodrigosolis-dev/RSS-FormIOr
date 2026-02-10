#' Adjust submissions by ID (delete or edit specific values)
#'
#' Sometimes you need to make small, targeted fixes before you export:
#' remove test submissions, correct a value for one submission, or blank out
#' a field for privacy reasons.
#'
#' This helper lets you:
#' - Delete submission(s) by ID (remove rows)
#' - Update one or more column values for specific submission ID(s)
#'
#' It works on either a plain data frame or the list produced by
#' [FlattenSubmissions()]. If audit logging is active (see [StartAuditLog()]),
#' this action is recorded.
#'
#' @param x A data frame of responses, or a list from [FlattenSubmissions()].
#' @param id_col Integer or character. Submission ID column (default 1).
#' @param delete_ids Optional character vector of submission IDs to delete.
#' @param updates Optional updates to apply. Supported formats:
#' - A data.frame with columns `id`, `column`, `value`
#' - A data.frame with columns `submissionId`/`submission_id`, `column`, `value`
#' - A list of lists, each with elements `id`, `column`, `value`
#'
#' Values are coerced to the target column type when possible. Use `"NA"` (or
#' an actual `NA`) to set a value to missing.
#' @param return_flat Logical. If `TRUE` and `x` came from [FlattenSubmissions()],
#'   include the updated list as `flat` in the output.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Updated data frame}
#'   \item{summary}{Data frame summarizing deletions/updates}
#'   \item{changes}{Data frame listing each requested update and its status}
#'   \item{flat}{If `return_flat = TRUE` and `x` was a FlattenSubmissions list, the updated list}
#' }
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   submissionId = c("a", "b", "c"),
#'   status = c("ok", "test", "ok"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Delete one submission and update a value
#' out <- AdjustSubmissions(
#'   df,
#'   id_col = "submissionId",
#'   delete_ids = "b",
#'   updates = data.frame(id = "c", column = "status", value = "review", stringsAsFactors = FALSE),
#'   quiet = TRUE
#' )
#' out$data
AdjustSubmissions <- function(
		x,
		id_col = 1,
		delete_ids = NULL,
		updates = NULL,
		return_flat = FALSE,
		quiet = FALSE
) {
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

	df <- extract_flat_df(x)
	df <- as.data.frame(df, stringsAsFactors = FALSE)
	id_col_name <- resolve_id_col(df, id_col)

	n_before <- nrow(df)
	change_rows <- list()

	# ---- Deletes ----------------------------------------------------------------
	deleted_rows <- 0L
	deleted_ids <- character(0)
	if (!is.null(delete_ids) && length(delete_ids) > 0) {
		delete_ids <- unique(as.character(delete_ids))
		delete_ids <- delete_ids[!is.na(delete_ids) & delete_ids != ""]

		if (length(delete_ids) > 0) {
			keep <- !(as.character(df[[id_col_name]]) %in% delete_ids)
			deleted_rows <- sum(!keep)
			deleted_ids <- unique(as.character(df[[id_col_name]][!keep]))
			df <- df[keep, , drop = FALSE]

			change_rows[[length(change_rows) + 1]] <- data.frame(
				action = "delete",
				id = paste(deleted_ids, collapse = ", "),
				column = NA_character_,
				old_value = NA_character_,
				new_value = NA_character_,
				rows_affected = as.integer(deleted_rows),
				status = if (deleted_rows > 0) "deleted" else "no_match",
				message = if (deleted_rows > 0) NA_character_ else "No matching submission IDs found.",
				stringsAsFactors = FALSE
			)
		}
	}

	# ---- Updates ----------------------------------------------------------------
	updates_df <- normalize_adjust_updates(updates)
	applied_updates <- 0L
	if (!is.null(updates_df) && nrow(updates_df) > 0) {
		for (i in seq_len(nrow(updates_df))) {
			id_val <- as.character(updates_df$id[i])
			col_nm <- as.character(updates_df$column[i])
			new_raw <- updates_df$value[[i]]

			if (is.na(id_val) || !nzchar(id_val)) {
				change_rows[[length(change_rows) + 1]] <- data.frame(
					action = "update",
					id = id_val,
					column = col_nm,
					old_value = NA_character_,
					new_value = to_update_text(new_raw),
					rows_affected = 0L,
					status = "invalid_id",
					message = "Missing submission ID.",
					stringsAsFactors = FALSE
				)
				next
			}

			if (is.na(col_nm) || !nzchar(col_nm) || !col_nm %in% names(df)) {
				change_rows[[length(change_rows) + 1]] <- data.frame(
					action = "update",
					id = id_val,
					column = col_nm,
					old_value = NA_character_,
					new_value = to_update_text(new_raw),
					rows_affected = 0L,
					status = "column_not_found",
					message = paste0("Column not found: ", col_nm),
					stringsAsFactors = FALSE
				)
				next
			}

			rows <- which(as.character(df[[id_col_name]]) == id_val)
			if (length(rows) == 0) {
				change_rows[[length(change_rows) + 1]] <- data.frame(
					action = "update",
					id = id_val,
					column = col_nm,
					old_value = NA_character_,
					new_value = to_update_text(new_raw),
					rows_affected = 0L,
					status = "id_not_found",
					message = "Submission ID not found in data.",
					stringsAsFactors = FALSE
				)
				next
			}

			old_preview <- to_update_text(df[[col_nm]][rows[1]])
			new_val <- coerce_update_value(new_raw, df[[col_nm]])

			if (is.factor(df[[col_nm]])) {
				lvls <- union(levels(df[[col_nm]]), as.character(new_val))
				levels(df[[col_nm]]) <- lvls
				df[[col_nm]][rows] <- as.character(new_val)
			} else if (is.list(df[[col_nm]]) && !inherits(df[[col_nm]], "POSIXt") && !inherits(df[[col_nm]], "Date")) {
				df[[col_nm]][rows] <- rep(list(new_val), length(rows))
			} else {
				df[[col_nm]][rows] <- rep(new_val, length(rows))
			}

			applied_updates <- applied_updates + 1L
			change_rows[[length(change_rows) + 1]] <- data.frame(
				action = "update",
				id = id_val,
				column = col_nm,
				old_value = old_preview,
				new_value = to_update_text(new_val),
				rows_affected = as.integer(length(rows)),
				status = "updated",
				message = NA_character_,
				stringsAsFactors = FALSE
			)
		}
	}

	n_after <- nrow(df)

	changes <- if (length(change_rows) == 0) {
		data.frame(
			action = character(),
			id = character(),
			column = character(),
			old_value = character(),
			new_value = character(),
			rows_affected = integer(),
			status = character(),
			message = character(),
			stringsAsFactors = FALSE
		)
	} else {
		do.call(rbind, change_rows)
	}

	summary <- data.frame(
		metric = c("rows_before", "rows_after", "rows_deleted", "updates_applied"),
		value = c(n_before, n_after, deleted_rows, applied_updates),
		stringsAsFactors = FALSE
	)

	out <- list(
		data = df,
		summary = summary,
		changes = changes
	)

	if (is_flat_list(x) && isTRUE(return_flat)) {
		x$FlatResponses <- df
		x$ColumnNames <- update_column_names(x$ColumnNames, names(df))
		out$flat <- x
	}

	if (!quiet) {
		if (deleted_rows > 0) message("Deleted ", deleted_rows, " row(s).")
		if (applied_updates > 0) message("Applied ", applied_updates, " update(s).")
		if (deleted_rows == 0 && applied_updates == 0) message("No changes applied.")
	}

	if (audit_depth == 1) {
		maybe_write_audit(
			"AdjustSubmissions",
			details = paste0("deleted_rows=", deleted_rows, "; updates_applied=", applied_updates),
			data = df
		)
	}

	out
}


# ---- Internal helpers ---------------------------------------------------------

normalize_adjust_updates <- function(updates) {
	if (is.null(updates)) return(NULL)

	if (inherits(updates, "data.frame")) {
		df <- as.data.frame(updates, stringsAsFactors = FALSE)
		id_nm <- intersect(names(df), c("id", "submissionId", "submission_id"))[1] %||% NULL
		col_nm <- intersect(names(df), c("column", "col", "field"))[1] %||% NULL
		val_nm <- intersect(names(df), c("value", "new_value", "newValue"))[1] %||% NULL

		if (is.null(id_nm) || is.null(col_nm) || is.null(val_nm)) {
			stop("updates must include columns id/submissionId, column, and value.")
		}

		values <- df[[val_nm]]
		if (!is.list(values)) values <- as.list(values)

		return(data.frame(
			id = as.character(df[[id_nm]]),
			column = as.character(df[[col_nm]]),
			value = I(values),
			stringsAsFactors = FALSE
		))
	}

	if (is.list(updates)) {
		if (length(updates) == 0) return(NULL)
		rows <- lapply(updates, function(u) {
			if (!is.list(u)) return(NULL)
			list(
				id = u$id %||% u$submissionId %||% u$submission_id %||% NA_character_,
				column = u$column %||% u$col %||% u$field %||% NA_character_,
				value = list(u$value %||% u$new_value %||% u$newValue %||% NA)
			)
		})
		rows <- rows[!vapply(rows, is.null, logical(1))]
		if (length(rows) == 0) return(NULL)

		df <- do.call(rbind, lapply(rows, function(r) {
			data.frame(
				id = as.character(r$id),
				column = as.character(r$column),
				value = I(r$value),
				stringsAsFactors = FALSE
			)
		}))
		rownames(df) <- NULL
		return(df)
	}

	stop("updates must be a data.frame or a list.")
}


coerce_update_value <- function(value, template) {
	# Unwrap common cases
	if (is.list(value) && length(value) == 1 && !inherits(value, "data.frame")) {
		value <- value[[1]]
	}
	if (is.null(value) || length(value) == 0) {
		return(na_value_for(template))
	}

	# Treat "NA" (and blank strings) as missing
	if (is.character(value)) {
		value <- trimws(value)[1]
		if (!nzchar(value) || tolower(value) == "na") {
			return(na_value_for(template))
		}
	}

	if (inherits(template, "Date")) {
		out <- suppressWarnings(as.Date(value))
		if (is.na(out)) return(as.Date(NA))
		return(out)
	}
	if (inherits(template, "POSIXt")) {
		out <- suppressWarnings(as.POSIXct(value, tz = "UTC"))
		if (is.na(out)) return(as.POSIXct(NA))
		return(out)
	}

	if (is.logical(template)) {
		return(parse_logical_value(value))
	}
	if (is.numeric(template)) {
		out <- suppressWarnings(as.numeric(value))
		if (is.na(out)) return(NA_real_)
		return(out)
	}

	if (is.factor(template)) {
		return(as.character(value))
	}

	if (is.list(template) && !inherits(template, "POSIXt") && !inherits(template, "Date")) {
		# Keep list columns as-is (best-effort).
		return(value)
	}

	as.character(value)[1]
}


parse_logical_value <- function(x) {
	if (is.logical(x)) return(as.logical(x)[1])
	if (is.numeric(x)) return(as.logical(x != 0)[1])

	chr <- tolower(trimws(as.character(x)[1]))
	if (!nzchar(chr) || chr == "na") return(NA)

	if (chr %in% c("true", "t", "yes", "y", "1")) return(TRUE)
	if (chr %in% c("false", "f", "no", "n", "0")) return(FALSE)
	NA
}


to_update_text <- function(x) {
	if (is.null(x) || length(x) == 0) return(NA_character_)
	if (is.list(x) && !inherits(x, "data.frame") && length(x) == 1) x <- x[[1]]
	if (is.list(x) || inherits(x, "data.frame")) return(list_element_to_text(x))
	if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.character(x)[1])
	as.character(x)[1]
}

