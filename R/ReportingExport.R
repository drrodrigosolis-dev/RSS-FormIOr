#' Export data to Excel (or CSV if needed)
#'
#' This is a simple, user-friendly export helper for FormIOr outputs.
#' It accepts a data.frame, a list of data.frames (multiple sheets),
#' or a list returned by [GetResponses()] / [FlattenSubmissions()].
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' If an Excel writer is available (`writexl` or `openxlsx`), it writes `.xlsx`.
#' If not, it falls back to CSV files and prints a clear message.
#'
#' @param data A data.frame, list of data.frames, or list containing
#'   `FlatResponses` or `submission_data`.
#' @param path File path for the Excel output (default: `"formio_export.xlsx"`).
#'   If you use a `.csv` or `.tsv` extension, delimited files will be written.
#' @param sheet Sheet name to use when exporting a single data.frame.
#' @param overwrite Logical. If `FALSE` (default), stop if the file exists.
#' @param include_row_names Logical. Include row names in the export.
#' @param quiet Logical. If `FALSE`, prints a short message.
#'
#' @return A list with:
#' \describe{
#'   \item{path}{Output file path(s).}
#'   \item{format}{`"xlsx"`, `"csv"`, or `"tsv"`.}
#'   \item{sheets}{Sheet names used.}
#'   \item{rows}{Row counts for each sheet.}
#'   \item{cols}{Column counts for each sheet.}
#' }
#'
#' @export
#'
#' @examples
#' df <- data.frame(name = c("A", "B"), score = c(1, 2))
#' \dontrun{
#'   ExportToExcel(df, "my_export.xlsx", overwrite = TRUE)
#' }
ExportToExcel <- function(
		data,
		path = "formio_export.xlsx",
		sheet = "Data",
		overwrite = FALSE,
		include_row_names = FALSE,
		quiet = FALSE
) {
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

	sheets <- coerce_export_sheets(data, sheet = sheet)
	ext <- tolower(tools::file_ext(path))

	if (ext %in% c("csv", "tsv")) {
		out <- write_delimited_sheets(
			sheets = sheets,
			path = path,
			ext = ext,
			overwrite = overwrite,
			include_row_names = include_row_names
		)

		if (!quiet) {
			message("Exported ", length(out$path), " file(s) to ", out$format, ".")
		}

		if (audit_depth == 1) {
			maybe_write_audit(
				"ExportToExcel",
				details = paste0("format=", out$format, "; path=", path, "; sheets=", length(out$sheets)),
				data = sheets[[1]]
			)
		}

		return(out)
	}

	out <- write_excel_sheets(
		sheets = sheets,
		path = path,
		overwrite = overwrite,
		include_row_names = include_row_names,
		quiet = quiet
	)

	if (audit_depth == 1) {
		maybe_write_audit(
			"ExportToExcel",
			details = paste0("format=", out$format, "; path=", path, "; sheets=", length(out$sheets)),
			data = sheets[[1]]
		)
	}

	out
}


#' Create a codebook for a dataset
#'
#' This produces a plain-language table that documents each column in your data.
#' It can optionally use a FormIO schema (via [FieldDictionary()]) to add labels,
#' sections, and descriptions.
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' When a schema is provided, the codebook also includes a `schema_key` column
#' showing which form field key was matched (best-effort). If a column could not
#' be matched to the schema, schema-related columns (type/required/section/etc.)
#' will be `NA`.
#'
#' @param data A data.frame of responses, or a list containing `FlatResponses`
#'   or `submission_data`.
#' @param form Optional form schema, JSON string, or metadata object (see
#'   [FieldDictionary()]). If provided, labels and sections are included.
#' @param include Which components to include when `form` is provided:
#'   `"input"` (default) or `"all"`.
#' @param include_summary Logical. If `TRUE` (default), include basic numeric
#'   summaries (min, max, mean).
#' @param max_levels Maximum number of category levels to list for character or
#'   factor columns.
#' @param quiet Logical. If `FALSE`, prints a short message.
#'
#' @return A data.frame codebook with one row per column.
#'
#' @export
#'
#' @examples
#' df <- data.frame(age = c(10, 12, NA), color = c("red", "blue", "red"))
#' MakeCodebook(df)
MakeCodebook <- function(
		data,
		form = NULL,
		include = c("input", "all"),
		include_summary = TRUE,
		max_levels = 20,
		quiet = FALSE
) {
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

	data <- coerce_single_df(data, arg_name = "data")

	if (any(vapply(data, is.list, logical(1)))) {
		if (!quiet) message("Detected nested columns; converting to text for codebook...")
		data <- coerce_list_columns(data)
	}

	field_info <- NULL
	if (!is.null(form)) {
		include <- match.arg(include)
		field_info <- FieldDictionary(form, include = include, quiet = TRUE)
	}

	out <- build_codebook(
		data = data,
		field_info = field_info,
		include_summary = include_summary,
		max_levels = max_levels
	)

	if (!quiet) {
		message("Codebook created for ", nrow(out), " field(s).")
	}

	if (audit_depth == 1) {
		maybe_write_audit("MakeCodebook", data = data)
	}

	out
}


#' Write a simple audit log entry
#'
#' This writes a small CSV/TSV log of key actions (e.g., downloads, cleaning,
#' exports). It is designed to be easy for non-technical users to open in Excel.
#' If you are using automatic logging, you generally do not need to call this
#' directly; it is used behind the scenes by other FormIOr functions.
#'
#' @param action Short action name (e.g., `"export"`). Can be a vector.
#' @param details Optional details or notes. Can be a character vector or list.
#' @param file Path to the audit log file (default: `"audit_log.csv"`). If `NULL`,
#'   uses the active audit log file when one is running.
#' @param data Optional data.frame (or list with `FlatResponses` / `submission_data`)
#'   used to record row/column counts.
#' @param user Optional user name. Defaults to the system user if available.
#' @param append Logical. If `TRUE` (default), append to the existing file.
#' @param quiet Logical. If `FALSE`, prints a short message.
#'
#' @return The data.frame entry that was written.
#'
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:3)
#' log_path <- tempfile(fileext = ".csv")
#' WriteAuditLog(
#'   "export",
#'   details = "Exported survey data",
#'   data = df,
#'   file = log_path,
#'   quiet = TRUE
#' )
WriteAuditLog <- function(
		action,
		details = NULL,
		file = "audit_log.csv",
		data = NULL,
		user = NULL,
		append = TRUE,
		quiet = FALSE
) {
	if (missing(action) || length(action) == 0) {
		stop("action is required.")
	}

	action <- as.character(action)
	details <- normalize_details(details, length(action))

	if (is.null(file)) {
		state <- get_audit_state()
		file <- state$file %||% "audit_log.csv"
	}

	if (is.null(user) || !nzchar(user)) {
		user <- Sys.info()[["user"]]
		if (is.null(user) || !nzchar(user)) {
			user <- Sys.getenv("USERNAME")
		}
		if (is.null(user) || !nzchar(user)) {
			user <- Sys.getenv("USER")
		}
		if (is.null(user) || !nzchar(user)) {
			user <- NA_character_
		}
	}

	dims <- infer_data_dimensions(data)

	entry <- data.frame(
		timestamp = rep(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), length(action)),
		action = action,
		details = details,
		user = rep(user, length(action)),
		data_rows = rep(dims[["rows"]], length(action)),
		data_cols = rep(dims[["cols"]], length(action)),
		stringsAsFactors = FALSE
	)

	dir_path <- dirname(file)
	if (!dir.exists(dir_path)) {
		dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
	}

	ext <- tolower(tools::file_ext(file))
	sep <- if (ext == "tsv") "\t" else ","

	append_mode <- append && file.exists(file)
	write_col_names <- !append_mode

	utils::write.table(
		entry,
		file = file,
		sep = sep,
		row.names = FALSE,
		col.names = write_col_names,
		append = append_mode,
		na = "",
		quote = TRUE
	)

	if (!quiet) {
		message("Wrote ", nrow(entry), " audit log entr", ifelse(nrow(entry) == 1, "y", "ies"), " to ", file, ".")
	}

	entry
}


# ---- Internal helpers ---------------------------------------------------------

coerce_export_sheets <- function(data, sheet = "Data") {
	if (inherits(data, "data.frame")) {
		sheets <- list(as.data.frame(data, stringsAsFactors = FALSE))
		names(sheets) <- unique_sheet_names(sheet, count = 1)
		return(sheets)
	}

	if (is.list(data) && !inherits(data, "data.frame")) {
		if (!is.null(data$FlatResponses) && inherits(data$FlatResponses, "data.frame")) {
			sheets <- list(as.data.frame(data$FlatResponses, stringsAsFactors = FALSE))
			names(sheets) <- unique_sheet_names(sheet, count = 1)
			return(sheets)
		}

		if (!is.null(data$submission_data) && inherits(data$submission_data, "data.frame")) {
			sheets <- list(as.data.frame(data$submission_data, stringsAsFactors = FALSE))
			names(sheets) <- unique_sheet_names(sheet, count = 1)
			return(sheets)
		}

		if (length(data) > 0 && all(vapply(data, inherits, logical(1), "data.frame"))) {
			sheets <- lapply(data, function(df) as.data.frame(df, stringsAsFactors = FALSE))
			names(sheets) <- unique_sheet_names(names(sheets), count = length(sheets))
			return(sheets)
		}
	}

	stop("data must be a data.frame, a list of data.frames, or a list containing FlatResponses/submission_data.")
}


unique_sheet_names <- function(names_vec, count) {
	if (is.null(names_vec) || length(names_vec) == 0) {
		names_vec <- rep("", count)
	}

	if (length(names_vec) < count) {
		names_vec <- c(names_vec, rep("", count - length(names_vec)))
	}

	clean_names <- vapply(seq_len(count), function(i) {
		name <- names_vec[i]
		if (is.null(name) || !nzchar(name)) {
			name <- paste0("Sheet", i)
		}
		sanitize_sheet_name(name)
	}, character(1))

	out <- character(count)
	for (i in seq_len(count)) {
		base <- clean_names[i]
		candidate <- base
		suffix_id <- 1

		while (candidate %in% out) {
			suffix <- paste0("_", suffix_id)
			max_base <- max(1, 31 - nchar(suffix))
			candidate <- paste0(substr(base, 1, max_base), suffix)
			suffix_id <- suffix_id + 1
		}

		out[i] <- candidate
	}

	out
}


sanitize_sheet_name <- function(name) {
	name <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", name)
	if (nchar(name) > 31) name <- substr(name, 1, 31)
	if (!nzchar(name)) name <- "Sheet"
	name
}


write_delimited_sheets <- function(
		sheets,
		path,
		ext = "csv",
		overwrite = FALSE,
		include_row_names = FALSE
) {
	if (!ext %in% c("csv", "tsv")) {
		stop("ext must be 'csv' or 'tsv'.")
	}

	if (length(sheets) == 1) {
		paths <- path
	} else {
		base <- tools::file_path_sans_ext(path)
		paths <- paste0(base, "_", names(sheets), ".", ext)
	}

	if (!overwrite && any(file.exists(paths))) {
		stop("One or more output files already exist. Set overwrite = TRUE to replace them.")
	}

	sep <- if (ext == "tsv") "\t" else ","
	for (i in seq_along(sheets)) {
		utils::write.table(
			sheets[[i]],
			file = paths[i],
			sep = sep,
			row.names = include_row_names,
			col.names = TRUE,
			na = "",
			quote = TRUE
		)
	}

	list(
		path = paths,
		format = ext,
		sheets = names(sheets),
		rows = vapply(sheets, nrow, integer(1)),
		cols = vapply(sheets, ncol, integer(1))
	)
}


write_excel_sheets <- function(
		sheets,
		path,
		overwrite = FALSE,
		include_row_names = FALSE,
		quiet = FALSE
) {
	if (requireNamespace("writexl", quietly = TRUE)) {
		if (file.exists(path)) {
			if (!overwrite) stop("Output file exists. Set overwrite = TRUE to replace it.")
			file.remove(path)
		}
		writexl::write_xlsx(sheets, path = path)
		format <- "xlsx"
		output_path <- path
	} else if (requireNamespace("openxlsx", quietly = TRUE)) {
		openxlsx::write.xlsx(
			sheets,
			file = path,
			overwrite = overwrite,
			rowNames = include_row_names
		)
		format <- "xlsx"
		output_path <- path
	} else {
		if (!quiet) {
			message("No Excel writer found (install 'writexl' or 'openxlsx'). Writing CSV instead.")
		}
		csv_path <- paste0(tools::file_path_sans_ext(path), ".csv")
		out <- write_delimited_sheets(
			sheets = sheets,
			path = csv_path,
			ext = "csv",
			overwrite = overwrite,
			include_row_names = include_row_names
		)
		return(out)
	}

	list(
		path = output_path,
		format = format,
		sheets = names(sheets),
		rows = vapply(sheets, nrow, integer(1)),
		cols = vapply(sheets, ncol, integer(1))
	)
}


coerce_single_df <- function(data, arg_name = "data") {
	if (inherits(data, "data.frame")) {
		return(as.data.frame(data, stringsAsFactors = FALSE))
	}

	if (is.list(data) && !inherits(data, "data.frame")) {
		if (!is.null(data$FlatResponses) && inherits(data$FlatResponses, "data.frame")) {
			return(as.data.frame(data$FlatResponses, stringsAsFactors = FALSE))
		}

		if (!is.null(data$submission_data) && inherits(data$submission_data, "data.frame")) {
			return(as.data.frame(data$submission_data, stringsAsFactors = FALSE))
		}

		if (!is.null(data$data) && inherits(data$data, "data.frame")) {
			return(as.data.frame(data$data, stringsAsFactors = FALSE))
		}

		if (!is.null(data$flat) && is.list(data$flat) &&
			!is.null(data$flat$FlatResponses) &&
			inherits(data$flat$FlatResponses, "data.frame")) {
			return(as.data.frame(data$flat$FlatResponses, stringsAsFactors = FALSE))
		}
	}

	stop(arg_name, " must be a data.frame or a list containing FlatResponses/submission_data.")
}


coerce_list_columns <- function(df) {
	out <- df
	for (i in seq_along(out)) {
		col <- out[[i]]
		if (is.list(col) && !inherits(col, "POSIXt") && !inherits(col, "Date")) {
			if (inherits(col, "data.frame")) {
				if (nrow(col) == 0) {
					out[[i]] <- character(0)
				} else {
					out[[i]] <- vapply(seq_len(nrow(col)), function(j) {
						list_element_to_text(col[j, , drop = FALSE])
					}, character(1))
				}
			} else {
				out[[i]] <- vapply(col, list_element_to_text, character(1))
			}
		}
	}
	out
}


build_codebook <- function(data, field_info = NULL, include_summary = TRUE, max_levels = 20) {
	col_names <- names(data)

	label <- rep(NA_character_, length(col_names))
	type <- rep(NA_character_, length(col_names))
	required <- rep(NA, length(col_names))
	description <- rep(NA_character_, length(col_names))
	section <- rep(NA_character_, length(col_names))
	options <- rep(NA_character_, length(col_names))
	schema_key <- rep(NA_character_, length(col_names))

	if (!is.null(field_info) && nrow(field_info) > 0) {
		idx <- match_field_info_rows(col_names, field_info)
		# Drop names to avoid data.frame() treating them as row names (and erroring on NA names).
		label <- unname(field_info$label[idx])
		type <- unname(field_info$type[idx])
		required <- unname(field_info$required[idx])
		description <- unname(field_info$description[idx])
		section <- unname(field_info$section[idx])
		options <- unname(vapply(field_info$options[idx], to_text, character(1)))
		schema_key <- unname(field_info$key[idx])
	}

	rows <- vector("list", length(col_names))

	for (i in seq_along(col_names)) {
		col <- data[[i]]
		summary <- summarize_column(col, max_levels = max_levels, include_summary = include_summary)

		name_val <- unname(col_names[i])
		label_val <- unname(ifelse(is.na(label[i]) | !nzchar(label[i]), name_val, label[i]))

			rows[[i]] <- data.frame(
				name = name_val,
				label = label_val,
				schema_key = unname(schema_key[i]),
				type = unname(type[i]),
				class = class(col)[1],
				required = unname(required[i]),
				description = unname(description[i]),
				section = unname(section[i]),
			options = unname(options[i]),
			missing = summary$missing,
			missing_pct = summary$missing_pct,
			unique = summary$unique,
			example = summary$example,
			levels = summary$levels,
			min = summary$min,
			max = summary$max,
			mean = summary$mean,
			stringsAsFactors = FALSE
		)
	}

	do.call(rbind, rows)
}


match_field_info_rows <- function(col_names, field_info) {
	if (is.null(field_info) || nrow(field_info) == 0) {
		return(rep(NA_integer_, length(col_names)))
	}

	keys <- as.character(field_info$key)
	keys[is.na(keys)] <- ""
	valid <- keys != ""
	keys <- keys[valid]
	key_rows <- which(valid)

	if (length(keys) == 0) {
		return(rep(NA_integer_, length(col_names)))
	}

	# 1) exact match
	pos <- match(col_names, keys)

	# 2) case-insensitive exact match
	miss <- is.na(pos)
	if (any(miss)) {
		pos2 <- match(tolower(col_names[miss]), tolower(keys))
		pos[miss] <- pos2
	}

	# 3) fuzzy match (common for flattened columns like "data-firstName")
	miss <- is.na(pos)
	if (any(miss)) {
		col_can <- canon_key(col_names[miss])
		col_last_can <- canon_key(last_segment(col_names[miss]))

		key_can <- canon_key(keys)
		key_len <- nchar(key_can)

		pos3 <- vapply(seq_along(col_can), function(i) {
			cc <- col_can[i]
			cl <- col_last_can[i]
			if (!nzchar(cc)) return(NA_integer_)

			min_len <- 4
			candidates <- which(key_len >= min_len & (endsWith(cc, key_can) | endsWith(cl, key_can)))

			# Fallback: substring match for longer keys (still best-effort).
			if (length(candidates) == 0) {
				min_len2 <- 6
				candidates <- which(key_len >= min_len2 & vapply(key_can, function(k) grepl(k, cc, fixed = TRUE), logical(1)))
			}

			if (length(candidates) == 0) return(NA_integer_)

			# Prefer the longest key to reduce false positives ("name" vs "firstName").
			best <- candidates[which.max(key_len[candidates])]
			best[1]
		}, integer(1))

		pos[miss] <- pos3
	}

	out <- rep(NA_integer_, length(col_names))
	ok <- !is.na(pos)
	out[ok] <- key_rows[pos[ok]]
	out
}


canon_key <- function(x) {
	x <- as.character(x)
	x[is.na(x)] <- ""
	tolower(gsub("[^A-Za-z0-9]+", "", x))
}


last_segment <- function(x) {
	x <- as.character(x)
	x[is.na(x)] <- ""
	parts <- strsplit(x, "[-_\\.]+")
	vapply(parts, function(p) if (length(p) == 0) "" else p[length(p)], character(1))
}


summarize_column <- function(x, max_levels = 20, include_summary = TRUE) {
	n <- length(x)
	missing <- sum(is.na(x))
	non_missing <- x[!is.na(x)]
	unique_count <- length(unique(non_missing))

	example <- to_text(if (length(non_missing) > 0) non_missing[1] else NA)

	level_values <- NULL
	if (is.factor(x)) {
		level_values <- levels(x)
	} else if (is.character(x)) {
		level_values <- unique(non_missing)
	} else if (is.logical(x)) {
		level_values <- c("TRUE", "FALSE")
	}

	levels_text <- NA_character_
	if (!is.null(level_values)) {
		level_values <- as.character(level_values)
		if (length(level_values) > max_levels) {
			levels_text <- paste(
				c(level_values[1:max_levels], paste0("... (+", length(level_values) - max_levels, " more)")),
				collapse = " | "
			)
		} else {
			levels_text <- paste(level_values, collapse = " | ")
		}
	}

	min_val <- NA
	max_val <- NA
	mean_val <- NA

	if (include_summary) {
		if (is.numeric(x)) {
			if (length(non_missing) > 0) {
				min_val <- min(non_missing)
				max_val <- max(non_missing)
				mean_val <- mean(non_missing)
			}
		} else if (inherits(x, "Date") || inherits(x, "POSIXt")) {
			if (length(non_missing) > 0) {
				min_val <- as.character(min(non_missing))
				max_val <- as.character(max(non_missing))
			}
		}
	}

	list(
		missing = missing,
		missing_pct = if (n == 0) NA_real_ else round((missing / n) * 100, 1),
		unique = unique_count,
		example = example,
		levels = levels_text,
		min = min_val,
		max = max_val,
		mean = mean_val
	)
}


to_text <- function(x) {
	if (is.null(x)) return(NA_character_)
	if (length(x) == 0) return(NA_character_)

	if (is.list(x) && !is.data.frame(x)) {
		x <- unlist(x, recursive = TRUE, use.names = FALSE)
	}

	if (length(x) > 1) {
		return(paste(as.character(x), collapse = " | "))
	}

	as.character(x)
}


normalize_details <- function(details, n) {
	if (is.null(details)) {
		return(rep(NA_character_, n))
	}

	if (length(details) == 1) {
		return(rep(to_text(details), n))
	}

	if (length(details) != n) {
		stop("details must be length 1 or the same length as action.")
	}

	vapply(details, to_text, character(1))
}


infer_data_dimensions <- function(data) {
	if (is.null(data)) {
		return(c(rows = NA_integer_, cols = NA_integer_))
	}

	df <- NULL
	if (inherits(data, "data.frame")) {
		df <- data
	} else if (is.list(data) && !inherits(data, "data.frame")) {
		if (!is.null(data$FlatResponses) && inherits(data$FlatResponses, "data.frame")) {
			df <- data$FlatResponses
		} else if (!is.null(data$submission_data) && inherits(data$submission_data, "data.frame")) {
			df <- data$submission_data
		}
	}

	if (is.null(df)) {
		return(c(rows = NA_integer_, cols = NA_integer_))
	}

	c(rows = nrow(df), cols = ncol(df))
}
