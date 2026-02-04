#' Describe a form in plain language
#'
#' This is a simple overview of a form: the name, version, and how many fields
#' it contains. It is meant for non-technical users who want a quick summary.
#'
#' What you can pass to `form`:
#' - A **schema list** (from a schema API response)
#' - A **JSON string**
#' - A **path to a JSON file**
#' - The output of [GetFormMetadata()]
#'
#' Important: [GetFormMetadata()] returns **metadata only**, not the full form.
#' If you pass metadata, this function will automatically fetch the form schema
#' using stored credentials (from [AskCredentials()] or `GetFormMetadata()`).
#' If credentials are not available, it will stop with a clear message.
#'
#' Tip: Use `include = "input"` (default) to count only the fields people fill in,
#' or `include = "all"` to include layout items like panels and tabs.
#'
#' @param form Form schema list, JSON string, or path to a JSON file.
#' @param include_fields Logical. If `TRUE`, include a field dictionary in the
#'   output (from [FieldDictionary()]).
#' @param version Which form version to use when `form` is metadata (from
#'   [GetFormMetadata()]). Use `"latest"` (default), a version number (e.g., `3`),
#'   or a version ID.
#' @param include Which components to count for fields. Defaults to input fields
#'   only (`"input"`). Use `"all"` to include layout components too.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{meta}{Form details like title, name, ID, and version}
#'   \item{counts}{How many components, fields, and sections}
#'   \item{fields}{Field dictionary data frame (only if `include_fields = TRUE`)}
#' }
#'
#' @export
#'
#' @examples
#' form <- list(
#'   title = "Sample Form",
#'   name = "sample_form",
#'   version = 2,
#'   components = list(
#'     list(type = "textfield", key = "first_name", label = "First name", input = TRUE),
#'     list(type = "panel", title = "Details", components = list(
#'       list(type = "number", key = "age", label = "Age", input = TRUE)
#'     ))
#'   )
#' )
#' DescribeForm(form)
#'
#' \dontrun{
#' meta <- GetFormMetadata(form_id = "123", api_key = "abc")
#' DescribeForm(meta)
#' }
DescribeForm <- function(
		form,
		include_fields = FALSE,
		version = "latest",
		include = c("input", "all"),
		quiet = FALSE
) {
	include <- match.arg(include)
	form <- as_form_schema(form, version = version)

	meta <- list(
		title = form$title %||% form$name %||% form$path %||% NA_character_,
		name = form$name %||% NA_character_,
		path = form$path %||% NA_character_,
		id = form[["_id"]] %||% form$id %||% NA_character_,
		version = form$version %||% NA_character_,
		created = form$created %||% form$createdAt %||% NA_character_,
		modified = form$modified %||% form$modifiedAt %||% NA_character_,
		status = form$status %||% NA_character_
	)

	components <- form$components
	all_components <- flatten_components(components, include = "all")
	field_components <- flatten_components(components, include = include)

	fields_df <- FieldDictionary(form, include = include, quiet = TRUE, version = version)
	sections <- unique(stats::na.omit(fields_df$section))

	counts <- list(
		components = nrow(all_components),
		fields = nrow(field_components),
		sections = length(sections)
	)

	out <- list(
		meta = meta,
		counts = counts
	)

	if (include_fields) {
		out$fields <- fields_df
	}

	if (!quiet) {
		form_name <- meta$title %||% meta$name %||% "(unnamed form)"
		message("Form ", form_name, " has ", counts$fields, " field(s) across ", counts$sections, " section(s).")
	}

	out
}


#' Build a field dictionary for a form
#'
#' This creates a clean table that lists each field in your form. It is meant
#' to be easy to read and share with non-technical staff.
#'
#' If you pass the output of [GetFormMetadata()], the schema is **not** included
#' in that object. In that case the function will automatically fetch the schema
#' using stored credentials (from [AskCredentials()] or `GetFormMetadata()`).
#' If credentials are not available, it will stop with a clear message.
#'
#' Tip: Use `include = "all"` to include layout components (panels, tabs,
#' fieldsets). These usually have `input = FALSE` and no field key.
#'
#' @param form Form schema list, JSON string, or path to a JSON file.
#' @param include Which components to include: `"input"` (default) or `"all"`.
#' @param version Which form version to use when `form` is metadata (from
#'   [GetFormMetadata()]). Use `"latest"` (default), a version number (e.g., `3`),
#'   or a version ID.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A data.frame with columns such as:
#' - `key`: field key (may be `NA` for layout components)
#' - `label`: field label or title
#' - `type`: component type (text, number, panel, etc.)
#' - `required`: whether the field is required
#' - `description`: help text/tooltip
#' - `default`: default value
#' - `options`: choice labels (for select/radio)
#' - `section`: nearest section/panel title
#' - `path`: location within the form
#' - `input`: `TRUE` for input fields, `FALSE` for layout
#'
#' @export
#'
#' @examples
#' form <- list(
#'   title = "Sample Form",
#'   components = list(
#'     list(type = "textfield", key = "first_name", label = "First name", input = TRUE),
#'     list(type = "select", key = "color", label = "Favorite color", input = TRUE,
#'          data = list(values = list(list(label = "Red", value = "red"))))
#'   )
#' )
#' FieldDictionary(form)
#'
#' \dontrun{
#' meta <- GetFormMetadata(form_id = "123", api_key = "abc")
#' FieldDictionary(meta, include = "all")
#' }
FieldDictionary <- function(
		form,
		include = c("input", "all"),
		version = "latest",
		quiet = FALSE
) {
	include <- match.arg(include)
	form <- as_form_schema(form, version = version)

	components <- form$components
	rows <- flatten_components(components, include = include)

	if (nrow(rows) == 0) {
		if (!quiet) message("No components found in form schema.")
		return(rows)
	}

	rows <- rows[, c(
		"key",
		"label",
		"type",
		"required",
		"description",
		"default",
		"options",
		"section",
		"path",
		"input"
	), drop = FALSE]

	if (!quiet) {
		message("Found ", nrow(rows), " field(s).")
	}

	rows
}


#' Compare two versions of a form
#'
#' Shows what changed between two form versions:
#' fields added, removed, or updated.
#'
#' If you pass the output of [GetFormMetadata()], the schema is **not** included
#' in that object. In that case the function will automatically fetch the schema
#' using stored credentials (from [AskCredentials()] or `GetFormMetadata()`). If
#' credentials are not available, it will stop with a clear message.
#'
#' Tip: Use `by = "key"` for stable field keys, or `by = "path"` when keys are
#' reused in repeating sections.
#'
#' @param old Form schema list, JSON string, or path to a JSON file (older version).
#' @param new Form schema list, JSON string, or path to a JSON file (newer version).
#' @param by How to match fields across versions: `"key"` (default) or `"path"`.
#' @param include Which components to include: `"input"` (default) or `"all"`.
#' @param old_version Which form version to use when `old` is metadata (from
#'   [GetFormMetadata()]). Use `"latest"` (default), a version number, or a
#'   version ID.
#' @param new_version Which form version to use when `new` is metadata (from
#'   [GetFormMetadata()]). Use `"latest"` (default), a version number, or a
#'   version ID.
#' @param compare_cols Character vector of columns to compare for changes.
#' @param include_unchanged Logical. If `TRUE`, include unchanged fields in the output.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{summary}{Counts of added, removed, changed, and unchanged fields}
#'   \item{added}{Fields only in the new version}
#'   \item{removed}{Fields only in the old version}
#'   \item{changed}{Fields that changed (with before/after values)}
#'   \item{unchanged}{Unchanged fields (if `include_unchanged = TRUE`)}
#' }
#'
#' @export
#'
#' @examples
#' old <- list(components = list(
#'   list(type = "textfield", key = "name", label = "Name", input = TRUE)
#' ))
#' new <- list(components = list(
#'   list(type = "textfield", key = "name", label = "Full name", input = TRUE),
#'   list(type = "number", key = "age", label = "Age", input = TRUE)
#' ))
#' CompareFormVersions(old, new)
#'
#' \dontrun{
#' old_meta <- GetFormMetadata(form_id = "123", api_key = "abc")
#' new_meta <- GetFormMetadata(form_id = "123", api_key = "abc")
#' CompareFormVersions(old_meta, new_meta)
#' }
CompareFormVersions <- function(
		old,
		new,
		by = c("key", "path"),
		include = c("input", "all"),
		old_version = "latest",
		new_version = "latest",
		compare_cols = c("label", "type", "required", "description", "default", "options", "section"),
		include_unchanged = FALSE,
		quiet = FALSE
) {
	by <- match.arg(by)
	include <- match.arg(include)

	old_dict <- FieldDictionary(old, include = include, quiet = TRUE, version = old_version)
	new_dict <- FieldDictionary(new, include = include, quiet = TRUE, version = new_version)

	if (!by %in% names(old_dict) || !by %in% names(new_dict)) {
		stop("Comparison column '", by, "' not found in field dictionaries.")
	}

	old_dict <- old_dict[!is.na(old_dict[[by]]) & old_dict[[by]] != "", , drop = FALSE]
	new_dict <- new_dict[!is.na(new_dict[[by]]) & new_dict[[by]] != "", , drop = FALSE]

	if (any(duplicated(old_dict[[by]]))) {
		warning("Duplicate keys found in old form; using first occurrence.")
		old_dict <- old_dict[!duplicated(old_dict[[by]]), , drop = FALSE]
	}
	if (any(duplicated(new_dict[[by]]))) {
		warning("Duplicate keys found in new form; using first occurrence.")
		new_dict <- new_dict[!duplicated(new_dict[[by]]), , drop = FALSE]
	}

	old_ids <- old_dict[[by]]
	new_ids <- new_dict[[by]]

	added <- new_dict[!new_ids %in% old_ids, , drop = FALSE]
	removed <- old_dict[!old_ids %in% new_ids, , drop = FALSE]

	common_ids <- intersect(old_ids, new_ids)
	idx_old <- match(common_ids, old_ids)
	idx_new <- match(common_ids, new_ids)

	compare_cols <- intersect(compare_cols, intersect(names(old_dict), names(new_dict)))

	changed_rows <- list()
	unchanged_rows <- list()

	for (i in seq_along(common_ids)) {
		old_row <- old_dict[idx_old[i], , drop = FALSE]
		new_row <- new_dict[idx_new[i], , drop = FALSE]

		diff_cols <- compare_cols[vapply(compare_cols, function(col) {
			!values_equal(old_row[[col]], new_row[[col]])
		}, logical(1))]

		if (length(diff_cols) > 0) {
			row <- list()
			row[[by]] <- common_ids[i]
			row$changes <- paste(diff_cols, collapse = ", ")
			for (col in compare_cols) {
				row[[paste0(col, "_old")]] <- old_row[[col]]
				row[[paste0(col, "_new")]] <- new_row[[col]]
			}
			changed_rows[[length(changed_rows) + 1]] <- row
		} else if (include_unchanged) {
			unchanged_rows[[length(unchanged_rows) + 1]] <- new_row
		}
	}

	changed <- bind_rows_safe(changed_rows)
	unchanged <- bind_rows_safe(unchanged_rows)

	summary <- data.frame(
		metric = c("added", "removed", "changed", "unchanged"),
		count = c(nrow(added), nrow(removed), nrow(changed), length(common_ids) - nrow(changed)),
		stringsAsFactors = FALSE
	)

	out <- list(
		by = by,
		summary = summary,
		added = added,
		removed = removed,
		changed = changed
	)

	if (include_unchanged) {
		out$unchanged <- unchanged
	}

	if (!quiet) {
		message("Added: ", nrow(added), ", removed: ", nrow(removed), ", changed: ", nrow(changed), ".")
	}

	out
}


# ---- Internal helpers -----------------------------------------------------------

as_form_schema <- function(form, version = "latest") {
	if (is.null(form)) stop("form cannot be NULL.")

	if (is.character(form) && length(form) == 1) {
		if (file.exists(form)) {
			json <- paste(readLines(form, warn = FALSE), collapse = "\n")
			form <- jsonlite::fromJSON(json, simplifyVector = FALSE)
		} else if (grepl("^\\s*\\{", form)) {
			form <- jsonlite::fromJSON(form, simplifyVector = FALSE)
		} else {
			stop("form must be a list, JSON string, or file path.")
		}
	}

	if (!is.list(form)) stop("form must be a list, JSON string, or file path.")

	if (has_components_field(form)) return(form)
	if (is.list(form$form) && has_components_field(form$form)) return(form$form)
	if (is.list(form$schema) && has_components_field(form$schema)) return(form$schema)

	found <- find_schema_root(form, require_schema_like = TRUE)
	if (is.null(found)) found <- find_schema_root(form, require_schema_like = FALSE)
	if (!is.null(found)) return(found)

	fetched <- try_fetch_schema(form, version = version)
	if (!is.null(fetched)) return(fetched)

	stop("Could not find a form schema with components. If you passed GetFormMetadata() output, make sure credentials are available so the schema can be fetched automatically.")
}


flatten_components <- function(components, include = c("input", "all")) {
	include <- match.arg(include)
	components <- normalize_list(components)
	rows <- collect_components(components, parent_path = NULL, section = NULL, include = include)

	if (length(rows) == 0) {
		return(data.frame(
			key = character(0),
			label = character(0),
			type = character(0),
			required = logical(0),
			description = character(0),
			default = character(0),
			options = character(0),
			section = character(0),
			path = character(0),
			input = logical(0),
			stringsAsFactors = FALSE
		))
	}

	df <- do.call(rbind, lapply(rows, function(x) {
		as.data.frame(x, stringsAsFactors = FALSE)
	}))

	rownames(df) <- NULL
	df
}


collect_components <- function(components, parent_path = NULL, section = NULL, include = "input") {
	rows <- list()
	components <- normalize_list(components)
	if (is.null(components) || length(components) == 0) return(rows)

	for (i in seq_along(components)) {
		comp <- components[[i]]
		if (!is.list(comp)) next

		comp_type <- comp$type %||% NA_character_
		key <- comp$key %||% NA_character_
		label <- first_non_empty(comp$label, comp$title, comp$legend, comp$caption, key, comp_type, paste0("component_", i))

		input_flag <- detect_input_flag(comp)
		path_key <- build_path_key(key, label, comp_type, i)
		path <- build_path(parent_path, path_key)

		required_raw <- comp$validate$required %||% comp$required
		required <- if (is.null(required_raw)) NA else isTRUE(required_raw)

		description <- comp$description %||% comp$tooltip %||% comp$help
		default <- comp$defaultValue %||% comp$default
		options <- extract_options(comp)

		row <- list(
			key = if (is.null(key) || is.na(key)) NA_character_ else as.character(key),
			label = as.character(label),
			type = if (is.null(comp_type) || is.na(comp_type)) NA_character_ else as.character(comp_type),
			required = required,
			description = to_character(description),
			default = to_character(default),
			options = to_character(options),
			section = if (is.null(section)) NA_character_ else as.character(section),
			path = if (is.null(path)) NA_character_ else as.character(path),
			input = input_flag
		)

		if (include == "all" || (include == "input" && isTRUE(input_flag))) {
			rows[[length(rows) + 1]] <- row
		}

		section_next <- section
		if (is_section_type(comp_type)) {
			section_next <- label
		}

		child_components <- extract_child_components(comp)
		if (length(child_components) > 0) {
			rows <- c(rows, collect_components(child_components, parent_path = path, section = section_next, include = include))
		}
	}

	rows
}


extract_child_components <- function(comp) {
	children <- list()

	if (!is.null(comp$components)) {
		children <- c(children, normalize_list(comp$components))
	}
	if (!is.null(comp$columns)) {
		cols <- normalize_list(comp$columns)
		for (col in cols) {
			if (is.list(col) && !is.null(col$components)) {
				children <- c(children, normalize_list(col$components))
			}
		}
	}
	if (!is.null(comp$rows)) {
		rows <- normalize_list(comp$rows)
		for (row in rows) {
			if (!is.list(row)) next
			for (col in row) {
				if (is.list(col) && !is.null(col$components)) {
					children <- c(children, normalize_list(col$components))
				}
			}
		}
	}
	if (!is.null(comp$tabs)) {
		tabs <- normalize_list(comp$tabs)
		for (tab in tabs) {
			if (is.list(tab) && !is.null(tab$components)) {
				children <- c(children, normalize_list(tab$components))
			}
		}
	}

	children
}


extract_options <- function(comp) {
	values <- NULL
	if (is.list(comp$data) && !is.null(comp$data$values)) {
		values <- comp$data$values
	} else if (!is.null(comp$values)) {
		values <- comp$values
	}

	if (is.null(values)) return(NULL)

	if (is.data.frame(values)) {
		values <- split(values, seq_len(nrow(values)))
	}

	if (is.list(values) && length(values) > 0 && is.list(values[[1]])) {
		labels <- vapply(values, function(v) {
			lab <- v$label %||% v$value %||% ""
			val <- v$value %||% v$label %||% ""
			if (identical(lab, val) || val == "") {
				as.character(lab)
			} else {
				paste0(lab, " (", val, ")")
			}
		}, character(1))
		return(paste(labels, collapse = "; "))
	}

	if (is.atomic(values)) {
		return(paste(as.character(values), collapse = "; "))
	}

	jsonlite::toJSON(values, auto_unbox = TRUE)
}


to_character <- function(x) {
	if (is.null(x)) return(NA_character_)
	if (is.atomic(x)) return(paste(as.character(x), collapse = "; "))
	jsonlite::toJSON(x, auto_unbox = TRUE)
}


normalize_list <- function(x) {
	if (is.null(x)) return(list())
	if (is.data.frame(x)) {
		if (nrow(x) == 0) return(list())
		rows <- split(x, seq_len(nrow(x)))
		return(lapply(rows, normalize_row))
	}
	if (is.list(x)) return(x)
	list()
}


normalize_row <- function(row_df) {
	row <- as.list(row_df)
	lapply(row, function(cell) {
		if (is.list(cell) && length(cell) == 1 && !is.data.frame(cell)) {
			cell[[1]]
		} else {
			cell
		}
	})
}


has_components_field <- function(x) {
	is.list(x) && !is.null(x$components) && (is.list(x$components) || is.data.frame(x$components))
}


is_schema_root_candidate <- function(x) {
	if (!has_components_field(x)) return(FALSE)

	type <- tolower(x$type %||% "")
	allowed_types <- c("", "form", "wizard", "survey", "resource", "container")
	if (!(type %in% allowed_types)) return(FALSE)
	if (type != "") return(TRUE)

	key <- x$key %||% ""
	if (nzchar(key)) return(FALSE)

	TRUE
}


components_count <- function(components) {
	if (is.null(components)) return(0L)
	if (is.data.frame(components)) return(nrow(components))
	if (is.list(components)) return(length(components))
	0L
}


collect_schema_candidates <- function(x, require_schema_like = TRUE, out = NULL) {
	if (!is.list(x) || is.data.frame(x)) return(out)

	if (require_schema_like) {
		if (is_schema_root_candidate(x)) out <- c(out, list(x))
	} else if (has_components_field(x)) {
		out <- c(out, list(x))
	}

	for (child in x) {
		if (is.list(child)) {
			out <- collect_schema_candidates(child, require_schema_like = require_schema_like, out = out)
		}
	}

	out
}


find_schema_root <- function(x, require_schema_like = TRUE) {
	candidates <- collect_schema_candidates(x, require_schema_like = require_schema_like, out = NULL)
	if (length(candidates) == 0) return(NULL)

	scores <- vapply(candidates, function(obj) {
		components_count(obj$components)
	}, numeric(1))

	candidates[[which.max(scores)]]
}


get_base_url <- function() {
	getOption("FormIOr.base_url", "https://submit.digital.gov.bc.ca/app/api/v1")
}


resolve_form_id <- function(form) {
	if (!is.list(form)) return(NULL)
	candidates <- c(
		form$formId %||% NULL,
		form$id %||% NULL,
		form[["_id"]] %||% NULL
	)

	if (is.data.frame(form$versions) && "formId" %in% names(form$versions)) {
		row <- select_version_row(form$versions)
		candidates <- c(candidates, row$formId %||% NULL)
	}

	candidates <- candidates[!is.na(candidates) & candidates != ""]
	if (length(candidates) == 0) return(NULL)
	candidates[1]
}


resolve_version_id <- function(form, version = "latest") {
	if (!is.data.frame(form$versions) || !"id" %in% names(form$versions)) return(NULL)

	versions <- form$versions
	if (!is.null(version) && !identical(version, "latest")) {
		version_chr <- as.character(version)[1]

		if ("id" %in% names(versions) && version_chr %in% versions$id) {
			return(versions$id[match(version_chr, versions$id)])
		}

		if ("version" %in% names(versions)) {
			ver_vals <- versions$version
			if (is.numeric(version)) {
				idx <- which(suppressWarnings(as.numeric(ver_vals)) == as.numeric(version))
			} else {
				idx <- which(as.character(ver_vals) == version_chr)
			}
			if (length(idx) > 0) return(versions$id[idx[1]])
		}

		available <- if ("version" %in% names(versions)) unique(versions$version) else versions$id
		stop("Version '", version, "' not found. Available versions: ", paste(available, collapse = ", "))
	}

	row <- select_version_row(versions)
	row$id %||% NULL
}


resolve_credentials <- function(form_id = NULL, api_key = NULL, reenter.credentials = FALSE) {
	Form_Info <- NULL

	if (reenter.credentials) {
		Form_Info <- AskCredentials()
	} else if (exists(".Form_Info")) {
		Form_Info <- .Form_Info
	} else if (interactive()) {
		Form_Info <- AskCredentials()
	}

	if (is.null(form_id) && !is.null(Form_Info)) form_id <- Form_Info[1]
	if (is.null(api_key) && !is.null(Form_Info)) api_key <- Form_Info[2]

	if (is.null(form_id) || is.null(api_key) || form_id == "" || api_key == "") {
		stop("Form ID or API key not available. Run AskCredentials() or pass a schema list instead.")
	}

	.Form_Info <<- Form_Info
	list(form_id = form_id, api_key = api_key)
}


try_fetch_schema <- function(form, version = "latest") {
	form_id <- resolve_form_id(form)
	if (is.null(form_id)) return(NULL)

	creds <- tryCatch(resolve_credentials(form_id = form_id), error = function(e) NULL)
	if (is.null(creds)) return(NULL)

	version_id <- resolve_version_id(form, version = version)
	base_url <- get_base_url()
	fetch_form_schema(base_url, creds$form_id, creds$api_key, version_id = version_id)
}


fetch_form_schema <- function(base_url, form_id, api_key, version_id = NULL) {
	paths <- c("/schema", "/form", "/definition", "", "/draft", "/live")

	for (path in paths) {
		url <- paste0(base_url, "/forms/", form_id, path)
		schema <- try_fetch_schema_url(url, form_id, api_key)
		if (!is.null(schema)) return(schema)
	}

	if (!is.null(version_id) && nzchar(version_id)) {
		version_paths <- c(
			paste0(base_url, "/forms/", form_id, "/versions/", version_id, "/schema"),
			paste0(base_url, "/forms/", form_id, "/versions/", version_id, "/form"),
			paste0(base_url, "/forms/", form_id, "/versions/", version_id)
		)
		for (url in version_paths) {
			schema <- try_fetch_schema_url(url, form_id, api_key)
			if (!is.null(schema)) return(schema)
		}
	}

	NULL
}


try_fetch_schema_url <- function(url, form_id, api_key) {
	resp <- tryCatch(
		GET(
			url = url,
			authenticate(user = form_id, password = api_key, type = "basic"),
			add_headers(Accept = "application/json"),
			timeout(30)
		),
		error = function(e) NULL
	)

	if (is.null(resp)) return(NULL)
	if (status_code(resp) < 200 || status_code(resp) >= 300) return(NULL)

	text <- content(resp, as = "text", encoding = "UTF-8")
	if (!nzchar(text)) return(NULL)

	parsed <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
	if (is.null(parsed)) return(NULL)

	schema <- find_schema_root(parsed, require_schema_like = TRUE)
	if (is.null(schema)) schema <- find_schema_root(parsed, require_schema_like = FALSE)
	schema
}


select_version_row <- function(versions) {
	if (!is.data.frame(versions) || nrow(versions) == 0) return(list())

	df <- versions
	if ("published" %in% names(df)) {
		pub <- df$published
		if (is.character(pub)) pub <- tolower(pub) %in% c("true", "t", "1", "yes", "y")
		if (is.logical(pub) && any(pub, na.rm = TRUE)) {
			df <- df[pub %in% TRUE, , drop = FALSE]
		}
	}

	if ("version" %in% names(df)) {
		ver_num <- suppressWarnings(as.numeric(df$version))
		if (any(!is.na(ver_num))) {
			return(df[which.max(ver_num), , drop = FALSE])
		}
	}

	for (col in c("updatedAt", "createdAt")) {
		if (col %in% names(df)) {
			times <- suppressWarnings(as.POSIXct(df[[col]]))
			if (any(!is.na(times))) {
				return(df[which.max(times), , drop = FALSE])
			}
		}
	}

	df[1, , drop = FALSE]
}


build_path_key <- function(key, label, comp_type, index) {
	if (!is.null(key) && !is.na(key) && nzchar(as.character(key))) {
		return(as.character(key))
	}

	base <- first_non_empty(label, comp_type, paste0("component_", index))
	base <- gsub("[^a-zA-Z0-9]+", "_", base)
	base <- gsub("^_+|_+$", "", base)
	if (is.na(base) || base == "") base <- paste0("component_", index)

	paste0(base, "_", index)
}


build_path <- function(parent_path, key) {
	if (is.null(parent_path) || parent_path == "") return(key)
	paste(parent_path, key, sep = ".")
}


is_section_type <- function(type) {
	if (is.null(type) || is.na(type)) return(FALSE)
	type %in% c("panel", "fieldset", "well", "tabs", "tab")
}


detect_input_flag <- function(comp) {
	if (!is.null(comp$input)) return(isTRUE(comp$input))
	type <- comp$type %||% ""
	if (!is.null(comp$key) && nzchar(comp$key) && !is_layout_type(type)) {
		return(TRUE)
	}
	FALSE
}


is_layout_type <- function(type) {
	if (is.null(type) || is.na(type)) return(TRUE)
	type %in% c("panel", "fieldset", "well", "tabs", "tab", "columns", "column", "table", "content", "html", "button")
}


values_equal <- function(x, y) {
	x_val <- if (length(x) == 0) NA_character_ else x
	y_val <- if (length(y) == 0) NA_character_ else y

	x_char <- to_character(x_val)
	y_char <- to_character(y_val)

	if (is.na(x_char) && is.na(y_char)) return(TRUE)
	normalize <- function(z) {
		z <- trimws(z)
		if (z == "") NA_character_ else z
	}

	x_char <- normalize(x_char)
	y_char <- normalize(y_char)

	if (is.na(x_char) && is.na(y_char)) return(TRUE)
	identical(x_char, y_char)
}


bind_rows_safe <- function(rows) {
	if (length(rows) == 0) {
		return(data.frame(stringsAsFactors = FALSE))
	}
	df <- do.call(rbind, lapply(rows, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
	rownames(df) <- NULL
	df
}


first_non_empty <- function(...) {
	values <- list(...)
	for (val in values) {
		if (is.null(val)) next
		if (length(val) == 0) next
		if (is.list(val) && length(val) == 1 && !is.data.frame(val)) {
			val <- val[[1]]
		}
		val_chr <- as.character(val)[1]
		if (is.na(val_chr)) next
		if (!nzchar(trimws(val_chr))) next
		return(val_chr)
	}
	NA_character_
}
