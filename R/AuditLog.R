#' Start an audit log for FormIOr actions
#'
#' Creates a new audit log file and turns on automatic logging for
#' FormIOr functions. Each subsequent action (downloads, cleaning,
#' exports, etc.) will append a new row to the log.
#'
#' The audit log is designed to be "Excel friendly": it is a simple CSV/TSV
#' with one row per action, including a timestamp, an action name, optional
#' details, and (when available) row/column counts for the dataset being
#' processed.
#'
#' If you do *not* start a log, many FormIOr functions will (once per R session)
#' ask whether you want to begin logging. Starting a log here avoids that prompt
#' and gives you control over the file location.
#'
#' @param file Path to the audit log file (default: `"audit_log.csv"`).
#' @param overwrite Logical. If `FALSE` (default), stop if the file exists.
#' @param append Logical. If `TRUE`, append to an existing log instead of
#'   erroring. Useful when resuming a previous session.
#' @param quiet Logical. If `FALSE`, prints a short message.
#'
#' @return Invisibly returns the first log entry (a data.frame row).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' log_file <- tempfile(fileext = ".csv")
#' StartAuditLog(log_file, overwrite = TRUE)
#'
#' # Run some FormIOr steps (each will append a row when possible)
#' flat <- FlattenSubmissions(FoodTypes)
#' norm <- NormalizeColumnNames(flat, quiet = TRUE)
#'
#' StopAuditLog()
#' read.csv(log_file, stringsAsFactors = FALSE)
#' }
StartAuditLog <- function(file = "audit_log.csv", overwrite = FALSE, append = FALSE, quiet = FALSE) {
	file_exists <- file.exists(file)
	if (file_exists) {
		if (isTRUE(overwrite)) {
			file.remove(file)
			file_exists <- FALSE
		} else if (!isTRUE(append)) {
			stop("Audit log already exists. Set overwrite = TRUE to replace it, or append = TRUE to continue it.")
		}
	}

	entry <- WriteAuditLog(
		action = "log_started",
		details = if (isTRUE(append) && file_exists) "Audit log resumed" else "Audit log started",
		file = file,
		append = isTRUE(append) && file_exists,
		quiet = TRUE
	)

	state <- get_audit_state()
	state$active <- TRUE
	state$file <- file
	state$prompted <- TRUE
	set_audit_state(state)

	if (!quiet) {
		message("Audit log started: ", file)
	}

	invisible(entry)
}


#' Stop audit logging for FormIOr
#'
#' Turns off automatic logging. The log file is not deleted.
#' You can start a new log later using [StartAuditLog()].
#'
#' @param quiet Logical. If `FALSE`, prints a short message.
#'
#' @return Invisibly returns `TRUE` when logging was active, `FALSE` otherwise.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' StopAuditLog()
#' }
StopAuditLog <- function(quiet = FALSE) {
	state <- get_audit_state()
	was_active <- isTRUE(state$active)

	state$active <- FALSE
	set_audit_state(state)

	if (!quiet) {
		if (was_active) {
			message("Audit logging paused.")
		} else {
			message("Audit logging was already off.")
		}
	}

	invisible(was_active)
}


#' Check whether audit logging is active
#'
#' This is most useful in scripts when you want to conditionally add a manual
#' note using [WriteAuditLog()] only when logging is enabled.
#'
#' @return `TRUE` if an audit log is active, otherwise `FALSE`.
#'
#' @export
#'
#' @examples
#' IsAuditLogActive()
IsAuditLogActive <- function() {
	state <- get_audit_state()
	isTRUE(state$active)
}


# ---- Internal helpers ---------------------------------------------------------

get_audit_state <- function() {
	state <- getOption("formior.audit")
	if (!is.list(state)) state <- list()

	if (is.null(state$active)) state$active <- FALSE
	if (is.null(state$file)) state$file <- NULL
	if (is.null(state$prompted)) state$prompted <- FALSE
	if (is.null(state$depth)) state$depth <- 0L

	state
}


set_audit_state <- function(state) {
	options(formior.audit = state)
	invisible(state)
}


audit_enter <- function() {
	state <- get_audit_state()
	state$depth <- state$depth + 1L
	set_audit_state(state)
	state$depth
}


audit_exit <- function() {
	state <- get_audit_state()
	state$depth <- max(0L, state$depth - 1L)
	set_audit_state(state)
	invisible(state$depth)
}


maybe_prompt_audit_log <- function() {
	state <- get_audit_state()
	if (isTRUE(state$active) || isTRUE(state$prompted) || !interactive()) {
		return(invisible(FALSE))
	}

	answer <- readline("No active audit log. Start one now? [y/N]: ")
	state$prompted <- TRUE
	set_audit_state(state)
	if (tolower(answer) %in% c("y", "yes")) {
		log_file <- state$file %||% "audit_log.csv"
		append_mode <- file.exists(log_file)
		StartAuditLog(file = log_file, overwrite = FALSE, append = append_mode, quiet = TRUE)
		return(invisible(TRUE))
	}

	invisible(FALSE)
}


maybe_write_audit <- function(action, details = NULL, data = NULL) {
	state <- get_audit_state()
	if (!isTRUE(state$active) || is.null(state$file)) {
		return(invisible(NULL))
	}

	WriteAuditLog(
		action = action,
		details = details,
		file = state$file,
		data = data,
		append = TRUE,
		quiet = TRUE
	)
}
