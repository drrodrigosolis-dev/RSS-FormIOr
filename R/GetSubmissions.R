#' Retrieve metadata for submissions of a specific form
#'
#' Fetches a list of submission metadata (not the full submission data) from a
#' Digital.gov.bc.ca form, including drafts, completed, and deleted submissions.
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' @section CHEF credentials and base URL:
#' FormIOr is designed for the BC Public Service CHEF FormIO service.
#' The default base URL is `https://submit.digital.gov.bc.ca/app/api/v1`.
#' If you use a different FormIO service, you must override `base_url` and
#' compatibility is not guaranteed.
#'
#' For CHEF users, generate your API key from the form's **Manage** page.
#' The Form ID is the final alphanumeric code after the `=` sign in the
#' Manage page URL.
#'
#' @param base_url Character. API base URL. Default: "https://submit.digital.gov.bc.ca/app/api/v1"
#' @param form_id Character. Form identifier. If NULL, read from stored credentials.
#' @param api_key Character. API key / secret. If NULL, read from stored credentials.
#' @param content.only Logical or character.
#'   - `TRUE` (default): return cleaned data.frame of submission metadata
#'   - `FALSE`: return full response object (status, headers, content)
#'   - `"raw"`: return raw JSON response as text
#' @param reenter.credentials Logical. Force re-entry of credentials (default: FALSE)
#' @param AdditionalCols Character vector. Extra field names to request (passed to `fields` query param)
#'
#' @return
#' - Default (`content.only = TRUE`): data.frame containing submission metadata
#'   (formId, submissionId, createdAt, version, confirmationId, status, assigned user, etc.)
#' - `content.only = FALSE`: list with status, headers, and parsed content
#' - `content.only = "raw"`: raw JSON string
#'
#' @export
#'
#'
GetSubmissions <- function(
		base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
		form_id = NULL,
		api_key = NULL,
		content.only = TRUE,
		reenter.credentials = FALSE,
	AdditionalCols = c("")
) {
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

	creds <- resolve_credentials(
		form_id = form_id,
		api_key = api_key,
		reenter.credentials = reenter.credentials
	)
	form_id <- creds$form_id
	api_key <- creds$api_key
	export_url <- paste0(base_url, "/forms/", form_id, "/submissions")

	query_params <- list(
		fields= AdditionalCols
	)

	resp <- GET(
		url = export_url, query = query_params, authenticate(
			user = form_id,
			password = api_key, type = "basic"
		), add_headers(Accept = "application/json"),
		timeout(60)
	)
	submission_data <- fromJSON(content(resp, as = "text"))
	full_submission_data <- list(
		status = status_code(resp),
		content_type = headers(resp)[["content-type"]], content_disposition = headers(resp)[["content-disposition"]],
		submission_data = submission_data
	)

	versions<-GetFormMetadata()$versions %>% select("formId", "version", "id", "published")
	submission_data$version<-versions$version[match(submission_data$formVersionId, versions$id)]

	submission_data <- submission_data %>% select(
		"formId",
		"submissionId",
		"createdBy",
		"createdAt",
		"version",
		"formVersionId",
		"confirmationId",
		"formSubmissionStatusCode",
		"deleted",
		"formSubmissionAssignedToUserId",
		"formSubmissionAssignedToUsernameIdp", "formSubmissionAssignedToEmail",
		"lateEntry"
	)

	out <- NULL
	if (isFALSE(content.only)) {
		out <- full_submission_data
	}
	if (isTRUE(content.only)) {
		out <- submission_data
	}
	if (content.only == "raw") {
		out <- content(resp, as = "text")
	}

	if (audit_depth == 1) {
		data_for_log <- NULL
		if (is.data.frame(out)) data_for_log <- out
		if (is.list(out) && !is.null(out$submission_data) && inherits(out$submission_data, "data.frame")) {
			data_for_log <- out$submission_data
		}
		maybe_write_audit("GetSubmissions", data = data_for_log)
	}

	return(out)
}
