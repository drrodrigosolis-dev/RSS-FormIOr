#' Retrieve form metadata (name, versions, settings)
#'
#' Downloads basic metadata for a FormIO form. This can be useful for:
#'
#' - confirming you have the right form (title/name/version)
#' - naming output folders (the wizard uses the form title when it can)
#' - understanding version history (useful before comparing versions)
#'
#' Note: this returns *metadata only*, not the full form schema/components. If
#' you want a field list and labels, use [DescribeForm()] or [FieldDictionary()]
#' (those functions can use the metadata to fetch the schema when credentials
#' are available).
#'
#' @param base_url API base URL. Default `"https://submit.digital.gov.bc.ca/app/api/v1"`.
#' @param form_id Form ID. If `NULL`, credentials are read from [AskCredentials()].
#' @param api_key API key/secret. If `NULL`, credentials are read from [AskCredentials()].
#' @param reenter.credentials Logical. If `TRUE`, forces re-entry of credentials.
#'
#' @details
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' This function returns its result using `invisible()`. Assign it to a variable
#' to inspect it.
#'
#' @return A named list (structure depends on the API) that typically includes
#' form identifiers, configuration fields, and a `versions` table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' meta <- GetFormMetadata(form_id = "your-form-id", api_key = "your-api-key")
#' meta$title
#' str(meta$versions)
#' }
GetFormMetadata<-function(base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
												 form_id = NULL, api_key = NULL,
												  reenter.credentials = FALSE) {
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

	versionRelation<-fromJSON(content(as='text',GET(
		url = paste0(base_url, "/forms/", form_id),
		query = list(formID=form_id), authenticate(user = form_id, password = api_key, type = "basic"), add_headers(Accept = "application/json"),timeout(60)  )))

	if (audit_depth == 1) {
		maybe_write_audit("GetFormMetadata", details = "Form metadata fetched")
	}

	return(invisible(versionRelation))

	}
