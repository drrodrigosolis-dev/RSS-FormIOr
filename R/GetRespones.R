

#' Download responses from a FormIO form
#'
#' Retrieves submissions ("responses") for a FormIO form using the export API.
#' This is one of the main entry points for getting data into FormIOr.
#'
#' Credentials:
#' - If `form_id` and `api_key` are `NULL`, FormIOr will use previously entered
#'   credentials from the current R session (stored internally), or prompt via
#'   [AskCredentials()].
#'
#' Audit logging:
#' - If audit logging is active (see [StartAuditLog()]), the download action is
#'   recorded (but your API key is never written to the log).
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
#' @param base_url Character. API base URL. Default:
#'   `"https://submit.digital.gov.bc.ca/app/api/v1"`.
#' @param form_id Character. Form ID. If `NULL`, uses stored credentials or
#'   prompts via [AskCredentials()].
#' @param api_key Character. API key/secret. If `NULL`, uses stored credentials
#'   or prompts via [AskCredentials()].
#' @param drafts Logical. Include draft submissions? Default `FALSE`.
#' @param deleted Logical. Include deleted submissions? Default `FALSE`.
#' @param content.only Logical or character.
#'   - `TRUE` (default): return the submissions as a data frame/list
#'   - `FALSE`: return a list with status, headers, and the parsed content
#'   - `"raw"`: return the raw JSON response as text
#' @param reenter.credentials Logical. Force re-entry of credentials (default:
#'   `FALSE`).
#'
#' @return
#' If `content.only = TRUE` (default), returns the parsed submissions
#' (typically a data frame).
#' If `content.only = FALSE`, returns a list with elements `status`,
#' `content_type`, `content_disposition`, and `submission_data`.
#' If `content.only = "raw"`, returns the raw JSON response as a character
#' string.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' responses <- GetResponses(form_id = "your-form-id", api_key = "your-api-key")
#' flat <- FlattenSubmissions(responses)
#' }

GetResponses <- function(base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
                         form_id= NULL,
                         api_key= NULL,
                         drafts = FALSE,
                         deleted = FALSE,
                         content.only = TRUE,
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

  export_url <- paste0(base_url, "/forms/", form_id, "/export")

  query_params <- list(
    format = "json",
    type = "submissions",
    drafts = ifelse(isTRUE(drafts), "true", "false"),
    deleted = ifelse(isTRUE(deleted), "true", "false")
  )

  # ---- Try request with x-token header (preferred) ----
  resp <- GET(
    url = export_url,
    query = query_params,
    authenticate(user = form_id, password = api_key, type = "basic"),
    add_headers("Accept" = "application/json"),
    timeout(60)
  )

  submission_data <- fromJSON(content(resp, as = "text"))

  full_submission_data <- list(
    status = status_code(resp),
    content_type = headers(resp)[["content-type"]],
    content_disposition = headers(resp)[["content-disposition"]],
    submission_data = submission_data  )

  # ---- Return object to R environment ----
  # (submissions_data will be either the parsed JSON list/data.frame or NULL if a file was saved)
  out <- NULL
  if (isFALSE(content.only)) {out <- full_submission_data}
  if (isTRUE(content.only)){out <- submission_data}
  if (content.only == 'raw'){out <- content(resp, as = "text")}

  if (audit_depth == 1) {
    data_for_log <- NULL
    if (is.data.frame(out)) data_for_log <- out
    if (is.list(out) && !is.null(out$submission_data) && inherits(out$submission_data, "data.frame")) {
      data_for_log <- out$submission_data
    }
    maybe_write_audit(
      "GetResponses",
      details = paste0("drafts=", drafts, "; deleted=", deleted),
      data = data_for_log
    )
  }

  return(out)
}
