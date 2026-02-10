#' Enter FormIO credentials
#'
#' Prompts for the Form ID and API key used to access the FormIO API.
#' This is usually called automatically by [GetResponses()] and related
#' functions when credentials are not already stored.
#'
#' @section CHEF credentials and base URL:
#' FormIOr is designed for the BC Public Service CHEF FormIO service.
#' The default base URL is `https://submit.digital.gov.bc.ca/app/api/v1`.
#' If you use a different FormIO service, you must override `base_url` in
#' [GetResponses()] / [GetSubmissions()], and compatibility is not guaranteed.
#'
#' For CHEF users, generate your API key from the form's **Manage** page.
#' The Form ID is the final alphanumeric code after the `=` sign in the
#' Manage page URL.
#'
#' @return A named character vector of length 2: `ID` and `Key`.
#'
#' @details
#' If audit logging is active (see [StartAuditLog()]), the action is recorded,
#' but the credentials themselves are **not** written to the log.
#' @export
#'
#' @examplesIf interactive()
#' AskCredentials()
#'
AskCredentials <- function() {
  audit_depth <- audit_enter()
  on.exit(audit_exit(), add = TRUE)
  if (audit_depth == 1) maybe_prompt_audit_log()

  cat("\014")
  cat("-> You can find your Form's ID by going to your Form Settings page, and copying it from the URL (after the '=' sign in the URL)")
  cat('\n')
  cat(inverse("Please enter your Form's ID: "))
  ID <- readline()
  cat('-> You can generate an API for the specific form from the "Manage Form" page')
  cat('\n')
  cat(inverse("Please enter your Form's API: "))

  API <- readline()

  Form_Info <- c(ID = ID, Key = API)
  .formior_state$Form_Info <- Form_Info

  if (audit_depth == 1) {
    maybe_write_audit("AskCredentials", details = "Credentials entered")
  }

  return(Form_Info)
}
