#' Ask Credentials
#'
#' Function to get the credentials that will be used to access the Form's API. Generally this function will be called from within GetResponses()
#' @returns Vector of 2
#' @export
#'
#' @examples
#' AskCredentials()
#'
AskCredentials <- function() {
  print("-> You can find your Form's ID by going to your Form Settings page, and copying it from the URL (after the '=' sign in the URL)")
  ID <- readline(prompt = "Please enter your Form's ID: ")
  print('-> You can generate an API for the specific form from the "Manage Form" page')
  API <- readline(prompt = "Please enter your API: ")

  Form_Info <- c(ID = ID, Key = API)
  return(Form_Info)
}
