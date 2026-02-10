#' Interactively rename columns
#'
#' Guides you through renaming each column in a flattened FormIO dataset.
#' This is helpful for non-technical users who want friendly column names
#' before analysis or export.
#'
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' @param x A list returned by [FlattenSubmissions()].
#' @param NamesDF Logical. If `TRUE` (default), return a data frame of the old
#'   and new names.
#' @param renameDF Logical. If `TRUE` (default), return the dataset with the
#'   updated column names.
#' @param rename_map Optional. A mapping to apply without prompting. Accepted
#'   forms:
#'   \describe{
#'     \item{named character vector}{Names are old column names, values are new column names.}
#'     \item{data.frame}{With columns `OldNames` and `NewNames`. Extra columns are ignored.}
#'   }
#'   If provided, `RenameCols()` runs non-interactively and applies the mapping
#'   to any matching columns.
#' @param quiet Logical. If `TRUE`, suppresses most printing (useful inside the
#'   workflow wizard). Default `FALSE`.
#'
#' @return A list that may include:
#' \describe{
#'   \item{renamedDF}{A data frame with columns `Number`, `OldNames`, `NewNames`.}
#'   \item{flat}{The updated [FlattenSubmissions()] list with renamed columns.}
#' }
#'
#' @export
#'
#' @examplesIf interactive()
#' flat <- FlattenSubmissions(GetResponses(form_id = "123", api_key = "abc"))
#' RenameCols(flat)

RenameCols <- function(x, NamesDF = TRUE, renameDF = TRUE, rename_map = NULL, quiet = FALSE) {
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

	FlatX<-x

	renamedDF <- FlatX$ColumnNames
	colnames(renamedDF)<-c("Number", "OldNames")

	if (!is.null(rename_map)) {
		map_vec <- NULL
		if (is.character(rename_map) && !is.null(names(rename_map))) {
			map_vec <- as.character(rename_map)
			names(map_vec) <- names(rename_map)
		} else if (inherits(rename_map, "data.frame") && all(c("OldNames", "NewNames") %in% names(rename_map))) {
			map_vec <- as.character(rename_map$NewNames)
			names(map_vec) <- as.character(rename_map$OldNames)
		}

		if (!is.null(map_vec)) {
			old <- as.character(renamedDF$OldNames)
			new <- old
			idx <- match(old, names(map_vec))
			keep <- !is.na(idx)
			new[keep] <- unname(map_vec[idx[keep]])
			new[is.na(new) | !nzchar(new)] <- old[is.na(new) | !nzchar(new)]

			renamedDF$NewNames <- new
			renamedDF <- tibble::as_tibble(renamedDF)

			colnames(FlatX$FlatResponses) <- new
			if (inherits(FlatX$ColumnNames, "data.frame") && all(c("Number", "Name") %in% names(FlatX$ColumnNames))) {
				FlatX$ColumnNames$Name <- new
			}

			Output <- list()
			if (NamesDF == TRUE) Output$renamedDF <- renamedDF
			if (renameDF == TRUE) Output$flat <- FlatX

			if (audit_depth == 1) {
				maybe_write_audit("RenameCols", details = paste0("applied_map=", sum(keep)), data = FlatX$FlatResponses)
			}

			return(Output)
		}
	}

	for (i in 1:nrow(renamedDF)) {
		if (!isTRUE(quiet)) cat("\014")
		currentcol <- renamedDF$OldNames[i]
		if (!isTRUE(quiet)) {
			cat(green(paste0(i, "/", nrow(renamedDF) )))
			cat("\n")
			cat("Provide a new name for:" %+% blurred(italic("(Leave blank to use same name)")))
			cat("\n")
			cat("-> " %+% bgWhite(black(currentcol)) )
		} else {
			cat("Rename column ", i, "/", nrow(renamedDF), ": ", currentcol, "\n", sep = "")
		}
		ask <- readline()
		newname <- ask
		if (ask == "") newname <- currentcol
		renamedDF$NewNames[i] <- newname
		renamedDF<-tibble::as_tibble(renamedDF)

	}
	colnames(FlatX$FlatResponses)<-renamedDF$NewNames
	if (!isTRUE(quiet)) print(renamedDF, n=min(100, nrow(renamedDF)))

	Output <- list()

	if (NamesDF == T) Output$renamedDF <- renamedDF

	if (renameDF == T) Output$flat <- FlatX
	if (audit_depth == 1) {
		maybe_write_audit("RenameCols", data = FlatX$FlatResponses)
	}

	return(Output)
}
