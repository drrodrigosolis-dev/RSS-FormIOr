

#' Flatten nested FormIO submissions into a single table
#'
#' FormIO submissions often contain nested lists (for repeating sections,
#' address blocks, uploads, etc.). `FlattenSubmissions()` expands those nested
#' structures into a regular 2D data frame so you can export and analyze the
#' data more easily.
#'
#' Nested columns are flattened using their parent name as a prefix, then a
#' `"-"` separator, and then the nested field name.
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' @param x A data frame or list of submissions, typically returned by
#'   [GetResponses()].
#'
#' @return A list with:
#' \describe{
#'   \item{FlatResponses}{A flattened data frame (one row per submission row in the source).}
#'   \item{ColumnNames}{A data frame listing the new column names and their order.}
#' }
#'
#' @export
#'
#' @examples
#' x <- FoodTypes
#'
#' # Flattened structure
#' xFlat <- FlattenSubmissions(x)
#' head(xFlat$FlatResponses)
#' head(xFlat$ColumnNames)



FlattenSubmissions <- function(x) {
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

	LineData<-list()
	for(j in 1:nrow(x)){
		RespondentLine <- x[j, ]
		LineData[[j]]<-list( tidyr::unnest(RespondentLine, cols = everything(), names_sep="-", keep_empty = T))
	}

	Output<-dplyr::bind_rows(LineData)
	ColumnNames<-data.frame(Number= 1:length(colnames(Output)), Name=  colnames(Output))
	out <- list(FlatResponses=Output, ColumnNames=ColumnNames)

	if (audit_depth == 1) {
		maybe_write_audit("FlattenSubmissions", data = out$FlatResponses)
	}

	return(out)
}
