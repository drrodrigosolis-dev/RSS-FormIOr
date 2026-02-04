#' Add Hierarchical Sections to FormIO Response Columns
#'
#' This interactive function guides the user through categorizing columns of a flattened FormIO response dataset into hierarchical sections (up to 3 levels deep). It is designed to facilitate easier analysis by adding grouping variables to columns. The function handles input that may be a data frame, a list from \code{FlattenSubmissions()}, or raw output from \code{GetResponses()}.
#'
#' @param x A data frame (possibly with nested list-columns), or a list containing \code{FlatResponses} (a flattened data frame) or \code{submission_data} (from \code{GetResponses()} with \code{content.only = FALSE}).
#'
#' @details
#' The function first extracts or flattens the input to obtain a flat data frame of responses. It then prompts the user to:
#' \itemize{
#'   \item Select the depth of sections (1, 2, or 3).
#'   \item Provide comma-separated names for sections at each level (no spaces or special characters).
#'   \item Assign row numbers (from the displayed column list) to each section at each level. Row numbers can be single values, comma-separated lists, or ranges (e.g., "1:5,8,10:12").
#' }
#' Empty assignments are filled with "General". The process uses console clearing (\code{\014}) and colored prompts (requires the \code{crayon} package).
#'
#' @return A list with two elements:
#' \item{FlatResponses}{The original flattened data frame of responses.}
#' \item{Sections}{A \code{tibble} with columns \code{No} (column number), \code{Names} (original column names), and \code{Level-1}, \code{Level-2}, \code{Level-3} (section assignments, filled with "General" if empty).}
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Assuming FoodTypes is a sample dataset with possible nests
#'   data("FoodTypes")
#'   sectioned <- AddSections(FoodTypes)
#'   print(sectioned$Sections)
#' }
#'
#' @importFrom tibble tibble add_column
#' @importFrom dplyr mutate across
#' @importFrom crayon inverse red bold blurred `%+%`
#'
#'
AddSections <- function(x) {
	# Improved input handling
	if (inherits(x, "data.frame")) {
		Data <- x
	} else if (!is.null(x$FlatResponses)) {
		Data <- x$FlatResponses
	} else if (!is.null(x$submission_data)) {
		Data <- x$submission_data
	} else {
		stop("Input must be a data frame or a list containing 'FlatResponses' or 'submission_data'")
	}

	# Check for nested columns and flatten if necessary
	if (any(sapply(Data, is.list))) {
		message("Detected nested columns; flattening...")
		Flat <- FlattenSubmissions(Data)
		Data <- Flat$FlatResponses
	}

	CtrlDF <- tibble(No = 1:length(colnames(Data)), Names = colnames(Data))
	print(CtrlDF, n = nrow(CtrlDF))

	Depth <- askingDepth()

	for (i in 1:Depth) {
		CtrlDF <- CtrlDF %>% add_column("Level-{i}" := "")
	}

	cat("\014")
	print(CtrlDF, n = nrow(CtrlDF))

	Sections <- askingSections(DepthAsked = Depth)
	Output <- assignSections(Sections = Sections, df = CtrlDF)
	Data <- list(FlatResponses = Data, Sections = Output)

	return(Data)
}

# Helper functions (not exported, so no separate Roxygen, but documented inline)

# Loop-based depth prompt
askingDepth <- function() {
	while (TRUE) {
		cat("\014")
		cat(inverse("Select the depth of sections that you want to work with (1, 2 or 3)"))
		DepthAsked <- as.numeric(readline())
		if (DepthAsked %in% c(1, 2, 3)) return(DepthAsked)
		cat("\014")
		cat(red("please select a number between 1 and 3"))
		Sys.sleep(2)
	}
}

# Simplified to list of character vectors
askingSections <- function(DepthAsked) {
	SectionsList <- list()
	for (i in 1:(min(3, DepthAsked))) {
		cat(inverse(paste("Please provide the names for the sections at level", i)) %+% blurred("\n Separate each name with a comma and do not use spaces or special characters"))
		SectionNames <- readline()
		SectionNames <- trimws(strsplit(SectionNames, split = ",")[[1]])
		SectionsList[[i]] <- SectionNames
	}
	return(SectionsList)
}

# Adjusted for list of vectors
assignSections <- function(Sections, df) {
	for (i in 1:length(Sections)) {
		for (j in 1:length(Sections[[i]])) {
			CurrentSection <- Sections[[i]][j]
			cat("\014")
			print(df, n = nrow(df))
			cat(inverse("Please provide the row number(s) of the items that will belong to " %+% bold(red(CurrentSection)) %+% " at Depth " %+% bold(red(i))))
			cat("\n")
			cat(blurred("use only numbers, separated by commas, ranges with :"))
			cat("\n")
			NoRowsAssigned <- get_numbers("")
			df[NoRowsAssigned, i + 2] <- CurrentSection
			print(df, n = nrow(df))
		}
		print(df, n = nrow(df))
	}
	df <- df %>% mutate(across(all_of(colnames(df)), ~ ifelse(. == "", "General", .)))
	print(df, n = nrow(df))
	return(df)
}

# Adjusted to allow empty prompt
get_numbers <- function(prompt = "Enter numbers (comma-separated, ranges with : ok): ") {
	if (prompt != "") cat(prompt)
	input <- readline()
	# Split by comma
	parts <- strsplit(input, ",")[[1]]
	parts <- trimws(parts) # remove extra spaces

	result <- integer(0)

	for (p in parts) {
		# Check if it's a range (contains ":")
		if (grepl(":", p)) {
			# Split range like "8:15" -> c("8", "15")
			rng <- strsplit(p, ":")[[1]]
			rng <- trimws(rng)

			# Must have exactly 2 parts
			if (length(rng) != 2) {
				warning("Invalid range format: ", p, " -> skipping")
				next
			}

			# Try to convert to numbers
			start <- suppressWarnings(as.numeric(rng[1]))
			end <- suppressWarnings(as.numeric(rng[2]))

			if (is.na(start) || is.na(end)) {
				warning("Non-numeric in range: ", p, " -> skipping")
				next
			}

			if (start <= end) {
				result <- c(result, seq(from = start, to = end, by = 1))
			} else {
				# Optional: allow descending ranges
				result <- c(result, seq(from = start, to = end, by = -1))
			}
		}
		# Single number
		else {
			val <- suppressWarnings(as.numeric(p))
			if (is.na(val)) {
				warning("Invalid number: ", p, " -> skipped")
			} else {
				result <- c(result, val)
			}
		}
	}

	# Remove possible duplicates and sort (optional but often desired)
	result <- sort(unique(result))

	return(result)
}


