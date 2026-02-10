#' Summarize a single field in a FormIO response dataset
#'
#' Designed for non-technical users: it accepts either a data frame or the list
#' produced by [FlattenSubmissions()] and returns a simple summary. Numeric
#' fields get descriptive statistics; categorical fields get counts and percents.
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [FlattenSubmissions()].
#' @param field Column name or number to summarize.
#' @param top_n For categorical fields, the number of top values to return
#'   (default 10). Use `NULL` to return all values.
#' @param include_na Logical. If `TRUE`, include missing values as "(Missing)"
#'   in categorical summaries.
#' @param digits Number of decimal places for percentages/statistics.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{field}{Resolved column name}
#'   \item{type}{Detected type: "numeric", "categorical", or "date"}
#'   \item{total}{Total rows}
#'   \item{missing}{Missing value count}
#'   \item{distinct}{Distinct non-missing values}
#'   \item{summary}{Data frame of summary stats or counts}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' flat <- FlattenSubmissions(GetResponses(form_id = "123", api_key = "abc"))
#' SummaryByField(flat, "age")
#' SummaryByField(flat, "favorite_food", top_n = 5)
#' }
SummaryByField <- function(
		x,
		field,
		top_n = 10,
		include_na = FALSE,
		digits = 2,
		quiet = FALSE
) {
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

	df <- extract_flat_df(x)
	field_name <- resolve_field_name(df, field)
	values <- df[[field_name]]
	total <- length(values)

	type <- detect_field_type(values)

	if (type == "numeric") {
		values_num <- values
		if (is.factor(values_num)) values_num <- as.character(values_num)
		num <- suppressWarnings(as.numeric(values_num))
		missing <- sum(is.na(num))
		distinct <- length(unique(num[!is.na(num)]))

		stats <- c(
			n = sum(!is.na(num)),
			missing = missing,
			missing_pct = if (total == 0) NA_real_ else (missing / total) * 100,
			mean = if (all(is.na(num))) NA_real_ else mean(num, na.rm = TRUE),
			median = if (all(is.na(num))) NA_real_ else stats::median(num, na.rm = TRUE),
			sd = if (all(is.na(num))) NA_real_ else stats::sd(num, na.rm = TRUE),
			min = if (all(is.na(num))) NA_real_ else min(num, na.rm = TRUE),
			p25 = if (all(is.na(num))) NA_real_ else stats::quantile(num, 0.25, na.rm = TRUE, names = FALSE),
			p75 = if (all(is.na(num))) NA_real_ else stats::quantile(num, 0.75, na.rm = TRUE, names = FALSE),
			max = if (all(is.na(num))) NA_real_ else max(num, na.rm = TRUE)
		)

		summary <- data.frame(
			metric = names(stats),
			value = round(stats, digits),
			stringsAsFactors = FALSE
		)
	} else if (type == "date") {
		times <- coerce_time(values, tz = "UTC")
		missing <- sum(is.na(times))
		distinct <- length(unique(times[!is.na(times)]))

		min_val <- if (all(is.na(times))) NA_character_ else format(min(times, na.rm = TRUE), "%Y-%m-%d %H:%M:%S")
		max_val <- if (all(is.na(times))) NA_character_ else format(max(times, na.rm = TRUE), "%Y-%m-%d %H:%M:%S")

		stats <- c(
			n = sum(!is.na(times)),
			missing = missing,
			missing_pct = if (total == 0) NA_real_ else (missing / total) * 100,
			min = min_val,
			max = max_val
		)

		summary <- data.frame(
			metric = names(stats),
			value = unname(stats),
			stringsAsFactors = FALSE
		)
	} else {
		vals <- values
		if (include_na) {
			vals <- ifelse(is.na(vals), "(Missing)", as.character(vals))
		} else {
			vals <- as.character(vals[!is.na(vals)])
		}

		if (length(vals) == 0) {
			counts <- integer(0)
			missing <- sum(is.na(values))
			distinct <- 0
		} else {
			counts <- sort(table(vals), decreasing = TRUE)
			missing <- sum(is.na(values))
			distinct <- length(unique(values[!is.na(values)]))
		}

		if (!is.null(top_n) && top_n > 0 && length(counts) > top_n) {
			counts <- counts[seq_len(top_n)]
		}

		percent <- if (length(counts) == 0) numeric(0) else as.numeric(counts) / sum(counts) * 100

		summary <- data.frame(
			value = names(counts),
			count = as.integer(counts),
			percent = round(percent, digits),
			stringsAsFactors = FALSE
		)
	}

	out <- list(
		field = field_name,
		type = type,
		total = total,
		missing = missing,
		distinct = distinct,
		summary = summary
	)

	if (!quiet) {
		message("Summarized ", field_name, " (", type, ").")
	}

	if (audit_depth == 1) {
		maybe_write_audit("SummaryByField", details = paste0("field=", field_name), data = df)
	}

	out
}


#' Cross-tabulate two fields
#'
#' Builds a simple cross-tabulation between two columns, returning both a
#' wide table and a long table that includes counts and (optional) percents.
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [FlattenSubmissions()].
#' @param row Column name or number for the rows.
#' @param col Column name or number for the columns.
#' @param include_na Logical. If `TRUE`, treat missing values as "(Missing)".
#' @param percent How to calculate percentages. One of `"overall"` (default),
#'   `"row"`, `"col"`, or `"none"`.
#' @param digits Number of decimal places for percentages.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{row}{Resolved row field name}
#'   \item{col}{Resolved column field name}
#'   \item{percent}{Percent calculation mode}
#'   \item{table}{Wide counts table as a data frame}
#'   \item{long}{Long data frame with counts and percents}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' CrossTab(flat, "region", "program", percent = "row")
#' }
CrossTab <- function(
		x,
		row,
		col,
		include_na = FALSE,
		percent = c("overall", "row", "col", "none"),
		digits = 1,
		quiet = FALSE
) {
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

	percent <- match.arg(percent)

	df <- extract_flat_df(x)
	row_name <- resolve_field_name(df, row)
	col_name <- resolve_field_name(df, col)

	row_vals <- df[[row_name]]
	col_vals <- df[[col_name]]

	if (include_na) {
		row_vals <- ifelse(is.na(row_vals), "(Missing)", as.character(row_vals))
		col_vals <- ifelse(is.na(col_vals), "(Missing)", as.character(col_vals))
	} else {
		keep <- !is.na(row_vals) & !is.na(col_vals)
		row_vals <- as.character(row_vals[keep])
		col_vals <- as.character(col_vals[keep])
	}

	xtab <- table(row_vals, col_vals)

	wide <- as.data.frame.matrix(xtab, stringsAsFactors = FALSE)
	wide <- cbind(rownames(wide), wide, stringsAsFactors = FALSE)
	colnames(wide)[1] <- row_name
	rownames(wide) <- NULL

	long_counts <- as.data.frame(xtab, stringsAsFactors = FALSE)
	colnames(long_counts) <- c(row_name, col_name, "count")

	if (percent != "none") {
		if (percent == "overall") norm <- prop.table(xtab)
		if (percent == "row") norm <- prop.table(xtab, 1)
		if (percent == "col") norm <- prop.table(xtab, 2)

		long_percent <- as.data.frame(as.table(norm), stringsAsFactors = FALSE)
		colnames(long_percent) <- c(row_name, col_name, "percent")
		long_percent$percent <- round(long_percent$percent * 100, digits)

		long <- merge(long_counts, long_percent, by = c(row_name, col_name), all = TRUE)
	} else {
		long <- long_counts
	}

	out <- list(
		row = row_name,
		col = col_name,
		percent = percent,
		table = wide,
		long = long
	)

	if (!quiet) {
		message("Cross-tabulated ", row_name, " by ", col_name, ".")
	}

	if (audit_depth == 1) {
		maybe_write_audit(
			"CrossTab",
			details = paste0("row=", row_name, "; col=", col_name),
			data = df
		)
	}

	out
}


#' Summarize responses over time
#'
#' Counts responses per day/week/month (or hour), using a timestamp column.
#' If `date_col` is `NULL`, the function tries common timestamp names
#' automatically (e.g., `created`, `modified`, `_createdAt`).
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [FlattenSubmissions()].
#' @param date_col Column name or number for the date/time field. If `NULL`,
#'   tries to guess a reasonable timestamp column.
#' @param interval One of `"day"` (default), `"week"`, `"month"`, or `"hour"`.
#' @param tz Time zone to use for parsing dates (default `"UTC"`).
#' @param start Optional start date/time to filter the range.
#' @param end Optional end date/time to filter the range.
#' @param include_empty Logical. If `TRUE`, fill missing periods with zeroes.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Data frame with `period` and `count` columns}
#'   \item{summary}{List with metadata about the calculation}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ResponseTimeline(flat, date_col = "created", interval = "week")
#' }
ResponseTimeline <- function(
		x,
		date_col = NULL,
		interval = c("day", "week", "month", "hour"),
		tz = "UTC",
		start = NULL,
		end = NULL,
		include_empty = TRUE,
		quiet = FALSE
) {
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

	interval <- match.arg(interval)

	df <- extract_flat_df(x)

	if (is.null(date_col)) {
		date_col <- guess_time_col(names(df))
	}
	if (is.null(date_col)) {
		stop("Could not find a date column. Please set date_col.")
	}
	date_name <- resolve_field_name(df, date_col)

	times <- coerce_time(df[[date_name]], tz = tz)
	if (all(is.na(times))) {
		stop("No valid dates found in column '", date_name, "'.")
	}

	if (!is.null(start)) start <- coerce_time(start, tz = tz)
	if (!is.null(end)) end <- coerce_time(end, tz = tz)

	if (is.null(start)) start <- min(times, na.rm = TRUE)
	if (is.null(end)) end <- max(times, na.rm = TRUE)

	keep <- !is.na(times) & times >= start & times <= end
	times <- times[keep]

	if (length(times) == 0) {
		stop("No dates remain after applying start/end filters.")
	}

	periods <- bucket_time(times, interval = interval, tz = tz)
	xtab <- table(periods)

	period_values <- names(xtab)
	if (interval == "hour") {
		period_values <- as.POSIXct(period_values, tz = tz)
	} else {
		period_values <- as.Date(period_values)
	}

	out_df <- data.frame(
		period = period_values,
		count = as.integer(xtab),
		stringsAsFactors = FALSE
	)

	if (include_empty) {
		seq_start <- bucket_time(start, interval = interval, tz = tz)
		seq_end <- bucket_time(end, interval = interval, tz = tz)
		full_seq <- build_time_sequence(seq_start, seq_end, interval = interval, tz = tz)
		full_df <- data.frame(period = full_seq, stringsAsFactors = FALSE)

		key <- as.character(full_df$period)
		counts <- as.integer(xtab)
		names(counts) <- names(xtab)
		full_df$count <- counts[key]
		full_df$count[is.na(full_df$count)] <- 0L

		out_df <- full_df
	}

	out_df <- out_df[order(out_df$period), , drop = FALSE]

	out <- list(
		data = out_df,
		summary = list(
			date_col = date_name,
			interval = interval,
			start = start,
			end = end,
			total = length(df[[date_name]]),
			used = length(times)
		)
	)

	if (!quiet) {
		message("Built timeline for ", date_name, " (", interval, ").")
	}

	if (audit_depth == 1) {
		maybe_write_audit(
			"ResponseTimeline",
			details = paste0("date_col=", date_name, "; interval=", interval),
			data = df
		)
	}

	out
}


#' Plot a histogram for a numeric field
#'
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [FlattenSubmissions()].
#' @param field Column name(s) or number(s) to plot. When multiple columns are
#'   provided, their text is combined into one wordcloud.
#' @param bins Histogram breaks passed to [hist()]. Default "Sturges".
#' @param include_na Logical. If `TRUE`, keep `NA` values (ignored by `hist`).
#' @param main Optional plot title.
#' @param xlab Optional x-axis label.
#' @param col Bar fill color.
#' @param border Bar border color.
#' @param plot Logical. If `FALSE`, return histogram data without plotting.
#' @param ... Additional arguments passed to [hist()].
#'
#' @return Invisibly returns a list with the field name and histogram object.
#' @export
#'
#' @examples
#' \dontrun{
#' PlotHistogram(flat, "age")
#' }
PlotHistogram <- function(
		x,
		field,
		bins = "Sturges",
		include_na = FALSE,
		main = NULL,
		xlab = NULL,
		col = "steelblue",
		border = "white",
		plot = TRUE,
		...
) {
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

	df <- extract_flat_df(x)
	field_name <- resolve_field_name(df, field)
	values <- df[[field_name]]

	if (is.factor(values)) values <- as.character(values)
	num <- suppressWarnings(as.numeric(values))
	if (!include_na) num <- num[!is.na(num)]
	if (all(is.na(num))) {
		stop("Field '", field_name, "' does not contain numeric values.")
	}

	if (is.null(main)) main <- paste("Histogram of", field_name)
	if (is.null(xlab)) xlab <- field_name

	h <- hist(
		num,
		breaks = bins,
		main = main,
		xlab = xlab,
		col = col,
		border = border,
		plot = plot,
		...
	)

	out <- list(field = field_name, hist = h)

	if (audit_depth == 1) {
		maybe_write_audit("PlotHistogram", details = paste0("field=", field_name), data = df)
	}

	invisible(out)
}


#' Plot a bar chart for a categorical field
#'
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [FlattenSubmissions()].
#' @param field Column name or number to plot.
#' @param top_n Number of categories to show (default 10). Use `NULL` for all.
#' @param include_na Logical. If `TRUE`, include missing values as "(Missing)".
#' @param horiz Logical. If `TRUE`, draw horizontal bars.
#' @param main Optional plot title.
#' @param xlab Optional x-axis label.
#' @param col Bar fill color.
#' @param plot Logical. If `FALSE`, return bar data without plotting.
#' @param ... Additional arguments passed to [barplot()].
#'
#' @return Invisibly returns a list with the field name and bar data.
#' @export
#'
#' @examples
#' \dontrun{
#' PlotBarSummary(flat, "region")
#' }
PlotBarSummary <- function(
		x,
		field,
		top_n = 10,
		include_na = FALSE,
		horiz = FALSE,
		main = NULL,
		xlab = NULL,
		col = "steelblue",
		plot = TRUE,
		...
) {
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

	summary <- SummaryByField(
		x = x,
		field = field,
		top_n = top_n,
		include_na = include_na,
		quiet = TRUE
	)

	if (summary$type %in% c("numeric", "date")) {
		stop("Field '", summary$field, "' is not categorical. Use PlotHistogram() or PlotResponseTimeline().")
	}

	data <- summary$summary
	if (nrow(data) == 0) {
		stop("No values available to plot for field '", summary$field, "'.")
	}

	if (is.null(main)) main <- paste("Top", nrow(data), summary$field)
	if (is.null(xlab)) xlab <- if (horiz) "Count" else ""

	if (plot) {
		barplot(
			height = data$count,
			names.arg = data$value,
			horiz = horiz,
			las = if (horiz) 1 else 2,
			col = col,
			main = main,
			xlab = xlab,
			...
		)
	}

	out <- list(field = summary$field, data = data)

	if (audit_depth == 1) {
		maybe_write_audit("PlotBarSummary", details = paste0("field=", summary$field), data = x)
	}

	invisible(out)
}


#' Plot a wordcloud for a text field
#'
#' Requires the optional `wordcloud` package. If it is not installed, the
#' function will stop with a helpful message.
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [FlattenSubmissions()].
#' @param field Column name or number to plot.
#' @param max_words Maximum number of words to display.
#' @param min_freq Minimum frequency to keep a word.
#' @param min_chars Minimum character length for a word to be kept.
#' @param remove_stopwords Logical. If `TRUE`, remove common stopwords.
#' @param stopwords Character vector of stopwords to remove.
#' @param seed Optional random seed for reproducible layout.
#' @param colors Vector of colors passed to [wordcloud::wordcloud()].
#' @param ... Additional arguments passed to [wordcloud::wordcloud()].
#'
#' @return Invisibly returns a list with the field name(s) and word frequencies.
#' @export
#'
#' @examples
#' \dontrun{
#' PlotWordcloud(flat, "feedback")
#' }
PlotWordcloud <- function(
		x,
		field,
		max_words = 100,
		min_freq = 1,
		min_chars = 2,
		remove_stopwords = TRUE,
		stopwords = default_stopwords(),
		seed = NULL,
		colors = NULL,
		...
) {
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

	if (!requireNamespace("wordcloud", quietly = TRUE)) {
		stop("Package 'wordcloud' is required. Install it with install.packages('wordcloud').")
	}

	df <- extract_flat_df(x)
	field_names <- resolve_field_names(df, field)
	values <- unlist(df[field_names], use.names = FALSE)

	words <- tokenize_words(values, min_chars = min_chars)
	if (remove_stopwords && length(words) > 0) {
		words <- words[!words %in% stopwords]
	}

	if (length(words) == 0) {
		stop("No words available to plot for the selected field(s).")
	}

	freq <- sort(table(words), decreasing = TRUE)
	freq <- freq[freq >= min_freq]
	if (!is.null(max_words) && length(freq) > max_words) {
		freq <- freq[seq_len(max_words)]
	}

	if (is.null(colors)) colors <- grDevices::rainbow(8)
	if (!is.null(seed)) set.seed(seed)

	wordcloud::wordcloud(
		words = names(freq),
		freq = as.numeric(freq),
		colors = colors,
		...
	)

	out <- list(fields = field_names, freq = freq)

	if (audit_depth == 1) {
		maybe_write_audit("PlotWordcloud", details = paste0("field=", paste(field_names, collapse = ", ")), data = df)
	}

	invisible(out)
}


#' Plot a response timeline
#'
#' Convenience wrapper around [ResponseTimeline()] that draws a simple line
#' chart using base graphics.
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' @param x A data frame of responses, or a list from [FlattenSubmissions()].
#' @param date_col Column name or number for the date/time field. If `NULL`,
#'   tries to guess a reasonable timestamp column.
#' @param interval One of `"day"` (default), `"week"`, `"month"`, or `"hour"`.
#' @param tz Time zone to use for parsing dates (default `"UTC"`).
#' @param start Optional start date/time to filter the range.
#' @param end Optional end date/time to filter the range.
#' @param include_empty Logical. If `TRUE`, fill missing periods with zeroes.
#' @param main Optional plot title.
#' @param xlab Optional x-axis label.
#' @param ylab Optional y-axis label.
#' @param col Line color.
#' @param lwd Line width.
#' @param type Plot type passed to [plot()]. Default "l".
#' @param plot Logical. If `FALSE`, return timeline data without plotting.
#' @param ... Additional arguments passed to [plot()].
#'
#' @return Invisibly returns the output of [ResponseTimeline()].
#' @export
#'
#' @examples
#' \dontrun{
#' PlotResponseTimeline(flat, date_col = "created", interval = "month")
#' }
PlotResponseTimeline <- function(
		x,
		date_col = NULL,
		interval = c("day", "week", "month", "hour"),
		tz = "UTC",
		start = NULL,
		end = NULL,
		include_empty = TRUE,
		main = NULL,
		xlab = NULL,
		ylab = "Responses",
		col = "steelblue",
		lwd = 2,
		type = "l",
		plot = TRUE,
		...
) {
	audit_depth <- audit_enter()
	on.exit(audit_exit(), add = TRUE)
	if (audit_depth == 1) maybe_prompt_audit_log()

	interval <- match.arg(interval)

	out <- ResponseTimeline(
		x = x,
		date_col = date_col,
		interval = interval,
		tz = tz,
		start = start,
		end = end,
		include_empty = include_empty,
		quiet = TRUE
	)

	if (plot) {
		data <- out$data
		if (nrow(data) == 0) {
			stop("No timeline data available to plot.")
		}

		if (is.null(main)) main <- paste("Responses by", interval)
		if (is.null(xlab)) xlab <- ""

		x_vals <- data$period
		plot(
			x = x_vals,
			y = data$count,
			type = type,
			col = col,
			lwd = lwd,
			main = main,
			xlab = xlab,
			ylab = ylab,
			...
		)
	}

	if (audit_depth == 1) {
		maybe_write_audit("PlotResponseTimeline", details = paste0("interval=", interval), data = x)
	}

	invisible(out)
}


# ---- Internal helpers -----------------------------------------------------------

resolve_field_name <- function(df, field) {
	if (is.numeric(field)) {
		field_name <- names(df)[field]
	} else {
		field_name <- as.character(field)
	}
	if (is.na(field_name) || !field_name %in% names(df)) {
		stop("field '", field, "' not found in data")
	}
	field_name
}

resolve_field_names <- function(df, field) {
	if (length(field) == 0) stop("field must include at least one column.")
	if (is.numeric(field)) {
		field_names <- names(df)[field]
	} else {
		field_names <- as.character(field)
	}
	if (any(is.na(field_names)) || any(!field_names %in% names(df))) {
		bad <- field_names[is.na(field_names) | !field_names %in% names(df)]
		stop("field(s) not found in data: ", paste(bad, collapse = ", "))
	}
	field_names
}


detect_field_type <- function(values) {
	if (inherits(values, "Date") || inherits(values, "POSIXt")) {
		return("date")
	}
	if (is.numeric(values)) {
		return("numeric")
	}
	if (is.factor(values)) {
		if (is_numeric_like(as.character(values))) return("numeric")
		return("categorical")
	}
	if (!is.logical(values) && is_numeric_like(values)) {
		return("numeric")
	}
	"categorical"
}


coerce_time <- function(values, tz = "UTC") {
	if (is.factor(values)) values <- as.character(values)
	if (inherits(values, "POSIXt")) {
		return(as.POSIXct(values, tz = tz))
	}
	if (inherits(values, "Date")) {
		return(as.POSIXct(values, tz = tz))
	}

	times <- suppressWarnings(as.POSIXct(values, tz = tz))
	if (all(is.na(times))) {
		dates <- suppressWarnings(as.Date(values))
		times <- as.POSIXct(dates, tz = tz)
	}
	times
}


bucket_time <- function(times, interval, tz = "UTC") {
	if (interval == "day") {
		return(as.Date(times, tz = tz))
	}
	if (interval == "week") {
		return(as.Date(cut(times, "week")))
	}
	if (interval == "month") {
		return(as.Date(cut(times, "month")))
	}
	as.POSIXct(cut(times, "hour"), tz = tz)
}


build_time_sequence <- function(start, end, interval, tz = "UTC") {
	if (interval == "hour") {
		return(seq.POSIXt(as.POSIXct(start, tz = tz), as.POSIXct(end, tz = tz), by = "hour"))
	}
	start_date <- as.Date(start)
	end_date <- as.Date(end)
	by <- if (interval == "day") "day" else if (interval == "week") "week" else "month"
	seq.Date(start_date, end_date, by = by)
}


default_stopwords <- function() {
	c(
		"a", "an", "and", "are", "as", "at", "be", "but", "by",
		"for", "from", "has", "have", "he", "her", "his", "i",
		"if", "in", "is", "it", "its", "me", "my", "not", "of",
		"on", "or", "our", "she", "so", "that", "the", "their",
		"them", "there", "they", "this", "to", "was", "we", "were",
		"will", "with", "you", "your"
	)
}


tokenize_words <- function(values, min_chars = 2) {
	if (is.factor(values)) values <- as.character(values)
	text <- paste(values[!is.na(values)], collapse = " ")
	text <- tolower(text)
	text <- gsub("[^a-z0-9']+", " ", text)
	words <- unlist(strsplit(text, "\\s+"))
	words <- words[words != ""]
	if (min_chars > 1) {
		words <- words[nchar(words) >= min_chars]
	}
	words
}
