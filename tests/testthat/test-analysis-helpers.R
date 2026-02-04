

test_that("SummaryByField summarizes numeric fields", {
	df <- data.frame(score = c(1, 2, 3, NA))

	res <- SummaryByField(df, "score", quiet = TRUE)
	metrics <- res$summary$metric

	expect_equal(res$type, "numeric")
	expect_equal(res$missing, 1)
	expect_true("mean" %in% metrics)
})


test_that("SummaryByField summarizes categorical fields", {
	df <- data.frame(fruit = c("Apple", "Apple", "Banana", NA))

	res <- SummaryByField(df, "fruit", include_na = TRUE, quiet = TRUE)

	expect_equal(res$type, "categorical")
	expect_equal(res$missing, 1)
	expect_equal(res$summary$count[res$summary$value == "Apple"], 2)
})


test_that("CrossTab builds counts and percents", {
	df <- data.frame(
		region = c("North", "North", "South"),
		program = c("A", "B", "A"),
		stringsAsFactors = FALSE
	)

	res <- CrossTab(df, "region", "program", percent = "overall", quiet = TRUE)

	expect_equal(res$row, "region")
	expect_equal(res$col, "program")
	expect_true("count" %in% names(res$long))
	expect_true("percent" %in% names(res$long))
	expect_equal(res$table[res$table$region == "North", "A"], 1)
})


test_that("ResponseTimeline counts by day and fills empty dates", {
	df <- data.frame(
		created = c("2024-01-01", "2024-01-01", "2024-01-03"),
		stringsAsFactors = FALSE
	)

	res <- ResponseTimeline(df, date_col = "created", interval = "day", include_empty = TRUE, quiet = TRUE)

	expect_true(as.Date("2024-01-02") %in% res$data$period)
	expect_equal(res$data$count[res$data$period == as.Date("2024-01-02")], 0)
})
