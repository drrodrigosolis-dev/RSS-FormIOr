

test_that("NormalizeColumnNames standardizes names", {
	df <- data.frame(
		`First Name` = c("Ana", "Ben"),
		`Age (Years)` = c(30, 25),
		check.names = FALSE
	)

	res <- NormalizeColumnNames(df)
	expect_equal(names(res$data), c("first_name", "age_years"))
	expect_equal(nrow(res$name_map), 2)
})


test_that("ResolveRepeats collapses rows by submission id", {
	df <- data.frame(
		submissionId = c("a", "a", "b"),
		fruit = c("Apple", "Banana", "Apple"),
		qty = c(1, 2, 1),
		check1 = c(TRUE, FALSE, TRUE),
		stringsAsFactors = FALSE
	)

res <- ResolveRepeats(df, id_col = "submissionId")
expect_equal(nrow(res$data), 2)
expect_true(grepl("Apple", res$data$fruit[res$data$submissionId == "a"]))
expect_equal(res$data$qty[res$data$submissionId == "a"], 3)
expect_equal(res$data$check1[res$data$submissionId == "a"], 1)
})


test_that("DeduplicateSubmissions keeps last by time column", {
	df <- data.frame(
		submissionId = c("a", "a", "b"),
		created = c("2020-01-01", "2020-01-02", "2020-01-03"),
		value = c(1, 2, 3),
		stringsAsFactors = FALSE
	)

	res <- DeduplicateSubmissions(df, id_col = "submissionId", keep = "last")
	expect_equal(nrow(res$data), 2)
	expect_equal(res$data$value[res$data$submissionId == "a"], 2)
})


test_that("CompactSelections merges checkbox columns", {
	df <- data.frame(
		`food-apple` = c(TRUE, FALSE),
		`food-banana` = c(FALSE, TRUE),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)

	res <- CompactSelections(df, sep = "-")
	expect_true("food-selected" %in% names(res$data))
	expect_equal(res$data[["food-selected"]], c("apple", "banana"))
})

test_that("CompactSelections tolerates blank/NA checkbox columns", {
	df <- data.frame(
		`Funding_Phase-NotSure` = c("", NA, "No"),
		`Funding_Phase-phase_I` = c("Yes", "", "No"),
		`Funding_Phase-phase_II` = c("", "", "Yes"),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)

	res <- CompactSelections(df, sep = "-")
	expect_true("Funding_Phase-selected" %in% names(res$data))
	expect_equal(res$data[["Funding_Phase-selected"]], c("phase_I", NA, "phase_II"))
})

test_that("CompactSelections works with logical columns", {
	df <- data.frame(
		`Funding_Phase-NotSure` = c(TRUE, FALSE, NA),
		`Funding_Phase-phase_I` = c(FALSE, TRUE, NA),
		`Funding_Phase-phase_II` = c(FALSE, TRUE, NA),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)

	res <- CompactSelections(df, sep = "-")
	expect_true("Funding_Phase-selected" %in% names(res$data))
	expect_equal(res$data[["Funding_Phase-selected"]], c("NotSure", "phase_I, phase_II", NA))
})
