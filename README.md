# FormIOr

Helper functions for downloading, flattening, cleaning, and simplifying FormIO submissions.
This package is designed for non-technical users who want a clean, analysis-ready data table.

## Install

```r
# From local folder
install.packages("/path/to/FormIOr", repos = NULL, type = "source")
```

## Typical Workflow

```r
library(FormIOr)

# 1) Download responses
responses <- GetResponses(form_id = "your-form-id", api_key = "your-api-key")

# 2) Flatten nested submissions
flat <- FlattenSubmissions(responses)

# 3) Optional: rename columns interactively
RenameCols(flat)

# 4) Simplify and clean
norm <- NormalizeColumnNames(flat)                # clean column names
resolved <- ResolveRepeats(flat)                  # collapse repeated answers
compact <- CompactSelections(flat)                # combine checkbox columns

dedup <- DeduplicateSubmissions(flat)             # remove duplicate submissions
```

## Cleaning & Simplification Helpers

All functions below accept either a plain data frame or the output of
`FlattenSubmissions()`.

### NormalizeColumnNames()
Standardizes column names for readability and consistency.

```r
norm <- NormalizeColumnNames(flat)
head(norm$data)
```

### ResolveRepeats()
Collapses repeated values within each submission ID using either a fixed strategy
or simple auto-detection.

```r
resolved <- ResolveRepeats(flat, id_col = "submissionId")
resolved$summary
```

### DeduplicateSubmissions()
Keeps one row per submission ID (uses a timestamp column when available).

```r
dedup <- DeduplicateSubmissions(flat, id_col = "submissionId")
```

### CompactSelections()
Combines checkbox/multi-select columns into a single comma-separated column.

```r
compact <- CompactSelections(flat)
compact$summary
```

## Notes

- If you want the updated `FlattenSubmissions()` list in the output, pass
  `return_flat = TRUE` to any of the new cleaning functions.
- Use `FixDups()` for a guided, interactive cleanup of multi-value fields.

