## Test environments
* local macOS Tahoe 26.2, R 4.5.2 (aarch64-apple-darwin20)
  - `R CMD check --as-cran FormIOr_1.0.0.tar.gz`
  - `_R_CHECK_FORCE_SUGGESTS_=false` (no internet to install Suggests)

## R CMD check results
1 ERROR | 1 WARNING | 5 NOTEs

ERROR/WARNING:
* PDF manual failed to build locally due to missing TeX font `tctt0900` (pdflatex).
  This is a local TeX installation issue; CRAN builders with full TeX should succeed.

NOTEs:
* URL checks failed locally due to no internet access (libcurl could not resolve github.com).
* unable to verify current time (local environment)
* README/NEWS check requires pandoc (not installed locally)
* HTML manual validation skipped: HTML Tidy not available locally
* Suggested package not available for checking: writexl (offline)
