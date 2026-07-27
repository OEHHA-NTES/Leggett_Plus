## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Notes for CRAN reviewers

* `README.md` and the vignette link to
  <https://pmc.ncbi.nlm.nih.gov/articles/PMC1519877/> (Leggett 1993). This URL
  returns HTTP 403 to non-browser user agents (a known PMC/NCBI bot-blocking
  behavior) but resolves normally (HTTP 200) in a real browser; it is not a
  dead link.

## R CMD check environments

* Local Windows 11, R 4.5.1 (release)
* GitHub Actions (`R-CMD-check.yaml`): macOS/Windows/Ubuntu release, Ubuntu
  devel, Ubuntu oldrel-1
