# Changelog

## LeggettPlus 0.2.0

- CRAN preparation: added a `@return` tag to
  [`launch_leggett_app()`](https://oehha-ntes.github.io/Leggett_Plus/reference/launch_leggett_app.md),
  cleaned up `DESCRIPTION` (author formatting, removed an unresolved
  `arxiv` tag), added a spelling wordlist and `Language` field, expanded
  unit test coverage to ~99%, and added `lint`, `test-coverage`, and
  `pkgdown` GitHub Actions workflows alongside the existing R CMD check
  workflow.
- About tab: introduced a two-column layout pairing the app overview,
  “Intended Use and Limitations,” and “What Leggett+ Simulates” sections
  with the LeggettPlus logo, enlarged the logo, and added consistent
  spacing between sections.
- About tab: added a “Source Code & R Package” section pointing power
  users to the GitHub codebase and to installing the `LeggettPlus` R
  package directly (`remotes::install_github()`) for
  scripted/programmatic use.
- Fixed stray indentation in the About tab’s install-code snippet.

## LeggettPlus 0.1.0

- Initial CRAN submission.
- Removed all NIOSH/OSHA (and related MIOSHA/ACOEM/AOEC/CDPH/ABLES/CSTE)
  blood lead level interpretation and classification features from the
  Shiny app, including the guidance-band lookup table, the
  “Interpretation (NIOSH/OSHA)” results panel, the downloadable HRA
  note, and the corresponding About-tab guidance section and reference.
  The app and package now report simulated BLL values without applying
  occupational medical-surveillance thresholds.
- Renamed `run_app()` to
  [`launch_leggett_app()`](https://oehha-ntes.github.io/Leggett_Plus/reference/launch_leggett_app.md).
- Expanded README and vignette documentation (installation, key
  functions, compartment reference, and worked examples).
