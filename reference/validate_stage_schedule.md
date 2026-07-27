# Validate a Leggett+ exposure stage schedule

Ensures required columns exist, coerces to numeric, and rounds durations
to whole days prior to integration.

## Usage

``` r
validate_stage_schedule(stage_schedule)
```

## Arguments

- stage_schedule:

  Data frame describing exposure stages.

## Value

Cleaned data frame with numeric columns.
