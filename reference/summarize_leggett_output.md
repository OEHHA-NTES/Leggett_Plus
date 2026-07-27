# Summarize Leggett+ simulation output

Generates compartment-level summaries (value at a target time, maxima,
and area under the curve) from a time series produced by
\[run_leggett_model()\].

## Usage

``` r
summarize_leggett_output(
  time_series,
  time_point,
  blood_volume_dl,
  plasma_volume_dl,
  rbc_volume_dl
)
```

## Arguments

- time_series:

  Data frame returned in the \`time_series\` element of
  \[run_leggett_model()\].

- time_point:

  Time (days) at which to extract compartment values.

- blood_volume_dl:

  Total blood volume (deciliters); used for derived BLL.

- plasma_volume_dl:

  Plasma volume (deciliters); used for derived plasma concentrations.

- rbc_volume_dl:

  Red blood cell volume (deciliters). Included for completeness.

## Value

Data frame of summary statistics per compartment.

## Examples

``` r
schedule <- data.frame(
  duration = 2,
  Occ.background.ug.d = 0,
  NonOcc.background.ug.d = 0,
  Occ.pb.ug.m3 = 0,
  NonOcc.pb.ug.m3 = 0,
  Occ.breath.rate.m3.d = 1,
  NonOcc.breath.rate.m3.d = 1,
  work.hour.perweek = 0,
  oral.intake.ug.d = 0
)
res <- run_leggett_model(schedule)
summarize_leggett_output(
  res$time_series,
  time_point = 1,
  blood_volume_dl = res$metadata$blood_volume_dl,
  plasma_volume_dl = res$metadata$plasma_volume_dl,
  rbc_volume_dl = res$metadata$rbc_volume_dl
)
#>                                Compartment Value Max Time_of_Max_days AUC
#> 1                 Plasma (diffusible) (ug)     0   0                0   0
#> 2                 Extracellular fluid (ug)     0   0                0   0
#> 3                     Red blood cells (ug)     0   0                0   0
#> 4                      Plasma (bound) (ug)     0   0                0   0
#> 5               Cortical bone surface (ug)     0   0                0   0
#> 6        Cortical bone (exchangeable) (ug)     0   0                0   0
#> 7    Cortical bone (non-exchangeable) (ug)     0   0                0   0
#> 8             Trabecular bone surface (ug)     0   0                0   0
#> 9      Trabecular bone (exchangeable) (ug)     0   0                0   0
#> 10 Trabecular bone (non-exchangeable) (ug)     0   0                0   0
#> 11             Liver (high perfusion) (ug)     0   0                0   0
#> 12              Liver (low perfusion) (ug)     0   0                0   0
#> 13                Soft tissue (rapid) (ug)     0   0                0   0
#> 14         Soft tissue (intermediate) (ug)     0   0                0   0
#> 15                 Soft tissue (slow) (ug)     0   0                0   0
#> 16                              Brain (ug)     0   0                0   0
#> 17                     Kidney (other) (ug)     0   0                0   0
#> 18                    Small intestine (ug)     0   0                0   0
#> 19              Kidney (urinary path) (ug)     0   0                0   0
#> 20            Large intestine (upper) (ug)     0   0                0   0
#> 21            Large intestine (lower) (ug)     0   0                0   0
#> 22                      Blood Lead (ug/dL)     0   0                0   0
#> 23             Red Blood Cell Lead (ug/dL)     0   0                0   0
#> 24                     Plasma Lead (ug/dL)     0   0                0   0
```
