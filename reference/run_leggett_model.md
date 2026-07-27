# Run the Leggett+ lead PBPK model

Integrates a user-provided exposure schedule through the Leggett+ system
of differential equations.

## Usage

``` r
run_leggett_model(
  stage_schedule,
  body_weight_kg = 73,
  hematocrit = 0.3802,
  age_years = 62.45,
  initial_bll_ug_dl = 0,
  inhalation_transfer = 0.3,
  abs_ratio = 0.2
)
```

## Arguments

- stage_schedule:

  Data frame of exposure stages with required columns: \`duration\`,
  \`Occ.background.ug.d\`, \`NonOcc.background.ug.d\`, \`Occ.pb.ug.m3\`,
  \`NonOcc.pb.ug.m3\`, \`Occ.breath.rate.m3.d\`,
  \`NonOcc.breath.rate.m3.d\`, \`work.hour.perweek\`, and
  \`oral.intake.ug.d\`.

- body_weight_kg:

  Body weight (kg). Model defaults and calibration constants are
  referenced to adult male physiology.

- hematocrit:

  Fractional hematocrit (0-1). Model defaults and calibration constants
  are referenced to adult male physiology; no female-specific range is
  implemented.

- age_years:

  Age (years).

- initial_bll_ug_dl:

  Initial blood lead level (micrograms per deciliter).

- inhalation_transfer:

  Fraction of inhaled lead transferred to plasma.

- abs_ratio:

  Oral absorption ratio.

## Value

List with \`time_series\` (data frame of compartment masses) and
\`metadata\` describing the run.

## Male-reference physiology

The default physiological parameters (\`body_weight_kg\`,
\`hematocrit\`) and the underlying calibration constants are derived
from adult male / male-dominated occupational-worker data (Leggett,
1993, and subsequent updates). There is no female-specific parameter set
(e.g. a lower hematocrit range or pregnancy-related blood-volume
expansion); results should not be assumed to represent female
physiology.

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
result <- run_leggett_model(schedule)
names(result)
#> [1] "time_series" "metadata"   
```
