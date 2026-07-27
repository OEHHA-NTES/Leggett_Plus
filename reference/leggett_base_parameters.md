# Build the base Leggett+ parameter set

Computes static physiological values and the initial state for the
Leggett+ pharmacokinetic model.

## Usage

``` r
leggett_base_parameters(
  body_weight_kg,
  hematocrit,
  age_years,
  initial_bll_ug_dl,
  inhalation_transfer = 0.3,
  abs_ratio = 0.2
)
```

## Arguments

- body_weight_kg:

  Body weight (kg). Model defaults and calibration constants are
  referenced to adult male physiology.

- hematocrit:

  Fractional hematocrit (0-1). Model defaults and calibration constants
  are referenced to adult male physiology; no female-specific range is
  implemented.

- age_years:

  Age in years.

- initial_bll_ug_dl:

  Initial blood lead level (micrograms per deciliter).

- inhalation_transfer:

  Fraction of inhaled lead transferred to plasma.

- abs_ratio:

  Oral absorption ratio.

## Value

List containing derived volumes, the initial state vector, and the ODE
parameter list.

## Male-reference physiology

The physiological defaults and calibration constants used here (e.g. the
RBC binding saturation constant, calibrated against a 73 kg body weight
and 0.3802 hematocrit) are derived from adult male / male-dominated
occupational-worker data (Leggett, 1993, and subsequent updates). There
is no female-specific parameter set (e.g. a lower hematocrit range or
pregnancy-related blood-volume expansion); results should not be assumed
to represent female physiology.

## Examples

``` r
params <- leggett_base_parameters(
  body_weight_kg = 73,
  hematocrit = 0.38,
  age_years = 62.45,
  initial_bll_ug_dl = 1
)
params$blood_volume_dl
#> [1] 54.2025
```
