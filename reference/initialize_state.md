# Initialize compartment masses for a target blood lead level

Converts an initial blood lead level (BLL) into initial compartment
burdens for the Leggett+ differential equation system.

## Usage

``` r
initialize_state(initial_bll_ug_dl, blood_volume_dl)
```

## Arguments

- initial_bll_ug_dl:

  Initial blood lead level (micrograms per deciliter).

- blood_volume_dl:

  Total blood volume (deciliters).

## Value

Named numeric vector of length 21 representing initial compartment
masses.
