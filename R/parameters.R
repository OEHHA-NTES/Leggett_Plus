#' Build the base Leggett+ parameter set
#'
#' Computes static physiological values and the initial state for the Leggett+
#' pharmacokinetic model.
#'
#' @section Male-reference physiology:
#' The physiological defaults and calibration constants used here (e.g. the
#' RBC binding saturation constant, calibrated against a 73 kg body weight
#' and 0.3802 hematocrit) are derived from adult male / male-dominated
#' occupational-worker data (Leggett, 1993, and subsequent updates). There
#' is no female-specific parameter set (e.g. a lower hematocrit range or
#' pregnancy-related blood-volume expansion); results should not be assumed
#' to represent female physiology.
#'
#' @param body_weight_kg Body weight (kg). Model defaults and calibration
#'   constants are referenced to adult male physiology.
#' @param hematocrit Fractional hematocrit (0-1). Model defaults and
#'   calibration constants are referenced to adult male physiology; no
#'   female-specific range is implemented.
#' @param age_years Age in years.
#' @param initial_bll_ug_dl Initial blood lead level (micrograms per deciliter).
#' @param inhalation_transfer Fraction of inhaled lead transferred to plasma.
#' @param abs_ratio Oral absorption ratio.
#'
#' @return List containing derived volumes, the initial state vector, and the
#'   ODE parameter list.
#' @export
#' @examples
#' params <- leggett_base_parameters(
#'   body_weight_kg = 73,
#'   hematocrit = 0.38,
#'   age_years = 62.45,
#'   initial_bll_ug_dl = 1
#' )
#' params$blood_volume_dl
leggett_base_parameters <- function(
  body_weight_kg,
  hematocrit,
  age_years,
  initial_bll_ug_dl,
  inhalation_transfer = 0.30,
  abs_ratio = 0.2
) {
  day <- 1

  blood_volume_dl <- 0.7425 * body_weight_kg
  rbc_volume_dl <- blood_volume_dl * hematocrit
  plasma_volume_dl <- blood_volume_dl * (1 - hematocrit)

  list(
    body_weight_kg = body_weight_kg,
    hematocrit = hematocrit,
    age_years = age_years,
    blood_volume_dl = blood_volume_dl,
    rbc_volume_dl = rbc_volume_dl,
    plasma_volume_dl = plasma_volume_dl,
    initial_state = initialize_state(initial_bll_ug_dl, blood_volume_dl),
    ode_parameters = list(
      day = day,
      RBCV.dL = rbc_volume_dl,
      Thresh.ug.dL = 0,
      Sat.ug.dL = 270 * (73 / body_weight_kg) * (0.3802 / hematocrit),
      Flow.evf.to.plasd = 333.3 / day,
      Flow.rbc.to.plasd = 0.1390 / day,
      Flow.plasb.to.plasd = 0.1390 / day,
      Flow.csf.to.plasd = 0.50 / day,
      Flow.tsf.to.plasd = 0.50 / day,
      Flow.csf.to.cex = 0.50 / day,
      Flow.tsf.to.tex = 0.50 / day,
      Flow.cex.to.csf = 0.0185 / day,
      Flow.tex.to.tsf = 0.0185 / day,
      Flow.cex.to.cne = 0.0825 * 0.00462 / day,
      Flow.tex.to.tne = 0.061 * 0.00462 / day,
      Flow.cne.to.plasd = 0.2 * 0.0000822,
      Flow.tne.to.plasd = 0.04 * 0.000493 / day,
      Flow.livhu.to.plasd = 0.0312 / day,
      Flow.livhu.to.livlu = 0.00693 / day,
      Flow.livlu.to.plasd = 0.00190 / day,
      Flow.livhu.to.sint = 0.0312 / day,
      a1_18 = 1.059 / day,
      a20_18 = 6.0 / day,
      a21_20 = 1.85 / day,
      Flow.lintl.to.feces = 1.0 / day,
      Flow.kidurp.to.urb = 0.1390 / day,
      Flow.kidoth.to.plasd = 0.00190 / day,
      Flow.strpd.to.plasd = 2.079 / day,
      Flow.stmid.to.plasd = 0.00416 / day,
      Flow.stmid.to.excret = 0.00277 / day,
      Flow.stslo.to.plasd = 0.00038 / day,
      Flow.brn.to.plasd = 0.00095 / day,
      STOM = 0,
      urine.clearance = 0.25528,
      ur.path.dep = 1.8380522,
      inhalation.transfer = inhalation_transfer,
      abs.ratio = abs_ratio,
      mass_balance_terms = 15
    )
  )
}
