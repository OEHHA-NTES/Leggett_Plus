#' Initialize compartment masses for a target blood lead level
#'
#' Converts an initial blood lead level (BLL) into initial compartment burdens
#' for the Leggett+ differential equation system.
#'
#' @param initial_bll_ug_dl Initial blood lead level (micrograms per deciliter).
#' @param blood_volume_dl Total blood volume (deciliters).
#'
#' @return Named numeric vector of length 21 representing initial compartment masses.
#' @keywords internal
initialize_state <- function(initial_bll_ug_dl, blood_volume_dl) {
  plasmaDp <- 0.0000
  evfp <- 0.0000
  rbcp <- 0.0279
  plasmaBp <- 0.0000
  csurfp <- 0.0010
  cexp <- 0.0207
  cnexp <- 0.5833
  tsurfp <- 0.0012
  texp <- 0.0259
  tnexp <- 0.2367
  Liv1p <- 0.0093
  Liv2p <- 0.0339
  st0p <- 0.0007
  st1p <- 0.0117
  st2p <- 0.0405
  brp <- 0.0025
  othkidp <- 0.0017
  smintp <- 0.0001
  urpathp <- 0.0023
  uLip <- 0.0002
  LLip <- 0.0004

  bburden <- c(
    plasmaDp,
    evfp,
    rbcp,
    plasmaBp,
    csurfp,
    cexp,
    cnexp,
    tsurfp,
    texp,
    tnexp,
    Liv1p,
    Liv2p,
    st0p,
    st1p,
    st2p,
    brp,
    othkidp,
    smintp,
    urpathp,
    uLip,
    LLip
  )

  state <- bburden *
    (initial_bll_ug_dl * blood_volume_dl / sum(bburden[1:4]) - bburden[2])
  names(state) <- paste0("A", seq_along(state))
  state
}
