#' Leggett+ differential equation system
#'
#' Returns an ODE function suitable for `deSolve::ode`.
#'
#' @param parameters Static parameters produced by [leggett_base_parameters()].
#'
#' @return Function of time, state, and stage parameters.
#' @keywords internal
leggett_ode <- function(parameters) {
  function(t, state, stage_parameters) {
    state_list <- as.list(state)
    with(c(state_list, parameters, stage_parameters), {
      occ.time <- work.hour.perweek / (7 * 24)
      Daily.OCC.Exposure <- (Occ.pb.ug.m3 *
        Occ.breath.rate.m3.d *
        inhalation.transfer *
        occ.time) +
        Occ.background.ug.d
      Daily.nonOCC.Exposure <- (NonOcc.pb.ug.m3 *
        NonOcc.breath.rate.m3.d *
        inhalation.transfer *
        (1 - occ.time)) +
        NonOcc.background.ug.d
      Daily.Oral.Exposure <- oral.intake.ug.d * abs.ratio
      Daily.Exposure <- Daily.OCC.Exposure +
        Daily.nonOCC.Exposure +
        Daily.Oral.Exposure

      RBC <- A3
      RBCCD.ug.dL <- RBC / RBCV.dL

      if (RBCCD.ug.dL > Thresh.ug.dL) {
        SF <- 0.24 *
          (1 - (RBCCD.ug.dL - Thresh.ug.dL) / (Sat.ug.dL - Thresh.ug.dL))^1.5
      } else {
        SF <- 1 * 0.24
      }
      FS <- (1 - SF) / (1 - 0.24)
      SFm <- SF / 0.24

      CU <- 1 + ((1 - urine.clearance) * FS / ur.path.dep / mass_balance_terms)

      Flow.plasd.to.evf <- CU * FS * 1000 / day
      Flow.plasd.to.rbc <- CU * SFm * 480 / day
      Flow.plasd.to.plasb <- CU * FS * 0.8 / day
      Flow.plasd.to.urb <- urine.clearance * FS * 30 / day
      Flow.plasd.to.sint <- CU * FS * 12 / day
      Flow.plasd.to.tsf <- CU * FS * 88.96 / day
      Flow.plasd.to.csf <- CU * FS * 71 / day
      Flow.plasd.to.livhu <- CU * FS * 80 / day
      Flow.plasd.to.kidurp <- urine.clearance * FS * 40 / day
      Flow.plasd.to.kidoth <- CU * FS * 0.4 / day
      Flow.plasd.to.strpd <- CU * FS * 177.5 / day
      Flow.plasd.to.stmid <- CU * FS * 10 / day
      Flow.plasd.to.stslo <- CU * FS * 2 / day
      Flow.plasd.to.brn <- CU * FS * 0.3 / day
      Flow.plasd.to.sweat <- CU * FS * 7 / day

      a1_2 <- Flow.evf.to.plasd
      a1_3 <- Flow.rbc.to.plasd
      a1_4 <- Flow.plasb.to.plasd
      a1_5 <- Flow.csf.to.plasd
      a1_7 <- Flow.cne.to.plasd
      a1_8 <- Flow.tsf.to.plasd
      a1_10 <- Flow.tne.to.plasd
      a1_11 <- Flow.livhu.to.plasd
      a1_12 <- Flow.livlu.to.plasd
      a1_13 <- Flow.strpd.to.plasd
      a1_14 <- Flow.stmid.to.plasd
      a1_15 <- Flow.stslo.to.plasd
      a1_16 <- Flow.brn.to.plasd
      a1_17 <- Flow.kidoth.to.plasd
      a1_18 <- Flow.plasd.to.sint
      a18_1 <- Flow.plasd.to.sint
      a2_1 <- Flow.plasd.to.evf
      a3_1 <- Flow.plasd.to.rbc
      a4_1 <- Flow.plasd.to.plasb
      a5_1 <- Flow.plasd.to.csf
      a5_6 <- Flow.cex.to.csf
      a6_5 <- Flow.csf.to.cex
      a7_6 <- Flow.cex.to.cne
      a8_1 <- Flow.plasd.to.tsf
      a8_9 <- Flow.tex.to.tsf
      a9_8 <- Flow.tsf.to.tex
      a10_9 <- Flow.tex.to.tne
      a11_1 <- Flow.plasd.to.livhu
      a12_11 <- Flow.livhu.to.livlu
      a13_1 <- Flow.plasd.to.strpd
      a14_1 <- Flow.plasd.to.stmid
      a15_1 <- Flow.plasd.to.stslo
      a16_1 <- Flow.plasd.to.brn
      a17_1 <- Flow.plasd.to.kidoth
      a18_11 <- Flow.livhu.to.sint
      a19_1 <- Flow.plasd.to.kidurp
      b1 <- Flow.plasd.to.urb
      b19 <- Flow.kidurp.to.urb
      e <- Flow.stmid.to.excret
      s <- Flow.plasd.to.sweat

      a1_1 <- -(a2_1 +
        a3_1 +
        a4_1 +
        a5_1 +
        a8_1 +
        a11_1 +
        a13_1 +
        a14_1 +
        a15_1 +
        a16_1 +
        a17_1 +
        a18_1 +
        a19_1 +
        b1 +
        s -
        STOM)
      a2_2 <- -a1_2
      a3_3 <- -a1_3
      a4_4 <- -a1_4
      a5_5 <- -(a6_5 + a1_5)
      a6_6 <- -(a5_6 + a7_6)
      a7_7 <- -a1_7
      a8_8 <- -(a1_8 + a9_8)
      a9_9 <- -(a8_9 + a10_9)
      a10_10 <- -a1_10
      a11_11 <- -(a1_11 + a12_11 + a18_11)
      a12_12 <- -a1_12
      a13_13 <- -a1_13
      a14_14 <- -(a1_14 + e)
      a15_15 <- -a1_15
      a16_16 <- -a1_16
      a17_17 <- -a1_17
      a18_18 <- -(Flow.plasd.to.sint + a20_18)
      a19_19 <- -b19
      a20_20 <- -a21_20
      a21_21 <- -Flow.lintl.to.feces

      dA1 <- a1_2 *
        A2 +
        a1_3 * A3 +
        a1_4 * A4 +
        a1_5 * A5 +
        a1_7 * A7 +
        a1_8 * A8 +
        a1_10 * A10 +
        a1_11 * A11 +
        a1_12 * A12 +
        a1_13 * A13 +
        a1_14 * A14 +
        a1_15 * A15 +
        a1_16 * A16 +
        a1_17 * A17 +
        a1_18 * A18 +
        Daily.Exposure +
        a1_1 * A1
      dA2 <- a2_1 * A1 + a2_2 * A2
      dA3 <- a3_1 * A1 + a3_3 * A3
      dA4 <- a4_1 * A1 + a4_4 * A4
      dA5 <- a5_1 * A1 + a5_6 * A6 + a5_5 * A5
      dA6 <- a6_5 * A5 + a6_6 * A6
      dA7 <- a7_6 * A6 + a7_7 * A7
      dA8 <- a8_1 * A1 + a8_9 * A9 + a8_8 * A8
      dA9 <- a9_8 * A8 + a9_9 * A9
      dA10 <- a10_9 * A9 + a10_10 * A10
      dA11 <- a11_1 * A1 + a11_11 * A11
      dA12 <- a12_11 * A11 + a12_12 * A12
      dA13 <- a13_1 * A1 + a13_13 * A13
      dA14 <- a14_1 * A1 + a14_14 * A14
      dA15 <- a15_1 * A1 + a15_15 * A15
      dA16 <- a16_1 * A1 + a16_16 * A16
      dA17 <- a17_1 * A1 + a17_17 * A17
      dA18 <- a18_1 * A1 + a18_11 * A11 + a18_18 * A18
      dA19 <- a19_1 * A1 + a19_19 * A19
      dA20 <- a20_18 * A18 + a20_20 * A20
      dA21 <- a21_20 * A20 + a21_21 * A21

      list(c(
        dA1,
        dA2,
        dA3,
        dA4,
        dA5,
        dA6,
        dA7,
        dA8,
        dA9,
        dA10,
        dA11,
        dA12,
        dA13,
        dA14,
        dA15,
        dA16,
        dA17,
        dA18,
        dA19,
        dA20,
        dA21
      ))
    })
  }
}
