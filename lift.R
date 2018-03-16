trellis.par.set(caretTheme())
cal_obj <- calibration(class ~ FDA + LDA + C5.0,
                       data = lift_results,
                       cuts = 13)