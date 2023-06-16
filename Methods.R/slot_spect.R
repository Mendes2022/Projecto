slot_spect <- function(ppm, intensity, ppm_lim = c(min(ppm), max(ppm)),
                        K = 0.67, pY = 1, dppm_max = 0.2 * (max(ppm_lim) - min(ppm_lim)),
                        asym = 1, beta = 0, cols = NULL, samples = NULL) {

  i2 <- which(ppm <= min(ppm_lim))[1]
  i1 <- length(which(ppm > max(ppm_lim)))
  ppm_sub <- ppm[i1:i2]
  specmat_sub <- intensity[, i1:i2]

  if (K == 0)
    dppm_max <- 0

  Ymax <- pY * max(specmat_sub)
  diffppm <- max(ppm_sub) - min(ppm_sub)
  Cy <- 1 - beta * (max(ppm_sub) - asym * dppm_max - ppm_sub)/(max(ppm_sub) -
    asym * dppm_max - min(ppm_sub))

  if (is.null(cols))
    cols <- grDevices::rainbow(dim(specmat_sub)[1], s = 0.8,
                               v = 0.75)

  graphics::plot(cbind(ppm_sub, specmat_sub[1, ]), xlim = rev(ppm_lim),
                 ylim = c(0, Ymax), type = "h", col = "white", xlab = "ppm",
                 ylab = "Intensity (u.a)")

  graphics::segments(max(ppm_sub) - asym * dppm_max, K * Ymax,
                     min(ppm_sub) + dppm_max, (1 - beta) * K * Ymax, col = "lightgrey")

  graphics::segments(max(ppm_sub), 0, min(ppm_sub), 0, col = "lightgrey")
  graphics::segments(max(ppm_sub), 0, max(ppm_sub) - asym *
                       dppm_max, K * Ymax, col = "lightgrey")
  graphics::segments(min(ppm_sub) + dppm_max, (1 - beta) *
                       K * Ymax, min(ppm_sub), 0, col = "lightgrey")

  if (is.null(samples)) {
    samples <- 1:dim(specmat_sub)[1]
  }

  for (i in samples) {
    dppm <- dppm_max * (i - 1)/(dim(specmat_sub)[1] - 1)
    ppmi <- ppm_sub * (1 - (1 + asym) * dppm/diffppm) +
      dppm * (1 + (1 + asym) * min(ppm_sub)/diffppm)
    y_offset <- K * Ymax * Cy * (i - 1)/(dim(specmat_sub)[1] -
      1)
    graphics::lines(cbind(ppmi, specmat_sub[i, ] + y_offset),
                    col = cols[i])
  }
}
