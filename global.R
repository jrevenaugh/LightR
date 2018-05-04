# Plotting structures ----------------------------------------------------------
a <- cos(pi / 6)
b <- sin(pi / 6)

diagonals <- data.frame(x = c(0, 4, NA, 0.5, 3.5, NA, 1, 3, NA, 1.5, 2.5, NA,
                              2, 4, NA, 1.5, 3, NA, 1, 2, NA, 0.5, 1, NA,
                              0, 2, NA, 1, 2.5, NA, 2, 3, NA, 3, 3.5),
                        y = c(0, 0, NA, a, a, NA, 2 * c(a, a), NA, 3 * c(a, a), NA,
                              4 * a, 0, NA, 3 * a, 0, NA, 2 * a, 0, NA, a, 0, NA,
                              0, 4 * a, NA, 0, 3 * a, NA, 0, 2 * a, NA, 0, a))

triangle <- data.frame( x = c(-a, 2, 4 + a, -a, 2),
                        y = c(-b, 4 * a + 1, -b, -b, 4 * a + 1))

mask1 <- data.frame( x = c(-a, 2, 4 + a, 5, 5, -1, -1, -a),
                     y = c(-b, 4 * a + 1, -b, -b, 5, 5, -b, -b))
mask2 <- data.frame( x = c(-1, 5, 5, -1, -1),
                     y = c(-b, -b, -1, -1, -b))

winningColor <- "deepskyblue1"

diffScale <- c(0.2, 0.4, 0.6, 0.8)
