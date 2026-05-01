# --- Data ---
radio_barplot <- function(){
  subtests <- c("Verbal", "Memory", "Proc.Speed", "Attention", "Spatial", "Executive")
  
  scores <- matrix(
    c(115, 108, 92, 85, 118, 97,   # Subject A
      88,  72, 105, 110, 95, 68), # Subject B
    nrow = 2, byrow = TRUE,
    dimnames = list(c("Subject A", "Subject B"), subtests)
  )
  
  colors_fill   <- c(rgb(0.2, 0.5, 0.8, 0.2), rgb(0.8, 0.3, 0.2, 0.2))
  colors_border <- c(rgb(0.2, 0.5, 0.8, 0.9), rgb(0.8, 0.3, 0.2, 0.9))
  
  # --- Layout: side by side ---
  par(mfrow = c(1, 2), mar = c(4, 4, 4, 2))
  
  # ============================================================
  # RADAR CHART (base R, manual)
  # ============================================================
  n      <- length(subtests)
  angles <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]  # evenly spaced, no repeat
  
  score_min <- 65   # set below lowest possible score to give margin (Subject B Executive = 68)
  score_max <- 130
  r_min     <- 0.15 # minimum plot radius — keeps lowest scores off the center
  
  # Helper: polar -> cartesian
  polar2cart <- function(r, theta) {
    list(x = r * cos(theta), y = r * sin(theta))
  }
  
  # Normalize score to [r_min, 1] for plotting radius
  normalize <- function(x) r_min + (1 - r_min) * (x - score_min) / (score_max - score_min)
  
  # Set up blank plot
  par(mar=c(0,3,4,3))
  
  plot.new()
  plot.window(xlim = c(-1.6, 1.6), ylim = c(-1.4, 1.4), asp = 1)
  title("Cognitive Profile — Radar", font.main = 2)
  
  # Grid circles
  grid_levels <- seq(score_min, score_max, by = 15)
  for (g in grid_levels) {
    r <- normalize(g)
    theta_seq <- seq(0, 2 * pi, length.out = 200)
    lines(r * cos(theta_seq), r * sin(theta_seq), col = "grey80", lty = 1)
    text(0, r, labels = g, cex = 0.6, col = "grey50")
  }
  
  # Axis spokes
  for (i in seq_len(n)) {
    p <- polar2cart(1, angles[i])
    lines(c(0, p$x), c(0, p$y), col = "grey70")
    # Labels — push slightly beyond the edge
    lab <- polar2cart(1.18, angles[i])
    text(lab$x, lab$y, labels = subtests[i], cex = 0.78, font = 1)
  }
  
  # Plot each profile
  for (s in seq_len(nrow(scores))) {
    r_vals  <- normalize(scores[s, ])
    pts     <- mapply(polar2cart, r_vals, angles, SIMPLIFY = FALSE)
    xs      <- c(sapply(pts, `[[`, "x"), pts[[1]]$x)  # close polygon
    ys      <- c(sapply(pts, `[[`, "y"), pts[[1]]$y)
    polygon(xs, ys, col = colors_fill[s], border = NA)
    lines(xs, ys, col = colors_border[s], lwd = 2)
    points(xs[-length(xs)], ys[-length(ys)], pch = 19,
           col = colors_border[s], cex = 0.9)
  }
  
  legend("topright", legend = rownames(scores),
         col = colors_border, lwd = 2, bty = "n", cex = 0.85, pch=19)
  
  # ============================================================
  # BAR CHART (base R)
  # ============================================================
  par(mar = c(6, 4, 4, 2))
  
  bp <- barplot(
    scores,
    beside      = TRUE,
    col         = colors_border,
    ylim        = c(0, 180),
    las         = 2,
    cex.names   = 0.78,
    ylab        = "Standard Score",
    main        = "Cognitive Profile — Bar Chart",
    font.main   = 2,
    border      = NA,
    space       = c(0.1, 0.6)
  )
  
  # Mean reference line
  abline(h = 100, lty = 2, col = "grey40", lwd = 1.5)
  mtext("Mean (100)", side = 4, at = 110, cex = 0.65, col = "grey40", las = 1, adj=0.5)
  
  # Score labels on bars
  for (s in seq_len(nrow(scores))) {
    text(bp[s, ], scores[s, ] + 2, labels = scores[s, ],
         cex = 0.7, col = "grey20", font = 2)
  }
  
  legend("topright", legend = rownames(scores),
         fill = colors_border, bty = "n", cex = 0.85, border = NA)
}