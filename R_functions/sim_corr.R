sim_corr <- function(n = 100, rho = 0, mean1 = 0, mean2 = 0, sd1 = 1, sd2 = 1, addx1=NULL, addx2=NULL) {
  
  # addx1 to add manually an x1 value (useful to simulate outliers)
  # addx2 to add manually an x1 value (useful to simulate outliers)
  
  if (abs(rho) >= 1) stop("rho must be in (-1, 1)")
  if (sd1 <= 0 || sd2 <= 0) stop("sd1 and sd2 must be positive")
  
  z1 <- rnorm(n)
  z2 <- rho * z1 + sqrt(1 - rho^2) * rnorm(n)
  
  x1 <- mean1 + sd1 * z1
  x2 <- mean2 + sd2 * z2
  
  x1=c(x1, addx1)
  x2=c(x2, addx2)
  
  data.frame(x1 = x1, x2 = x2)
}

plot_sim_corr <- function(plot_title=TRUE, method="pearson", ...){
  # method = person, spearman or both (only for plot title, simulation is always with pearson's R)
  
  dat = sim_corr(...)
  
  if (plot_title){
    if (method=="pearson"){
      emp_r = round(cor(dat, method="pearson"), 2)[1,2]
      plt_title <- bquote(bold("r = " ~ .(as.character(emp_r))))
    } else if (method=="spearman"){
      emp_r = round(cor(dat, method="spearman"), 2)[1,2]
      plt_title <- bquote(bold(rho ~ " = " ~ .(as.character(emp_r))))
    } else if (method=="both"){
      emp_r_1 = round(cor(dat, method="pearson"), 2)[1,2]
      emp_r_2 = round(cor(dat, method="spearman"), 2)[1,2]
      plt_title <- bquote(bold("r = " ~ .(as.character(emp_r_1)) ~ " ; " ~ rho ~ " = " ~ .(as.character(emp_r_2))))
    } else {plt_title=""}
  } else {plt_title=""}
  plot(dat[,1], dat[,2], pch=21, col="darkgray", bg="lightgray", xlab="X", ylab="Y", main = plt_title)
}