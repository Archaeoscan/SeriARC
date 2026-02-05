# ellipse_helper.R - Confidence ellipse calculation for Bootstrap
# Extracted from mod_bootstrap.R (FIX 9)

# Confidence ellipse calculation from covariance matrix
# @param cov_matrix 2x2 covariance matrix
# @param center_x X-coordinate of center
# @param center_y Y-coordinate of center
# @param confidence_level Confidence level (default: 0.95)
# @param npoints Number of points for ellipse (default: 100)
# @return data.frame with x,y coordinates of ellipse or NULL on error
calculate_ellipse_from_cov <- function(cov_matrix, center_x, center_y, confidence_level = 0.95, npoints = 100) {
  if (!is.matrix(cov_matrix) || any(dim(cov_matrix) != 2L)) return(NULL)
  
  tryCatch({
    e <- eigen(cov_matrix)
    evals <- pmax(e$values, 0)
    chi2  <- stats::qchisq(confidence_level, df = 2)
    a <- sqrt(evals[1] * chi2)
    b <- sqrt(evals[2] * chi2)
    ang <- atan2(e$vectors[2, 1], e$vectors[1, 1])
    t  <- seq(0, 2 * pi, length.out = npoints)
    cx <- a * cos(t)
    cy <- b * sin(t)
    ca <- cos(ang)
    sa <- sin(ang)
    
    data.frame(
      x = center_x + cx * ca - cy * sa,
      y = center_y + cx * sa + cy * ca,
      stringsAsFactors = FALSE
    )
  }, error = function(e) NULL)
}
