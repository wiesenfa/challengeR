default_colors=
  function(n = length(algorithms), algorithms = NULL) {
    # Based on ggplot2:::ScaleHue
    h <- c(0, 360) + 15
    l <- 65
    c <- 100
    
    start <-0# 1
    direction <- 1
    
    rotate <- function(x) (x + start) %% 360 * direction
    
    if ( (diff(h) %% 360) < 1 ) {
      h[2] <- h[2] - 360 / n
    }
    
    structure(grDevices::hcl(h = rotate(seq(h[1], h[2], length = n)),
                             c = c, l = l),
              names = algorithms)
  }
