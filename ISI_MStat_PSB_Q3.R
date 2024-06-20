simulate_draws <- function(verbose = FALSE) {
  colors <- c("G", "W", "B")
  balls <- c(rep("G", 7), rep("W", 5), rep("B", 6))
  
  # To keep track of obtained colors
  obtained_colors <- c()
  
  # Number of draws counter
  draws <- 0
  
  while(length(unique(obtained_colors)) < 3) {
    draw <- sample(balls, 1)
    obtained_colors <- c(obtained_colors, draw)
    draws <- draws + 1
  }
  
  if (verbose) {
    cat("Sequence of colors obtained: ", paste(obtained_colors, collapse = " "), "\n")
  }
  
  return(draws)
}

set.seed(108)
# Example of running the function with verbose output
replicate(10,simulate_draws(verbose = TRUE))
