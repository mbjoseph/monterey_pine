a <- 0.25
b <- 0.25

x <- seq(0.1, 75, length.out = 1e3)

grw <- function(a, b) {
  a * b * x
}

plot(x, grw(a, b), type = 'l')
