
count <- 0
hanoi <- function(n, a, b, c) {
  if (n > 0) {
    hanoi(n-1, a, c, b)
    count <<- count + 1
    cat("第", count, "次移动: moving from", a, "to", c,"\n")
    hanoi(n-1, b, a, c)
  }
}

hanoi(10, "A", "B", "C")

