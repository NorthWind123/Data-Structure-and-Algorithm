
bubble_sort <- function(x, desc = FALSE) {
  count <- 0
  if (!desc) {
    for (i in 1:(length(x) - 1)) {
      for (j in 1:(length(x) - 1)) {
        if (x[j] > x[j+1]) {
          flag <- x[j]
          x[j] <- x[j+1]
          x[j+1] <- flag
          count <- count + 1
        }
      }
    }
    cat("共",count,"次\n")
    cat("升序:",x,"\n")
  }
  else {
    for (i in 1:(length(x) - 1)) {
      for (j in 1:(length(x) - 1)) {
        if (x[j] < x[j+1]) {
          flag <- x[j]
          x[j] <- x[j+1]
          x[j+1] <- flag
          count <- count + 1
        }
      }
    }
    cat("共",count,"次\n")
    cat("降序:",x,"\n")
  }
}

set.seed(1)
x <- sample(10000)
print(system.time(bubble_sort(x)))











