
bubble_sort <- function(x) {
    n <- length(x)
    i <- 1
    flag <- TRUE
    count <- 0
    while (i < n && flag) {
        flag <- FALSE
        if (!flag) {
          for (j in 1:(n - i)) {
              if (x[j] > x[j+1]) {
                  temp <- x[j]
                  x[j] <- x[j+1]
                  x[j+1] <- temp
                  flag <- TRUE
                  count <- count + 1
              }
          }
          if (!flag) {
            cat("共遍历", i, "趟,", "共交换", count, "次\n")
            return(x)
          }
        }
        i <- i + 1
    }
    cat("共遍历", i-1, "趟 ", "共交换", count, "次\n")
    return(x)
}

set.seed(1)
x <- sample(10000)
print(system.time(bubble_sort(x)))









