
bubble_sort <- function(x) {
    n <- length(x)
    if (n < 2) {
        return(x)
    }
    for (i in 1:(n-1)) {
        swap <- FALSE
        for (j in 1:(n - i)) {
            if (x[j] > x[j+1]) {
                temp <- x[j]
                x[j] <- x[j+1]
                x[j+1] <- temp
                swap <- TRUE
            }
        }
        if (!swap) {
            return(x)
        }
    }
}

set.seed(1)
x <- sample(10000)
print(system.time(bubble_sort(x)))









