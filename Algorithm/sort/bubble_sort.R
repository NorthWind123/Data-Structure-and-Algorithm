
bubble_sort <- function(x) {
    n <- length(x)
    if (n < 2) {
        return(x)
    }
    count <- 0
    for (i in 1:(n-1)) {
        flag <- TRUE
        for (j in 1:(n - i)) {
            if (x[j] > x[j+1]) {
                temp <- x[j]
                x[j] <- x[j+1]
                x[j+1] <- temp
                flag <- FALSE
                count <- count + 1
            }
        }
        if (flag) {
            cat("共遍历", i, "趟,", "共交换", count, "次\n")
            return(x)
        }
    }
    cat("共遍历", i, "趟 ", "共交换", count, "次\n")
    return(x)
}

set.seed(1)
x <- sample(10000)
print(system.time(bubble_sort(x)))









