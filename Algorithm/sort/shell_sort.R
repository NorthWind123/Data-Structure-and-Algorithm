
shell_sort <- function(x) {
    n <- length(x)
    h <- 1
    count <- 0
    while (h < floor(n/3)) {
        h <- h * 3 + 1
    }
    while (h >= 1) {
        for (i in (h+1):n) {
            j <- i
            while (j > h && x[j] < x[j-h]) {
                temp <- x[j]
                x[j] <- x[j-h]
                x[j-h] <- temp
                count <- count + 1
                j <- j - h
            }
        }
        h <- floor(h / 3)
    }
    cat("共交换", count, "次\n")
    return(x)
}

set.seed(1)
x <- sample(10000)
print(shell_sort(x))