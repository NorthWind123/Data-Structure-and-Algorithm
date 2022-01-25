
# 快速排序原位修改
quick_sort_inplace <- function(x) {
    n <- length(x)
    if (n <= 1) {
        return(x)
    }
    i <- 1
    j <- n
    val <- x[1]
    while(TRUE) {
        while(x[j] >= val) {
            if (j == 1) {
                break
            }
            j <- j - 1
        }
        while (x[i] <= val) {
            if (i == n) {
                break
            }
            i <- i + 1
        }
        if (i >= j) {
            break
        }
        temp <- x[i]
        x[i] <- x[j]
        x[j] <- temp
    }
    temp <- x[1]
    x[1] <- x[j]
    x[j] <- temp
    if (j == 1) {
        return(c(val, 
                 quick_sort_inplace(x[(j+1):n])))
    }
    else if(j == n) {
        return(c(quick_sort_inplace(x[1:(j-1)]),
                 val))
    }
    return(c(quick_sort_inplace(x[1:(j-1)]),
             val,
             quick_sort_inplace(x[(j+1):n])))
}

set.seed(1)
x <- sample(10000)
print(quick_sort_inplace(x))



