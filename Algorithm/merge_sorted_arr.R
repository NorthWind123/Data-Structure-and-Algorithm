
merge_sorted_arr <- function(arr1, m, arr2, n) {
    # 合并有序数组
    L <- m + n
    for (i in 1:L) {
        if (m < 1) {
            arr1[L+1-i] <- arr2[n]
            n <- n -  1
        }
        else if (n < 1) {
            arr1[L+1-i] <- arr1[m]
            m <- m - 1
        }
        else if (arr1[m] < arr2[n]) {
            arr1[L+1-i] <- arr2[n]
            n <- n - 1
        }
        else {
            arr1[L+1-i] <- arr1[m]
            m <- m - 1
        }
    }
    return(arr1)
}

# > arr1 <- c(3, 4, 6, 0, 0, 0)
# > arr2 <- c(1, 2, 5)
# > merge_sorted_arr(arr1, 3, arr2, 3)
# [1] 1 2 3 4 5 6