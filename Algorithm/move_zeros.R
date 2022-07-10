
move_zeros <- function(arr) {
    # 移动0到数组末尾
    n <- length(arr)
    i <- 1
    for (j in 1:n) {
        if (arr[j] != 0 && arr[i] != 0) {
            i <- i + 1
        }
        else if (arr[j] != 0 && arr[i] == 0) {
            temp <- arr[i]
            arr[i] <- arr[j]
            arr[j] <- temp
            i <- i + 1
        }
    }
    return(arr)
}

# > arr <- c(0,1,0,2,0,3)
# > move_zeros(arr)
# [1] 1 2 3 0 0 0