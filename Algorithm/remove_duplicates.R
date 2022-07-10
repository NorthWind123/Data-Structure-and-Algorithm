
# 删除有序数组重复项
remove_duplicates <- function(arr) {
    n <- length(arr)
    if (n <= 1) {
        return(n)
    }
    j <- 1
    for (i in 2:n) {
        if (arr[i] != arr[j]) {
            j <- j + 1
            arr[j] <- arr[i]
        }
    }
    print(arr[1:j])
    return(j)
}

# > arr <- c(1, 1, 2)
# > remove_duplicates(arr)
# [1] 1 2
# [1] 2
# > arr <- c(0,0,1,1,1,2,2,3,3,4)
# > remove_duplicates(arr)
# [1] 0 1 2 3 4
# [1] 5
