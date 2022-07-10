
# 部分数组求和
range_sum <- function(arr, left, right) {
    n <- length(arr)
    # 设置前缀数组
    # 前缀数组第n位等于原数组前n项和
    pre_arr <- vector(length = n) 
    pre_arr[1] <- arr[1]
    for (i in 2:n) {
        pre_arr[i] <- pre_arr[i-1] + arr[i]
    }
    if (left == 1) {
        return(pre_arr[right])
    }
    else {
        return(pre_arr[right] - pre_arr[left - 1])
    }
}

set.seed(1)
arr <- sample(10)
print(range_sum(arr, 2, 5))
 
# > arr
# [1]  9  4  7  1  2  5  3 10  6  8
# > print(range_sum(arr, 2, 5))
# [1] 14