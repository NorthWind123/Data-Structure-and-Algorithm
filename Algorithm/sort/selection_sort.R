
# 选择排序
# 1.首先将当前首位数设置为最小数
# 2.将此最小数与后面的数进行比较，找到最小数与当前首位数交换位置
# 3.每轮都重复此操作，直到排序完毕

selection_sort <- function(arr) {
    n <- length(arr)
    if (length(arr) < 2) {
      return(arr)
    }
    for (i in 1:(n-1)) {
        smallest <- i
        for (j in (i+1):n) {
            if (arr[j] < arr[smallest]) {
                smallest <- j
            }
        }
        flag <- arr[i]
        arr[i] <- arr[smallest]
        arr[smallest] <- flag
    }
    return(arr)
}

set.seed(1)
arr <- sample(10000)
selection_sort(arr)














