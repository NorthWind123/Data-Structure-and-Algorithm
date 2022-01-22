
# 归并排序
# 将数组递归拆分成两个子数组，对子数组进行排序
# 将两个有序数组合并为一个数组

merge_sort <- function(x) {
    n <- length(x)
    mid <- n %/% 2
    if (n <= 1) {
        return(x)
    }
    left <- merge_sort(x[1:mid])
    right <- merge_sort(x[(mid+1):n])
    sub_x <- merge_sub(left, right)
    return(sub_x)
}

merge_sub <- function(left, right) {
    n_left <- length(left)
    n_right <- length(right)
    a <- vector(length = (n_left+n_right))
    n_a <- length(a)
    i <- 1
    j <- 1
    for (k in 1:n_a) {
        if (i > n_left) {
            a[k] <- right[j]
            j <- j+1
        }
        else if (j > n_right) {
            a[k] <- left[i]
            i <- i + 1
        }
        else if (left[i] < right[j]) {
            a[k] <- left[i]
            i <- i + 1
        }
        else {
            a[k] <- right[j]
            j <- j + 1
        }
    }
    return(a)
}

set.seed(1)
arr <- sample(10000)
print(merge_sort(arr))