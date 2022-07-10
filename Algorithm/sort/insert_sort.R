
# 把元素插入到索引左边的有序数组里, 左边的数组并非最终结果
# 把当前元素依次和左边的元素比较, 若当前元素更小，则交换
insert_sort <- function(x) {
    n <- length(x)
    if (n < 2) {
      return(x)
    }
    count <- 0
    for (i in 2:n) {
        # 从索引2处开始
        j <- i
        while (j > 1 && x[j] < x[j-1]) {
            # 若x[j] < x[j-1], 则交换; 否则不交换
            temp <- x[j]
            x[j] <- x[j-1]
            x[j-1] <- temp
            j <- j -1
        }
    }
    return(x)
}

set.seed(1)
x <- sample(10000)
print(insert_sort(x))


