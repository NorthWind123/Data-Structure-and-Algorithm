
# 选择排序
# 1.首先将当前首位数设置为最小数
# 2.将此最小数与后面的数进行比较，找到最小数与当前首位数交换位置
# 3.每轮都重复此操作，直到排序完毕

selection_sort <- function(arr) {
  for (i in 1:(length(arr)-1)) {
    smallest <- i
    for (j in (i+1):length(arr)) {
      if (arr[j] < arr[smallest]) {
        smallest <- j
      }
    }
    flag <- arr[i]
    arr[i] <- arr[smallest]
    arr[smallest] <- flag
  }
  print(arr)
}

arr <- sample(10000)
print(arr)
selection_sort(arr)














