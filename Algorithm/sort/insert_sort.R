
insert_sort <- function(arr) {
  count <- 0
  for (i in 2:length(arr)) {
    j <- i -1
    tmp <- arr[i]
    while((j >= 1) && (arr[j] > tmp)) {
      arr[j+1] <- arr[j]
      j <- j -1
    }
    arr[j+1] <- tmp
    count <- count +1
    cat("第",count,"躺",arr,"\n")
  }
}

arr <- sample(100^3)
cat("原数组:", arr, "\n")
insert_sort(arr)