
binary_search <- function(arr, value) {
  count <- 0
  left <- 1
  right <- length(arr)
  while (left <= right) {
    mid <- (left + right) %/% 2
    count <- count + 1
    if (arr[mid] == value) {
      cat("共查找", count, "次\n")
      return(mid)
    }
    else if (arr[mid] < value) {
      left <- mid + 1
    }
    else {
      right <- mid - 1
    }
  }
  return(NULL)
}

a <- 1:100
print(binary_search(a, 52))



