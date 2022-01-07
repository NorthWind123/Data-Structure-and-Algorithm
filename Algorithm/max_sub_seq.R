
# 最大子串

max_seq1 <- function(vec) {
  # 算法1
  max_sum <- 0
  n <- length(vec)
  for (i in 1:n) {
    for (j in i:n) {
      this_sum <- 0
      for (k in i:j) {
        this_sum <- this_sum + vec[k]
      }
      if (this_sum > max_sum) {
        max_sum <- this_sum
      }
    }
  }
  return(max_sum)
}

max_seq2 <- function(vec) {
  # 算法2
  max_sum <- 0
  n <- length(vec)
  for (i in 1:n) {
    this_sum <- 0
    for (j in i:n) {
      this_sum <- this_sum + vec[j]
      if (this_sum > max_sum) {
        max_sum <- this_sum
      }
    }
  }
  return(max_sum)
}

max_seq3 <- function(vec) {
  # 算法3
  max_sum <- 0
  n <- length(vec)
  this_sum <- 0
  for (i in vec) {
    this_sum <- this_sum + i
    if (this_sum > max_sum) {
      max_sum <- this_sum
    }
    else if (this_sum < 0) {
      this_sum <- 0
    }
  }
  return(max_sum) 
}

vec <- c(-2, 11, -4, 13, -5, -2)
print(max_seq1(vec))
print(max_seq2(vec))
print(max_seq3(vec))

  
  
  
  
  
  
  
  
  
  










