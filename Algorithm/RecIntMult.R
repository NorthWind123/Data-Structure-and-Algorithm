
# 整数相乘
RecIntMult <- function(x, y) {
    # x, y ：要相乘的整数
    n <- nchar(x) # 整数长度,默认为2的整数次方
    if (n == 1) {
        return(x * y)
    }
    # 将每个整数分为两部分
    # x * y = ((a * 10^(n/2)) + b) * ((c * 10^(n/2)) + d)
    # x * y = (10^n * ac + 10^(n/2) * (ad + bc) + bd)
    a <- as.integer(substr(x, 1, n/2))
    b <- as.integer(substr(x, n/2+1, n))
    c <- as.integer(substr(y, 1, n/2))
    d <- as.integer(substr(y, n/2+1, n))
    ac <- RecIntMult(a, c)
    ad <- RecIntMult(a, d)
    bc <- RecIntMult(b, c)
    bd <- RecIntMult(b, d)
    return(10^n * ac + 10^(n / 2) * (ad + bc) + bd)
}

Karatsuba <- function(x, y) {
    # RecIntMult算法的改进版本
    n <- nchar(x)  # 整数长度,默认为2的整数次方
    if (n == 1) {
        return(x * y)
    }
    # 将每个整数分为两部分
    # x * y = ((a * 10^(n/2)) + b) * ((c * 10^(n/2)) + d)
    # x * y = (10^n * ac + 10^(n/2) * (ad + bc) + bd)
    a <- as.integer(substr(x, 1, n/2))
    b <- as.integer(substr(x, n/2+1, n))
    c <- as.integer(substr(y, 1, n/2))
    d <- as.integer(substr(y, n/2+1, n))
    # ad和bc不需要单独计算, 只关心(ad + bc)的值
    # ad + bc = (a + b) * (c + d) - ac - bd
    # 只需递归计算三个值
    ac <- Karatsuba(a, c)
    ab_cd <- Karatsuba(a+b, c+d)
    bd <- Karatsuba(b, d)
    ad_bc <- ab_cd - ac - bd
    return(10^n * ac + 10^(n / 2) * ad_bc + bd)
}

# > 1335 * 4256
# [1] 5681760
# > RecIntMult(1335, 4256)
# [1] 5681760
# > Karatsuba(1335, 4256)
# [1] 5681760



find_max_num <- function(arr) {
    max <- arr[1]
    sec <- arr[2]
    n <- length(arr)
    for (i in 2:n) {
        if (arr[i] > max) {
            index <- i
            sec <- max
            max <- arr[i]
        }
        else if(arr[i] > sec) {
            sec <- arr[i]
        }
    }
    cat("max:", max, " sec:", sec, "\n")
    if (max >= sec * 2) {
        return(index)
    }
    else {
        return(-1)
    }
}
