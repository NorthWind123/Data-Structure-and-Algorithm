
# 快速排序
# 快速排序采用分而治之策略,递归式进行
# 1：首先在数组中选择一个元素作为基准值
# 2：在剩下的数组中将小于基准值的元素分为左组，大于基准值的元素放在右组
# 3：在左右两个组中再次选择基准值进行分组，直到满足基线条件
# 4：最后合并各数组

quick_sort <- function(arr) {
    if (length(arr) < 2) {
        return(arr)
    }
    else {
        base_value <- arr[1]
        left <- arr[-1][arr[-1] <= base_value]
        right <- arr[-1][arr[-1] > base_value]
        return(c(quick_sort(left), base_value, quick_sort(right)))
    }
}

arr <- sample(10000)
print(arr)
print(quick_sort(arr))



