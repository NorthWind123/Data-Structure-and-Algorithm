
library(R6)
library(purrr)

# 栈的实现
Stack <- R6Class(
    "Stack",
    public = list(
        items = list(),
      
        is_empty = function() {
            # 是否为空
            return(length(self$items) == 0)
        },
      
        size = function() {
            # 栈的大小
            return(length(self$items))
        },
      
        push = function(item) {
            # 入栈
            self$items <- c(self$items, item)
        },
      
        pop = function() {
            # 出栈
            if (self$is_empty()) {
            stop("Can't pop  value from empty stack!",
                 call. = FALSE)
            }
            n <- self$size()
            item <- self$items[[n]]
            self$items <- self$items[-n]
            return(item)
        },
        
        peek = function() {
            # 返回栈顶元素
            n <- self$size()
            item <- self$items[[n]]
            return(item)
        },
      
        iter = function() {
            # 遍历
            for (i in seq_along(self$items) ) {
                cat(self$items[[i]], " ")
            }
            cat("\n")
        }
    )
)

s <- Stack$new()
for (i in 1:10) {
    s$push(i)
}

s$iter()
print(s$peek())
s$pop()
s$pop()
print(s$is_empty())
print(s$size())
s$iter()
print(s$peek())








