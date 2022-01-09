
library(R6)
library(purrr)
library(magrittr)

Node <- R6Class(
    "Node",
    public = list(
        item = NULL,
        after = NULL,
        initialize = function(item) {
            self$item <- item
        }
    ))

LinkList <- R6Class(
    "LinkList",
    public = list(
        head = NULL, # 头指针指向头部节点
        tail = NULL, # 尾指针指向尾部节点
        
        is_empty = function() {
            # 是否为空
            return(is.null(self$head))
        },
        
        size = function() {
            # 链表长度
            if (is.null(self$head)) {
                return(0)
            }
            else {
                count <- 0
                p <- self$head
                while (!is.null(p)) {
                    count <- count + 1
                    p <- p$after
                }
                return(count)
            }
        },
        
        insert_head = function(item) {
            # 头部插入
            node <- Node$new(item)
            if (is.null(self$head)) {
                self$head <- node
                self$tail <- node
            }
            else {
                old_head <- self$head
                node$after <- old_head
                self$head <- node
            }
        },
        
        insert_tail = function(item) {
            # 尾部插入
            node <- Node$new(item)
            if (is.null(self$head)) {
                self$head <- node
                self$tail <- node
            }
            else {
                old_tail <- self$tail
                self$tail <- node
                old_tail$after <- self$tail
            }
        },
        
        remove_head = function() {
            # 头部删除
            if (self$size() == 0) {
                stop("Can't remove item from empty list!",
                     call. = FALSE)
            }
            else {
                if (self$size() == 1) {
                    self$tail <- NULL
                }
                item = self$head$item
                self$head <- self$head$after
                return(item)
            }
            
        },
        
        reverse = function() {
            # 反转链表
            self$tail <- self$head
            pre <- NULL
            while (!is.null(self$head)) {
                after_node <- self$head$after
                self$head$after <- pre
                pre <- self$head
                self$head <- after_node
            }
            self$head <- pre
            cat("已反转!\n")
        },
        
        reverse_rec= function(node) {
            # 递归反转
            self$tail <- self$head
            if (is.null(node)) {
                return(NULL)
            }
            if (is.null(node$after)) {
                self$head <- node
                return(node)
            }
            after_node <- node$after
            rest <- self$reverse_rec(after_node)
            after_node$after <- node
            node$after <- NULL
            return(rest)
        },
              
        iter = function() {
            # 遍历链表
            if (is.null(self$head)) {
                return(NULL)
            }
            else {
                p <- self$head
                while (!is.null(p)) {
                    cat(p$item, " ")
                    p <- p$after
                }
                cat("\n")
            }
        }
    )
)

# LL1 <- LinkList$new()
# for (i in 1:10) {
#     LL1$insert_head(i)
# }
# LL1$iter()
# LL1$reverse()
# LL1$iter()
# LL1$size() %>% print()

LL2 <- LinkList$new()
for (i in 1:10) {
  LL2$insert_head(i)
}
LL2$iter()
LL2$head$item %>% print()
LL2$tail$item %>% print()
LL2$reverse()
LL2$iter()
LL2$head$item %>% print()
LL2$tail$item %>% print()
LL2$size() %>% print()
LL2$reverse_rec(LL2$head)
LL2$iter()
LL2$head$item %>% print()
LL2$tail$item %>% print()





















