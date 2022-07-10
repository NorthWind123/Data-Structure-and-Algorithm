
library(R6)

Node <- R6Class(
    "Node",
    public = list(
        val = NULL,
        nxt = NULL,
        initialize = function(val = NULL, nxt = NULL) {
            self$val <- val
            self$nxt <- nxt
        }
        
    )
)

add_two_num <- function(node1, node2) {
    head <- NULL 
    tail <- NULL
    carry <- 0
    while(!is.null(node1) || !is.null(node2)) {
        n1 <- if (!is.null(node1)) node1$val else 0
        n2 <- if (!is.null(node2)) node2$val else 0
        res <- n1 + n2 + carry
        if (is.null(head)) {
            head <- Node$new(res %% 10)
            tail <- head
        }
        else {
            tail$nxt <- Node$new(res %% 10)
            tail <- tail$nxt
        }
        carry <- res %/% 10 
        if (!is.null(node1)) node1 <- node1$nxt
        if (!is.null(node2)) node2 <- node2$nxt
    }
    if (carry > 0) {
        tail$nxt <- Node$new(carry)
        tail <- tail$nxt
    }
    return(head)
}

# l1: 2, 4, 3
l1 <- Node$new(2)
l1$nxt <- Node$new(4)
l1$nxt$nxt <- Node$new(3)

# l2: 5, 6, 4
l2 <- Node$new(5)
l2$nxt <- Node$new(6)
l2$nxt$nxt <- Node$new(4)

# l3: 7, 0, 8
l3 <- add_two_num(l1, l2)

