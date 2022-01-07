
library(R6)
library(purrr)
library(magrittr)


Queue <- R6Class(
    "Queue",
    public = list(
        items = list(),
        
        is_empty = function() {
            return(length(self$items) == 0)
        },
        
        size = function() {
            return(length(self$items))
        },
        
        enqueue = function(x) {
            self$items <- c(self$items, x)
        },
        
        dequeue = function() {
            item <- self$items[[1]]
            self$items <- self$items[-1]
            return(item)
        },
        
        iter = function() {
            for (i in seq_along(self$items)) {
                cat(self$items[[i]], " ")
            }
          cat("\n")
        }
    )
)

qe <- Queue$new()
for (i in 1:10) {
    qe$enqueue(i)
}

qe$iter()
qe$dequeue()
qe$dequeue()
qe$is_empty() %>% print()
qe$size() %>% print()
qe$iter()












