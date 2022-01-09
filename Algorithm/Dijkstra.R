
library(R6)
library(magrittr)


# 迪杰斯特拉算法 -----------------------------------------------------------------
Dijkstra <- R6Class(
    "Dijkstra",
    public = list(
        graph = NULL, # 存储各节点的邻居和到邻居节点权重
        costs = NULL, # 存储起点到各节点的开销
        parents = NULL, # 存储各节点的父节点
        processed = c(), # 存储已处理过的节点
        
        initialize = function(graph, costs, parents) {
            self$graph <- graph
            self$costs <- costs
            self$parents <- parents
        },
        
        lowest_cost_node = function(costs) {
            # 在未处理的节点中寻找距离起点最近的节点
            lowest_cost <- Inf
            lowest_node <- NULL
            for (node in self$costs %>% names()) {
                # 遍历costs中未处理的节点, 直到找到距离起点最近的节点
                if (!any(self$processed == node)) {
                    cost <- self$costs[[node]]
                    if (cost <= lowest_cost) {
                        lowest_cost <- cost
                        lowest_node <- node
                    }
                }
            }
            # 若所有的节点都处理过，返回NULL
            return(lowest_node)
        },
        
        Dijkstra = function()  {
            node <- self$lowest_cost_node(self$costs)
            while (!is.null(node)) {
                cost <- self$costs[[node]]
                neighbors <- self$graph[[node]] %>% names()
                # 遍历node的邻居节点，更新起点到邻居节点的距离
                for (neighbor in neighbors) {
                    new_cost <- cost + self$graph[[node]][[neighbor]]
                    if (new_cost <= self$costs[[neighbor]]) {
                        self$costs[[neighbor]] <- new_cost
                    }
                    self$parents[[neighbor]] <- node
                }
                # 加入已处理过的节点
                self$processed <- c(self$processed, node)
                node <- self$lowest_cost_node(self$costs)
            }
          cat("the shortest distance to end is: ", self$costs[["end"]])
        }
    )
)

graph <- list(
    start = list(a = 6, b = 2),
    a = list(end = 1),
    b = list(a = 3, end = 5),
    end = NULL
)

costs <- list(
    a = 6,
    b = 2,
    end = Inf
)

parents <- list(
    a = "start",
    b = "start",
    end = NULL
)
D <- Dijkstra$new(graph, costs, parents)
D$Dijkstra()





















