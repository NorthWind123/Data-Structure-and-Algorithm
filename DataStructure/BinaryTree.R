source("DataStructure/Queue.R", echo = FALSE)

library(R6)
library(lobstr)

BinaryTreeNode <- R6Class(
    "BinaryTreeNode",
    public = list(
        val = NULL,
        lchild = NULL,
        rchild = NULL,
        parent = NULL,
        initialize = function(val) {
            self$val <- val
        }
    )
)

BinaryTree <- R6Class(
    "BinaryTree",
    public = list(
        root = NULL,
        initialize = function(arr) {
            # 初始化二叉树
            # 插入元素
            if (!missing(arr)) {
                for (val in arr) {
                    self$insert_loop(val)
                    # self$root <- self$insert_rec(self$root, val)
                }
            }
        },
        
        pre_order_rec = function(root) {
            # 前序遍历
            if (!is.null(root)) {
                cat(root$val, " ")
                self$pre_order_rec(root$lchild)
                self$pre_order_rec(root$rchild)
            }
        },
        
        in_order_rec = function(root) {
            # 中序遍历
            if (!is.null(root)) {
                self$in_order_rec(root$lchild)
                cat(root$val, " ")
                self$in_order_rec(root$rchild)
            }
        },
        
        post_order_rec = function(root) {
            # 后序遍历
            if (!is.null(root)) {
                self$post_order_rec(root$lchild)
                self$post_order_rec(root$rchild)
                cat(root$val, " ")
            }
        },
        
        layer_order = function(root) {
            # 层次遍历
            qe <- Queue$new()
            qe$enqueue(root)
            while (!qe$is_empty()) {
                node <- qe$dequeue()
                cat(node$val, " ")
                qe$enqueue(node$lchild)
                qe$enqueue(node$rchild)
            }
        },
        
        insert_loop = function(val) {
            # 循环插入
            p <- self$root
            if (is.null(self$root)) {
               self$root <- BinaryTreeNode$new(val)
                return()
            }
            while(TRUE) {
                if (val <= p$val) {
                    if (is.null(p$lchild)) {
                        p$lchild <- BinaryTreeNode$new(val)
                        p$lchild$parent <- p
                        return()
                    }
                    else {
                        p <- p$lchild
                    }
                }
                else if(val > p$val) {
                    if (is.null(p$rchild)) {
                        p$rchild <- BinaryTreeNode$new(val)
                        p$rchild$parent <- p
                        return()
                    }
                    else {
                        p <- p$rchild
                    }
                }
                else {
                    return()
                }
            }
        },
        
        insert_rec = function(node, val) {
            # 递归插入
            if (is.null(node)) {
                node <- BinaryTreeNode$new(val)
            }
            else if(val <= node$val) {
                node$lchild <- self$insert_rec(node$lchild, val)
                node$lchild$parent <- node
            }
            else if(val > node$val) {
                node$rchild <- self$insert_rec(node$rchild, val)
                node$rchild$parent <- node
            }
            return(node)
        },
        
        query_rec = function(node, val) {
            # 递归查找
            if (is.null(self$root)) {
                print("树为空!")
                return(NULL)
            }
            if (is.null(node)) {
                print("不存在此节点!")
                return(NULL)
            }
            if (node$val == val) {
                return(node)
            }
            else if (node$val < val) {
                return(self$query_rec(node$rchild, val))
            }
            else {
                return(self$query_rec(node$lchild, val))
            }
        },
        
        query_loop = function(val) {
            # 递归查找
            if (is.null(self$root)) {
                print("树为空!")
                return(NULL)
            }
            p <- self$root
            while (!is.null(p)) {
                if (p$val == val) {
                    return(p)
                }
                else if (p$val < val) {
                    p <- p$rchild
                }
                else {
                    p <- p$lchild
                }
            }
            print("不存在此节点!")
            return(NULL)
        },
        
        delete = function(node, val) {
            node <- self$query_rec(node, val)
            if (is.null(node)) {
                return(NULL)
            }
            if (is.null(node$lchild) && is.null(node$rchild)) {
                # 叶节点,无子节点
                self$delete_no_child(node)
            }
            else if (is.null(node$rchild)) {
                # 只有左子节点
                self$delete_only_lchild(node)

            }
            else if (is.null(node$lchild)) {
                # 只有右子节点
                self$delete_only_rchild(node)
                
            }
            else {
                # 既有左子节点又有右子节点
                # 找寻右子树的最左节点或左子树的最右节点
                # 将当前节点值替换为上述找寻的节点值
                # 再删去上述节点
                right_min_node <- node$rchild
                while (!is.null(right_min_node$lchild)) {
                    right_min_node <- right_min_node$lchild
                }
                node$val <- right_min_node$val # 修改值即可
                if (is.null(right_min_node$rchild)) {
                    self$delete_no_child(right_min_node)
                }
                else {
                    self$delete_only_rchild(right_min_node)
                }
            }
         },
        
        delete_no_child = function(node) {
            # 叶节点,无子节点
            if (is.null(node$parent)) {
                # 为根节点
                self$root <- NULL
            }
            else if (obj_addr(node) == obj_addr(node$parent$lchild)) {
                # 父节点的左子节点
                node$parent$lchild <- NULL
            }
            else if (obj_addr(node) == obj_addr(node$parent$rchild)) {
                # 父节点的右子节点
                node$parent$rchild <- NULL
            }
        },
        
        delete_only_lchild = function(node) {
            # 只有左子节点
            if (is.null(node$parent)) {
                # 为根节点
                self$root <- self$root$lchild
                self$root$parent <- NULL
            }
            else if (obj_addr(node) == obj_addr(node$parent$lchild)) {
                # 父节点的左子节点
                node$parent$lchild <- node$lchild
                node$lchild$parent <- node$parent
            }
            else if (obj_addr(node) == obj_addr(node$parent$rchild)) {
                # 父节点的右子节点
                node$parent$rchild <- node$lchild
                node$lchild$parent <- node$parent
            }
        },
        
        delete_only_rchild = function(node, val) {
            # 只有右子节点
            if (is.null(node$parent)) {
                # 为根节点
                self$root <- self$root$rchild
                self$root$parent <- NULL
            }
            else if (obj_addr(node) == obj_addr(node$parent$lchild)) {
                # 父节点的左子节点
                node$parent$lchild <- node$rchild
                node$rchild$parent <- node$parent
            }
            else if (obj_addr(node) == obj_addr(node$parent$rchild)) {
                # 父节点的右子节点
                node$parent$rchild <- node$rchild
                node$rchild$parent <- node$parent
            }
        }
    
    )
    
)


t <- BinaryTree$new(c(4, 2, 1, 3, 6, 5, 7))
t$pre_order_rec(t$root)
cat("\n")
t$in_order_rec(t$root)
cat("\n")
t$post_order_rec(t$root)
cat("\n")
t$layer_order(t$root)
cat("\n")
cat("======================================================\n")
t$delete(t$root, 4)
t$pre_order_rec(t$root)
cat("\n")
t$in_order_rec(t$root)
cat("\n")
t$post_order_rec(t$root)
cat("\n")
t$layer_order(t$root)
cat("\n")





