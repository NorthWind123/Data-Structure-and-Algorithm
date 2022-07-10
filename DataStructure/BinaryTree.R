library(R6)

BinaryTreeNode <- R6Class(
    "BinaryTreeNode",
    public = list(
        val = NULL,
        lchild = NULL,
        rchild = NULL,
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
            for (val in arr) {
                self$insert_loop(val)
                #self$root <- self$insert_rec(self$root, val)
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
        
        insert_loop = function(val) {
            # 循环插入
            p <- self$root
            if (is.null(self$root)) {
                self$root <- BinaryTreeNode$new(val)
                return()
            }
            while(TRUE) {
                if (val < p$val) {
                    if (is.null(p$lchild)) {
                        p$lchild <- BinaryTreeNode$new(val)
                        return()
                    }
                    else {
                        p <- p$lchild
                    }
                }
                else if(val > p$val) {
                    if (is.null(p$rchild)) {
                        p$rchild <- BinaryTreeNode$new(val)
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
            else if(val < node$val) {
                node$lchild <- self$insert_rec(node$lchild, val)
            }
            else if(val > node$val) {
                node$rchild <- self$insert_rec(node$rchild, val)
            }
            return(node)
        },
        
        remove = function(val) {
            # 删除元素
        } 
        
        
    )
)