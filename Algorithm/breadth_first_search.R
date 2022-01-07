
# 实现一个队列
create_queue <- function() {
  return(vector())
}

# 压入
push <- function(queue, neighbors) {
  queue <- c(queue, neighbors)
  return(queue)
}

# 弹出
pop <- function(queue) {
  queue <- queue[-1]
  return(queue)
}

# 搜索邻居
select_neighbors <- function(name) {
  switch (name,
    you = c("alice", "bob", "claire"),
    alice = c("peggy"),
    bob = c("alice", "anuj", "peggy"),
    claire = c("thom", "jonny"),
    anuj = c(),
    peggy = c(),
    thom = c(),
    jonny = c()
  )
} 

#判断是否是芒果销售商
person_is_seller <- function(name) {
  return(endsWith(name, "m"))
}

# 寻找芒果销售商
search_seller <- function(name) {
  search_queue <- create_queue()
  search_queue <- push(search_queue, select_neighbors(name))
  searched <- c() # 将已检查的人放入其中
  while (length(search_queue)) {
    person <- search_queue[1]
    search_queue <- pop(search_queue)
    if (!(person  %in% searched)) { # 如果此人没被检查过，则进行检查
      if (person_is_seller(person)) {
        cat(person,"is seller!", "\n")
        return(TRUE)
      } else {
        search_queue <- push(search_queue, select_neighbors(person))
        searched <- c(searched, person)
      }
    } else {
      cat(person, "has been already ckecked!","\n")
    }
  }
  cat("sorry, nobudy is seller!", "\n")
  return(FALSE)
}

search_seller("you")













