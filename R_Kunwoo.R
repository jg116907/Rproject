sum(is.na(wt2016))
library(arules)
a_list <- list(
  c("a", "b", "c"), # 경우의 수를 파악 item
  c("a", "b"),
  c("a", "b", "d"),
  c("c", "e"),
  c("a", "b", "d", "e")
)