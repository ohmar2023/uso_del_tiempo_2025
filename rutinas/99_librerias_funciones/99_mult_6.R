
mult_6 <- function(n, N){
  n1 = ifelse(n %% 6 == 1, n - 1, 
              ifelse(n %% 6 == 0, n, n + (6 - n %% 6)))
  
  n1 = ifelse(n1 <= N,n1, n)
  return(n1)
}
