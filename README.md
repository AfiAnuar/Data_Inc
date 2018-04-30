
# starting points
x_ini = 0;  y_ini=0


# target points
m = 11 # x-axis
n = 7  # y-axis


# find path
path = NULL
x = 0; y = 0

a = 1
while (a == 1){
  if (x < m & y < n){  # when x and y smaller than m and n
    moveDIR <- sample(1:2, 1) #1=x-direction, 2=y-direction
    if (moveDIR == 1){
      x = x + 1
      y = y
    } else {
      x = x
      y = y + 1
    }
  } else if (x == m & y < n){  # when x reaches m but not y
    moveDIR <- 2
    y = y + 1
  } else if (x < m & y == n){  # when y reaches n but not x
    moveDIR <- 1
    x = x + 1
  } else {
    break
  }
  path <- rbind(path, data.frame(x, y))
}


D <- pmax((path$x/m)-(path$y/n), (path$y/n)-(path$x/m)) # find maximum by each row between two vectors

options(digits=10)
mean(D)
sd(D)
