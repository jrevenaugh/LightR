# Perform gaussian elimination on matrix A with RHS b.  Returns
# the reduced row-echelon matrix E (RA) and Rb.
gaussGA2 <- function(A, b) {
#  if (nrow(A) != ncol(A)) stop()
  m <- nrow(A)
  n <- m - 1

  for (i in 1:n) {
    if (A[i,i] != 1) {
      # Row swap if needed
      k <- which(A[i:m,i] == 1)
      if (length(k) == 0) next
      l <- min(k) + i - 1
      x <- A[l,]
      A[l,] <- A[i,]
      A[i,] <- x
      x <- b[l]
      b[l] <- b[i]
      b[i] <- x
    }
    # Do row summing as necessary
    for (j in (i + 1):m) {
      if (A[j,i] != 0) {
        A[j,] <- addGA2(A[j,], A[i,])
        b[j] <- addGA2(b[j], b[i])
      }
    }
  }
  return(list(E = A, r = b))
}

# Determine the null space basis for an n by n lights out board.
# If there is a null space, return the null space basis vectors.
# Otherwise return NULL
nsBasisGA2 <- function(n) {
  b <- rep(0, n^2)
  A <- buildA(n)
  X <- gaussGA2(A, b)
  E <- cbind(t(X$E), diag(n^2))
  X <- gaussGA2(E,b)
  i <- which(rowSums(X$E[,1:(n^2)]) == 0)
  if (length(i) == 0) return(NULL)
  NS <- X$E[i,(n^2+1):(2 * n^2)]
  return(NS)
}

# Do modulo 2 (GA2) addition
addGA2 <- function(x, y) {
  if (length(x) != length(y)) stop()
  z <- x
  for (i in seq_along(x)) {
    z[i] <- (x[i] + y[i]) %% 2
  }
  return(z)
}

# Construct an "A" matrix for n by n lights out board.  This is always the
# 5 lights in a "plus" shape as applied to a square board.
buildA <- function(n) {
  B <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    B[i,i] <- 1
    if (i < n) B[i+1,i] <- 1
    if (i > 1) B[i-1,i] <- 1
  }
  I <- diag(5)
  m <- n^2
  A <- matrix(0, nrow = m, ncol = m)
  for (i in 1:m) {
    br <- (i - 1) %/% n + 1
    ir <- (i - 1) %% n + 1
    for (j in 1:m) {
      bc <- (j - 1) %/% n + 1
      ic <- (j - 1) %% n + 1
      if (br == bc) A[i,j] <- B[ir,ic]
      else if (abs(br - bc) == 1) A[i,j] <- I[ir,ic]
      else A[i,j] <- 0
    }
  }
  return(A)
}

# Solve an even determined GA2 problem.  Not needed for lights out.
solveGA2 <- function(A, b) {
  g <- gaussGA2(A, b)
  E <- g$A
  b <- g$b
  n <- nrow(E)
  x <- rep(0, ncol(E))
  i <- n - 1
  x[n] <- b[n]
  while(i > 0) {
    if (E[i,i] != 0)
    x[i] <- (b[i] + t(E[i,(i+1):n]) %*% x[(i+1):n]) %% 2
    i <- i - 1
  }
  return(x)
}

# Matrix-vector multiplication for GA2.
mmultGA2 <- function(A, b) {
  x <- rep(0, nrow(A))
  for (i in seq_along(nrow(A))) {
    x[i] <- (t(A[i,]) %*% b) %% 2
  }
  return(x)
}

# Given a b, first check to see if it has a solution.  Do this by obtaining
# the null space basis and forming inner products with b.  If they are all
# zero (modulo 2), then there is a solution.  Another check is to do
# the Gaussian elimination.  If the last n rows (where n is the dimension of
# the null space) of Rb are 0, there is a solution and, in fact, the solution
# Rb (ARb = Ax = 0).
#
# Then form additional solutions using combinations of x (Rb) and null space
# basis vectors.  For the 5 by 5 LO square, n = 2, so there are four solutions
# x, x + n1, x + n2, x + n1 + n2.  The one with the least 1s is the minimum
# solution.
#
# Say n = 3, then there are: 1 + 3 + 3 + 1 solutions = 2^n.
#
# Is it possible to construct a starting grid that is guaranteed to be a solution?
# Yes, but since roughly (1/2)^n will be solutions, it's probably efficient enough
# to generate trials grids and test them.
#
# If you start with a winnable grid, it will remain winnable.
#
# If you want a hint, solve the grid and provide any of the non-zero elements of
# the solution as the hint.  Order doesn't matter.