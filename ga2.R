# ga2.R
#
# Functions for simple linear algebra in Galois 2 (GA2) space.  This is a binary
# field with -1, 0 and 1 that follows:
#
# 1 + 0 = 1
# 0 + 1 = 1
# 1 + 1 = 0
# 1 * 0 = 0
# 1 * 1 = 1
# -1 = 1
#
# Solvers not for general use--assumes conditions appropriate to Lights Out game.
#
# Perform gaussian elimination on matrix A with RHS b.  Returns
# the reduced row-echelon matrix E (RA) and Rb.
gaussGA2 <- function(A, b) {
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
# n lights in a "plus" shape as applied to a square board.
buildA <- function(n) {
  B <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    B[i,i] <- 1
    if (i < n) B[i+1,i] <- 1
    if (i > 1) B[i-1,i] <- 1
  }
  I <- diag(nrow = n)
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

# Solve GA2 problem encountered in Lights Out.
solveGA2 <- function(A, b) {
  g <- gaussGA2(A, b)
  E <- g$E
  b <- g$r
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
  x <- rep(0, ncol(A))
  for (i in 1:nrow(A)) {
    x[i] <- (t(A[i,]) %*% b) %% 2
  }
  return(x)
}

# Compute the optimal solution.  Return toggle vector and # button presses.
optimalSolution <- function(b) {
  n <- sqrt(length(b))
  x <- solveGA2(buildA(n), b)
  N <- nsBasisGA2(n)
  if (is.null(N)) {
    return(list(x = x, n = sum(x)))
  } else {
    m <- nrow(N)
    xs <- matrix(x, nrow = 2^m, ncol = n^2, byrow = TRUE)
    k <- 2
    for (i in 1:m) {
      cmb <- combn(1:m, i)
      for (j in 1:ncol(cmb)) {
        if (nrow(cmb) > 1) {
          xs[k,] <- x + apply(N[cmb[,j],], 2, sum)
        } else {
          xs[k,] <- x + N[cmb[1,j],]
        }
        k <- k + 1
      }
    }
  }
  xs <- xs %% 2
  ls <- apply(xs, 1, sum)
  i <- which.min(ls)
  return(list(x = xs[i,], n = min(ls)))
}

# Notes specific to Lights Out.
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
# Yes, easily.  Simply create an x vector, multiply by A and you've got a puzzle with
# a solution.  If a null space component sneaks in, who cares?  It won't change the
# puzzle.  Difficulty ought to scale decently well with the number of non-zero entries
# in x.
#
# If you start with a winnable grid, it will remain winnable.
#
# If you want a hint, solve the grid and provide any of the non-zero elements of
# the solution as the hint.  Order doesn't matter.
