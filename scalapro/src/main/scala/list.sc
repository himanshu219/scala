val a = List(2,3,4,1,4)
a.map(x => x * x)

// scalar product of vectors

// . character
// cross product

// reverse

// remove nth

// 1 < j < i < n and i + j is prime

// n queens


def isprime(n: Int) = (2 until n).forall(d => n%d != 0)

for {
  i <- 1 to 10
  j <- 1 to i
  if isprime(i+j)
} yield (i, j)