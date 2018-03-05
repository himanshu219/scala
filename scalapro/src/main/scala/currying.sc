object currying {

    def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
      if (a > b) zero
      else
        combine(f(a), mapReduce(f,combine,zero)(a + 1, b))
    }
    mapReduce(x => x, (a, b) => a*b, 1)(1, 10)
    mapReduce(x => x, (a, b) => a+b, 0)(1, 10)

}

