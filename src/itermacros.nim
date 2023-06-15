## itermacros
## ==========
##
## The **itermacros** module provides a set of templates for working with
## iterators and performing common operations such as filtering, mapping,
## accumulation, and element retrieval. These templates offer a convenient way
## to manipulate and process collections of data in a concise and expressive
## manner. They provide a declarative, more readable and arguably less
## error-prone alternative to writing manual imperative loops.
##
runnableExamples:
  import std/[strutils, options, tables, sets]

  # Find the first element in a sequence of the transformed initial numbers
  # that is bigger than 35.
  # Note using `HSlice.items` instead of `CountUp`.
  assert (-25..25).items.mapIt(it * 10 div 7).findIt(it > 35) == none(int)

  # Filter a table of philosophers by country of origin, compose a sentence
  # and join each to a string.

  let philosophers: Table[string, string] = {
    "Plato": "Greece", "Aristotle": "Greece", "Socrates": "Greece",
    "Confucius": "China", "Descartes": "France"}.toTable()

  const Phrase = "$1 is a famous philosopher from $2."
  let facts = philosophers.pairs()
                .filterIt(it[1] != "Greece")
                .mapIt([it[0], it[1]])
                .mapIt(Phrase % it)
                .foldIt("", acc & it & '\n')
  assert facts == """
Confucius is a famous philosopher from China.
Descartes is a famous philosopher from France.
"""

  # Find expensive stocks, convert the company name to uppercase and collect
  # to a custom container type.

  let stocks: Table[string, tuple[symbol:string, price:float]] = {
    "Pineapple": (symbol: "PAPL", price: 148.32),
    "Foogle": (symbol: "FOOGL", price: 2750.62),
    "Visla": (symbol: "VSLA", price: 609.89),
    "Mehzon": (symbol: "MHZN", price: 3271.92),
    "Picohard": (symbol: "PCHD", price: 265.51),
  }.toTable()

  let shoutExpensive = stocks.pairs()
                          .mapIt((name: it[0], price:it[1].price))
                          .filterIt(it.price > 1000.0)
                          .mapIt(it.name).map(toUpperAscii)
                          .collect(HashSet[string])

  assert shoutExpensive == ["FOOGLE", "MEHZON"].toHashSet()


import std/[macros, genasts, options]

macro genIter*[T](iter: iterable[T], body: varargs[untyped]): untyped =
  if body.len == 0:
    error("Expected 1 or 2 arguments passed.", body)
  let
    iter =
      if iter.kind != nnkCall:
        iter[^1]
      else:
        iter
    pre =
      if body.len > 1:
        body[0]
      else:
        newStmtList()
    post =
      if body.len == 1:
        body[0]
      else:
        body[1]
  if body.len > 2:
    error("Too many arguments provided.", body)

  let decl = genast():
    iterator name(): auto {.gensym.} =
      discard

  let call = newCall(iter[0])
  if iter.len > 1:
    for i, arg in iter[1..^1]:
      let name = ident "arg" & $i
      decl.params.add newIdentDefs(name, newCall("typeof", arg))
      decl.add arg
      call.add name

  decl[^2] = genast(call, pre, post):
    pre
    for it {.inject.} in call:
      post
  let iterInvoke = copyNimTree(iter)
  iterInvoke[0] = decl[0]
  result = newStmtList(decl, iterInvoke)

#--------------------------------------------------------------------------
# Adaptors
#--------------------------------------------------------------------------

when defined(nimdoc):
  template map*[T; Y](iter: iterable[T]; fn: proc(x: T): Y): untyped =
    ## Transforms elements of the iterator using a mapping function.
    ##
    ## `map` applies the mapping function to each element of the input
    ## iterator, yielding the returned values of that function.
    ##
    runnableExamples:
      let nums = [1, 2, 3, 4, 5]
      assert nums.items.map(proc(x: int): int = x * x).collect() == @[1, 4, 9, 16, 25]
else:
  template map*[T; Y](iter: iterable[T]; fn: proc(x: T): Y): untyped =
    genIter(iter):
      yield fn(it)

template map*[T; Y](iter: iterable[T]; fn: proc(x: T): Y {.inline.}): untyped =
  # ## `map` overload for inline procs.
  genIter(iter):
    yield fn(it)

when defined(nimdoc):
  template mapIt*[T](iter: iterable[T]; expr: untyped): untyped =
    ## Transforms elements of the iterator using an expression.
    ##
    ## `mapIt` applies the expression `expr` to each element of the input
    ## iterator, yielding the results of the evaluation.
    ##
    runnableExamples:
      let nums = [1, 2, 3, 4, 5]
      assert nums.items.mapIt(it * 2).collect() == @[2, 4, 6, 8, 10]
else:
  template mapIt*[T](iter: iterable[T]; expr: untyped): untyped =
    genIter(iter):
      yield expr

when defined(nimdoc):
  template filter*[T](iter: iterable[T]; pred: proc(x: T): bool): untyped =
    ## Filters elements of the iterator using a predicate function.
    ##
    ## `filter` yields only the elements of the input iterator for which the
    ## predicate function `pred` returns `true`.
    ##
    runnableExamples:
      let nums = [1, 2, 3, 4, 5]
      assert nums.items.filter(proc(x: int): bool = x mod 2 == 0).collect() == @[2, 4]
else:
  template filter*[T](iter: iterable[T]; pred: proc(x: T): bool): untyped =
    genIter(iter):
      if pred(it):
        yield it

when defined(nimdoc):
  template filterIt*[T](iter: iterable[T]; expr: untyped): untyped =
    ## Filters elements of the iterator based on a specified condition.
    ##
    ## `filterIt` yields only the elements of the input iterator for which the
    ## specified expression `expr` evaluates to `true`.
    ##
    runnableExamples:
      let nums = [1, 2, 3, 4, 5]
      assert nums.items.filterIt(it mod 2 == 0).collect() == @[2, 4]
else:
  template filterIt*[T](iter: iterable[T]; expr: untyped): untyped =
    genIter(iter):
      if expr:
        yield it

macro genTuple(typ: untyped; amount: static int): untyped =
  result = nnkPar.newTree()
  for _ in 0..<amount:
    result.add typ

proc set(t: var tuple; ind: int; val: auto) =
  var i = 0
  for field in t.fields:
    if i == ind:
      field = val
    inc i

when defined(nimdoc):
  template group*[T](iter: iterable[T]; amount: static Positive): untyped =
    ## Groups elements of the iterator into tuples of a specified size.
    ##
    ## `group` yields tuples with `amount` elements until all elements from the
    ## input iterator are exhausted.
    ##
    ## .. Note:: If the iterator can't fully fill the tuple, tailing elements are
    ## discarded
    ##
    runnableExamples:
      let nums = [1, 2, 3, 4, 5, 6]
      assert nums.items.group(3).collect() == @[(1, 2, 3), (4, 5, 6)]
else:
  template group*[T](iter: iterable[T]; amount: static Positive): untyped =
    genIter(iter):
      var
        val: genTuple(T, amount)
        ind = 0
    do:
      val.set(ind mod amount, it)
      if ind mod amount == amount - 1:
        yield val
      inc ind

when defined(nimdoc):
  template skip*[T](iter: iterable[T]; amount: Natural): untyped =
    ## Modifies the iterator to skip a specified number of elements.
    ##
    ## `skip` produces an iterator that skips the specified number of elements
    ## from the input iterator. Once the specified number of elements have been
    ## skipped, the iteration continues and all subsequent elements are yielded.
    ##
    ## .. Note:: If the input iterator has fewer than `amount` elements, the
    ## resulting iterator will not produce any elements.
    ##
    runnableExamples:
      let nums = [1, 2, 3, 4, 5]
      assert nums.items.skip(2).collect() == @[3, 4, 5]
else:
  template skip*[T](iter: iterable[T]; amount: Natural): untyped =
    genIter(iter):
      var counter = 0
    do:
      if counter >= amount:
        yield it
      inc counter

when defined(nimdoc):
  template skipWhileIt*[T](iter: iterable[T]; expr: untyped): untyped =
    ## Skips elements of the iterator while the specified expression evaluates to
    ## `true`.
    ##
    ## `skipWhileIt` returns an iterator that starts yielding elements from the
    ## input iterator from the first element where the given expression
    ## evaluates to `false`.
    ##
    ## Once `expr` returns `false`, all subsequent elements are yielded.
    ##
    runnableExamples:
      let nums = [1, 2, 3, 4, 5]
      assert nums.items.skipWhileIt(it < 3).collect() == @[3, 4, 5]
else:
  template skipWhileIt*[T](iter: iterable[T]; expr: untyped): untyped =
    genIter(iter):
      var skipping = true
    do:
      if skipping:
        skipping = expr
        if skipping:
          continue
      yield it

when defined(nimdoc):
  template skipWhile*[T](iter: iterable[T]; pred: proc(x: T): bool): untyped =
    ## Skips elements of the iterator while the specified predicate function
    ## returns `true`.
    ##
    ## `skipWhile` returns an iterator that starts yielding elements from the
    ## input iterator from the first element where `pred` returns `false`.
    ##
    ## Once `pred` returns `false`, all subsequent elements are yielded.
    ##
    runnableExamples:
      let nums = [1, 2, 3, 4, 5]
      assert nums.items.skipWhile(proc(x: int): bool = x < 3).collect() == @[3, 4, 5]
else:
  template skipWhile*[T](iter: iterable[T]; pred: proc(x: T): bool): untyped =
    skipWhileIt(iter, pred(it))

when defined(nimdoc):
  template take*[T](iter: iterable[T]; amount: Natural): untyped =
    ## Modifies the iterator to yield a specified number of elements.
    ##
    ## `take` produces an iterator that yields elements up to the specified
    ## `amount`. Once the specified number of elements is reached (or the iterator
    ## is exhausted), the iteration stops and no further elements are yielded.
    ##
    ## .. Note:: If the input iterator contains fewer elements than the specified
    ## `amount`, only that number of elements will be yielded.
    ##
    runnableExamples:
      let nums = [1, 2, 3, 4, 5]
      assert nums.items.take(3).collect() == @[1, 2, 3]
else:
  template take*[T](iter: iterable[T]; amount: Natural): untyped =
    genIter(iter):
      var counter = 0
    do:
      if counter >= amount:
        break
      yield it
      inc counter

when defined(nimdoc):
  template takeWhileIt*[T](iter: iterable[T]; expr: untyped): untyped =
    ## Modifies the iterator to yield elements as long as the specified expression
    ## evaluates to `true`. Once the expression evaluates to `false`, the
    ## iteration stops and no further elements are yielded.
    ##
    runnableExamples:
      let nums = [1, 2, 3, 4, 5]
      assert nums.items.takeWhileIt(it < 4).collect() == @[1, 2, 3]
else:
  template takeWhileIt*[T](iter: iterable[T]; expr: untyped): untyped =
    genIter(iter):
      if not expr:
        break
      yield it

when defined(nimdoc):
  template takeWhile*[T](iter: iterable[T]; pred: proc(x: T): bool): untyped =
    ## Modifies the iterator to yield elements as long as the specified predicate
    ## function returns `true`. Once `pred` returns `false`, the iteration stops
    ## and no further elements are yielded.
    ##
    runnableExamples:
      let nums = [1, 2, 3, 4, 5]
      assert nums.items.takeWhile(proc(x: int): bool = x < 4).collect() == @[1, 2, 3]
else:
  template takeWhile*[T](iter: iterable[T]; pred: proc(x: T): bool): untyped =
    takeWhileIt(iter, pred(it))

when defined(nimdoc):
  template stepBy*[T](iter: iterable[T]; step: Positive): untyped =
    ## Modifies the iterator to yield elements stepping by a specified amount.
    ##
    ## `stepBy` takes an iterable and a step size and returns an iterator that
    ## yields elements at the specified step intervals. The resulting iterator
    ## skips `step-1` elements after yielding an element.
    ##
    ## The first element is always yielded, regardless of the step given.
    ##
    runnableExamples:
      let nums = [1, 2, 3, 4, 5, 6, 7, 8, 9]
      assert nums.items.stepBy(2).collect() == @[1, 3, 5, 7, 9]
else:
  template stepBy*[T](iter: iterable[T]; step: Positive): untyped =
    genIter(iter):
      var count = step
    do:
      if count == step:
        yield it
        count = 0
      inc count

when defined nimdoc:
  template enumerate*[T](iter: iterable[T]): untyped =
    ## Modifies the iterator to provide the current iteration count and value.
    ##
    ## `enumerate` takes an iterable and returns an iterator that yields pairs of
    ## (count, element) of type `(int, T)`.
    ##
    ## .. Note:: The count starts from 0 for the first element.
    ##
    runnableExamples:
      let letters = ["Alpha", "Beta", "Gamma"]
      assert letters.items.enumerate().collect() == @[(0, "Alpha"), (1, "Beta"), (2, "Gamma")]
else:
  template enumerate*[T](iter: iterable[T]): untyped =
    genIter(iter):
      var count = 0
    do:
      yield (count, it)
      inc count

#--------------------------------------------------------------------------
# Consumers
#--------------------------------------------------------------------------

type
  Comparable* = concept a
    (a < a) is bool
  Summable* = concept a, type T # Additive?
    (a + a) is T
  Multipliable* = concept a, type T # Multiplicative?
    (a * a) is T

template collect*[T](iter: iterable[T]; size: Natural = 1): seq[T] =
  ## Collects the elements of the iterator into a new sequence.
  ##
  ## `collect` takes an iterator of elements and an optional size parameter. It
  ## creates a new sequence and adds each element from the iterator into the
  ## sequence. The resulting sequence is returned.
  ##
  ## For collecting into the user-specified type of container, see
  ## `collect<#collect.t,iterable[T],typedesc[U]>`_.
  ##
  ## .. Note:: The size parameter is optional and can be used to provide an initial
  ##   capacity for the sequence. If not specified, the sequence will be created
  ##   with a capacity for one element.
  ##
  runnableExamples:
    let nums = [1, 2, 3, 4, 5]
    let plusOne = nums.items.mapIt(it + 1).collect()
    assert plusOne == @[2, 3, 4, 5, 6]
  var val = newSeqOfCap[T](size)
  for x in iter:
    val.add x
  val

template collect*[T, U](iter: iterable[T]; t: typeDesc[U]): U =
  ## Collects the elements of the iterator into a new container.
  ##
  ## `collect` takes an iterator and a type `U` for the container to put the
  ## elements of the iterator in. It creates a new collection and applies the
  ## first available proc of `add`, `incl` or `push`, with the current iteration
  ## element as an argument. The resulting collection is returned.
  ##
  ## .. Note:: The type `U` should be compatible with the elements in the iterator.
  ##   If `U` is a reference type, a new instance is created. Otherwise, the
  ##   default value of `U` is used.
  ##
  runnableExamples:
    let nums = [1, 2, 3, 4, 5]
    let minusOne = nums.items.mapIt(it - 1).collect(seq[int])
    assert minusOne == @[0, 1, 2, 3, 4]
  # TODO? Replace `when compiles` with specialisation on concepts
  var acc = when t is ref:
      new t
    else:
      default t
  for x in iter:
    when compiles(acc.add(default(typeOf(T)))):
      acc.add x
    elif compiles(acc.incl(default(typeOf(T)))):
      acc.incl x
    elif compiles(acc.push(default(typeOf(T)))):
      acc.push x
  acc

template fold*[T, U](iter: iterable[T]; init: U; fn: proc(acc: sink U; it: T): U): U =
  ## Accumulates the values of the iterator using an accumulation function `fn`.
  ## This operation is also commonly known as "reduce" and is useful for
  ## producing a single value from a collection.
  ##
  ## `fold` takes an iterable of elements, an initial value `init`, and an
  ## accumulation function `fn`. It updates the accumulating variable `acc` with
  ## the results of applying the accumulating function to the accumulator and
  ## the current element of the iterator. The final value of `acc` is returned.
  ##
  ## .. Note:: `fold` loops through every element of the iterator and thus will not
  ##   terminate for infinite iterators.
  ##
  runnableExamples:
    let nums = [1, 2, 3, 4, 5]
    var sum = nums.items.fold(0, proc(acc: sink int; it: int): int = acc + it)
    assert sum == 15
  var acc: U = init
  for it in iter:
    acc = fn(acc, it)
  acc

template fold*[T, U](iter: iterable[T]; init: U; fn: proc(acc: var U; it: T)): U =
  ## Accumulates the values of the iterator using an accumulation function `fn`.
  ##
  ## See `fold<#fold.t,iterable[T],U,proc(sinkU,T)>`_ for general description.
  ## This version of `fold` requires an accumulation function that
  ## takes the current accumulated value as a var parameter for in-place
  ## modification.
  ##
  ## .. Note:: `fold` loops through every element of the iterator and thus will not
  ##   terminate for infinite iterators.
  ##
  runnableExamples:
    let nums = [1, 2, 3, 4, 5]
    var product = nums.items.fold(1, proc(acc: var int; it: int) = acc *= it)
    assert product == 120
  var acc: U = init
  for it in iter:
    fn(acc, it)
  acc

template foldIt*[T, U](iter: iterable[T]; init: U; expr: untyped): U =
  ## Accumulates the values of the iterator using an expression.
  ##
  ## `foldIt` takes an iterable of elements, an initial value `init`, and an
  ## expression that defines the accumulation logic. The expression should
  ## evaluate to the updated value of the accumulating variable `acc`.
  ## The final value of `acc` is returned.
  ##
  ## .. Note:: `expr` should assign the updated accumulated value to the `acc` variable.
  ##
  ## .. Note:: `foldIt` loops through every element of the iterator and thus will not
  ##   terminate for infinite iterators.
  ##
  runnableExamples:
    let nums = [1, 2, 3, 4, 5]
    var sum = nums.items.foldIt(1, acc + it)
    assert sum == 16
  var acc {.inject.}: U = init
  for it {.inject.} in iter:
    acc = expr
  acc

template selectByCmp[T: Comparable](iter: iterable[T]; expr: untyped): T =
  var
    requiresInit = true
    acc {.inject.}: T
  for it {.inject.} in iter:
    if unlikely(requiresInit):
      acc = it
      requiresInit = false
    if expr:
      acc = it
  acc

template min*[T: Comparable](iter: iterable[T]): T =
  ## Finds the minimum element in the iterator.
  ##
  ## `min` takes an iterable of elements that support comparison (satisfy the
  ## `Comparable` concept) and returns the minimal element.
  ##
  runnableExamples:
    let nums = [5, 2, 8, 1, 9]
    assert nums.items.min() == 1
  selectByCmp(iter, it < acc)

template max*[T: Comparable](iter: iterable[T]): T =
  ## Finds the maximum element in the iterator.
  ##
  ## `max` takes an iterable of elements that support comparison (satisfy the
  ## `Comparable` concept) and returns the maximal element.
  ##
  runnableExamples:
    let nums = [5, 2, 8, 1, 9]
    assert nums.items.max() == 9
  selectByCmp(iter, acc < it)

template count*[T](iter: iterable[T]): Natural =
  ## Counts the number of elements in the iterator.
  ##
  ## `count` takes an iterable and returns the total number of elements.
  ##
  ## .. Note:: The count iterates through and discards all elements of the iterator.
  ##
  runnableExamples:
    let nums = [1, 2, 3, 4, 5]
    assert nums.items.count() == 5
  iter.foldIt(0, acc+1)

template sum*[T: Summable](iter: iterable[T]): T =
  ## Calculates the sum of all elements in the iterator.
  ##
  ## `sum` takes an iterable of elements that support addition
  ## (satisfy the `Summable` concept). It adds up all the elements
  ## together and returns the resulting sum.
  ##
  ## .. Note:: An empty iterator returns the default value for the type `T`.
  ##
  runnableExamples:
    let nums = [1, 2, 3, 4, 5]
    assert nums.items.sum() == 15
  iter.foldIt(default(typeOf(T)), acc + it)

template product*[T: Multipliable](iter: iterable[T]): T =
  ## Calculates the product of all elements in the iterator.
  ##
  ## `product` takes an iterable of elements that support multiplication
  ## (satisfy the `Multipliable` concept). It multiplies all the elements
  ## together and returns the resulting product.
  ##
  ## .. Note:: An empty iterator returns the default value for the type `T`.
  ##
  runnableExamples:
    let nums = [1, 2, 3, 4, 5]
    assert nums.items.product() == 120
  var
    requiresInit = true
    acc: T
  for it in iter:
    if unlikely(requiresInit):
      acc = it
      requiresInit = false
      continue
    acc = acc * it
  acc

template anyIt*[T](iter: iterable[T]; expr: untyped): bool =
  ## Checks if for at least one element of the iterator the expression evaluates
  ## to `true`.
  ##
  ## `anyIt` is short-circuiting; in other words, it will stop processing
  ## as soon as it encounters `true`.
  ##
  ## .. Note:: An empty iterator returns `false`.
  ##
  runnableExamples:
    let nums = [1, 2, 3, 4, 5]
    assert nums.items.anyIt(it > 3)
    assert "".items.anyIt(it is char) == false
  var result = false
  for it {.inject.} in iter:
    if expr:
      result = true
      break
  result

template any*[T](iter: iterable[T]; pred: proc(x: T): bool): bool =
  ## Checks if for at least one element of the iterator the specified predicate
  ## function returns `true`.
  ##
  ## `any` is short-circuiting; in other words, it will stop processing
  ## as soon as `pred` returns `true`.
  ##
  ## .. Note:: An empty iterator returns `false`.
  ##
  runnableExamples:
    let nums = @[1, 2, 3, 4, 5]
    assert nums.items.any(proc(x: int): bool = x > 3) == true
  anyIt(iter, pred(it))

template allIt*[T](iter: iterable[T]; expr: untyped): bool =
  ## Checks if for every iteration the expression evaluates to `true`.
  ##
  ## `allIt` is short-circuiting; in other words, it will stop processing
  ## as soon as it encounters `false`.
  ##
  ## .. Note:: An empty iterator returns `true`.
  ##
  runnableExamples:
    let nums = [1, 2, 3, 4, 5]
    assert nums.items.allIt(it < 10) == true
    assert "".items.allIt(it == '!') == true
  var result = true
  for it {.inject.} in iter:
    if not (expr):
      result = false
      break
  result

template all*[T](iter: iterable[T]; pred: proc(x: T): bool): bool =
  ## Checks if for every iteration the specified predicate function returns
  ## true.
  ##
  ## `all` is short-circuiting; in other words, it will stop processing
  ## as soon as `pred` returns `false`.
  ##
  ## .. Note:: An empty iterator returns `true`.
  ##
  runnableExamples:
    let nums = [1, 2, 3, 4, 5]
    assert nums.items.all(proc(x: int): bool = x < 10) == true
    assert "".items.all(proc(x: char): bool = x == '!') == true
  allIt(iter, pred(it))

template findIt*[T](iter: iterable[T]; expr: untyped): Option[T] =
  ## Searches for the first element in the iterator for which the specified
  ## expression evaluates to `true`. `none` is returned if such an element is
  ## not found (every expression evaluates to `false`). If such an element is
  ## found, it is wrapped in an `Option` and returned as `some(element)`.
  ## Otherwise, `none` is returned.
  ##
  ## If you need the index of the element, see
  ## `positionIt<#positionIt.t,iterable[T],untyped>`_.
  ##
  ## .. Note:: `findIt` is short-circuiting; in other words, it will stop
  ##   iterating as soon as expression `expr` returns true.
  ##
  runnableExamples:
    import std/options
    let nums = [1, 2, 3, 4, 5]
    assert nums.items.findIt(it == 3) == some(3)
  var result = none(typeOf(T))
  for it {.inject.} in iter:
    if expr:
      result = some(it)
      break
  result


template positionIt*[T](iter: iterable[T]; expr: untyped): Option[int] =
  ## Searches for the index of the first element in the iterator for which the
  ## specified expression evaluates to `true`. `none` is returned if such an
  ## element is not found (every expression evaluates to `false`). If such an
  ## element is found, its iteration index is wrapped in an `Option` and
  ## returned as `some(index)`. Otherwise, `none` is returned.
  ##
  ## If you need to find the element itself, see
  ## `findIt<#findIt.t,iterable[T],untyped>`_.
  ##
  ## .. Note:: `positionIt` is short-circuiting; in other words, it will stop
  ##   iterating as soon as expression `expr` returns true.
  ##
  runnableExamples:
    import std/options
    let nums = [1, 2, 3, 4, 5]
    assert nums.items.positionIt(it == 3) == some(2)
  var
    position = none(int)
    counter = 0
  for it {.inject.} in iter:
    if expr:
      position = some(counter)
      break
    inc(counter)
  position

template position*[T](iter: iterable[T]; pred: proc(x: T): bool): Option[int] =
  ## Searches for the index of the first element in the iterator for which the
  ## specified predicate function returns `true`. `none` is returned if such an
  ## element is not found (every predicate returns `false`). If such an element
  ## is found, its iteration index is wrapped in an `Option` and returned as
  ## `some(index)`. Otherwise, `none` is returned.
  ##
  ## If you need to find the element itself, see
  ## `find<#find.t,iterable[T],proc(T)>`_.
  ##
  ## .. Note:: `position` is short-circuiting; in other words, it will stop
  ##   iterating as soon as the predicate returns `true`.
  ##
  runnableExamples:
    import std/options
    let nums = [1, 2, 3, 4, 5]
    assert nums.items.position(proc(x: int): bool = x == 3) == some(2)
  positionIt(iter, pred(it))

template find*[T](iter: iterable[T]; pred: proc(x: T): bool): Option[T] =
  ## Searches for the first element in the iterator that satisfies the specified
  ## predicate function. If such an element is found, it is wrapped in an
  ## `Option` and returned as `some(element)`. If no element satisfies the
  ## predicate, `none` is returned.
  ##
  ## If you need the index of the element, see
  ## `position<#position.t,iterable[T],proc(T)>`_.
  ##
  ## .. Note:: `find` is short-circuiting; in other words, it will stop
  ##   iterating as soon as `pred` returns true.
  ##
  runnableExamples:
    import std/options
    let nums = @[1, 2, 3, 4, 5]
    assert nums.items.find(proc(x: int): bool = x > 3) == some(4)
  findIt(iter, pred(it))


template nth*[T](iter: iterable[T]; n: Natural): Option[T] =
  ## Returns the nth element of the iterator.
  ## If `n` is greater than or equal to the number of iterator elements, returns
  ## `none`.
  ##
  ## Like most indexing operations, the count starts from zero, so
  ## `nth(0)` returns the first value, `nth(1)` the second, and so on.
  ##
  ## .. Note:: This adaptor consumes the iterator and discards all of
  ##   the preceding elements.
  ##
  runnableExamples:
    import std/options

    let nums = [1, 2, 3, 4, 5]
    let thirdElement = nums.items.nth(2)
    assert thirdElement == some(3)
    let sixthElement = nums.items.nth(5)
    assert sixthElement.isNone()
  var
    result = none(typeOf(T))
    counter = 0
  for it in iter:
    if counter == n:
      result = some(it)
      break
    inc counter
  result
