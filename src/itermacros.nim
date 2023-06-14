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

template map*[T; Y](iter: iterable[T]; fn: proc(x: T): Y): untyped =
  genIter(iter):
    yield fn(it)

template map*[T; Y](iter: iterable[T]; fn: proc(x: T): Y {.inline.}): untyped =
  # Overloard for inline procs
  genIter(iter):
    yield fn(it)

template mapIt*[T](iter: iterable[T]; expr: untyped): untyped =
  genIter(iter):
    yield expr

template filter*[T](iter: iterable[T]; pred: proc(x: T): bool): untyped =
  genIter(iter):
    if pred(it):
      yield it

template filterIt*[T](iter: iterable[T]; expr: untyped): untyped =
  genIter(iter):
    if expr:
      yield it

macro genTuple*(typ: untyped; amount: static int): untyped =
  result = nnkPar.newTree()
  for _ in 0..<amount:
    result.add typ

proc set(t: var tuple; ind: int; val: auto) =
  var i = 0
  for field in t.fields:
    if i == ind:
      field = val
    inc i

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

template skip*[T](iter: iterable[T]; amount: Natural): untyped =
  genIter(iter):
    var counter = 0
  do:
    if counter >= amount:
      yield it
    inc counter

template skipWhileIt*[T](iter: iterable[T]; expr: untyped): untyped =
  genIter(iter):
    var skipping = true
  do:
    if skipping:
      skipping = expr
    else:
      yield it

template skipWhile*[T](iter: iterable[T]; pred: proc(x: T): bool): untyped=
  skipWhileIt(iter, pred(it))

template take*[T](iter: iterable[T]; amount: Natural): untyped =
  genIter(iter):
    var counter = 0
  do:
    if counter >= amount:
      break
    yield it
    inc counter

template takeWhileIt*[T](iter: iterable[T]; expr: untyped): untyped =
  genIter(iter):
    if not expr:
      break
    yield it

template takeWhile*[T](iter: iterable[T]; pred: proc(x: T): bool): untyped=
  takeWhileIt(iter, pred(it))

template stepBy*[T](iter: iterable[T]; step: Positive): untyped =
  genIter(iter):
    var count = step
  do:
    if count == step:
      yield it
      count = 0
    inc count

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

template collect*[T](iter: iterable[T]; size: Natural = 0): seq[T] =
  var val = newSeqOfCap[T](size)
  for x in iter:
    val.add x
  val

template collect*[T, U](iter: iterable[T]; t: typeDesc[U]): U =
  # TODO? Replace `when compiles` with specialization on concepts
  var acc = default(t)
  for x in iter:
    when compiles(acc.add(default(typeOf(T)))):
      acc.add x
    elif compiles(acc.incl(default(typeOf(T)))):
      acc.incl x
    elif compiles(acc.push(default(typeOf(T)))):
      acc.push x
  acc

template fold*[T, U](iter: iterable[T]; init: U; fn: proc(acc: sink U; it: T): U): U =
  var acc: U = init
  for it in iter:
    acc = fn(acc, it)
  acc

template fold*[T, U](iter: iterable[T]; init: U; fn: proc(acc: var U; it: T)): U =
  var acc: U = init
  for it in iter:
    fn(acc, it)
  acc

template foldIt*[T, U](iter: iterable[T]; init: U; expr: untyped): U =
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
  selectByCmp(iter, it < acc)

template max*[T: Comparable](iter: iterable[T]): T =
  selectByCmp(iter, acc < it)

template count*[T](iter: iterable[T]): Natural =
  iter.foldIt(0, acc+1)

template sum*[T: Summable](iter: iterable[T]): T =
  iter.foldIt(default(typeOf(T)), acc + it)

template product*[T: Multipliable](iter: iterable[T]): T =
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
  var result = false
  for it {.inject.} in iter:
    if expr:
      result = true
      break
  result

template any*[T](iter: iterable[T]; pred: proc(x: T): bool): bool =
  anyIt(iter, pred(it))

template allIt*[T](iter: iterable[T]; expr: untyped): bool =
  var result = true
  for it {.inject.} in iter:
    if not expr:
      result = false
      break
  result

template all*[T](iter: iterable[T]; pred: proc(x: T): bool): bool =
  allIt(iter, pred(it))

template findIt*[T](iter: iterable[T]; expr: untyped): Option[T] =
  var result = none(typeOf(T))
  for it {.inject.} in iter:
    if expr:
      result = some(it)
      break
  result

template find*[T](iter: iterable[T]; pred: proc(x: T): bool): Option[T] =
  findIt(iter, pred(it))
