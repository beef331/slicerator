import std/[macros, genasts]

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

template map*[T; Y](iter: iterable[T], fn: proc(x: T): Y): untyped =
  genIter(iter):
    yield fn(it)

template mapIt*[T](iter: iterable[T], expr: untyped): untyped =
  genIter(iter):
    yield expr

template filter*[T](iter: iterable[T], fn: proc(x: T): bool): untyped =
  genIter(iter):
    if fn(it):
      yield it

template filterIt*[T](iter: iterable[T], expr: untyped): untyped =
  genIter(iter):
    if expr:
      yield it

macro genTuple*(typ: untyped, amount: static int): untyped =
  result = nnkPar.newTree()
  for _ in 0..<amount:
    result.add typ

proc set(t: var tuple, ind: int, val: auto) =
  var i = 0
  for field in t.fields:
    if i == ind:
      field = val
    inc i

template group*[T](iter: iterable[T], amount: static int): untyped =
  genIter(iter):
    var
      val: genTuple(T, amount)
      ind = 0
  do:
    val.set(ind mod amount, it)
    if ind mod amount == amount - 1:
      yield val
    inc ind

template skip*[T](iter: iterable[T], amount: int): untyped =
  genIter(iter):
    var counter = amount
  do:
    if counter > 0:
      dec counter
      continue
    yield it

template take*[T](iter: iterable[T], amount: int): untyped =
  genIter(iter):
    var counter = amount - 1
  do:
    if counter > 0:
      yield it
      dec counter
    else:
      break

template collect*[T](iter: iterable[T], size = 0): seq[T] =
  var val = newSeqOfCap[T](size)
  for x in iter:
    val.add x

template enumerate*[T](iter: iterable[T]): untyped =
  genIter(iter):
    var count = 0
  do:
    yield (count, it)
    inc count
