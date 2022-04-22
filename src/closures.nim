## This module implements a bunch of sugar for closure iterators.

import std/[macros, sugar, genasts, sequtils]
type
  Iterator[T] = (iterator: T) or (iterator: lent T)
  Lendable = ref or seq or string or ptr or pointer

proc generateClosure(iter: NimNode): NimNode =
  let
    iter = copyNimTree(iter)
    impl = getImpl(iter[0])

  for i in countdown(impl[4].len - 1, 0): 
    let x = impl[4][i]
    if x.eqIdent("closure"):
      error("cannot convert closure to closure", iter[0])

  let
    procName = genSym(nskProc, "closureImpl")
    call = newCall(procName)

  for i in 1 .. iter.len - 1: # Unpacks the values if they're converted
    if iter[i].kind == nnkHiddenStdConv:
      iter[i] = iter[i][^1]
    call.add iter[i]

  var paramList = collect(newSeq):
    for i, x in impl[3]:
      let def = x.copyNimTree()
      if i > 0:
        def[^2] = nnkCommand.newTree(ident"sink", getTypeInst(iter[i]))
      def

  var vars = 1 # For each variable
  for i, defs in paramList:
    if i > 0:
      for j, def in defs[0..^3]:
        defs[j] = genSym(nskParam, $def) # Replace params with new symbol
        iter[vars] = defs[j] # Changes call parameter aswell
        inc vars

  let
    res = ident"result"
    body = genast(iter, res):
      when typeof(iter) is Lendable:
        res = iterator(): lent typeof(iter) =
          for x in iter:
            yield x
      else:
        res = iterator(): typeof(iter) =
          for x in iter:
            yield x

  paramList[0] = ident"auto" # Set return type to auto

  result = newProc(procName, paramList, body) # make proc
  result[4] = nnkPragma.newTree(ident"inline")
  result = nnkBlockStmt.newTree(newEmptyNode(), newStmtList(result, call)) # make block statment

macro asClosure*(iter: iterable): untyped =
  ## Takes a call to an iterator and captures it in a closure iterator for easy usage.
  iter.generateClosure()

proc reset*[T](clos: var Iterator[T]) =
  ## Resets the closure so iterations can continue
  runnableExamples:
    var a = @[10, 20].items.asClosure
    for _ in a():
      discard
    assert a.finished
    a.reset
    assert not a.finished

  cast[ptr UncheckedArray[int]](clos.rawEnv)[1] = 0

iterator iterThenReset*[T](clos: var iterator: T): T =
  ## Iterates over `closure` resetting after done
  runnableExamples:
    var a = @[10, 20].items.asClosure
    for _ in a.iterThenReset:
      discard
    assert not a.finished
  for x in clos():
    yield x
  reset(clos)

iterator iterThenReset*[T](clos: var iterator: lent T): T =
  ## same as the other but for `lent T` result
  reset(clos)

proc peek*[T](clos: var Iterator[T]): T =
  ## Gets the next value from a closure iterator.
  runnableExamples:
    var a = @[10, 20].items.asClosure
    assert a.peek == 10
    assert a() == 10
    assert a.peek == 20
    assert a() == 20
  var data: array[8, int] # Appears for our closures it's always 8 ints?
  let envPointer = cast[ptr UncheckedArray[int]](clos.rawEnv)
  copyMem(data.addr, envPointer[1].addr, sizeof(data))
  result = clos()
  copyMem(envPointer[1].addr, data.addr, sizeof(data))

proc map*[T, Y](iter: Iterator[T], mapProc: proc(a: T): Y): iterator: Y =
  result = iterator: Y =
    for x in iter():
      yield mapProc(x)

proc collect*[T](iter: Iterator[T]): seq[T] =
  for x in iter:
    result.add x

proc group*[T](iter: Iterator[T], count: static int): iterator: (int, array[count, T]) =
  result = iterator: (int, array[count, T]) =
    var result: (int, array[count, T])
    for x in iter():
      result[1][result[0]] = x
      inc result[0]
      if result[0] == count:
        yield result
        reset result
    if result[0] > 0:
      yield result

proc filter*[T](iter: Iterator[T], cond: proc(a: T): bool): iterator: T =
  result = iterator: T =
    for x in iter:
      if cond(x):
        yield x

