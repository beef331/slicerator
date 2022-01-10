## This module implements a bunch of sugar for closure iterators.
## Making closures from iterators  and closure features.

import std/[macros, sugar, genasts]

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
    call.add iter[i].copyNimTree()

  var paramList = collect(newSeq):
    for i, x in impl[3]:
      let def = x.copyNimTree()
      if i > 0:
        def[^2] = getTypeInst(iter[i])
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
      res = iterator(): auto {.closure.} =
        for x in iter:
          yield x

  paramList[0] = ident"auto" # Set return type to auto

  result = newProc(procName, paramList, body) # make proc
  result = nnkBlockStmt.newTree(newEmptyNode(), newStmtList(result, call)) # make block statment

macro asClosure*(iter: iterable): untyped =
  ## Takes a call to an iterator and captures it in a closure iterator for easy usage.
  iter.generateClosure()

proc reset*(clos: var iterator) =
  {.emit: """
      ((NI*) `clos`->ClE_0)[1] = 0;
  """.}

iterator iterThenReset*[T](clos: var iterator: T): T =
  ## Iterates over `ResettableClosure` resetting after done
  for x in clos():
    yield x
  reset(clos)

proc peek*[T](clos: var iterator(): T): T =
  ## Gets the next value from a closure iterator.
  ## Copies the environment not ideal for memory usage.
  # Todo: implement this by copying pertinent ENV data instead of the entire closuree
  let base = deepCopy(clos)
  result = clos()
  clos = base