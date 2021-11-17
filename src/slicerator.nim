import std/[macros, sugar, genasts, enumerate]

type
  ResetableClosure = concept r
    r.data is tuple
    r.theProc is proc
    r.theIter is iterator

iterator `[]`*[T](a: openArray[T], slice: Slice[int]): T =
  ## Immutable slice iteration over an `openarray`
  for x in a.toOpenArray(slice.a, slice.b):
    yield x

iterator `[]`*[T](a: openArray[T], slice: HSlice[int, BackwardsIndex]): T =
  ## Immutable slice iteration over an `openarray`, taking `BackwardsIndex`
  for x in a[slice.a .. a.len - slice.b.int]:
    yield x

iterator `{}`*[T](a: var openArray[T], slice: Slice[int]): var T =
  ## Mutable slice iteration over an `openarray`
  for i in slice.a..slice.b:
    yield a[i]

iterator `{}`*[T](a: var openArray[T], slice: HSlice[int, BackwardsIndex]): var T =
  ## Mutable slice iteration over an `openarray`, taking `BackwardsIndex`
  for ch in a{slice.a .. a.len - slice.b.int}:
    yield ch

iterator revItems*[T](a: openArray[T]): T =
  ## Reversed immutable items over an `openArray`
  for x in countdown(a.high, 0):
    yield a[x]

iterator revMitems*[T](a: var openArray[T]): var T =
  ## Reversed mutable items over an `openArray`
  for x in countdown(a.high, 0):
    yield a[x]

iterator findAll*[T](a: openArray[T], val: T): int =
  ## Iterates the `openArray` yielding indices that match `val`
  for i, x in a:
    if x == val:
      yield i

iterator mFindAll*[T](a: var openArray[T], val: T): var T =
  ## Iterates the `openarray` yielding mutable values that match `val`
  for i, x in a:
    if x == val:
      yield a[i]

iterator rFindAll*[T](a: openArray[T], val: T): int =
  ## Iterates the `openArray` backwards yield all indices that match `val`
  var i = a.high
  for x in a.revItems:
    if x == val:
      yield i
    dec i

iterator rMFindAll*[T](a: var openArray[T], val: T): var T =
  ## Iterates the `openArray` backwards yielding all mutable values that match `val`
  for x in a.revMitems:
    if x == val:
      yield x

template forMItems*[T](a: var openArray[T], indexName, valName, body: untyped): untyped =
  ## Sugar for iterating over mutable entries getting their indices and value
  var index = 0
  for valname in a.mitems:
    let indexName = index
    body
    inc index

proc generateClosure(iter: NimNode): NimNode =
  let
    iter = copyNimTree(iter)
    impl = getImpl(iter[0])

  for i in countdown(impl[4].len - 1, 0): 
    let x = impl[4][i]
    if x.eqIdent("closure"):
      error("cannot convert closure to closure", iter[0])

  let
    procName = ident"closureImpl"
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

template skipIter*(iter, val: untyped, toSkip: Natural, body: untyped) =
  ## Skip over a certain number of iterations
  for i, x in enumerate(iter):
    if i > toSkip:
      let val = x
      body

template iterRange*(iter, val: untyped, rng: Slice[int], body: untyped) =
  ## Only runs code for a given range of iterations
  for i, x in enumerate(iter):
    if i in rng:
      let val = x
      body

macro asResetableClosure*(iter: iterable): untyped =
  var tupleData = nnkTupleConstr.newTree()
  for x in iter[1..^1]:
    tupleData.add:
      case x.kind
      of nnkHiddenStdConv, nnkConv:
        x[^1]
      else:
        x
  let
    closure = generateClosure(iter)
    closureProc = closure[1][0]
    closureCall = closure[1][1]
    closureName = closure[1][0][0]
  result =
    genAst(closure, closureName, tupleData, closureProc, closureCall):
      block:
        closureProc
        let clos = closureCall
        type AnonResetClos = object
          data: typeof(tupleData)
          theProc: typeof(closureName)
          theIter: typeof(clos)
        AnonResetClos(data: tupleData, theProc: closureName, theIter: clos)

macro `<-`(prc: proc, data: tuple): untyped =
  result = newCall(prc)
  for i, _ in data.getTypeInst:
    result.add nnkBracketExpr.newTree(data, newLit(i))

proc reset*(rc: var ResetableClosure) =
  rc.theIter = rc.theProc <- rc.data

iterator items*(rc: var ResetableClosure, reset = false): char =
  for x in rc.theIter():
    yield x
  if reset:
    reset(rc)

#[ TODO: Make this something
type Collectable* = concept c, type C
  when C is ref:
    new(typeof(c)) is C
  else:
    init(typeof(c)) is C


proc insertResCall(n: NimNode) =
  var n = n
  while n[^1].kind != 

macro collect*(c: typedesc[Collectable], body: untyped): untyped =
  let res = ident"res"
  echo body.treeRepr
]#