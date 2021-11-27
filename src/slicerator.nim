import std/[macros, sugar, genasts, enumerate, macrocache]

type
  ResettableClosure = concept r
    r.data is tuple
    r.theProc is proc
    r.theIter is iterator
  Reset* = distinct void

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

const closureTable = CacheTable"ClosureObjectTable"

macro subClosureType(t: tuple, rstClosure: typedesc[ResettableClosure]) =
  let strType = t.getType.repr
  closureTable[strType] = rstClosure

macro hasClosureType(t: tuple): untyped =
  let strType = t.getType.repr
  result = newLit(false)
  for x, y in closureTable:
    if strType.eqIdent(x):
      result = newLit(true)
      break

macro getClosureType(t: tuple): untyped =
  let strType = t.getType.repr
  for x, y in closureTable:
    if strType.eqIdent(x):
      result = y
      break

macro asResettableClosure*(iter: iterable): untyped =
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
    closureProc = closure[1][0][0]
    typName = genSym(nskType, "AnonResetClos")
  result =
    genAst(closure, closureProc, tupleData, typName):
      block:
        let clos = closure
        when hasClosureType(tupleData):
          getClosureType(tupleData)(data: tupleData, theProc: closureProc, theIter: clos)
        else:
          type typName = object
            data: typeof(tupleData)
            theProc: typeof(closureProc)
            theIter: typeof(clos)
          subClosureType(tupleData, typName)
          typName(data: tupleData, theProc: closureProc, theIter: clos)

macro `<-`(prc: proc, data: tuple): untyped =
  result = newCall(prc)
  for i, _ in data.getTypeInst:
    result.add nnkBracketExpr.newTree(data, newLit(i))

proc reset*(rc: var ResettableClosure) =
  rc.theIter = rc.theProc <- rc.data

iterator items*(rc: var ResettableClosure): auto =
  ## Iterates over `ResettableClosure`
  for x in rc.theIter():
    yield x

iterator items*(rc: var ResettableClosure, _: typedesc[Reset]): auto =
  ## Iterates over `ResettableClosure` resetting after done
  for x in rc.theIter():
    yield x
  reset(rc)

macro nameConstr(t: typed, useNew: static bool): untyped =
  var t = getType(t)
  if t[0].eqIdent("typedesc"):
    t = t[^1]
  let
    isGeneric = t.kind == nnkBracketExpr
    name =
      if useNew:
        "new"
      else:
        "init"
    procName = ident name & (
      if isGeneric:
        $t[0]
      else:
        $t)
  result =
    if isGeneric:
      newCall(nnkBracketExpr.newTree(procName, t[^1]))
    else:
      newCall(procName)

template isNew(b, B): untyped =
  nameConstr(b, true) is B

template isInit(b, B): untyped =
  nameConstr(b, false) is B

type
  BuiltInInit = concept b, type B
    isInit(b, B)
  BuiltInNew = concept b, type B
    isNew(b, B)
  UserInited = concept u, type U
    init(U) is U
  UserNewed = concept u, type U
    new(U) is U


template collectIt*(collection: typedesc, body: untyped): untyped =
  ## Much like `std/sugar`.
  ## Supply a type that you want to, instantiates `it` and exposes it as such.
  runnableExamples:
    let a = collectit(seq[int]):
      for x in 0..3:
        it.add(x)
    assert a == @[1, 2, 3, 4]
    let c = collectit(HashSet[int]):
      for x in 1..3:
        var a = "hello"
        if x == 2:
          it.incl(x)
        else:
          it.incl(10)

  block:
    var it {.inject.} =
      when collection is BuiltInInit:
        namedConstr(collection, false)
      elif collection is BuiltInNew:
        nameConstr(collection, true)
      elif collection is UserInited:
        init(collection)
      elif collection is UserNewed or collection is ref:
        new(collection)
      else:
        collection()
    body
    it
