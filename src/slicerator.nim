import std/[macros, enumerate, macrocache, typetraits, genasts, strformat]
import closures
export closures

iterator `[]`*[T](a: openArray[T], slice: Slice[int]): T =
  ## Immutable slice iteration over an `openarray`
  for i in slice.a..slice.b:
    yield a[i]

iterator `[]`*[T](a: openArray[T], slice: HSlice[int, BackwardsIndex]): T =
  ## Immutable slice iteration over an `openarray`, taking `BackwardsIndex`
  for x in a[slice.a .. a.len - slice.b.int]:
    yield x

iterator pairs*[T](a: openArray[T], slice: HSlice[int, int]): (int, T) =
  ## Immutable slice iteration over an `openarray`, yielding index and element
  var i = slice.a
  for x in a[slice.a .. slice.b.int]:
    yield (i, x)
    inc i

iterator pairs*[T](a: openArray[T], slice: HSlice[int, BackwardsIndex]): (int, T) =
  ## Immutable slice iteration over an `openarray`, taking `BackwardsIndex`, yielding index and element
  for x in a.pairs(slice.a .. a.len - slice.b.int):
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

import chainimpl
export chain, colChain

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

macro groups*(body: ForLoopStmt): untyped =
  ## Allows iterating over a user defined selection of values in an open array
  runnableExamples:
    for x, y in groups [100, 300]:
      assert [x, y] == [100, 300]
    for x, y in groups [10, 20, 30, 60, 50, 100, 90, 180]:
      assert x * 2 == y

  let
    val = body[^2][1]
    valueName = ident"value"
    assignment = newStmtList()
    count = newLit(body.len - 2)
    packed = # Do we yield every 4 steps or every step after the 4th
      if body[^2].len > 2:
        body[^2][^1]
      else:
        newLit(false)
  for i, x in body[0..^3]:
    assignment.add newLetStmt(x, nnkBracketExpr.newTree(valueName, newLit(i)))
  result = genAst(val, valueName, assignment, packed, count, bod = body[^1]):
    block:
      var valueName: array[count, elementType(val)]
      var pos = 0
      for x in val:
        when packed:
          if pos < count: # We need to buffer them all for packed method
            valueName[pos] = x
            inc pos
            if pos == count:
              assignment
              bod
          else:
            for i in 1..<count:
              valueName[i - 1] = valueName[i]
            valueName[^1] = x
            assignment
            bod
        else:
          valueName[pos] = x
          inc pos
          if pos >= count:
            assignment
            bod
            pos = 0

template map*[T; Y](i: iterable[T], p: proc(x: T): Y): untyped =
  # Applys procedure to an iterator's yielded values
  var res: seq[Y]
  for x in i:
    res.add p(x)
  res

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

macro zip*(others: varargs[untyped]): untyped =
  ## Iterates over the iterators making a `seq[tuple]` of them all,
  ## `tuple` coresponds to the passed in iterators
  runnableExamples:
    assert zip([10, 20, 30], ['a', 'b', 'c']) == @[(10, 'a'), (20, 'b'), (30, 'c')]

  # Ideally this would take `iterable`, but nooooo not allowed
  if others.len == 1:
    error("zipping requires atleast 2 iterators", others)
  elif others.len == 0:
    error("zipping nothing is silly")

  let
    first = others[0]
    tupleConstr = nnkTupleConstr.newTree()
    elemType = bindSym"elementType"
  for other in others:
    tupleConstr.add newCall(elemType, other)

  let
    iSym = genSym(nskVar, "i")
    resSym = genSym(nskVar, "res")

  # First setup the first value
  result = genAst(first, iSym, resSym, tupleConstr):
    var resSym: seq[tupleConstr]
    var iSym = 0
    for x in first:
      resSym.add default(tupleConstr)
      resSym[^1][0] = x
      inc iSym
    iSym = 0

  for tuplInd, other in others:
    if tuplInd > 0: # For each other iterator add another
      result.add:
        genAst(iSym, resSym, tuplInd, other):
          for x in other:
            if iSym < resSym.len:
              resSym[iSym][tuplInd] = x
            else:
              break
            inc iSym
          resSym.setLen(iSym)
          iSym = 0

  result.add:
    genAst(resSym):
      resSym


macro map*(forLoop: ForLoopStmt): untyped =
  ## Iterator based map, iterates over all values yielding the expression applied to the values.
  ## Can be used `for x in map(y, x + 1)` or `for i, x in map(y, x + 3)`.
  runnableExamples:
    let data = [10, 20, 30]
    for i, x in map(data, x * 3):
      assert data[i] * 3 == x

  for i, x in forLoop:
    if i > 1 and x.kind == nnkIdent:
      error("Invalid number of for loop variables for 'map'", x)

  if forLoop[^2].len != 3:
    error("No expression to map provided.", forLoop[^2])

  let
    iter = forLoop[^2][1]
    expr = forLoop[^2][2]
    body = forLoop[^1]
  if forLoop[1].kind == nnkIdent:
    let
      indField = forLoop[0]
      iterField = forLoop[1]
    result = genAst(iter, expr, iterField, body, indField):
      block:
        var indField = 0
        for iterField in iter:
          let iterField = expr
          body
          inc indField

  else:
    let iterField = forLoop[0]
    result = genAst(iter, expr, iterField, body):
      for iterField in iter:
        let iterField = expr
        body

macro filter*(forLoop: ForLoopStmt): untyped =
  ## Iterator based 'filter', runs the iterator yielding only on those that match the expression.
  ## Can be used `for x in all(y, x == 1)` or `for i, x in all(y, x == 3)`.
  runnableExamples:
    let data = [10, 20, 30]
    for i, x in all(data, x == 10):
      assert x == 10

  for i, x in forLoop:
    if i > 1 and x.kind == nnkIdent:
      error("Invalid number of for loop variables for 'filter'", x)

  if forLoop[^2].len != 3:
    error("No expression to filter provided.", forLoop[^2])

  let
    iter = forLoop[^2][1]
    expr = forLoop[^2][2]
    body = forLoop[^1]
  if forLoop[1].kind == nnkIdent:
    let
      indField = forLoop[0]
      iterField = forLoop[1]
    result = genAst(iter, expr, iterField, body, indField):
      block:
        var indField = 0
        for iterField in iter:
          if expr:
            body
          inc indField

  else:
    let iterField = forLoop[0]
    result = genAst(iter, expr, iterField, body):
      for iterField in iter:
        if expr:
          body

macro zipIter*(forBody: ForLoopStmt): untyped =
  ## A version of `zip` that captures iterators as closures which can improve speed and
  ## reduce memory usage.
  ## Supports `for (x, y) in zipIter(a.items, b.items)` and `for x, y in zipIter(a.items, b.items)`.
  runnableExamples:
    let
      a = [10, 20, 30]
      b = "abcdef"
    var count = 0
    for (x, y) in zipIter(a.items, b.items):
      echo x, y # should run 3 times
      inc count
    assert count == 3


  let
    isVarTupl = forBody[0].kind == nnkVarTuple # Is it doing `(x, y) in zipiter`?
    got = forBody[^2].len - 1                  # How many iterators did we get
    expected =                                 # How many fields were passed
      if isVarTupl:
        forBody[0].len - 1
      else:
        forBody[0..^3].len

  if got != expected:
    error(fmt"Expecting {expected} iterators, but got {got}.", forBody[0])

  if forBody[^2].len <= 2:
    error("Cannot zip a single iterator.", forBody[^2])

  let asClos = bindSym"asClosure"
  var
    closNames: seq[NimNode]
    closDecls = newStmtList()

  for x in forBody[^2][1..^1]: # Convert all iters to closures
    let name = genSym(nskLet, "closIter")
    closNames.add name
    closDecls.add newLetStmt(name, newCall(asClos, x))

  template finished(n: NimNode): untyped = newCall("finished", n) # Checks if iterator is finished

  var
    isFin = finished closNames[0] # are we there yet
    asgn = newStmtList()          # The assignments to values before iter

  let varName =
    if isVarTupl:
      forBody[0][0]
    else:
      forBody[0]
  asgn.add newLetStmt(varName, newCall(closNames[0])) # first value is special kinda

  for name in closNames[1..^1]:
    let varName =
      if isVarTupl:
        forBody[0][asgn.len]
      else:
        forBody[asgn.len]
    asgn.add newLetStmt(varName, newCall(name))
    isFin = infix(isFin, "or", finished name) # If any are finished we exit instead of yielding

  result = genAst(asgn, isFin, body = forBody[^1], closDecls):
    closDecls
    while(true):
      asgn
      if isFin: # Closures need to be called one more time to be "finished", which is why not `while(isFin)`
        break
      body
