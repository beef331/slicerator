## This module implements a bunch of sugar for closure iterators.
## Making closures from iterators and resettable closures.

import std/[macros, macrocache, sugar, genasts]

type
  ResettableClosure = concept r
    r.data is tuple
    r.theProc is proc
    r.theIter is iterator
  Reset* = distinct void

const closureTable = CacheTable"ClosureObjectTable"


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