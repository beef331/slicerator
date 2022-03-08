import std/[macros, genasts, strutils, strformat, sugar]

type
  ChainableOps = enum
    coFilter = "filter"
    coMap = "map"
    coUnpack = "unpack"

  ChainOp = object
    op: NimNode
    isIter: bool
    kind: ChainableOps
    val: NimNode ## Only used for deepest

macro unpackCheck(left: untyped, right: typed): untyped =
  ## Internal macro for emitting helpful errors on unpacking attempts
  let rightImpl = right.getType
  if rightImpl.len == 0 or not rightImpl[0].eqIdent"tuple":
    error(fmt"Can only unpack 'tuples': '{right.repr}' is '{rightImpl.repr}'", right)
  elif left.len != rightImpl.len - 1:
    error(fmt"Expected {right.getType.len - 1} variables to unpack into, but got {left.len}.", left[0])
  else:
    result = nnkLetSection.newTree(nnkVarTuple.newNimNode)
    result[0].add left[0..^1]
    result[0].add newEmptyNode()
    result[0].add right

proc getOps(call: NimNode): seq[ChainOp] =
  var n = call[1]
  while n.kind in {nnkCall, nnkDotExpr}:
    # Recurse down statement tree
    try:
      if n[0].kind notin {nnkCall, nnkDotExpr}:
        # it's in `lines("test.txt")` format
        result.add ChainOp(isIter: true, val: n)
        break
      elif n[0][0].kind notin {nnkCall, nnkDotExpr}:
        # it's in `"test.txt".lines()` format
        result.add ChainOp(isIter: true, val: n[0])
      else:
        # It's a chained operation
        let opKind = parseEnum[ChainableOps](n[0][^1].strVal)
        result.add:
          case opKind
          of coUnpack:
            ChainOp(isIter: false, kind: opKind, op: newStmtList(n[1..^1]))
          else:
            ChainOp(isIter: false, kind: opKind, op: n[^1])
      n = n[0][0]
    except:
      error(fmt"Invalid operation {n.repr}.", n)

proc addOp(n, fieldName: NimNode, chainOp: ChainOp) =
  let op = chainOp.op

  proc addOpImpl: NimNode =
    case chainOp.kind:
    of coMap:
      genAst(n, fieldName, op):
        let fieldName = op
    of coFilter:
      # Add discard so we can add later
      genAst(n, fieldName, op):
        if op: discard
    of coUnpack:
      var varTup = nnkTupleConstr.newTree(op[0..^1])
      newCall(bindSym"unpackCheck", varTup, fieldName)

  if n[^1].kind in {nnkLetSection, nnkCall}:
    n.add addOpImpl()
  elif n[^1].kind == nnkDiscardStmt:
    n[^1] = addOpImpl()
  else:
    n[^1].addOp(fieldName, chainOp)

proc addBody(n, body: NimNode) =
  if n[^1].kind == nnkDiscardStmt:
    n[^1] = body
  elif n[^1].kind == nnkLetSection:
    n.add body
  else:
    n[^1].addBody(body)

macro chain*(forloop: ForLoopStmt): untyped =
  ## Allows functional like chaining to iterators,
  ## presently supports `unpack`, `map`, `filter`.
  runnableExamples:
    var a = [10, 20, 30, 40, 50, 60]
    for i, x in chain a.items.filter(i > a.len div 2).map(x * 10):
      assert a[i] * 10 == x

    for i, x in chain a.pairs.unpack(y, z).filter(y > 3):
      assert i > 3
      assert i == y
      assert z in [40, 50, 60]


  let passedVars = block:
    var val = 0
    for n in forLoop:
      if n.kind != nnkIdent:
        break
      inc val
    val

  if passedVars > 2:
    error("Invalid variable count passed to 'chain'.", forLoop[2])

  var ops = forloop[^2].getOps
  let
    varName =
      if passedVars == 2:
        forLoop[1]
      else:
        forLoop[0]
    body = forLoop[^1]
    iter = ops.pop
    lastOp = ops.pop

  case lastOp.kind: # Last op is kinda special
  of coMap:
    result = genAst(iter = iter.val, varName, op = lastOp.op):
      for varName in iter:
        let varName = op
  of coFilter:
    result = genAst(iter = iter.val, varName, op = lastOp.op):
      for varName in iter:
        if op: discard
  of coUnpack:
    var varTup = nnkTupleConstr.newTree(lastOp.op[0..^1])
    varTup = newCall(bindSym"unpackCheck", varTup, varName)
    result = genAst(iter = iter.val, varName, varTup):
      for varName in iter:
        varTup

  while ops.len > 0: # Add remaining ops
    result.addOp(varName, ops.pop)
  result.addBody(body)
  if passedVars == 2: # We have an index variable, so emit calls for it
    result[^1].add newCall("inc", forLoop[0])
    result = nnkBlockStmt.newTree(newEmptyNode(), newStmtList(newVarStmt(forLoop[0], newLit(0)), result))

macro colChain*(forloop: ForLoopStmt): untyped =
  ## Wrapper for collecting `chain`.
  ## Works the same as chain but collects the stmt of the for loop
  result = forLoop.copyNimTree
  if result[1].kind != nnkCommand:
    error("Too many iterator parameters provided", forLoop)
  result[1][0] = bindSym"chain"
  result = newCall(bindSym"collect", result)
