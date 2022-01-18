import std/[macros, genasts, strutils, strformat]
import slicerator

type
  ChainableOps = enum
    coFilter = "filter"
    coMap = "map"

  ChainOp = object
    op: NimNode
    isIter: bool
    kind: ChainableOps
    val: NimNode ## Only used for deepest

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
        result.add ChainOp(isIter: false, kind: opKind, op: n[^1])
      n = n[0][0]
    except:
      error(fmt"Invalid operation {n.repr}.", n)

proc addOp(n, fieldName: NimNode, chainOp: ChainOp) =
  let op = chainOp.op

  proc addOpImpl: NimNode =
    case chainOp.kind:
    of coMap:
      # Add block so we can redefine the variable in scope
      genAst(n, fieldName, op):
        let fieldName = op
    of coFilter:
      # Add discard so we can add later
      genAst(n, fieldName, op):
        if op: discard

  if n[^1].kind == nnkLetSection:
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

  while ops.len > 0: # Add remaining ops
    result.addOp(varName, ops.pop)

  result.addBody(body)
  if passedVars == 2: # We have an index variable, so emit calls for it
    result[^1].add newCall("inc", forLoop[0])
    result = nnkBlockStmt.newTree(newEmptyNode(), newStmtList(newVarStmt(forLoop[0], newLit(0)), result))
