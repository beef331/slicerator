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
    echo n.treeRepr
    try:
      if n[0].kind notin {nnkCall, nnkDotExpr}:
        result.add ChainOp(isIter: true, val: n)
        break
      elif n[0][0].kind notin {nnkCall, nnkDotExpr}:
        result.add ChainOp(isIter: true, val: n[0])
      else:
        let opKind = parseEnum[ChainableOps](n[0][^1].strVal)
        result.add ChainOp(isIter: false, kind: opKind, op: n[^1])
      n = n[0][0]
    except:
      error(fmt"Invalid operation {n.repr}.", n)

proc addOp(n, fieldName: NimNode, chainOp: ChainOp) =
  let op = chainOp.op

  if n[^1].kind == nnkLetSection:
    n.add:
      case chainOp.kind:
      of coMap:
        genAst(n, fieldName, op):
          block:
            let fieldName = op
      of coFilter:
        genAst(n, fieldName, op):
          if op: discard
  elif n[^1].kind == nnkDiscardStmt:
    n[^1] =
      case chainOp.kind:
      of coMap:
        genAst(n, fieldName, op):
          block:
            let fieldName = op
      of coFilter:
        genAst(n, fieldName, op):
          if op: discard
  else:
    n[^1].addOp(fieldName, chainOp)

proc addBody(n, body: NimNode) =
  if n[^1].kind == nnkDiscardStmt:
    n[^1] = body
  elif n[^1].kind == nnkLetSection:
    n.add body
  else:
    n[^1].addBody(body)

macro chain(forloop: ForLoopStmt): untyped =
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

  case lastOp.kind:
  of coMap:
    result = genAst(iter = iter.val, varName, op = lastOp.op):
      for varName in iter:
        let varName = op
  of coFilter:
    result = genAst(iter = iter.val, varName, op = lastOp.op):
      for varName in iter:
        if op: discard

  while ops.len > 0:
    result.addOp(varName, ops.pop)

  result.addBody(body)
  if passedVars == 2:
    result[^1].add newCall("inc", forLoop[0])
    result = nnkBlockStmt.newTree(newEmptyNode(), newStmtList(newVarStmt(forLoop[0], newLit(0)), result))

  echo result.repr


var val = 0

for x in chain lines("input.txt").map(parseInt(x)).filter(x < 3):
  inc val
