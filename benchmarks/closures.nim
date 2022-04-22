import slicerator
import timeit
import std/[random, strutils, sequtils, streams]

type MyObject = object
  a, b: int
  c: string
  d: seq[int]

block:
  const runs = 1000

  timeit "closures", runs:
    var myData = newSeq[MyObject](300000)
  do:
    let myClos = myData.items.asClosure
    for x in myClos():
      let y = x
      discard y

  timeit "inline iterators", runs:
    var myData = newSeq[MyObject](300000)
  do:
    for x in myData.items:
      let y = x
      discard y

block:
  const runs = 1000
  var data = newSeq[string](300000)
  for x in data.mitems:
    x = $rand(0..1000000)

  proc filterProc(a: int): bool = a > 3000
  proc mapProc(a: int): int = a * 30

  timeit "closure complex statement", runs:
    var presentData = data
  do:
    for val in presentData.items.asClosure.map(parseInt).filter(filterProc).map(mapProc):
      let newVal = val
      discard newVal

  timeit "manual move closure complex statement", runs:
    var presentData = data
  do:
    var
      closA = presentData.items.asClosure
      closB = map(move closA, parseInt)
      closC = filter(move closB, filterProc)
      closD = map(move closC, mapProc)
    for val in closD:
      let newVal = val
      discard newVal

  timeit "sequtils complex statement", runs:
    var presentData = data
  do:
    for val in presentData.map(parseInt).filter(filterProc).map(mapProc):
      let newVal = val
      discard newVal

  timeit "manual statements", runs:
    var presentData = data
  do:
    for x in presentData:
      var myInt = parseInt(x)
      if myInt > 3000:
        let newVal = myInt * 30
        discard newVal

  timeit "chain macro", runs:
    var presentData = data
  do:
    for x in chain presentData.items.map(parseInt(x)).filter(x > 3000).map(x * 30):
      let newVal = x
      discard newVal

  when defined(writeoutput):
    block:
      var presentData = data
      var myFs = newFileStream("/tmp/closureComplexStmt.tst", fmWrite)
      for val in presentData.items.asClosure.map(parseInt).filter(filterProc).map(mapProc):
        myFs.writeLine($val)
      myFs.close()


    block:
      var presentData = data
      var myFs = newFileStream("/tmp/closureMoveComplexStmt.tst", fmWrite)
      var
        closA = presentData.items.asClosure
        closB = map(move closA, parseInt)
        closC = filter(move closB, filterProc)
        closD = map(move closC, mapProc)
      for val in closD:
        myFs.writeLine($val)
      myFs.close()

    block:
      var presentData = data
      var myFs = newFileStream("/tmp/manualloop.tst", fmWrite)
      for x in presentData:
        var myInt = parseInt(x)
        if myInt > 3000:
          let newVal = myInt * 30
          myFs.writeLine($newVal)
      myFs.close()

