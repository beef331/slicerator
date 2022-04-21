import slicerator
import timeit
import std/[random, strutils]
import std/sequtils

type MyObject = object
  a, b: int
  c: string
  d: seq[int]
#[
block:
  const runs = 1000
  timeit "non-move closures", runs:
    var myData = newSeq[MyObject](300000)
  do:
    let myClos = myData.items.asClosure(false)
    for x in myClos():
      let y = x
      discard y

  timeit "move closures", runs:
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
]#

block:
  const runs = 1000
  var data = newSeq[string](300000)
  for x in data.mitems:
    x = $rand(0..1000000)

  proc filterProc(a: int): bool = a > 3000
  proc mapProc(a: int): int = a * 30


  timeit "non-move closure complex statement", runs:
    discard
  do:
    for val in data.items.asClosure(false).map(parseInt).filter(filterProc).map(mapProc):
      let newVal = val
      discard newVal

  timeit "move closure complex statement", runs:
    var presentData = data # The closure moves data so need a copy (saves 3ms on my machine)
  do:
    for val in presentData.items.asClosure.map(parseInt).filter(filterProc).map(mapProc):
      let newVal = val
      discard newVal

  timeit "sequtils complex statement", runs:
    discard
  do:
    for val in data.map(parseInt).filter(filterProc).map(mapProc):
      let newVal = val
      discard newVal

  timeit "manual statements", runs:
    discard
  do:
    for x in data:
      var myInt = parseInt(x)
      if myInt > 3000:
        let newVal = myInt * 30
        discard newVal

  timeit "chain macro", runs:
    discard
  do:
    for x in chain data.items.map(parseInt(x)).filter(x > 3000).map(x * 30):
      let newVal = x
      discard newVal
