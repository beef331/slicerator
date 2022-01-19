import std/[unittest, strutils]
import slicerator


suite "chain":
  test"strtest":
    const data = "1000\n2000\n3000\n4000\n5000\n6000"
    for x in chain data.splitLines.map(parseint(x) * 2).filter(x < 3000):
      check x == 2000
    for x in chain splitLines(data).filter(x[0] notin {'3', '5'}).map(parseint(x)):
      check x in [1000, 2000, 4000, 6000]

  test"intarray":
    var a = [10, 20, 30, 40, 50, 60]
    for i, x in chain a.items.filter(i > a.len div 2).map(x * 10):
      check a[i] * 10 == x

    for i, x in chain a.pairs.unpack(y, z).filter(y > 3):
      check i > 3
      check i == y
      check z in [40, 50, 60]
