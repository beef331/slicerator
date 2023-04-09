import itermacros, closures
import std/unittest

var theVal = [10, 20, 30, 40]
suite "Iterator macros":
  test "Basic tests":
    for (i, x) in theVal.items.mapIt(it * 2).group(2).enumerate():
      case i
      of 0:
        check x == (20, 40)
      of 1:
        check x == (60, 80)
      else: doAssert false

    for x in theVal.items.mapIt($it).filterIt('1' in it):
      check x == "10"

    for x in theVal.items.skip(2).group(2):
      check x == (30, 40)
  test "Closure tests":
    let myClos1 = asClosure theVal.items.skip(2).group(2)
    check myClos1() == (30, 40)
    let myClos2 = asClosure theVal.items.group(2).mapIt($it)
    check myClos2() == "(10, 20)"
