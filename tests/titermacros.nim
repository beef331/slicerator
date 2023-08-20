import slicerator/[itermacros, closures]
import std/unittest

suite "Iterator macros":
  const theVal = [10, 20, 30, 40]
  test "Basic collect":
    # This is used extensively for other tests, so checking it first
    check theVal.items.collect() == @theVal

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
  check theVal.items.group(2).mapIt($it).collect == ["(10, 20)", "(30, 40)"]


import std/[options, tables, sets]
from std/strutils import isUpperAscii, isLowerAscii, toUpperAscii, split

const
  ints = [-2, -1, 1, 3, -4, 5]
  chars = ['a', '.', 'b', 'C', 'z', 'd']
  strs = ["foo", "BAR", "Niklaus", "deadbeef"]
  text = "Epicurus sought tranquility through simplicity and pleasure."

suite "Iterator adaptors":
  test "map":
    # abs is {.inline.}
    check ints.items.map(proc(x:int):int = abs(x)).collect() == @[2, 1, 1, 3, 4, 5]
    check chars.items.map(toUpperAscii).collect() == @['A', '.', 'B', 'C', 'Z', 'D']
    check strs.items.map(toUpperAscii).collect() == @["FOO", "BAR", "NIKLAUS", "DEADBEEF"]

    check ints.items.mapIt(abs(it)).collect() == @[2, 1, 1, 3, 4, 5]
    check chars.items.mapIt(char(it.ord + 1)).collect() == @['b', '/', 'c', 'D', '{', 'e']
    check strs.items.mapIt((var s = it; s.setLen(1); s)).collect() == @["f", "B", "N", "d"]

  test "filter":
    check ints.items.filter(proc(x:int):bool = x > 0).collect() == @[1, 3, 5]
    check chars.items.filter(proc(x:char):bool = x in {'a'..'z'}).collect() == @['a', 'b', 'z', 'd']
    check strs.items.filter(proc(x:string):bool = x.len == 3).collect() == @["foo", "BAR"]

    check ints.items.filterIt(it mod 2 == 0).collect() == @[-2, -4]
    check chars.items.filterIt(it notin {'a'..'d'}).collect() == @['.', 'C', 'z']
    check strs.items.filterIt(it.len > 7).collect() == @["deadbeef"]

  test "group":
    check ints.items.group(2).collect() == @[(-2, -1), (1, 3), (-4, 5)]
    # partial tails are dropped
    check ints.items.group(4).collect() == @[(-2, -1, 1, 3)]
    check chars.items.group(6).collect() == @[('a', '.', 'b', 'C', 'z', 'd')]

  test "skip":
    check ints.items.skip(3).collect() == @[3, -4, 5]
    check chars.items.skip(6).collect() == newSeq[char](0)
    check strs.items.skip(0).collect() == @strs

  test "skipWhile":
    check ints.items.skipWhile(proc(x:int):bool = x < 0).collect() == @[1, 3, -4, 5]
    check ints.items.skipWhileIt(it < 0).collect() == @[1, 3, -4, 5]

  test"take":
    check ints.items.take(3).collect() == @[-2, -1, 1]
    # more than items in the iterator
    check chars.items.take(6).collect() == @chars
    # take zero items
    check strs.items.take(0).collect() == newSeq[string](0)

  test "takeWhile":
    check ints.items.takeWhile(proc(x:int):bool = x < 0).collect() == @[-2, -1]
    check ints.items.takeWhileIt(it < 0).collect() == @[-2, -1]

  test "stepBy":
    check ints.items.stepBy(2).collect() == @[-2, 1, -4]
    check text.items.stepBy(5).foldIt("", (acc.add(it); acc)) == "Ero qtr ly s"
    # first element is always returned
    check chars.items.stepBy(9000).collect() == @['a']

  test "enumerate":
    check ints.items.enumerate.collect() == @[(0, -2), (1, -1), (2, 1), (3, 3), (4, -4), (5, 5)]

  test "flatten":
    iterator splat(s: string): string = (for w in s.split(): yield w) # hack
    let wordEndBytes = text.splat.mapIt(it[^2..^1]).flatten().mapIt(ord(it).byte).collect(set[byte])
    check wordEndBytes == {46.byte, 100, 101, 103, 104, 110, 115, 116, 117, 121}

suite "Iterator consumers":
  test "fold":
    func appended(acc: sink seq[string]; it:int): seq[string] =
      result = acc
      result.add($it)
    proc grow(acc: var seq[string]; it:int) =
      acc.add($it)

    check ints.items.fold(@["acc"], appended) == @["acc", "-2", "-1", "1", "3", "-4", "5"]
    check ints.items.fold(@["acc"], grow) == @["acc", "-2", "-1", "1", "3", "-4", "5"]
    check chars.items.foldIt({'@'}, (acc.incl(it); acc)) == {'.', '@', 'C', 'a', 'b', 'd', 'z'}
    let t = chars.items.enumerate.foldIt(initTable[char, int](), (acc[it[1]]=it[0]; acc))
    check t['d'] == 5

  test "collect to seq":
    check ints.items.collect() == @ints
    check chars.items.collect() == @chars
    check strs.items.collect() == @strs

  test "collect to specific containers":
    check text.items.collect(set[char]) == {' ', '.', 'E', 'a', 'c', 'd', 'e', 'g', 'h', 'i', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'y'}
    check ints.items.collect(seq[int]) == @[-2, -1, 1, 3, -4, 5]
    check strs.items.collect(HashSet[string]) == toHashSet(strs)
    check chars.items.collect(string) == "a.bCzd"

  test "min-max":
    check ints.items.min() == -4
    check chars.items.min() == '.'
    check strs.items.min() == "BAR"

    check ints.items.max() == 5
    check chars.items.max() == 'z'
    check strs.items.max() == "foo"

  test "count":
    check ints.items.count() == 6
    check chars.items.count() == 6
    check strs.items.count() == 4

  test "sum":
    check ints.items.sum() == 2

  test "product":
    check ints.items.product() == -120

  test "any-all":
    check ints.items.anyIt(it > 1) == true
    check chars.items.anyIt(it.isUpperAscii)
    check chars.items.any(isLowerAscii)
    check chars.items.allIt(it in  {'.', 'C', 'a'..'z'})
    check chars.items.all(isLowerAscii) == false
    # empty iterator returns true
    check "".items.allIt(it == '!')

  test "find":
    check ints.items.find(proc(x:int):bool = x > 1) == some(3)
    check ints.items.findIt(it < -2) == some(-4)
    check strs.items.find(proc(x:string):bool = x.items.all(isUpperAscii)) == some("BAR")
    check strs.items.findIt(it == "Dijkstra").isNone()
    check chars.items.find(proc(x:char):bool = x.ord > 'y'.ord) == some('z')

  test "position":
    check ints.items.position(proc(x:int):bool = x > -1) == some(2)
    check ints.items.positionIt(it == 1) == some(2)
    check strs.items.position(proc(x:string):bool = x.items.all(isUpperAscii)) == some(1)
    check strs.items.positionIt(it == "Dijkstra").isNone()
    check chars.items.position(proc(x:char):bool = x.ord > 'y'.ord) == some(4)

  test "nth":
    check ints.items.nth(0) == some(-2)
    check chars.items.nth(6) == none(char)
    check strs.items.nth(1) == some("BAR")
    check text.items.enumerate.filterIt(it[1] in {'x'..'z'}).nth(0) == some((26, 'y'))
