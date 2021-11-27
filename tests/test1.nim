import unittest
import slicerator

test "Test 1":
  let s = "Hello world"
  var res: string
  for x in s[0..2]:
    res.add x
  
  check res == "Hel"
  res.setLen(0)
  for x in s[3..^1]:
    res.add x
  check res == "lo world"
  for x in res{3..5}:
    x = 'a'
  check res == "lo aaald"
  for x in res{3..^1}:
    x = 'b'
  check res == "lo bbbbb"

  var secBuff = ""
  for ch in res.revitems:
    secBuff.add ch
  check secBuff == "bbbbb ol"

  for ch in res.revMitems:
    if ch == 'b':
      ch = 'a'
    else:
      break
  check res == "lo aaaaa"

  for i in res.rFindAll('l'):
    check i == 0

  for i in res.rMFindAll('l'):
    i = 'a'

  check res == "ao aaaaa"

  res.forMItems(ind, it):
    if it == 'o':
      check ind == 1
      it = 'b'

  check res == "ab aaaaa"
  
  var
    someOtherString = "helloworld"
  res = ""
  someOtherString.forMItems(ind, it):
    res.add $ind
    res.add $it
  check res == "0h1e2l3l4o5w6o7r8l9d"

test "asClosure":
  var a = asClosure("hello"[1..2])
  check a() == 'e'
  check a() == 'l'
  discard a()
  check a.finished

  var b = asClosure("hello".pairs)
  check b() == (0, 'h')
  check b() == (1, 'e')
  check b() == (2, 'l')
  check b() == (3, 'l')

  var c = asClosure(1..3)
  check c() == 1
  check c() == 2
  check c() == 3

  var d = asClosure(5..20)
  for x in d():
    check x in 5..20
  check d.finished

  var e = asClosure(countup(1, 4))
  check e() == 1
  check e() == 2
  check e() == 3
  check e() == 4

  iterator numfile(filename: string): int =
    defer: echo "ended"
    for l in fileName:
      yield l.ord

  proc sum(it: iterator): int =
    var t = 0
    for n in it():
        t += n
    return t

  var it = asClosure(numfile"foo")
  check sum(it) == 324
  it = asClosure(numfile"World")
  check sum(it) == 520
  check sum(asClosure(numFile"Hehe")) == 378


import std/strutils

test "Range iters":
  skipIter(1..2, x, 1):
    check x == 2

  const theValue = "Hello\ncruel world"
  skipIter(theValue.splitLines, it, 1):
    check it == "cruel world"
  iterRange(1..4, it, 1..2):
      check it in 2..3

test "Resetable closures":
  var a = asResetableClosure("hello"[1..2])
  for x in a.items(Reset):
    discard

  var b = 0
  for x in a:
    inc b
  check b == 2

  for x in a:
    inc b
  check b == 2
  a.reset

  for x in a:
    inc b
  check b == 4

  a = asResetableClosure("GoodDay"[1..5])
  for x in a.items(Reset):
    inc b
  check b == 9

  for x in a:
    inc b
  check b == 14

import std/sets
test "collectit":
  let a = collectIt(seq[int]):
    for x in 0..3:
      it.add(x)

  check a == @[0, 1, 2, 3]

  let b = collectIt(HashSet[int]):
    for x in 1..3:
      let a = x
      it.incl(a)

  check b == [1, 2, 3].toHashSet

  proc incl(s: var string, b: string) = discard

  let c = collectIt(HashSet[int]):
    for x in 1..3:
      var a = "hello"
      `incl`(a, "Hello")
      if x == 2:
        if true:
          it.incl(x)
        else:
          discard
      else:
        it.incl(10)
  check c == [2, 10].toHashSet