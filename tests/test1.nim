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