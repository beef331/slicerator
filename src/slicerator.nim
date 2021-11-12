iterator `[]`*[T](a: openArray[T], slice: Slice[int]): T =
  ## Immutable slice iteration over an `openarray`
  for x in a.toOpenArray(slice.a, slice.b):
    yield x

iterator `[]`*[T](a: openArray[T], slice: HSlice[int, BackwardsIndex]): T =
  ## Immutable slice iteration over an `openarray`, taking `BackwardsIndex`
  for x in a[slice.a .. a.len - slice.b.int]:
    yield x

iterator `{}`*[T](a: var openArray[T], slice: Slice[int]): var T =
  ## Mutable slice iteration over an `openarray`
  for i in slice.a..slice.b:
    yield a[i]

iterator `{}`*[T](a: var openArray[T], slice: HSlice[int, BackwardsIndex]): var T =
  ## Mutable slice iteration over an `openarray`, taking `BackwardsIndex`
  for ch in a{slice.a .. a.len - slice.b.int}:
    yield ch

iterator revItems*[T](a: openArray[T]): T =
  ## Reversed immutable items over an `openArray`
  for x in countdown(a.high, 0):
    yield a[x]

iterator revMitems*[T](a: var openArray[T]): var T =
  ## Reversed mutable items over an `openArray`
  for x in countdown(a.high, 0):
    yield a[x]

iterator findAll*[T](a: openArray[T], val: T): int =
  ## Iterates the `openArray` yielding indices that match `val`
  for i, x in a:
    if x == val:
      yield i

iterator mFindAll*[T](a: var openArray[T], val: T): var T =
  ## Iterates the `openarray` yielding mutable values that match `val`
  for i, x in a:
    if x == val:
      yield a[i]

iterator rFindAll*[T](a: openArray[T], val: T): int =
  ## Iterates the `openArray` backwards yield all indices that match `val`
  var i = a.high
  for x in a.revItems:
    if x == val:
      yield i
    dec i

iterator rMFindAll*[T](a: var openArray[T], val: T): var T =
  ## Iterates the `openArray` backwards yielding all mutable values that match `val`
  for x in a.revMitems:
    if x == val:
      yield x

template forMItems*[T](a: var openArray[T], indexName, valName, body: untyped): untyped =
  ## Sugar for iterating over mutable entries getting their indices and value
  var index = 0
  for valname in a.mitems:
    let indexName = index
    body
    inc index