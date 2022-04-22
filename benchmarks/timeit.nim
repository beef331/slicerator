import std/[monotimes, times]
export `$`
template timeit*(name: string, myRuns: int, pre, body: untyped) =
  var totalTime: Duration
  let myTimeProc = proc(): Duration =
    pre
    let start = getMonoTime()
    body
    result = getMonoTime() - start
  for _ in 0..<myRuns:
    totalTime += myTimeProc()
  echo name, " took on average: ", totalTime div myRuns
