import std/[monotimes, times]
export `$`
template timeit*(name: string, runs: int, pre, body: untyped) =
  var totalTime: Duration
  for x in 0..<runs:
    pre
    let start = getMonoTime()
    body
    totalTime += getMonoTime() - start
  echo name, " took on average: ", totalTime div runs
