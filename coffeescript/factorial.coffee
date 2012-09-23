fact = (n) ->
  if n < 1 then 1 else n * fact(n - 1)

memos = []
ffact = (n) ->
  memos[n] ?= if n < 1 then 1 else n * ffact(n - 1)

seconds_passed = (d1, d2) ->
  Math.round((d2.getTime() - d1.getTime()))

s = new Date
console.log fact(50)
console.log seconds_passed(s, new Date)
s = new Date
console.log ffact(50)
console.log seconds_passed(s, new Date)
