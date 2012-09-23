fib = (n) ->
  if n < 2 then n else fib(n - 2) + fib(n - 1)

memos = []
ffib = (n) ->
  memos[n] ?= if n < 2 then n else ffib(n - 2) + ffib(n - 1)

seconds_passed = (d1, d2) ->
  Math.round((d2.getTime() - d1.getTime()))

s = new Date
console.log fib(40)
console.log seconds_passed(s, new Date)
s = new Date
console.log ffib(40)
console.log seconds_passed(s, new Date)
