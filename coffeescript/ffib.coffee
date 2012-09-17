class FastFibonacci

  constructor: (@n) ->
    @memos = []

  toInt: ->
    @fibonacci(@n)

  fibonacci: (n) ->
	  @memos[n] ?= if n < 2 then n else @fibonacci(n - 2) + @fibonacci(n - 1)

module.exports = FastFibonacci