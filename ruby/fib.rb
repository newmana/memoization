require "rubygems"
require "bundler/setup"
require "method_decorators"
require 'method_decorators/decorators/memoize'
require "benchmark"

class Fib 
  def fib(n)
    n < 2 ? n : fib(n - 1) + fib(n - 2)
  end
end

class FastFib
  extend MethodDecorators 
  +Memoize 
  def fib(n)
    n < 2 ? n : fib(n - 1) + fib(n - 2)
  end
end

time = Benchmark.measure do
  f = Fib.new
  puts f.fib(30)
end
puts time
time = Benchmark.measure do
  ff = FastFib.new
  puts ff.fib(30)
end
puts time
