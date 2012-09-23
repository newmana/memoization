require "rubygems"
require "bundler/setup"
require "method_decorators"
require 'method_decorators/decorators/memoize'
require "benchmark"

class Object
  extend MethodDecorators 
end

def fib(n)
  n < 2 ? n : fib(n - 1) + fib(n - 2)
end

+Memoize 
def ffib(n)
  n < 2 ? n : ffib(n - 1) + ffib(n - 2)
end

def time(exec)
  time = Benchmark.measure do
    puts exec.call
  end
  puts time
end  

l1 = lambda { fib(40) }
l2 = lambda { ffib(40) }
time(l1)
time(l2)