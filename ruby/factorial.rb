require "rubygems"
require "bundler/setup"
require "method_decorators"
require 'method_decorators/decorators/memoize'
require "benchmark"

class Object
  extend MethodDecorators 
end

def fact(n)
  n < 1 ? 1 : n * fact(n - 1)
end

+Memoize 
def ffact(n)
  n < 1 ? 1 : n * ffact(n - 1)
end

def time(exec)
  time = Benchmark.measure do
    puts exec.call
  end
  puts time
end  

time(lambda { fact(50) })
time(lambda { ffact(50) })
time(lambda { fact(500) })
time(lambda { fact(500) })