#!/usr/bin/env python3

def absValue(x):
  if x < 0:
    return x*-1
  else:
    return x
  
def power(x, p):
  if p == 0:
    return 1
  else:
    i = power(x, int(p/2))
    if p%2 == 0:
      return i*i
    else:
      return i*i*x
    
def isPrime(x):
  if x == 0:
    return False
  elif x == 1:
    return False
  else:
    return not (hasDivisors (x, x-1))

def hasDivisors(n, x):
  if x == 1:
    return False
  else:
    return (n%x == 0 or hasDivisors(n, x-1))
  
def slowFib(n):
  if n == 0:
    return 0
  elif n == 1:
    return 1
  else:
    return slowFib(n-1) + slowFib(n-2)

def quickFib(n):
  i = 1
  j = 0
  for x in range (0, n):
    t = i + j
    j = i
    i = t
  return j
    