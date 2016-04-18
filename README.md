eratosthenes
============

[![Build Status](https://img.shields.io/travis/altenwald/eratosthenes/master.svg)](https://travis-ci.org/altenwald/eratosthenes)
[![License: LGPL 2.1](https://img.shields.io/github/license/altenwald/eratosthenes.svg)](https://raw.githubusercontent.com/altenwald/eratosthenes/COPYING)

The sieve of Eratosthenes is an ancient method to obtain all the prime numbers under `N`. The method to calculate is:

- We have a list of numbers `1..N`, we consider all of them as prime numbers.
- Starting with 2, we have to mark as _no prime_ all the numbers multiple of 2 (2x1, 2x2, 2x3, ...), and then we follow with the next prime number, in this case 3.
- If we find a number marked as _no prime_ we skip it and follow to the next.
- Finally we get all the numbers still marked as _prime_ when we arrive to N.

As a improvement, checks are performed only for `1..sqrt(N)`, because we know that all of the following numbers are marked and remains only prime numbers there.

You can use this library in this way:

```erlang
% generate prime numbers from 1..1000
eratosthenes:start_link(1000).

% use it for checks:
eratosthenes:is_prime(97).
% true

eratosthenes:is_prime(6).
% false
```

Enjoy!
