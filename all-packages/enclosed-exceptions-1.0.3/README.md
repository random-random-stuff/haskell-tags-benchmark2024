enclosed-exceptions
===================

The purpose of this module is to allow you to capture all exceptions originating from within the enclosed computation,
while still reacting to asynchronous exceptions aimed at the calling thread.

This way, you can be sure that the function that calls, for example,  ```catchAny```,
will still respond to ```ThreadKilled``` or ```Timeout``` events raised by another thread 
(with ``throwTo``), while capturing all exceptions, synchronous or asynchronous,
resulting from the execution of the enclosed computation.

One particular use case is to allow the safe execution of code from various
libraries (which you do not control), capturing any faults that might occur,
while remaining responsive to higher level events and control actions.

This library was originally developed by [Michael Snoyman](http://www.snoyman.com/) for the
[ClassyPrelude](http://hackage.haskell.org/package/classy-prelude) library,
and was latter spun-off into a separate independent package.

For a more detailed explanation of the motivation behind this functions, see:

[Catching all exceptions](https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions)

and 
 
[the discussion in haskell-cafe](https://groups.google.com/forum/#!topic/haskell-cafe/e9H2I-3uVJE)
