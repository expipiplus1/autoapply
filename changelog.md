# Change Log

## WIP

## [0.4] - 2020-05-06
  - Allow specifying if the types of potential arguments should subsume or just
    unify with the function's argument types
  - Respect constraints when passing arguments (ignoring type families)

## [0.3] - 2020-05-06
  - Check constraints on functions

## [0.2.0.0] - 2020-05-01
  - Allow instantiating a polymorphic return type as a monadic value. For
    example `autoapply '[getFooIO] (bar :: Foo -> b)` will have type `IO b`.

## [0.1.0.0] - 2020-04-26
  - Initial release
  - `autoapply` and `autoapplyDecs`
