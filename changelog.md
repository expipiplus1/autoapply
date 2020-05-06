# Change Log

## WIP

## [0.3] - 2020-05-06
  - Check constraints on functions

## [0.2.0.0] - 2020-05-01
  - Allow instantiating a polymorphic return type as a monadic value. For
    example `autoapply '[getFooIO] (bar :: Foo -> b)` will have type `IO b`.

## [0.1.0.0] - 2020-04-26
  - Initial release
  - `autoapply` and `autoapplyDecs`
