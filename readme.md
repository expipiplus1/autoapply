# autoapply

A Template-Haskell program to automatically pass arguments to functions
wherever the type fits.

## Why to use it

One nice use-case is to avoiding writing boilerplate wrappers for using an API
in your Monad stack. For instance imagine the following API.

```haskell
data Instance
data ExtraOpenInfo
data Foo
data Bar
data Handle
openHandle :: MonadIO m => Instance -> Maybe ExtraOpenInfo -> m Handle
closeHandle :: MonadIO m => Instance -> Handle -> m ()
useHandle :: MonadIO m => Instance -> Handle -> Foo -> m Bar
```

You'd like to use this in your `polysemy` application, using the `Input`
effect to pass the `Instance` handle around, and always passing `Nothing` for
`ExtraOpenInfo` because you don't use that functionality. You define the
following values.

```haskell
myExtraOpenInfo :: Maybe ExtraOpenInfo
myExtraOpenInfo = Nothing
getInstance :: Member (Input Instance) r => Sem r Instance
getInstance = input
```

You then create the wrapped API thusly:

```haskell
autoapplyDecs (<> "'") ['myExtraOpenInfo, 'getInstance] ['openHandle, 'closeHandle, 'useHandle]
```

Which creates the following declarations. Notice how the `Instance` is supplied
with the `Member (Input Instance) r` constraint and the `ExtraOpenInfo` is not
present at all, being supplied internally by `myExtraOpenInfo`. To see the
generated code (it's exactly what you'd expect) compile `test/Types.hs` with
`-ddump-splices`.

```haskell
openHandle'
  :: (Member (Input Instance) r, MonadIO (Sem r)) => Sem r Handle
closeHandle'
  :: (Member (Input Instance) r, MonadIO (Sem r)) => Handle -> Sem r ()
useHandle'
  :: (Member (Input Instance) r, MonadIO (Sem r)) => Handle -> Foo -> Sem r Bar
```

## How to use this

To generate a new top-level declaration you'll need:

- The `Name` of a function to apply to some arguments.
- The `Name`s of some values to try and pass as arguments.
- A way of generating a name for this declaration given the wrapped name
  `:: String -> String`.

The new declaration will be generated, equal to the wrapped one but using the
supplied arguments wherever possible.

Arguments can be used in two ways:

- As regular parameters
  - If the type of the argument matches directly
  - An example is applying `takeWhile` to `not`; `not` is passed as the `a -> Bool`
    argument to `takeWhile`. `$(autoapply ['not] 'takeWhile) :: [Bool] -> [Bool]`

- Using a monadic bind
  - If the wrapped function returns a value of type `m a` and there exists an instance `Monad m`
  - If the argument is of type `n a` and there exists an instance `Monad m`
  - If `m` unifies with `n`
  - An example is applying `putStrLn` to `getLine`. The `String` result of `getLine` is passed to `putStrLn`
    `$(autoapply ['getLine] 'putStrLn) :: IO ()`

It's important to note that `Monad` instance checking only goes as far as
`template-haskell`'s `reifyInstances`. i.e. only the instance heads are
checked.

Aside for checking for a `Monad` instance, no constraints are checked. So `autoapply`
will happily pass `reverse` to `(+)` yielding a value of type `Num ([a] -> [a]) => [a] -> [a]`.

You may want to either type your generated declarations manually (putting the
type after the splice) or turn on `-XNoMonomorphismRestriction` if your
arguments have polymorphic constraints.

## Where to use it

- In an expression context:
  - `$(autoApply ['my, 'arguments] 'myFunction)`

- At the top level to generate several declarations
  - `$(autoApplyDecs (funNameToNewFunName :: String -> String) ['my, 'arguments] ['myFunction, 'anotherFunction])`

## See also

This has a similar feel to some other programs which also generate Haskell
expressions based on types.

- [djinn](https://hackage.haskell.org/package/djinn)
- [exference](http://hackage.haskell.org/package/exference) ([github](https://github.com/lspitzner/exference))
- [JustDoIt](https://www.joachim-breitner.de/blog/735-The_magic_%E2%80%9CJust_do_it%E2%80%9D_type_class)

There are a couple of differences here:

- One doesn't need to specify the desired type up front, this tool will just go
  as far as it can.
- This tool isn't doing any interesting search instead it's just "if it fits, I sits"
