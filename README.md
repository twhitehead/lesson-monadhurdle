% The Monad Hurtle
% Tyson Whitehead
% 2019-07-17

# Abstract

Monads are a design pattern in programming linked to category theory in the early 90s.  They have been a
fundamental part, that is, required to perform IO, of the lazy functional language Haskell for almost as long.  Due
to their considerable success in Haskell, formulations have also now sprung up for many other languages.  This
includes, according to Wikipedia, Scheme, Perl, Python, Racket, Clojure, Scala, F#, and possibly even the newest ML
standard.

There is a lot of confusing information about monads on the internet.  This ranges from very technical category
theory based statements (e.g., monads are just monoids in the category of endofunctors) to very non-technical
analogies (e.g., ones involving nuclear waste and spacesuits).  The goal of this presentation is to present a
non-confusing, practical, programmer-based perspective on monads with plenty of examples that also doesn't skimp on
the technicalities.


# Introduction

I would like to start by saying it isn't important that everything makes immediate sense.  Each time you dive into
monads, you learn a little more.  I now have a lot better understanding than I did when I first started over twelve
years ago, but I'm still learning too, especially when it comes to the category theoretic side.

Over the years since their introduction, it has been realized that the monad structure can actually be broken out
into several layers, where each new layer builds upon the previous.  These layers are

Pointed:
 ~ values
 
Functor:
 ~ functions

Applicative:
 ~ values + functions + products
 
Monad:
 ~ values + functions + products + layers

This is much like an object hierarchy in a object orientated language.  Later layers build on and add functionality
to prior layer.  Each later layer also has less inhabitants as the increased functionality corresponds to increased
requirements on the objects providing that functionality, resulting in there being fewer of them.


# Functions

One item we need to get out the way is that Haskell functions are curried.  That is, in another language we might
have the prototype and implementation of the `add` function as

```haskell
add :: (Int,Int) -> Int
add (x,y) = x + y
```

where the `::` indicates we are specifying the type and the `->` indicates a function from the first type to the
second.  The first line is thus a prototype which specifies `add` is a function that takes two `Int`s and returns a
new `Int`, and the second line provides the actual implementation.  We would then use it like so

```haskell
add (1,2) :: Int
```

where we are using `::` to clarify that the type (i.e., the type of `add` applied to `(1,2)` is an `Int`).  The
issue with grouping all are arguments into a tuple is that always have to supply both at once.  Haskell, while
accepting this, also givess us the option of defining `add` such that it takes its arguments one at a time

```haskell
add :: Int -> (Int -> Int)
add x y = x + y
```

In this version, `add` is a function that takes a single `Int` and returns a function that that accepts the
remaining `Int` and then adds it to the previously given `Int`.  This allows us to use `add` as

```haskell
add :: Int -> (Int -> Int)
add 1 :: Int -> Int
(add 1) 2 :: Int
```

where the brackets are all redudant as the function type `->` associates to the right and argument application
associates to the left.


# Pointed

Let us start with the functionality we refer to as pointed by considering the `Maybe a` type

```haskell
data Maybe a = Nothing
             | Just a
             deriving Show
```

This type allows us to extend other types that don't have the notion of a missing value or a failure to have this
notion.  For example, an `Int` has to be a number.  If we wrap it with `Maybe` though, we get a `Maybe Int`, which
now lets us have either a `Nothing` or a `Just Int`.  What we are doing here is taking our set of built in types
and building upon them to get new set of types

* `Int  -> Maybe Int`
* `Char -> Maybe Char`
* etc.

The above `Maybe a` definition defines the value constructors

```haskell
Nothing :: Maybe a
Just :: a -> Maybe a
```

The type for `Just` specified that it is a function that takes a generic type `a` and returns a new type `Maybe a`.

This is our first design pattern.  We have a type constructor that creates new types from old types paired with a
value constructor that creates new values of the new type from old values of the old type.  To be explicit, using
`*` as the type of types, the `Maybe` constructs a types from a given type

```haskell
Maybe :: * -> *
```

and `Just` constructs a new value of the new type from a given value of the original type

```haskell
Just :: a -> Maybe a
```

where we use the unconstrained type variable `a` to indicate an arbitrary type.  Commonly we would refer to the
operation provided by `Just` as lifting values of the original type to the new type.

This design pattern of providing a function to lift values of an original type to derived type applies pretty
widely and the generic name given to operation is `pure` (more recently) or `return` (historically).  The essence
of the pattern is captured in the class declaration

```haskell
class Pointed m where
  pure :: a -> m a
```

This says that for a type contructor `m` to be pointed a corresponding function has to be given to take arbitrary
types `a` and lift them to the new type `m a`.  The corresponding type of `pure` is

```haskell
pure :: Pointed m => a -> m a
```

which reads that `pure` is a function that can, for a given pointed type constructor `m`, take a value of some type
`a` and return a new value of some type `m a`.  The primary restriction on this function is that it has to be
generic because it has to work for all types.  This is evident by the fact that there are no restrictions on what
`a` is, and without knowing anything about `a`, we are limited to just interweaving it into some structure.

This is precisely what we do with our `Maybe` type constructor.  We just wrap the given value with `Just`

```haskell
instance Pointed Maybe where
  pure :: a -> Maybe a
  pure x = Just x
```

Now let us consider a list type

```haskell
data List a = Nil
           | Cons a (List a)
           deriving Show
```

With some thought it also evident that it isn't too difficult to make a function that takes any type and returns
a list of that type.  For example, we could just wrap the given value up as a singelton list

```haskell
instance Pointed List where
  pure a = Cons a Nil
```

or, at the other extreme, we could even repeat it forever

```haskell
instance Pointed List where
  pure :: a -> List a
  pure a = Cons a (pure a)
```

Both of these are perfectly valid.  Which one we want will depend on what we are doing.  The first one is what
is used with the builtin haskell lists.

In reality, the pointed functionality isn't considered worth its weight on its own.  Instead it is combined with
with other functionality.


# Functor

Pointed gives us the ability to lift our types.  That is, to take our old values of our old type and turn them into
new values of the new type.  It isn't too long before you also want to lift your functions.  That is, take your old
functions over those old values and turn them into new functions over the new values.

This is the functor pattern.  We can use our class syntax to describe it

```haskell
class Functor m where
  fmap :: (a -> b) -> (m a -> m b)
```

which says that for a type contructor `m` to be a functor, there has to be a function that can take an arbitrary
function, that is, one from some (unspecified) type `a` to some type `b`, and give a new version that works on the
new types, that is from `m a` to `m b`.

How would this look with our `Maybe` type constructor?  Our `fmap` function would take an arbitrary function `xy`
between two arbitrary types `a` and `b`, and it would have to create an correponding function between the
corresponding types `Maybe a` and `Maybe b`.  Without knowing what `a` and `b` are ahead of time, we are again very
limited in what we can.  If we don't have an `a` the only `Maybe b` we can generate is `Nothing`.  If we do have an
`a` though, we can apply `xy` to it to get a `b` and then wrap it with `Just`

```haskell
instance Functor Maybe where
  fmap :: (a -> b) -> (Maybe a -> Maybe b)
  fmap xy (Just x) = Just (xy x)
  fmap _  Nothing  = Nothing
```

where `_` is used as a placeholder for arguments we don't care about.

Now consider our list type.  It too follows the same pattern.  Given a function `xy :: a -> b`, we can generate a
function that, when given a list of values of type `a` will apply `xy :: a -> b` to each of them to generate a list
of values of type `b`.  That is

```haskell
instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap xy Nil         = Nil
  fmap xy (Cons x xs) = Cons (xy x) (fmap xy xs)
```

In Haskell, the class of types associated with I/O operations are wrapped with the type constructor `IO`.  As
an example of such a type, consider the `getLine` compuation

```haskell
getLine :: IO String
```

This is a compuation that will get a line worth of input from stdin.  As `IO` is a functor, we can use its
`fmap` functionality to apply a `String -> String` function that appends a newline to the given string
to generate a new computation from `getLine` the returns the given line plus an appended newline.

```haskell
getLineNL :: IO String
getLineNL = fmap (++ "\n") getLine
```

where `++` is the `String` append operation.


# Pointed + Functor

There is more to this story than just recognizing common functionality and creating associated classes that
encapsulates them.  We also require the implementations to interact in a reasonable manner.  Generally this means
things like two apparently equivalent ways of doing the same thing should both give the same answer.

For instance, given two functions

```haskell
xy :: a -> b
yz :: b -> c
```

proper `fmap` and `pure` implementations must be such that it doesn't matter if we lift and then combine or combine
and then lift.  That is, it should be equivalent to first lift `xy` and apply it, and then lift `yz` and apply it,
or just lift the combination of `yz . xy :: a -> c` and apply it

```haskell
fmap yz (fmap xy mx) = fmap (yz . xy) mx
```

Similairly, applying a function `xy :: a -> b` to value `x :: a` and then lifting it needs to give the same result
as first lifting the value and then applying the lifted function to it

```haskell
fmap xy (pure x) = pure (xy x)
```

These guarantees, which esssentialy limit our lifting of values to weaving them through our structures and our
lifting of functions to applying them to our weaved values, allow the programmer to do basic reasoning about
arbitrary functor code.  It also allows the compiler to perform basic optimizations, such as merging two walkings
of a list into one.


# Applicative

The applicative pattern allows extends the pointed and functor patterns with the ability to combine our lifted
types.  This pattern is captured by the class declaration

```haskell
class (Pointed m, Functor m) => Applicative m where
  pair :: m a -> m b -> m (a,b)
```

where the `=>` is saying that for a type contructor `m` to be applicative, it also has to be pointed and a functor.

The type signature for the uncurried version of pair makes it especially obvious that the essense of the next level
of the pattern is that the product can pass into the type constructor

```haskell
uncurry pair :: (m a, m b) -> m (a, b)
```

The implementation for the `Maybe` type constructor follows the same pattern as before.  The type are arbitrary,
and we cannot generate values for types we do not know, so the only way we can generate a pair is if we are
given values of both types.  Otherwise we will have to return a `Nothing`.

```haskell
instance Applicative Maybe where
  pair :: Maybe a -> Maybe b -> Maybe (a, b)
  pair (Just x) (Just y) = Just (x, y)
  pair _        _        = Nothing
```

Our list type is more interesting.  In this case we need to come up with some way of pairing some number of values
of some type `a` with a potentially different number of values of some type `b`.  Two possible solutions are a
truncated inner product or an outer product.  The first is commonly known as zipping the lists

```haskell
instance Applicative List where
  pair :: List a -> List b -> List (a, b)
  pair (Cons x xs) (Cons y ys) = Cons (x, y) (pair xs ys)
  pair _           _           = Nil
```

and it pairs elements at the same position in both lists, truncatin on the shorter of the two lists.  The second
pairs all elements of the first list with all elements of the second list

```haskell
instance Applicative List where
  pair :: List a -> List b -> List (a, b)
  pair (Cons x xs) ys = append (fmap (x,) ys) (pair xs ys)
    where
      append (Cons x xs) ys = Cons x (append xs ys)
```

where `(x,)` is a function that that takes the second argument of the tuple and returns the completed tuple.

As with the functor pattern, the applicative pattern requires the `pair` function to operate properly with respect
to nested applications, lifted functioned, and lifted values.  It shouldn't matter whether we compose pairing on
the left or the right, that is, `(x,(y,z))` should be essentially equivalent to `((x,y),z)`

```haskell
fmap (\(x,(y,z)) -> (x,y,z)) ( pair mx (pair my mz) ) = fmap (\((x,y),z) -> (x,y,z)) ( pair (pair mx my) mz )
```

where the `fmap` are used to normalize the two three tuple expressions to the commnon `(x,y,z)`.  As with
the functor pattern, we also expect two ways of apparently achieving the same results with lifted values
to indeed be just two ways of achieving the same result

```haskell
pair (pure x) my = fmap (x,) my
pair mx (pure y) = fmap (,y) mx
```

An equivalent formulation to `pair` is `apply`, which is characterized by allowing `->` to pass out of the type
constructor

```haskell
apply :: m (a -> b) -> m a -> m b
apply mab ma = fmap go (pair mab a)
  where
    go ab a = ab a
```

and the applicative pattern is usually given in terms of `apply` and not `pair`.  Given `apply` it is equally
easy to define `pair` though, which, combined with the above, shows the two are entirely equivalent

```haskell
pair :: m a -> m b -> m (a,b)
pair ma mb = apply (fmap (,) ma) mb
```

Haskell provides operator versions of both `fmap` and `apply`, respectively `<$>` and `<*>`.  This allows
the more pleasant specification of pair as

```haskell
pair :: m a -> m b -> m (a,b)
pair ma mb = apply <$> ma <*> mb
```

where the general pattern is the first arguement is applied with `<$>` and the remaining arguments with
`<*>`.  The reason this works can be seen from considering lifting an arbitrary function `xyz` and applying
arguements to it

```haskell
xyz :: a -> b -> c
xyz <$> ::  m a -> m (b -> c)
xyz <$> mx :: m (b -> c)
xyz <$> mx <*> :: m b -> m c
xyz <$> mx <*> my :: m c
```

The reason we choose to introduce it in terms of the `pair` function though is this emphasizes that one of
the key features of applicative is it introduces an ordering.  This is key to understanding why these
design patterns are useful for modeling I/O.

The evaluation ordering in a lazy language is notoriously difficult to think about.  Arguments are not evaluated
when they passed to functions or assigned to variables.  Rather they are stored as a computation to be performed at
some later point should the value actually be used as required by the data dependencies.

This is a nightmare with respect to I/O.  Operating system calls involve all sorts of non-data dependencies
whereby one call has to be made before another but that isn't obvious from looking at the flow of inputs and
outputs to the functions.

The applicative pattern provides a solution to this.  The `IO` type constructor represents compuations returning
values of the given type, and functionality provided by applicative gives a way of combining them in a manner
that provides an explicit ordering.

As an example, consider we can use pair to build an I/O operation that runs the `getLine` operation twice

```haskell
pair getLine getLine :: IO (String, String)
```


#  Monad

It may seem that the applicative pattern may give us everthing we need, and this is frequently the case.  It turns
out though that it can only compose computations where we collect all the individual bits independently and then
compute some final answer with them.  It can not compose computations were the results of prior computations is
used in subsequent computations.

To make this more concrete, consider a simple example of such a computation.  We want to compose `getLine` and
`putStrLn` in such a way that it will read a name from stdin and then write "hello" followed by that name to
stdout.  The requried `input` and `output` functions are as follows

```haskell
input :: IO String
input = getLine

output :: String -> IO ()
output name = putStrLn ("hello " ++ name)
```

where `()` is the empty tuple and reflects the fact that `putStrLn` computation does not generate any results.
Rather is used purely for its side effect of printing its output to stdout.  The string associated with the `input`
computation is wrapped in the `IO` type.  This means that to apply our `output` function, we are going to have
to lift our `output` function so its input becomes `IO String` as that is the type of our `input`

```haskell
output :: String -> IO ()
fmap output :: IO String -> IO (IO ())
fmap output input :: IO (IO ())
```

and now we are stuck because our next computation has wound up becoming result of the prior computation
and we have no way of sequencing it.

This is the functionality provide by the monad pattern.  It is the ability to squash two levels of our type
constructor into one.  This is captured in the class declaration

```haskell
class Monad m where
  join :: m (m a) -> m a
```

In the case of the `IO` type constructor this corresponds to sequencing a nested set of I/O computations.
The outer one runs first and the result of it is what to do next, and that is run next.  Completing our example

```haskell
join (fmap output input) :: IO ()
```

Now let us turn our attention back our `Maybe` constructor.  In this case, the `join` functionality would be to turn
a `Maybe (Maybe a)` value, that is, a potentially double wrapped value, into just a `Maybe a` value.  Again, the
underlying value is fully generic, so the only we can generate one is if we are given one.  This means we have to
return `Nothing` unless we happened to get a double `Just` wrapped value.  In which case we can return it

```haskell
instance Monad Maybe where
  join :: Maybe (Maybe a) -> Maybe a
  join (Just (Just x)) = Just x
  join _               = Nothing
```

How does this look for out `List` type constructor.  We have to turn a list of lists into just a single list.  One
obvious solution is to just flatten the list

```haskell
instance Monad List where
  join :: List (List a) -> List a
  join Nil           = Nil
  join (Cons xs xss) = append xs (join xss)
    where
      append (Cons x xs) ys = Cons x (append xs ys)
```

Another, less obvious, solution would be to essentially take the trace of the list of lists.  That is, the first
element from the first, the second element from the second, the third element from the third list, and so on as
long as possible (i.e., truncating at the first missing element)

```haskell
instance Monad List where
  join :: List (List a) -> List a
  join xss = go 0 0 xss
    where
      go i j (Cons (Cons x xs) xss) | i == j    = Cons x (go 0 (j+1) xss)
                                    | otherwise = go (i+1) j (Cons xs xss)
      go _ _ _                                  = Nil
```

The monad pattern, as we have seen with the other patterns, also requires the `join` functionality to operate
properly with the other existing functionality.  We again require no suprises.  Introducing a layer inside another
and then immediately squashing it should have no effect

```haskell
join (pure mx) = mx
```

As before, this requirement is both important for us to reason about monads and for the compiler to be able to
optimize the monad expressions that it generates.

It is also the case monads are not usually formulated in terms of `join`, but rather a `bind` operation.  This
relates back to the `IO` type constructor were generally we want to take the result of the previous computation
and feed it directly into the next computation, that is, do a combined `fmap` and `join` operation

```haskell
bind :: m a -> (a -> m b) -> m b
bind ma amb = join (fmap amb ma)
```

Given `bind` it is equally easy to define `join`, which shows the two are equivalent

```haskell
join :: m (m a)
join mma = bind mma id
```

where `id` is the identity function.  The key to understanding this is to realize that the compiler will
instantiate the type `a` in the `bind` type signature to `m a'` (`a'` being a different `a`) and `b` to `a'` as
well giving

```haskell
bind :: m (m a') -> (m a' -> m a') -> m a'
```

Haskell also provides two operators for bind `>>=` and `>>`.  The first is just `bind` and the second is
a wrapping of `bind` that throws away the intermediate result

```haskell
(>>) :: m a -> m b -> m b
mx >> my = bind mx xmy
  where
    xmy _ = my
```

With these we can rewrite our earlier example as the more pleasing

```haskell
input :: IO String
input = getLine

output :: String -> IO ()
output name = putStrLn ("hello " ++ name)

computation :: IO ()
computation = input >>= output
```


# IO

As mentioned earlier, the `IO` monad solves the problem of sequencing I/O operations in Haskell.  It does this by
making it explicit behind the scene that an I/O operations is actually a function on the state of the computer

```haskell
data IO a = RealWorld -> (a, RealWorld)

getLine :: IO String = RealWorld -> (String, RealWorld)
putStrLn :: String -> IO () = String -> RealWorld -> ((), RealWorld)
```

The programmer then has to be explicit about the required ordering of the operations of their program through the
way they thread `RealWorld`.  The induced `RealWorld` data dependencies also force even a lazy language to evaluate
the operations in the given order.

The monad design pattern encapsulates the plumbing of threading the state through these stateful operations. The
pointed instances attaches a given value to the state.  This allows standard (non-`IO`) values to be used in `IO`
contexts.

```haskell
instance Pointed IO where
  pure :: a -> IO a = a -> RealWorld -> (a, RealWorld)
  pure x s = (x,s)
```

The functor instance arranges for a function to be applied to the result of a stateful operation.  This allows
standard functions (non-`IO`) to be used in `IO` contexts.

```haskell
instance Functor IO where
  fmap :: (a -> b) -> IO a -> IO b = (a -> b) -> (RealWorld -> (a, RealWorld)) -> RealWorld -> (b, RealWorld)
  fmap ab sas s1 = (b,s2)  -- Return b along with the new state
    where
      (a,s2) = sas s1         -- Use input state to get a and the new state
      b = ab a                -- Use function to b from a
```

The applicative instance combines two stateful operations in order by taking the output state of the first and
feeding it into the second.  This allows stateful operations to be sequenced.

```haskell
instance Applicative IO where
  pair :: IO a -> IO b -> IO (a,b) -- (RealWorld -> (a, RealWorld)) -> (RealWorld -> (b, RealWorld)) -> RealWorld -> ((a,b), RealWorld)
  pair ia ib s1 = ((a,b), s3)  -- Return a and b along with the final state
    where
      (a,s2) = ia s1             -- Use input state to get a and the next state
      (b,s3) = ib s2             -- Use next state to get b and the final state
```

The monad instance feeds the output state of a computation into the results of the computation itself.  This allows
the next stateful computation to be computed by the current stateful computation.

```haskell
instance Monad IO where
  join :: IO (IO a) -> IO a -- (RealWorld -> (RealWorld -> (a, RealWorld), RealWorld) -> RealWorld -> (a, RealWorld)
  join iia s1 = (a,s3)  -- Return a along with the final state
    where
      (ia,s2) = iia s1    -- Use input state to get inner IO action along with the next state
      ( a,s3) =  ia s2    -- Use the next state to get a and the final state from the retrieved inner IO action
```

Returning to our earlier example we had

```haskell
computation :: IO ()
computation = input >>= output
```

which, after a lot of tedious substition of the prior defintions, gives

```haskell
computation :: IO () -- RealWorld -> ((), RealWorld)
computation s1 = ((),s3)
  where
    (x ,s2) = input s1
    ((),s3) = output x s2
```

By encapsulating this tedious plumbing, the monad design pattern makes our programs both easier to read and write.
