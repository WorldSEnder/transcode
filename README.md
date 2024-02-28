# transcode

This project is a proof-of-concept implementation of an approach to decoding and encoding. The main
goals are

- [x] a large part of user-written code should be ambivalent towards the phase, i.e. written without duplication
  where one portion is responsible for encoding and another portion is responsible for decoding.
- [x] Algebraic data types can be transcoded by composing the codes of their fields. This currently lacks
  machinery for deriving boilerplate things, that can look scary, but it should be straight-forward to add.
  For a tour of the managery, check out `Data.Transcoder.Maybe`.
- [x] primitive values have an encoding defined that can be used out of the box
- [ ] Macro and splicing for generating boilerplate for ADTs and possibly GADTS.


### Encoding rethought

The idea behind the project took some time to develop into the concrete form you see today. In the course of
development, I had to rethink how I think about encoding schemes and the responsibilites of a code. To sum up
my conclusion in one sentence would probably be

> A code for a value on a stream of bits defines what properties of the value are encoded.

During the process of encoding a value, we know these properties by inspecting the value directly. During decoding,
even though we might not immediately recover the value, we still discover the same properties from the incoming
stream. Most commonly, a good code for a value finishes with the fact that the combinations of properties that are
stored are sufficient to uniquely recover the value.

The central datatype is that of a `Transcoder phase a m n r`. `a` identifies the datatype of the code, `m` and `n`
are two monads in which encoding and decoding happens. `r` is the evidence of the properties we have written to band.
In a sense, Haskell is not quite powerful enough to capture the fact that `r` captures information about the `a`.
The key point is that most of the user written code works with the evidence of the written properties, and has
no direct access to the data itself. The `phase` argument can be specialized to drop down to either encoding
or decoding phase, but this should be avoided to not duplicate logic.

When you write an encoding/decoding scheme, you want to construct a `Transcoder phase a m n (TheUnique a)`. Here,
we have substituted `r` with a specific type from the library: `TheUnique a`. That is, you want to "proof" that you
have written sufficient data to fully identify the value. The type synonym `Code phase m n a` is short-hand for
the longer `Transcoder` type.

`Transcoder phase a m n` is a monad. This gives you a simple syntax to construct your "proof-carrying" code. A
sketch for an implementation that writes a list would be as follows

```haskell
-- Combinators from Data.Transcode.List
-- | a value of this type is evidence that the list some specific length
type HasLength a 
-- | this encoder writes the length of the list to the output
codeListLen :: Transcoder phase [a] m n (HasLength a)
forEachEv :: HasLength a
          -- ^ if we have evidence that the list has a specific length
          -> Code phase m n a
          -- ^ we can transcode each element of the list
          -> Code phase m n [a]
-- with this, the user code becomes
codeList :: Code phase m n a -> Code phase m n [a]
codeList elementCode = do
  evLen <- codeListLen
  forEachEv evLen elementCode
```

The take-away really should be this:  For the clarity of the code, it should be clearly reasoned with proof-carrying
source code that you can fully recover your written values. To end up with a compositional model of this reasoning,
the only place that should have different code paths for encoding and decoding should be found where these properties
originate, i.e. when writing a primitive value and identifying a datatype tag, but not when these properties are
later combined.
