module Unison.Test.TermPrinter where

import EasyTest
import qualified Data.Text as Text
import Unison.Term
import Unison.TermPrinter
import Unison.Symbol (Symbol)
import Unison.Builtin
import Unison.Parser (Ann(..))
import Unison.Reference
import qualified Unison.Util.PrettyPrint as PP

-- Test the result of the pretty-printer.  Expect the pretty-printer to
-- produce output that differs cosmetically from the original code we parsed.
-- Check also that re-parsing the pretty-printed code gives us the same ABT.
-- (Skip that latter check if rtt is false.)
-- Note that this does not verify the position of the PrettyPrint Break elements.
tc_diff_rtt :: Bool -> String -> String -> Int -> Test ()
tc_diff_rtt rtt s expected width =
   let input_term = Unison.Builtin.tm s :: Unison.Term.AnnotatedTerm Symbol Ann
       get_names x _ = case x of
                         Builtin t -> t
                         Derived _ -> Text.empty
       actual = if width == 0
                then PP.renderUnbroken $ pretty get_names (-1) input_term
                else PP.renderBroken width True '\n' $ pretty get_names (-1) input_term
       actual_reparsed = Unison.Builtin.tm actual
   in scope s $ tests [(
       if actual == expected then ok
       else do note $ "expected: " ++ show expected
               note $ "actual  : "   ++ show actual
               note $ "show(input)  : "   ++ show input_term
               crash "actual != expected"
       ), (
       if (not rtt) || (input_term == actual_reparsed) then ok
       else do note $ "round trip test..."
               note $ "single parse: " ++ show input_term
               note $ "double parse: " ++ show actual_reparsed
               crash "single parse != double parse"
       )]

-- As above, but do the round-trip test unconditionally.
tc_diff s expected = tc_diff_rtt True s expected 0

-- As above, but expect not even cosmetic differences between the input string
-- and the pretty-printed version.
tc s = tc_diff s s

-- Use renderBroken to render the output to some maximum width.
tc_breaks s width expected = tc_diff_rtt True s expected width

test :: Test ()
test = scope "termprinter" . tests $
  [ tc "if true then +2 else -2"
  , tc "[2, 3, 4]"
  , tc "and true false"
  , tc "or false false"
  , tc "if _something then _foo else _"
  , tc "3.14159"
  , tc "\"some text\""
  , tc "2 : UInt64"
  , tc "x -> and x false"
  , tc "x y -> and x y"
  , pending $ tc_diff "()" $ "()#0"  -- todo
  , tc "Pair"
  , tc "foo"
  , pending $ tc_diff "Sequence.empty" $  "Sequence.empty : [a]"  -- TODO whatever is adding the annotations
         -- is adding a second one on the reparse.  Also it's showing 'Sequence a' not '[a]'
  , tc "None"
  , pending $ tc_diff "Optional.None" $ "Optional#0"
  , tc "handle foo in bar"
  , tc "Pair 1 1"
  , tc "let\n\
       \  x = 1\n\
       \  x\n"
  , pending $ tc "case x of Pair t 0 -> foo t" -- hitting UnknownDataConstructor when parsing pattern
  , pending $ tc "case x of Pair t 0 | pred t -> foo t" -- ditto
  , pending $ tc "case x of Pair t 0 | pred t -> foo t; Pair t 0 -> foo' t; Pair t u -> bar;" -- ditto
  , tc "case x of () -> foo"
  , tc "case x of _ -> foo"
  , tc "case x of y -> y"
  , tc "case x of 1 -> foo"
  , tc "case x of +1 -> foo"
  , tc "case x of -1 -> foo"
  , tc "case x of 3.14159 -> foo"
  , tc "case x of true -> foo"
  , tc "case x of false -> foo"
  , tc "case x of y@(()) -> y"           -- TODO lose the brackets for `As (unary constructor)`
  , tc "case x of a@(b@(c@(()))) -> c"
  , tc "case e of { a } -> z"
  --, tc "case e of { () -> k } -> z" -- doesn't parse since 'many leaf' expected before the "-> k"
                                      -- need an actual effect constructor to test this with
  , pending $ tc "if a then (if b then c else d) else e"
  , pending $ tc "(if b then c else d)"   -- TODO raise issue - parser doesn't like bracketed ifs (`unexpected )`)
  , tc "handle Pair 1 1 in bar"
  , tc "handle x -> foo in bar"
  , tc "let\n\
       \  x = (1 : Int)\n\
       \  (x : Int)\n"
  , tc "case x of 12 -> (y : Int)"
  , tc "if a then (b : Int) else (c : Int)"
  , tc "case x of 12 -> if a then b else c"
  , tc "case x of 12 -> x -> f x"
  , tc "if c then x -> f x else x -> g x"
  , tc "(f x) : Int"
  , tc "(f x) : Pair Int Int"
  , tc "let\n\
       \  x = if a then b else c\n\
       \  if x then y else z\n"
  , tc "f x y"
  , tc "f (g x) y"
  , tc_diff "(f x) y" $ "f x y"
  , pending $ tc "1.0e-19"         -- TODO, raise issue, parser throws UnknownLexeme
  , pending $ tc "-1.0e19"         -- ditto
  , tc "0.0"
  , tc "-0.0"
  , pending $ tc_diff "+0.0" $ "0.0"  -- parser throws "Prelude.read: no parse" - should it?

  , pending $ tc_breaks "case x of 12 -> if a then b else c" 21 $
              "case x of 12 -> \n\
              \  if a then b else c"
  ]
