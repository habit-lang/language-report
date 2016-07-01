#! runhugs

This file contains code for extracting language.tex and prelude.lhb
from the language report sources in language.src.

> import Char
> import System
> import IO

> main = do "language.src" `toLatex` "language.tex"
>           "language.src" `toCode`  "prelude.lhb"

How do we classify lines in the input file?

> data Line = Lit String | Code String | Blank | Text String

Lines beginning with "> " or with ">" followed only by spaces are Literate
lines that will appear in both LaTeX output (without the "> " prefix) and
code (with the ">" prefix).

Any other lines that begin with ">" are Code lines that are ignored in
LaTeX output, but appear in some form in the code.  Specifically, a line
beginning ">>" is treated in code like a line beginning with "> "; a
line beginning with ">:" is copied to the code output without the ">:"
prefix; and any other line is copied to the code output without the
">" prefix.

As such, ">>" and ">:" can be used to create blank lines that appear
only in the code file, the former including a ">" prefix, the latter
without.

> classify                    :: String -> Line
> classify ('>':' ': s)        = Lit s
> classify ('>':s)
>            | all isSpace s   = Lit s
>            | otherwise       = Code s
> classify s | all isSpace s   = Blank
>            | otherwise       = Text s

How do we process a file?

> type Out = String -> IO ()

> process :: (Out -> [Line] -> IO ()) -> String -> String -> IO ()
> process pro inf outf
>      = do src <- readFile inf
>           h   <- openFile outf WriteMode
>           pro (hPutStrLn h) (map classify (lines src))
>           hClose h

What are the specifics for generating a LaTeX file?

> toLatex    :: String -> String -> IO ()
> toLatex     = process latexSM

> latexSM    :: Out -> [Line] -> IO ()
> latexSM out = intext
>  where
>    -- functions for producing output:
>    text s = out s
>    begin  = out "\\begin{Verbatim}"
>    end    = out "\\end{Verbatim}"
>    blank  = out ""
>    lit s  = out s
>
>    -- in regular text:
>    intext []             = return ()
>    intext (Text s : ls)  = text s >> intext ls
>    intext (Blank  : ls)  = textblank ls
>    intext (Lit  s : ls)  = begin >> lit s >> incode ls
>    intext (Code s : ls)  = intext ls
>
>    -- blanks appearing in text:
>    textblank []            = blank
>    textblank (Text s : ls) = blank >> text s >> intext ls
>    textblank (Blank  : ls) = blank >> textblank ls
>    textblank (Lit  s : ls) = begin >> lit s >> incode ls
>    textblank (Code s : ls) = textblank ls
>
>    -- in code lines:
>    incode []            = end
>    incode (Text s : ls) = end >> text s >> intext ls
>    incode (Blank  : ls) = codeblank ls
>    incode (Lit  s : ls) = lit s >> incode ls
>    incode (Code s : ls) = incode ls
>
>    -- blanks appearing in code:
>    codeblank []            = end
>    codeblank (Text s : ls) = end   >> text s >> intext ls
>    codeblank (Blank  : ls) = end   >> blank  >> intext ls
>    codeblank (Lit  s : ls) = blank >> lit s >> incode ls
>    codeblank (Code s : ls) = codeblank ls

The state machine described by the code above ignores Code lines
completely, as if they had been filtered out of the input stream
in the first place.

> toCode     :: String -> String -> IO ()
> toCode      = process codeSM

> codeSM    :: Out -> [Line] -> IO ()
> codeSM out = intext
>  where
>    -- functions for producing output:
>    blank        = out ""
>    lit s        = out ('>' : ' ' : s)
>    code ('>':s) = lit s
>    code (':':s) = out s
>    code other   = out other
>
>    -- in regular text:
>    intext []             = return ()
>    intext (Text s : ls)  = intext ls
>    intext (Blank  : ls)  = textblank ls
>    intext (Lit  s : ls)  = lit s >> incode ls
>    intext (Code s : ls)  = code s >> incode ls
>
>    -- blanks appearing in text:
>    textblank []            = return ()
>    textblank (Text s : ls) = intext ls
>    textblank (Blank  : ls) = textblank ls
>    textblank (Lit  s : ls) = blank >> lit s >> incode ls
>    textblank (Code s : ls) = blank >> code s >> incode ls
>
>    -- in code lines:
>    incode []            = return ()
>    incode (Text s : ls) = blank >> intext ls
>    incode (Blank  : ls) = codeblank ls
>    incode (Lit  s : ls) = lit s >> incode ls
>    incode (Code s : ls) = code s >> incode ls
>
>    -- blanks appearing in code:
>    codeblank []            = return ()
>    codeblank (Text s : ls) = intext ls
>    codeblank (Blank  : ls) = codeblank ls
>    codeblank (Lit  s : ls) = blank >> lit s >> incode ls
>    codeblank (Code s : ls) = blank >> code s >> incode ls

