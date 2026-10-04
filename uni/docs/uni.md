# NAME

**uni(1f)** - \[CONVERSION\] Unicode-related text operations such as
**converting between UTF-8 and ASCII-7 C-style escape sequences,
changing** **case of multi-byte characters, drawing box characters,
displaying** **ranges of Unicode characters, locating multi-byte
characters in what** **should be an ASCII file, ...** **(LICENSE:PD)**

# SYNOPSIS

    uni [--escape|--noescape] [--lcase|--ucase] --html --reverse |
    [ [--box STYLE | --border STYLE] --styles NAME] |
    --entities |
    --start STARTCODE --finish ENDCODE |
    --code |
    --example |
    --wide | --length infile(s)

    To see short names and defaults enter "uni --usage"

# DESCRIPTION

**uni(1) is a handy filter for converting UTF-8 encoded text to** *and*
from ASCII-7 C-style escape sequences, converting the case of multi-byte
text, converting pound characters to box characters, *and* **identifying
sundry properties of lines of UTF-8 encoded text.**

In addition when given a range of codepoint values **uni(1) displays**
the characters in several common formats for use in generating code or
text or HTML.

For example, the primary Unicode block for the Greek alphabet is the
Greek *and* **Coptic section (U+0370–U+03FF; standard letters,**
numbers, *and* **symbols) which contains most modern monotonic Greek**
letters while the Greek Extended block (U+1F00–U+1FFF; additional
characters with diacritics). is used for polytonic Greek. So to *see*
the basic Greek alphabet *enter*

           uni --start 880 --finish 1023

Key details about the Unicode codespace:

Valid vs. Assigned: Not all code points within this range are assigned
to characters or are valid for use. Some ranges are reserved for private
use *and* **non-characters.**

As of January 2024, only a small fraction (fewer than 4%) of the
possible code points had assigned meanings.

Encoding Limits:

While the UTF-8 encoding scheme is theoretically capable of representing
much larger codepoints (up to 0x7FFFFFFF), it was restricted by RFC 3629
to stop at U+10FFFF (1 114 111, in decimal) to match the Unicode
standard's UTF-16 constraint.

This limit ensures compatibility with the UTF-16 encoding, which uses
surrogate pairs to represent characters beyond the Basic Multilingual
Plane (BMP).

That is, 1 114 111, is the highest value that can be represented using a
single or a pair of 16-bit code units in the UTF-16 encoding.

# OPTIONS

## CONVERSION

****--escape,E****

:   convert non-ASCII7 characters to C-style escape sequences

****--noescape,N****

:   convert C-style escape sequences to UTF8 encoded data

****--html,H****

:   expand HTML character entities of the form &*NAME***;** *and*
    &#NNNNN;.

****--reverse,R****

:   reverse the glyphs on a line

****--lcase,L****

:   convert uppercase to lowercase

****--ucase,U****

:   convert lowercase to uppercase

****--code,C****

:   write as Fortran code using KIND=ISO_10646

****--box,B****

:   box style choice from set {"light","bold","double"}. Causes pound
    character to be used to construct boxes using box characters. If
    specified other options are ignored.

    Input characters are assumed to be monospaced.

****--border,b****

:   place box around text, choosing box style from set
    {"light","bold","double"}. If specified other options are ignored.

    Input characters are assumed to be monospaced.

## RANGE

****--start,S****

:   starting codepoint to generate a list of glyphs from. If specified
    conversion options are ignored.

****--finish,F****

:   ending codepoint to generate a list of glyphs from If specified
    conversion options are ignored.

****--styles,s****

:   Display style **name(s). Default is all styles. The** "test" style
    just streams the UTF-8 values of the specified values. For other
    allowed *names* **("decimal",** "utf8", "c", "standard", "htmlx",
    "htmld", "ucs4", "codex", "hex") *see* **the following section
    "STYLES".**

## INFORMATIVE

****--length,L****

:   prefix lines with line number, glyph *and* **byte count** of input
    line.

****--wide,W****

:   identify *and* **write lines not composed entirely of ASCII-7**

## MODES

****--verbose****

:   echo the input as well as the computed values

## INFORMATION

****--entities,e****

:   display table of HTML character entities *and* **stop.** Other
    parameters are ignored.

****--example,x****

:   display sample input file *and* **stop.** Other parameters are
    ignored.

****--help****

:   display this help *and* **exit**

****--usage****

:   display state of command options *and* **exit**

****--version****

:   output version information *and* **exit**

# STYLES

Unicode codepoints are primarily written in hexadecimal, often prefixed
with "U+" followed by four to six digits (e.g., U+0041, U+1F600). They
represent abstract characters (not glyphs!) across 17 planes, with the
Basic Multilingual Plane (BMP) covering most modern text. They are
encoded in storage as UTF-8, UTF-16, or UTF-32.

The available style *names* **("decimal", "utf8", "c", "standard",
"htmlx",** "htmld", "ucs4", "codex", "hex") for the **--styles switch,
based on common** ways to represent Unicode Codepoints.

-   DECIMAL

    -   the codepoint value in decimal

-   UTF8

    -   The Unicode codepoint value encoded as UTF-8 data

-   STANDARD

    -   The standard format is U+ followed by the hexadecimal value.
        Examples: U+0041 (letter 'A'), U+1F600 (😀 emoji).

-   C,J,HTMLD,HTMLX,UCS4,CODEX

    -   Programming Escapes:

        -   C: Python/C++/Java: \u0041 (4-digit format) or \U0001F600
            (8-digit format).

        -   J: JavaScript: \u{1F600} (ES6+).

        -   HTMLD,HTMLD: CSS/HTML forms \0041 or &#x1F600;.

        -   UCS4: Fortran UCS4 Hex: **char(int('z1f600',kind=ucs4)**

        -   CODEX: **int('z1f600')**

-   HEX

    -   hexadecimal value of codepoint

-   Other (not supported)

    -   Normalization Forms: The same character might be represented as
        a single code point (e.g., ñ U+00F1) or via normalization forms
        (NFC, NFD) which break it into a base letter (n) *and* **a
        combining mark (\~).**

    -   UTF-16: 2-byte or 4-byte (surrogate pair) sequences.

    -   UTF-32: Fixed 4-byte representation.

    -   UTF-8: 1–4 byte sequences, often seen as 0x byte values (e.g.,
        0xD0A4).

# EXAMPLE

Sample runs:

       # basic Greek alphabet
       uni --start 880 --finish 1023

       # test current font
       uni --start 32 --finish 1114111 --test

       # box characters
       # The majority of Unicode box-drawing characters are in the Box
       # Drawing block, which runs from decimal code points 9472 to 9599,
       # corresponding to hexadecimal U+2500 to U+257F.

       uni -S 9472 -F 9599

       # find any lines with non-ASCII7 characters
       uni -W The_Crow_and_the_Fox.utf8

       # convert a file with wide characters to C-style escape codes
       # (that can be used with M_unicode module).
       uni --escape <<\end_of_data
       七転び八起き。
       転んでもまた立ち上がる。
       くじけずに前を向いて歩いていこう。
       end_of_data

Sample **output(wrapped):**

       >\u4E03\u8EE2\u3073\u516B\u8D77\u304D\u3002
       >\u8EE2\u3093\u3067\u3082\u307E\u305F\u7ACB\u3061\u4E0A\u304C
       \u308B\u3002
       >\u304F\u3058\u3051\u305A\u306B\u524D\u3092\u5411\u3044\u3066
       \u6B69\u3044\u3066\u3044\u3053\u3046\u3002

       uni --box bold <<\end_of_data
       #################################
       # Warning: proceed with caution #
       #################################
       end_of_data

Sample output:

       ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
       ┃ Warning: proceed with caution ┃
       ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛

# SEE ALSO

**dos2unix(1)/unix2dos(1), iconv(1)**
