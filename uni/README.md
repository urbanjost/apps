
[![fpm](https://img.shields.io/badge/fpm-install-blue?logo=fortran)](https://fpm.fortran-lang.org/registry/package/M_unicode)

# uni

The **uni** utility provides simple interfaces to features of the **M\_unicode** module.

The [**M\_unicode**](https://github.com/urbanjost/M_unicode) module provides direct support for UTF-8 encoded files
and data, allowing UTF-8 encoded strings to be processed as easily as
ASCII-7 encoded strings, without depending on the compiler supporting
the optional ISO_10646 extension.

It can 

+ convert UTF-8 files to ASCII-7 with C-style escape sequences
  and vice-versa

+ change case of UTF-8 files

+ display a range of Unicode characters in multiple formats

+ convert boxes defined with the pound character as the appropriate
  box characters

+ identify lines in files that contain non-ASCII characters

+ give line lengths in units of Unicode glyphs instead of just bytes

+ render HTML character entities (ie. "&NAME;" and "&#NNNN;") to UTF-8

## EXAMPLES

### Display Unicode ranges
   basic Greek alphabet
```bash
   uni --start 880 --finish 1023
```
#### output sample
```text
   .
   .
   .
 913 Α \U00000391 U\00000391 &#x391; &#913; char(int(z'391'),kind=ucs4) int(z'391')
 914 Β \U00000392 U\00000392 &#x392; &#914; char(int(z'392'),kind=ucs4) int(z'392')
 915 Γ \U00000393 U\00000393 &#x393; &#915; char(int(z'393'),kind=ucs4) int(z'393')
 916 Δ \U00000394 U\00000394 &#x394; &#916; char(int(z'394'),kind=ucs4) int(z'394')
 917 Ε \U00000395 U\00000395 &#x395; &#917; char(int(z'395'),kind=ucs4) int(z'395')
 918 Ζ \U00000396 U\00000396 &#x396; &#918; char(int(z'396'),kind=ucs4) int(z'396')
 919 Η \U00000397 U\00000397 &#x397; &#919; char(int(z'397'),kind=ucs4) int(z'397')
 920 Θ \U00000398 U\00000398 &#x398; &#920; char(int(z'398'),kind=ucs4) int(z'398')
  .
  .
  .
 1020 ϼ \U000003FC U\000003FC &#x3FC; &#1020; char(int(z'3FC'),kind=ucs4) int(z'3FC')
 1021 Ͻ \U000003FD U\000003FD &#x3FD; &#1021; char(int(z'3FD'),kind=ucs4) int(z'3FD')
 1022 Ͼ \U000003FE U\000003FE &#x3FE; &#1022; char(int(z'3FE'),kind=ucs4) int(z'3FE')
 1023 Ͽ \U000003FF U\000003FF &#x3FF; &#1023; char(int(z'3FF'),kind=ucs4) int(z'3FF')
```
The majority of Unicode box-drawing characters are in the Box
Drawing block, which runs from decimal code points 9472 to 9599,
corresponding to hexadecimal U+2500 to U+257F.
```bash
   uni -S 9472 -F 9599
```
## Locate and Display wide characters

### find any lines with non-ASCII7 characters
```bash
   uni -W The_Crow_and_the_Fox.utf8
```
#### output
```text
       1 [“The Crow and the Fox” by Jean de la Fontaine]
         [\u201CThe Crow and the Fox\u201D by Jean de la Fontaine]
       5 [   Maître Corbeau, sur un arbre perché,]
         [   Ma\xEEtre Corbeau, sur un arbre perch\xE9,]
       7 [   Maître Renard, par l’odeur alléché,]
         [   Ma\xEEtre Renard, par l\u2019odeur all\xE9ch\xE9,]
       8 [   Lui tint à peu près ce langage :]
         [   Lui tint \xE0 peu pr\xE8s ce langage :]
       9 [   «Hé ! bonjour, Monsieur du Corbeau.]
         [   \xABH\xE9 ! bonjour, Monsieur du Corbeau.]
      10 [   Que vous êtes joli ! que vous me semblez beau !]
         [   Que vous \xEAtes joli ! que vous me semblez beau !]
      12 [   Se rapporte à votre plumage,]
         [   Se rapporte \xE0 votre plumage,]
      13 [   Vous êtes le Phénix des hôtes de ces bois.»]
         [   Vous \xEAtes le Ph\xE9nix des h\xF4tes de ces bois.\xBB]
      17 [   Le Renard s’en saisit, et dit : «Mon bon Monsieur,]
         [   Le Renard s\u2019en saisit, et dit : \xABMon bon Monsieur,]
      19 [   Vit aux dépens de celui qui l’écoute :]
         [   Vit aux d\xE9pens de celui qui l\u2019\xE9coute :]
      20 [   Cette leçon vaut bien un fromage, sans doute.»]
         [   Cette le\xE7on vaut bien un fromage, sans doute.\xBB]
      22 [   Jura, mais un peu tard, qu’on ne l’y prendrait plus.]
         [   Jura, mais un peu tard, qu\u2019on ne l\u2019y prendrait plus.]
```
#### Escape-style translation
```bash
# convert a file with wide characters to C-style escape codes
# (that can be used with M_unicode module, C, C++, ...).
uni --escape <<\end_of_data
七転び八起き。
転んでもまた立ち上がる。
くじけずに前を向いて歩いていこう。
end_of_data
```
#### Sample output:
```text
   \u4E03\u8EE2\u3073\u516B\u8D77\u304D\u3002
   \u8EE2\u3093\u3067\u3082\u307E\u305F\u7ACB\u3061\u4E0A\u304C\u308B\u3002
   \u304F\u3058\u3051\u305A\u306B\u524D\u3092\u5411\u3044\u3066\u6B69\u3044\u3066\u3044\u3053\u3046\u3002
```
### Easy fixed-space boxes for terminal windows
```bash
   uni --boxes bold <<\end_of_data
   #################################
   # Warning: proceed with caution #
   #################################
   end_of_data
```
#### Sample output:
```text
   ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
   ┃ Warning: proceed with caution ┃
   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```
