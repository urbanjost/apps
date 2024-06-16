### [fpm-tools](https://github.com/search?q="fpm-tools"%20in:topic%20language:fortran):[playground](https://urbanjost.github.io/playground/playground.1.html)

Build playground(1) using fpm(1):
```bash
git clone https://github.com/urbanjost/apps
cd apps/playground
fpm install
playground --help
```
## NAME
  playground(1f) - convert Fortran file to an HTML document that uploads the
  code to "Fortran Playground"

## SYNOPSIS
  playground [ --help| --version] *.[fF]90

## DESCRIPTION
  create an HTML document from Fortran source files that includes a click-able
  download to the "Fortran Playground".

  URL encoding, officially known as percent-encoding, is a method to encode
  arbitrary data in a Uniform Resource Identifier (URI) using only the limited
  ASCII characters, replacing them with one or more character triplets that
  consist of the percent character and a two-digit hexadecimal value.

## OPTIONS
  --help
    display this help and exit

  --version
    output version information and exit

  filename(s)
    Fortran source files

## EXAMPLES
  Sample commands
```text
     > $  playground hello.f90 > playground.html
     > <!DOCTYPE html>
     > <html xmlns="http://www.w3.org/1999/xhtml">
     > <head>
     >	 <meta name="generator" content="playground" />
     >	 <title>playground</title>
     > </head>
     > <body>
     >
     >	 <a href="https://play.fortran-lang.org/?code=
     >	 program%20hello%5Fworld
     >	 %0A%20%20%20write%28%2A%2C%2A%29%27Hello%20World%21%27
     >	 %0Aend%20program%20hello%5Fworld
     >	 %0A"
     >	 target="_blank" title="Open in Fortran Playground">
     >	 <img src="https://raw.githubusercontent.com/fortran-lang/
     >	 playground/main/frontend/src/fortran-logo.png"
     >	 alt="Fortran logo" class="align-text-bottom" height="15.5" /> hello
     >	 </a>
     >	 <xmp>
     > program hello_world
     >	  write(*,*)'Hello World!'
     > end program hello_world
     >	 </xmp>
     > </body>
     > </html>
