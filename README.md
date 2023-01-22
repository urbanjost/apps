### [fpm-tools](https://github.com/search?q="fpm-tools"%20in:topic%20language:fortran):[numdiff](https://urbanjost.github.io/numdiff/numdiff.1.html)

Build numdiff(1) using fpm(1):
```bash
git clone https://github.com/urbanjost/numdiff
cd numdiff
fpm install
numdiff --help
```

## Name
   numdiff(1f) - [DEVELOPER] Compare numeric differences in a file (LICENSE:PD)

## Synopsis
```text
   numdiff
   -old FILENAME -new FILENAME
   [ -percent REAL_VALUE|-digits N|-margin XXX.XX]
   [ -verbose]|
   [ --help|--version]
```

## Description
   NUMDIFF assumes two files are basically identical except for
   numeric differences, and finds values whose differences exceed
   a specified tolerance.

        file widths are required to be less than 264 characters.

        numbers are assumed to be delimited by spaces, commas,
        semi-colons, or vertical line characters (" ,;|").
        Adjacent delimiters are ignored.

   This program was originally written to simplify the comparison
   of values generated by new versions of numeric libraries to
   previous versions of the libraries. In particular, when I library
   is compiled with a new compiler or on a new platform it can help
   look for unexpected changes in answers.

## Options
   The options are

       -old       FILENAME name of file containing template values

       -new       FILENAME name of file containing new values

       -percent   REAL_VALUE set threshold at which to report values as
                  a percentage of the template value

       -digits    N set threshold at which to report values as a number of
                  digits if -digits is specified -percent is ignored.

       -margin XXX.XX   set threshold to a relative margin of the
                        magnitude of the values

       -verbose   shows the lines that pass the criteria from OLDFILE
                  as well.

       --help     display this help text and exit

       --version  display information on the code version and exit

## Usage
       1. GENERATE TEMPLATE: call all your numeric procedures over
          their allowed ranges and print the values to a file on your
          original system.  Save this file as your master QA template.

       2. GENERATE TRIAL DATA: When you port the procedures to another
          system or recompile using a new compiler run your QA
          program again and save the second file.

       3. TEST/COMPARE: run the numdiff(1) program:
```text
   numdiff -old MASTER_TEMPLATE_FILE -new NEW_OUTPUT_FILE -percent 0.0001
```

## Example
   We will assume we have two files meeting the above criteria called
   "cray_results.txt" and "cygwin_results.txt". To compare the values
   we enter

      numdiff -old cray_results.txt -new cygwin_results.txt -percent 0.00001

   A diff(1) of the following input files would show every line,
   as one uses the "E" prefix for exponents, while the other uses
   "D". Even when a diff(1) would show few lines, you have to
   inspect each difference to see how large a difference in value
   was found. Using numdiff(1) you can ignore most insignificant
   differences.

### Output
```text
       Results from the run are

  >        | *numdiff* numerical differences
  >        | old file=in1
  >        | new file=in2
  >        | percent threshold =  0.1E-006 (%)
  >--------|------------------------------------------------------------------------------------------------------------
  >old     |    0.32000000E+02    0.88589141E-01   -0.17863993E-01   -0.36879559E-04    0.16022057E-01    0.31990327E+02
  >       8|                                       ###############   ###############
  >new     |    0.32000000D+02    0.88589141D-01   -0.17863994D-01   -0.36879562D-04    0.16022057D-01    0.31990327D+02
  >>>>>>>>>|maximum difference =.81345875138179356E-005(%)
  >--------|------------------------------------------------------------------------------------------------------------
```

   This indicates values did not pass on line 8. "#" characters underline the values.

              Input file "cray_results.txt"
```text
       1TESTS STARTED.

       1          T,PSL(T),HSL(T),SSL(T),VSL(T),TSLH(H)
       0         PMIN=    0.885891E-01    PMAX=    0.320823E+04    DELP=    0.100000E+03
                 TMIN=    0.320000E+02    TMAX=    0.705470E+03    DELT=    0.200000E+02
       0
           0.32000000E+02    0.88589141E-01   -0.17863993E-01   -0.36879559E-04    0.16022057E-01    0.31990327E+02
           0.40000000E+02    0.12163360E+00    0.80272719E+01    0.16194175E-01    0.16018940E-01    0.40003431E+02
           0.60000000E+02    0.25611401E+00    0.28059533E+02    0.55503370E-01    0.16033165E-01    0.60005984E+02
           0.80000000E+02    0.50682853E+00    0.48036541E+02    0.93222577E-01    0.16071928E-01    0.79998151E+02
           0.10000000E+03    0.94923553E+00    0.67998872E+02    0.12954114E+00    0.16129956E-01    0.99994830E+02
           0.12000000E+03    0.16927366E+01    0.87966229E+02    0.16459153E+00    0.16204270E-01    0.11999684E+03
           0.14000000E+03    0.28891787E+01    0.10794963E+03    0.19847772E+00    0.16293096E-01    0.14000080E+03
           0.16000000E+03    0.47413557E+01    0.12795766E+03    0.23128914E+00    0.16395272E-01    0.16000351E+03
           0.18000000E+03    0.75110274E+01    0.14799946E+03    0.26310734E+00    0.16510005E-01    0.18000351E+03
           0.20000000E+03    0.11526035E+02    0.16808606E+03    0.29400880E+00    0.16636808E-01    0.20000116E+03
           0.22000000E+03    0.17186197E+02    0.18823058E+03    0.32406594E+00    0.16775474E-01    0.21999785E+03
           0.24000000E+03    0.24967794E+02    0.20844802E+03    0.35334719E+00    0.16926045E-01    0.23999513E+03
           0.26000000E+03    0.35426601E+02    0.22875486E+03    0.38191689E+00    0.17088786E-01    0.25999384E+03
           0.28000000E+03    0.49199533E+02    0.24916873E+03    0.40983516E+00    0.17264170E-01    0.27999361E+03
           0.30000000E+03    0.67005075E+02    0.26970823E+03    0.43715817E+00    0.17452875E-01    0.29999280E+03
           0.32000000E+03    0.89642735E+02    0.29039297E+03    0.46393855E+00    0.17655789E-01    0.31998899E+03
           0.34000000E+03    0.11799178E+03    0.31124389E+03    0.49022610E+00    0.17874025E-01    0.33998018E+03
           0.36000000E+03    0.15300954E+03    0.33228370E+03    0.51606862E+00    0.18108958E-01    0.35996652E+03
           0.38000000E+03    0.19572950E+03    0.35353750E+03    0.54151277E+00    0.18362259E-01    0.38000205E+03
           0.40000000E+03    0.24725940E+03    0.37509279E+03    0.56667770E+00    0.18637646E-01    0.40003002E+03
           0.42000000E+03    0.30877960E+03    0.39689616E+03    0.59150311E+00    0.18935141E-01    0.41998032E+03
           0.44000000E+03    0.38154169E+03    0.41902722E+03    0.61609004E+00    0.19258996E-01    0.43998928E+03
           0.46000000E+03    0.46686778E+03    0.44153583E+03    0.64049662E+00    0.19613160E-01    0.46000991E+03
           0.48000000E+03    0.56615075E+03    0.46448172E+03    0.66478705E+00    0.20002609E-01    0.48002037E+03
           0.50000000E+03    0.68085599E+03    0.48793750E+03    0.68903400E+00    0.20433708E-01    0.50001633E+03
           0.52000000E+03    0.81252603E+03    0.51199311E+03    0.71332237E+00    0.20914751E-01    0.52000390E+03
           0.54000000E+03    0.96279001E+03    0.53676266E+03    0.73775486E+00    0.21456790E-01    0.53999326E+03
           0.56000000E+03    0.11333816E+04    0.56239502E+03    0.76246065E+00    0.22074947E-01    0.55999280E+03
           0.58000000E+03    0.13261708E+04    0.58909059E+03    0.78760893E+00    0.22790623E-01    0.58000418E+03
           0.60000000E+03    0.15432192E+04    0.61712901E+03    0.81343122E+00    0.23635444E-01    0.60001858E+03
           0.62000000E+03    0.17868695E+04    0.64691984E+03    0.84026231E+00    0.24659110E-01    0.62001710E+03
           0.64000000E+03    0.20598878E+04    0.67911317E+03    0.86863104E+00    0.25947611E-01    0.63998803E+03
           0.66000000E+03    0.23656783E+04    0.71493433E+03    0.89954128E+00    0.27678770E-01    0.66001396E+03
           0.68000000E+03    0.27085898E+04    0.75845725E+03    0.93651903E+00    0.30369395E-01    0.68000016E+03
           0.70000000E+03    0.30943291E+04    0.82243999E+03    0.99006883E+00    0.36618048E-01    0.70000112E+03
           0.70547000E+03    0.32082348E+04    0.90600741E+03    0.10611600E+01    0.50778529E-01    0.00000000E+00
       0         SUM=  0.6353636479732371E+05
```

   Input file "cygwin_results.txt"

```text
       1TESTS STARTED.

       1          T,PSL(T),HSL(T),SSL(T),VSL(T),TSLH(H)
       0         PMIN=    0.885891E-01    PMAX=    0.320823E+04    DELP=    0.100000E+03
                 TMIN=    0.320000E+02    TMAX=    0.705470E+03    DELT=    0.200000E+02
       0
           0.32000000D+02    0.88589141D-01   -0.17863994D-01   -0.36879562D-04    0.16022057D-01    0.31990327D+02
           0.40000000D+02    0.12163360D+00    0.80272719D+01    0.16194175D-01    0.16018940D-01    0.40003431D+02
           0.60000000D+02    0.25611401D+00    0.28059533D+02    0.55503370D-01    0.16033165D-01    0.60005984D+02
           0.80000000D+02    0.50682853D+00    0.48036541D+02    0.93222577D-01    0.16071928D-01    0.79998151D+02
           0.10000000D+03    0.94923553D+00    0.67998872D+02    0.12954114D+00    0.16129956D-01    0.99994830D+02
           0.12000000D+03    0.16927366D+01    0.87966229D+02    0.16459153D+00    0.16204270D-01    0.11999684D+03
           0.14000000D+03    0.28891787D+01    0.10794963D+03    0.19847772D+00    0.16293096D-01    0.14000080D+03
           0.16000000D+03    0.47413557D+01    0.12795766D+03    0.23128914D+00    0.16395272D-01    0.16000351D+03
           0.18000000D+03    0.75110274D+01    0.14799946D+03    0.26310734D+00    0.16510005D-01    0.18000351D+03
           0.20000000D+03    0.11526035D+02    0.16808606D+03    0.29400880D+00    0.16636808D-01    0.20000116D+03
           0.22000000D+03    0.17186197D+02    0.18823058D+03    0.32406594D+00    0.16775474D-01    0.21999785D+03
           0.24000000D+03    0.24967794D+02    0.20844802D+03    0.35334719D+00    0.16926045D-01    0.23999513D+03
           0.26000000D+03    0.35426601D+02    0.22875486D+03    0.38191689D+00    0.17088786D-01    0.25999384D+03
           0.28000000D+03    0.49199533D+02    0.24916873D+03    0.40983516D+00    0.17264170D-01    0.27999361D+03
           0.30000000D+03    0.67005075D+02    0.26970823D+03    0.43715817D+00    0.17452875D-01    0.29999280D+03
           0.32000000D+03    0.89642735D+02    0.29039297D+03    0.46393855D+00    0.17655789D-01    0.31998899D+03
           0.34000000D+03    0.11799178D+03    0.31124389D+03    0.49022610D+00    0.17874025D-01    0.33998018D+03
           0.36000000D+03    0.15300954D+03    0.33228370D+03    0.51606862D+00    0.18108958D-01    0.35996652D+03
           0.38000000D+03    0.19572950D+03    0.35353750D+03    0.54151277D+00    0.18362259D-01    0.38000205D+03
           0.40000000D+03    0.24725940D+03    0.37509279D+03    0.56667770D+00    0.18637646D-01    0.40003002D+03
           0.42000000D+03    0.30877960D+03    0.39689616D+03    0.59150311D+00    0.18935141D-01    0.41998032D+03
           0.44000000D+03    0.38154169D+03    0.41902722D+03    0.61609004D+00    0.19258996D-01    0.43998928D+03
           0.46000000D+03    0.46686778D+03    0.44153583D+03    0.64049662D+00    0.19613160D-01    0.46000991D+03
           0.48000000D+03    0.56615075D+03    0.46448172D+03    0.66478705D+00    0.20002609D-01    0.48002037D+03
           0.50000000D+03    0.68085599D+03    0.48793750D+03    0.68903400D+00    0.20433708D-01    0.50001633D+03
           0.52000000D+03    0.81252603D+03    0.51199311D+03    0.71332237D+00    0.20914751D-01    0.52000390D+03
           0.54000000D+03    0.96279001D+03    0.53676266D+03    0.73775486D+00    0.21456790D-01    0.53999326D+03
           0.56000000D+03    0.11333816D+04    0.56239502D+03    0.76246065D+00    0.22074947D-01    0.55999280D+03
           0.58000000D+03    0.13261708D+04    0.58909059D+03    0.78760893D+00    0.22790623D-01    0.58000418D+03
           0.60000000D+03    0.15432192D+04    0.61712901D+03    0.81343122D+00    0.23635444D-01    0.60001858D+03
           0.62000000D+03    0.17868695D+04    0.64691984D+03    0.84026231D+00    0.24659110D-01    0.62001710D+03
           0.64000000D+03    0.20598878D+04    0.67911317D+03    0.86863104D+00    0.25947611D-01    0.63998803D+03
           0.66000000D+03    0.23656783D+04    0.71493433D+03    0.89954128D+00    0.27678770D-01    0.66001396D+03
           0.68000000D+03    0.27085898D+04    0.75845725D+03    0.93651903D+00    0.30369395D-01    0.68000016D+03
           0.70000000D+03    0.30943291D+04    0.82243999D+03    0.99006883D+00    0.36618048D-01    0.70000112D+03
           0.70547000D+03    0.32082348D+04    0.90600741D+03    0.10611600D+01    0.50778529D-01    0.00000000D+00
       0         SUM=  0.6353636479675987E+05
```

## Author
       John S. Urban

## License
       Public Domain
