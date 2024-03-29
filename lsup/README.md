### [fpm-tools](https://github.com/apps)

Build lsup(1) using fpm(1):
```bash
git clone https://github.com/urbanjost/apps
cd apps/ls
fpm install
ls --help
```
## NAME
   lsup(1f) - [FUNIX:FILESYSTEM] list permissions of pathname and directories in pathname
   (LICENSE:PD)

## SYNOPSIS
   lsup NAME... |-help|-version]

## DESCRIPTION
   Output the permissions of the specified pathnames. Then recursively
   output each NAME with its last non-slash component and trailing slashes removed.
   if NAME contains no /'s, output '.' (meaning the current directory).

## OPTIONS
   NAME      pathname. Defaults to current directory
   -help     display this help and exit
   -version  output version information and exit

## EXAMPLES
  Sample program executions:

   lsup
    40755 drwxr-xr-x --- 1  JSU      Users    0        2017-10-12T21:46:51 /home/urbanjs/V600/LIBRARY/libGPF/EXE
    40755 drwxr-xr-x --- 1  JSU      None     0        2017-10-08T14:50:41 /home/urbanjs/V600/LIBRARY/libGPF
    40700 drwx------ --- 1  JSU      Users    0        2017-10-12T22:38:14 /home/urbanjs/V600/LIBRARY
    40700 drwx------ --- 1  JSU      None     0        2017-10-12T22:35:00 /home/urbanjs/V600
    40700 drwx------ --- 1  JSU      None     0        2017-09-19T21:53:23 /home/urbanjs
    41777 drwxrwxrwx --S 1  JSU      None     0        2017-03-16T08:23:34 /home

   lsup /etc/hosts /usr/share/man
   100750 -rwxr-x--- --- 1  SYSTEM   SYSTEM   824      2017-07-14T03:58:59 /etc/hosts
    40755 drwxr-xr-x --- 1  JSU      None     0        2017-09-11T03:29:02 /etc

    40755 drwxr-xr-x --- 1  JSU      None     0        2017-09-04T19:40:32 /usr/share/man
    40755 drwxr-xr-x --- 1  JSU      None     0        2017-09-11T03:19:16 /usr/share
    40755 drwxr-xr-x --- 1  JSU      None     0        2017-09-11T03:05:57 /usr

## SEE ALSO
   dirname(1), realpath(1)
## AUTHOR
   John S. Urban
## LICENSE
   Public Domain


