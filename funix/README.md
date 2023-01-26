### [fpm-tools](https://github.com/search?q="fpm-tools"%20in:topic%20language:fortran):[funix](https://urbanjost.github.io/funix/funix.1.html)

Build fpm example programs using fpm(1):

```bash
   git clone https://github.com/urbanjost/apps
   cd apps/funix
   fpm build
   fpm run # list programs
   fpm run ls- --help # list a demo program's help
```
Example programs that use fpm(1) and fpm(1) packages to 
emulate the basics of common Unix and GNU/Linux commands.

Although some of the programs are useful of themselves, they were
primarily initially written to demonstrate how to use various fpm(1)
packages.

Several of the programs require a POSIX-compatible programming
environment.

### Currently available program examples:
``text
rmdir-      logname-    hostname-   dirname-    rm-         showumask-  
basename-   yes-        whoami-     colrm-      rename-     ln-         
unlink-     touch-      mv-         mkdir-      shuf-       echo-       
tty-        fmt-        paste-      which-      seq-        tac-        
realpath-   expand-     printenv-   mkfifo-     time-       stat-       
tr-         ls-         kill-       uname-      sleep-      pwd-        
link-       rev-        cmp-        
```
