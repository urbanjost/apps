### [fpm-tools](https://github.com/search?q="fpm-tools"%20in:topic%20language:fortran):[funix](https://urbanjost.github.io/funix/funix.1.html)

Build fpm example programs using fpm(1):

```bash
   git clone https://github.com/urbanjost/apps
   cd apps/funix
   fpm build
   fpm run # list programs
   fpm run ls- --help # list a demo program's help
```
Example programs that use fpm(1) packages to emulate the basics of common
Unix and GNU/Linux commands.

The programs were initially written to demonstrate how to use various
fpm(1) packages such as M_system(3f). That being said, some of the
programs provide unique actions and are useful of themselves.

Most of the programs require a POSIX-compatible programming environment.

They have a dash added to their names so they will not clash with the
GNU commands of the same names that they resemble. 

### Currently available program examples:
```text
rmdir-      logname-    hostname-   dirname-    rm-         showumask-  
basename-   yes-        whoami-     colrm-      rename-     ln-         
unlink-     touch-      mv-         mkdir-      shuf-       echo-       
tty-        fmt-        paste-      which-      seq-        tac-        
realpath-   expand-     printenv-   mkfifo-     time-       stat-       
tr-         ls-         kill-       uname-      sleep-      pwd-        
link-       rev-        cmp-        
```
