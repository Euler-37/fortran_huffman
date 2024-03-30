# fortran_huffman
huffman code compression


```fortran
program main
   use huffman_mod
   implicit none
   block
      type(hftree)::hf
      call hf%encode("fpm.txt")
   end block
   block
      type(hftree)::ef
      call ef%decode("fpm.txt.bin")
   end block
end program main

```
``` sh
$ ls -lh
1.5M  fpm.txt
882K  fpm.txt.bin
1.5M  fpm.txt.bin.txt
$ md5sum fpm.txt
ee53d0a83afcbac27808d0da254861f0  fpm.txt
$ md5sum fpm.txt.bin.txt
ee53d0a83afcbac27808d0da254861f0  fpm.txt.bin.txt
```
