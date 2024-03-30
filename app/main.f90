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
