module huffman_mod
   implicit none
   type bits
      integer(1),allocatable::a(:)
   end type bits
   type hftree
      integer::n
      integer(1),allocatable::ch(:) ![0,255] [-128,127]
      integer   ,allocatable::weight(:)
      integer   ,allocatable::parent(:)
      integer   ,allocatable::child(:,:)
      type(bits) ::map(-128:127)
   contains
      generic::write(formatted)    => hftree_write
      generic::read(formatted)     => hftree_read
      generic::write(unformatted)  => hftree_write_binary
      generic::read(unformatted)   => hftree_read_binary
      procedure,pass::creat        => hftree_creat
      procedure,pass::select       => hftree_select
      procedure,pass::code         => hftree_code
      procedure,pass::init         => hftree_init
      procedure,pass::decode       => hftree_decode_file
      procedure,pass::encode       => hftree_encode_file
      procedure,pass::encode_array => hftree_encode_array
      procedure,pass::hftree_write,hftree_write_binary
      procedure,pass::hftree_read,hftree_read_binary
      final::hftree_final
   end type hftree
contains

   subroutine hftree_init(this,n)
      class(hftree),intent(inout)::this
      integer   ,intent(in)::n
      this%n=n
      allocate(this%ch(2*n-1),source=0_1)
      allocate(this%weight(2*n-1),source=0)
      allocate(this%parent(2*n-1),source=-1)
      allocate(this%child(0:1,2*n-1),source=-1)
   end subroutine hftree_init


   subroutine hftree_creat(this,ch,weight)
      class(hftree),intent(inout)::this
      integer(1),intent(in)::ch(:)
      integer   ,intent(in)::weight(:)

      integer::idx1,idx2
      associate(n=>size(ch))
         call this%init(n)
         this%ch(1:n)=ch
         this%weight(1:n)=weight
         block
            integer::i
            do i=n+1,2*n-1
               call this%select(i-1,idx1,idx2)
               this%weight(i)=this%weight(idx1)+this%weight(idx2)
               this%child(0,i)=idx1
               this%child(1,i)=idx2
               this%parent(idx1)=i
               this%parent(idx2)=i
            end do
         end block
      end associate
   end subroutine hftree_creat


   subroutine hftree_select(this,k,idx1,idx2)
      class(hftree),intent(in)::this
      integer,intent(in)::k
      integer,intent(inout)::idx1,idx2
      integer::i
      integer::val1,val2
      val1=huge(1)
      val2=huge(1)
      idx1=0
      idx2=0
      do i=1,k
         if(this%parent(i)==-1)then
            if(this%weight(i)<val2)then
               if(this%weight(i)<val1)then
                  val2=val1          ; idx2=idx1
                  val1=this%weight(i); idx1=i
               else
                  val2=this%weight(i); idx2=i
               end if
            end if
         end if
      end do
   end subroutine hftree_select

   subroutine hftree_write(this,unit,iotype,v_list,iostat,iomsg)
      class(hftree),intent(in)::this
      integer, intent(in) :: unit
      character (len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character (len=*), intent(inout) :: iomsg
      block
         integer::i
         write(*,"(*(g0,:,1x))",iostat=iostat,iomsg=iomsg)this%n
         do i=1,size(this%ch)
            write(*,"(*(g0,:,1x))",iostat=iostat,iomsg=iomsg)i,&
               &this%ch(i),this%weight(i),this%parent(i),this%child(0,i),this%child(1,i),new_line("")
         end do
         do i=1,this%n
            write(*,"(*(g0,:,1x))",iostat=iostat,iomsg=iomsg)this%ch(i),this%map(this%ch(i))%a,new_line("")
         end do
      end block
   end subroutine hftree_write

   subroutine hftree_write_binary(this,unit,iostat,iomsg)
      class(hftree),intent(in)::this
      integer, intent(in) :: unit
      integer, intent(out) :: iostat
      character (len=*), intent(inout) :: iomsg
      block
         integer::i
         write(unit,iostat=iostat,iomsg=iomsg)this%n
         do i=1,2*this%n-1
            write(unit,iostat=iostat,iomsg=iomsg)&
               &this%ch(i),this%weight(i),this%parent(i),this%child(0,i),this%child(1,i)
         end do
      end block
   end subroutine hftree_write_binary

   subroutine hftree_read(this,unit,iotype,v_list,iostat,iomsg)
      class(hftree),intent(inout)::this
      integer, intent(in) :: unit
      character (len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character (len=*), intent(inout) :: iomsg
      block
         integer::i,n
         read(*,*,iostat=iostat,iomsg=iomsg)n
         call this%init(n)
         do i=1,2*n-1
            read(*,*,iostat=iostat,iomsg=iomsg)&
               &this%ch(i),this%weight(i),this%parent(i),this%child(0,i),this%child(1,i)
         end do
      end block
   end subroutine hftree_read


   subroutine hftree_read_binary(this,unit,iostat,iomsg)
      class(hftree),intent(inout)::this
      integer, intent(in) :: unit
      integer, intent(out) :: iostat
      character (len=*), intent(inout) :: iomsg
      block
         integer::i,n
         read(unit,iostat=iostat,iomsg=iomsg)n
         call this%init(n)
         do i=1,2*n-1
            read(unit,iostat=iostat,iomsg=iomsg)&
               &this%ch(i),this%weight(i),this%parent(i),this%child(0,i),this%child(1,i)
         end do
      end block
   end subroutine hftree_read_binary

   subroutine hftree_encode_file(this,file)
      use iso_fortran_env
      class(hftree),intent(inout)::this
      character(len=*),intent(in)::file
      integer,allocatable::weight(:)
      integer(1),allocatable::ch(:)
      integer::nums(-128:127)
      integer::unit,output
      integer(1)::a
      integer::cnt,i,k,ios
      nums=0
      open(newunit=unit,file=file,form="unformatted",access="stream",action="read")
      do
         read(unit,iostat=ios)a
         if(is_iostat_end(ios))exit
         nums(a)=nums(a)+1
      end do
      close(unit)
      cnt=count(nums/=0)
      allocate(ch(cnt))
      allocate(weight(cnt))
      i=0
      do k=-128,127
         if(nums(k)/=0)then
            i=i+1
            ch(i)=k
            weight(i)=nums(k)
         end if
      end do
      call this%creat(ch, weight)
      call this%code()
      open(newunit=unit,file=file,form="unformatted",access="stream",action="read")
      open(newunit=output,file=file//".bin",form="unformatted",access="stream")
      write(output)this
      block
         use loop_bit_mod
         type(loop_bit)::la
         integer(1)::tmp
         call la%init()
         do
            read(unit,iostat=ios)a
            if(is_iostat_end(ios))exit
            associate(p=>this%map(a)%a)
               call la%push(p)
               do 
                  if(la%interval < 8)exit
                  tmp=la%popleft8()
                  write(output)tmp
               end do
            end associate
         end do
         if(la%interval==0)then
            write(output)0_1
         else
            k=la%interval
            tmp=la%popleft8()
            write(output)tmp
            write(output)int(k,1)
         end if
      end block
      close(unit)
      close(output)
   end subroutine hftree_encode_file

   subroutine hftree_encode_array(this,file,array,size)
      use iso_fortran_env
      use iso_c_binding
      class(hftree),intent(inout)::this
      character(len=*),intent(in)::file
      type(*),intent(in),target::array(*)
      integer,intent(in)::size
      integer(1),pointer::ptr(:)
      integer,allocatable::weight(:)
      integer(1),allocatable::ch(:)
      integer::nums(-128:127)
      integer::unit,output
      integer(1)::a
      integer::cnt,i,k,ios
      call c_f_pointer(c_loc(array), ptr, [size])
      nums=0
      do i=1,size
         nums(ptr(i))=nums(ptr(i))+1
      end do
      cnt=count(nums/=0)
      allocate(ch(cnt))
      allocate(weight(cnt))
      i=0
      do k=-128,127
         if(nums(k)/=0)then
            i=i+1
            ch(i)=k
            weight(i)=nums(k)
         end if
      end do
      call this%creat(ch, weight)
      call this%code()
      open(newunit=output,file=file//".bin",form="unformatted",access="stream")
      write(output)this
      block
         use loop_bit_mod
         type(loop_bit)::la
         integer(1)::tmp
         call la%init()
         do i=1,size
            if(is_iostat_end(ios))exit
            associate(p=>this%map(ptr(i))%a)
               call la%push(p)
               do 
                  if(la%interval < 8)exit
                  tmp=la%popleft8()
                  write(output)tmp
               end do
            end associate
         end do
         if(la%interval==0)then
            write(output)0_1
         else
            k=la%interval
            tmp=la%popleft8()
            write(output)tmp
            write(output)int(k,1)
         end if
      end block
      close(output)
   end subroutine hftree_encode_array

   subroutine hftree_code(this)
      class(hftree),intent(inout)::this

      integer(1),allocatable::buffer(:)
      integer::start,f
      integer::n,c,i
      n=this%n
      allocate(buffer(n))
      do i=1,n
         c=i
         start=n+1
         do
            f=this%parent(c)
            if(f==-1)exit
            start=start-1
            buffer(start)=merge(0_1,1_1,this%child(0,f)==c)
            c=f
         end do
         this%map(this%ch(i))%a=buffer(start:)
      end do
   end subroutine hftree_code

   subroutine hftree_decode_file(this,file)
      use loop_bit_mod
      class(hftree),intent(inout)::this
      character(len=*),intent(in)::file
      integer::start,f
      integer::n,c,i,k
      integer(1)::a
      integer(1)::tmp(8)
      integer::input,ios,output
      type(loop_bit)::la
      open(newunit=input,file=file,form="unformatted",access="stream",action="read")
      open(newunit=output,file=file//".txt",form="unformatted",access="stream")
      read(input)this
      n=this%n
      f=2*n-1
      i=0
      call la%init()
      do
         i=i+1
         read(input,iostat=ios)a
         if(is_iostat_end(ios)) then
            do i=1,8
               call la%pop()
            end do
            if(a/=0)then
               do i=a+1,8
                  call la%pop()
               end do
            end if
            do
               if(la%interval==0)exit
               k=la%popleft()
               c=this%child(k,f)
               if(this%child(0,c)==-1)then
                  write(output)this%ch(c)
                  c=2*n-1
               end if
               f=c
            end do
            exit
         end if
         tmp=ibits(a,[(k,k=7,0,-1)],1)
         call la%push(tmp)
         do
            if(la%interval<=16)exit
            k=la%popleft()
            c=this%child(k,f)
            if(this%child(0,c)==-1)then
               write(output)this%ch(c)
               c=2*n-1
            end if
            f=c
         end do 
      end do
      close(input)
      close(output)
   end subroutine hftree_decode_file

   subroutine hftree_final(this)
      type(hftree),intent(inout)::this
      deallocate(this%ch)
      deallocate(this%weight)
      deallocate(this%parent)
      deallocate(this%child)
   end subroutine hftree_final

end module huffman_mod
