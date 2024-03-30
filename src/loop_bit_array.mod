
module loop_bit_mod
   implicit none
   type loop_bit
      integer(1)::a(0:63)
      integer::start,end,interval
   contains
      procedure,pass::init     => loop_bit_init
      procedure,pass::append   => loop_bit_append
      procedure,pass::push     => loop_bit_push
      procedure,pass::pop      => loop_bit_pop
      procedure,pass::popleft  => loop_bit_popleft
      procedure,pass::popleft8 => loop_bit_popleft8
   end type loop_bit
contains

   subroutine loop_bit_init(this)
      class(loop_bit),intent(inout)::this
      this%start=0
      this%end=-1
      this%interval=0
   end subroutine loop_bit_init

   subroutine loop_bit_append(this,val)
      class(loop_bit),intent(inout)::this
      integer(1),intent(in)::val
      integer::pos,idx
      this%end=modulo(this%end+1,512)
      pos=this%end/8
      idx=modulo(this%end,8)
      if(val==1_1)then
         this%a(pos)=ibset(this%a(pos),idx)
         this%interval=this%interval+1
      else
         this%a(pos)=ibclr(this%a(pos),idx)
         this%interval=this%interval+1
      end if
   end subroutine loop_bit_append

   subroutine loop_bit_push(this,val)
      class(loop_bit),intent(inout)::this
      integer(1),intent(in)::val(:)
      integer::i
      do i=1,size(val)
         call this%append(val(i))
      end do
   end subroutine loop_bit_push

   integer function  loop_bit_popleft(this)result(val)
      class(loop_bit),intent(inout)::this
      integer::pos,idx
      pos=this%start/8
      idx=modulo(this%start,8)
      val=ibits(this%a(pos), idx, 1)
      this%start=modulo(this%start+1,512)
      this%interval=this%interval-1
   end function loop_bit_popleft 

   subroutine  loop_bit_pop(this)
      class(loop_bit),intent(inout)::this
      this%end=modulo(this%end-1,512)
      this%interval=this%interval-1
   end subroutine loop_bit_pop

   integer(1) function  loop_bit_popleft8(this)result(val)
      class(loop_bit),intent(inout)::this
      integer::i
      !val=0
      !do i=7,0,-1
      !if(this%popleft()==1)then
      !val=ibset(val,i)
      !end if
      !end do
      val=reverse_byte(this%a(this%start/8))
      this%start=modulo(this%start+8,512)
      this%interval=this%interval-8
   end function loop_bit_popleft8

   function reverse_byte(x) result(reversed)
      implicit none
      integer(1), intent(in) :: x
      integer(1) :: reversed
      reversed = iand(shiftl(x, 1), Z'AA') + iand(shiftr(x, 1), Z'55')
      reversed = iand(shiftl(reversed, 2), Z'CC') + iand(shiftr(reversed, 2), Z'33')
      reversed = iand(shiftl(reversed, 4), Z'F0') + iand(shiftr(reversed, 4), Z'0F')
   end function reverse_byte
end module loop_bit_mod
