module txt2man
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, txt2man!"
  end subroutine say_hello
end module txt2man
