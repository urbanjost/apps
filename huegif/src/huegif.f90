module huegif
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, huegif!"
  end subroutine say_hello
end module huegif
