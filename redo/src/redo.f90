module redo
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, redo!"
  end subroutine say_hello
end module redo
