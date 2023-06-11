module colors
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, colors!"
  end subroutine say_hello
end module colors
