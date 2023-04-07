module minipre
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, minipre!"
  end subroutine say_hello
end module minipre
