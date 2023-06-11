module kolor
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, kolor!"
  end subroutine say_hello
end module kolor
