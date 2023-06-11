####################################################################################################################################
ChatGPT

The provided script is written in Gawk. Here's the equivalent code in Fortran:

fortran

program txt2man
    implicit none
    character(len=1000) :: line
    character(len=1000), dimension(2) :: avar
    character(len=1000), dimension(1000) :: tt
    character(len=1000) :: mark
    character(len=1000), dimension(1000) :: subwords
    character(len=1000) :: head, btxt, itxt
    character(len=1000) :: section, tag, b, a, w
    integer :: k, i, eoff
    integer :: line_start_index, previous_line_start_index, previous_non_zero_line_start_index, indent_level
    integer, dimension(1000) :: indent_offset_table
    logical :: FALSE, TRUE, in_bd, prevblankline

    ! Print header
    head = "####################################################################################################################################"
    print *, head

    avar(1) = btxt
    avar(2) = itxt

    ! Split list of words to make bold or italic and build dictionary
    ! of the words and the string to replace them with
    do k = 1, 2
        mark = ""
        if (k == 1) then
            mark = "\\fB"
        else
            mark = "\\fI"
        end if

        call split_string(avar(k), tt, "�")

        do i = 1, size(tt)
            if (trim(tt(i)) /= "") then
                subwords(trim(tt(i))) = mark // trim(tt(i)) // "\\fP"
            end if
        end do

        tt = ""
    end do

    avar = ""

    FALSE = .false.
    TRUE = .true.
    in_bd = FALSE

    do
        read(*, '(A)') line

        ! To avoid some side effects in regexp
        ! Replace ellipses (...) with escaped string (\.\.\.)
        line = change(line, "...", "\.\.\.")

        ! Remove spaces in empty lines
        line = trim(line)

        if (line == "") then
            ! Skip empty lines
            cycle
        else if (line(1:1) == "*" .or. line(1:1) == "-" .or. line(1:1) == "o" .or. line(1:1) == "+") then
            ! bullet list item
            call adjust_indent()
            print *, ".IP \\(bu 3"
            prevblankline = .false.
            line = line(2:)

        else if (line(1:1) >= "0" .and. line(1:1) <= "9" .and. index(line, ".") /= 0) then
            ! enum list item
            call adjust_indent()
            print *, ".IP " // trim(line) // " 4"
            prevblankline = .false.
            line = line(2:)

        else if (index(line, "  ") /= 0) then
            ! tag list item
            call adjust_indent()
            tag = trim(line(:index(line, "  ")))
            line = trim(line(index(line, "  ") + 1:))

            if (index(line, "  ") /= 0) then
                line = trim(line)
                print *, ".TP\n.B " // tag
                prevblankline = .false.
                if (len_trim(line) == 0) then
                    cycle
                end if
            end if
        else if.


