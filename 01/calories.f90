program calories
implicit none
    integer :: elf
    integer :: cal
    integer :: i, u, s, ios

    open (1, file = 'input', status = 'old', iostat=ios)
    do i = 1,5
        read (1, *) cal
        print *, i, cal
    end do

    close(1)

end program calories