module utils
    implicit none
contains
function array_from_file(name, mode)

    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: mode
    character(len=3), allocatable :: arr(:), tmp(:)
    character(len=2), allocatable :: array_from_file(:)

    character(len=3) :: line
    integer :: i, ios, array_len

    array_len = 9000
    allocate(arr(array_len))

    open (1, file=name, status='old')

    i = 1

    i_loop: do
        read (1, mode, iostat=ios) line

        if (ios < 0) then
            exit i_loop
        endif

        if (i >= array_len) then
            array_len = int(array_len * 1.5)
            if (array_len > 1000000) then
                call exit(1)
            endif

            allocate(tmp(array_len))
            tmp(1:i) = arr(1:i)

            deallocate(arr)
            allocate(arr(array_len))

            arr(1:i) = tmp(1:i)

            deallocate(tmp)
        endif

        arr(i) = line(1:1)//line(3:3)

        i = i + 1
    end do i_loop

    allocate(array_from_file(i))
    array_from_file(1:i) = arr(1:i)
    deallocate(arr)

    return
end function array_from_file
end module utils

program sps
    use utils
    implicit none
    character(len=3), allocatable :: arr(:)
    character(len=1) :: p1, p2, win, loose, tie
    integer :: i, j, len, result

    arr = array_from_file('input', '(A)')
    len = size(arr) -1 

    result = 0

    i_loop: do i = 1, len
        p1 = arr(i)(1:1)
        p2 = arr(i)(2:2)

        if (p2 == 'X') then
            result = result + 1
        else if (p2 == 'Y') then
            result = result + 2
        else if (p2 == 'Z') then
            result = result + 3
        endif

        SELECT CASE (p1)
            CASE ('A')
                win = 'Y'
                loose = 'Z'
                tie = 'X'
            CASE ('B')
                win = 'Z'
                loose = 'X'
                tie = 'Y'
            CASE ('C')
                win = 'X'
                loose = 'Y'
                tie = 'Z'
            CASE DEFAULT
                call exit(2)
        END SELECT

        if (p2 == win) then
            result = result + 6
            !print *, 'win'
        else if (p2 == tie) then
            result = result + 3
            !print *, 'tie'
        else 
            !print *, 'loose'
        endif

        !print *, 'round: ', i, 'player 1: ', p1, ' player2: ', p2
        
    end do i_loop
    print *, result
    stop
end program sps

