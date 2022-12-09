module utils
    implicit none
contains
function array_from_file(name, mode)

    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: mode
    character(len=7), allocatable :: arr(:), tmp(:)
    character(len=4), allocatable :: array_from_file(:)

    character(len=7) :: line
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

        arr(i) = line(1:1)//line(3:3)//line(5:5)//line(7:7)

        i = i + 1
    end do i_loop

    allocate(array_from_file(i))
    array_from_file(1:i) = arr(1:i)
    deallocate(arr)

    return
end function array_from_file
end module utils


program camp_clean
    use utils
    implicit none
    character(len=4), allocatable :: arr(:)
    integer :: i, j, result, len, sec11, sec12, sec21, sec22, longest_start, longest_end, shortes_start, shortes_end
    logical :: start, end

    arr = array_from_file('input-test', '(A)')
    len = size(arr) - 1
    result = 0

    i_loop: do i = i, len
        read(arr(i)(1:1),'(I1)') sec11
        read(arr(i)(2:2),'(I1)') sec12
        read(arr(i)(3:3),'(I1)') sec21
        read(arr(i)(4:4),'(I1)') sec22

        longest_start = 0
        longest_end = 0
        shortes_start = 0
        shortes_end = 0
        start = .false.
        end = .false.
        if (sec12-sec11 > sec22-sec21) then
            longest_start = sec11
            longest_end = sec12
            shortes_start = sec21
            shortes_end = sec22
        else
            longest_start = sec21
            longest_end = sec22
            shortes_start = sec11
            shortes_end = sec12
        endif
        j_loop: do j = longest_start, longest_end
            if (j == shortes_start) then
                start = .true.
            endif
            if (j == shortes_end) then
                end = .true.
            endif
        end do j_loop
        if (start .and. end) then
            result = result+1
            !print *, arr(i), i, sec11, arr(i)(1:1), sec12, arr(i)(2:2), sec21, arr(i)(3:4), sec22, arr(i)(4:4)
        endif
        print *, i
    end do i_loop
    
    print *, 'test'
    print *, result

stop
end program camp_clean
