module utils
    implicit none
contains
function array_from_file(name, mode)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: mode
    character(len=11), allocatable :: arr(:), tmp(:), array_from_file(:)
    character(len=11) :: line
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

        arr(i) = line

        i = i + 1
    end do i_loop

    allocate(array_from_file(i))
    array_from_file(1:i) = arr(1:i)
    deallocate(arr)

    return
end function array_from_file

function sectors_from_array(arr)
    character(len=11), dimension(:), intent(in)  :: arr ! input
    character(len=11) :: line
    integer, dimension(:, :), allocatable :: sectors_from_array, sector_tmp
    integer :: i, array_len, sector_start, sector_end, pos_dash, pos_com

    array_len = size(arr) -1

    allocate(sector_tmp(array_len,4))
    
    do i = 1, array_len
        line = arr(i)
        pos_com = scan(line, ',')

        pos_dash = scan(line, '-')
        read(line(1:pos_dash-1),*) sector_start
        read(line(pos_dash+1:pos_com-1),*) sector_end
        sector_tmp(i,1) = sector_start
        sector_tmp(i,2) = sector_end

        pos_dash = scan(line, '-', back=.true.)
        read(line(pos_com+1:pos_dash-1),*) sector_start
        read(line(pos_dash+1:len_trim(line)),*) sector_end
        sector_tmp(i,3) = sector_start
        sector_tmp(i,4) = sector_end
    end do

    allocate(sectors_from_array(i-1, 4))
    sectors_from_array(1:i-1,1:4) = sector_tmp(1:i-1,1:4)
    deallocate(sector_tmp)
end function sectors_from_array
end module utils


program camp_clean
    use utils
    implicit none
    character(len=11), allocatable :: arr(:)
    integer, dimension(:, :), allocatable :: sectors
    integer :: i, j, k, result1, result2, len
    integer :: longest_start, longest_end, shortes_start, shortes_end
    logical :: start, end

    arr = array_from_file('input', '(A)')
    len = size(arr) - 1

    sectors = sectors_from_array(arr)

    result1 = 0
    result2 = 0

    i_loop: do i = 1, size(sectors)/4
        longest_start = 0
        longest_end = 0
        shortes_start = 0
        shortes_end = 0
        start = .false.
        end = .false.
        if (sectors(i, 2)-sectors(i, 1) > sectors(i, 4)-sectors(i, 3)) then
            longest_start = sectors(i, 1)
            longest_end = sectors(i, 2)
            shortes_start = sectors(i, 3)
            shortes_end = sectors(i, 4)
        else
            longest_start = sectors(i, 3)
            longest_end = sectors(i, 4)
            shortes_start = sectors(i, 1)
            shortes_end = sectors(i, 2)
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
            result1 = result1+1
        endif
        if (start .or. end) then
            result2 = result2+1
        endif
    end do i_loop

    print *, 'Exercise 1', result1
    print *, 'Exercise 2', result2

stop
end program camp_clean
