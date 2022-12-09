module utils
    implicit none
contains
function array_from_file(name, mode)

    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: mode
    character(len=64), allocatable :: arr(:), tmp(:)
    character(len=64), allocatable :: array_from_file(:)

    character(len=64) :: line
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
end module utils


program rucksack_cleanup
    use utils
    implicit none
    character(len=64), allocatable :: arr(:)
    character(len=32), allocatable :: rucksack(:)
    character(len=1), allocatable :: priority_arr(:)
    character(len=1), dimension(52) :: alphabet=(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',&
                                                  'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',&
                                                  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',&
                                                  'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'])
    character(len=32) :: item1, item2
    integer :: i, j, k, l, len, result, items_len, item_len

    arr = array_from_file('input', '(A)')
    len = size(arr) - 1

    allocate(rucksack(len*2))

    result = 0

    i_loop: do i = 1, len
        arr(i) = trim(arr(i))
        item_len = len_trim(arr(i))
        rucksack(i*2-1) = trim(arr(i)(1:item_len/2))
        rucksack(i*2) = trim(arr(i)(item_len/2+1:item_len))
    end do i_loop

    deallocate(arr)
    allocate(priority_arr(len))

    i_loop2: do i = 1, len
        item1 = rucksack(i*2-1)
        item2 = rucksack(i*2)
        item_len = len_trim(item1)
        j_loop2: do j = 1, item_len
            k_loop2: do k = 1, item_len
                if (item1(j:j) == item2(k:k)) then
                    priority_arr(i) = item2(k:k)
                    exit j_loop2
                endif
            end do k_loop2
        end do j_loop2
        l_loop3: do l = 1, 52
            if (priority_arr(i) == alphabet(l)) then
                result = result + l
            endif
        end do l_loop3
    end do i_loop2

    print *, result

    stop
end program rucksack_cleanup