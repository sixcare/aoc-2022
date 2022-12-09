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
    character(len=1), dimension(52) :: alphabet=(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',&
                                                  'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',&
                                                  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',&
                                                  'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'])
    character(len=1) :: badge
    character(len=64) :: item1, item2, item3
    integer :: i, j, k, l, m, len, result, item1_len, item2_len, item3_len

    arr = array_from_file('input', '(A)')
    len = size(arr) - 1

    result = 0

    i_loop: do i = 1, len
        if (modulo(i,3) /= 0) cycle
            continue
        badge = ''
        item1 = arr(i-2)
        item1_len = len_trim(item1)
        item2 = arr(i-1)
        item2_len = len_trim(item2)
        item3 = arr(i)
        item3_len = len_trim(item3)
        j_loop: do j = 1, item1_len
            k_loop: do k = 1, item2_len
                if (item1(j:j) == item2(k:k)) then
                    l_loop: do l = 1, item3_len
                        if (item3(l:l) == item2(k:k)) then
                            badge = item3(l:l)
                            exit j_loop
                        endif
                    end do l_loop
                endif
            end do k_loop
        end do j_loop

        m_loop: do m = 1, 52
            if (badge == alphabet(m)) then
                result = result + m
            endif
        end do m_loop
    end do i_loop

    print *, result

    stop
end program rucksack_cleanup