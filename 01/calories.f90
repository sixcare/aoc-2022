program calories
implicit none
    integer :: cal, i, elf_index, ios, a_want, a_max
    integer, allocatable :: elfs(:), tmp(:), top_elfs(:)


    elf_index = 1
    a_want = 100
    a_max = 5000

    allocate(elfs(a_want))
    allocate(top_elfs(3))

    open (1, file='input', status='old')

    i_loop: do i = 1, a_max
        read (1, '(I10)', iostat=ios) cal

        if (ios < 0) then
            exit i_loop
        endif

        if (cal /= 0) then
            elfs(elf_index) = elfs(elf_index) + cal
        else
            elf_index = elf_index + 1
        endif

        if (elf_index >= a_want) then
            a_want = a_want * 2

            if (a_want > a_max) then
                call exit(1)
            endif

            allocate(tmp(a_want))
            tmp(1:elf_index) = elfs(1:elf_index)

            deallocate(elfs)
            allocate(elfs(a_want))

            elfs(1:elf_index) = tmp(1:elf_index)

            deallocate(tmp)
        endif

    end do i_loop
    close(1)

    !i_loop: do i = 1, 3
    !    
    !end do i_loop

    print *, ''
    print *, maxval(elfs)
    print *, maxloc(elfs)
    deallocate(elfs)


end program calories