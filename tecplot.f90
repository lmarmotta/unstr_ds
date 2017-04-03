subroutine tecplot

    use shared
    implicit none

    integer(kind=4) :: i

    write(*,'(A)') " + Outputing solution."

    open(5,file="output.dat")

    write(5,'(A)') 'TITLE = "Visualization of the volumetric solution"'
    write(5,'(A)') 'VARIABLES = '
    write(5,'(A)') '"x"'
    write(5,'(A)') '"y"'
    write(5,'(A)') '"area"'
    write(5,'(A)') '"rho"'
    write(5,'(A)') '"rhou"'
    write(5,'(A)') '"rhov"'
    write(5,'(A)') '"e"'
    write(5,'(A,I8,A,I8,A)')'ZONE T="VOLUME", N=',npoin,', E=',ncells,', DATAPACKING=BLOCK, ZONETYPE=FEQUADRILATERAL,VARLOCATION=([3-7]=CELLCENTERED)'

    ! x coordinate.
    do i = 1, npoin
       write(5,'(F10.4)') coord(1,i)
    end do

    ! y coordinate.
    do i = 1, npoin
       write(5,'(F10.4)') coord(2,i)
    end do

    ! Area of the cell.
    do i = 1,ncells 
        write(5,'(F10.4)') f_area(i)
    end do

    ! Density.
    do i = 1,ncells 
        write(5,'(F10.4)') q(1,i)
    end do
    
    ! Density * u.
    do i = 1,ncells 
        write(5,'(F10.4)') q(2,i)
    end do

    ! Density * v.
    do i = 1,ncells 
        write(5,'(F10.4)') q(3,i)
    end do

    ! Energy.
    do i = 1,ncells 
        write(5,'(F10.4)') q(4,i)
    end do

    ! Cells connectibity information.
    do i = 1,ncells 
        if (inpoel(4,i) > 0) then
            write(5,'(4I8)') inpoel(1,i),inpoel(2,i),inpoel(3,i),inpoel(4,i)
        else
            write(5,'(4I8)') inpoel(1,i),inpoel(2,i),inpoel(3,i),inpoel(3,i)
        end if
    end do

    close(5)


end subroutine tecplot
