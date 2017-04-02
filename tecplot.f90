subroutine tecplot

    use shared
    implicit none

    integer(kind=4) :: i

    open(5,file="output.dat")

    write(5,'(A)') 'TITLE = "Visualization of the volumetric solution"'
    write(5,'(A)') 'VARIABLES = "x","y"'
    write(5,'(A,I8,A,I8,A)') 'ZONE NODES=,',npoin,', ELEMENTS=',nelem,', DATAPACKING=POINT, ZONETYPE=FEQUADRILATERAL'

    do i = 1, npoin
       write(5,'(2F10.4)') coord(2,i), coord(1,i)
    end do

    do i = 1, nelem
        if (inpoel(4,i) > 0) then
            write(5,'(4I8)') inpoel(1,i),inpoel(2,i),inpoel(3,i),inpoel(4,i)
        else
            write(5,'(4I8)') inpoel(1,i),inpoel(2,i),inpoel(3,i),inpoel(3,i)
        end if
    end do

    close(5)

end subroutine tecplot


