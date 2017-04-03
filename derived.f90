subroutine area_tri3(ax,ay, bx,by, cx,cy, area_t)
    ! Calculate the area of triangle or quad with vectorial dark magic.
    !             b     d
    !            /\----/
    !   side_a  /  \ side_b
    !          /____\/
    !        a       c
    !          side_c

    ! The formula used here is heron's formula.
    ! area = ( s(s-side_a)(s-side_b)(s-side_c) )**0.5d0 

    use shared
    implicit none

    ! Input/output.
    real(kind=8) :: ax,ay, bx,by, cx,cy, area_t

    ! Internal.
    real(kind=8) :: side_a, side_b, side_c, s

    side_a = ( (bx - ax)**2.0d0 + (by - ay)**2.0d0 )**0.5d0
    side_b = ( (cx - bx)**2.0d0 + (cy - by)**2.0d0 )**0.5d0
    side_c = ( (ax - cx)**2.0d0 + (ay - cy)**2.0d0 )**0.5d0

    s = (side_a + side_b + side_c) / 2.0d0

    area_t = ( s*(s-side_a)*(s-side_b)*(s-side_c) )**0.5d0 

    if (area_t < 0.0d0) then
        write(*,*) ""
        write(*,*) "AREA ERROR: The normals are inverted."
        stop
    end if

end subroutine area_tri3

subroutine init

    use shared
    implicit none

    integer(kind=4) :: neq, vol, eq

    write(*,'(A)') " + Initializing properties."


    if (formulation == 1) neq = 4

    ! Allocate the number of equations.
    allocate(q(neq,ncells))

    do vol = 1,ncells
        do eq = 1,neq
            q(eq,vol) = 0.0d0
        end do
    end do

end subroutine init

subroutine calc_cell_area

    use shared
    implicit none

    integer(kind=4) :: i
    real(kind=8) :: a1, a2
    real(kind=8) :: ax,ay, bx,by, cx,cy, dx, dy

    write(*,'(A)') " + Calculating cells areas."

    allocate(f_area(ncells))

    f_area = 0.0d0


    do i = 1, ncells

        a1 = 0.0d0
        a2 = 0.0d0

        ! Calc the area for TRI_3 cell.

        if (inpoel(5,i) == 3) then

            ax = coord(1,inpoel(1,i))
            ay = coord(2,inpoel(1,i))

            bx = coord(1,inpoel(2,i))
            by = coord(2,inpoel(2,i))

            cx = coord(1,inpoel(3,i))
            cy = coord(2,inpoel(3,i))

            call area_tri3(ax,ay, bx,by, cx,cy, f_area(i))

        ! Calc the area for QUAD_4 cell.

        else if (inpoel(5,i) == 4) then 

            ax = coord(1,inpoel(1,i))
            ay = coord(2,inpoel(1,i))

            bx = coord(1,inpoel(2,i))
            by = coord(2,inpoel(2,i))

            cx = coord(1,inpoel(3,i))
            cy = coord(2,inpoel(3,i))

            dx = coord(1,inpoel(4,i))
            dy = coord(2,inpoel(4,i))

            call area_tri3(ax,ay, bx,by, cx,cy, a1)
            call area_tri3(bx,by, cx,cy, dx,dy, a2)

            f_area(i) = a1 + a2

        end if
    end do

end subroutine calc_cell_area

subroutine initial_condition

    use shared
    implicit none

    integer(kind=4) :: i

    write(*,'(A)') " + Applying initial condition."


    do i = 1, ncells
        q(1,i) = rho
        q(2,i) = rhou
        q(3,i) = rhov
        q(4,i) = e
    end do

end subroutine initial_condition
