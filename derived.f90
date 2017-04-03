subroutine area(ax,ay, bx,by, cx,cy, area_t)

    ! Calculate the area of triangle or quad with vectorial dark magic.
    !             b
    !            /\
    !   side_a  /  \ side_b
    !          /____\
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

end subroutine area
