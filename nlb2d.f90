! New leo's Bru 2D code (nlb2d). !
! Author: Leonardo Motta
! Contributions: Edson, Andre.

program nlb2d

    use shared
    implicit none


    ! Pre-Processing.
    call indat
    call basic_ds
    call hc_faces

end program nlb2d

