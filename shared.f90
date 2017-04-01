module shared

    implicit none 

    !
    ! NAMELIST VARIABLES (INPUTS).
    !
    integer(kind=4) :: hash_typ, hs, restart

    !
    ! BASIC DATASTRUCTURES
    !

    ! nnode: Max number of nodes in an element.
    integer(kind=4) :: nnode = 4

    ! nelem: Number of elements in a mesh.
    integer(kind=4) :: nelem

    ! npoin: Number of points in a mesh.
    integer(kind=4) :: npoin

    ! nghos: Number of boundary faces.
    integer(kind=4) :: nghos

    ! Connectivit matrix inpoel(1:nnode, 1:nelem).
    integer(kind=4), allocatable, dimension(:,:) :: inpoel

    ! Coordinate points coord(1:ndimn, 1:npoin).
    real(kind=8), allocatable, dimension(:,:) :: coord

    ! Hash table. It will be missed after preproc.
    integer(kind=4), allocatable, dimension(:) :: ihash


    !
    ! FACE-BASED DATA STRUCTURES CREATION
    !

    ! Faces vector.
    ! face(i,1): Vertex number one of the face.
    ! face(i,2): Vertex number two of the face.
    ! face(i,3): Element to the left.
    ! face(i,4): Element to the right.
    integer(kind=4), allocatable, dimension(:,:) :: face

    ! Ghost vector (Read).
    integer(kind=4), allocatable, dimension(:,:) :: ghost

    ! Ghost vector (use).
    integer(kind=4), allocatable, dimension(:,:) :: ighost

end module shared
