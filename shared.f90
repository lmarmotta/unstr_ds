module shared

    implicit none 

    !
    ! NAMELIST VARIABLES (INPUTS).

    !
    real(kind=8) :: rho,rhou,rhov,e
    integer(kind=4) :: hash_typ, hs, restart,formulation
    integer(kind=4) :: n_iter

    !
    ! BASIC DATASTRUCTURES
    !

    ! nnode: Max number of nodes in an element (2D = QUAD_4 = 4).
    integer(kind=4) :: max_nnode_pcell = 4

    ! ncells: Number of cells in a mesh.
    integer(kind=4) :: ncells 

    ! npoin: Number of points in a mesh.
    integer(kind=4) :: npoin

    ! nghos: Number of boundary faces.
    integer(kind=4) :: nghos

    ! nfaces: Number of faces in the mesh.
    integer(kind=4) :: nfaces

    ! Connectivit matrix inpoel(1:nnode, 1:ncells).
    ! inpoel(1,cell): cell node 1.
    ! inpoel(2,cell): cell node 2.
    ! inpoel(3,cell): cell node 3.
    ! inpoel(4,cell): cell node 4.
    ! inpoel(5,cell): cell type.
    integer(kind=4), allocatable, dimension(:,:) :: inpoel

    ! Coordinate points coord(1:ndimn, 1:npoin).
    real(kind=8), allocatable, dimension(:,:) :: coord

    ! Hash table. It will be missed after preproc.
    integer(kind=4), allocatable, dimension(:) :: ihash

    ! Area of each element.
    real(kind=8), allocatable, dimension(:) :: f_area

    ! Normals to each edge.
    real(kind=8), allocatable,dimension(:) :: sx
    real(kind=8), allocatable,dimension(:) :: sy

    !
    ! FACE-BASED DATA STRUCTURES CREATION
    !

    ! Faces vector.
    ! face(i,1): Vertex number one of the face.
    ! face(i,2): Vertex number two of the face.
    ! face(i,3): Cell to the left.
    ! face(i,4): Cell to the right.
    integer(kind=4), allocatable, dimension(:,:) :: face

    ! Ghost vector (Read).
    integer(kind=4), allocatable, dimension(:,:) :: ghost

    ! Ghost vector (Use).
    integer(kind=4), allocatable, dimension(:,:) :: ighost

    ! 
    ! PROPERTIES AND DATASTRUCTURES.
    !

    real(kind=8), allocatable, dimension(:,:) :: q


    !
    ! AUXILIARY DATASTRUCTURES.
    !

    ! Tecplot friendly property variable.
    real(kind=8), allocatable, dimension(:) :: prop_nodeval
    real(kind=8), allocatable, dimension(:) :: vol_prop


end module shared
