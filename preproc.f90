subroutine indat

    use shared
    implicit none


    ! Read namelist parameters declared in the module.

    namelist /PAR_preproc/ hash_typ, hs, restart

    open(1,file='input.in')

    read(1,PAR_preproc)

    close(1)

end subroutine indat

subroutine basic_ds

    use shared
    implicit none

    integer(kind=4) :: i, elemn, kindof, bc_t, n1, n2, n3, n4
    integer(kind=4) :: point, x_coord, y_coord


    ! At first let's get our mesh size.

    open(1,file='elemnConn.dat')
    open(2,file='pointCord.dat')
    open(3,file='elemnBonc.dat')

    read(1,*) nelem
    read(2,*) npoin
    read(3,*) nghos

    write(*,*)
    write(*,'(A,I8)') " + The number of elements in the mesh is   : ", nelem
    write(*,'(A,I8)') " + The number of points in the mesh is     : ", npoin
    write(*,'(A,I8)') " + The number of boundary faces in the mesh: ", nghos


    ! Now, let's build the connective matrix. Here I am loading the connectivity
    ! nodes as well as the element type in the same guy. It's not usual but...
    ! inpoel(1,elem): Element node 1.
    ! inpoel(2,elem): Element node 2.
    ! inpoel(3,elem): Element node 3.
    ! inpoel(4,elem): Element node 4.
    ! inpoel(5,elem): Element type.

    allocate(inpoel(nnode+1, nelem))

    inpoel = 0

    do i = 1, nelem
        read(1,*) elemn, kindof, n1, n2, n3, n4

        inpoel(1,i) = n1
        inpoel(2,i) = n2
        inpoel(3,i) = n3
        inpoel(4,i) = n4  ! Will be zero if TRI_3 element.
        inpoel(5,i) = kindof

    end do


    ! Now, Let's build the coordinates.

    allocate(coord(2,npoin))

    do i = 1, npoin

        read(2,*) point, x_coord, y_coord

        coord(1,point) = x_coord
        coord(2,point) = y_coord

    end do


    ! Now, Let's read the ghosts. Note that for our 2D meshes, the boundary cond
    ! ditions will always have two points.

    allocate(ghost(nghos,3))

    do i = 1, nghos
        
        read(3,*) bc_t, n1, n2

        ghost(i,1) = bc_t
        ghost(i,2) = n1
        ghost(i,3) = n2

    end do


    close(1)
    close(2)
    close(3)

end subroutine basic_ds

subroutine hc_faces

    ! To do: Support for mixed cells needed.

    ! Here I use hash table to build the faces of the mesh. The goal here is to 
    ! build efficiently the mesh datastructure. Let's roll...

    use shared
    use functions
    implicit none

    integer(kind=4) :: ivol, nfaces, idx, nf, bc, n_colision,inf,i,infi
    integer(kind=4) :: ig, is_bc, pf1, pf2, pg1, pg2, max_hash_size

    ! Inner element face.
    integer(kind=4), dimension(4,2) :: ine 


    ! This is a dummy allocation for resize later during hashing process.
    allocate(face(1,4))
    allocate(ighost(-nghos:-1,4))

    ighost = 0

    nf            =  0
    bc            = -1
    n_colision    =  0
    max_hash_size =  0
    infi          =  0


    ! Get the correct hash size for efficient allocation.

    call hash_size(max_hash_size)

    write(*,'(A,I8)') " + The maximun hash size is                : ", max_hash_size + hs

    allocate(ihash(max_hash_size + hs))

    ihash = 0
    ine   = 0
    inf   = 0


    ! Now, proceed with the mesh itself.

    do ivol = 1, nelem

        if (inpoel(5,ivol) == 3) then

            ! TRI_3 internal element.
            
            inf = 3

            ! First face.
            ine(1,1) = inpoel(1,ivol)
            ine(1,2) = inpoel(2,ivol)

            ! Second face.
            ine(2,1) = inpoel(2,ivol)
            ine(2,2) = inpoel(3,ivol)

            ! Third face.
            ine(3,1) = inpoel(3,ivol)
            ine(3,2) = inpoel(1,ivol)

        else if (inpoel(5,ivol) == 4) then

            ! QUAD_4 internal element.

            inf = 4

            ! First face.
            ine(1,1) = inpoel(1,ivol)
            ine(1,2) = inpoel(2,ivol)

            ! Second face.
            ine(2,1) = inpoel(2,ivol)
            ine(2,2) = inpoel(3,ivol)

            ! Third face.
            ine(3,1) = inpoel(3,ivol)
            ine(3,2) = inpoel(4,ivol)

            ! Fourth face.
            ine(4,1) = inpoel(4,ivol)
            ine(4,2) = inpoel(1,ivol)

        end if


        ! Loop trough faces of this element and hash then man !

        do i = 1, inf

            if (hash_typ == 1) then
                idx = hash_b(ine(i,1),ine(i,2))
            else
                idx = hash_a(ine(i,1),ine(i,2))
            end if

            ! If the hash has an empty position it means that no face using these
            ! two points was created. Being that the case, we can create it without
            ! any fear.

            if (ihash(idx) == 0) then

                ! Increase the number of faces counter.

                nf = nf + 1

                ! Now allocate the face size.

                call realloc_int2D_faces(nf-1,nf,4,4)

                ! We are now creating a face what means that this hash position is
                ! no longer availiable.

                ihash(idx) = nf

                ! Hey there ! I'm your face based datastructure that you need to
                ! graduate.... ;)

                face(nf,1) = ine(i,1)
                face(nf,2) = ine(i,2)
                face(nf,3) = ivol
                face(nf,4) = -1

            else if (face(ihash(idx),4) == -1) then

                face(ihash(idx),4) = ivol

            else if (ihash(idx) /= 0 .and. face(ihash(idx),4) /= -1) then

                ! Colision on the hash table !

                n_colision = n_colision + 1

                ! Here I'am dealing with colisions using linear probing. Fairly 
                ! simpler and less memory hungry than linked lists.

                ! Loop through the hash table to find empty spots.

                ! do while (ihash(idx) /= 0 .and. infi == 0)
                do while (ihash(idx) /= 0 .and. infi == 0)

                    if (idx == max_hash_size + hs) then
                        idx = 1
                        infi = 1
                    end if

                    idx = idx + 1

                end do 

                ! Increase the number of faces counter.

                nf = nf + 1

                ! Now allocate the face size.

                call realloc_int2D_faces(nf-1,nf,4,4)

                ! We are now creating a face what means that this hash position is
                ! no longer availiable.

                ihash(idx) = nf

                ! Hey there ! I'm your face based datastructure that you need to
                ! graduate.... ;)

                face(nf,1) = ine(i,1)
                face(nf,2) = ine(i,2)
                face(nf,3) = ivol
                face(nf,4) = -1

            end if

        end do 

    end do

    nfaces = nf

    write(*,'(A,I8)') " + The number of faces in the mesh         : ", nfaces

    ! Now we have to number the ghosts.

    do nf = 1, nfaces
        if (face(nf,4) < 0) then
            face(nf,4) = bc
            bc = bc - 1
        end if
    end do

    
    ! Now, to deal with the mesh ghosts, Let's loop through the faces of the
    ! mesh. Check the boundary faces, and check the nodes that are contained
    ! inside that element.

    is_bc = 0

    do nf = 1, nfaces
        do ig = 1, nghos

            is_bc = face(nf,4)


            ! If the face I'm in is a boundary face, I have to link the ighost
            ! vector with the face index.

            if (is_bc < 0) then

                pf1 = face(nf,1)
                pf2 = face(nf,2)

                pg1 = ghost(ig,2)
                pg2 = ghost(ig,3)

                if (pf1 == pg1 .and. pf2 == pg2 .or. pf1 == pg2 .and. pf2 == pg1) then

                    ighost(-ig,1) = ghost(ig,1)  ! Boundary type.
                    ighost(-ig,2) = nf           ! Face index.
                    ighost(-ig,3) = face(nf,1)   ! Face point 1.
                    ighost(-ig,4) = face(nf,2)   ! Face point 2.

                end if

            end if
        end do
    end do


    write(*,'(A,I8)') " + The number of collisions                : ", n_colision


    ! Print data for restart.

    open(4,file="face.dat")
    open(5,file="ighost.dat")

    do idx = 1,nfaces
        write(4,'(5I8)') idx,face(idx,1),face(idx,2),face(idx,3),face(idx,4)
    end do

    do ig = 1, nghos
        write(5,'(5I8)') ig,ighost(-ig,1),ighost(-ig,2),ighost(-ig,3),ighost(-ig,4)
    end do


    ! Forget the colisions for a while.

    if (n_colision /= 0) then
        write(*,*) ""
        write(*,*) "HASH ERROR: Colision management not fully validated."
        stop
    end if

    close(4)
    close(5)

    deallocate(ihash)

end subroutine hc_faces

subroutine realloc_int2D_faces(size_1a,size_1b,size_2a,size_2b)

    use shared
    implicit none

    ! Input values.
    ! vec_1(size_1a,size_2a) --> vec_1(size_1b,size_2b) 
    !
    ! size_1a: Size of dim1 original vector.
    ! size_2a: Size of dim2 original vector.
    ! size_1b: Size of dim2 new vector.
    ! size_2b: Size of dim2 new vector.

    integer(kind=4) :: size_1a,size_1b,size_2a,size_2b

    integer(kind=4) :: i,j
    integer(kind=4), allocatable, dimension(:,:) :: new


    allocate(new(size_1a,size_2a))

    new = 0

    do i = 1, size_1a
        do j = 1, size_2a
            new(i,j) = face(i,j)
        end do
    end do

    deallocate(face)

    allocate(face(size_1b,size_2b))

    do i = 1, size_1a
        do j = 1, size_2a
            face(i,j) = new(i,j)
        end do
    end do

end subroutine  realloc_int2D_faces

subroutine realloc_int1D_ihash(size_1,size_2)

    use shared
    implicit none

    ! Input values.
    ! vec_1(size_1a,size_2a) --> vec_1(size_1b,size_2b) 
    !
    ! size_1: Size of dim1 original vector.
    ! size_2: Size of dim2 original vector.

    integer(kind=4) :: size_1, size_2

    integer(kind=4) :: i
    integer(kind=4), allocatable, dimension(:) :: new


    allocate(new(size_1))

    new = 0

    do i = 1, size_1
        new(i) = ihash(i)
    end do

    deallocate(ihash)

    allocate(ihash(size_2))

    do i = 1, size_1
        ihash(i) = new(i)
    end do

end subroutine  realloc_int1D_ihash

subroutine hash_size(max_hash_size)

    use shared
    use functions
    implicit none

    integer(kind=4) :: max_hash_size
    integer(kind=4) :: ivol, p1, p2, idx

    ! Get the size of the hash main vector.

    do ivol = 1, nelem


        ! First face combination.

        p1 = inpoel(1,ivol)
        p2 = inpoel(2,ivol)

        if (hash_typ == 1) then
            idx = hash_b(p1,p2)
        else
            idx = hash_a(p1,p2)
        end if

        if (idx > max_hash_size) then
            max_hash_size = idx
        end if


        ! Second face combination.

        p1 = inpoel(2,ivol)
        p2 = inpoel(3,ivol)

        if (hash_typ == 1) then
            idx = hash_b(p1,p2)
        else
            idx = hash_a(p1,p2)
        end if

        if (idx > max_hash_size) then
            max_hash_size = idx
        end if


        ! Third face combination.

        p1 = inpoel(3,ivol)
        p2 = inpoel(4,ivol)

        if (hash_typ == 1) then
            idx = hash_b(p1,p2)
        else
            idx = hash_a(p1,p2)
        end if

        if (idx > max_hash_size) then
            max_hash_size = idx
        end if


        ! Fourth face combination.

        p1 = inpoel(4,ivol)
        p2 = inpoel(1,ivol)

        if (hash_typ == 1) then
            idx = hash_b(p1,p2)
        else
            idx = hash_a(p1,p2)
        end if

        if (idx > max_hash_size) then
            max_hash_size = idx
        end if

    end do


end subroutine hash_size
