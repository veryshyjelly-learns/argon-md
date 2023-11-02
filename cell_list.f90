SUBROUTINE new_nlist(TotAtom, r, rn, ncel, ll, hoc)
    use general, only: dp
    implicit none
    integer,intent(in) :: TotAtom
    real,intent(in) :: r(TotAtom,3)
    real(kind=dp) :: rn 
    integer:: icelx, icely, icelz, ncel, i
    integer, intent(inout):: hoc(ncel, ncel, ncel), ll(TotAtom)
    
    write(*, *) "new_nlist"

    do icelx=1,ncel
        do icely=1, ncel
            do icelz=1, ncel
                hoc(icelx, icely, icelz) = 0
            enddo
        enddo
    enddo
    do i=1,TotAtom
        write(*, *) 'r(i, 1)', r(i, 1)
        write(*, *) 'r(i, 2)', r(i, 2)
        write(*, *) 'r(i, 3)', r(i, 3)
        write(*, *) 'rn', rn
        icelx=int(r(i, 1)/rn) + 1
        icely=int(r(i, 2)/rn) + 1
        icelz=int(r(i, 3)/rn) + 1
        write(*, *) 'icelx=', icelx
        write(*, *) 'icely=', icely
        write(*, *) 'icelz=', icelz
        ll(i) = hoc(icelx, icely, icelz)
        hoc(icelx, icely, icelz) =i
    enddo
    return
end