SUBROUTINE cell_list(TotAtom, r, rn, ncel, ll, hoc)
    use general, only: dp
    implicit none
    integer,intent(in) :: TotAtom
    real(kind=dp),intent(in) :: r(TotAtom,3)
    real(kind=dp) :: rn 
    integer:: icelx, icely, icelz, ncel, i, icel(3)
    integer, intent(inout):: hoc(ncel, ncel, ncel), ll(TotAtom)
    
    do icelx=1,ncel
        do icely=1, ncel
            do icelz=1, ncel
                hoc(icelx, icely, icelz) = 0
            enddo
        enddo
    enddo

    do i=1,TotAtom
        icel(:) = int(r(i, :)/rn + 1)
        ll(i) = hoc(icel(1), icel(2), icel(3))
        hoc(icel(1), icel(2), icel(3)) = i
    enddo
    return
end