function lmedsea(lat,lon)
!--------------------------------------------------------------------------------
!
! Return true if the point with lat/lon is within the medeterranian sea
!
!--------------------------------------------------------------------------------

implicit none

real, intent(in) :: lat,lon
logical          :: lmedsea


lmedsea = .false.

! first rough check:
if( lat > 50.0 .or. lat < 30.0 .or. lon < -10.0 .or. lon > 40.0 ) return


! closer check, central medsea
if(lat > 30.0 .and. lat < 50.0 .and. lon > 0.0 .and. lon < 27.0 ) then
  lmedsea = .true.
  return
end if

! closer check, eastern medsea
if(lat > 30.0 .and. lat < 40.0 .and. lon > 20.0 .and. lon < 40.0 ) then
  lmedsea = .true.
  return
end if

! closer check, wester medsea
if(lat > 30.0 .and. lat < 42.0 .and. lon > -8.0 .and. lon < 2.0 ) then
  lmedsea = .true.
  return
end if

!--------------------------------------------------------------------------------
end function lmedsea
