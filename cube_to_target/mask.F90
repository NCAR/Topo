module mask

use shr_kind_mod, only: r8  => shr_kind_r8

IMPLICIT NONE

public 

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

logical function greenland_mask(lat, lon)
    implicit none
    real(8), intent(in) :: lat, lon

    if ((lat >= 59.0 .AND. lat < 60.0 .AND. lon >= 314.36 .AND. lon <= 317.29)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 60.0 .AND. lat < 61.0 .AND. lon >= 310.69 .AND. lon <= 319.22)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 61.0 .AND. lat < 62.0 .AND. lon >= 309.42 .AND. lon <= 319.71)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 62.0 .AND. lat < 63.0 .AND. lon >= 308.41 .AND. lon <= 320.35)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 63.0 .AND. lat < 64.0 .AND. lon >= 307.45 .AND. lon <= 321.49)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 64.0 .AND. lat < 65.0 .AND. lon >= 306.73 .AND. lon <= 321.69)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 65.0 .AND. lat < 66.0 .AND. lon >= 305.49 .AND. lon <= 325.93)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 66.0 .AND. lat < 67.0 .AND. lon >= 305.07 .AND. lon <= 328.19)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 67.0 .AND. lat < 68.0 .AND. lon >= 304.91 .AND. lon <= 330.07)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 68.0 .AND. lat < 69.0 .AND. lon >= 305.42 .AND. lon <= 336.59)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 69.0 .AND. lat < 70.0 .AND. lon >= 304.25 .AND. lon <= 339.81)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 70.0 .AND. lat < 71.0 .AND. lon >= 304.02 .AND. lon <= 340.80)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 71.0 .AND. lat < 72.0 .AND. lon >= 302.70 .AND. lon <= 340.75)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 72.0 .AND. lat < 73.0 .AND. lon >= 302.94 .AND. lon <= 339.49)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 73.0 .AND. lat < 74.0 .AND. lon >= 302.34 .AND. lon <= 342.33)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 74.0 .AND. lat < 75.0 .AND. lon >= 300.93 .AND. lon <= 343.64)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 75.0 .AND. lat < 76.0 .AND. lon >= 291.16 .AND. lon <= 343.45)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 76.0 .AND. lat < 77.0 .AND. lon >= 287.00 .AND. lon <= 344.66)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 77.0 .AND. lat < 78.0 .AND. lon >= 286.00 .AND. lon <= 344.81)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 78.0 .AND. lat < 79.0 .AND. lon >= 286.00 .AND. lon <= 343.48)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 79.0 .AND. lat < 80.0 .AND. lon >= 290.00 .AND. lon <= 349.58)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 80.0 .AND. lat < 81.0 .AND. lon >= 292.00 .AND. lon <= 349.56)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 81.0 .AND. lat < 82.0 .AND. lon >= 295.44 .AND. lon <= 349.64)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 82.0 .AND. lat < 83.0 .AND. lon >= 299.09 .AND. lon <= 349.15)) then
        greenland_mask = .true.
        return
    endif
    if ((lat >= 83.0 .AND. lat < 84.0 .AND. lon >= 309.33 .AND. lon <= 341.22)) then
        greenland_mask = .true.
        return
    endif

    greenland_mask = .false.
    return
end function greenland_mask

logical function antarctica_mask(lat, lon)
    implicit none
    real(8), intent(in) :: lat, lon

    if (lat <= -60.0) then
        antarctica_mask = .true.
        return
     endif
     antarctica_mask = .false.
     return
   end function antarctica_mask
end module mask
