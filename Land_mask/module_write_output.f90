module module_write_output
  implicit none
contains
  subroutine write_output_netcdf(outfile_method, outfile, nlong1, nlat1,  &
     long1,lat1,region_codes) !, &

    use timeroutines,                  only: time, eqtime, timediff, inctime
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE
    
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile
    integer,          intent(in) :: nlong1, nlat1

    real, dimension(nlong1,nlat1), intent(in) :: region_codes

    real, dimension(:), intent(in) :: lat1, long1
!    type(time), intent(inout) :: ctime
!    integer(kind=8),  intent(in) :: hrinc 
    !Local variables
    character(len = 1000) :: Times,BTIMESTR, inclusion_method
    type(time)            :: btime
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT1, DIMID_NLONG1, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

    !FT is hardwired
    ft =104 

    !
    ! Setup output time
    !
     print*, 'method create'
!      print*, 'hrinc',hrinc
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlong1))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlat1))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"inclusion_method",inclusion_method))
     print*,"inclusion_method"
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"lon",nlong1,dimid_nlong1))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat1,dimid_nlat1))
       ! The data variables...

      call nf90call(nf90_def_var(ncidout,"region_codes",NF90_FLOAT, &
            (/dimid_nlong1,dimid_nlat1/),varid))
      print*,"region_codes"
          call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlong1/),varid))
          call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat1/),varid))
      print*,"lat"
       !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","GENERATED OFFLINE"))
       call nf90call(nf90_enddef(ncidout))

    
    end if !End of outfile setup
    print*, 'end if setup file'
    !
    !print 77, lat1,long1,region_codes     
    call  write2das3dnc(ncidout,"region_codes",region_codes,(/1,1,1/),(/nlong1,nlat1,1/))
    call  write1das2dnc(ncidout,"lat",lat1,(/1,1/),(/nlat1,1/))
    call  write1das2dnc(ncidout,"lon",long1,(/1,1/),(/nlong1,1/))
    print*, 'everything was written'
    call nf90call(nf90_sync(ncidout))
    !
    call nf90call(nf90_close(ncidout))
 77 format(3f8.2)
end subroutine write_output_netcdf
end module 
