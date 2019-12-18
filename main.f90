program in_out
	use diffur
    use boundary
    implicit none
    	
!	-----------------------------------------------------
!	|	Program integrates differential equation   		|
!	|	Available methods are:              			|
!	|			Runge-Kutta - rk key					|
!	|			Adams interpolation - ai key			|
!	|			Adams extrapolation - ae key			|
!	-----------------------------------------------------

    real(16), dimension(1:int(interval/h + 1), size(x_0)) :: x, y
    real(16), dimension(0:2*n+1) :: z
!    real(16), dimension(dims) :: u = (/(-1.0, i=1, dims)/)
    real(16) :: t
    
    x = shooting('forward')
    t = 0
    
    open(1, file = trim("shooting_forward.dat"))
    do i = 1, int(interval/h + 1)
        
        write(1,*) t, x(i, 1), x(i, 2)
        t = t + h
    
    enddo    
    close(1)
    
    
    t = 0
    
    y = shooting('reverse')
    open(33, file = trim("shooting_reverse.dat"))
    do i = 1, size(y, 1)
        
        write(33,*) t, y(i, 1), y(i, 2)
        t = t + h
    
    enddo    
    close(33)   
    
    t = 0
    z(0:n) = x(:, 1)
    z(n+1:2*n+1) = x(:, 2) 
    z = finite_differences(x)
    open(33, file = trim("finite_differences.dat"))
    do i = 1, n
        
        write(33,*) t, z(i), z(n+i)
        t = t + h
    
    enddo    
    close(33)
    
    
end program in_out
