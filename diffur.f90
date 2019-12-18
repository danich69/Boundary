module diffur

    implicit none
    
    integer, parameter :: dims = 4
    integer :: i
    
    real(16), parameter :: interval = 2.0
    real(16), dimension(dims), parameter :: x_0 = (/1.0, -1.0 , 1.0, 1.0/)
    real(16), dimension(dims), parameter :: x_final = (/2.7416, -1.4711, 0.0, 0.0 /)
    real(16), parameter :: h = 0.005
    integer, parameter :: n = int(interval/h)
    real(16), dimension(n), parameter :: u = 0.0

    contains
    
        function f(x)
        
            real(16), dimension(:) :: x
            real(16), dimension(size(x)) :: f
            
             f(1) = x(3)
             f(2) = x(4)
             f(3) = cos(x(3)) - x(4)**2
             f(4) = sin(x(4)) - x(1)
        
        end function f

end module diffur
