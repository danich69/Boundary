module boundary
    use diffur
    use non_linear_system
    use non_linear_system_algebraic
    implicit none
    
    contains
    
        function forward_Runge_Kutta(start_vel) result(x)
        
            real(16), dimension(1:int(interval/h + 1), size(x_0)) :: x
            real(16), dimension(size(x_0)/2) :: start_vel
            real(16), dimension(dims) :: k1, k2, k3, k4
            
            x(1, :) = x_0
            x(1, 3) = start_vel(1)
            x(1, 4) = start_vel(2)
            
            do i = 1, size(x, 1) - 1
                
                k1 = h * f(x(i, :))
                k2 = h * f(x(i, :) + k1/2.0)
                k3 = h * f(x(i, :) + k2/2.0)
                k4 = h * f(x(i, :) + k3)

                x(i+1, :) = x(i, :) + (k1 + 2 * k2 + 2 * k3 + k4)/6.0
            
            enddo
            
        end function forward_Runge_Kutta
    
        function reverse_Runge_Kutta(start_vel) result(x)
        
            real(16), dimension(1:int(interval/h + 1), size(x_0)) :: x
            real(16), dimension(size(x_0)/2) :: start_vel
            real(16), dimension(dims) :: k1, k2, k3, k4
            real(16) :: k
            
            k = -h
            x(size(x, 1), :) = x_final
            x(size(x, 1), 3) = start_vel(1)
            x(size(x, 1), 4) = start_vel(2)
            
            do i = size(x, 1), 2, -1
            
                k1 = k * f(x(i, :))
                k2 = k * f(x(i, :) + k1/2.0)
                k3 = k * f(x(i, :) + k2/2.0)
                k4 = k * f(x(i, :) + k3)

                x(i-1, :) = x(i, :) + (k1 + 2 * k2 + 2 * k3 + k4)/6.0
                                
            enddo
            
        end function reverse_Runge_Kutta
        
        function forward_to_minimize(x)
        
            real(16), dimension(size(x_0)/2) :: x
            real(16), dimension(1:int(interval/h + 1), size(x_0)) :: solution
            real(16), dimension(2) :: forward_to_minimize, y
            
            solution = forward_Runge_Kutta(x)
            y = solution(size(solution, 1), 1:2)
            forward_to_minimize = y - x_final(1:2)
        
        end function forward_to_minimize
        
        function reverse_to_minimize(x)
        
            real(16), dimension(size(x_0)/2) :: x
            real(16), dimension(1:int(interval/h + 1), size(x_0)) :: solution
            real(16), dimension(2) :: reverse_to_minimize, y
            
            solution = reverse_Runge_Kutta(x)
            y = solution(1, 1:2)
            reverse_to_minimize = y - x_0(1:2)
        
        end function reverse_to_minimize
        
        function shooting(key) result(x)
        
            real(16), dimension(1:int(interval/h + 1), size(x_0)) :: x
            real(16), dimension(size(x_0)/2) :: junk_guess
            real(16), dimension(size(x_0)/2) :: actual_speed
            character*7 :: key
                        
            call random_number(junk_guess)
            
            if(key == 'forward') then
                do while(.True.)
                    junk_guess = abs(junk_guess) * 0.6 + 0.7
                    call nonlinear_system_solver(junk_guess, forward_to_minimize, actual_speed)
                    x = forward_Runge_Kutta(actual_speed)
                    if(.NOT. isnan(actual_speed(1)) .AND. .NOT. isnan(actual_speed(2))) then
                        exit
                    endif
                enddo
            else
                do while(.True.)                    
                    junk_guess = abs(junk_guess) - (/1, 3/)
                    call nonlinear_system_solver(junk_guess, reverse_to_minimize, actual_speed)
                    x = reverse_Runge_Kutta(actual_speed)
                    if(.NOT. isnan(actual_speed(1)) .AND. .NOT. isnan(actual_speed(2))) then
                        exit
                    endif
                enddo
            endif
        
        end function shooting
        
        function finite_differences(x) result(y)
        
            real(16), dimension(0:2*n+1) :: x, y
            
            x = (/(1, i=0, 2*n+1)/)
            call nonlinear_system_solver_algebraic(x, algebraic_system, y)
        
        end function finite_differences
        
        function algebraic_system(y) result(f)
            
            real(16), dimension(0:2*size(u)+1) :: y
            real(16), dimension(0:n, 2) :: x
            real(16), dimension(0:2*size(u)+1) :: f
            
            x(:, 1) = y(0:n)
            x(:, 2) = y(n+1:2*n+1)
            
            f(0) = y(0) - x_0(1)
            f(n) = y(n) - x_final(1)
            f(n+1) = y(n+1) - x_0(2)
            f(2*n+1) = y(2*n+1) - x_final(2)
            
            do i = 1, n - 1
                f(i) = ( x(i+1,1) - 2*x(i,1) + x(i-1,1) )/h**2 - cos((x(i+1,2) - x(i,2))/h) + ((x(i+1,2)-x(i,2))/h)**2
            enddo
            
            do i = n + 2, 2 * n
                f(i) = ( x(i+1-n-1,2) - 2*x(i-n-1,2) + x(i-1-n-1,2) )/h**2 - sin((x(i+1-n-1,2) - x(i-n-1,2))/h) + x(i-n-1,1)
            enddo
            
        end function algebraic_system
        
end module boundary
