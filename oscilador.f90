program oscilador 

   implicit none 
   integer :: i, n, d, j   
   real :: m, k, b, h, x0, v0, x, v, a, t, t_ac
   real :: cuadratura, factor                            ! d es el número de unidades de tiempo de 0.01 s hasta el cual queremos calcular la          
   real, allocatable, dimension(:) :: f                  ! energía disipada por el oscilador; debe ser un número entero par, por ejemplo, si queremos conocer la energía disipada 
                                                         ! hasta t = 1 s, debemos establecer d = 100.  
   open(10, file = "input_data.inp")
        read(10,*)
        read(10,*)m, k, b, d  
   close(10) 

   h = 0.01 
   n = 1000
   i = 0 
   x0 = 1.0 
   v0 = 0.0 
   t = 0.0  
   t_ac = d * h 

   allocate(f(1:n+1))

   !Asignamos los valores inciales de x y v, calculamos el valor incial de a  

   x = x0 
   v = v0
   a = - (b/m) * v - (k/m) * x 
   f(1) = v0 

   !Ciclo que calcula y reasigna los valores de la posición y velocidad

   open(30, file = "output_data.out")
        write(30, 20)real(i), t, x, v, f(1), b*f(1)   !Se escriben los valores iniciales en nuestro archivo de datos   
         do i = 1, n  
                a = - (b/m) * v - (k/m) * x 
                x = x + v*h + (a/2.0) * h**2 
                v = v + a*h
                t = h*i    
                f(i+1) = v**2                                 !Se guardan los valores de v**2 en las componentes de f 
                write(30,20)real(i), t, x , v, f(i+1), b*f(i+1)       !Se escriben los valores de t, x, v, f(i+1) para i     
         end do 
   close(30)

   20 format(6F15.10)

   cuadratura = f(1) + f(d+1)
   j = 0  
   
   do j =2, d
        if (mod(j,2)==0) then 
           factor = 4.0 
        else 
           factor = 2.0 
        end if 
        cuadratura = cuadratura + factor * f(j)      
   end do 

   cuadratura = (h/3) * b * cuadratura 
   write(*,'(A,F8.3,A)')"La energía disipada desde t = 0 hasta t =", t_ac, "s es: "
   write(*,70)cuadratura 

   70 format(1F15.10)


end program oscilador  
