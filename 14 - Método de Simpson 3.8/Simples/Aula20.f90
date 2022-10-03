







 program Aula14

         implicit none

         real a, b, h, f, integral

          real, allocatable :: x(:)

          integer i, n
          


        print*, "Forneça os valores do intervalo [a,b] de integração para integrar f(x): "

        read*, a, b
        
        h = (b-a)/2.0  ! Largura do intervalo entre os valores
        
        print*, "Diga quantos elementos tem no intervalo"
        
        read*, n
        
        allocate(x(n))
        
        x(1)=a; x(2)=b
        
        i = 2; integral =0
        
        do while(f(x(i))/=b .and. i<=n)

              x(i) = x(i-1)+h
               integral = integral +3*f(x(i))
              i = i+1
              
        end do
        
        integral = (3/8.0)*h*(integral + f(a) + f(b))

        print*, "A integral da função no intevalo [", a, ",", b, "]é:", integral


end program Aula14



real function f(a)    ! Definimos uma função para teste

    real a

     f = 3*a**2+2

       return
end function f






