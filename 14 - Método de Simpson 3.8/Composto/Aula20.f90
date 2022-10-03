







 program Aula14

         implicit none

         real a, b, f, integral, h

        real, allocatable :: x(:)

        integer i, n




        print*, "Forneça os valores a e b do intervalo [a,b] de integração para integrar f(x): "

        read*, a, b


        print*, "Diga quantos elementos tem nesse intervalo: "

        read*, n

        allocate(x(n))

        x(1) = a; x(n) = b

        h =(b-a)/n  ! Largura dos subintervalos
        
        i=2
        
        do while( x(i)/=b .and. i<=n)

              x(i) = x(i-1)+h
              i=i+1
         end do

        print*, "A integral da função no intevalo [", a, ",", b, "]é:", integral(x, h, n)


end program Aula14



real function f(a)    ! Definimos uma função para teste

    real a

     f = 3*a**2+2

       return
end function f



real function integral(a1, h, n)

     real a1(n), h, soma, soma2

     integer n, i

      soma = 0; soma2=0

      do i=2, n-1, 3

         soma = soma + f(a1(i)) + f(a1(i+1))
      end do
      
      do i=4, n-2, 3

         soma2 = soma2 + f(a1(i))

       end do
       
       


      integral = (3*h/8.0)*(f(a1(1))+3*soma+2*soma2+f(a1(n)))

      return

end function integral






