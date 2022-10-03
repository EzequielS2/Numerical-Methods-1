







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

      do i=2, n

         soma = soma + f(a1(i))
         
         if(mod(i, 2)/=0 .and. i<=n-1)   then

                  soma2 = soma2 + f(a1(i))
          else if(mod(i, 2)==0) then
          
          soma = soma + f(a1(i))
          
          end if
          
       end do
       
       


      integral = (h/3.0)*(f(a1(1))+4*soma+2*soma+f(a1(n)))

      return

end function integral






