

!Exercício 1)

!Usarei aqui o Método de Newton 




include "tudo"

program main

    use tudo, only: MetNewton
    
! tol_ini é a tolerancia maxima

    implicit none 
    
    real(8):: t, tol_ini=0.01, g=9.805, k=0.1473, m=0.113
    
    t=0.2 !Conforme o Método de Newton, devemos começar com um chute da solucao 
    
    write(*,*) 'Resoluçao de Função: f(t)=0'
    
    
    write(*,*) '----------------------------------------------'
    write(*,*) '--------------Método Newton-------------------'
    write(*,*) 'Equação: f(t)=yo-(mg/k)t+(1/k^2)(((m^2)g(1-exp(-k/m)t))=0'
    write(*, '(a, 1x, f13.3)') ' g = ',  g
    write(*, '(a, 1x, f13.4)') ' k = ',  k
    write(*, '(a, 1x, f13.3)') ' m = ',  m
    write(*, '(a, 1x, f13.3)') ' Tolerancia maxima = ',  tol_ini
    write(*, '(a, 1x, f13.3)') ' Chute do valor inicial da solucao: ',  t
    write(*,*) '----------------------------------------------'
    write(*,*) 'Tempo, t em segundos,  que o objeto leva pra atingir o solo->'
    call MetNewton(tol_ini, t)
    write(*,*) '--------------Fim: Newton---------------------'
     
   
end program main



!-------------------------------------------------------

real function F(t) ! (1) Função que será usada para o método de newton

      		implicit none
      		
      		real(8)::t, g=9.805, y0=91.0, k=0.1473, m=0.113
      
      		F = y0-((m*g)/k)*t+(((m**2)*g)/k**2)*(1-exp(-(k/m)*t)) !(função não linear)
      
      		return
      		
end function F

real function Fder(t) ! Será usada no método de newton
! Aqui é a derivada de F (A função anterior)

     implicit none
     
     real(8)::  t, g=9.805, k=0.1473, m=0.113
     
     Fder=  ((m*g*(exp(-k*t/m)))/k)-m*g/k! É a derivada 
     
     return
end function Fder


!-------------------------------------------------------

