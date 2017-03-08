
(*********************************
here is a program with free variable z 

cps transformation
defunctionalize to remove all higher order functions 
no function takes a function as an argument
no function returns a function as result
no function is anonymous 
all functions have explicit name

z comes from (f 1)   z is 1 here
z comes from (f 10)  z is 10 here

**********************************)


fun aux f = (f 1) + (f 10);

fun main x y b =
  aux ( fn z => x + z ) *
  aux ( fn z => if b then y + z else y - z ) ;

fun opt x y b =
  let val xy4 = 4*x*y
      val x22 = 22*x
      val y22 = 22*y			 
      val s11 = 11*11	 
  in
      if b
      then xy4 + x22 + y22 + s11
      else xy4 + ~x22 + y22 + s11
  end

      

      

  



					    
