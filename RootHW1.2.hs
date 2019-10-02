roots 0 0 0 = error "R"
roots 0 0 c = error "Корней нет"
roots 0 b c = (x, x)
    where
	x  = -c/b
roots a b c = (x1, x2)
    where
    d  = b^2 - 4*a*c
    sd = if (d<0) then error ("Нет действительных корней") else sqrt d  
    x1 = (-b - sd) / (2*a)
    x2 = (-b + sd) / (2*a)
   
