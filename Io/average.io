List myAverage := method(if(self size == 0, 0, self sum / self size))

list(1,2,3) myAverage println
list() myAverage println


// Alternative implementation using tuple prototype
Tuple := Object clone
Tuple one := 0
Tuple two := 0
tuple := method(a,b, t := Tuple clone; t one = a; t two = b; t)
Tuple + := method(t, tuple(self one + t one, self two + t two))


List myAverage = method( if(self size == 0, 0,
                           i := tuple(0,0);
                           t := self foreach(v, if(v type == "Number",
                                   i = i + tuple(v,1),
                                   Exception raise("Not a number!")));
                           t one / t two))

list(1,2,3) myAverage println
list() myAverage println
list(1, "foo") myAverage println