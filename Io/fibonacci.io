Fibonacci := Object clone

Fibonacci fib_loop := method(n,
        self f := 1;
        self g := 1;
        self tmp := 0;
        for(i , 2 , n,
                self tmp = f;
                self f = self tmp + self g;
                self g = self tmp;)
        self f)

writeln("With loop")
writeln(Fibonacci fib_loop(0))
writeln(Fibonacci fib_loop(1))
writeln(Fibonacci fib_loop(2))
writeln(Fibonacci fib_loop(5))
writeln

Fibonacci fib_rec := method(n,
        if( n == 0 or n == 1, 1,
                self fib_rec(n - 1) + self fib_rec(n - 2)))

writeln("With recursion")
writeln(Fibonacci fib_rec(0))
writeln(Fibonacci fib_rec(1))
writeln(Fibonacci fib_rec(2))
writeln(Fibonacci fib_rec(5))
writeln