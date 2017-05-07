Number oldDiv := Number getSlot("/")

Number / = method(b, if( b == 0, 0,
        self oldDiv(b)))

writeln(4 / 2)
writeln(4 / 0)