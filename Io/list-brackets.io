squareBrackets := method(
        l := List clone
        call message arguments foreach(arg,
                l append(doMessage(arg))))

test := [1,2,[3,4]] println