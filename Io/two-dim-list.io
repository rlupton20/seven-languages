TwoDimArray := List clone

List replicate := method(n, v, a := call target clone;
        for(i, 1, n, a append(v clone)); a)

dim := method(x,y,
                t := TwoDimArray clone;
                t := TwoDimArray replicate(x, List replicate(y,0));
                t)

dim(3,4) println

TwoDimArray set := method(x,y,value, self at(x) atPut(y, value); self)
TwoDimArray get := method(x,y, self at(x) at(y))

dim(3,4) set(1,1,"x") println
dim(3,4) set(1,1,"x") get(1,1) println

TwoDimArray transpose := method(
        if(self size == 0, dim(0,0),
                rows := self size;
                columns := self at(0) size;
                new := dim(columns, rows);
                for(i, 0, rows - 1,
                        for(j, 0, columns - 1,
                                new set(j,i, self get(i,j))))))

dim(3,4) set(2,1,"x") println
dim(3,4) set(2,1,"x") transpose println

TwoDimArray writeToFile := method(filename,
        File with(filename) openForUpdating write(self serialized) close)

TwoDimArray fromFile := method(filename,
        data := doString(File with(filename) openForReading contents);
        rows := data size;
        if(rows == 0, dim(0,0),
                a := dim(rows, data at(0) size)
                for(i, 0, rows - 1,
                        a atPut(i, data at(i));)
                a))


dim(3,4) set(2,1,"x") transpose writeToFile("test.mat")
TwoDimArray fromFile("test.mat") println