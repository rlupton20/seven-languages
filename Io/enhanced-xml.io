Builder := Object clone
Builder indent := 0
Builder tab := method(
        for(i, 0, self indent, " " print))

Builder forward := method(
        self tab; writeln("<", call message name, ">")
        self indent = self indent + 2
        call message arguments foreach(
                arg,
                content := self doMessage(arg);
                if(content type == "Sequence", self tab; writeln(content)))
        self indent = self indent - 2
        self tab; writeln("</", call message name, ">"))

Builder ul(
        li("Io"),
        li("Lua"),
        li("Javascript"))