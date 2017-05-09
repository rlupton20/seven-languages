// Map interpreter
OperatorTable addAssignOperator(":", "atPutNumber")
curlyBrackets := method(
        r := Map clone
        call message arguments foreach(arg,
                r doMessage(arg)
                )
        r
)

Map atPutNumber := method(
        self atPut(
                call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""),
                call evalArgAt(1))
)


// XML builder
Builder := Object clone
Builder indent := 0
Builder tab := method(
        for(i, 0, self indent, " " print))


AttrBuilder := Object clone
AttrBuilder forward := method("")
AttrBuilder render := method(attributes,
        if(attributes type == "Map",
                init := "" asMutable
                attributes foreach(k, v, init appendSeq(" ", k, "=", v serialized))
                init,
                ""))

Builder forward := method(
        attributes := if(call argAt(0), AttrBuilder doMessage(call argAt(0)), Map clone)
        self tab; writeln("<", call message name, AttrBuilder render(attributes), ">")
        self indent = self indent + 2
        call message arguments foreach(
                arg,
                content := self doMessage(arg);
                if(content type == "Sequence", self tab; writeln(content)))
        self indent = self indent - 2
        self tab; writeln("</", call message name, ">")
)


// OperatorTable can't be updated in the same file -
// test by running another file
doRelativeFile("attribute-xml-test.io")