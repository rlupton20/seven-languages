// Test file for XML attribues

writeln("Test interpreting maps: should print 1234")
{ "test" : 1234 } at("test") println
writeln

writeln("Test XML with attributes")
Builder ul(
        li({"type" : "OOP", "thing" : 4}, "Io"),
        li("Lua"),
        li("Javascript"))