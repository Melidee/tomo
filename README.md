# The Tomo compiler

This is a toy compiler for a language kinda-sorta like c, heres an example:
```c
module hello;

use std.io;

fn main(): i32 {
    let message: String = say("Hello!")
    io.println(message);
    message.free();
    return 0;
}

fn say(text: String): String {
    let result: String = String.format("Hello, %s!", message);
    return result;
}
```
