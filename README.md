# inp-proj-1
- Author: Tomáš Dolák 
- Login: [xdolak09](https://www.vut.cz/lide/tomas-dolak-247220)
- Email: <xdolak09@stud.fit.vutbr.cz>

### Project Overview
This project involves implementing a processor using VHDL that can execute a program written in an extended version of the BrainF*ck language. The basic version of this language uses only eight simple commands, forming a complete set capable of implementing any algorithm. To verify correct functionality, automated tests are available.

### Processor Functionality
The implemented processor will process instructions directly as 8-bit character codes. The instruction set includes 10 commands, each represented by a unique code, as listed below:

| Command | OpCode | Description                       | C Equivalent          |
|---------|--------|-----------------------------------|-----------------------|
| `>`     | 0x3E   | Increment pointer                 | `ptr += 1;`           |
| `<`     | 0x3C   | Decrement pointer                 | `ptr -= 1;`           |
| `+`     | 0x2B   | Increment value at pointer        | `*ptr += 1;`          |
| `-`     | 0x2D   | Decrement value at pointer        | `*ptr -= 1;`          |
| `[`     | 0x5B   | Jump forward to `]` if `*ptr` is 0| `while (*ptr) {`      |
| `]`     | 0x5D   | Jump back to `[` if `*ptr` is not 0| `}`                  |
| `~`     | 0x7E   | Break current loop                | `while break;`        |
| `.`     | 0x2E   | Output value at pointer           | `putchar(*ptr);`      |
| `,`     | 0x2C   | Input value to pointer            | `*ptr = getchar();`   |
| `@`     | 0x40   | Program and data separator        | `return;`             |

### Memory Model
	- The program and data share a common memory space with a capacity of 8192 8-bit cells.
	- Memory is initialized to zero.
	- Memory access uses a pointer, which can be moved left or right, with the memory being treated as a circular buffer.
 
### Execution
Execution starts at the first instruction and ends when a zero-value ASCII character is detected. Unrecognized commands are ignored, allowing for inline comments in the program.
