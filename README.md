## Purpose
- This is a revised take at making the mips-compiler that I made around 5 months ago.
- I had made this in around 4 days during Christmas break, and I want to come back to it and make it a little something more.
- My goals for revision:
    - [x] Revise the syntax and make the language actually useable
    - [x] Write a context free grammar for the language
    - Create a treesitter/syntax highlighting grammar
    - Rewrite code generation to x86-64
    <!--- Write custom assembler for more familiar and basic instruction set of x86-64-->
    - Bootstrap compiler

## Language Features
- [ ] Strongly typed (with type-checking) & compiled
    - Parser adds declared symbols, manages scopes, and asserts that only one declaration (function or variable) appears per identifier.
    - Type checker resolves references, validates assignments and operations on resolved types.
        - Handles context-sensitive rules (location of break/continue/return statements, and forward references)
- [x] No main function entrypoint is required. Expressions outside of functions are evaluated from top to bottom.
- [ ] Panic mode recovery for parsing errors: multiple error reporting without cascading
    - Has full memory management and freeing all dynamic memory upon any errors. (Complicates the code but can be removed and simplified very easily)
- [ ] Isolated String expressions are automatically printed to stdout
- [ ] Structs but no classes
- [ ] Enums associated to integers
- [ ] Spaces only
- [ ] Inline assembly with the `asm { }` block
- [ ] Function pointers
- [ ] Implicit value returns
- [ ] Default parameters in any order
- [ ] Immutability (final in java) by default, forbidding reassignment
    - `mut` keyword upon definition for mutability
- [ ] Value model for primitive assignment and parameters
- [ ] Stack trace logs
- [ ] Operator overload for custom structs
- [ ] Conditional ranges: '0'...'9'

## Number Types
- `u0`: null, zero size type, used as void
- `u8`: unsigned 8-bits
- `u16`: unsigned 16-bits
- `u32`: unsigned 32-bits
- `u64`: unsigned 64-bits
- `i8`: signed 8-bits
- `i16`: signed 16-bits
- `i32`: signed 32-bits
- `i64`: signed 64-bits
- `f64`: 64-bit float (no 32-bit, 64 seems to be standard)

## Other Types
- There is no char type but char literals can be used (converted into ascii code and stored as `i8` integers)
- bool will be either one or zero stored as `i8`
- Strings will be allocations of chars (1 byte)

## Sample
```
################ Currently Parsable to AST ####################
# Standard variables
mut u8 foo := 4;        # mutable declaration and initialization
u8 bar := 8;            # immutable (cannot be reassigned)
foo := bar;             # copy by value re-assignment
f64 my_float;           # immutable float declaration

String str := "this is a string";
bool my_bool := true;

u8 my_char_lit := 'a'; # stored as u8 97 in ascii

# Function Declarations
# functions implicitly return whatever value is in the "returns" clause
func ADD (x: u8, y: u8) returns (z: u8)
{
    # Note that we cannot redeclare z with a type
    z := x + y;         # implicit return of z

    # it is also possible to explicitly return something with a return statement
    # if a return statement is ever reached, as long as it is of type that z is declared as
    # it will be returned.
}

# "void" function
func FOO ()
{
    print "this is a void function with no return type";
}

# Function calls
ADD(5, 2);

####################### Future Additions #######################
# Pointers
mut u8* ptr_c_to_m := &foo;

mut u8* mut ptr_m_to_m := null;
ptr_m_to_m := &foo;

u8* mut ptr_m_to_c := null;
ptr_m_to_c := &bar;


u8 (*func_ptr)(u8, u8) := &ADD;
u8 result := (*func_ptr)(2, 3);

func EXECUTE(x: u8, y: u8, f: u8 (*)(u8, u8)) returns (z: u8)
{
    z := (*f)(x, y);
}

func GET_ADDER() returns (f: u8 (*)(u8, u8))
{
    f := &ADD;
    # return  &ADD;
}
```

## Old-Implemented-Features
- [x] int and string literals
- [x] int and string variables
- [x] printing both int and strings
- [x] start program with main function and return integer
- [x] if statements with a singular condition (literals and types can be intermixed): ==, !=, <, <=, >, >=, |. No else or elseif conditionals.
- [x] string comparisons
- [x] multi-comparison conditionals with logical AND, logical OR
- [x] comments (notated with `#`, and only work when `#` is the first character of the line)
- [x] variable arithmetic (+=, -=, +, -, *, /, %, &, | on variables), notated with `(<operation> <var/literal> <var/literal>;).`
  - Note: The ending semicolon is required, and the second operand is optional for `+=` and `-=` operations.
- [x] while loops
