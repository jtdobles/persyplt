# D

Authors:

- Project Manager: Lisa Jeong (dj2570)
- Sysyem Architect: Leeah Yoo (ly2497)
- Language Guru: Tiffany Neumann (tjn2124)
- Tester: Javier Dobles (jd3626)

### How To Use

#### Build the D compiler

```
ocamlbuild -pkgs llvm d.native
```

#### Run the D compiler and generate LLVM code

```
./d.native -l example.mc > example.out
```

#### Run the LLVM code

```
lli example.out
```

if your machine displays err: "lli not found," try these commands in terminal and try again

```
echo 'export PATH="/usr/local/opt/llvm/bin:$PATH"' >> ~/.bash_profile
export LDFLAGS="-L/usr/local/opt/llvm/lib" >> ~/.bash_profile
export CPPFLAGS="-I/usr/local/opt/llvm/include" >> ~/.bash_profile
source ~/.bash_profile
```

# `0` Contents

1. [Introduction](#1-introduction)
2. [Lexical Conventions](#2-lexical-conventions)
   1. [Comments](#21-comments)
   2. [Identifiers](#22-identifiers)
   3. [Reserved Keywords](#23-reserved-keywords)
   4. [Scoping](#24-scoping)
3. [Data Types](#3-data-types)
   1. [Primitive Types](#31-primitive-data-types)
   1. [int](#311-int)
   1. [float](#312-float)
   1. [string](#313-string)
   1. [bool](#314-bool)
   1. [var](#315-var)
   1. [none](#316-none)
   1. [Non-primitive Types](#32-non-primitive-types)
   1. [array](#321-array)
   1. [tuple](#322-tuple)
   1. [struct](#323-struct)
   1. [Type Qualifiers](#33-type-qualifiers)
   1. [const](#331-const)
   1. [static](#332-static)
   1. [Type Cast](#34-type-cast)
4. [Operators](#4-operators)
   1. [Unary Operators](#41-unary-operators)
   1. [Not Operator](#411-not)
   1. [Increment Operator](#412-increment)
   1. [Decrement Operator](#413-decrement)
   1. [Binary Operators](#42-binary-operators)
   1. [The Addition Operator](#421-the-addition-operator)
   1. [The Subtraction Operator](#422-the-subtraction-operator)
   1. [The Multiplicative Operator](#423-the-multiplicative-operator)
   1. [The Division Operator](#424-the-division-operator)
   1. [The Modulus Operator](#425-the-modulus-operator)
   1. [Comparative Operators](#43-comparative-operators)
   1. [The Equals Operator](#431-the-equals-operator)
   1. [The Not Equals Operator](#432-the-not-equals-operator)
   1. [The Greater Than Operator](#433-the-greater-than-operator)
   1. [The Greater Than Or Equal To Operator](#434-the-greater-than-or-equal-to-operator)
   1. [The Less Than Operator](#435-the-less-than-operator)
   1. [The Less Than Or Equal To Operator](#436-the-less-than-or-equal-to-operator)
   1. [Logical Operators](#44-logical-operators)
   1. [The AND Operator](#441-the-and-operator)
   1. [The OR Operator](#442-the-or-operator)
   1. [Variable Operators](#45-variable-operators)
   1. [The Assignment Operator](#451-the-assignment-operator)
   1. [Varaible Add](#542-variable-add)
   1. [Variable Subtract](#543-varaible-subtract)
   1. [Variable Multiply](#544-variable-multiply)
   1. [Variable Divide](#545-variable-divide)
   1. [Variable Modulo]($546-variable-modulo)
   1. [Precedence of Operators](#46-precedence-of-operators)
   1. [Unary Operators](#461-unary-operators)
   1. [Binary Operators](#462-binary-operators)
   1. [Comparative Operators](#463-comparative-operators)
   1. [Logical Operators](#464-logical-operators)
   1. [Variable Operators](#465-variable-operators)
5. [Statements and Expressions](#5-statements-and-expressions)
   1. [Declarations](#51-declarations)
   2. [Literal Expressions](#52-literal-expressions)
   3. [Integer Literals](#521-integer-literals)
   4. [Float Literals](#522-float-literals)
   5. [String Literals](#523-string-literals)
   6. [Boolean Literals](#524-boolean-literals)
   7. [None Literals](#525-none-literals)
   8. [Array Expressions](#53-array-expressions)
   9. [Tuple Expressions](#54-tuple-expressions)
   10. [Struct Expressions](#55-struct-expressions)
   11. [Function Calls](#56-function-calls)
   12. [Function Structure](#561-function-structure)
   13. [Nested Functions](#562-nested-functions)
   14. [Return Statement](#57-return-statement)
   15. [Control Flow](#58-control-flow)
   16. [if](#581-if)
   17. [if/else if/else](#582-if/else-if/else)
   18. [for](#583-for)
   19. [while](#584-while)
   20. [switch/case/default](#585-switch/case/default)
   21. [continue](#586-continue)
   22. [break](#587-break)
6. [Scope](#6-scope)
7. [Memory and Pointers](#7-memory-and-pointers)
8. [Standard Library](#8-standard-library)
   1. [Arrays](#81-arrays)
   1. [append()](<#811-append()>)
   1. [prepend()](<#812-prepend()>)
   1. [remove()](<#813-remove()>)
   1. [join()](<#814-join()>)
   1. [Strings](#82-strings)
   1. [toupper()](<#821-toupper()>)
   1. [tolower()](<#822-tolower()>)
   1. [Miscellaneous](#83-miscellaneous)
   1. [leng()](<#831-leng()>)
   1. [printf()](<#832-printf()>)
   1. [fopen()](<#833-fopen()>)
   1. [fread()](<#834-fread()>)
   1. [fwrite()](<#835-fwrite()>)
   1. [fclose()](<#836-fclose()>)
9. [Sample Code](#9-sample-code)

# `1` Introduction

D is a simplified functional language based on C. D adopts C’s procedural fundamentals while eliminating complex concepts of C like memory allocation, which will minimize the burden of learning C. D can be used as a general programming language. But the intention focuses on providing a more intuitive learning experience with C. D uses LLVM as a compiler IR.

[↩️ Back to Contents](#0-contents)

# `2` Lexical Conventions

There are five kinds of tokens: identifiers, keywords, constants, operators, and separators. Usually, white space such as tabs, newlines, blanks, and comments are ignored, but sometimes they are required to separate tokens.

## `2.1` Comments

D supports two types of commenting; single-line and multi-line comments.

```c
// single-line comments

/* multi-line
   comments
*/
```

## `2.2` Identifiers

An identifier is a sequence of characters used for naming variables, functions, and new data types. Letters, digits, and the underscore can be included in identifiers. The first character of an identifier cannot be a digit. Uppercase letters and lowercase letter are distinct, so "average" and "AVERAGE" are two different identifiers. An identifier cannot be the same as a reserved keyword.

Grammar:

```c
let letter = ['a' - 'z' 'A' - 'Z']
let digit = ['0' - '9']
let id = letter (letter | digit | '_')*
```

Valid identifiers:

```c
hello_world
hiThere
h13z4Tg
```

Invalid identifiers:

```c
Z*x
--+5!a
123abc
```

## `2.3` Reserved Keywords

Keywords are special identifiers reserved for use as part of the programming language.

```c
//control flow
if else for while switch case default return continue break

//function and types
f int float bool string var none static const array tuple struct

//operators and literals
and or not true false
```

## `2.4` Scoping

D uses a pair of opening and closing curly brackets to indicate a scope of statements for function definitions and control-flow. All statements within the scope must be followed by a semi-colon to mark the end of statement, but it's not required in indentation, which indicates the nested-scoping of statements. Also, if the statements or functions have been enclosed with closing curly bracket, semi-colon is not required. The parameters of the control-flow statement followed by the keyword must use parenthesis.

Bracket scope example:

```c
 f int foo() {
     printf("Hello World\n");
 }

 even = 0;
 for (int i = 0; i < 10; i++){
     if (even % 2 == 0){
         even += 1;
     }
 }
```

[↩️ Back to Contents](#0-contents)

# `3` Data Types

## `3.1` Primitive Types

### `3.1.1` int

`int`s represent signed integer values. In our language D, the integer date type is in 32 bits. The integer data type is used for storing whole number values. It can hold integer values in the range of -2<sup>31</sup> to 2<sup>31</sup> - 1. Negative integer values must be defined with a preceding minus (-) symbol, whereas positive integer values cannot be defined with a preceding plus (+) symbol.

```c
int a = 2;
int b = -2;
int c = +5; //invalid
```

### `3.1.2` float

`float`s represent signed floating-point numbers. To define a float, there should be at least one digit preceding a decimal point and at least one digit must follow after.

```c
float b = 0.2;

//invalid floats
float x = .8;
float y = 1.;
```

### `3.1.3` string

The `string` data type represents a sequence of characters surrounded by either single quotes `' '` or double quotes `" "`.

```c
string a = "D is cool";
string b = 'hello World!';
```

Our language support string as a data type, meaning that a single character is considered as an array of characters with length one.

### `3.1.4` bool

The `bool` data type represents either `true` or `false`.

```c
bool a = false;
```

### `3.1.5` var

TBD; drop this method if we don't have enough time to implement

### `3.1.6` none

`none` is D's `null` value. D predefines C standard's NULL implementations "#define NULL 0" for users using `none`. `None` can be used to initialize other data types or valid return value for function calls, regardless of the return types that's been expected to return. Functions with no return values will be declared with type `none`.

## `3.2` Non-primitive Data Types

### `3.2.1` Array

D supports arrays of any data type. An array can be defined by either specifying a data type or using var, followed by at least one set of square brackets. Multi-dimensional arrays can be defined by adding a set of square brackets to the 1-dimensional array definition.

Grammar:

```c
LBRACK expr_list RBRACK { ArrayLit(List.rev $2)} //array creation
expr LBRACK expr RBRACK  {ArrayTupleAccess($1, $3)} //array access
```

Example:

```c
// empty array
int[] a;
char[][] b;

// array size being implicitly defined by the length of literals
int[] arr = [0, 2, 4, 6];
string[] ar = ["hi", "hello", "sup"];
```

An array can be accessed and modified with indices in square brackets, which it must be integers within the range [0, arrayLength - 1).

### `3.2.2` Tuple

A tuple is a immutable set of data. It can store different data types in a variable.

Grammar:

```c
LPAREN RPAREN {TupleLit([])}
LPAREN tuple_list RPAREN {TupleLit(List.rev $2)}
expr LBRACK expr RBRACK  {ArrayTupleAccess($1, $3)}
```

Example1:

```c
(int, float, int) tupl = (1, 2.0, 3);
(int, float, bool) a = (4, 4.5, true);
```

Similar to array, tuple's element can be accessed by passing indices in a parenthesis.
Example2:

```c
(int, float, int) tupl = (1, 2.0, 3);
float a = tupl(1); //set a to 2.0
```

### `3.2.3` Struct

A struct is a programmer-defined data type, and it contains data fields with variables.

Grammar:

```c
ID {StructT($1)} //struct type
STRUCT ID LBRACE structs RBRACE {StructDef($2, $4)
ID LBRACE structs RBRACE {StructCreate($1, $3)}
expr DOT ID {StructAccess($1, $3)}
```

Example:

```c
struct Student {
	name: string;
	age: int;
    major: string;
}

f main(){
    struct student_info = {name: "Adam", age: 15, major: "Computer Science"}

    printf("Student’s name is {}, age is {}, and the major is {}",
        student_info.name, student_info.age, student_info.major)
}
/* main will print:
   Student's name is Adam, age is 15, and the major is Computer Science
*/
```

## `3.3` Type Qualifiers

### `3.3.1` const

The value of variables that is assigned with the const qualifier does not change.

Grammar:

```c
CONST ID ASSIGN expr {Constant($4, $6)}
```

Example:

```c
const x = 10;
x = 11; // error: since we declared x as a constant
```

### `3.3.2` static

The static qualifier indicates that the variable functions as a global variable, meaning that its value will get preserved even after it's out of its scope, not initialized again in the new scope.

Grammar:

```c
STATIC ID ASSIGN expr {Static($4, $6)}
```

Example:

```c
f int foo {
    static int a = 0;
    a++;
    return a;
}

f int fooo {
    int b = 10;
    b++;
    return b;
}

f int main {
    //printf with foo() will print 1 2
    printf("%d ", foo());
    printf("%d ", foo());

    //printf with fooo() will print 11 11
    printf("%d ", fooo());
    printf("%d ", fooo());
    return var;
}
```

## `3.4` Type Cast

A type cast can be used for changing an expression to be of a specific data type. It consists of a data type specifier with parentheses, followed by an expression. D supports string, float, and int type specifiers for type casting.

Grammar:

```c
LPAREN typ RPAREN expr {Cast($1, $3)} //(str) 42
```

Example:

```c
int a = (str) 42; // a casts to "42"
float b = (int) 8.5; // b casts to 8
```

[↩️ Back to Contents](#0-contents)

# `4` Operators

## `4.1` Unary Operators

All unary operators are evaluated from left to right.

Grammar:

```c
expr:
  NOT expr   {Unop(Not, $2)}
  | expr INCREMENT   {Unop($1, Add, Int 1)}
  | expr DECREMENT   {Unop($1, Sub, Int 1)}

```

### `4.1.1` Not Operator

Negation, `!`, must be used on the leftmost side of an operands. Placing a negation operator before an expression returns its truth value in the form of a boolean value.

```c
f main () {
     int a = 0;
     int b = 0;
     return a != b;
}
/*returns:*/
false
```

### `4.1.2` Increment Operator

Urinary increment operator, `++`, can be placed after an integer to increase the nearest operand by one. The return value matches the type that was incremented namely an int or float.

```c
f foo () {
    int a = 0;
	a++;
	return a;
}
//returns 1
```

### `4.1.3` Decrement Operator

Similar to increment operator, decrement operator, `--`, can be placed after an integer to decrease the nearest operand by one.

```c
 f foo () {
    int a = 2;
 	a--;
 	return a;
 }
    //returns 1
```

## `4.2` Binary Operators

Binary operators act on two values, which are used for arithmetic integer division resulting in a truncated or discarded fractional part.

Grammar:

```c
expr:
  expr PLUS   expr {Binop($1, Add, $3)}
   | expr MINUS  expr {Binop($1, Sub, $3)}
   | expr MULT   expr {Binop($1, Mult, $3)}
   | expr DIVIDE expr {Binop($1, Div, $3)}
   | expr MODULO expr {Binop($1, Mod, $3)}
```

### `4.2.1` The Addition Operator

The addition operator, `+`, acts like addition in mathematics.

```c
int x = 2;
int y = 3;
printf(x + y);
//prints 5
```

### `4.2.2` The Subtraction Operator

The subtraction operator, `-`, acts like subtraction in mathematics.

```c
int x = 3;
int y = 2;
printf(x - y);
//prints 1
```

### `4.2.3` The Multiplication Operator

The multiplication operator, `*`, acts like multiplication in mathematics.

```c
int x = 3;
int y = 3;
printf(x * y);
//prints 9
```

### `4.2.4` The Division Operator

The division operator, `/`, acts like division in mathematics.

```c
int x = 4;
int y = 2;
printf(x / y);
//prints 2
```

### `4.2.5` The Modulus Operator

The modulo operator, `%`, acts like modulo in mathematics.

```c
int x = 3;
int y = 2;
printf(x % y);
//prints 1
```

## `4.3` Comparative Operators

Comparative operators compare two values and return a boolean value.

Grammar:

```c
expr:
  expr EQUAL  expr {Binop($1, Equal, $3)}
   | expr NEQUAL expr {Binop($1, Neq, $3)}
   | expr LESS   expr {Binop($1, Less, $3)}
   | expr LEQUAL expr {Binop($1, Leq, $3)}
   | expr GREAT  expr {Binop($1, Greater, $3)}
   | expr GEQUAL expr {Binop($1, Geq $3)}
```

### `4.3.1` The Equals Operator

The equals operator, `==`, returns true if two values are identical. If not, it returns false.

```c
int x = 3;
int y = 2;
printf(x == y);
//prints false
```

### `4.3.2` The Not Equals Operator

The not equals operator, `!=`, returns true if two values are not identical. If both values are the same, it returns false.

```c
int x = 3;
int y = 2;
printf(x != y);
//prints true
```

### `4.3.3` The Greater Than Operator

The greater than operator, `>`, returns true when the first value is greater than the second value. Otherwise, it returns false.

```c
int x = 3;
int y = 2;
printf(x > y);
//prints true
```

### `4.3.4` The Greater Than Or Equal To Operator

The greater than or equal to operator, `>=`, returns true when the first value is greater than or equals to the second value. Otherwise, it returns false.

```c
int x = 1;
int y = 2;
printf(x >= y);
//prints false
```

### `4.3.5` The Less Than Operator

The less than operator, `<`, returns false if the first value is greater than or equal to the second value. Otherwise, it returns true.

```c
int x = 3;
int y = 5;
printf(x < y);
//prints true
```

### `4.3.6` The Less Than Or Equal To Operator

The less than equal to operator, `<=`, returns false if the first value is greater than the second value. Otherwise, it returns true.

```c
int x = 3;
int y = 3;
printf(x <= y);
//prints true
```

## `4.4` Logical Operators

The logical operators take two boolean values and return a boolean value based on its operation.

Grammar:

```c
expr:
  expr AND    expr {Binop($1, And, $3)}
   | expr OR   expr {Binop($1, Or, $3)}
```

### `4.4.1` The AND Operator

The `AND` operator returns true if both variables are true. Otherwise, it returns false.

```c
bool x = true;
bool y = true;
if (x && y) {
    printf("Condition is true\n");
}
else {
    printf("Condition is false\n");
}
//prints Condition is true

bool a = true;
bool b = false;
if (a && b) {
    printf("Condition is true\n");
}
else {
    printf("Condition is false\n");
}
//prints Condition is false
```

### `4.4.2` The OR Operator

The `OR` operator returns false if both variables are false. Otherwise, it returns true.

```c
bool x = true;
bool y = false;
if (x || b) {
    printf("Condition is true\n");
}
else {
    printf("Condition is false\n");
}
//prints Condition is true

bool a = false;
bool b = false;
if (x || b) {
    printf("Condition is true\n");
}
else {
    printf("Condition is false\n");
}
//prints Condition is false
```

## `4.5` Variable Operators

Variable operators interact with a variable and integer.

### `4.5.1` The Assignment Operator

The assignment operator, `=`, is used between a variable name and value that bound to the variable name. If the variable exists in code already, the value of the variable gets overwritten, otherwise, a new variable gets declared.

Grammar:

```c
VAR ID ASSIGN expr {Assign($4, $6)}
```

Example:

```c
int x = 0;
float z = 2.5;
```

### `4.5.2` Varaible Add

Variable addition, `+=`, operator is used between a variable name and an integer value, which adds to the variable value.

```c
int x = 5;
x += 2;

// x = 7
```

### `4.5.3` Variable Subtract

Variable subtraction, `-=`, operator is used between a variable name and an integer value, which subtracts the integer value from the variable value.

```c
int x = 5;
x -= 2;

// x = 3
```

### `4.5.4` Variable Multiply

Variable multiplication, `*=`, operator is used between a variable name and an integer value, which multiplies the variable value with the integer value.

```c
int x = 5;
x *= 2;

// x = 10
```

### `4.5.5` Variable Divide

Variable division, `/=`, operator is used between a variable name and an integer value, which divides the variable value with the integer value.

```c
int x = 10;
x /= 2;

// x = 5
```

### `4.5.6` Variable Modulo

Variable modulo, `%=`, is used between a variable name and an integer value, which divides the variable value with the integer value and returns the remainder of it.

```c
int x = 10;
x %= 2;

// x = 0
```

## `4.6` Precedence of Operators

The precedence of operator in D is identical to C. General notion is that any expression that wrapped in parentheses or followed by postfix always have the highest precedence.

### `4.6.1` Unary Operators

Unary operators get the second precedence.

### `4.6.2` Binary Operators

Binary operators precedence follows mathematical rules: the multiplicative operator, division operator, and modulus operator have a higher precedence than the additive operator and the subtraction operator. All binary operators are left associative.

### `4.6.3` Comparative Operators

The `>, >=, <, <=` operators have higher precedence than the `==` and `!=` operators.

### `4.6.4` Logical Operators

The `AND` operator has higher precedence than the `OR` operator.

### `4.6.5` Variable Operators

The variable operators have a lower precedence than binary operators and are right associative. Lastly, comma operator gets the lowest precedence.

[↩️ Back to Contents](#0-contents)

# `5` Statements and Expressions

## `5.1` Declarations

Variables are declared through variable statements. When declaring a variable, variable type and its initialization is must.

```c
int x = 10;
// x = 10
```

## `5.2` Literal Expressions

Literal Expressions are a singular instance of a literal. Because the language combines characters and strings into one type, `‘’` and `“”` may be used interchangeably for string and character literals.

### `5.2.1` Integer Literals

Int literals represent a whole decimal number and always take on the int type.

```c
0
245
-2832
```

### `5.2.2` Float Literals

Float literals represent a decimal floating number and always take on the float type.

```c
0.5
12.3457
-2.155
```

### `5.2.3` String Literals

String literals are a sequence of characters. D allows both single quotation and double quotation marks for string literals and it takes string type.

```c
"CS is fun"
'PLT is hard'
```

### `5.2.4` Boolean Literals

Boolean literals represent the truth value of an expression and takes the bool data type. Instead of using `0` and `1`, D takes string forms, `true` and `false` to represent the bool literals.

### `5.2.5` None Literals

The none literal represents a reference to a null value and always takes on the none type.

## `5.3` Array Expressions

An array expression consists of zero or more expressions of matching type, separated by commas and enclosed in square brackets `[]`.

```c
[ ]	// empty array
["Example", 'of', 'String', "Array"]	// string array
[1.2, 3.4, 5.6]  // float array
```

The language also accepts array expressions constructed using the _ operator, using the form, `[array_expression] _ [integer_expression]`.

```c
[1.2, 3.4, 5.6] * 3 = [1.2, 3.4, 5.6, 1.2, 3.4, 5.6, 1.2, 3.4, 5.6];
```

Arrays can also be constructed by combining two arrays of the same type using the + operator or by using join() function.

```c
[1,2,3,4] + [5,6] = [1,2,3,4,5,6];
```

## `5.4` Tuple Expressions

A tuple is an expression that consists of zero or more expressions of any type, separated by commas and enclosed in parentheses.

```c
(1.2, 300, "Jeffrey");
// A tuple of type (float expression, integer expression, string expression)
```

## `5.5` Struct Expressions

A struct is declared using the keyword struct, a name for the new struct, and field values that the struct will contain/track. The field values can have any valid type and are separated by commas.

```c
struct struct_name{
	struct_field1: field_type1,
	struct_field2: field_type2, ... ,
	struct_fieldn: field_typen
}
```

Using the given syntax, we can create a new struc student_record like this:

```c
struct student_record{
	name: string,
      age: integer,
	grad: bool
}
```

A student_record would then be declared like this:

```c
struct bob = {name: "Bob Roberts", age: 20, grad: false};
```

## `5.6` Function Calls

### `5.6.1` Function Structure

Functions consist of a function name, a set of parameters, and a sequence of expressions. The keyword to indicate a function definition is `f`.

Grammar:

```c
fdecl: /* f int foo(args){...} */
    FUNCTION typ ID LPAREN params RPAREN LBRACE stmt_list RBRACE
    {
       outputType = $7;
       funcName = $2;
       args = $4;
       body = List.rev $9
    }

    //Function type
    FUNCTION typ LPAREN func_input RPAREN
    {
       FunctionT{
           input: $3;
           output: $6
       }
    }

    //Function Call
    ID typ LPAREN args RPAREN { Call($1, $3)}

```

Example:

```c
f int hello_world() {
   printf("Hello World!");
}

The function would then be called like this:
hello_world();	// This line would print "Hello World!" to stdout
```

### `5.6.2` Nested Functions

D supports nested functions. Similar to Javascript's nested function, D's nested function works like this:

1. Write one function inside another function.
2. Make a call to the inner function in the return statement of the outer function
3. Call it f(a)(b) where a is a parameter to outer b is to the inner function.
4. Return the combined output from the nested function

```c
f int addNum(int a, int b) {
   f none printSum(string txt) {
     printf(txt);
   }
   int res = a + b;
   printSum(res);
}

addNum(2,3);
//addNum returns 5
```

## `5.7` Return Statement

A return statement is used in a function to pass values and control back to its caller function. Functions do not use explicit typing so return statements can pass any type of data.

```c
return expression;
```

## `5.8` Control Flow

### `5.8.1` if

The expression must result in a boolean value of either true or false. If the expression within parentheses evaluates to true, the statement block within its scope will be run.

### `5.8.2` if/else if/else

The if statement has optional statements that can get accompanied with, which are `else if` and `else`. `Else if` will be run if the the previous `if` statement's boolean value gets evaluated to false. Similar to `if` statement, `else if` statement aslo takes boolean value in parentheses and if the boolean value evaluates to true, the statement within its scope will be run. User can have as many as `else if` statements the user wants after an if statement. The `else` statement must come aftere the `if` and all `else if` statements, if any. The `else` statement will run the statements within its scope if all the `if` statements and `else if` statements returns boolean value of false.

Grammar:

```c
stmt:
    IF LPAREN expr RPAREN stmt %prec NOELSE {If($3, $5, [])}
  | IF LPAREN expr RPAREN stmt ELSE stmt {If($3, $5, $7)}
```

Example:

```c
if(a > b){
   printf(a);
}
…
else if (a < b){
   printf(b);
}
else {
   printf("a and b are the same");
}
```

### `5.8.3` for

A `for` statement is also used to repeatedly run the same block of code but allows for different methods of control. A `for` statement takes in an argument in the form: (assignment; condition; iterator), followed by statement list within its scope. The assignment part creates a variable and initializes it to a given number, mostly integer. The condition is a boolean expression; if it returns true, the statement list within the `for` statement scope runs. The iterator changes assigned variable in the assignment part based on stated expression. Once iterator updates assigned variable's value, the condition checks the new value and if it returns true, the statement runs again, otherwise, the program exit out of for statement's scope.

Grammar:

```c
FOR LPAREN ID expr SEMI ID expr SEMI ID expr RPAREN LBRACE stmt RBRACE {For($2, $4, $6, $8, $10)}
```

Example:

```c
for(int i = 0; i < 10; i++){
    printf(“PLT”); //print “PLT” 10 times
}
```

### `5.8.4` while

A `while` statement is used to run the same block of code repeatedly while a condition remains true. If the boolean expression returns truem the statements within its scope runs. After all statements are run, the boolean expression gets evaluated again, which if true then we re-run statements, else, the program exist out of while's scope. This process repeats until the boolean expression returns false.

Grammar:

```c
WHILE LPAREN expr RPAREN LBRACE stmt RBRACE {While($3, $5)}
```

Example:

```c
int i = 0;
While (i < 5){ /* print “Hello World!” 5 times.
    printf(“Hello World!”);
    i++;
}
```

### `5.8.5` switch/case/default

The switch statement is used to test a variable for equality against a list of values, and to execute statements based on which value the variable is equal to. The different possible values are declared using the case statement, and there is also the option to have a default statement that will execute if no value is equal to the variable.

Grammar:

```c
SWITCH LPAREN expr RPAREN stmt_list {Switch($3, $5)}
```

Example:

```c
string grade = 'C';

switch(a) {
   case 'A':
      printf("Excellent");
      break;
   case 'B':
      printf("Well done");
      break;
   case 'C':
   case 'D':
      printf("Passed");
      break;
   default:
      printf("Invalid grade");
}
```

### `5.8.6` continue

The `continue` statement is used in `for` or `while` statements scope. When the program encounters `continue`, it will force the program to run the next iteration of the loop to take place, skipping any code inbetween.

```c
int a = 10;
while(a < 16){
   if(a==12){
      a += 1;
      continue;
   }
   printf(a);
   a++;
} -> returns:
10
11
12
13
14
15
```

### `5.8.7` break

The break statement is used to immediately exit a control loop, either while or for, while the continue statement is used to enter the next iteration of that loop. In nested loops, the break statement will terminate the smallest/closest enclosing statement.

```c
int ar[] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
for (int i = 0; i < leng(ar); i++){
   if(ar[i] == 2){
      print("Found 2");
      break;
   }
}
```

[↩️ Back to Contents](#0-contents)

# `6` Scope

As this LRM stated in the previous section "Scoping," D uses curly brackets to define scope. For example, a for look can be written in several ways:

```c
for (int i = 0; i < 10; i++){
   ...
}

// is the same as:
for (int i = 0; i < 10; i++)
{
   ...
}

// is the same as:
for (int i = 0; i < 10; i++)
{...}
```

These different notations will function in the same manner as regular function definitions, conditionals, etc.

[↩️ Back to Contents](#0-contents)

# `7` Memory and Pointers

D will allow for memory pointers and memory referencing. The `*` operator can be placed on the left of an operand and assigns a memory address to the given operand. The & operator can be used to retrieve the address of a memory location of a variable that was previously assigned a location in memory and the result is an int. When a variable is assigned to a memory location and the variable goes out of scope that variable’s memory location is freed and can be used by a new resource. The variable assigned to a resource must be a valid primitive or non-primitive data type or will it result in an error.

D allows for resources to be mapped to a single variable. We can use the & operator to create a copy of a variable which will be mapped to a new resource in memory. If a variable that has been copied is taken out of scope and freed, that variable's copy is still a valid copy of the original variable as long as the copy is in scope. In other words, once a variable copies the value from a resource it is no longer dependent on the original resource. Declaring a data type without assignment will result in an error.

Grammar:

```c
expr:
  MULT expr %prec DEREF {Unop(Deref, $2)} //deref *
  | REF expr {Unop(Borrow(Immut), $2)} //ref &
```

[↩️ Back to Contents](#0-contents)

# `8` Standard Library

D's standard library includes methods and functionalities that are presented in many other languages.

## `8.1` Arrays

### `8.1.1` append()

`append()` adds an input elemeent to the end of a speicified array. Because arrays have fixed sizes, the original array remains unmodified, and `append()` returns a new array with the input element added at the end. The input type must match the type of the array.

```c
int[] ar = [11, 22, 33, 44];
int[] ar_new = ar.append(55);
// ar_new: [11, 22, 33, 44, 55]
```

### `8.1.2` prepend()

`prepend()` works in the same mannar as `append()`, but it adds the element to the front of the array.

```c
int[] ar = [22, 33, 44];
int[] ar_new = ar.prepend(11);
// ar_new: [11, 22, 33, 44]
```

### `8.1.3` remove()

`remove()` takes int index value and removes the element at the given index in the array. If the array has no such index, it will throw error. Because arrays have fixed sizes, the original array remains unmodified, and `remove()` returns a new array with the specified input element removed.

```c
string[] arr = ["New York", "Boston", "Raleigh", "Cupertino"];
string[] arr_new = arr.remove(1);
// arr_new = ["New York", "Raleigh", "Cupertino"]
```

### `8.1.4` join()

`join()` takes one list as an input and appends it to the other list that join() called on. Because arrays have fixed sizes, the original list remains unmodified, and `join()` returns a new list with the elements of the second array appended to the elements of the first. The type of both arrays must match to execute `join()`, if not, it will flag error.

```c
int[] ar_1 = [1, 2, 3];
int[] ar_2 = [4, 5, 6];
int[] ar_12 = ar_1.join(ar_2);
// ar_12 = [1, 2, 3, 4, 5, 6]
```

## `8.2` Strings

### `8.2.1` toupper()

The `toupper()` function converts lowercase alphabets to uppercase alphabets. When an uppercase alphabet is passed into this method, it will return the same alphabet.

```c
string c = 'ab';
print(toupper(c));
//prints AB
```

### `8.2.2` tolower()

The `tolower()` function converts uppercase alphabets to lowercase alphabets. When an lowercase alphabet is passed into this method, it will return the same alphabet.

```c
string c = 'AbC';
print(toupper(c));
//prints abc
```

## `8.3` Miscellaneous

### `8.3.1` leng()

The `leng()` function returns the int size of strings, arrays, and tuples. For strings, `leng()` returns the number of characters in the string, excluding the null terminator at the end of its underlying array of chars. For arrays, `leng()` returns the number of elements in the array, and for tuples, `leng()` returns the number of elements in the tuple.

```c
int str_length = leng("Hello World!"); //str_length=12
int arr_length = leng({1,2,3,4}); //arr_length=4
(int, int, int) tupl = (1,2,3);
int tupl_length = leng(tupl); //tupl_length=3
```

### `8.3.2` printf()

The `printf()` prints the given string to a new line of standard output. It throws errors if it encounters a type other than string, and prints empty new line if there's no input given. To print non-string type values, you can type cast desired value to string.

```c
printf("Hello World!");
printf();
a = (str) 10;
printf(a);
```

stdout:

```c
Hello World!

10
```

### `8.3.3` fopen()

TBD; drop this method if we don't have enough time to implement

### `8.3.4` fread()

TBD; drop this method if we don't have enough time to implement

### `8.3.5` fwrite()

TBD; drop this method if we don't have enough time to implement

### `8.3.6` fclose()

TBD; drop this method if we don't have enough time to implement

[↩️ Back to Contents](#0-contents)

# `9` Sample Code

TBD

[↩️ Back to Contents](#0-contents)
