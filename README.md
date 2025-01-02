`#` here follows a comment `#` must be the first non whitespace character in a line

**--------------------------------- Intro -----------------------------**

`G7th` is an objectoriented procedural programming language that took the
original minimalist stack language design of Forth as developed by Charles
(Chuck) Havice Moore in 1968 and enriched it with data types and a bunch of
functions that can work with these.  
In Forth you have eg. an `add` operator for each data type: `+` for integers,
`f+` for floats, `s+` for strings. Same is true for storing `!` `f!` `s!`, fetching
`@` `f@` `s@`, printing `.` `f.`.  
In `g7th` `+`, `!`, `@`, `.` works for all: integers, floats, strings, arrays
and matrices.  
During the development of Forth, special emphasis had to be placed on the
compactness of words due to the limited storage capacity at that time,
whereas storage space is no longer an issue these days.  
Therefore, when developing `g7th` using Rust, we refrained from programming at
the assembly level, which makes `g7th` more robust, flexible and easier to 
maintain and extend by Rust functions.  
Consequently one major design change is that `g7th` uses no memory addresses but
function pointers to work on.  
A large part of programming in `g7th` is done by manipulating the data stack
which is referred as stack.  
Following numbers get pushed from left to right on the stack.  

```
34 5 44 848 37 939
.s  			(*34 5 44 848 37 939 ---*)
```
The stack is intentionally printed upside down on the screen and from left to
right in this documentation what means that the TOS (top of stack) is printed
as the latest data over the `---` line and the rightmost here.  
So the TOS goes never out of sight on the screen even if there are many entries
on the stack.  
In `g7th`, everything is either a word (item) or a value.

**--------------------------------- Braces -------------------------------**

```
(*is a comment in line*) and can appear anywhere in a line
( -- ) Stack usage
(    ) reserved for AOL expressions
[] definition of and access to an array or a matrix
```

**------------------------------- Arithmetic -----------------------------**

All calculations expects at least one item on the data stack.

```
5 4 +                     (*9*)

`.` short for `print` - pops the TOS from the stack and prints it
9 .                       (*9*)
pi .                      (*3.14159265` the ` means the number is a BigDecimal*)

6 7 *     println         (*42*)
1234 23 - println         (*1211*)
1234 23 + println         (*1211*)
15 12 /   println         (*1.25*)
15 12 //  println         (*1*)
13 2 %    println         (*1*)

99 negate println         (*-99*)
-99 abs   println         (*99*)
52 23 max println         (*52*)
52 23 min println         (*23*)
```

Calculate and store:

```
variable foo
83.7 foo ! foo @ println
3 foo *!   foo @ println
16 foo +!  foo @ println
38 foo -!  foo @ println
```

More arithmetic functions:

```
rad                 (*Rad radians*)
pi 2/ sin . cr	    (*1*)
pi 2/ cos . cr	    (*0*)
pi tan . cr	    (*-0*)
0 sinh . cr	    (*0*)
0 cosh . cr	    (*0*)
0 tanh . cr	    (*0*)
deg                 (*Deg degrees*)
90 sin . cr	    (*1*)
90 cos . cr	    (*0*)
180 tan . cr        (*-0*)
0 tanh . cr	    (*0*)
```

**------------------------------ Data Stack ------------------------------**

Naturally, as we work with the stack, we'll need some useful methods:

```
1 dup             (*duplicates the top item:     		      1 1*)
1 2 swap          (*swaps the top with the second item:        	      2 1*)
1 2 3 rot      	  (*rotates the top 3 items:                   	      2 3 1*)
1 2 3 -rot        (*rotates the top 3 items back:                     2 3 1*)
1 2 drop       	  (*removes the top item (don't print to screen):     1*)
1 2 3 nip      	  (*removes the second item:    		      1 3*)
1 2 3 4 tuck   	  (*duplicates the top item below the second item:    1 2 4 3 4*)
1 2 3 4 over   	  (*duplicates the second item to the top:            1 2 3 4 3*)
1 2 3 4 2 roll 	  (*moves the item at given position to the top:      1 2 4 3*)
1 2 3 4 2 pick 	  (*duplicates the item at given position to the top: 1 2 3 4 2*)
```

The next manipulation methods should look vaguely familiar:

```
1 2 3 4 2dup      (*duplicates the top 2 items:    		     1 2 3 4 2 4*)
1 2 3 4 2swap     (*swaps the top 2 with the 3rd and 4th item:       3 4 1 2*)
1 2 3 4 5 6 2rot  (*rotates the top 6 items:                   	     3 4 5 6 1 2*)
1 2 3 4 5 6 -2rot (*rotates the top 6 items back:                    5 6 1 2 3 4*)
1 2 3 4 2drop     (*removes the top 2 items (don't print to screen): 1 2*)
1 2 3 4 2nip      (*removes the 3rd and 4th item:    		     3 4*)
1 2 3 4 2tuck     (*duplicates the top 2 items below the 4th item:   3 4 1 2 3 4*)
1 2 3 4 2over     (*duplicates the 3rd and 4th item to the top:      1 2 3 4 1 2*)
clst
```

**------------------------------ Return Stack ----------------------------**

The return stack has no function. It only exists for backward compatibility.
It can be used as interim storage for data from and to the data stack.  
Especially `loop` or any other words doesn't use the return stack for execution.
You get the state of nested loops with the word `loop_indices`.  
After the program start of `g7th` you find the arguments on the return stack.

**----------------------------- Defining Words ---------------------------**

The `:` word sets `g7th` into compile mode until it sees the `;` word.

```
: square ( n -- n' ) dup * ;
5 square .                     (*25*)
```
While interactively entering in compile mode the prompt is changed from `ok>`
to `>`.

---------------------------------- Conditionals ---------------------------**

```
42 42 ==    (*true*)
12 53 ==    (*false*)
cr
```

`if` _<stuff to do>_ `endif` _<rest of program>_

```
: >64? dup 64 > if . space ." is greater than 64!" cr endif ;
100 >64?    (*100 is greater than 64!*)
```

`else`

```
: >64? dup . space 64 > if ." is greater than 64!" cr else ." is less than 64!" cr endif ;
100 >64?    (*100 is greater than 64!*)
20 >64?     (*20 is less than 64!*)
```

**----------------------------- Control structures -----------------------**

`do`

```
: myloop 5 0 do ." hello!" cr loop ;
myloop cr		

hello!  
hello!  
hello!  
hello!  
hello!  
```
`do` expects two numbers on the stack: the end number (exclusive) and the
start number (inclusive).

We can get the value of the index as we loop with `I`:

```
: one-to-12 ( -- ) 12 0 do I . space loop cr ;
one-to-12                       (*0 1 2 3 4 5 6 7 8 9 10 11*)
```

Because `loop` checks the termination criterion, a loop will always entered at
least one time.

```
: loop-once 1 1 do I square . loop ;
loop-once                       (*1*)
```

Change the step-size with `+loop`:

```
: threes ( n n -- ) do I . cr 3 +loop ;
15 0 threes                     (*0 3 6 9 12*)
```
Loop with `begin` _<flag>_ `while` _<stuff to do>_ `endwhile`:

```
: count_down 10 begin 1- dup 0>= while dup . space endwhile ;
```

Indefinite loop with `repeat` _<stuff to do>_ _<flag>_ `until` if _<flag>_ remains `false`:

```
: death repeat ." Are we there yet?" cr false until ;
```
Phonetic alphabet with the `case` _<match>_ `of` _<stuff to do>_ `endof` `endcase`
construct:

```
'G'
case
  'A' of ."Alpha" cr
  endof
  'B' of ."Bravo" cr
  endof
  'C' of ."Charlie" cr
  endof
  ."todo!" cr                   (*D, E, F,...*)
endcase
```
Working on every element of a collection with `for` `each`:

```
["cat", "dog", "cow"]
for
  case
    dup "cat" of
      . space ."miao" cr
    endof
    dup "dog" of
      . space ."wuff" cr
    endof
    dup "cow" of
      . space ."moo" cr
    endof
  endcase
each
```

**------------------------- Variables and Memory -------------------------**

Use `variable` to declare `age` to be a variable.

```
variable age
age .	      (*{UNASSIGNED}*)
```
Then we write 21 to age with the word `!`.

```
21 age !
```
Finally we can `print` our variable after using the "fetch" word `@`, which adds
the value to the stack, or use `?` to print that value and some more informations
where available.

```
age .         (*{21}*)
age @ .       (*21*)
age ?         (*5 {21} where 5 is an index into the array storage*)
```
Constants are quite similar, but it prints it value immediately (no `@` necessary):

```
constant water-boiling-point
100 water-boiling-point !
water-boiling-point .            (*100*)
water-boiling-point ?            (*100*)
```
Additionaly there is allocated space to store data to via `,`.
Don't forget the space in front of the `,` and the `,` after the last
value `384`.
This space can be accessed with the corresponding index of the data and
`cell`.

```
55 , 823 , 384 ,
2 cell			 	 (*384*)
```

**-------------------------------- Arrays --------------------------------**

```
variable mynumbers
[64, 9001, 1337] mynumbers !
```

on the stack after `mynumbers @`:

```
                            (*┌                ┐*)
mynumbers @                 (*│   64 9001 1337 │*)
                            (*└                ┘*)
2 @ .                       (*1337*)

```

**---------------------- Floating Point Operations -----------------------**

`G7th` attempts to perform a calculation with the least loss of accuracy.

```
8.3e1 0.8e1 +		  (*9.1*)
7 9 /                     (*0.77777778*)
```
If you want to make an integer division:

```
7 9 //			  (*0*)
7 9 /mod		  (*0 7*)


variable myfloatingvar
4.4 myfloatingvar !
myfloatingvar @           (*4.4*)
myfloatingvar decr        (*3.4*)
myfloatingvar incr        (*4.4*)
```

**-------------------------- Number Conversion ---------------------------**

```
decimal 12345
hex .s                          (*"3039‹16›" (12345)*)
                                (*-‹16›-*)
bin .s                          (*"11000000111001‹2›" (12345)*)
                                (*-‹2›-*)
oct .s                          (*"30071‹8›" (12345)*)
                                (*-‹8›-*)
0xAFFE                          (*45054*)
0b111000111                     (*455*)
decimal
"245" >num .                    (*245*)
```

**------------------------- Vectors and Matrices  ------------------------**

the stack after `*`:

```
                                (*┌    ┐*)
[[7, 2], [0, 3]] [[4], [1]] *   (*│ 30 │*)
                                (*│  3 │*)
                                (*└    ┘*)
```

**---------------------------- Complex Numbers  --------------------------**

```
(30, 5i) (6, 1i) *              (*(175, 60i)*)
```

**--------------------------- String Functions  --------------------------**

Every word that leaves a string on the stack ends with a `$`.

```
"the quick brown fox"
dup 10 left$ .	                (*the quick*)
dup 10 5 mid$ .                 (*brown*)
3 right$ .                      (*fox*)

"foo" "bar" + .                 (*foobar*)
"foo" 'x' + .                   (*foox*)
'x' "foo" + .                   (*xfoo*)
'a' 'b' + ?                     (*ab*)

len .                           (*2*)
```

**-------------------------------- Printing ------------------------------**

```
'a' .                            (*a*)
'a' ?                            (*don't drops the number*)
'a' print                        (*a*)
'a' println                      (*a*)

space
cr
```

**------------------------------ Final Notes -----------------------------**

There's a word to clear the stack.  
Entering a non-existent word keeps the current stack.

```
clst
```

Clear a terminal screen:

```
cls
```

Loading `g7th` files:

```
load <my program filwname>.7th  (*load.7th is loaded once on program start*)
```

You can list every word that's in g7th's dictionary:

```
words
```

Exiting `g7th`:

```
bye
```
