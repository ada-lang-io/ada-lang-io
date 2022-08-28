---
slug: summary-after-four-months-with-ada
title: Summary After Four Months with Ada
authors: [pyjarrett]
tags: [ada, learning]
---

For the last four months I've been learning and writing Ada 2012.
I wanted to share my experiences with the language for those who aren't familiar with it.

<!--truncate-->

I'm neither endorsing or denouncing Ada.
For those unfamiliar with Ada terms, I'll using the vernacular common to C family languages.
This will make Ada programmers cringe, but will be much more clear to the general reader.

When I refer to "Ada" here, I'm referring to Ada 2012 and the SPARK 2014 language,
which is a subset of Ada 2012 used for formal verification.

## Why?


Ada's an obscure language.
It's been around for decades and strong opinions about it existed in the past, but overall it seems forgotten outside its niche.
When I mention I made things in it, I've gotten baffled responses of "That language is still around?"
Another comment was, "I've never heard of anyone else ever working, or having working in Ada."

I've written code in a variety of languages, but Ada by far is the most bizarre and strangely familiar one at the same time.
My intent was to mess around with it for a few weekends and move on, but it wasn't the "dead language" I expected.
There's been a lot of modernization in the last couple of years, which makes it a surprisingly modern language to work in.


## Resources

These are listed in order of what I used:

* [learn.adacore.com](https://learn.adacore.com/)

    * A good tutorial site.  It's enough to get you started.

* [John Barnes "Programming in Ada 2012"](https://www.amazon.com/Programming-Ada-2012-John-Barnes/dp/110742481X)

    * Barnes works on several major Ada references, and this book walks through the language.
      At over 700 pages, it took me abotu a month to get through.

    * This seems to be a common way Ada programmers get onboarded, and Ada programmers
      stay updated.

* [Ada Language Wikibook](https://en.m.wikibooks.org/wiki/Ada_Programming)

    * This is exceptionally good and very complete.
    
* [Awesome-Ada](https://github.com/ohenley/awesome-ada)

    * A site with way more resources than I list here.

* [Ada Reference Manual](http://ada-auth.org/standards/rm12_w_tc1/RM-Final.pdf)

    * Authoritative reference about how Ada **should** work.
    
* [Building High Integrity Applications with SPARK](https://www.amazon.com/Building-High-Integrity-Applications-SPARK/dp/1107656842/ref=sr_1_2?dchild=1&keywords=Building+High+Integrity+Applications+with+SPARK&qid=1630108759&s=books&sr=1-2)

    * It's ok, the first third of the book is an Ada crash couse before it gets into SPARK.

## Projects

During this timeframe I made a few projects:

- [Septum](https://github.com/pyjarrett/septum) - context-based source code search
- [dir_iterators](https://github.com/pyjarrett/dir_iterators) - library similar to the incredible [walkdir](https://github.com/BurntSushi/walkdir)
- [project_indicators](https://github.com/pyjarrett/progress_indicators) - library for spinners and progress bars
- [trendy_terminal](https://github.com/pyjarrett/trendy_terminal) - library for cross-platform terminal setup
- [trendy_test](https://github.com/pyjarrett/trendy_test) - library for simple unit testing
- [Ada Ray Tracer](https://github.com/pyjarrett/ada-ray-tracer) - a port of [Ray Tracing in One Weekend](https://raytracing.github.io/books/RayTracingInOneWeekend.html))
- [dirs_to_graphviz](https://github.com/pyjarrett/dirs_to_graphviz) : Make graphviz files from directory trees

## Ecosystem

### Alire

[Alire](https://alire.ada.dev/) simplifies Ada development significantly, by simplifying project generation,
building, running, and dependency management.

It borrows **heavily** from Cargo, and if "good artists borrow, great artists steal," then Ada is on par with Michaelangelo.
In its quest for modernizing, many concepts of Rust's cargo are being built into this tool.

```bash
alr build

alr run

alr edit
```

Before Alire, I had a a lot of confusion about "How do I use ______ project as a dependency?" and with Alire it's one command line and go.

```bash
# Add dir_iterators library
alr with dir_iterators
```


This tool only went to 1.0 since I've been working with Ada and it simplifies development considerably.
Alire interfaces with the pre-existing tool [grpbuild](https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug.html), which is a common interface into the GNAT ecosystem and tools.
Since it wraps it, you can do additional configuration past the initial setup, and it works well with GNAT Studio, one of the major editors.
You can also set the config to run any other editor command with environment variables setup, such as Visual Studio Code:

```bash
alr config --set editor.cmd "code ."
```

Toolchains are integrated into Alire as of [the 1.1 release candidate](https://github.com/alire-project/alire/releases/tag/v1.1.0-rc1).
so it's becoming a one-stop shop for what you need.

However, there's a few hoops to jump through to get a crate into the manager.
For now, every package and version update requires manual approval.
This is supposed to prevent name squatting and ensures existing libraries can get their appropriate names.
It still can be frustrating waiting for approval, though things usually get approved within a day or so.
You can use local versions as a dependency, which helps when developing libraries,
and keeps you moving if you're waiting that day for approval.

Overall, Alire is a fantastic tool which makes working with Ada easy and I wouldn't recommend learning Ada without it.

### GNAT

The major Ada ecosystem is [GNAT](https://gcc.gnu.org/wiki/GNAT).  Yes, there supposedly are
commercially supported compilers and [AdaCore offers paid support](https://www.adacore.com/gnatpro/comparison),
but the Free Software Foundation (FSF) offers a Ada front-end to GCC.  This is usually referred to as "FSF GNAT".

Can I use Ada for free?  Yes, if you can use GCC for your project, you can use Ada.
You can grab the FSF GNAT toolchain with Alire 1.1 and ``alr toolchain --select gnat_native``.

The entire ecosystem is designed around running tools from the command line, which allows editors
and CI to use the same actions for behaviors.  For example, GNAT Studio just wraps many of the GNAT
tools and it shows you the command line for the actions you want to use.  This acts as training wheels
if you want to get started quickly and then transition to using another editor,
especially a terminal one like Emacs or Vim.

There's a lot of parts to GNAT, which is a super deep dive I'm not interested in doing here.
To give an idea, there's a formatter (``gnatpp``) and a document generator (``gnatdoc``).

* [GNAT Reference Manual](https://gcc.gnu.org/onlinedocs/gcc-11.2.0/gnat_rm/)

* [GNAT User's Guide for Native Platforms](https://gcc.gnu.org/onlinedocs/gcc-11.2.0/gnat_ugn/)

There is also an [LLVM frontend for Ada](https://github.com/AdaCore/gnat-llvm) in progress.
It'd be exciting to see this integrated as a toolchain you can download with Alire.

### Editors

If you believe reddit, editor usage is split roughly in thirds between GNAT Studio and Visual Studio Code,
with Emacs/Vim balanced in the remaining third.

The big editor is [GNAT Studio](https://github.com/AdaCore/gps),
which used to be known as "GPS", and supports Ada, C and C++.  It comes bundled with GNAT Community
Edition, but you can build and run it separately as well.  There's some quirky behavior,
like `tab` indenting to where it thinks the indent should be and not actually inserting a tab,
and some obscure keyboard shortcuts, but otherwise is a mature IDE experience.  You can make and
export your own keybindings, [which I've done for Visual Studio](https://github.com/pyjarrett/gps_keybindings) 
which I should probably contribute back at some point.

There's also support for [Visual Studio code](https://www.reddit.com/r/ada/comments/p29o7r/tutorial_using_ada_in_vs_code/),
which relies on the [Ada Language Server](https://github.com/AdaCore/ada_language_server).

A coordinated set of [Vim plugins](https://github.com/thindil/vim-ada) is available for those who want to go that route.

I had been primarily using GNAT Studio and then moved over to Visual Studio code, you can get
a good editing experience with either one of these.

I'm not an Emacs user, so I'm not familiar with how folks work in that environment.

#### Cross-Platform Behavior

Alire hooks into [GPRbuild's](https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug.html) external
variable system for cross-platform behavior.
In general, I haven't had to mess with GPR configurations too much since Alire wraps it well and provides good defaults on project creation.

This is the Alire side, describing the external values to set for gprbuild to do the "right thing."

Just like in cargo, we have a ``.toml`` describing the build.

```toml
# alire.toml
# Platform selection by Alire on download.
[gpr-set-externals.'case(os)']
windows = { Trendy_Terminal_Platform = "windows" }
linux = { Trendy_Terminal_Platform = "linux" }
macos = { Trendy_Terminal_Platform = "macos" }
```

On the GPR side, we select the source we want to use, since Ada doesn't have a common preprocessor.
GNAT has a preprocessor but it seems frowned upon to use it.

```ada
-- my_project.gpr
type Platform_Type is ("windows", "linux", "macos");
Platform : Platform_Type := external ("Trendy_Terminal_Platform");
case Platform is
    when "windows" => Trendy_Terminal_Sources := Trendy_Terminal_Sources & "src/windows";
    when "linux"   => Trendy_Terminal_Sources := Trendy_Terminal_Sources & "src/linux";
    when "macos"   => Trendy_Terminal_Sources := Trendy_Terminal_Sources & "src/mac";
end case;
```

## Community

The online Ada community is exceptionally small.
Online counts in [r/ada](https://www.reddit.com/r/ada/) hover in the mid twenties, compared to Rust's ~900, and seem to peak in the 50s.

Despite the size, the community is extremely knowledgeable and 
there's a lot of folks with decades of Ada experience chomping at the bit to answer questions.
This means Reddit and Stack Overflow answers regularly include the applicable language rule's section and paragraph from the Ada Reference Manual.

An interesting aside is that if Github locations are to be believed, the Ada community is predominantly European.
This correlates with what I've seen, since I'm in the US Eastern time zone, people online tend to be very active in the morning.

## AdaCore and Ada Modernization

[AdaCore](https://www.adacore.com/) didn't invent Ada, so they're not a direct comparison to Mozilla with Rust,
but they did help develop GNAT originally, and continue to contribute back.
I'm sure there's other companies promoting Ada, but they seem to be the most visible.

It looks like there was a huge burst of activity after the release of Ada 2012,
including a lot of [quite-detailed YouTube videos](https://www.youtube.com/playlist?list=PLkoa8uxigENkneyEEeDWVPgpMhPc9IJ7o)
and marketing.

The second wave promoting Ada now seems to be getting better traction and community involvement.
AdaCore [open-sourced a lot of libraries](https://github.com/AdaCore) and have been the primary group I've seen promoting the language.
The movement they're pushing seems to be the right direction: building an LLVM front-end to the language, a language server,
open-sourcing a lot of libraries, creating learning resources and improving IDE support.

The modernization push is intense.
There is a marked improvement working in the language even in the short time span I've seen.
It's gone from "a quirky and sometimes clunky car" to feeling "modern."

## A Free-Function Focused C++, or a Strongly Typed C

Ada focuses on creating packages of code which contain types and functions.
There's no preprocessor, so instead of ``#include``, you ``with`` packages which contain what you need.

```ada
with RT.Vecs; -- depend on another package, RT.Vecs

package RT.Rays is
    use RT.Vecs;  -- use the names inside RT.Vecs

    type Ray is record
        Origin    : Point3;
        Direction : Vec3;
    end record;

    function Point_At(R : Ray; T: F32) return Point3;

end RT.Rays;
```

Packages are namespaces for functions and types, unlike other languages where types can "contain" functions and types.
Function overloading acts as a key design element, made clear by the lack of implicit casts.

```ada
-- Idiomatic versions of "ToString"
function Image(S : Some_Type) return String;
function Image(A : Another_Type) return String;
```

Everything in a package is related, there's no syntactical split between "free function", "class function", or "member function" (method).

```ada
package RT.Vecs is
    type Vec3 is record
        X, Y, Z : F32 := 0.0;
    end record;

    function Length (V : Vec3) return F32;

    -- Is this a free function, a class function or a member function (method)?
    -- It doesn't really matter, because it's declared the same.
    function Dot (A, B : Vec3) return F32;
end RT.Vecs
```

Functions get declared similarly, with parameter type and ordering describing the difference between C++'s "free functions" vs instance functions.
A class type as a leading parameter determines if one would be considered a member function ("methods").
There's a concept called "primitive operations" which describe bringing in operations of a parent type (even primitive, like Integer) where this matters.

```ada
package SP.Filters is
    -- An empty "abstract class"
    type Filter is abstract tagged null record;

    -- A "pure virtual method"
    function Matches_Line (F : Filter; Str : String) return Boolean is abstract;

    -- Accepts any derived class of Filter
    function Matches_File (F : Filter'Class; Lines : String_Vectors.Vector) return Boolean;

    -- A "subclass"
    type Case_Sensitive_Match_Filter is new Filter with record
        Text : Unbounded_String;
    end record;

    -- An overridden method of a base class.
    overriding function Matches_Line (F : Case_Sensitive_Match_Filter; Str : String) return Boolean;

    -- "Free-function" which is just part of the package.
    function Is_Quoted (S : String) return Boolean;
end SP.Filters
```

Packages are split between a "specification" (.ads file) and a "body" (.adb file), akin to the "header" and "source"
files you encounter in C/C++, though this distinction is understood at the language level.
Since physical design matters in ways similar to C and C++, some physical design techniques in [Lakos' Large-Scale C++ Software Design](https://www.amazon.com/Large-Scale-Software-Design-John-Lakos/dp/0201633620)
book actually work.

```ada
package body RT.Rays is
    -- Use all the operators of Vec3
    use all type RT.Vecs.Vec3;

    function Point_At(R : Ray; T: F32) return Point3 is (R.Origin + T * R.Direction);
end RT.Rays;
```

Packages can contain startup code executed by the "environment task" prior to entering the main procedure for initialization.
Unlike C++, there are language pragmas you can use to control order of initialization of these elements.

```ada
package body RT.Debug is
begin
    Ada.Text_IO.Put_Line("Executed before the program enters the main function!");
end RT.Debug;
```

## SPARK

SPARK is a subset of Ada I glazed over earlier.
It's a language for formal verification which you can intermingle with Ada code.
Think of it as sort of like `extern C` in C++, or `unsafe` in Rust, except it marks code as undergoing verification.
Since it's a subset of Ada, in addition to verification, you get all the rest of the Ada tooling here, like Alire.
Yes, there are SPARK formally verified crates in Alire, such as a [formally verified implementation of NaCl](https://alire.ada.dev/crates/sparknacl)

## Focus on Intent

### Parameter Modes

Ada source focuses on describing intent and modeling semantics.

I misled you earlier for expediency.
What most C-family languages call "functions", Ada calls "subprograms".
Ada distinguishes between those which return a value and are truly "functions" and those which do not return a value, and are "procedures."

For example, subprogram parameters can be either `in`, `out`, or both.
`in` parameters are readonly, and while you can force passing by reference via specifics in the language, you often just ignore how this happens.
Parameters are implicitly `in`, so you can omit that if you want.
This is common in SPARK code because `out` parameters are forbidden.

```ada
-- Receives and modifies a parameter.
procedure Clear_Filters (Srch : in out Search);

-- Like a "const" member function in C++, which can't modify its argument
-- Could also be declared like this:
-- function Num_Files (Srch : Search) return Natural;
function Num_Files (Srch : in Search) return Natural;
```

### Derived Types

Describing semantics goes all the way into primitive types, and the rules are consistent between primitive and user-defined types.
Creating lightweight types with domain-specific meaning, prevents mishandling of semantics on primitive types due to no implicit casting.
Interfaces rarely use `Integer` or `Float` directly, instead you'll find semantic versions ("derived types") created such as "Meters" or "Kilometers".


```ada
type Seconds is new Natural;
type Milliseconds is new Natural;

S : Seconds := 10;
M : Milliseconds := 50;

M := S;                       -- Compile error!
M := Milliseconds (1000 * S); -- Allowed because I told compiler it's ok
```

Function overloading checks parameters as well as return types, so creating functions and transforms of types is straightforward.

```ada
procedure Update (S : Seconds);
procedure Update (M : Milliseconds);

function Delta_Time return Seconds;
function Delta_Time return Milliseconds;
```

Compile and runtime checks provide bounds-checking and numerical types can have their bounds constrained to "known good" values.

```ada
-- Let's write this with our own defined range, only allowing less than 60 seconds.
type Seconds is new Integer range 0 .. 59;
```

You can also access the ranges of types with the `'First` and `'Last` attributes (read as "tick first" and "tick last").

```ada
pragma Assert(Seconds'First = 0);
pragma Assert(Seconds'Last = 59);
```

### Enumeration Types

Enumeration types have first class support, with many automatically generated attributes.
`'First` and `'Last` get the bounds of the values and `'Pred` (predecessor) and `'Succ` move between individual values.
Iteration over all values and conversions to and from strings and integers gets provided for free.
Together, these attributes and iteration capability allow writing of generic code which operates on discrete types like integers, or enumerations.

```ada
with Ada.Unchecked_Conversion;

type Filter_Action is (Keep, Exclude);    

-- Specify internal values for the enumeration (optional).
for Filter_Action use (
    Keep => 2,
    Exclude => 3
);

-- Allow conversion to get the values out
function Repr is new Ada.Unchecked_Conversion(Filter_Action, Integer);

procedure Print_Actions is
begin
    -- Which order are they in?
    pragma Assert(Keep = Filter_Action'Val(0));
    pragma Assert(Exclude = Filter_Action'Val(1));
    pragma Assert(0 = Filter_Action'Pos(Keep));
    pragma Assert(1 = Filter_Action'Pos(Exclude));

    -- Comparison operators
    pragma Assert(Keep /= Exclude);  -- /= is Ada's not equal (i.e. !=)
    pragma Assert(Keep < Exclude);

    pragma Assert(Filter_Action'First = Keep);
    pragma Assert(Filter_Action'Last = Exclude);
    pragma Assert(Filter_Action'Succ(Keep) = Exclude);
    pragma Assert(Filter_Action'Pred(Exclude) = Keep);

    -- Parsing from string
    pragma Assert(Filter_Action'Value("KEEP") = Keep);
    pragma Assert(Filter_Action'Value("Exclude") = Exclude);

    -- Getting underlying representation
    pragma Assert(2 = Repr(Keep));
    pragma Assert(3 = Repr(Exclude));

    for Action in Filter_Action loop
        Ada.Text_IO.Put_Line(Action'Image);  -- Prints KEEP and then EXCLUDE
    end loop;
end Print_Actions;
```

Since arrays operate using a discrete type as an index, enumerations can be used as the type to index into an array.

```ada
type Test_Status is (Passed, Failed, Skipped);
type Test_Report is array (Test_Status) of Natural;

procedure Foo is
    Report : Test_Report;
begin
    -- ...

    -- Increment number of passed tests.
    Report(Passed) := Report(Passed) + 1;

    -- ...
```


### Pre- and Post-Conditions

Ada 2012 adds built-in support for pre and post conditions, through the use of "aspects."
This is a "killer feature" of Ada 2012, on top of all of the other type checking and safety checking.
Though used in SPARK analyses, you can also write them in plain Ada 2012 code and as part of specification of the function.
Clients can see it as part of the interface and the compiler inserts these runtime checks if enabled.
A lot of languages have an assertion mechanism which often effectively gets used for these checks, but it's nice to have a client-visible built-in way of doing this
I've also found that adding pre and post condition checks during debugging to be a very effective tool.

```ada
function Merge (A, B : Context_Match) return Context_Match with
    Pre  => Is_Valid (A) and then Is_Valid (B),
    Post => Is_Valid (Merge'Result);
```

Types which expose no private state can also have type invariants which are checked prior to usage as function arguments and after assignments.

```ada
type Spinner is record
    Ticks_Per_Move : Positive;
    Ticks          : Natural;
    State          : Spinner_State;
    Style          : Spinner_Style;
end record with
    Type_Invariant => Ticks < Ticks_Per_Move;
```

## Concurrency

### Protected Objects

Protected objects coordinate concurrent access to shared state.
The control can also include arbitrarily complex conditionals as well, such as not allowing any writers when readers exist, or blocking any more readers when a writer is waiting.

```ada
protected body Concurrent_Context_Results is
    -- Calling Get_Results will wait for Pending_Results to equal 0.
    entry Get_Results (Out_Results : out SP.Contexts.Context_Vectors.Vector)
        when Pending_Results = 0 is
    begin
        Out_Results := Results;
    end Get_Results;

    procedure Wait_For (Num_Results : Natural) is
    begin
        Pending_Results := Num_Results;
    end Wait_For;

    procedure Add_Result (More : SP.Contexts.Context_Vectors.Vector) is
    begin
        Results.Append (More);
        Pending_Results := Pending_Results - 1;
    end Add_Result;
end Concurrent_Context_Results;
```

### Tasks

Tasks provide concurrent execution.
Additionally, they have special procedures called "entries" which can be "accepted" by a related task during its flow of execution to synchronize (rendezvous) with other tasks and share data at these points.

Tasks run concurrently in the block in which they're declared, and the block will not exit until the task finishes or terminates, unless it is allocated on the heap.

Both single instance and instantiable versions of protected objects and tasks can be created.

```ada
task type File_Loader_Task is
    entry Wake;
end File_Loader_Task;

task body File_Loader_Task is
    Elem : Ada.Strings.Unbounded.Unbounded_String;
begin
    -- Concurrent execution starts here when the task is created.

    -- Don't proceed until the "Wake" Entry has been called.
    select
        accept Wake;
    or
        -- If Wake was never received the program can terminate
        -- this task if needed to exit the block.
        terminate;
    end select;

    loop
        select
            -- Blocking dequeue
            File_Queue.Dequeue (Elem);
        or
            -- Timeout: we waited 1 second, and nothing else to process
            -- was on the queue, so quit this task.
            delay 1.0;
            exit;
        end select;

        if Is_Text (Elem) then
            Cache_File (File_Cache, Elem);
        end if;

        -- "Progress" is an (unshown) declared variable in the same scope
        -- as the task, so the task has access to it.
        Progress.Finish_Work (1);
    end loop;
end File_Loader_Task;
```

## Generics

Ada generics are similar to ML signatures, and may contain types, functions and even other packages as parameters.
Generic packages or functions must be explicitly instantiated for use.
This eliminates the debate of angled brackes (`<>`) versus square brackets for generics ([]), but leads to additional names being created.
This makes their usage and their cost explicit, at the expense of verboseness.

```ada
-- Instantiate a generic package which contains an
-- Ada equivalent to std::vector<std::string>
package String_Vectors is new Ada.Containers.Vectors(
        Index_Type   => Positive,
        Element_Type => Ada.Strings.Unbounded.Unbounded_String,
        "="          => Ada.Strings.Unbounded."=");

-- Since String_Vectors is a package, Vector is the actual vector type.
-- There are more related types in the instantiated package.
function Shell_Split (S : Ada.Strings.Unbounded.Unbounded_String) return String_Vectors.Vector;
```

## Low Level Control

Accessing C functions and compiler intrinsics is straightforward.
You create a declaration of the subprogram and then describe where it comes from using aspects or the ``Import`` pragma.

```ada
with Interfaces.C;

type FD is new Interfaces.C.int;
function isatty (File_Descriptor : FD) return BOOL
    with Import     => True,
            Convention => C;

-- Bring in the stdout file pointer from C
type FILE_Ptr is new System.Address;
stdout : aliased FILE_Ptr;
pragma Import (C, stdout, "stdout");
```

Since the usage is the same as with an Ada function, imported functions can be replaced with actual Ada code if needed.
Inline assembler is also available, but due to the lack of a preprocessor, the build system (gpr) is leveraged to choose the appropriate definition (body) file to compile.

```ada
function File_Line return Natural;
pragma Import (Intrinsic, File_Line, "__builtin_LINE");
```
    
Representation clauses allow you to match struct layout or binary formats such as for files.

```ada
type Bitmap_File_Header is record
    Identifier      : Integer_16;
    File_Size_Bytes : Integer_32;
    Reserved        : Integer_16 := 0;
    Reserved2       : Integer_16 := 0;
    Offset          : Integer_32;
end record with
    Size => Byte * 14;

for Bitmap_File_Header use record
    Identifier      at  0 range 0 .. 15;
    File_Size_Bytes at  2 range 0 .. 31;
    Reserved        at  6 range 0 .. 15;
    Reserved2       at  8 range 0 .. 15;
    Offset          at 10 range 0 .. 31;
end record;
```

## Vocabulary

Ada suffers from a lack of familiarity for many programmers due to being a Pascal family language and also its peculiar, but very specific vocabulary.
However, the usage of keywords over punctuation helps ease many problems of dealing with an unfamiliar language.
While this helps with googling and a lot of terms appear in code, many are specific to, or have Ada-specific definitions.
Examples are "accesses" (sort of like pointers), "accesibility" (similar to a scope for borrowing), 
"tagged types" (classes), "derived types" (unrelated to OOP), and "subprogram".

## Verboseness

The language has a mind of its own as well.
As [one online quote says](https://people.cs.kuleuven.be/~dirk.craeynest/quotes.html).

> When I find myself fighting the [Ada] language, it usually means that I need to revisit my design.

I've found this to be true overall.
Ada makes some easy things verbose and some verbose things easy.
When things go from "verbose" to "writing like a Charles Dickens novels", that's when I rethink my approach to the problem.
There's usually a significantly better and shorted way to accomplish the task.

## Was it worth it?

If I were never to write Ada again, I still learned a lot about program correctness.
Correctness isn't usually a fun language feature to talk about and no one likes to admit they write bugs.
Ada excels at modeling your program in the language while automating a lot of error checking.
For example, you may know you're not going to use the full range of an integer, and Ada enables specification and automatic checking of valid values.
Combined with built-in pre and post conditions, this has helped me improve the way I think while programming.
