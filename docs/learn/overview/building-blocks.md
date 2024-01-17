---
sidebar_position: 2
---

# Building Blocks

Ada often uses different terminology than any of the languages I've used. The
goal is an overview without jargon, the only Ada-specific terms necessary
for this overview are "subprogram", which refers to functions and procedures, and
"procedures" which in most C-like language would be a "function" which returns
nothing (i.e. void).

# Packages, the Building Blocks of Ada

Ada descends from Pascal, and yet uses many concepts already familiar to C or
C++ programmers. As a long time C++ programmer, I find Ada leverages the concepts
I'm used to in more formal, and often compiler-checked ways.

## Types and Ada, The Elephant in the Room

Ada emphasizes types, but their consistent use despite their complexity
means I can ignore them for now. Nearly every other Ada overview and tutorial
focuses on them, but they're an incredibly deep topic in Ada. Rather than get
bogged down there, let's look at what Ada looks like and how programs are structured.

## Subprograms (functions and procedures)

Ada draws a line between functions, which return values, and procedures which
do not return a value. Collectively referred to as "subprograms", either of
these may have input (`in`) and output (`out`) parameters, with parameters
being allowed to also be both an input and output (`in out`). `in` is
optional for functions. Note that parameters are separated by semicolons
(`;`), not by commas (`,`).

```ada
procedure Rectangle_Area(Width : in Float; Height : in Float; Area : out Float) is
begin
    Area := Width * Height;
end Rectangle_Area;

function Rectangle_Area(Width : Float; Height : Float) return Float is
begin
    return Width * Height;
end Rectangle_Area;
```

Short functions may be written as expressions bounded by parentheses. `in` is
also optional for functions, and parameters with the same type can be grouped.

```ada
-- Add two-dimensional vectors.
function Add (L, R : Float2) return Float2 is (L.X + R.X, L.Y + R.Y);
```

Multiple subprograms can exist with the same name, so the one used is determined
by the types of the parameters and also the returned type. There are no implicit
conversions between floating point and integer types, which maximize clarity.

```ada
function Area (Width, Height : in Float) return Float is (Width * Height);
function Area (Width, Height : in Integer) return Integer is (Width * Height);
```

The basic operators can be overloaded as well. Assignment (`:=`) is
not considered an operator, and therefore cannot be overloaded.

```ada
function "+"(L, R : Float2) return Float2 is (L.X + R.X, L.Y + R.Y);
```

## Packages

As the fundamental building block of Ada, "packages" compose Ada programs,
namespaces which exhibit the logical and physical interfaces in a manner
similar to C++ header and source files. Most languages don't have a concept
of "physical interface"--it's things the compiler needs to know, but
are not part of the logical interface of the program, such as
size details for structs and classes.

Instead of a header files providing an informal spec and an associated source
file being the translation unit, in Ada a package is roughly analogous to a
"header file," and that package's body is the "source file", except as if
everything in each file is in a namespace given by the file name.

Top-level package specifications, appear in `*.ads` (Ada specification) files,
with their implementations ("bodies") in `*.adb` (Ada body) files, and only
one top-level package specification or package body per file.

Ada packages can also be nested and support visiblity rules for sharing details
with child packages. Child packages are given by dotted names; `A.B`
is a child of package `A`.

Packages can also contain initialization code for the package to run at program
startup prior to entering the main procedure, with the ability to describe
dependencies in package start up order. This solves a specific C++ issue in
which static initialization order is not known, while also offering the ability
to avoid deferred first-time usage costs, such as with singletons.

Ada uses "aspects" to denote additional properites of packages, subprograms, and
types. Along with aspects, compiler pragmas allows description of initialization
dependencies, as well as providing high level checks, such as `pragma Preelaborate`
to ensure a package has no initialization, or the `with Pure` aspect to ensure
that a package has no state and subprograms cannot have side effects.

## The Core Tenet of Ada

Program text must be clear without having to read ahead, referred to as:
**"Linear elaboration of declarations"**. A clear demarcation exists between
declarations of things to exist and executable statements which use those things.

The simplicity backing this is the ability to make any declaration, including
types, variables, functions/procedures, and packages in any declaration block. This means
the basic rule of "declare, then use" repeats itself throughout the language,
in `package/package body`, `task/task body`, subprograms, and executable blocks of code can have a
`declare ... begin ... end` block.

```ada
package P is
    -- Not declaring Foo here is like making the subprogram `static` in C or C++ or
    -- putting it into an anonymous namespace.
    procedure Foo;
end P;

package body P is
    -- Declarations for the body of P go here.

    procedure Foo is
        -- Declarations for Foo can go here.

        -- Declare a subprogram, only visible to Foo, to be used to implement Foo.
        procedure Bar is
        begin
            null; -- "null statement" here since this subprogram actually does nothing.
                -- and one statement is required.
        end Bar;
    begin
        -- Executable statements go here.
        Ada.Text_IO.Put_Line("Hello, World!");

        declare
            -- Declare a package here, inside the subprogram, to show that you can.
            package Wat is
                -- Declare a new type, which has the same set of possible values
                -- as a Float, but is different than a Float.
                type Capricorn is new Float;
            end Wat;

            -- A constant created using the package defined inside of Foo.
            -- Temporary variables can be declared here too.
            Temp: constant Wat.Capricorn := 0.0;
        begin
            -- Print "0.0".
            -- "Image" is the Ada idiomatic equivalent of toString().
            -- ' is "tick" and is used to access compiler-defined properties of types.
            Ada.Text_IO.Put_Line(Wat.Capricorn'Image(Temp));

            -- Call the helper procedure defined in Foo.
            -- Procedures and functions without parameters are called
            -- without parentheses.
            Bar;
        end;
    end Foo;
begin
    -- Static initialization body of P.
end P;
```

This nesting of declarations very verbose. It does however makes it straightforward to
refactor out behavior while you're working on a subprogram and then you can extract
the newly created components into more appropriate places when you're done.
The inability to use statements in declarations causes me to sometimes rewrite
my declarations in sequential order of constant processing, and overall makes the
declarations feel like a Haskell `where` clause.

```ada
function Normalize(F : Float2) return Float2 is
    L : constant F32 := Length(F);
begin
    return F / L;
end Normalize;
```

```ada
function Evaluate
    (Ctx : in out Context; Line : in Ada.Strings.Unbounded.Unbounded_String)
        return Evaluate_Result is
    use Ada.Containers;
    use type Ada.Strings.Unbounded.Unbounded_String;
    Whitespace     : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set (" ");
    Sanitized_Line : constant Ada.Strings.Unbounded.Unbounded_String :=
                        Ada.Strings.Unbounded.Trim (Line, Whitespace, Whitespace);
    Words          : String_Vectors.Vector := Split (Sanitized_Line);
    Command        : constant Ada.Strings.Unbounded.Unbounded_String := (if Words.Length > 0 then Words.First_Element else Ada.Strings.Unbounded.Null_Unbounded_String);
begin
    -- ...
```

Ada does not provide separate syntactical units for classes, structs and
namespaces. Instead, packages contain types, constants and related subprograms.
A lot of specialized syntax goes away due to
this, for example there are no "member functions" and "class functions" and
hence no specialized syntax for things like member function pointers or class
function pointers exist. Namespacing and overloading on parameters and/or the returned type determine
the subprogram called.

What would be "member functions" in C++ have the "controlling type(s)" as the
first parameter(s). "`const` member functions" pass in the type as an `in` parameter,
which are immutable. "non-`const` member functions (methods)" pass in the type as an
`in out` parameter, allowing the parameter to be modified. This mirrors
Rust's notation wherein it reflects C++-like `const` behavior of member
functions with `self`, `&self`, and `&mut self` as a first parameter.
These are referred to as "primitive operations."

```ada
-- Box "non-const function"
procedure Move(Box : in out AABB2; Direction : Float2) is
begin
    Box.Min := Box.Min + Direction;
    Box.Max := Box.Max + Direction;
end Scale;
```

```ada
-- Box "const function"
function Midpoint(Box : in AABB2) return Float is
begin
    return (Box.Min + Box.Max) / 2.0;
end Scale;
```
