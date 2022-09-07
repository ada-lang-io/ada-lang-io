"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2815],{7130:(t,l,e)=>{e.r(l),e.d(l,{assets:()=>B,contentTitle:()=>m,default:()=>C,frontMatter:()=>p,metadata:()=>y,toc:()=>T});var n=e(1716),k=e(7556),d=Object.defineProperty,o=Object.defineProperties,u=Object.getOwnPropertyDescriptors,r=Object.getOwnPropertySymbols,a=Object.prototype.hasOwnProperty,i=Object.prototype.propertyIsEnumerable,s=(t,l,e)=>l in t?d(t,l,{enumerable:!0,configurable:!0,writable:!0,value:e}):t[l]=e,c=(t,l)=>{for(var e in l||(l={}))a.call(l,e)&&s(t,e,l[e]);if(r)for(var e of r(l))i.call(l,e)&&s(t,e,l[e]);return t};const p={sidebar_position:50},m="Syntax Cheat Sheet",y={unversionedId:"getting-started/cheat-sheet",id:"getting-started/cheat-sheet",title:"Syntax Cheat Sheet",description:"Names used in examples",source:"@site/docs/getting-started/cheat-sheet.mdx",sourceDirName:"getting-started",slug:"/getting-started/cheat-sheet",permalink:"/docs/getting-started/cheat-sheet",draft:!1,tags:[],version:"current",sidebarPosition:50,frontMatter:{sidebar_position:50},sidebar:"tutorialSidebar",previous:{title:"Being More Terse",permalink:"/docs/getting-started/tips/being-more-terse"},next:{title:"Glossary",permalink:"/docs/getting-started/glossary"}},B={},T=[{value:"Names used in examples",id:"names-used-in-examples",level:2},{value:"Overview",id:"overview",level:2},{value:"Program Structure",id:"program-structure",level:2},{value:"Memory",id:"memory",level:2},{value:"Special Notations",id:"special-notations",level:2},{value:"Expressions",id:"expressions",level:2},{value:"Mathematics",id:"mathematics",level:2},{value:"Control Flow",id:"control-flow",level:2},{value:"Boolean Algebra",id:"boolean-algebra",level:2},{value:"Functions and Procedures",id:"functions-and-procedures",level:2},{value:"Types",id:"types",level:2},{value:"Object-Oriented Programming",id:"object-oriented-programming",level:2}],x={toc:T};function C(t){var l,e=t,{components:d}=e,s=((t,l)=>{var e={};for(var n in t)a.call(t,n)&&l.indexOf(n)<0&&(e[n]=t[n]);if(null!=t&&r)for(var n of r(t))l.indexOf(n)<0&&i.call(t,n)&&(e[n]=t[n]);return e})(e,["components"]);return(0,n.kt)("wrapper",(l=c(c({},x),s),o(l,u({components:d,mdxType:"MDXLayout"}))),(0,n.kt)("h1",c({},{id:"syntax-cheat-sheet"}),"Syntax Cheat Sheet"),(0,n.kt)("h2",c({},{id:"names-used-in-examples"}),"Names used in examples"),(0,n.kt)("table",null,(0,n.kt)("tr",null,(0,n.kt)("td",null,(0,n.kt)("strong",null,"Concept")),(0,n.kt)("td",null,(0,n.kt)("strong",null,"Ada"))),(0,n.kt)("tr",null,(0,n.kt)("td",null," Reference "),(0,n.kt)("td",null," Access ")),(0,n.kt)("tr",null,(0,n.kt)("td",null," Pointer "),(0,n.kt)("td",null," Access All ")),(0,n.kt)("tr",null,(0,n.kt)("td",null," Pointer "),(0,n.kt)("td",null," Access All ")),(0,n.kt)("tr",null,(0,n.kt)("td",null," Namespace "),(0,n.kt)("td",null," P, Q, R ")),(0,n.kt)("tr",null,(0,n.kt)("td",null," Class "),(0,n.kt)("td",null," Capricorn ")),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Struct "),(0,n.kt)("td",null," Scorpio ")),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Type "),(0,n.kt)("td",null," S, T, V, W ")),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Variables "),(0,n.kt)("td",null," A, B, C ")),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Function "),(0,n.kt)("td",null," Foo, Bar "))),(0,n.kt)("h2",c({},{id:"overview"}),"Overview"),(0,n.kt)("table",null,(0,n.kt)("tr",null,(0,n.kt)("td",null,"Identifiers "),(0,n.kt)("td",null,"Can't start with number or underscore, ",(0,n.kt)("strong",null,"case insensitive"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Keywords "),(0,n.kt)("td",null,"Case insensitive, usually lower case ")),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Naming Conventions(s) "),(0,n.kt)("td",null,(0,n.kt)("code",null,"Ada_Case"),"(types and functions), ",(0,n.kt)("code",null,"keywords"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Declaration file "),(0,n.kt)("td",null,(0,n.kt)("code",null,"FileName.ads"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Definition file "),(0,n.kt)("td",null,(0,n.kt)("code",null,"FileName.adb"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Dependency "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"with Package.Child;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Line comment "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"-- line comment"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Block comment "),(0,n.kt)("td",null,(0,n.kt)("strong",null,"N/A"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Inline docs "),(0,n.kt)("td",null," Line comment before or after element "))),(0,n.kt)("h2",c({},{id:"program-structure"}),"Program Structure"),(0,n.kt)("table",null,(0,n.kt)("tr",null,(0,n.kt)("td",null,"Compile-time config"),(0,n.kt)("td",null,(0,n.kt)("strong",null,"N/A"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Static assert"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"pragma Assert(cond);"))),(0,n.kt)("tr",null,(0,n.kt)("td",null),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"pragma Assert(cond, message);"))),(0,n.kt)("tr",null,(0,n.kt)("td",null),(0,n.kt)("td",null,(0,n.kt)("strong",null,"N/A"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Namespacing"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"package P"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Child Namespaces"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"package P.R"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Namespacing"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"package P"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Namespace aliasing"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"package TIO renames Ada.Text_IO;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Using namespace"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"use Ada.Text_IO;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Using subprograms of type"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"use type T;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Scope resolution"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"P.Q.R"))),(0,n.kt)("tr",null,(0,n.kt)("td",null),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"private with Q;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"limited with P;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"private package P;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Ensuring module has no state"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"package P with Pure"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"No module initialization required"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"pragma Preelaborate(P);"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Ensure elaboration immediately after specification"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"pragma Elaborate_Body;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Ensure other package is initialized before this one"),(0,n.kt)("td",null)),(0,n.kt)("tr",null,(0,n.kt)("td",null),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"pragma Elaborate(P);"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Ensure other package and all dependencies are initialized before this one."),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"pragma Elaborate_All(P);"))),(0,n.kt)("tr",null,(0,n.kt)("td",null),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"pragma Restrictions(No_Dependencies => Other_Package)")))),(0,n.kt)("h2",c({},{id:"memory"}),"Memory"),(0,n.kt)("table",null,(0,n.kt)("tr",null,(0,n.kt)("td",null,"Pointer "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"Ptr : access all T;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Pointer to allocation "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"Ptr : access T;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"from a specific pool "),(0,n.kt)("td",null," ")),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Pointer deference "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"Ptr.all"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Reference "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"Ptr : not null access T;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Variable used by Pointer "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A: aliased T;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Address "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"Ptr : access T := T'Access(A)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Address "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"Ptr : access all T := T'Unchecked_Access(A)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Constant pointer "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"Ptr : constant access T;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Pointer to constant "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"Ptr : access constant T;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Constant pointer to constant "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"Ptr : constant access constant T"))),(0,n.kt)("tr",null,(0,n.kt)("td",null," "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"pragma Restrictions(No_Implicit_Heap_Allocation)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Prevents allocations to anonymous access types. "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"pragma Restrictions(No_Anonymous_Allocators)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Dynamic allocation "),(0,n.kt)("td",null," A : access T := new T; "))),(0,n.kt)("h2",c({},{id:"special-notations"}),"Special Notations"),(0,n.kt)("table",null,(0,n.kt)("tr",null,(0,n.kt)("td",null,"Equality "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A = B"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Inequality "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A /= B"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Assignment "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A := B"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Array Access "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A(i)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Range "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"min .. max"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,'"Box" '),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"<>"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Exponentiation "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"Base ** Exponent"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Discrete type "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"(<>)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,'"Tick" '),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"'"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"String Concatenation"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A & B")))),(0,n.kt)("h2",c({},{id:"expressions"}),"Expressions"),(0,n.kt)("table",null,(0,n.kt)("tr",null,(0,n.kt)("td",null,"qualified expression "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"for all A of B => expr"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"qualified expression "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"for some A of B => expr"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"if expression "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A : Boolean := (if A then B else C);"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"case expression "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A : Integer = (case Value is","\n   ","when 0 => 1,","\n   ","when 1 => 1,","\n   ","when 2 .. 4 => 5,","\n   ","when 5 | 9 => 10,","\n   ","when others => 0);"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Expression renaming "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"L2 : Float renames V.Length * V.Length;")))),(0,n.kt)("h2",c({},{id:"mathematics"}),"Mathematics"),(0,n.kt)("table",null,(0,n.kt)("tr",null,(0,n.kt)("td",null,"Modify in-place "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A := A + 1;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Modulus "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"mod"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Remainder "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"rem"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Exponentiation "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A ** B"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Bit shifting "),(0,n.kt)("td",null,"In standard library "))),(0,n.kt)("h2",c({},{id:"control-flow"}),"Control Flow"),(0,n.kt)("table",null,(0,n.kt)("tr",null,(0,n.kt)("td",null,"if "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"if A then","\n   ","statements;","\n","elsif B then","\n   ","statements;","\n","else","\n   ","statements;","\n","end if;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"while "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"while A loop","\n    ","statements;","\n","end loop;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"do-while"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"loop","\n","   ","-- statements","\n","   ","exit when A;","\n","end loop;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"value-based loop"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"for Value in 0 .. 99 loop","\n","   ","statements;","\n","end loop;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"iterator-based loop"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"for Elem of Container loop","\n","   ","statements;","\n","end loop;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Multiple choice"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"case Value is","\n","   ","when 0 => Handle_Zero;","\n","   ","when 1 => Handle_One;","\n","   ","when 2 .. 4 =>","\n","      ","Handle_Range;","\n","   ","when 7 | 9 =>","\n","      ","Handle_Choices;","\n","   ","when others => Handle_Default;","\n","end case;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Iterate over enum"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"for Elem in EnumName loop","\n   ","statements;","\n","end loop;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Stop iterating"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"exit"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Start exception handling "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"declare","\n   ","statements;","\n","exception","\n   ","when A =>","\n      ","statements;","\n   ","when others =>","\n      ","statements;","\n","end;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Empty statement (pass) "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"null;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Label "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"<<LABEL_NAME>>"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"goto "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"goto LABEL_NAME")))),(0,n.kt)("h2",c({},{id:"boolean-algebra"}),"Boolean Algebra"),(0,n.kt)("table",null,(0,n.kt)("tr",null,(0,n.kt)("td",null,"Equality "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A = B"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Inequality "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A /= B"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Not "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"not A"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Boolean operators "),(0,n.kt)("td",null)),(0,n.kt)("tr",null,(0,n.kt)("td",null,"or "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A or B"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"and "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A and B"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Short circuiting boolean operators "),(0,n.kt)("td",null)),(0,n.kt)("tr",null,(0,n.kt)("td",null,"or"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A or else B"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"and"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A and then B"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Exclusive-Or (XOR) "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A xor B"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Implies (not A, or B) "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"(if A then B)")))),(0,n.kt)("h2",c({},{id:"functions-and-procedures"}),"Functions and Procedures"),(0,n.kt)("table",null,(0,n.kt)("tr",null,(0,n.kt)("td",null,"Procedure                "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"procedure Foo(X: in T; Y: in V) is","\n","   ","begin","\n","   ","statements;","\n","end Foo;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Function "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"function Fibonacci(X: Natural) return Natural is","\n","   ","if X = 0 or X = 1 then","\n","      ","return X;","\n","   ","else","\n","      ","return Fibonacci(X - 1) + Fibonacci(X - 2);","\n","   ","end if;","\n","end Fibonacci;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Subprogram call (no parameters)"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Subprogram call with named Parameters "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"Foo(Bar1 => Value, Baz => Value2)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Override specifier "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"overriding procedure Foo procedure Foo(obj : in Object)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Ensure that a subprogram definition does not override an existing one"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"not overriding procedure Foo(obj : in Object)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Pass by pointer"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"procedure Foo (B : in access Bar)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Pass by reference"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"procedure Foo(B : in Bar)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Inline "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"procedure Foo with Inline"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Using functions for a type unqualified.        "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"use type P.Foo;      -- Make primitive ops visible"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"                         "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"use all type P.Foo;  -- Make all ops visible for type"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Modifiable parameters    "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"procedure Foo(B : in out Bar)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Expression function      "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"function Foo return T is (expr)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Empty procedure          "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"procedure Foo is null;")))),(0,n.kt)("h2",c({},{id:"types"}),"Types"),(0,n.kt)("table",null,(0,n.kt)("tr",null,(0,n.kt)("td",null,"Statically sized array "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type Buffer is array(1 .. 128) of Integer;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Array Access "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A(i)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Multi-dimensional Array "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"Mat4 : array (1 .. 4, 1 .. 4) of Float;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Built-In Variable length array"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type Buffer is array(1 .. N) of Integer;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Semantic type "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type Microseconds is new Integer;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Range checks on type "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type My_Positive is range 1 .. 10;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Size "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"T'Size"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Alignment "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"T'Alignment"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Type Aliasing "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"subtype T is W;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Type parameterized by value (run-time)"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type S(T: t) is record -- ..."))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Enum range "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A'Range"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Membership test "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A in E"))),(0,n.kt)("tr",null,(0,n.kt)("td",null," "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A not in E"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Type invariant checks "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type T is new V with Type_Invariant => Expr(T)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Sum types "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type S is (T, V, W);"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Coercion (casting) "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A := B(C);"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Coercion with constraint check(casting) "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A := B'(C);")))),(0,n.kt)("h2",c({},{id:"object-oriented-programming"}),"Object-Oriented Programming"),(0,n.kt)("table",null,(0,n.kt)("tr",null,(0,n.kt)("td",null,"Class-like "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type T is private;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Abstract class "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type T is interface;","\n\n","function Foo(A : T) return V is abstract;","\n","procedure Bar(A: in out T) is abstract;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Subprogram call of object-like type"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A.B;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null," "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"B (A);"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Member access from pointer"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A.all.B; -- Explicit"))),(0,n.kt)("tr",null,(0,n.kt)("td",null," "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"A.B; -- Implicit"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Prevent copying "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type X is limited type;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Inheritance "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type Foo is Bar with null record;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Dynamic dispatch (virtual function call)"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"procedure Foo(A : T'Class)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Prevent implicit cast "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type T is new W;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Runtime type checking "),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"if A in T then end if;"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Passing parameter by base class"),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"procedure Foo(A : BaseClass'Class)"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Array-like indexing of user-defined type."),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type My_Container is tagged type with","\n   ","Constant_Indexing =>Foo","\n   ","Variable_Indexing => Bar","\n   ","-- Foo and Bar are functions defined on the type."))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Automatic dereference of a handle-type to the handle's contents."),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type Handle(Target: not null access Element) is with Implicit_Dereference => Element;","\n","-- Old usage, calling Foo A_Handle.Target.all.Foo","\n","-- New usage A_Handle.Foo","\n"))),(0,n.kt)("tr",null,(0,n.kt)("td",null,"Iterator for loops for user-defined types."),(0,n.kt)("td",null,(0,n.kt)(k.Z,{mdxType:"CodeBlock"},"type My_Container with Default_Iterator => Iterate, Iterator_Element => Element_Type;","\n","type Cursor;","\n","\n","function First (M : in My_Container) return Cursor;","\n","procedure Next (C : in out Cursor);","\n","function Has_Element (C : in Cursor) return Boolean;","\n")))))}C.isMDXComponent=!0}}]);