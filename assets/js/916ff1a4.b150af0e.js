"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[5145],{71138:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>r,contentTitle:()=>l,default:()=>h,frontMatter:()=>o,metadata:()=>a,toc:()=>d});const a=JSON.parse('{"id":"style-guide/s5/07","title":"5.7 Visibility","description":"As noted in Guideline 4.2, Ada\'s ability to enforce information hiding","source":"@site/docs/style-guide/s5/07.mdx","sourceDirName":"style-guide/s5","slug":"/style-guide/s5/07","permalink":"/docs/style-guide/s5/07","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"title":"5.7 Visibility"},"sidebar":"styleGuideSidebar","previous":{"title":"5.6 Statements","permalink":"/docs/style-guide/s5/06"},"next":{"title":"5.8 Using exceptions","permalink":"/docs/style-guide/s5/08"}}');var t=i(74848),s=i(28453);const o={title:"5.7 Visibility"},l=void 0,r={},d=[{value:"The Use Clause",id:"the-use-clause",level:3},{value:"guideline",id:"guideline",level:4},{value:"example",id:"example",level:4},{value:"rationale",id:"rationale",level:4},{value:"notes",id:"notes",level:4},{value:"automation notes",id:"automation-notes",level:4},{value:"The Renames Clause",id:"the-renames-clause",level:3},{value:"guideline",id:"guideline-1",level:4},{value:"example",id:"example-1",level:4},{value:"rationale",id:"rationale-1",level:4},{value:"notes",id:"notes-1",level:4},{value:"Overloaded Subprograms",id:"overloaded-subprograms",level:3},{value:"guideline",id:"guideline-2",level:4},{value:"example",id:"example-2",level:4},{value:"rationale",id:"rationale-2",level:4},{value:"notes",id:"notes-2",level:4},{value:"Overloaded Operators",id:"overloaded-operators",level:3},{value:"guideline",id:"guideline-3",level:4},{value:"example",id:"example-3",level:4},{value:"rationale",id:"rationale-3",level:4},{value:"notes",id:"notes-3",level:4},{value:"Overloading the Equality Operator",id:"overloading-the-equality-operator",level:3},{value:"guideline",id:"guideline-4",level:4},{value:"rationale",id:"rationale-4",level:4}];function c(e){const n={a:"a",code:"code",em:"em",h3:"h3",h4:"h4",li:"li",p:"p",pre:"pre",ul:"ul",...(0,s.R)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsxs)(n.p,{children:["As noted in Guideline 4.2, Ada's ability to enforce information hiding\nand separation of concerns through its visibility controlling features\nis one of the most important advantages of the language. Subverting\nthese features, for example, by too liberal use of the ",(0,t.jsx)(n.code,{children:"use"})," clause, is\nwasteful and dangerous."]}),"\n",(0,t.jsx)(n.h3,{id:"the-use-clause",children:"The Use Clause"}),"\n",(0,t.jsx)(n.h4,{id:"guideline",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["When you need to provide visibility to operators, use the ",(0,t.jsx)(n.code,{children:"use type"}),"\nclause."]}),"\n",(0,t.jsxs)(n.li,{children:["Avoid/minimize the use of the ",(0,t.jsx)(n.code,{children:"use"})," clause (Nissen and Wallis 1984)."]}),"\n",(0,t.jsxs)(n.li,{children:["Consider using a package ",(0,t.jsx)(n.code,{children:"renames"})," clause rather than a ",(0,t.jsx)(n.code,{children:"use"})," clause\nfor a package."]}),"\n",(0,t.jsxs)(n.li,{children:["Consider using the ",(0,t.jsx)(n.code,{children:"use"})," clause in the following situations:","\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"When standard packages are needed and no ambiguous references\nare introduced"}),"\n",(0,t.jsx)(n.li,{children:"When references to enumeration literals are needed"}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["Localize the effect of all ",(0,t.jsx)(n.code,{children:"use"})," clauses."]}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"example",children:"example"}),"\n",(0,t.jsxs)(n.p,{children:["This is a modification of the example from Guideline 4.2.3. The effect\nof a ",(0,t.jsx)(n.code,{children:"use"})," clause is localized:"]}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:'----------------------------------------------------------------------------------\npackage Rational_Numbers is\n   type Rational is private;\n   function "=" (X, Y : Rational) return Boolean;\n   function "/" (X, Y : Integer)  return Rational;  -- construct a rational number\n   function "+" (X, Y : Rational) return Rational;\n   function "-" (X, Y : Rational) return Rational;\n   function "*" (X, Y : Rational) return Rational;\n   function "/" (X, Y : Rational) return Rational;  -- rational division\nprivate\n   ...\nend Rational_Numbers;\n----------------------------------------------------------------------------------\npackage body Rational_Numbers is\n   procedure Reduce (R : in out Rational) is . . . end Reduce;\n   . . .\nend Rational_Numbers;\n----------------------------------------------------------------------------------\npackage Rational_Numbers.IO is\n   ...\n\n   procedure Put (R : in  Rational);\n   procedure Get (R : out Rational);\nend Rational_Numbers.IO;\n----------------------------------------------------------------------------------\nwith Rational_Numbers;\nwith Rational_Numbers.IO;\nwith Ada.Text_IO;\nprocedure Demo_Rationals is\n   package R_IO renames Rational_Numbers.IO;\n\n   use type Rational_Numbers.Rational;\n   use R_IO;\n   use Ada.Text_IO;\n\n   X : Rational_Numbers.Rational;\n   Y : Rational_Numbers.Rational;\nbegin  -- Demo_Rationals\n   Put ("Please input two rational numbers: ");\n   Get (X);\n   Skip_Line;\n   Get (Y);\n   Skip_Line;\n   Put ("X / Y = ");\n   Put (X / Y);\n   New_Line;\n   Put ("X * Y = ");\n   Put (X * Y);\n   New_Line;\n   Put ("X + Y = ");\n   Put (X + Y);\n   New_Line;\n   Put ("X - Y = ");\n   Put (X - Y);\n   New_Line;\nend Demo_Rationals;\n'})}),"\n",(0,t.jsx)(n.h4,{id:"rationale",children:"rationale"}),"\n",(0,t.jsxs)(n.p,{children:["These guidelines allow you to maintain a careful balance between\nmaintainability and readability. Use of the ",(0,t.jsx)(n.code,{children:"use"})," clause may indeed make\nthe code read more like prose text. However, the maintainer may also\nneed to resolve references and identify ambiguous operations. In the\nabsence of tools to resolve these references and identify the impact of\nchanging use clauses, fully qualified names are the best alternative."]}),"\n",(0,t.jsxs)(n.p,{children:["Avoiding the ",(0,t.jsx)(n.code,{children:"use"})," clause forces you to use fully qualified names. In\nlarge systems, there may be many library units named in ",(0,t.jsx)(n.code,{children:"with"})," clauses.\nWhen corresponding ",(0,t.jsx)(n.code,{children:"use"})," clauses accompany the ",(0,t.jsx)(n.code,{children:"with"})," clauses and the\nsimple names of the library packages are omitted (as is allowed by the\n",(0,t.jsx)(n.code,{children:"use"})," clause), references to external entities are obscured and\nidentification of external dependencies becomes difficult."]}),"\n",(0,t.jsxs)(n.p,{children:["In some situations, the benefits of the ",(0,t.jsx)(n.code,{children:"use"})," clause are clear. A\nstandard package can be used with the obvious assumption that the reader\nis very familiar with those packages and that additional overloading\nwill not be introduced."]}),"\n",(0,t.jsxs)(n.p,{children:["The ",(0,t.jsx)(n.code,{children:"use type"})," clause makes both infix and prefix operators visible\nwithout the need for ",(0,t.jsx)(n.code,{children:"renames"})," clauses. You enhance readability with the\n",(0,t.jsx)(n.code,{children:"use type"})," clause because you can write statements using the more\nnatural infix notation for operators. See also Guideline 5.7.2."]}),"\n",(0,t.jsxs)(n.p,{children:["You can minimize the scope of the ",(0,t.jsx)(n.code,{children:"use"})," clause by placing it in the body\nof a package or subprogram or by encapsulating it in a block to restrict\nvisibility."]}),"\n",(0,t.jsx)(n.h4,{id:"notes",children:"notes"}),"\n",(0,t.jsxs)(n.p,{children:["Avoiding the ",(0,t.jsx)(n.code,{children:"use"})," clause completely can cause problems with enumeration\nliterals, which must then be fully qualified. This problem can be solved\nby declaring constants with the enumeration literals as their values,\nexcept that such constants cannot be overloaded like enumeration\nliterals."]}),"\n",(0,t.jsx)(n.p,{children:"An argument defending the use clause can be found in Rosen (1987)."}),"\n",(0,t.jsx)(n.h4,{id:"automation-notes",children:"automation notes"}),"\n",(0,t.jsxs)(n.p,{children:["There are tools that can analyze your Ada source code, resolve\noverloading of names, and automatically convert between the ",(0,t.jsx)(n.code,{children:"use"})," clause\nor fully qualified names."]}),"\n",(0,t.jsx)(n.h3,{id:"the-renames-clause",children:"The Renames Clause"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-1",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Limit the scope of a renaming declaration to the minimum necessary\nscope."}),"\n",(0,t.jsx)(n.li,{children:"Rename a long, fully qualified name to reduce the complexity if it\nbecomes unwieldy (see Guideline 3.1.4)."}),"\n",(0,t.jsx)(n.li,{children:"Use renaming to provide the body of a subprogram if this subprogram\nmerely calls the first subprogram."}),"\n",(0,t.jsx)(n.li,{children:"Rename declarations for visibility purposes rather than using the\nuse clause, except for operators (see Guideline 5.7.1)."}),"\n",(0,t.jsx)(n.li,{children:"Rename parts when your code interfaces to reusable components\noriginally written with nondescriptive or inapplicable nomenclature."}),"\n",(0,t.jsx)(n.li,{children:"Use a project-wide standard list of abbreviations to rename common\npackages."}),"\n",(0,t.jsxs)(n.li,{children:["Provide a ",(0,t.jsx)(n.code,{children:"use type"})," rather than a ",(0,t.jsx)(n.code,{children:"renames"})," clause to provide\nvisibility to operators."]}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"example-1",children:"example"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:"procedure Disk_Write (Track_Name : in     Track;\n                      Item       : in     Data) renames\n   System_Specific.Device_Drivers.Disk_Head_Scheduler.Transmit;\n"})}),"\n",(0,t.jsxs)(n.p,{children:["See also the example in Guideline 5.7.1, where a package-level ",(0,t.jsx)(n.code,{children:"renames"}),"\nclause provides an abbreviation for the package ",(0,t.jsx)(n.code,{children:"Rational_Numbers_IO"}),"."]}),"\n",(0,t.jsx)(n.h4,{id:"rationale-1",children:"rationale"}),"\n",(0,t.jsxs)(n.p,{children:["If the renaming facility is abused, the code can be difficult to read. A\n",(0,t.jsx)(n.code,{children:"renames"})," clause can substitute an abbreviation for a qualifier or long\npackage name locally. This can make code more readable yet anchor the\ncode to the full name. You can use the ",(0,t.jsx)(n.code,{children:"renames"})," clause to evaluate a\ncomplex name once or to provide a new ",(0,t.jsx)(n.em,{children:'"view"'})," of an object (regardless\nof whether it is tagged). However, the use of ",(0,t.jsx)(n.code,{children:"renames"})," clauses can\noften be avoided or made obviously undesirable by carefully choosing\nnames so that fully qualified names read well."]}),"\n",(0,t.jsxs)(n.p,{children:["When a subprogram body calls another subprogram without adding local\ndata or other algorithmic content, it is more readable to have this\nsubprogram body rename the subprogram that actually does the work. Thus,\nyou avoid having to write code to ",(0,t.jsx)(n.em,{children:'"pass through"'})," a subprogram call\n(Rationale 1995, \xa7II.12)."]}),"\n",(0,t.jsxs)(n.p,{children:["The list of renaming declarations serves as a list of abbreviation\ndefinitions (see Guideline 3.1.4). As an alternative, you can rename a\npackage at the library level to define project-wide abbreviations for\npackages and then ",(0,t.jsx)(n.code,{children:"with"})," the renamed packages. Often the parts recalled\nfrom a reuse library do not have names that are as general as they could\nbe or that match the new application's naming scheme. An interface\npackage exporting the renamed subprograms can map to your project's\nnomenclature. See also Guideline 5.7.1."]}),"\n",(0,t.jsxs)(n.p,{children:["The method described in the ",(0,t.jsx)(n.a,{href:"http://www.adahome.com/rm95/rm9x-08-05.html",children:"Ada Reference Manual\n(1995)"})," for renaming a type\nis to use a subtype (see Guideline 3.4.1)."]}),"\n",(0,t.jsxs)(n.p,{children:["The ",(0,t.jsx)(n.code,{children:"use type"})," clause eliminates the need for renaming infix operators.\nBecause you no longer need to rename each operator explicitly, you avoid\nerrors such as renaming a ",(0,t.jsx)(n.code,{children:"+"})," to a ",(0,t.jsx)(n.code,{children:"-"}),". See also Guideline 5.7.1."]}),"\n",(0,t.jsx)(n.h4,{id:"notes-1",children:"notes"}),"\n",(0,t.jsxs)(n.p,{children:["You should choose package names to be minimally meaningful, recognizing\nthat package names will be widely used as prefixes (e.g.,\n",(0,t.jsx)(n.code,{children:"Pkg.Operation"})," or ",(0,t.jsx)(n.code,{children:"Object : Pkg.Type_Name;"}),"). If you rename every\npackage to some abbreviation, you defeat the purpose of choosing\nmeaningful names, and it becomes hard to keep track of what all the\nabbreviations represent."]}),"\n",(0,t.jsxs)(n.p,{children:["For upward compatibility of Ada 83 programs in an Ada 95 environment,\nthe environment includeslibrary-level renamings of the Ada 83 library\nlevel packages (",(0,t.jsx)(n.a,{href:"http://www.adahome.com/rm95/rm9x-J-01.html",children:"Ada Reference Manual 1995,\n\xa7J.1"}),"). It is not\nrecommended that you use these renamings in Ada 95 code."]}),"\n",(0,t.jsx)(n.h3,{id:"overloaded-subprograms",children:"Overloaded Subprograms"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-2",children:"guideline"}),"\n",(0,t.jsx)(n.p,{children:"Limit overloading to widely used subprograms that perform similar\nactions on arguments of different types (Nissen and Wallis 1984)."}),"\n",(0,t.jsx)(n.h4,{id:"example-2",children:"example"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:"function Sin (Angles : in     Matrix_Of_Radians) return Matrix;\nfunction Sin (Angles : in     Vector_Of_Radians) return Vector;\nfunction Sin (Angle  : in     Radians)           return Small_Real;\nfunction Sin (Angle  : in     Degrees)           return Small_Real;\n"})}),"\n",(0,t.jsx)(n.h4,{id:"rationale-2",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"Excessive overloading can be confusing to maintainers (Nissen and Wallis\n1984, 65). There is also the danger of hiding declarations if\noverloading becomes habitual. Attempts to overload an operation may\nactually hide the original operation if the parameter profile is not\ndistinct. From that point on, it is not clear whether invoking the new\noperation is what the programmer intended or whether the programmer\nintended to invoke the hidden operation and accidentally hid it."}),"\n",(0,t.jsx)(n.h4,{id:"notes-2",children:"notes"}),"\n",(0,t.jsx)(n.p,{children:"This guideline does not prohibit subprograms with identical names\ndeclared in different packages."}),"\n",(0,t.jsx)(n.h3,{id:"overloaded-operators",children:"Overloaded Operators"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-3",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Preserve the conventional meaning of overloaded operators (Nissen\nand Wallis 1984)."}),"\n",(0,t.jsxs)(n.li,{children:['Use "',(0,t.jsx)(n.code,{children:"+"}),'" to identify adding, joining, increasing, and enhancing\nkinds of functions.']}),"\n",(0,t.jsxs)(n.li,{children:['Use "',(0,t.jsx)(n.code,{children:"-"}),'" to identify subtraction, separation, decreasing, and\ndepleting kinds of functions.']}),"\n",(0,t.jsx)(n.li,{children:"Use operator overloading sparingly and uniformly when applied to\ntagged types."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"example-3",children:"example"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:'function "+" (X : in     Matrix;\n              Y : in     Matrix)\n  return Matrix;\n...\nSum := A + B;\n'})}),"\n",(0,t.jsx)(n.h4,{id:"rationale-3",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"Subverting the conventional interpretation of operators leads to\nconfusing code."}),"\n",(0,t.jsx)(n.p,{children:"The advantage of operator overloading is that the code can become more\nclear and written more compactly (and readably) when it is used. This\ncan make the semantics simple and natural. However, it can be easy to\nmisunderstand the meaning of an overloaded operator, especially when\napplied to descendants. This is especially true if the programmer has\nnot applied natural semantics. Thus, do not use overloading if it cannot\nbe used uniformly and if it is easily misunderstood."}),"\n",(0,t.jsx)(n.h4,{id:"notes-3",children:"notes"}),"\n",(0,t.jsxs)(n.p,{children:["There are potential problems with any overloading. For example, if there\nare several versions of the ",(0,t.jsx)(n.code,{children:'"+"'})," operator and a change to one of them\naffects the number or order of its parameters, locating the occurrences\nthat must be changed can be difficult."]}),"\n",(0,t.jsx)(n.h3,{id:"overloading-the-equality-operator",children:"Overloading the Equality Operator"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-4",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Define an appropriate equality operator for private types."}),"\n",(0,t.jsx)(n.li,{children:"Consider redefining the equality operator for a private type."}),"\n",(0,t.jsx)(n.li,{children:"When overloading the equality operator for types, maintain the\nproperties of an algebraic equivalence relation."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"rationale-4",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"The predefined equality operation provided with private types depends on\nthe data structure chosen to implement that type . If access types are\nused, then equality will mean the operands have the same pointer value.\nIf discrete types are used, then equality will mean the operands have\nthe same value. If a floating- point type is used, then equality is\nbased on Ada model intervals (see Guideline 7.2.7). You should,\ntherefore, redefine equality to provide the meaning expected by the\nclient. If you implement a private type using an access type, you should\nredefine equality to provide a deep equality. For floating-point types,\nyou may want to provide an equality that tests for equality within some\napplication-dependent epsilon value."}),"\n",(0,t.jsx)(n.p,{children:"Any assumptions about the meaning of equality for private types will\ncreate a dependency on the implementation of that type. See Gonzalez\n(1991) for a detailed discussion."}),"\n",(0,t.jsxs)(n.p,{children:['When the definition of "',(0,t.jsx)(n.code,{children:"="}),'" is provided, there is a conventional\nalgebraic meaning implied by this symbol. As described in Baker (1991),\nthe following properties should remain true for the equality operator:']}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["Reflexive: ",(0,t.jsx)(n.code,{children:"a = a"})]}),"\n",(0,t.jsxs)(n.li,{children:["Symmetric: ",(0,t.jsx)(n.code,{children:"a = b ==> b = a"})]}),"\n",(0,t.jsxs)(n.li,{children:["Transitive:",(0,t.jsx)(n.code,{children:"a = b and b = c ==> a = c"})]}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.p,{children:["In redefining equality, you are not required to have a result type of\n",(0,t.jsx)(n.code,{children:"Standard.Boolean"}),". The Rationale (1995, \xa76.3) gives two examples where\nyour result type is a user-defined type. In a three-valued logic\nabstraction, you redefine equality to return one of ",(0,t.jsx)(n.code,{children:"True"}),", ",(0,t.jsx)(n.code,{children:"False"}),", or\n",(0,t.jsx)(n.code,{children:"Unknown"}),". In a vector processing application, you can define a\ncomponent-wise equality operator that returns a vector of Boolean\nvalues. In both these instances, you should also redefine inequality\nbecause it is not the Boolean complement of the equality function."]})]})}function h(e={}){const{wrapper:n}={...(0,s.R)(),...e.components};return n?(0,t.jsx)(n,{...e,children:(0,t.jsx)(c,{...e})}):c(e)}}}]);