"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[1832],{38872:(e,a,n)=>{n.r(a),n.d(a,{assets:()=>s,contentTitle:()=>r,default:()=>p,frontMatter:()=>o,metadata:()=>l,toc:()=>u});var t=n(58168),i=(n(96540),n(15680));const o={title:"7.2 Numeric Types and Expressions"},r=void 0,l={unversionedId:"style-guide/s7/02",id:"style-guide/s7/02",title:"7.2 Numeric Types and Expressions",description:"A great deal of care was taken with the design of the Ada features",source:"@site/docs/style-guide/s7/02.mdx",sourceDirName:"style-guide/s7",slug:"/style-guide/s7/02",permalink:"/docs/style-guide/s7/02",draft:!1,tags:[],version:"current",frontMatter:{title:"7.2 Numeric Types and Expressions"},sidebar:"styleGuideSidebar",previous:{title:"7.1 Fundamentals",permalink:"/docs/style-guide/s7/01"},next:{title:"7.3 Tasking",permalink:"/docs/style-guide/s7/03"}},s={},u=[{value:"Predefined Numeric Types",id:"predefined-numeric-types",level:3},{value:"guideline",id:"guideline",level:4},{value:"example",id:"example",level:4},{value:"rationale",id:"rationale",level:4},{value:"notes",id:"notes",level:4},{value:"Accuracy Model",id:"accuracy-model",level:3},{value:"guideline",id:"guideline-1",level:4},{value:"rationale",id:"rationale-1",level:4},{value:"Accuracy Analysis",id:"accuracy-analysis",level:3},{value:"guideline",id:"guideline-2",level:4},{value:"rationale",id:"rationale-2",level:4},{value:"Accuracy Constraints",id:"accuracy-constraints",level:3},{value:"guideline",id:"guideline-3",level:4},{value:"rationale",id:"rationale-3",level:4},{value:"Comments",id:"comments",level:3},{value:"guideline",id:"guideline-4",level:4},{value:"rationale",id:"rationale-4",level:4},{value:"Subexpression Evaluation",id:"subexpression-evaluation",level:3},{value:"guideline",id:"guideline-5",level:4},{value:"example",id:"example-1",level:4},{value:"rationale",id:"rationale-5",level:4},{value:"Relational Tests",id:"relational-tests",level:3},{value:"guideline",id:"guideline-6",level:4},{value:"example",id:"example-2",level:4},{value:"rationale",id:"rationale-6",level:4},{value:"notes",id:"notes-1",level:4},{value:"Decimal Types and the Information Systems Annex",id:"decimal-types-and-the-information-systems-annex",level:3},{value:"guideline",id:"guideline-7",level:4},{value:"example",id:"example-3",level:4},{value:"rationale",id:"rationale-7",level:4},{value:"Storage Control",id:"storage-control",level:3},{value:"Representation Clause",id:"representation-clause",level:3},{value:"guideline",id:"guideline-8",level:4},{value:"rationale",id:"rationale-8",level:4},{value:"notes",id:"notes-2",level:4},{value:"Access-to-Subprogram Values",id:"access-to-subprogram-values",level:3},{value:"guideline",id:"guideline-9",level:4},{value:"rationale",id:"rationale-9",level:4},{value:"exceptions",id:"exceptions",level:4},{value:"Storage Pool Mechanisms",id:"storage-pool-mechanisms",level:3},{value:"guideline",id:"guideline-10",level:4},{value:"example",id:"example-4",level:4}],d={toc:u},c="wrapper";function p(e){let{components:a,...n}=e;return(0,i.yg)(c,(0,t.A)({},d,n,{components:a,mdxType:"MDXLayout"}),(0,i.yg)("p",null,"A great deal of care was taken with the design of the Ada features\nrelated to numeric computations to ensure that the language could be\nused in embedded systems and mathematical applications where precision\nwas important. As far as possible, these features were made portable.\nHowever, there is an inevitable tradeoff between maximally exploiting\nthe available precision of numeric computation on a particular machine\nand maximizing the portability of Ada numeric constructs. This means\nthat these Ada features, particularly numeric types and expressions,\nmust be used with great care if full portability of the resulting\nprogram is to be guaranteed."),(0,i.yg)("h3",{id:"predefined-numeric-types"},"Predefined Numeric Types"),(0,i.yg)("h4",{id:"guideline"},"guideline"),(0,i.yg)("ul",null,(0,i.yg)("li",{parentName:"ul"},"Avoid using the predefined numeric types in package Standard. Use\nrange and digits declarations and let the implementation pick the\nappropriate representation."),(0,i.yg)("li",{parentName:"ul"},"For programs that require greater accuracy than that provided by the\nglobal assumptions, define a package that declares a private type\nand operations as needed; see Pappas (1985) for a full explanation\nand examples."),(0,i.yg)("li",{parentName:"ul"},"Consider using predefined numeric types (Integer, Natural, Positive)\nfor:",(0,i.yg)("ul",{parentName:"li"},(0,i.yg)("li",{parentName:"ul"},"Indexes into arrays where the index type is not significant,\nsuch as type String"),(0,i.yg)("li",{parentName:"ul"},'"Pure" numbers, that is, numbers with no associated physical\nunit (e.g., exponents)'),(0,i.yg)("li",{parentName:"ul"},"Values whose purpose is to control a repeat or iteration count")))),(0,i.yg)("h4",{id:"example"},"example"),(0,i.yg)("p",null,"The second and third examples below are not representable as subranges\nof Integer on a machine with a 16-bit word. The first example below\nallows a compiler to choose a multiword representation, if necessary."),(0,i.yg)("p",null,"Use:"),(0,i.yg)("pre",null,(0,i.yg)("code",{parentName:"pre",className:"language-ada"},"type    Second_Of_Day is             range 0 .. 86_400;\n")),(0,i.yg)("p",null,"rather than:"),(0,i.yg)("pre",null,(0,i.yg)("code",{parentName:"pre",className:"language-ada"},"type    Second_Of_Day is new Integer range 1 .. 86_400;\n")),(0,i.yg)("p",null,"or:"),(0,i.yg)("pre",null,(0,i.yg)("code",{parentName:"pre",className:"language-ada"},"subtype Second_Of_Day is     Integer range 1 .. 86_400;\n")),(0,i.yg)("h4",{id:"rationale"},"rationale"),(0,i.yg)("p",null,"An implementor is free to define the range of the predefined numeric\ntypes. Porting code from an implementation with greater accuracy to one\nof lesser accuracy is a time consuming and error-prone process. Many of\nthe errors are not reported until run-time."),(0,i.yg)("p",null,"This applies to more than just numerical computation. An\neasy-to-overlook instance of this problem occurs if you neglect to use\nexplicitly declared types for integer discrete ranges (array sizes, loop\nranges, etc.) (see Guidelines 5.5.1 and 5.5.2). If you do not provide an\nexplicit type when specifying index constraints and other discrete\nranges, a predefined integer type is assumed."),(0,i.yg)("p",null,"The predefined numeric types are useful when you use them wisely. You\nshould not use them to avoid declaring numeric types\u2014then you lose the\nbenefits of strong typing. When your application deals with different\nkinds of quantities and units, you should definitely separate them\nthrough the use of distinct numeric types. However, if you are simply\ncounting the number of iterations in an iterative approximation\nalgorithm, declaring a special integer type is probably overkill. The\npredefined exponentiation operators ","*","*"," require an integer as the type\nof its right operand."),(0,i.yg)("p",null,"You should use the predefined types Natural and Positive for\nmanipulating certain kinds of values in the predefined language\nenvironment. The types String and Wide_String use an index of type\nPositive. If your code indexes into a string using an incompatible\ninteger type, you will be forced to do type conversion, reducing its\nreadability. If you are performing operations like slices and\nconcatenation, the subtype of your numeric array index is probably\ninsignificant and you are better off using a predefined subtype. On the\nother hand, if your array represents a table (e.g., a hash table), then\nyour index subtype is significant, and you should declare a distinct\nindex type."),(0,i.yg)("h4",{id:"notes"},"notes"),(0,i.yg)("p",null,"There is an alternative that this guideline permits. As Guideline 7.1.5\nsuggests, implementation dependencies can be encapsulated in packages\nintended for that purpose. This could include the definition of a 32-bit\ninteger type. It would then be possible to derive additional types from\nthat 32-bit type."),(0,i.yg)("h3",{id:"accuracy-model"},"Accuracy Model"),(0,i.yg)("h4",{id:"guideline-1"},"guideline"),(0,i.yg)("ul",null,(0,i.yg)("li",{parentName:"ul"},"Use an implementation that supports the Numerics Annex (Ada\nReference Manual 1995, Annex G) when performance and accuracy are\noverriding concerns.")),(0,i.yg)("h4",{id:"rationale-1"},"rationale"),(0,i.yg)("p",null,'The Numerics Annex defines the accuracy and performance requirements for\nfloating- and fixed-point arithmetic. The Annex provides a "strict" mode\nin which the compiler must support these requirements. To guarantee that\nyour program\'s numerical performance is portable, you should compile and\nlink in the strict mode. If your program relies upon the numeric\nproperties of the strict mode, then it will only be portable to other\nenvironments that support the strict numerics mode.'),(0,i.yg)("p",null,"The accuracy of floating-point numbers is based on what machine numbers\ncan be represented exactly in storage. A computational result in a\nregister can fall between two machine numbers when the register contains\nmore bits than storage. You can step through the machine numbers using\nthe attributes 'Pred and 'Succ. Other attributes return values of the\nmantissa, exponent, radix, and other characteristics of floating- and\nfixed-point numbers."),(0,i.yg)("h3",{id:"accuracy-analysis"},"Accuracy Analysis"),(0,i.yg)("h4",{id:"guideline-2"},"guideline"),(0,i.yg)("ul",null,(0,i.yg)("li",{parentName:"ul"},"Carefully analyze what accuracy and precision you really need.")),(0,i.yg)("h4",{id:"rationale-2"},"rationale"),(0,i.yg)("p",null,'Floating-point calculations are done with the equivalent of the\nimplementation\'s predefined floating-point types. The effect of extra\n"guard" digits in internal computations can sometimes lower the number\nof digits that must be specified in an Ada declaration. This may not be\nconsistent over implementations where the program is intended to be run.\nIt may also lead to the false conclusion that the declared types are\nsufficient for the accuracy required.'),(0,i.yg)("p",null,"You should choose the numeric type declarations to satisfy the lowest\nprecision (smallest number of digits) that will provide the required\naccuracy. Careful analysis will be necessary to show that the\ndeclarations are adequate. When you move to a machine with less\nprecision, you probably can use the same type declaration."),(0,i.yg)("h3",{id:"accuracy-constraints"},"Accuracy Constraints"),(0,i.yg)("h4",{id:"guideline-3"},"guideline"),(0,i.yg)("ul",null,(0,i.yg)("li",{parentName:"ul"},"Do not press the accuracy limits of the machine(s).")),(0,i.yg)("h4",{id:"rationale-3"},"rationale"),(0,i.yg)("p",null,"Just because two different machines use the same number of digits in the\nmantissa of a floating-point number does not imply they will have the\nsame arithmetic properties. Some Ada implementations may give slightly\nbetter accuracy than required by Ada because they make efficient use of\nthe machine. Do not write programs that depend on this."),(0,i.yg)("h3",{id:"comments"},"Comments"),(0,i.yg)("h4",{id:"guideline-4"},"guideline"),(0,i.yg)("ul",null,(0,i.yg)("li",{parentName:"ul"},"Comment the analysis and derivation of the numerical aspects of a\nprogram.")),(0,i.yg)("h4",{id:"rationale-4"},"rationale"),(0,i.yg)("p",null,"Decisions and background about why certain precisions are required in a\nprogram are important to program revision or porting. The underlying\nnumerical analysis leading to the program should be commented."),(0,i.yg)("h3",{id:"subexpression-evaluation"},"Subexpression Evaluation"),(0,i.yg)("h4",{id:"guideline-5"},"guideline"),(0,i.yg)("ul",null,(0,i.yg)("li",{parentName:"ul"},"Anticipate the range of values of subexpressions to avoid exceeding\nthe underlying range of their base type. Use derived types,\nsubtypes, factoring, and range constraints on numeric types (see\nGuidelines 3.4.1, 5.3.1, and 5.5.3).")),(0,i.yg)("h4",{id:"example-1"},"example"),(0,i.yg)("p",null,"This example is adapted from the Rationale (1995, \xa73.3):"),(0,i.yg)("pre",null,(0,i.yg)("code",{parentName:"pre",className:"language-ada"},'with Ada.Text_IO;\nwith Ada.Integer_Text_IO;\nprocedure Demo_Overflow is\n-- assume the predefined type Integer has a 16-bit range\n   X : Integer := 24_000;\n   Y : Integer;\nbegin  -- Demo_Overflow\n   y := (3 * X) / 4;  -- raises Constraint_Error if the machine registers used are 16-bit\n  -- mathematically correct intermediate result if 32-bit registers\n   Ada.Text_IO.Put ("(");\n   Ada.Integer_Text_IO.Put (X);\n   Ada.Text_IO.Put (" * 3 ) / 4 = ");\n   Ada.Integer_Text_IO.Put (Y);\nexception\n   when Constraint_Error =>\n      Ada.Text_IO.Put_Line ("3 * X too big for register!");\nend Demo_Overflow;\n')),(0,i.yg)("h4",{id:"rationale-5"},"rationale"),(0,i.yg)("p",null,"The Ada language does not require that an implementation perform range\nchecks on subexpressions within an expression. Ada does require that\noverflow checks be performed. Thus, depending on the order of evaluation\nand the size of the registers, a subexpression will either overflow or\nproduce the mathematically correct result. In the event of an overflow,\nyou will get the exception Constraint_Error. Even if the implementation\non your program's current target does not result in an overflow on a\nsubexpression evaluation, your program might be ported to an\nimplementation that does."),(0,i.yg)("h3",{id:"relational-tests"},"Relational Tests"),(0,i.yg)("h4",{id:"guideline-6"},"guideline"),(0,i.yg)("ul",null,(0,i.yg)("li",{parentName:"ul"},"Consider using \\<= and ",">","= to do relational tests on real valued\narguments, avoiding the \\<, ",">",", =, and /= operations."),(0,i.yg)("li",{parentName:"ul"},"Use values of type attributes in comparisons and checking for small\nvalues.")),(0,i.yg)("h4",{id:"example-2"},"example"),(0,i.yg)("p",null,'The following examples test for (1) absolute "equality" in storage, (2)\nabsolute "equality" in computation, (3) relative "equality" in storage,\nand (4) relative "equality" in computation:'),(0,i.yg)("pre",null,(0,i.yg)("code",{parentName:"pre",className:"language-ada"},"abs (X - Y) <= Float_Type'Model_Small                -- (1)\nabs (X - Y) <= Float_Type'Base'Model_Small           -- (2)\nabs (X - Y) <= abs X * Float_Type'Model_Epsilon      -- (3)\nabs (X - Y) <= abs X * Float_Type'Base'Model_Epsilon -- (4)\n")),(0,i.yg)("p",null,'And, specifically, for "equality" to 0:'),(0,i.yg)("pre",null,(0,i.yg)("code",{parentName:"pre",className:"language-ada"},"abs X <= Float_Type'Model_Small                      -- (1)\nabs X <= Float_Type'Base'Model_Small                 -- (2)\nabs X <= abs X * Float_Type'Model_Epsilon            -- (3)\nabs X <= abs X * Float_Type'Base'Model_Epsilon       -- (4)\n")),(0,i.yg)("h4",{id:"rationale-6"},"rationale"),(0,i.yg)("p",null,"Strict relational comparisons ( \\<, ",">",", =, /= ) are a general problem\nwith computations involving real numbers. Because of the way comparisons\nare defined in terms of model intervals, it is possible for the values\nof the comparisons to depend on the implementation. Within a model\ninterval, the result of comparing two values is nondeterministic if the\nvalues are not model numbers. In general, you should test for proximity\nrather than equality as shown in the examples. See also Rationale (1995,\n\xa7\xa7G.4.1 and G.4.2.)."),(0,i.yg)("p",null,"Type attributes are the primary means of symbolically accessing the\nimplementation of the Ada numeric model. When the characteristics of the\nmodel numbers are accessed by type attributes, the source code is\nportable. The appropriate model numbers of any implementation will then\nbe used by the generated code."),(0,i.yg)("p",null,"Although 0 is technically not a special case, it is often overlooked\nbecause it looks like the simplest and, therefore, safest case. But in\nreality, each time comparisons involve small values, you should evaluate\nthe situation to determine which technique is appropriate."),(0,i.yg)("h4",{id:"notes-1"},"notes"),(0,i.yg)("p",null,"Regardless of language, real-valued computations have inaccuracy. That\nthe corresponding mathematical operations have algebraic properties\nusually introduces some confusion. This guideline explains how Ada deals\nwith the problem that most languages face."),(0,i.yg)("h3",{id:"decimal-types-and-the-information-systems-annex"},"Decimal Types and the Information Systems Annex"),(0,i.yg)("h4",{id:"guideline-7"},"guideline"),(0,i.yg)("ul",null,(0,i.yg)("li",{parentName:"ul"},"In information systems, declare different numeric decimal types to\ncorrespond to different scales (Brosgol, Eachus, and Emery 1994)."),(0,i.yg)("li",{parentName:"ul"},"Create objects of different decimal types to reflect different units\nof measure (Brosgol, Eachus, and Emery 1994)."),(0,i.yg)("li",{parentName:"ul"},"Declare subtypes of the appropriately scaled decimal type to provide\nappropriate range constraints for application-specific types."),(0,i.yg)("li",{parentName:"ul"},"Encapsulate each measure category in a package (Brosgol, Eachus, and\nEmery 1994)."),(0,i.yg)("li",{parentName:"ul"},"Declare as few decimal types as possible for unitless data (Brosgol,\nEachus, and Emery 1994)."),(0,i.yg)("li",{parentName:"ul"},"For decimal calculations, determine whether the result should be\ntruncated toward 0 or rounded."),(0,i.yg)("li",{parentName:"ul"},"Avoid decimal types and arithmetic on compilers that do not support\nthe Information Systems Annex (Ada Reference Manual 1995, Annex F)\nin full.")),(0,i.yg)("h4",{id:"example-3"},"example"),(0,i.yg)("pre",null,(0,i.yg)("code",{parentName:"pre",className:"language-ada"},"-- The salary cap today is $500,000; however this can be expanded to $99,999,999.99.\ntype Executive_Salary is delta 0.01 digits 10 range 0 .. 500_000.00;\n\n------------------------------------------------------------------------------\npackage Currency is\n\n   type Dollars is delta 0.01 digits 12;\n\n   type Marks   is delta 0.01 digits 12;\n\n   type Yen     is delta 0.01 digits 12;\n\n   function To_Dollars (M : Marks) return Dollars;\n   function To_Dollars (Y : Yen)   return Dollars;\n\n   function To_Marks (D : Dollars) return Marks;\n   function To_Marks (Y : Yen)     return Marks;\n\n   function To_Yen (D : Dollars) return Yen;\n   function To_Yen (M : Marks)   return Yen;\n\nend Currency;\n")),(0,i.yg)("h4",{id:"rationale-7"},"rationale"),(0,i.yg)("p",null,"The Ada language does not provide any predefined decimal types.\nTherefore, you need to declare decimal types for the different scales\nyou will need to use. Differences in scale and precision must be\nconsidered in deciding whether or not a common type will suffice\n(Brosgol, Eachus, and Emery 1994)."),(0,i.yg)("p",null,"You need different types for objects measured in different units. This\nallows the compiler to detect mismatched values in expressions. If you\ndeclare all decimal objects to be of a single type, you forego the\nbenefits of strong typing. For example, in an application that involves\nseveral currencies, each currency should be declared as a separate type.\nYou should provide appropriate conversions between different currencies."),(0,i.yg)("p",null,"You should map data with no particular unit of measure to a small set of\ntypes or a single type to avoid the explosion of conversions between\nnumeric types."),(0,i.yg)("p",null,"Separate the range requirement on a decimal type from its precision,\ni.e., the number of significant digits required. From the point of view\nof planning for change and ease of maintenance, you can use the digit's\nvalue to accommodate future growth in the values to be stored in objects\nof the type. For example, you may want to anticipate growth for database\nvalues and report formats. You can constrain the values of the type\nthrough a range constraint that matches current needs. It is easier to\nmodify the range and avoid redefining databases and reports."),(0,i.yg)("p",null,"Ada automatically truncates toward 0. If your requirements are to round\nthe decimal result, you must explicitly do so using the 'Round\nattribute."),(0,i.yg)("p",null,"The core language defines the basic syntax of and operations on decimal\ntypes. It does not specify, however, the minimum number of significant\ndigits that must be supported. Nor does the core language require the\ncompiler to support values of Small other than powers of 2, thus\nenabling the compiler effectively to reject a decimal declaration (Ada\nReference Manual 1995, \xa73.5.9). The Information Systems Annex provides\nadditional support for decimal types. It requires a minimum of 18\nsignificant digits. It also specifies a Text_IO.Editing package that\nprovides support analogous to the COBOL picture approach."),(0,i.yg)("h3",{id:"storage-control"},"Storage Control"),(0,i.yg)("p",null,"The management of dynamic storage can vary between Ada environments. In\nfact, some environments do not provide any deallocation. The following\nAda storage control mechanisms are implementation-dependent and should\nbe used with care in writing portable programs."),(0,i.yg)("h3",{id:"representation-clause"},"Representation Clause"),(0,i.yg)("h4",{id:"guideline-8"},"guideline"),(0,i.yg)("ul",null,(0,i.yg)("li",{parentName:"ul"},"Do not use a representation clause to specify number of storage\nunits.")),(0,i.yg)("h4",{id:"rationale-8"},"rationale"),(0,i.yg)("p",null,"The meaning of the 'Storage_Size attribute is ambiguous; specifying a\nparticular value will not improve portability. It may or may not include\nspace allocated for parameters, data, etc. Save the use of this feature\nfor designs that must depend on a particular vendor's implementation."),(0,i.yg)("h4",{id:"notes-2"},"notes"),(0,i.yg)("p",null,"During a porting activity, it can be assumed that any occurrence of\nstorage specification indicates an implementation dependency that must\nbe redesigned."),(0,i.yg)("h3",{id:"access-to-subprogram-values"},"Access-to-Subprogram Values"),(0,i.yg)("h4",{id:"guideline-9"},"guideline"),(0,i.yg)("ul",null,(0,i.yg)("li",{parentName:"ul"},"Do not compare access-to-subprogram values.")),(0,i.yg)("h4",{id:"rationale-9"},"rationale"),(0,i.yg)("p",null,'The Ada Reference Manual (1995, \xa73.10.2) explains that an\n"implementation may consider two access-to-subprogram values to be\nunequal, even though they designate the same subprogram. This might be\nbecause one points directly to the subprogram, while the other points to\na special prologue that performs an Elaboration_Check and then jumps to\nthe subprogram." The Ada Reference Manual (1995, \xa74.5.2) states that it\nis "unspecified whether two access values that designate the same\nsubprogram but are the result of distinct evaluations of Access\nattribute references are equal or unequal."'),(0,i.yg)("p",null,"See also Guideline 5.3.4."),(0,i.yg)("h4",{id:"exceptions"},"exceptions"),(0,i.yg)("p",null,"If you must compare an access-to-subprogram value, you should define a\nconstant using the access-to-subprogram value and make all future\ncomparisons against the constant. However, if you attempt to compare\naccess-to-subprogram values with different levels of indirection, the\nvalues might still be unequal, even if designating the same subprogram."),(0,i.yg)("h3",{id:"storage-pool-mechanisms"},"Storage Pool Mechanisms"),(0,i.yg)("h4",{id:"guideline-10"},"guideline"),(0,i.yg)("ul",null,(0,i.yg)("li",{parentName:"ul"},"Consider using explicitly defined storage pool mechanisms.")),(0,i.yg)("h4",{id:"example-4"},"example"),(0,i.yg)("p",null,"See the Ada Reference Manual 1995, \xa713.11.2)."),(0,i.yg)("p",null,"You use allocators as before. Instead of using unchecked deallocation,\nyou maintain your own free lists of objects that are no longer in use\nand available for reuse."),(0,i.yg)("p",null,"You use allocators and possibly unchecked deallocation; however, you\nimplement a storage pool and associate it with the access type(s) via a\nStorage_Pool clause. You can use this technique to implement a\nmark/release storage management paradigm, which might be significantly\nfaster than an allocate/deallocate paradigm. Some vendors may provide a\nmark/release package as part of their Ada environment."),(0,i.yg)("p",null,"You do not use allocators, but instead use unchecked conversion from the\naddress and do all your own default initialization, etc. It is unlikely\nyou would use this last option because you lose automatic default\ninitialization."))}p.isMDXComponent=!0}}]);