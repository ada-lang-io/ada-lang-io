"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[6890],{70797:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>l,contentTitle:()=>s,default:()=>h,frontMatter:()=>o,metadata:()=>a,toc:()=>d});const a=JSON.parse('{"id":"style-guide/s8/04","title":"8.4 Independence","description":"A reusable part should be as independent as possible from other reusable","source":"@site/docs/style-guide/s8/04.mdx","sourceDirName":"style-guide/s8","slug":"/style-guide/s8/04","permalink":"/docs/style-guide/s8/04","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"title":"8.4 Independence"},"sidebar":"styleGuideSidebar","previous":{"title":"8.3 Adaptability","permalink":"/docs/style-guide/s8/03"},"next":{"title":"8.5 Summary","permalink":"/docs/style-guide/s8/05"}}');var t=i(74848),r=i(28453);const o={title:"8.4 Independence"},s=void 0,l={},d=[{value:"Subsystem Design",id:"subsystem-design",level:3},{value:"guideline",id:"guideline",level:4},{value:"rationale",id:"rationale",level:4},{value:"Using Generic Parameters to Reduce Coupling",id:"using-generic-parameters-to-reduce-coupling",level:3},{value:"guideline",id:"guideline-1",level:4},{value:"example",id:"example",level:4},{value:"rationale",id:"rationale-1",level:4},{value:"Coupling Due to Pragmas",id:"coupling-due-to-pragmas",level:3},{value:"guideline",id:"guideline-2",level:4},{value:"example",id:"example-1",level:4},{value:"rationale",id:"rationale-2",level:4},{value:"notes",id:"notes",level:4},{value:"Part Families",id:"part-families",level:3},{value:"guideline",id:"guideline-3",level:4},{value:"example",id:"example-2",level:4},{value:"rationale",id:"rationale-3",level:4},{value:"notes",id:"notes-1",level:4},{value:"Conditional Compilation",id:"conditional-compilation",level:3},{value:"guideline",id:"guideline-4",level:4},{value:"example",id:"example-3",level:4},{value:"rationale",id:"rationale-4",level:4},{value:"caution",id:"caution",level:4},{value:"Table-Driven Programming",id:"table-driven-programming",level:3},{value:"guideline",id:"guideline-5",level:4},{value:"example",id:"example-4",level:4},{value:"rationale",id:"rationale-5",level:4},{value:"notes",id:"notes-2",level:4},{value:"String Handling",id:"string-handling",level:3},{value:"guideline",id:"guideline-6",level:4},{value:"example",id:"example-5",level:4},{value:"rationale",id:"rationale-6",level:4},{value:"Tagged Type Hierarchies",id:"tagged-type-hierarchies",level:3},{value:"guideline",id:"guideline-7",level:4},{value:"example",id:"example-6",level:4},{value:"rationale",id:"rationale-7",level:4}];function c(e){const n={code:"code",h3:"h3",h4:"h4",li:"li",p:"p",pre:"pre",ul:"ul",...(0,r.R)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsx)(n.p,{children:'A reusable part should be as independent as possible from other reusable\nparts. A potential user is less inclined to reuse a part if that part\nrequires the use of other parts that seem unnecessary. The "extra\nbaggage" of the other parts wastes time and space. A user would like to\nbe able to reuse only that part that is perceived as useful. The concept\nof a "part" is intentionally vague here. A single package does not need\nto be independent of each other package in a reuse library if the\n"parts" from that library that are typically reused are entire\nsubsystems. If the entire subsystem is perceived as providing a useful\nfunction, the entire subsystem is reused. However, the subsystem should\nnot be tightly coupled to all the other subsystems in the reuse library\nso that it is difficult or impossible to reuse the subsystem without\nreusing the entire library. Coupling between reusable parts should only\noccur when it provides a strong benefit perceptible to the user.'}),"\n",(0,t.jsx)(n.h3,{id:"subsystem-design",children:"Subsystem Design"}),"\n",(0,t.jsx)(n.h4,{id:"guideline",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Consider structuring subsystems so that operations that are only\nused in a particular context are in different child packages than\noperations used in a different context."}),"\n",(0,t.jsx)(n.li,{children:"Consider declaring context-independent functionality in the parent\npackage and context-dependent functionality in child packages."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"rationale",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"The generic unit is a basic building block. Generic parameterization can\nbe used to break dependencies between program units so that they can be\nreused separately. However, it is often the case that a set of units,\nparticularly a set of packages, are to be reused together as a\nsubsystem. In this case, the packages can be collected into a hierarchy\nof child packages, with private packages to hide internal details. The\nhierarchy may or may not be generic. Using the child packages allows\nsubsystems to be reused without incorporating too many extraneous\noperations because the unused child packages can be discarded in the new\nenvironment."}),"\n",(0,t.jsx)(n.p,{children:"See also Guidelines 4.1.6 and 8.3.1."}),"\n",(0,t.jsx)(n.h3,{id:"using-generic-parameters-to-reduce-coupling",children:"Using Generic Parameters to Reduce Coupling"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-1",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Minimize with clauses on reusable parts, especially on their\nspecifications."}),"\n",(0,t.jsx)(n.li,{children:"Consider using generic parameters instead of with statements to\nreduce the number of context clauses on a reusable part."}),"\n",(0,t.jsx)(n.li,{children:"Consider using generic formal package parameters to import directly\nall the types and operations defined in an instance of a preexisting\ngeneric."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"example",children:"example"}),"\n",(0,t.jsx)(n.p,{children:"A procedure like the following:"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:"------------------------------------------------------------------------\nwith Package_A;\nprocedure Produce_And_Store_A is\n   ...\nbegin  -- Produce_And_Store_A\n   ...\n   Package_A.Produce (...);\n   ...\n   Package_A.Store (...);\n   ...\nend Produce_And_Store_A;\n------------------------------------------------------------------------\n"})}),"\n",(0,t.jsx)(n.p,{children:"can be rewritten as a generic unit:"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:"------------------------------------------------------------------------\ngeneric\n   with procedure Produce (...);\n   with procedure Store   (...);\nprocedure Produce_And_Store;\n------------------------------------------------------------------------\nprocedure Produce_And_Store is\n   ...\nbegin  -- Produce_And_Store\n   ...\n   Produce (...);\n   ...\n   Store   (...);\n   ...\nend Produce_And_Store;\n------------------------------------------------------------------------\n"})}),"\n",(0,t.jsx)(n.p,{children:"and then instantiated:"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:"------------------------------------------------------------------------\nwith Package_A;\nwith Produce_And_Store;\nprocedure Produce_And_Store_A is\n   new Produce_And_Store (Produce => Package_A.Produce,\n                          Store   => Package_A.Store);\n------------------------------------------------------------------------\n"})}),"\n",(0,t.jsx)(n.h4,{id:"rationale-1",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"Context (with) clauses specify the names of other units upon which this\nunit depends. Such dependencies cannot and should not be entirely\navoided, but it is a good idea to minimize the number of them that occur\nin the specification of a unit. Try to move them to the body, leaving\nthe specification independent of other units so that it is easier to\nunderstand in isolation. Also, organize your reusable parts in such a\nway that the bodies of the units do not contain large numbers of\ndependencies on each other. Partitioning your library into independent\nfunctional areas with no dependencies spanning the boundaries of the\nareas is a good way to start. Finally, reduce dependencies by using\ngeneric formal parameters instead of with statements, as shown in the\nexample above. If the units in a library are too tightly coupled, then\nno single part can be reused without reusing most or all of the library."}),"\n",(0,t.jsx)(n.p,{children:"The first (nongeneric) version of Produce_And_Store_A above is\ndifficult to reuse because it depends on Package_A that may not be\ngeneral purpose or generally available. If the operation\nProduce_And_Store has reuse potential that is reduced by this\ndependency, a generic unit and an instantiation should be produced as\nshown above. The with clause for Package_A has been moved from the\nProduce_And_Store generic procedure, which encapsulates the reusable\nalgorithm to the Produce_And_Store_A instantiation. Instead of naming\nthe package that provides the required operations, the generic unit\nsimply lists the required operations themselves. This increases the\nindependence and reusability of the generic unit."}),"\n",(0,t.jsx)(n.p,{children:"This use of generic formal parameters in place of with clauses also\nallows visibility at a finer granularity. The with clause on the\nnongeneric version of Produce_And_Store_A makes all of the contents\nof Package_A visible to Produce_And_Store_A, while the generic\nparameters on the generic version make only the Produce and Store\noperations available to the generic instantiation."}),"\n",(0,t.jsx)(n.p,{children:'Generic formal packages allow for "safer and simpler composition of\ngeneric abstractions" ( Rationale 1995, \xa712.6). The generic formal\npackage allows you to group a set of related types and their operations\ninto a single unit, avoiding having to list each type and operation as\nan individual generic formal parameter. This technique allows you to\nshow clearly that you are extending the functionality of one generic\nwith another generic, effectively parameterizing one abstraction with\nanother.'}),"\n",(0,t.jsx)(n.h3,{id:"coupling-due-to-pragmas",children:"Coupling Due to Pragmas"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-2",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"In the specification of a generic library unit, use pragma\nElaborate_Body."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"example-1",children:"example"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:"---------------------------------------------------------------------------\ngeneric\n   ...\npackage Stack is\n\n   pragma Elaborate_Body (Stack); -- in case the body is not yet elaborated\n\n   ...\nend Stack;\n---------------------------------------------------------------------------\nwith Stack;\npackage My_Stack is\n   new Stack (...);\n---------------------------------------------------------------------------\npackage body Stack is\nbegin\n   ...\nend Stack;\n---------------------------------------------------------------------------\n"})}),"\n",(0,t.jsx)(n.h4,{id:"rationale-2",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"The elaboration order of compilation units is only constrained to follow\nthe compilation order. Furthermore, any time you have an instantiation\nas a library unit or an instantiation in a library package, Ada requires\nthat you elaborate the body of the generic being instantiated before\nelaborating the instantiation itself. Because a generic library unit\nbody may be compiled after an instantiation of that generic, the body\nmay not necessarily be elaborated at the time of the instantiation,\ncausing a Program_Error. Using pragma Elaborate_Body avoids this by\nrequiring that the generic unit body be elaborated immediately after the\nspecification, whatever the compilation order."}),"\n",(0,t.jsx)(n.p,{children:"When there is clear requirement for a recursive dependency, you should\nuse pragma Elaborate_Body. This situation arises, for example, when you\nhave a recursive dependency (i.e., package A's body depends on package\nB's specification and package B's body depends on package A's\nspecification)."}),"\n",(0,t.jsx)(n.h4,{id:"notes",children:"notes"}),"\n",(0,t.jsx)(n.p,{children:"Pragma Elaborate_All controls the order of elaboration of one unit with\nrespect to another. This is another way of coupling units and should be\navoided when possible in reusable parts because it restricts the number\nof configurations in which the reusable parts can be combined.\nRecognize, however, that pragma Elaborate_All provides a better\nguarantee of elaboration order because if using this pragma uncovers\nelaboration problems, they will be reported at link time (as opposed to\na run-time execution error)."}),"\n",(0,t.jsx)(n.p,{children:"Any time you call a subprogram (typically a function) during the\nelaboration of a library unit, the body of the subprogram must have been\nelaborated before the library unit. You can ensure this elaboration\nhappens by adding a pragma Elaborate_Body for the unit containing the\nfunction. If, however, that function calls other functions, then it is\nsafer to put a pragma Elaborate_All on the unit containing the\nfunction."}),"\n",(0,t.jsx)(n.p,{children:"For a discussion of the pragmas Pure and Preelaborate, see also the Ada\nReference Manual (1995, \xa710.2.1) and the Rationale (1995, \xa710.3). If you\nuse either pragma Pure or Preelaborate, you will not need the pragma\nElaborate_Body."}),"\n",(0,t.jsx)(n.p,{children:'The idea of a registry is fundamental to many object-oriented\nprogramming frameworks. Because other library units will need to call it\nduring their elaboration, you need to make sure that the registry itself\nis elaborated early. Note that the registry should only depend on the\nroot types of the type hierarchies and that the registry should only\nhold "class-wide" pointers to the objects, not more specific pointers.\nThe root types should not themselves depend on the registry. See Chapter\n9 for a more complete discussion of the use of object-oriented features.'}),"\n",(0,t.jsx)(n.h3,{id:"part-families",children:"Part Families"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-3",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Create families of generic or other parts with similar\nspecifications."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"example-2",children:"example"}),"\n",(0,t.jsx)(n.p,{children:"The Booch parts (Booch 1987) are an example of the application of this\nguideline."}),"\n",(0,t.jsx)(n.h4,{id:"rationale-3",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"Different versions of similar parts (e.g., bounded versus unbounded\nstacks) may be needed for different applications or to change the\nproperties of a given application. Often, the different behaviors\nrequired by these versions cannot be obtained using generic parameters.\nProviding a family of parts with similar specifications makes it easy\nfor the programmer to select the appropriate one for the current\napplication or to substitute a different one if the needs of the\napplication change."}),"\n",(0,t.jsx)(n.h4,{id:"notes-1",children:"notes"}),"\n",(0,t.jsx)(n.p,{children:"A reusable part that is structured from subparts that are members of\npart families is particularly easy to tailor to the needs of a given\napplication by substitution of family members."}),"\n",(0,t.jsx)(n.p,{children:"Guideline 9.2.4 discusses the use of tagged types in building different\nversions of similar parts (i.e., common interface, multiple\nimplementations)."}),"\n",(0,t.jsx)(n.h3,{id:"conditional-compilation",children:"Conditional Compilation"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-4",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Structure reusable code to take advantage of dead code removal by\nthe compiler."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"example-3",children:"example"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:"------------------------------------------------------------------------\npackage Matrix_Math is\n   ...\n   type Algorithm is (Gaussian, Pivoting, Choleski, Tri_Diagonal);\n   generic\n      Which_Algorithm : in     Algorithm := Gaussian;\n   procedure Invert ( ... );\nend Matrix_Math;\n------------------------------------------------------------------------\npackage body Matrix_Math is\n   ...\n   ---------------------------------------------------------------------\n   procedure Invert ( ... ) is\n      ...\n   begin  -- Invert\n      case Which_Algorithm is\n         when Gaussian     => ... ;\n         when Pivoting     => ... ;\n         when Choleski     => ... ;\n         when Tri_Diagonal => ... ;\n      end case;\n   end Invert;\n   ---------------------------------------------------------------------\nend Matrix_Math;\n------------------------------------------------------------------------\n"})}),"\n",(0,t.jsx)(n.h4,{id:"rationale-4",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"Some compilers omit object code corresponding to parts of the program\nthat they detect can never be executed. Constant expressions in\nconditional statements take advantage of this feature where it is\navailable, providing a limited form of conditional compilation. When a\npart is reused in an implementation that does not support this form of\nconditional compilation, this practice produces a clean structure that\nis easy to adapt by deleting or commenting out redundant code where it\ncreates an unacceptable overhead."}),"\n",(0,t.jsx)(n.p,{children:"This feature should be used when other factors prevent the code from\nbeing separated into separate program units. In the above example, it\nwould be preferable to have a different procedure for each algorithm.\nBut the algorithms may differ in slight but complex ways to make\nseparate procedures difficult to maintain."}),"\n",(0,t.jsx)(n.h4,{id:"caution",children:"caution"}),"\n",(0,t.jsx)(n.p,{children:"Be aware of whether your implementation supports dead code removal, and\nbe prepared to take other steps to eliminate the overhead of redundant\ncode if necessary."}),"\n",(0,t.jsx)(n.h3,{id:"table-driven-programming",children:"Table-Driven Programming"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-5",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Write table-driven reusable parts wherever possible and appropriate."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"example-4",children:"example"}),"\n",(0,t.jsx)(n.p,{children:'The epitome of table-driven reusable software is a parser generation\nsystem. A specification of the form of the input data and of its output,\nalong with some specialization code, is converted to tables that are to\nbe "walked" by preexisting code using predetermined algorithms in the\nparser produced. Other forms of "application generators" work similarly.'}),"\n",(0,t.jsx)(n.h4,{id:"rationale-5",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"Table-driven (sometimes known as data-driven) programs have behavior\nthat depends on data with'ed at compile time or read from a file at\nrun-time. In appropriate circumstances, table-driven programming\nprovides a very powerful way of creating general-purpose, easily\ntailorable, reusable parts."}),"\n",(0,t.jsx)(n.p,{children:"See Guideline 5.3.4 for a short discussion of using access-to-subprogram\ntypes in implementing table-driven programs."}),"\n",(0,t.jsx)(n.h4,{id:"notes-2",children:"notes"}),"\n",(0,t.jsx)(n.p,{children:"Consider whether differences in the behavior of a general-purpose part\ncould be defined by some data structure at compile- or run-time, and if\nso, structure the part to be table-driven. The approach is most likely\nto be applicable when a part is designed for use in a particular\napplication domain but needs to be specialized for use in a specific\napplication within the domain. Take particular care in commenting the\nstructure of the data needed to drive the part."}),"\n",(0,t.jsx)(n.p,{children:"Table-driven programs are often more efficient and easier to read than\nthe corresponding case or if-elsif-else networks to compute the item\nbeing sought or looked up."}),"\n",(0,t.jsx)(n.h3,{id:"string-handling",children:"String Handling"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-6",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Use the predefined packages for string handling."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"example-5",children:"example"}),"\n",(0,t.jsx)(n.p,{children:"Writing code such as the following is no longer necessary in Ada 95:"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:"function Upper_Case (S : String) return String is\n\n   subtype Lower_Case_Range is Character range 'a'..'z';\n\n   Temp : String := S;\n   Offset : constant := Character'Pos('A') - Character'Pos('a');\n\nbegin\n   for Index in Temp'Range loop\n      if Temp(Index) in Lower_Case_Range then\n         Temp(Index) := Character'Val (Character'Pos(Temp(Index)) + Offset);\n      end if;\n   end loop;\n   return Temp;\nend Upper_Case;\n\nwith Ada.Characters.Latin_1;\nfunction Trim (S : String) return String is\n   Left_Index  : Positive := S'First;\n   Right_Index : Positive := S'Last;\n   Space : constant Character := Ada.Characters.Latin_1.Space;\nbegin\n   while (Left_Index < S'Last) and then (S(Left_Index) = Space) loop\n      Left_Index := Positive'Succ(Left_Index);\n   end loop;\n\n   while (Right_Index > S'First) and then (S(Right_Index) = Space) loop\n      Right_Index := Positive'Pred(Right_Index);\n   end loop;\n\n   return S(Left_Index..Right_Index);\nend Trim;\n"})}),"\n",(0,t.jsx)(n.p,{children:"Assuming a variable S of type String, the following expression:"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:"Upper_Case(Trim(S))\n"})}),"\n",(0,t.jsx)(n.p,{children:"can now be replaced by more portable and preexisting language-defined\noperations such as:"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:"with Ada.Characters.Handling;  use Ada.Characters.Handling;\nwith Ada.Strings;              use Ada.Strings;\nwith Ada.Strings.Fixed;        use Ada.Strings.Fixed;\n\n...\nTo_Upper (Trim (Source => S, Side => Both))\n"})}),"\n",(0,t.jsx)(n.h4,{id:"rationale-6",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"The predefined Ada language environment includes string handling\npackages to encourage portability. They support different categories of\nstrings: fixed length, bounded length, and unbounded length. They also\nsupport subprograms for string construction, concatenation, copying,\nselection, ordering, searching, pattern matching, and string\ntransformation. You no longer need to define your own string handling\npackages."}),"\n",(0,t.jsx)(n.h3,{id:"tagged-type-hierarchies",children:"Tagged Type Hierarchies"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-7",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Consider using hierarchies of tagged types to promote generalization\nof software for reuse."}),"\n",(0,t.jsx)(n.li,{children:"Consider using a tagged type hierarchy to decouple a generalized\nalgorithm from the details of dependency on specific types."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"example-6",children:"example"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:"with Wage_Info;\npackage Personnel is\n   type Employee is abstract tagged limited private;\n   type Employee_Ptr is access all Employee'Class;\n   ...\n   procedure Compute_Wage (E : Employee) is abstract;\nprivate\n   type Employee is tagged limited record\n      Name  : ...;\n      SSN   : ... ;\n      Rates : Wage_Info.Tax_Info;\n      ...\n   end record;\nend Personnel;\npackage Personnel.Part_Time is\n   type Part_Timer is new Employee with private;\n   ...\n   procedure Compute_Wage (E : Part_Timer);\nprivate\n   ...\nend Personnel.Part_Time;\npackage Personnel.Full_Time is\n   type Full_Timer is new Employee with private;\n   ...\n   procedure Compute_Wage (E : Full_Timer);\nprivate\n   ...\nend Personnel.Full_Time;\n"})}),"\n",(0,t.jsx)(n.p,{children:"Given the following array declaration:"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:"type Employee_List is array (Positive range <>) of Personnel.Employee_Ptr;\n"})}),"\n",(0,t.jsx)(n.p,{children:"you can write a procedure that computes the wage of each employee,\nregardless of the different types of employees that you create. The\nEmployee_List consists of an array of pointers to the various kinds of\nemployees, each of which has an individual Compute_Wage procedure. (The\nprimitive Compute_Wage is declared as an abstract procedure and,\ntherefore, must be overridden by all descendants.) You will not need to\nmodify the payroll code as you specialize the kinds of employees:"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:"procedure Compute_Payroll (Who : Employee_List) is\nbegin -- Compute_Payroll\n   for E in Who'Range loop\n      Compute_Wage (Who(E).all);\n   end loop;\nend Compute_Payroll;\n"})}),"\n",(0,t.jsx)(n.h4,{id:"rationale-7",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"The general algorithm can depend polymorphically on objects of the\nclass-wide type of the root tagged type without caring what specialized\ntypes are derived from the root type. The generalized algorithm does not\nneed to be changed if additional types are added to the type hierarchy.\nSee also Guideline 5.4.2. Furthermore, the child package hierarchy then\nmirrors the inheritance hierarchy."}),"\n",(0,t.jsx)(n.p,{children:"A general root tagged type can define the common properties and have\ncommon operations for a hierarchy of more specific types. Software that\ndepends only on this root type will be general, in that it can be used\nwith objects of any of the more specific types. Further, the general\nalgorithms of clients of the root type do not have to be changed as more\nspecific types are added to the type hierarchy. This is a particularly\neffective way to organize object-oriented software for reuse."}),"\n",(0,t.jsx)(n.p,{children:"Separating the hierarchy of derived tagged types into individual\npackages enhances reusability by reducing the number of items in package\ninterfaces. It also allows you to with only the capabilities needed."}),"\n",(0,t.jsx)(n.p,{children:"See also Guidelines 9.2, 9.3.1, 9.3.5, and 9.4.1."})]})}function h(e={}){const{wrapper:n}={...(0,r.R)(),...e.components};return n?(0,t.jsx)(n,{...e,children:(0,t.jsx)(c,{...e})}):c(e)}}}]);