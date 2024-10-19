"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[6720],{89774:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>l,contentTitle:()=>r,default:()=>h,frontMatter:()=>s,metadata:()=>o,toc:()=>d});var t=i(74848),a=i(28453);const s={title:"7.5 Representation Clauses And Implementation-Dependent Features"},r=void 0,o={id:"style-guide/s7/05",title:"7.5 Representation Clauses And Implementation-Dependent Features",description:"Ada provides many implementation-dependent features that permit greater",source:"@site/docs/style-guide/s7/05.mdx",sourceDirName:"style-guide/s7",slug:"/style-guide/s7/05",permalink:"/docs/style-guide/s7/05",draft:!1,unlisted:!1,tags:[],version:"current",frontMatter:{title:"7.5 Representation Clauses And Implementation-Dependent Features"},sidebar:"styleGuideSidebar",previous:{title:"7.4 Exceptions",permalink:"/docs/style-guide/s7/04"},next:{title:"7.6 Input/Output",permalink:"/docs/style-guide/s7/06"}},l={},d=[{value:"Representation Clauses",id:"representation-clauses",level:3},{value:"guideline",id:"guideline",level:4},{value:"rationale",id:"rationale",level:4},{value:"notes",id:"notes",level:4},{value:"Package System",id:"package-system",level:3},{value:"guideline",id:"guideline-1",level:4},{value:"rationale",id:"rationale-1",level:4},{value:"notes",id:"notes-1",level:4},{value:"exceptions",id:"exceptions",level:4},{value:"Machine Code Inserts",id:"machine-code-inserts",level:3},{value:"guideline",id:"guideline-2",level:4},{value:"rationale",id:"rationale-2",level:4},{value:"exceptions",id:"exceptions-1",level:4},{value:"Interfacing to Foreign Languages",id:"interfacing-to-foreign-languages",level:3},{value:"guideline",id:"guideline-3",level:4},{value:"example",id:"example",level:4},{value:"rationale",id:"rationale-3",level:4},{value:"exceptions",id:"exceptions-2",level:4},{value:"Implementation-Specific Pragmas and Attributes",id:"implementation-specific-pragmas-and-attributes",level:3},{value:"guideline",id:"guideline-4",level:4},{value:"rationale",id:"rationale-4",level:4},{value:"Unchecked Deallocation",id:"unchecked-deallocation",level:3},{value:"guideline",id:"guideline-5",level:4},{value:"rationale",id:"rationale-5",level:4},{value:"notes",id:"notes-2",level:4},{value:"exceptions",id:"exceptions-3",level:4},{value:"Unchecked Access",id:"unchecked-access",level:3},{value:"guideline",id:"guideline-6",level:4},{value:"rationale",id:"rationale-6",level:4},{value:"Unchecked Conversion",id:"unchecked-conversion",level:3},{value:"guideline",id:"guideline-7",level:4},{value:"rationale",id:"rationale-7",level:4},{value:"exceptions",id:"exceptions-4",level:4},{value:"Run-Time Dependencies",id:"run-time-dependencies",level:3},{value:"guideline",id:"guideline-8",level:4},{value:"rationale",id:"rationale-8",level:4},{value:"exceptions",id:"exceptions-5",level:4}];function c(e){const n={code:"code",h3:"h3",h4:"h4",li:"li",p:"p",pre:"pre",ul:"ul",...(0,a.R)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsx)(n.p,{children:"Ada provides many implementation-dependent features that permit greater\ncontrol over and interaction with the underlying hardware architecture\nthan is normally provided by a high-order language. These mechanisms are\nintended to assist in systems programming and real-time programming to\nobtain greater efficiency (e.g., specific size and layout of variables\nthrough representation clauses) and direct hardware interaction (e.g.,\ninterrupt entries) without having to resort to assembly level\nprogramming. Given the objectives for these features, it is not\nsurprising that you must usually pay a significant price in portability\nto use them. In general, where portability is the main objective, do not\nuse these features. When you must use these features, encapsulate them\nin packages that are well-commented as interfacing to the particular\ntarget environment. This section identifies the various features and\ntheir recommended use with respect to portability."}),"\n",(0,t.jsx)(n.h3,{id:"representation-clauses",children:"Representation Clauses"}),"\n",(0,t.jsx)(n.h4,{id:"guideline",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Use algorithms that do not depend on the representation of the data\nand, therefore, do not need representation clauses."}),"\n",(0,t.jsx)(n.li,{children:"Consider using representation clauses when accessing or defining\ninterface data or when a specific representation is needed to\nimplement a design."}),"\n",(0,t.jsx)(n.li,{children:"Do not assume that sharing source files between programs guarantees\nthe same representation of data types in those files."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"rationale",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"In many cases, it is easy to use representation clauses to implement an\nalgorithm, even when it is not necessary. There is also a tendency to\ndocument the original programmer's assumptions about the representation\nfor future reference. But there is no guarantee that another\nimplementation will support the representation chosen. Unnecessary\nrepresentation clauses also confuse porting or maintenance efforts,\nwhich must assume that the programmer depends on the documented\nrepresentation."}),"\n",(0,t.jsx)(n.p,{children:"Interfaces to external systems and devices are the most common\nsituations where a representation clause is needed. Uses of pragma\nImport and address clauses should be evaluated during design and porting\nto determine whether a representation clause is needed."}),"\n",(0,t.jsx)(n.p,{children:"Without representation clauses, the language does not require two\ncompilations of an unchanged file to result in the same data\nrepresentation. Things that can change the representation between\ncompilations include:"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"A change in a file earlier in the compilation order"}),"\n",(0,t.jsx)(n.li,{children:"A change in the optimization strategy or level"}),"\n",(0,t.jsx)(n.li,{children:"A change in versions of the compiler"}),"\n",(0,t.jsx)(n.li,{children:"A change in actual compilers"}),"\n",(0,t.jsx)(n.li,{children:"A change in the availability of system resources"}),"\n"]}),"\n",(0,t.jsx)(n.p,{children:"Therefore, two independently linked programs or partitions should only\nshare data that has their representations explicitly controlled."}),"\n",(0,t.jsx)(n.h4,{id:"notes",children:"notes"}),"\n",(0,t.jsx)(n.p,{children:"During a porting effort, all representation clauses can be evaluated as\neither design artifacts or specifications for accessing interface data\nthat might change with a new implementation."}),"\n",(0,t.jsx)(n.h3,{id:"package-system",children:"Package System"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-1",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Avoid using package System constants except in attempting to\ngeneralize other machine-dependent constructs."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"rationale-1",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"Because the values in this package are implementation-provided,\nunexpected effects can result from their use."}),"\n",(0,t.jsx)(n.h4,{id:"notes-1",children:"notes"}),"\n",(0,t.jsx)(n.p,{children:"If you must guarantee that physical record layouts will remain the same\nbetween implementations, you can express record fields by their first\nand last bit positions as shown in the Ada Reference Manual (1995,\n\xa713.5.1). Static expressions and named numbers should be used to let\nthe compiler compute the endpoints of each range in terms of earlier\nfields. In this case, greater portability can be achieved by using\nSystem.Storage_Unit to let the compiler compute the value of the named\nnumber. However, this method might not work for all values of\nSystem.Storage_Unit."}),"\n",(0,t.jsx)(n.h4,{id:"exceptions",children:"exceptions"}),"\n",(0,t.jsx)(n.p,{children:"Do use package System constants to parameterize other\nimplementation-dependent features (see Pappas (1985, \xa713.7.1)."}),"\n",(0,t.jsx)(n.h3,{id:"machine-code-inserts",children:"Machine Code Inserts"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-2",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Avoid machine code inserts."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"rationale-2",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"The Ada Reference Manual (1995, Annex C) suggests that the package that\nimplements machine code inserts is optional. Additionally, it is not\nstandardized so that machine code inserts are most likely not portable.\nIn fact, it is possible that two different vendors' syntax will differ\nfor an identical target, and differences in lower-level details, such as\nregister conventions, will hinder portability."}),"\n",(0,t.jsx)(n.h4,{id:"exceptions-1",children:"exceptions"}),"\n",(0,t.jsx)(n.p,{children:"If machine code inserts must be used to meet another project\nrequirement, recognize and document the portability decreasing effects."}),"\n",(0,t.jsx)(n.p,{children:"In the declarative region of the body of the routine where machine code\ninserts are being used, insert comments explaining what functions\ninserts provide and (especially) why the inserts are necessary. Comment\nthe necessity of using machine code inserts by delineating what went\nwrong with attempts to use other higher level constructs."}),"\n",(0,t.jsx)(n.h3,{id:"interfacing-to-foreign-languages",children:"Interfacing to Foreign Languages"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-3",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Use the package Interfaces and its language-defined child packages\nrather than implementation-specific mechanisms."}),"\n",(0,t.jsx)(n.li,{children:'Consider using pragma Import rather than access-to-subprogram types\nfor interfacing to subprograms in other languages. (Preferably using\nthe "External_Name =>" argument.)'}),"\n",(0,t.jsx)(n.li,{children:"Isolate all subprograms employing pragmas Import, Export, and\nConvention to implementation-specific (interface) package bodies."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"example",children:"example"}),"\n",(0,t.jsx)(n.p,{children:"This example shows how to interface with the following cube root\nfunction written in C:"}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-c",children:"double cbrt (double x);\n"})}),"\n",(0,t.jsx)(n.pre,{children:(0,t.jsx)(n.code,{className:"language-ada",children:'package Math_Utilities is\n\n   Argument_Error : exception;\n\n   function Cube_Root (X : Float) return Float;\n\n   ...\n\nend Math_Utilities;\n\n------------------------------------------------------------------------------\nwith Interfaces.C;\npackage body Math_Utilities is\n\n   function Cube_Root (X : Float) return Float is\n\n      function C_Cbrt (X : Interfaces.C.Double) return Interfaces.C.Double;\n      pragma Import (Convention    => C,\n                     Entity        => C_Cbrt,\n                     External_Name => "cbrt");\n\n   begin\n      if X < 0.0 then\n         raise Argument_Error;\n      else\n         return Float (C_Cbrt (Interfaces.C.Double (X)));\n      end if;\n   end Cube_Root;\n\n   ...\n\nend Math_Utilities;\n'})}),"\n",(0,t.jsx)(n.h4,{id:"rationale-3",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"For static interfacing to subprograms in other languages, the pragma\nImport provides a better solution than access to subprograms because no\nindirection is required. The pragma Interface (Ada Reference Manual 1983) has been replaced by pragmas Import, Export, and Convention. Annex\nB of the Rationale (1995) discusses how to use these pragmas in\nconjunction with the access-to-subprogram types in interfacing to other\nlanguages."}),"\n",(0,t.jsx)(n.p,{children:'Note especially the distinction between the "External_Name =>" and\n"Link_Name =>" parameters to pragma Import which are frequently\nconfused. External_Name specifies the procedure name as it appears in\nthe source code of the other language (such as C or Fortran). Link_Name\nspecifies the name used by the linker. Typically, only one of these\nparameters is specified, and generally External_Name is the preferred\nchoice for portability.'}),"\n",(0,t.jsx)(n.p,{children:"Access to subprogram types is useful for implementing callbacks in a\nseparate subsystem, such as the X Window system."}),"\n",(0,t.jsx)(n.p,{children:"The problems with interfacing to foreign languages are complex. These\nproblems include pragma syntax differences, conventions for\nlinking/binding Ada to other languages, and mapping Ada variables to\nforeign language variables. By hiding these dependencies within\ninterface packages, the amount of code modification can be reduced."}),"\n",(0,t.jsx)(n.h4,{id:"exceptions-2",children:"exceptions"}),"\n",(0,t.jsx)(n.p,{children:"It is often necessary to interact with other languages, if only an\nassembly language, to reach certain hardware features. In these cases,\nclearly comment the requirements and limitations of the interface and\npragma Import, Export, and Conventions usage."}),"\n",(0,t.jsx)(n.h3,{id:"implementation-specific-pragmas-and-attributes",children:"Implementation-Specific Pragmas and Attributes"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-4",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Avoid pragmas and attributes added by the compiler implementor."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"rationale-4",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"The Ada Reference Manual (1995) permits an implementor to add pragmas\nand attributes to exploit a particular hardware architecture or software\nenvironment. These are obviously even more implementation-specific and\ntherefore less portable than an implementor's interpretations of the\npredefined pragmas and attributes. However, the Ada Reference Manual\n(1995) defines a set of annexes that have a uniform and consistent\napproach to certain specialized needs, namely, real-time systems,\ndistributed systems, information systems, numerics, interfacing to\nforeign languages, and safety and security. You should always prefer the\nfacilities defined in the annexes to any vendor-defined pragmas and\nattributes."}),"\n",(0,t.jsx)(n.h3,{id:"unchecked-deallocation",children:"Unchecked Deallocation"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-5",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Avoid dependence on Ada.Unchecked_Deallocation (see Guideline\n5.9.2)."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"rationale-5",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"The unchecked storage deallocation mechanism is one method for\noverriding the default time at which allocated storage is reclaimed. The\nearliest default time is when an object is no longer accessible, for\nexample, when control leaves the scope where an access type was declared\n(the exact point after this time is implementation-dependent). Any\nunchecked deallocation of storage performed prior to this may result in\nan erroneous Ada program if an attempt is made to access the object."}),"\n",(0,t.jsx)(n.p,{children:"This guideline is stronger than Guideline 5.9.2 because of the extreme\ndependence on the implementation of Ada.Unchecked_Deallocation. Using\nit could cause considerable difficulty with portability."}),"\n",(0,t.jsx)(n.h4,{id:"notes-2",children:"notes"}),"\n",(0,t.jsx)(n.p,{children:"Ada.Unchecked_Deallocation is a supported feature in all Ada\nimplementations. The portability issue arises in that unchecked storage\ndeallocations might cause varying results in different implementations."}),"\n",(0,t.jsx)(n.h4,{id:"exceptions-3",children:"exceptions"}),"\n",(0,t.jsx)(n.p,{children:"Using unchecked deallocation of storage can be beneficial in local\ncontrol of highly iterative or recursive algorithms where available\nstorage may be exceeded."}),"\n",(0,t.jsx)(n.h3,{id:"unchecked-access",children:"Unchecked Access"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-6",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Avoid dependence on the attribute Unchecked_Access (see Guideline\n5.9.2)."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"rationale-6",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"Access values are subject to accessibility restrictions. Using the\nattribute Unchecked_Access prevents these rules from being checked, and\nthe programmer runs the risk of having dangling references."}),"\n",(0,t.jsx)(n.h3,{id:"unchecked-conversion",children:"Unchecked Conversion"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-7",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Avoid dependence on Ada.Unchecked_Conversion (see Guideline 5.9.1)."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"rationale-7",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"The unchecked type conversion mechanism is, in effect, a means of\nbypassing the strong typing facilities in Ada. An implementation is free\nto limit the types that may be matched and the results that occur when\nobject sizes differ."}),"\n",(0,t.jsx)(n.h4,{id:"exceptions-4",children:"exceptions"}),"\n",(0,t.jsx)(n.p,{children:"Unchecked type conversion is useful in implementation-dependent parts of\nAda programs where lack of portability is isolated and where low-level\nprogramming and foreign language interfacing are the objectives."}),"\n",(0,t.jsx)(n.p,{children:"If an enumeration representation clause is used, unchecked type\nconversion is the only language-provided way to retrieve the internal\ninteger code of an enumeration value."}),"\n",(0,t.jsx)(n.h3,{id:"run-time-dependencies",children:"Run-Time Dependencies"}),"\n",(0,t.jsx)(n.h4,{id:"guideline-8",children:"guideline"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Avoid the direct invocation of or implicit dependence upon an\nunderlying host operating system or Ada run-time support system,\nexcept where the interface is explicitly defined in the language\n(e.g., Annex C or D of the Ada Reference Manual [1995])."}),"\n",(0,t.jsx)(n.li,{children:"Use standard bindings and the package Ada.Command_Line when you\nneed to invoke the underlying run-time support system."}),"\n",(0,t.jsx)(n.li,{children:"Use features defined in the Annexes rather than vendor-defined\nfeatures."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"rationale-8",children:"rationale"}),"\n",(0,t.jsx)(n.p,{children:"Features of an implementation not specified in the Ada Reference Manual\n(1995) will usually differ between implementations. Specific\nimplementation-dependent features are not likely to be provided in other\nimplementations. In addition to the mandatory predefined language\nenvironment, the annexes define various packages, attributes, and\npragmas to standardize implementation-dependent features for several\nspecialized domains. You enhance portability when you use the features\ndeclared in the packages in the Annexes because you can port your\nprogram to other vendor environments that implement the same Annexes you\nhave used. Even if a majority of vendors eventually provide similar\nfeatures, they are unlikely to have identical formulations. Indeed,\ndifferent vendors may use the same formulation for (semantically)\nentirely different features."}),"\n",(0,t.jsx)(n.p,{children:"When coding, try to avoid depending on the underlying operating system.\nConsider the consequences of including system calls in a program on a\nhost development system. If these calls are not flagged for removal and\nreplacement, the program could go through development and testing only\nto be unusable when moved to a target environment that lacks the\nfacilities provided by those system calls on the host."}),"\n",(0,t.jsx)(n.p,{children:"Guideline 7.1.5 discusses the use of the package Ada.Command_Line. If\nan Ada environment implements a standard binding to operating system\nservices, such as POSIX/Ada, and you write POSIX-compliant calls, your\nprogram should be portable across more systems."}),"\n",(0,t.jsx)(n.h4,{id:"exceptions-5",children:"exceptions"}),"\n",(0,t.jsx)(n.p,{children:"In real-time, embedded systems, making calls to low-level support system\nfacilities may often be unavoidable. Isolating the uses of these\nfacilities may be too difficult. Comment them as you would machine code\ninserts (see Guideline 7.6.3); they are, in a sense, instructions for\nthe virtual machine provided by the support system. When isolating the\nuses of these features, provide an interface for the rest of your\nprogram to use, which can be ported through replacement of the\ninterface's implementation."})]})}function h(e={}){const{wrapper:n}={...(0,a.R)(),...e.components};return n?(0,t.jsx)(n,{...e,children:(0,t.jsx)(c,{...e})}):c(e)}}}]);