"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[3026],{6302:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>l,contentTitle:()=>o,default:()=>h,frontMatter:()=>r,metadata:()=>t,toc:()=>d});const t=JSON.parse('{"id":"style-guide/s7/07","title":"7.7 Summary","description":"fundamentals","source":"@site/docs/style-guide/s7/07.mdx","sourceDirName":"style-guide/s7","slug":"/style-guide/s7/07","permalink":"/docs/style-guide/s7/07","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"title":"7.7 Summary"},"sidebar":"styleGuideSidebar","previous":{"title":"7.6 Input/Output","permalink":"/docs/style-guide/s7/06"},"next":{"title":"8. Reusability","permalink":"/docs/style-guide/s8/"}}');var s=i(4848),a=i(8453);const r={title:"7.7 Summary"},o=void 0,l={},d=[{value:"fundamentals",id:"fundamentals",level:3},{value:"numeric types and expressions",id:"numeric-types-and-expressions",level:3},{value:"storage control",id:"storage-control",level:3},{value:"tasking",id:"tasking",level:3},{value:"exceptions",id:"exceptions",level:4},{value:"representation clauses and implementation-dependent features",id:"representation-clauses-and-implementation-dependent-features",level:3},{value:"input/output",id:"inputoutput",level:3}];function c(e){const n={a:"a",admonition:"admonition",h3:"h3",h4:"h4",li:"li",p:"p",ul:"ul",...(0,a.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(n.h3,{id:"fundamentals",children:"fundamentals"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:'In programs or components intended to have a long life, avoid using\nthe features of Ada declared as "obsolescent" by Annex J of the Ada\nReference Manual (1995), unless the use of the feature is needed for\nbackward compatibility with Ada 83 (Ada Reference Manual 1983).'}),"\n",(0,s.jsx)(n.li,{children:"Document the use of any obsolescent features."}),"\n",(0,s.jsxs)(n.li,{children:["Avoid using the following features:","\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"The short renamings of the packages in the predefined\nenvironment (e.g., Text_IO as opposed to Ada.Text_IO)"}),"\n",(0,s.jsx)(n.li,{children:"The character replacements of ! for |, : for #, and % for\nquotation marks"}),"\n",(0,s.jsx)(n.li,{children:"Reduced accuracy subtypes of floating-point types"}),"\n",(0,s.jsx)(n.li,{children:"The 'Constrained attribute as applied to private types"}),"\n",(0,s.jsx)(n.li,{children:"The predefined package ASCII"}),"\n",(0,s.jsx)(n.li,{children:"The exception Numeric_Error"}),"\n",(0,s.jsx)(n.li,{children:"Various representation specifications, including at clauses, mod\nclauses, interrupt entries, and the Storage_Size attribute"}),"\n"]}),"\n"]}),"\n",(0,s.jsxs)(n.li,{children:["Make informed assumptions about the support provided for the\nfollowing on potential target platforms:","\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Number of bits available for type Integer (range constraints)"}),"\n",(0,s.jsx)(n.li,{children:"Number of decimal digits of precision available for\nfloating-point types"}),"\n",(0,s.jsx)(n.li,{children:"Number of bits available for fixed-point types (delta and range\nconstraints)"}),"\n",(0,s.jsx)(n.li,{children:"Number of characters per line of source text"}),"\n",(0,s.jsx)(n.li,{children:"Number of bits for Root_Integer expressions"}),"\n",(0,s.jsx)(n.li,{children:"Number of seconds for the range of Duration"}),"\n",(0,s.jsx)(n.li,{children:"Number of milliseconds for Duration'Small"}),"\n",(0,s.jsx)(n.li,{children:"Minimum and maximum scale for decimal types"}),"\n"]}),"\n"]}),"\n",(0,s.jsx)(n.li,{children:"Avoid assumptions about the values and the number of values included\nin the type Character."}),"\n",(0,s.jsx)(n.li,{children:"Use highlighting comments for each package, subprogram, and task\nwhere any nonportable features are present."}),"\n",(0,s.jsx)(n.li,{children:"For each nonportable feature employed, describe the expectations for\nthat feature."}),"\n",(0,s.jsx)(n.li,{children:"Consider using only a parameterless procedure as the main\nsubprogram."}),"\n",(0,s.jsx)(n.li,{children:"Consider using Ada.Command_Line for accessing values from the\nenvironment, but recognize that this package's behavior and even its\nspecification are nonportable."}),"\n",(0,s.jsx)(n.li,{children:"Encapsulate and document all uses of package Ada.Command_Line."}),"\n",(0,s.jsx)(n.li,{children:"Create packages specifically designed to isolate hardware and\nimplementation dependencies and designed so that their specification\nwill not change when porting."}),"\n",(0,s.jsx)(n.li,{children:"Clearly indicate the objectives if machine or solution efficiency is\nthe reason for hardware or implementation-dependent code."}),"\n",(0,s.jsx)(n.li,{children:"For the packages that hide implementation dependencies, maintain\ndifferent package bodies for different target environments."}),"\n",(0,s.jsx)(n.li,{children:"Isolate interrupt receiving tasks into implementation-dependent\npackages."}),"\n",(0,s.jsx)(n.li,{children:"Refer to Annex M of the Ada Reference Manual (1995) for a list of\nimplementation-dependent features."}),"\n",(0,s.jsx)(n.li,{children:"Avoid the use of vendor-supplied packages."}),"\n",(0,s.jsx)(n.li,{children:"Avoid the use of features added to the predefined packages that are\nnot specified in the Ada language definition or Specialized Needs\nAnnexes."}),"\n",(0,s.jsx)(n.li,{children:"Use features defined in the Specialized Needs Annexes rather than\nvendor-defined features."}),"\n",(0,s.jsx)(n.li,{children:"Document clearly the use of any features from the Specialized Needs\nAnnexes (systems programming, real-time systems, distributed\nsystems, information systems, numerics, and safety and security)."}),"\n",(0,s.jsx)(n.li,{children:"Do not write code whose correct execution depends on the particular\nparameter passing mechanism used by an implementation (Ada Reference\nManual 1995, \xa76.2; Cohen 1986)."}),"\n",(0,s.jsx)(n.li,{children:"If a subprogram has more than one formal parameter of a given\nsubtype, at least one of which is [in] out, make sure that the\nsubprogram can properly handle the case when both formal parameters\ndenote the same actual object."}),"\n",(0,s.jsx)(n.li,{children:"Avoid depending on the order in which certain constructs in Ada are\nevaluated ."}),"\n"]}),"\n",(0,s.jsx)(n.h3,{id:"numeric-types-and-expressions",children:"numeric types and expressions"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Avoid using the predefined numeric types in package Standard . Use\nrange and digits declarations and let the implementation pick the\nappropriate representation."}),"\n",(0,s.jsx)(n.li,{children:"For programs that require greater accuracy than that provided by the\nglobal assumptions, define a package that declares a private type\nand operations as needed; see Pappas (1985) for a full explanation\nand examples."}),"\n",(0,s.jsxs)(n.li,{children:["Consider using predefined numeric types (Integer, Natural, Positive)\nfor:","\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Indexes into arrays where the index type is not significant,\nsuch as type String"}),"\n",(0,s.jsx)(n.li,{children:'"Pure" numbers, that is, numbers with no associated physical\nunit (e.g., exponents)'}),"\n",(0,s.jsx)(n.li,{children:"Values whose purpose is to control a repeat or iteration count"}),"\n"]}),"\n"]}),"\n",(0,s.jsx)(n.li,{children:"Use an implementation that supports the Numerics Annex (Ada\nReference Manual 1995, Annex G) when performance and accuracy are\noverriding concerns ."}),"\n",(0,s.jsx)(n.li,{children:"Carefully analyze what accuracy and precision you really need."}),"\n",(0,s.jsx)(n.li,{children:"Do not press the accuracy limits of the machine(s)."}),"\n",(0,s.jsx)(n.li,{children:"Comment the analysis and derivation of the numerical aspects of a\nprogram."}),"\n",(0,s.jsx)(n.li,{children:"Anticipate the range of values of subexpressions to avoid exceeding\nthe underlying range of their base type. Use derived types,\nsubtypes, factoring, and range constraints on numeric types."}),"\n",(0,s.jsx)(n.li,{children:"Consider using <= and >= to do relational tests on real valued\narguments, avoiding the <, >, =, and /= operations."}),"\n",(0,s.jsx)(n.li,{children:"Use values of type attributes in comparisons and checking for small\nvalues."}),"\n",(0,s.jsx)(n.li,{children:"In information systems, declare different numeric decimal types to\ncorrespond to different scales (Brosgol, Eachus, and Emery 1994)."}),"\n",(0,s.jsx)(n.li,{children:"Create objects of different decimal types to reflect different units\nof measure (Brosgol, Eachus, and Emery 1994)."}),"\n",(0,s.jsx)(n.li,{children:"Declare subtypes of the appropriately scaled decimal type to provide\nappropriate range constraints for application-specific types."}),"\n",(0,s.jsx)(n.li,{children:"Encapsulate each measure category in a package (Brosgol, Eachus, and\nEmery 1994)."}),"\n",(0,s.jsx)(n.li,{children:"Declare as few decimal types as possible for unitless data (Brosgol,\nEachus, and Emery 1994)."}),"\n",(0,s.jsx)(n.li,{children:"For decimal calculations, determine whether the result should be\ntruncated toward 0 or rounded."}),"\n",(0,s.jsx)(n.li,{children:"Avoid decimal types and arithmetic on compilers that do not support\nthe Information Systems Annex (Ada Reference Manual 1995, Annex F)\nin full."}),"\n"]}),"\n",(0,s.jsx)(n.h3,{id:"storage-control",children:"storage control"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Do not use a representation clause to specify number of storage\nunits."}),"\n",(0,s.jsx)(n.li,{children:"Do not compare access-to-subprogram values."}),"\n",(0,s.jsx)(n.li,{children:"Consider using explicitly defined storage pool mechanisms."}),"\n"]}),"\n",(0,s.jsx)(n.h3,{id:"tasking",children:"tasking"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Do not depend on the order in which task objects are activated when\ndeclared in the same declarative list."}),"\n",(0,s.jsx)(n.li,{children:"Do not depend on a particular delay being achievable (Nissen and\nWallis 1984)."}),"\n",(0,s.jsx)(n.li,{children:"Never use knowledge of the execution pattern of tasks to achieve\ntiming requirements."}),"\n",(0,s.jsx)(n.li,{children:"Do not assume a correlation between System.Tick and type Duration."}),"\n",(0,s.jsx)(n.li,{children:"Do not depend on the order in which guard conditions are evaluated\nor on the algorithm for choosing among several open select\nalternatives."}),"\n",(0,s.jsx)(n.li,{children:"Do not assume that tasks execute uninterrupted until they reach a\nsynchronization point."}),"\n",(0,s.jsx)(n.li,{children:"Use pragma Priority to distinguish general levels of importance\nonly."}),"\n",(0,s.jsx)(n.li,{children:"Avoid using the abort statement."}),"\n",(0,s.jsx)(n.li,{children:"Do not use unprotected shared variables."}),"\n",(0,s.jsx)(n.li,{children:"Consider using protected types to provide data synchronization."}),"\n",(0,s.jsx)(n.li,{children:"Have tasks communicate through the rendezvous mechanism."}),"\n",(0,s.jsx)(n.li,{children:"Do not use unprotected shared variables as a task synchronization\ndevice."}),"\n",(0,s.jsx)(n.li,{children:"Consider using protected objects to encapsulate shared data."}),"\n",(0,s.jsx)(n.li,{children:"Use pragma Atomic or Volatile only when you are forced to by\nrun-time system deficiencies."}),"\n"]}),"\n",(0,s.jsx)(n.h4,{id:"exceptions",children:"exceptions"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Do not depend on the exact locations at which predefined exceptions\nare raised."}),"\n",(0,s.jsx)(n.li,{children:"Do not rely on the behavior of Ada.Exceptions beyond the minimum\ndefined in the language."}),"\n",(0,s.jsx)(n.li,{children:"Do not raise implementation-specific exceptions."}),"\n",(0,s.jsx)(n.li,{children:"Convert implementation-specific exceptions within interface packages\nto visible user-defined exceptions."}),"\n"]}),"\n",(0,s.jsx)(n.h3,{id:"representation-clauses-and-implementation-dependent-features",children:"representation clauses and implementation-dependent features"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Use algorithms that do not depend on the representation of the data\nand, therefore, do not need representation clauses."}),"\n",(0,s.jsx)(n.li,{children:"Consider using representation clauses when accessing or defining\ninterface data or when a specific representation is needed to\nimplement a design ."}),"\n",(0,s.jsx)(n.li,{children:"Do not assume that sharing source files between programs guarantees\nthe same representation of data types in those files."}),"\n",(0,s.jsx)(n.li,{children:"Avoid using package System constants except in attempting to\ngeneralize other machine-dependent constructs."}),"\n",(0,s.jsx)(n.li,{children:"Avoid machine code inserts."}),"\n",(0,s.jsx)(n.li,{children:"Use the package Interfaces and its language-defined child packages\nrather than implementation-specific mechanisms."}),"\n",(0,s.jsx)(n.li,{children:"Consider using pragma Import rather than access-to-subprogram types\nfor interfacing to subprograms in other languages."}),"\n",(0,s.jsx)(n.li,{children:"Isolate all subprograms employing pragmas Import, Export, and\nConvention to implementation-specific (interface) package bodies."}),"\n",(0,s.jsx)(n.li,{children:"Avoid pragmas and attributes added by the compiler implementor."}),"\n",(0,s.jsx)(n.li,{children:"Avoid dependence on Ada.Unchecked_Deallocation."}),"\n",(0,s.jsx)(n.li,{children:"Avoid dependence on the attribute Unchecked_Access."}),"\n",(0,s.jsx)(n.li,{children:"Avoid dependence on Ada.Unchecked_Conversion."}),"\n",(0,s.jsx)(n.li,{children:"Avoid the direct invocation of or implicit dependence upon an\nunderlying host operating system or Ada run-time support system,\nexcept where the interface is explicitly defined in the language\n(e.g., Annex C or D of the Ada Reference Manual [1995])."}),"\n",(0,s.jsx)(n.li,{children:"Use standard bindings and the package Ada.Command_Line when you\nneed to invoke the underlying"}),"\n"]}),"\n",(0,s.jsx)(n.p,{children:"run-time support system."}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Use features defined in the Annexes rather than vendor-defined\nfeatures."}),"\n"]}),"\n",(0,s.jsx)(n.h3,{id:"inputoutput",children:"input/output"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Use constants and variables as symbolic actuals for the Name and\nForm parameters on the predefined I/O packages. Declare and\ninitialize them in an implementation dependency package."}),"\n",(0,s.jsx)(n.li,{children:"Close all files explicitly."}),"\n",(0,s.jsx)(n.li,{children:"Avoid performing I/O on access types."}),"\n",(0,s.jsx)(n.li,{children:"Consider using Sequential_IO or Direct_IO instead of Stream_IO\nunless you need the low-level, heterogeneous I/O features provided\nby Stream_IO."}),"\n",(0,s.jsx)(n.li,{children:"Consider using Current_Error and Set_Error for run-time error\nmessages."}),"\n"]}),"\n",(0,s.jsx)(n.admonition,{type:"note",children:(0,s.jsxs)(n.p,{children:['This page of the "Ada Quality and Style Guide" has been adapted from the\noriginal work at ',(0,s.jsx)(n.a,{href:"https://en.wikibooks.org/wiki/Ada_Style_Guide",children:"https://en.wikibooks.org/wiki/Ada_Style_Guide"}),", which is\nlicensed under the\n",(0,s.jsx)(n.a,{href:"https://creativecommons.org/licenses/by-sa/3.0/",children:"Creative Commons Attribution-ShareAlike License"}),";\nadditional terms may apply. Page not endorsed by Wikibooks or the Ada\nStyle Guide Wikibook authors. This page is licensed under the same license\nas the original work."]})})]})}function h(e={}){const{wrapper:n}={...(0,a.R)(),...e.components};return n?(0,s.jsx)(n,{...e,children:(0,s.jsx)(c,{...e})}):c(e)}}}]);