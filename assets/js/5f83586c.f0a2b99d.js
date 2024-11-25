"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[7647],{57858:(e,n,a)=>{a.r(n),a.d(n,{assets:()=>l,contentTitle:()=>o,default:()=>h,frontMatter:()=>s,metadata:()=>t,toc:()=>d});const t=JSON.parse('{"id":"style-guide/s7/01","title":"7.1 Fundamentals","description":"This section introduces some generally applicable principles of writing","source":"@site/docs/style-guide/s7/01.mdx","sourceDirName":"style-guide/s7","slug":"/style-guide/s7/01","permalink":"/docs/style-guide/s7/01","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"title":"7.1 Fundamentals"},"sidebar":"styleGuideSidebar","previous":{"title":"7. Portability","permalink":"/docs/style-guide/s7/"},"next":{"title":"7.2 Numeric Types and Expressions","permalink":"/docs/style-guide/s7/02"}}');var i=a(74848),r=a(28453);const s={title:"7.1 Fundamentals"},o=void 0,l={},d=[{value:"Obsolescent Features",id:"obsolescent-features",level:3},{value:"guideline",id:"guideline",level:4},{value:"rationale",id:"rationale",level:4},{value:"exceptions",id:"exceptions",level:4},{value:"Global Assumptions",id:"global-assumptions",level:3},{value:"guideline",id:"guideline-1",level:4},{value:"instantiation",id:"instantiation",level:4},{value:"rationale",id:"rationale-1",level:4},{value:"notes",id:"notes",level:4},{value:"Comments",id:"comments",level:3},{value:"guideline",id:"guideline-2",level:4},{value:"example",id:"example",level:4},{value:"rationale",id:"rationale-2",level:4},{value:"Main Subprogram",id:"main-subprogram",level:3},{value:"guideline",id:"guideline-3",level:4},{value:"example",id:"example-1",level:4},{value:"rationale",id:"rationale-3",level:4},{value:"notes",id:"notes-1",level:4},{value:"Encapsulating Implementation Dependencies",id:"encapsulating-implementation-dependencies",level:3},{value:"guideline",id:"guideline-4",level:4},{value:"example",id:"example-2",level:4},{value:"rationale",id:"rationale-4",level:4},{value:"notes",id:"notes-2",level:4},{value:"Implementation-Added Features",id:"implementation-added-features",level:3},{value:"guideline",id:"guideline-5",level:4},{value:"rationale",id:"rationale-5",level:4},{value:"exceptions",id:"exceptions-1",level:4},{value:"Specialized Needs Annexes",id:"specialized-needs-annexes",level:3},{value:"guideline",id:"guideline-6",level:4},{value:"rationale",id:"rationale-6",level:4},{value:"Dependence on Parameter Passing Mechanism",id:"dependence-on-parameter-passing-mechanism",level:3},{value:"guideline",id:"guideline-7",level:4},{value:"example",id:"example-3",level:4},{value:"rationale",id:"rationale-7",level:4},{value:"exceptions",id:"exceptions-2",level:4},{value:"Arbitrary Order Dependencies",id:"arbitrary-order-dependencies",level:3},{value:"guideline",id:"guideline-8",level:4},{value:"example",id:"example-4",level:4},{value:"rationale",id:"rationale-8",level:4}];function c(e){const n={code:"code",h3:"h3",h4:"h4",li:"li",p:"p",pre:"pre",ul:"ul",...(0,r.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(n.p,{children:"This section introduces some generally applicable principles of writing\nportable Ada programs. It includes guidelines about the assumptions you\nshould make with respect to a number of Ada features and their\nimplementations and guidelines about the use of other Ada features to\nensure maximum portability."}),"\n",(0,i.jsx)(n.h3,{id:"obsolescent-features",children:"Obsolescent Features"}),"\n",(0,i.jsx)(n.h4,{id:"guideline",children:"guideline"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsx)(n.li,{children:'In programs or components intended to have a long life, avoid using\nthe features of Ada declared as "obsolescent" by Annex J of the Ada\nReference Manual (1995), unless the use of the feature is needed for\nbackward compatibility with Ada 83 (Ada Reference Manual 1983).'}),"\n",(0,i.jsx)(n.li,{children:"Document the use of any obsolescent features."}),"\n",(0,i.jsxs)(n.li,{children:["Avoid using the following features:","\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsx)(n.li,{children:"The short renamings of the packages in the predefined\nenvironment (e.g., Text_IO as opposed to Ada.Text_IO)"}),"\n",(0,i.jsx)(n.li,{children:"The character replacements of ! for |, : for #, and % for\nquotation marks"}),"\n",(0,i.jsx)(n.li,{children:"Reduced accuracy subtypes of floating-point types"}),"\n",(0,i.jsx)(n.li,{children:"The 'Constrained attribute as applied to private types"}),"\n",(0,i.jsx)(n.li,{children:"The predefined package ASCII"}),"\n",(0,i.jsx)(n.li,{children:"The exception Numeric_Error"}),"\n",(0,i.jsx)(n.li,{children:"Various representation specifications, including at clauses, mod\nclauses, interrupt entries, and the Storage_Size attribute"}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h4,{id:"rationale",children:"rationale"}),"\n",(0,i.jsx)(n.p,{children:"Ten years of reflection on the use of Ada 83 led to the conclusion that\nsome features of the original language are not as useful as originally\nintended. These features have been replaced with others in the Ada 95\nrevision. It would have been desirable to remove the obsolescent\nfeatures completely, but that would have prevented the upward compatible\ntransition of programs from Ada 83 to Ada 95. Thus, the obsolescent\nfeatures remain in the language and are explicitly labeled as such in\nAnnex J of the Ada Reference Manual (1995). The features listed in Annex\nJ are candidates for removal from the language during its next revision.\nIf a program's lifetime may extend beyond the next language revision, it\nshould avoid the obsolescent language features unless backward\ncompatibility with Ada 83 forces their use."}),"\n",(0,i.jsx)(n.h4,{id:"exceptions",children:"exceptions"}),"\n",(0,i.jsx)(n.p,{children:"When you instantiate Ada.Text_IO.Float_IO, the values of the\nDefault_Fore and Default_Aft fields are set from the values of the\n'Fore and 'Aft attributes of the actual floating-point type used in the\ninstantiation. If you declare a reduced accuracy floating-point type\nthat you then use to instantiate Ada.Text_IO.Float_IO, the output\nfield widths are determined from the reduced accuracy type, although the\nimplementation accuracy is unchanged (Rationale 1995, \xa73.3)."}),"\n",(0,i.jsx)(n.h3,{id:"global-assumptions",children:"Global Assumptions"}),"\n",(0,i.jsx)(n.h4,{id:"guideline-1",children:"guideline"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["Make informed assumptions about the support provided for the\nfollowing on potential target platforms:","\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsx)(n.li,{children:"Number of bits available for type Integer (range constraints)"}),"\n",(0,i.jsx)(n.li,{children:"Number of decimal digits of precision available for\nfloating-point types"}),"\n",(0,i.jsx)(n.li,{children:"Number of bits available for fixed-point types (delta and range\nconstraints)"}),"\n",(0,i.jsx)(n.li,{children:"Number of characters per line of source text"}),"\n",(0,i.jsx)(n.li,{children:"Number of bits for Root_Integer expressions"}),"\n",(0,i.jsx)(n.li,{children:"Number of seconds for the range of Duration"}),"\n",(0,i.jsx)(n.li,{children:"Number of milliseconds for Duration'Small"}),"\n",(0,i.jsx)(n.li,{children:"Minimum and maximum scale for decimal types"}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.li,{children:"Avoid assumptions about the values and the number of values included\nin the type Character."}),"\n"]}),"\n",(0,i.jsx)(n.h4,{id:"instantiation",children:"instantiation"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["These are minimum values (or minimum precision in the case of\nDuration'Small) that a project or application might assume that an\nimplementation provides. There is no guarantee that a given\nimplementation provides more than the minimum, so these would be\ntreated by the project or application as maximum values also.","\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsx)(n.li,{children:"16 bits available for type Integer (-2**15 .. 2**15 - 1)"}),"\n",(0,i.jsx)(n.li,{children:"6 decimal digits of precision available for floating-point types"}),"\n",(0,i.jsx)(n.li,{children:"24 bits available for fixed-point types"}),"\n",(0,i.jsx)(n.li,{children:"200 characters per line of source text"}),"\n",(0,i.jsx)(n.li,{children:"16 bits for expressions"}),"\n",(0,i.jsx)(n.li,{children:"-86_400 .. 86_400 seconds (1 day) for the range of Duration\n(as specified in Ada Reference Manual [1995, \xa79.6])"}),"\n",(0,i.jsx)(n.li,{children:"20 milliseconds for Duration'Small (as specified in Ada\nReference Manual [1995, \xa79.6])"}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h4,{id:"rationale-1",children:"rationale"}),"\n",(0,i.jsx)(n.p,{children:"Some assumptions must be made with respect to certain\nimplementation-specific values. The exact values assumed should cover\nthe majority of the target equipment of interest. Choosing the lowest\ncommon denominator for values improves portability. Implementations may\nsupply an alternate character set specific to a locale or environment.\nFor instance, the implementation on an IBM-compatible PC may support\nthat machine's native character set rather than Latin 1. As a result,\nsome character values may or may not be supported, for example, the\nsmiley face."}),"\n",(0,i.jsx)(n.h4,{id:"notes",children:"notes"}),"\n",(0,i.jsx)(n.p,{children:"Of the microcomputers currently available for incorporation within\nembedded systems, 16-bit and 32-bit processors are prevalent. Using\ncurrent representation schemes, 6 decimal digits of floating point\naccuracy imply a representation mantissa at least 21 bits wide, leaving\n11 bits for exponent and sign within a 32-bit representation. This\ncorrelates with the data widths of floating point hardware currently\navailable for the embedded systems market. A 32-bit minimum on\nfixed-point numbers correlates with the accuracy and storage\nrequirements of floating point numbers. The 16-bit example for\nRoot_Integer expressions matches that for Integer storage. (The 32-bit\nintegers can be assumed if the application will only be considered for\n32-bit processors with a corresponding 32-bit operating system and\nsupporting compiler.)"}),"\n",(0,i.jsx)(n.p,{children:"The values for the range and accuracy of values of the predefined type\nDuration are the limits expressed in the Ada Reference Manual (1995,\n\xa79.6). You should not expect an implementation to provide a wider range\nor a finer granularity."}),"\n",(0,i.jsx)(n.p,{children:"A standard-mode Ada character set of Latin 1 can be assumed in most\ncases for the contents and internal behavior of type Character and\npackages Character.Latin_1, Character.Handling, and Strings.Maps.\nHowever, this does not mean that the target hardware platform is capable\nof displaying the entire character set. You should not use a nonstandard\nAda character set unless intentionally producing a nonportable user\ninterface with a specific purpose."}),"\n",(0,i.jsx)(n.h3,{id:"comments",children:"Comments"}),"\n",(0,i.jsx)(n.h4,{id:"guideline-2",children:"guideline"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsx)(n.li,{children:"Use highlighting comments for each package, subprogram, and task\nwhere any nonportable features are present."}),"\n",(0,i.jsx)(n.li,{children:"For each nonportable feature employed, describe the expectations for\nthat feature."}),"\n"]}),"\n",(0,i.jsx)(n.h4,{id:"example",children:"example"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-ada",children:"------------------------------------------------------------------------\npackage Memory_Mapped_IO is\n   -- WARNING - This package is implementation specific.\n   -- It uses absolute memory addresses to interface with the I/O\n   -- system. It assumes a particular printer's line length.\n   -- Change memory mapping and printer details when porting.\n   Printer_Line_Length : constant := 132;\n   type Data is array (1 .. Printer_Line_Length) of Character;\n   procedure Write_Line (Line : in     Data);\nend Memory_Mapped_IO;\n------------------------------------------------------------------------\nwith System;\nwith System.Storage_Elements;\npackage body Memory_Mapped_IO is\n   -- WARNING: Implementation specific memory address\n\n   Buffer_Address : constant System.Address\n      := System.Storage_Elements.To_Address(16#200#);\n\n   ---------------------------------------------------------------------\n   procedure Write_Line (Line : in     Data) is\n      Buffer : Data;\n      for Buffer'Address use Buffer_Address;\n\n   begin  -- Write_Line\n       -- perform output operation through specific memory locations.\n       ...\n   end Write_Line;\n   ---------------------------------------------------------------------\nend Memory_Mapped_IO;\n------------------------------------------------------------------------\n"})}),"\n",(0,i.jsx)(n.h4,{id:"rationale-2",children:"rationale"}),"\n",(0,i.jsx)(n.p,{children:"Explicitly commenting each breach of portability will raise its\nvisibility and aid in the porting process. A description of the\nnonportable feature's expectations covers the common case where vendor\ndocumentation of the original implementation is not available to the\nperson performing the porting process."}),"\n",(0,i.jsx)(n.h3,{id:"main-subprogram",children:"Main Subprogram"}),"\n",(0,i.jsx)(n.h4,{id:"guideline-3",children:"guideline"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsx)(n.li,{children:"Consider using only a parameterless procedure as the main\nsubprogram."}),"\n",(0,i.jsx)(n.li,{children:"Consider using Ada.Command_Line for accessing values from the\nenvironment, but recognize that this package's behavior and even its\nspecification are nonportable (see Guideline 7.1.6)."}),"\n",(0,i.jsx)(n.li,{children:"Encapsulate and document all uses of package Ada.Command_Line."}),"\n"]}),"\n",(0,i.jsx)(n.h4,{id:"example-1",children:"example"}),"\n",(0,i.jsx)(n.p,{children:'The following example encapsulates the arguments for a hypothetical\n"execution mode" argument passed from the environment. It encapsulates\nboth the expected position and the expected values of the argument, as\nwell as provides a default in cases where the environment was unable to\nprovide the information:'}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-ada",children:'package Environment is\n\n   type Execution_Mode is (Unspecified, Interactive, Batch);\n\n   function Execution_Argument return Execution_Mode;\n\n   ...\n\nend Environment;\n\n----------------------------------------------------------------------\n\nwith Ada.Command_Line;       use Ada.Command_Line;\nwith Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;\n\npackage body Environment is\n\n   function Execution_Argument return Execution_Mode is\n\n      Execution_Argument_Number : constant := 1;\n\n      Interactive_Mode_String : constant String := "-i";\n      Batch_Mode_String       : constant String := "-b";\n\n   begin\n      if Argument_Count < Execution_Argument_Number then\n         return Unspecified;\n      elsif To_Unbounded_String (Argument (Execution_Argument_Number)) =\n               Interactive_Mode_String then\n         return Interactive;\n      elsif To_Unbounded_String (Argument (Execution_Argument_Number)) =\n               Batch_Mode_String then\n         return Batch;\n      else\n         return Unspecified;\n      end if;\n   end Execution_Argument;\n\nend Environment;\n'})}),"\n",(0,i.jsx)(n.h4,{id:"rationale-3",children:"rationale"}),"\n",(0,i.jsx)(n.p,{children:"The predefined language environment declares the package\nAda.Command_Line, providing a standardized way for a program to obtain\nthe values of a command line. Because all Ada compilers must implement\nthe packages in the predefined language environment, you can create a\nprogram that is more portable, maintainable, and readable by using this\npackage. You should, however, be aware that even though the language\ndefines the objects and type profiles of this package, it does not force\na relationship between the function results and any other entity or\noperation, and thus, allows the possibility of a nonportable behavior\nand specification."}),"\n",(0,i.jsx)(n.p,{children:"The value returned by the function Ada.Command_Line.Argument_Count is\nimplementation-dependent. Different operating systems follow different\nconventions regarding the parsing and meaning of command line\nparameters. To enhance your program's portability, assume the simplest\ncase: that the external execution environment does not support passing\narguments to a program."}),"\n",(0,i.jsx)(n.p,{children:"Some operating systems are capable of acquiring and interpreting\nreturned integer values near 0 from a function, but many others cannot.\nFurther, many real-time, embedded systems will not be designed to\nterminate, so a function or a procedure having parameters with modes out\nor in out will be inappropriate to such applications."}),"\n",(0,i.jsx)(n.p,{children:"This leaves procedures with in parameters. Although some operating\nsystems can pass parameters into a program as it starts, others are not.\nAlso, an implementation may not be able to perform type checking on such\nparameters even if the surrounding environment is capable of providing\nthem."}),"\n",(0,i.jsx)(n.h4,{id:"notes-1",children:"notes"}),"\n",(0,i.jsx)(n.p,{children:'Real-time, embedded applications may not have an "operator" initiating\nthe program to supply the parameters, in which case it would be more\nappropriate for the program to have been compiled with a package\ncontaining the appropriate constant values or for the program to read\nthe necessary values from switch settings or a downloaded auxiliary\nfile. In any case, the variation in surrounding initiating environments\nis far too great to depend upon the kind of last-minute (program)\nparameterization implied by (subprogram) parameters to the main\nsubprogram. POSIX 5 provides a standard operating system command line\ninterface that might be a more appropriate alternative to the Ada\ncommand line facility depending on the implementation family of an\napplication.'}),"\n",(0,i.jsx)(n.h3,{id:"encapsulating-implementation-dependencies",children:"Encapsulating Implementation Dependencies"}),"\n",(0,i.jsx)(n.h4,{id:"guideline-4",children:"guideline"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsx)(n.li,{children:"Create packages specifically designed to isolate hardware and\nimplementation dependencies and designed so that their specification\nwill not change when porting."}),"\n",(0,i.jsx)(n.li,{children:"Clearly indicate the objectives if machine or solution efficiency is\nthe reason for hardware or implementation-dependent code."}),"\n",(0,i.jsx)(n.li,{children:"For the packages that hide implementation dependencies, maintain\ndifferent package bodies for different target environments."}),"\n",(0,i.jsx)(n.li,{children:"Isolate interrupt receiving tasks into implementation-dependent\npackages."}),"\n",(0,i.jsx)(n.li,{children:"Refer to Annex M of the Ada Reference Manual (1995) for a list of\nimplementation-dependent features."}),"\n"]}),"\n",(0,i.jsx)(n.h4,{id:"example-2",children:"example"}),"\n",(0,i.jsx)(n.p,{children:"See Guideline 7.1.3."}),"\n",(0,i.jsx)(n.h4,{id:"rationale-4",children:"rationale"}),"\n",(0,i.jsx)(n.p,{children:"Encapsulating hardware and implementation dependencies in a package\nallows the remainder of the code to ignore them and, thus, to be fully\nportable. It also localizes the dependencies, making it clear exactly\nwhich parts of the code may need to change when porting the program."}),"\n",(0,i.jsx)(n.p,{children:"Some implementation-dependent features may be used to achieve particular\nperformance or efficiency objectives. Commenting these objectives\nensures that the programmer can find an appropriate way to achieve them\nwhen porting to a different implementation or explicitly recognize that\nthey cannot be achieved."}),"\n",(0,i.jsx)(n.p,{children:'Interrupt entries are implementation-dependent features that may not be\nsupported (e.g., VAX Ada uses pragmas to assign system traps to "normal"\nrendezvous). However, interrupt entries cannot be avoided in most\nembedded, real-time systems, and it is reasonable to assume that they\nare supported by an Ada implementation. The value for an interrupt is\nimplementation-defined. Isolate it.'}),"\n",(0,i.jsx)(n.h4,{id:"notes-2",children:"notes"}),"\n",(0,i.jsx)(n.p,{children:"You can use Ada to write machine-dependent programs that take advantage\nof an implementation in a manner consistent with the Ada model but that\nmake particular choices where Ada allows implementation freedom. These\nmachine dependencies should be treated in the same way as any other\nimplementation-dependent features of the code."}),"\n",(0,i.jsx)(n.h3,{id:"implementation-added-features",children:"Implementation-Added Features"}),"\n",(0,i.jsx)(n.h4,{id:"guideline-5",children:"guideline"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsx)(n.li,{children:"Avoid the use of vendor-supplied packages."}),"\n",(0,i.jsx)(n.li,{children:"Avoid the use of features added to the predefined packages that are\nnot specified in the Ada language definition or Specialized Needs\nAnnexes."}),"\n"]}),"\n",(0,i.jsx)(n.h4,{id:"rationale-5",children:"rationale"}),"\n",(0,i.jsx)(n.p,{children:"Vendor-added features are not likely to be provided by other\nimplementations. Even if a majority of vendors eventually provide\nsimilar additional features, they are unlikely to have identical\nformulations. Indeed, different vendors may use the same formulation for\n(semantically) entirely different features. See Guideline 7.5.2 for\nfurther information on vendor-supplied exceptions."}),"\n",(0,i.jsx)(n.p,{children:"Ada has introduced a number of new pragmas and attributes that were not\npresent in Ada 83 (Ada Reference Manual 1983). These new pragmas and\nattributes may clash with implementation-defined pragmas and attributes."}),"\n",(0,i.jsx)(n.h4,{id:"exceptions-1",children:"exceptions"}),"\n",(0,i.jsx)(n.p,{children:"There are many kinds of applications that require the use of these\nfeatures. Examples include multilingual systems that standardize on a\nvendor's file system, applications that are closely integrated with\nvendor products (i.e., user interfaces), and embedded systems for\nperformance reasons. Isolate the use of these features into packages."}),"\n",(0,i.jsx)(n.p,{children:"If a vendor-supplied package is provided in compilable source code form,\nuse of the package does not make a program nonportable provided that the\npackage does not contain any nonportable code and can be lawfully\nincluded in your program."}),"\n",(0,i.jsx)(n.h3,{id:"specialized-needs-annexes",children:"Specialized Needs Annexes"}),"\n",(0,i.jsx)(n.h4,{id:"guideline-6",children:"guideline"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsx)(n.li,{children:"Use features defined in the Specialized Needs Annexes rather than\nvendor-defined features."}),"\n",(0,i.jsx)(n.li,{children:"Document clearly the use of any features from the Specialized Needs\nAnnexes (systems programming, real-time systems, distributed\nsystems, information systems, numerics, and safety and security)."}),"\n"]}),"\n",(0,i.jsx)(n.h4,{id:"rationale-6",children:"rationale"}),"\n",(0,i.jsx)(n.p,{children:"The Specialized Needs Annexes define standards for specific application\nareas without extending the syntax of the language. You can port a\nprogram with specific domain needs (e.g., distributed systems,\ninformation systems) across vendor implementations more easily if they\nsupport the features standardized in an annex rather than rely on\nspecific vendor extensions. The purpose of the annexes is to provide a\nconsistent and uniform way to address issues faced in several\napplication areas where Ada is expected to be used. Because different\ncompilers will support different sets of annexes if any, you may have\nportability problems if you rely on the features defined in any given\nannex."}),"\n",(0,i.jsx)(n.p,{children:"The Specialized Needs Annexes provide special capabilities that go\nbeyond the core language definition. Because compilers are not required\nto support the special-purpose annexes, you should localize your use of\nthese features where possible. By documenting their usage, you are\nleaving a record of potential porting difficulties for future\nprogrammers."}),"\n",(0,i.jsx)(n.h3,{id:"dependence-on-parameter-passing-mechanism",children:"Dependence on Parameter Passing Mechanism"}),"\n",(0,i.jsx)(n.h4,{id:"guideline-7",children:"guideline"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsx)(n.li,{children:"Do not write code whose correct execution depends on the particular\nparameter passing mechanism used by an implementation (Ada Reference\nManual 1995, \xa76.2; Cohen 1986)."}),"\n",(0,i.jsx)(n.li,{children:"If a subprogram has more than one formal parameter of a given\nsubtype, at least one of which is [in] out, make sure that the\nsubprogram can properly handle the case when both formal parameters\ndenote the same actual object."}),"\n"]}),"\n",(0,i.jsx)(n.h4,{id:"example-3",children:"example"}),"\n",(0,i.jsx)(n.p,{children:"The output of this program depends on the particular parameter passing\nmechanism that was used:"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-ada",children:"------------------------------------------------------------------------\nwith Ada.Integer_Text_IO;\nprocedure Outer is\n   type Coordinates is\n      record\n         X : Integer := 0;\n         Y : Integer := 0;\n      end record;\n   Outer_Point : Coordinates;\n   ---------------------------------------------------------------------\n   procedure Inner (Inner_Point : in out Coordinates) is\n   begin\n      Inner_Point.X := 5;\n      -- The following line causes the output of the program to\n      -- depend on the parameter passing mechanism.\n      Ada.Integer_Text_IO.Put(Outer_Point.X);\n   end Inner;\n   ---------------------------------------------------------------------\nbegin  -- Outer\n   Ada.Integer_Text_IO.Put(Outer_Point.X);\n   Inner(Outer_Point);\n   Ada.Integer_Text_IO.Put(Outer_Point.X);\nend Outer;\n------------------------------------------------------------------------\n"})}),"\n",(0,i.jsx)(n.p,{children:"If the parameter passing mechanism is by copy, the results on the\nstandard output file are:"}),"\n",(0,i.jsx)(n.p,{children:"0 0 5"}),"\n",(0,i.jsx)(n.p,{children:"If the parameter passing mechanism is by reference, the results are:"}),"\n",(0,i.jsx)(n.p,{children:"0 5 5"}),"\n",(0,i.jsx)(n.p,{children:"The following code fragment shows where there is a potential for bounded\nerror when a procedure is called with actual parameters denoting the\nsame object:"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-ada",children:"procedure Test_Bounded_Error (Parm_1 : in out    Integer;\n                              Parm_2 : in out Integer) is\n   procedure Inner (Parm : in out Integer) is\n   begin\n      Parm := Parm * 10;\n   end Inner;\nbegin\n   Parm_2 := 5;\n   Inner (Parm_1);\nend Test_Bounded_Error;\n"})}),"\n",(0,i.jsx)(n.p,{children:"In executing the procedure Test_Bounded_Error, both Parm_1 and\nParm_2 denote the object Actual_Parm. After executing the first\nstatement, the object Actual_Parm has the value 5. When the procedure\nInner is called, its formal parameter Parm denotes Actual_Parm. It\ncannot be determined whether it denotes the old value of Parm_1, in\nthis case 1, or the new value, in this case 5."}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-ada",children:"Actual_Parm : Integer := 1;\n...\nTest_Bounded_Error (Actual_Parm, Actual_Parm);  -- potential bounded error\n"})}),"\n",(0,i.jsx)(n.h4,{id:"rationale-7",children:"rationale"}),"\n",(0,i.jsx)(n.p,{children:"Certain composite types (untagged records and arrays) can be passed\neither by copy or by reference. If there are two or more formal\nparameters of the same type, one or more of which is writable, then you\nshould document whether you assume that these formal parameters do not\ndenote the same actual object. Similarly, if a subprogram that has a\nformal parameter of a given subtype also makes an up-level reference to\nan object of this same type, you should document whether you assume that\nthe formal parameter denotes a different object from the object named in\nthe up-level reference. In these situations where an object can be\naccessed through distinct formal parameter paths, the exception\nProgram_Error may be raised, the new value may be read, or the old\nvalue of the object may be used (Ada Reference Manual 1995, \xa76.2)."}),"\n",(0,i.jsx)(n.p,{children:"See also Guideline 8.2.7."}),"\n",(0,i.jsx)(n.h4,{id:"exceptions-2",children:"exceptions"}),"\n",(0,i.jsx)(n.p,{children:"Frequently, when interfacing Ada to foreign code, dependence on\nparameter-passing mechanisms used by a particular implementation is\nunavoidable. In this case, isolate the calls to the foreign code in an\ninterface package that exports operations that do not depend on the\nparameter-passing mechanism."}),"\n",(0,i.jsx)(n.h3,{id:"arbitrary-order-dependencies",children:"Arbitrary Order Dependencies"}),"\n",(0,i.jsx)(n.h4,{id:"guideline-8",children:"guideline"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsx)(n.li,{children:"Avoid depending on the order in which certain constructs in Ada are\nevaluated."}),"\n"]}),"\n",(0,i.jsx)(n.h4,{id:"example-4",children:"example"}),"\n",(0,i.jsx)(n.p,{children:"The output of this program depends upon the order of evaluation of\nsubprogram parameters, but the Ada Reference Manual (1995, \xa76.4)\nspecifies that these evaluations are done in an arbitrary order:"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-ada",children:"package Utilities is\n   function Unique_ID return Integer;\nend Utilities;\n\npackage body Utilities is\n\n   ID : Integer := 0;\n\n   function Unique_ID return Integer is\n   begin\n      ID := ID + 1;\n      return ID;\n   end Unique_ID;\n\nend Utilities;\n\n--------------------------------------------------------------------------------\nwith Ada.Text_IO;\nwith Utilities; use Utilities;\nprocedure P is\nbegin\n   Ada.Text_IO.Put_Line (Integer'Image(Unique_ID) & Integer'Image(Unique_ID));\nend P;\n"})}),"\n",(0,i.jsx)(n.p,{children:'If the parameters to the "&" function are evaluated in textual order,\nthe output is:'}),"\n",(0,i.jsx)(n.p,{children:"1 2"}),"\n",(0,i.jsx)(n.p,{children:"If the parameters are evaluated in the reverse order, the output is:"}),"\n",(0,i.jsx)(n.p,{children:"2 1"}),"\n",(0,i.jsx)(n.h4,{id:"rationale-8",children:"rationale"}),"\n",(0,i.jsx)(n.p,{children:"The Ada language defines certain evaluations to occur in arbitrary order\n(e.g., subprogram parameters). While a dependency on the order of\nevaluation may not adversely affect the program on a certain\nimplementation, the code might not execute correctly when it is ported.\nFor example, if two actual parameters of a subprogram call have side\neffects, the effect of the program could depend on the order of\nevaluation (Ada Reference Manual 1995, \xa71.1.4). Avoid arbitrary order\ndependencies, but also recognize that even an unintentional error of\nthis kind could prohibit portability."})]})}function h(e={}){const{wrapper:n}={...(0,r.R)(),...e.components};return n?(0,i.jsx)(n,{...e,children:(0,i.jsx)(c,{...e})}):c(e)}}}]);