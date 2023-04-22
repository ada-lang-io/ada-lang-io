"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[9278],{15272:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>g,contentTitle:()=>h,default:()=>y,frontMatter:()=>p,metadata:()=>m,toc:()=>f});var a=n(91716),i=Object.defineProperty,o=Object.defineProperties,r=Object.getOwnPropertyDescriptors,s=Object.getOwnPropertySymbols,l=Object.prototype.hasOwnProperty,d=Object.prototype.propertyIsEnumerable,u=(e,t,n)=>t in e?i(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,c=(e,t)=>{for(var n in t||(t={}))l.call(t,n)&&u(e,n,t[n]);if(s)for(var n of s(t))d.call(t,n)&&u(e,n,t[n]);return e};const p={title:"4.1 High-Level Structure"},h=void 0,m={unversionedId:"style-guide/s4/01",id:"style-guide/s4/01",title:"4.1 High-Level Structure",description:"Well-structured programs are easily understood, enhanced, and",source:"@site/docs/style-guide/s4/01.mdx",sourceDirName:"style-guide/s4",slug:"/style-guide/s4/01",permalink:"/docs/style-guide/s4/01",draft:!1,tags:[],version:"current",frontMatter:{title:"4.1 High-Level Structure"},sidebar:"styleGuideSidebar",previous:{title:"4. Program Structure",permalink:"/docs/style-guide/s4/"},next:{title:"4.2 Visibility",permalink:"/docs/style-guide/s4/02"}},g={},f=[{value:"Separate Compilation Capabilities",id:"separate-compilation-capabilities",level:3},{value:"guideline",id:"guideline",level:4},{value:"example",id:"example",level:4},{value:"rationale",id:"rationale",level:4},{value:"Configuration Pragmas",id:"configuration-pragmas",level:3},{value:"guideline",id:"guideline-1",level:4},{value:"rationale",id:"rationale-1",level:4},{value:"exceptions",id:"exceptions",level:4},{value:"Subprograms",id:"subprograms",level:3},{value:"guideline",id:"guideline-2",level:4},{value:"example",id:"example-1",level:4},{value:"rationale",id:"rationale-2",level:4},{value:"notes",id:"notes",level:4},{value:"Functions",id:"functions",level:3},{value:"guideline",id:"guideline-3",level:4},{value:"example",id:"example-2",level:4},{value:"rationale",id:"rationale-3",level:4},{value:"Packages",id:"packages",level:3},{value:"guideline",id:"guideline-4",level:4},{value:"example",id:"example-3",level:4},{value:"rationale",id:"rationale-4",level:4},{value:"notes",id:"notes-1",level:4},{value:"Child Library Units",id:"child-library-units",level:3},{value:"guideline",id:"guideline-5",level:4},{value:"example",id:"example-4",level:4},{value:"rationale",id:"rationale-5",level:4},{value:"Cohesion",id:"cohesion",level:3},{value:"guideline",id:"guideline-6",level:4},{value:"example",id:"example-5",level:4},{value:"rationale",id:"rationale-6",level:4},{value:"notes",id:"notes-2",level:4},{value:"Data Coupling",id:"data-coupling",level:3},{value:"guideline",id:"guideline-7",level:4},{value:"example",id:"example-6",level:4},{value:"rationale",id:"rationale-7",level:4},{value:"notes",id:"notes-3",level:4},{value:"Tasks",id:"tasks",level:3},{value:"guideline",id:"guideline-8",level:4},{value:"rationale",id:"rationale-8",level:4},{value:"Protected Types",id:"protected-types",level:3},{value:"guideline",id:"guideline-9",level:4},{value:"example",id:"example-7",level:4},{value:"rationale",id:"rationale-9",level:4}],b={toc:f};function y(e){var t,n=e,{components:i}=n,u=((e,t)=>{var n={};for(var a in e)l.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&s)for(var a of s(e))t.indexOf(a)<0&&d.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=c(c({},b),u),o(t,r({components:i,mdxType:"MDXLayout"}))),(0,a.kt)("p",null,"Well-structured programs are easily understood, enhanced, and\nmaintained. Poorly structured programs are frequently restructured\nduring maintenance just to make the job easier. Many of the guidelines\nlisted below are often given as general program design guidelines."),(0,a.kt)("h3",c({},{id:"separate-compilation-capabilities"}),"Separate Compilation Capabilities"),(0,a.kt)("h4",c({},{id:"guideline"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Place the specification of each library unit package in a separate\nfile from its body."),(0,a.kt)("li",{parentName:"ul"},"Avoid defining library unit subprograms that are not intended to be\nused as main programs. If such subprograms are defined, then create\nan explicit specification, in a separate file, for each library unit\nsubprogram."),(0,a.kt)("li",{parentName:"ul"},"Minimize the use of subunits."),(0,a.kt)("li",{parentName:"ul"},"In preference to subunits, use child library units to structure a\nsubsystem into manageable units."),(0,a.kt)("li",{parentName:"ul"},"Place each subunit in a separate file."),(0,a.kt)("li",{parentName:"ul"},"Use a consistent file naming convention."),(0,a.kt)("li",{parentName:"ul"},"In preference to nesting in a package body, use a private child and\nwith it to the parent body."),(0,a.kt)("li",{parentName:"ul"},"Use private child unit specifications for data and subprograms that\nare required by (other) child units that extend a parent unit's\nabstraction or services.")),(0,a.kt)("h4",c({},{id:"example"}),"example"),(0,a.kt)("p",null,"The file names below illustrate one possible file organization and\nassociated consistent naming convention. The library unit name uses the\nadb suffix for the body. The suffix ads indicates the specification, and\nany files containing subunits use names constructed by separating the\nbody name from the subunit name with an underscore:"),(0,a.kt)("p",null,(0,a.kt)("inlineCode",{parentName:"p"},"text_io.ads \u2014 the specification"),(0,a.kt)("br",{parentName:"p"}),"\n",(0,a.kt)("inlineCode",{parentName:"p"},"text_io.adb \u2014 the body"),(0,a.kt)("br",{parentName:"p"}),"\n",(0,a.kt)("inlineCode",{parentName:"p"},"text_io_integer_io.adb \u2014 a subunit"),(0,a.kt)("br",{parentName:"p"}),"\n",(0,a.kt)("inlineCode",{parentName:"p"},"text_io_fixed_io.adb \u2014 a subunit"),(0,a.kt)("br",{parentName:"p"}),"\n",(0,a.kt)("inlineCode",{parentName:"p"},"text_io_float_io.adb \u2014 a subunit"),(0,a.kt)("br",{parentName:"p"}),"\n",(0,a.kt)("inlineCode",{parentName:"p"},"text_io_enumeration_io.adb \u2014 a subunit")),(0,a.kt)("p",null,'Depending on what characters your file system allows you to use in file\nnames, you could show the distinction between parent and subunit name\nmore clearly in the file name. If your file system allows the "',"#",'"\ncharacter, for example, you could separate the body name from the\nsubunit name with a ',"#",":"),(0,a.kt)("p",null,(0,a.kt)("inlineCode",{parentName:"p"},"text_io.ads \u2014 the specification"),(0,a.kt)("br",{parentName:"p"}),"\n",(0,a.kt)("inlineCode",{parentName:"p"},"text_io.adb \u2014 the body"),(0,a.kt)("br",{parentName:"p"}),"\n",(0,a.kt)("inlineCode",{parentName:"p"},"text_io#integer_io.adb \u2014 a subunit"),(0,a.kt)("br",{parentName:"p"}),"\n",(0,a.kt)("inlineCode",{parentName:"p"},"text_io#fixed_io.adb \u2014 a subunit"),(0,a.kt)("br",{parentName:"p"}),"\n",(0,a.kt)("inlineCode",{parentName:"p"},"text_io#float_io.adb \u2014 a subunit"),(0,a.kt)("br",{parentName:"p"}),"\n",(0,a.kt)("inlineCode",{parentName:"p"},"text_io#enumeration_io.adb \u2014 a subunit")),(0,a.kt)("p",null,"Some operating systems are case-sensitive, although Ada itself is not a\ncase-sensitive language. For example, you could choose a convention of\nall lowercase file names."),(0,a.kt)("h4",c({},{id:"rationale"}),"rationale"),(0,a.kt)("p",null,"The main reason for the emphasis on separate files in this guideline is\nto minimize the amount of recompilation required after each change.\nTypically, during software development, bodies of units are updated far\nmore often than specifications. If the body and specification reside in\nthe same file, then the specification will be compiled each time the\nbody is compiled, even though the specification has not changed. Because\nthe specification defines the interface between the unit and all of its\nusers, this recompilation of the specification typically makes\nrecompilation of all users necessary in order to verify compliance with\nthe specification. If the specifications and bodies of the users also\nreside together, then any users of these units will also have to be\nrecompiled and so on. The ripple effect can force a huge number of\ncompilations that could have been avoided, severely slowing the\ndevelopment and test phase of a project. This is why you should place\nspecifications of all library units (nonnested units) in separate files\nfrom their bodies."),(0,a.kt)("p",null,"Library unit subprograms should be minimized. The only real use for\nlibrary unit subprograms is as the main subprogram. In almost all other\ncases, it is better to embed the subprogram into a package. This\nprovides a place (the package body) to localize data needed by the\nsubprogram. Moreover, it cuts down on the number of separate modules in\nthe system."),(0,a.kt)("p",null,"In general, you should use a separate specification for any library\nsubprogram that is mentioned in a with clause. This makes the with'ing\nunit dependent on the library subprogram specification, not its body."),(0,a.kt)("p",null,"You should minimize the use of subunits because they create maintenance\nproblems. Declarations appearing in the parent body are visible in the\nsubunit, increasing the amount of data global to the subunit and, thus,\nincreasing the potential ripple effect of changes. Subunits hinder reuse\nbecause they provide an incentive to put otherwise reusable code in the\nsubunit directly rather than in a common routine called from multiple\nsubprograms."),(0,a.kt)("p",null,"With the availability of child library units in Ada 95, you can avoid\nmost uses of subunits. For example, instead of using a subunit for a\nlarge nested body, you should try to encapsulate this code in a child\nlibrary unit and add the necessary context clauses. You can modify the\nbody of the child unit without having to recompile any of the other\nunits in a subsystem."),(0,a.kt)("p",null,"An additional benefit of using multiple, separate files is that it\nallows different implementors to modify different parts of the system at\nthe same time with conventional editors, which do not allow multiple\nconcurrent updates to a single file."),(0,a.kt)("p",null,"Finally, keeping bodies and specifications separate makes it possible to\nhave multiple bodies for the same specification or multiple\nspecifications for the same body. Although Ada requires that there be\nexactly one specification per body in a system at any given time, it can\nstill be useful to maintain multiple bodies or multiple specifications\nfor use in different builds of a system. For example, a single\nspecification may have multiple bodies, each of which implements the\nsame functionality with a different tradeoff of time versus space\nefficiency, or, for machine-dependent code, there may be one body for\neach target machine. Maintaining multiple package specifications can\nalso be useful during development and test. You may develop one\nspecification for delivery to your customer and another for unit\ntesting. The first one would export only those subprograms intended to\nbe called from outside of the package during normal operation of the\nsystem. The second one would export all subprograms of the package so\nthat each of them could be independently tested."),(0,a.kt)("p",null,"A consistent file naming convention is recommended to make it easier to\nmanage the large number of files that may result from following this\nguideline."),(0,a.kt)("p",null,"In implementing the abstraction defined in a package specification, you\noften need to write supporting subprograms that manipulate the internal\nrepresentation of the data. These subprograms should not be exported on\nthe interface. You have a choice of whether to place them in the package\nbody of the parent program or in a child package named in a context\nclause of the parent package body. When you place them in the parent\npackage body, you make them inaccessible to all clients of the parent,\nincluding extensions of the parent declared in child packages. If these\nsubprograms are needed to implement extensions of the parent\nabstraction, you would be forced to modify both the parent specification\nand the body because you would have to declare the extensions within the\nparent specification. This technique would then force recompilation of\nthe entire package (specification and body) as well as all its clients."),(0,a.kt)("p",null,"Alternatively, you can implement the supporting subprograms in a private\nchild package. Because the parent unit's specification is not modified,\nneither it nor its clients need to be recompiled. The data and\nsubprograms that might have declared in the parent unit body must now be\ndeclared in the private child unit's specification to make them visible\nto both the parent unit body and to any child units that extend the\nparent unit's services or abstractions. (See also Guidelines 4.1.6 and\n4.2.) This use of private child units will generally minimize\nrecompilations within the unit family and among its clients."),(0,a.kt)("p",null,"In declaring the child package private, you achieve a similar effect to\ndeclaring it in the parent package body to the extent that clients of\nthe parent cannot name the private child in a context clause. You gain\nflexibility because now you can extend the parent abstraction using\nchild packages without having to recompile the parent specification or\nits body, assuming that you do not otherwise modify the parent or its\nbody. This added flexibility will usually compensate for the increased\ndependency between units, in this case, the additional context clause on\nthe parent body (and other child package bodies) that names the private\nchild package of supporting subprograms."),(0,a.kt)("h3",c({},{id:"configuration-pragmas"}),"Configuration Pragmas"),(0,a.kt)("h4",c({},{id:"guideline-1"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"When possible, express configuration pragmas through compiler\noptions or other means that do not require modifications to the\nsource code."),(0,a.kt)("li",{parentName:"ul"},"When configuration pragmas must be placed in source code, consider\nisolating them to one compilation unit per partition; if specified,\nthe main subprogram for the partition is recommended.")),(0,a.kt)("h4",c({},{id:"rationale-1"}),"rationale"),(0,a.kt)("p",null,"Configuration pragmas are generally used to select a partition-wide or\nsystem-wide option. Usually, they reflect either high-level software\narchitecture decisions (e.g., pragma Task_Dispatching_Policy) or the\nuse of the software in a particular application domain (e.g.,\nsafety-critical software). If a configuration pragma is embedded within\na software component and that component is reused in a different context\nwhere the pragma is no longer appropriate, then it may cause problems in\nthe new application. Such problems can include the rejection by the\ncompilation system of otherwise legal source code or unexpected behavior\nat run-time. These problems can be significant given the wide scope of a\nconfiguration pragma. In addition, maintenance of the original system\nmay require that some of these system-wide decisions be changed. If the\nconfiguration pragmas are scattered throughout the software, it may be\ndifficult to locate the lines that need to change."),(0,a.kt)("p",null,"As a result, it is recommended that all configuration pragmas be kept in\na single compilation unit if possible to make them easy to locate and\nmodify as needed. If this compilation unit is unlikely to be reused\n(e.g., a main subprogram), then the likelihood of conflicts with future\nreusers is reduced. Finally, if these system-wide decisions are\nindicated without embedding them in the code at all, such as through a\ncompiler option, then the problems described above are even less likely\nto occur."),(0,a.kt)("h4",c({},{id:"exceptions"}),"exceptions"),(0,a.kt)("p",null,"Certain pragmas (e.g., pragma Suppress) can be used in several forms,\nincluding as a configuration pragma. This guideline does not apply to\nsuch pragmas when they are not used as a configuration pragma."),(0,a.kt)("h3",c({},{id:"subprograms"}),"Subprograms"),(0,a.kt)("h4",c({},{id:"guideline-2"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Use subprograms to enhance abstraction."),(0,a.kt)("li",{parentName:"ul"},"Restrict each subprogram to the performance of a single action.")),(0,a.kt)("h4",c({},{id:"example-1"}),"example"),(0,a.kt)("p",null,"Your program is required to draw a menu of user options as part of a\nmenu-driven user interface package. Because the contents of the menu can\nvary depending on the user state, the proper way to do this is to write\na subprogram to draw the menu. This way, the output subprogram has one\npurpose and the way to determine the menu content is described\nelsewhere."),(0,a.kt)("pre",null,(0,a.kt)("code",c({parentName:"pre"},{className:"language-ada"}),'...\n----------------------------------------------------------------------\nprocedure Draw_Menu\n      (Title   : in    String;\n       Options : in    Menu) is\n   ...\nbegin  -- Draw_Menu\n   Ada.Text_IO.New_Page;\n   Ada.Text_IO.New_Line;\n   Ada.Text_IO.Set_Col (Right_Column);\n   Ada.Text_IO.Put_Line (Title);\n   Ada.Text_IO.New_Line;\n   for Choice in Alpha_Numeric loop\n     if Options (Choice) /= Empty_Line then\n         Valid_Option (Choice) := True;\n         Ada.Text_IO.Set_Col (Left_Column);\n         Ada.Text_IO.Put (Choice & " -- ");\n         Ada.Text_IO.Put_Line (Options (Choice));\n     end if;\n     ...\n   end loop;\nend Draw_Menu;\n----------------------------------------------------------------------\n')),(0,a.kt)("h4",c({},{id:"rationale-2"}),"rationale"),(0,a.kt)("p",null,"Subprograms are an extremely effective and well-understood abstraction\ntechnique. Subprograms increase program readability by hiding the\ndetails of a particular activity. It is not necessary that a subprogram\nbe called more than once to justify its existence."),(0,a.kt)("h4",c({},{id:"notes"}),"notes"),(0,a.kt)("p",null,"Guideline 10.7.1 discusses dealing with the overhead of subroutine\ncalls."),(0,a.kt)("h3",c({},{id:"functions"}),"Functions"),(0,a.kt)("h4",c({},{id:"guideline-3"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Use a function when the subprogram's primary purpose is to provide a\nsingle value."),(0,a.kt)("li",{parentName:"ul"},"Minimize the side effect of a function."),(0,a.kt)("li",{parentName:"ul"},"Consider using a parameterless function when the value does not need\nto be static."),(0,a.kt)("li",{parentName:"ul"},"Use a parameterless function (instead of a constant) if the value\nshould be inherited by types derived from the type."),(0,a.kt)("li",{parentName:"ul"},"Use a parameterless function if the value itself is subject to\nchange.")),(0,a.kt)("h4",c({},{id:"example-2"}),"example"),(0,a.kt)("p",null,"Although reading a character from a file will change what character is\nread next, this is accepted as a minor side effect compared to the\nprimary purpose of the following function:"),(0,a.kt)("pre",null,(0,a.kt)("code",c({parentName:"pre"},{className:"language-ada"}),"function Next_Character return Character is separate;\n")),(0,a.kt)("p",null,"However, the use of a function like this could lead to a subtle problem.\nAny time the order of evaluation is undefined, the order of the values\nreturned by the function will effectively be undefined. In this example,\nthe order of the characters placed in Word and the order that the\nfollowing two characters are given to the Suffix parameters are unknown.\nNo implementation of the Next_Character function can guarantee which\ncharacter will go where:"),(0,a.kt)("pre",null,(0,a.kt)("code",c({parentName:"pre"},{className:"language-ada"}),"   Word : constant String := String'(1 .. 5 => Next_Character);\nbegin  -- Start_Parsing\n   Parse(Keyword => Word,\n         Suffix1 => Next_Character,\n         Suffix2 => Next_Character);\nend Start_Parsing;\n")),(0,a.kt)("p",null,"Of course, if the order is unimportant (as in a random number\ngenerator), then the order of evaluation is unimportant."),(0,a.kt)("p",null,"The following example shows the use of a parameterless function instead\nof a constant:"),(0,a.kt)("pre",null,(0,a.kt)("code",c({parentName:"pre"},{className:"language-ada"}),"type T is private;\nfunction Nil return T;        -- This function is a derivable operation of type T\nfunction Default return T;    -- Also derivable, and the value can be changed by\n                              -- recompiling the body of the function\n")),(0,a.kt)("p",null,"This same example could have been written using constants:"),(0,a.kt)("pre",null,(0,a.kt)("code",c({parentName:"pre"},{className:"language-ada"}),"type T is private;\nNil : constant T;\nDefault : constant T;\n")),(0,a.kt)("h4",c({},{id:"rationale-3"}),"rationale"),(0,a.kt)("p",null,"A side effect is a change to any variable that is not local to the\nsubprogram. This includes changes to variables by other subprograms and\nentries during calls from the function if the changes persist after the\nfunction returns. Side effects are discouraged because they are\ndifficult to understand and maintain. Additionally, the Ada language\ndoes not define the order in which functions are evaluated when they\noccur in expressions or as actual parameters to subprograms. Therefore,\na program that depends on the order in which side effects of functions\noccur is erroneous. Avoid using side effects anywhere."),(0,a.kt)("h3",c({},{id:"packages"}),"Packages"),(0,a.kt)("h4",c({},{id:"guideline-4"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Use packages for information hiding."),(0,a.kt)("li",{parentName:"ul"},"Use packages with tagged types and private types for abstract data\ntypes."),(0,a.kt)("li",{parentName:"ul"},"Use packages to model abstract entities appropriate to the problem\ndomain."),(0,a.kt)("li",{parentName:"ul"},"Use packages to group together related type and object declarations\n(e.g., common declarations for two or more library units)."),(0,a.kt)("li",{parentName:"ul"},"Encapsulate machine dependencies in packages. Place a software\ninterface to a particular device in a package to facilitate a change\nto a different device."),(0,a.kt)("li",{parentName:"ul"},"Place low-level implementation decisions or interfaces in\nsubprograms within packages."),(0,a.kt)("li",{parentName:"ul"},"Use packages and subprograms to encapsulate and hide program details\nthat may change (Nissen and Wallis 1984).")),(0,a.kt)("h4",c({},{id:"example-3"}),"example"),(0,a.kt)("p",null,"Reading the names and other attributes of external files is highly\nmachine dependent. A package called Directory could contain type and\nsubprogram declarations to support a generalized view of an external\ndirectory that contains external files. Its internals may, in turn,\ndepend on other packages more specific to the hardware or operating\nsystem:"),(0,a.kt)("pre",null,(0,a.kt)("code",c({parentName:"pre"},{className:"language-ada"}),"package Directory is\n\n   type Directory_Listing is limited private;\n\n   procedure Read_Current_Directory (D : in out Directory_Listing);\n\n   generic\n      with procedure Process (Filename : in String);\n   procedure Iterate (Over : in Directory_Listing);\n\n   ...\n\nprivate\n\n   type Directory_Listing is ...\n\nend Directory;\n\n---------------------------------------------------------------\n\npackage body Directory is\n\n   -- This procedure is machine dependent\n   procedure Read_Current_Directory (D : in out Directory_Listing) is separate;\n\n   procedure Iterate (Over : in Directory_Listing) is\n      ...\n   begin\n      ...\n\n      Process (Filename);\n\n      ...\n   end Iterate;\n\n   ...\n\nend Directory;\n")),(0,a.kt)("h4",c({},{id:"rationale-4"}),"rationale"),(0,a.kt)("p",null,"Packages are the principal structuring facility in Ada. They are\nintended to be used as direct support for abstraction, information\nhiding, and modularization. For example, they are useful for\nencapsulating machine dependencies as an aid to portability. A single\nspecification can have multiple bodies isolating implementation-specific\ninformation so other parts of the code do not need to change."),(0,a.kt)("p",null,"Encapsulating areas of potential change helps to minimize the effort\nrequired to implement that change by preventing unnecessary dependencies\namong unrelated parts of the system."),(0,a.kt)("h4",c({},{id:"notes-1"}),"notes"),(0,a.kt)("p",null,"The most prevalent objection to this guideline usually involves\nperformance penalties. See Guideline 10.7.1 for a discussion about\nsubprogram overhead."),(0,a.kt)("h3",c({},{id:"child-library-units"}),"Child Library Units"),(0,a.kt)("h4",c({},{id:"guideline-5"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"If a new library unit represents a logical extension to the original\nabstraction, define it as a child library unit."),(0,a.kt)("li",{parentName:"ul"},"If a new library unit is independent (e.g., introduces a new\nabstraction that depends only in part on the existing one), then\nencapsulate the new abstraction in a separate library unit."),(0,a.kt)("li",{parentName:"ul"},"Use child packages to implement a subsystem."),(0,a.kt)("li",{parentName:"ul"},"Use public child units for those parts of a subsystem that should be\nvisible to clients of the subsystem."),(0,a.kt)("li",{parentName:"ul"},"Use private child units for those parts of a subsystem that should\nnot be visible to clients of the subsystem."),(0,a.kt)("li",{parentName:"ul"},"Use private child units for local declarations used only in\nimplementing the package specification."),(0,a.kt)("li",{parentName:"ul"},"Use child packages to implement constructors, even when they return\naccess values.")),(0,a.kt)("h4",c({},{id:"example-4"}),"example"),(0,a.kt)("p",null,"The following example of a windowing system is taken from Cohen et al.\n(1993) and illustrates some of the uses of child units in designing\nsubsystems. The parent (root) package declares the types, subtypes, and\nconstants that its clients and subsystems need. Individual child\npackages provide specific parts of the windowing abstraction, such as\natoms, fonts, graphic output, cursors, and keyboard information:"),(0,a.kt)("pre",null,(0,a.kt)("code",c({parentName:"pre"},{className:"language-ada"}),"package X_Windows is\n   ...\nprivate\n   ...\nend X_Windows;\n\npackage X_Windows.Atoms is\n   type Atom is private;\n   ...\nprivate\n   ...\nend X_Windows.Atoms;\n\npackage X_Windows.Fonts is\n   type Font is private;\n   ...\nprivate\n   ...\nend X_Windows.Fonts;\n\npackage X_Windows.Graphic_Output is\n   type Graphic_Context is private;\n   type Image is private;\n   ...\nprivate\n   ...\nend X_Windows.Graphic_Output;\n\npackage X_Windows.Cursors is\n   ...\nend X_Windows.Cursors;\n\npackage X_Windows.Keyboard is\n   ...\nend X_Windows.Keyboard;\n")),(0,a.kt)("h4",c({},{id:"rationale-5"}),"rationale"),(0,a.kt)("p",null,"The user can create more precise packages with less cluttered\ninterfaces, using child library packages to extend the interfaces as\nneeded. The parent contains only the relevant functionality. The parent\nprovides a general-purpose interface, while the child units provide more\ncomplete programming interfaces, tailored to that aspect of an\nabstraction that they are extending or defining."),(0,a.kt)("p",null,'Child packages build on the modular strength of Ada where "the distinct\nspecification and body decouple the user interface to a package (the\nspecification) from its implementation (the body)" (Rationale 1995,\n\xa7II.7). Child packages provide the added capability of being able to\nextend a parent package without recompiling the parent or the parent\'s\nclients.'),(0,a.kt)("p",null,"Child packages allow you to write logically distinct packages that share\na private type. The visibility rules give the private part of the child\nspecification and the body of the child visibility into the private part\nof the parent. Thus, you can avoid creating a monolithic package for the\nsake of developing abstractions that share a private type and need to\nknow its representation. The private representation is not available to\nclients of the package, so the abstraction in the package and its\nchildren is maintained."),(0,a.kt)("p",null,"Using private child packages for local declarations enables you to have\navailable the support declarations you need when implementing both the\nparent package and extensions to the parent package. You enhance the\nmaintainability of your program by using a common set of support\ndeclarations (data representations, data manipulation subprograms). You\ncan modify the internal representation and the implementation of the\nsupport subprograms without modifying or recompiling the rest of your\nsubsystem because these support subprograms are implemented in the body\nof the private child package. See also Guidelines 4.1.1, 4.2.1, 8.4.1,\nand 8.4.8."),(0,a.kt)("p",null,"See also Guideline 9.4.1 for a discussion of the use of child library\nunits in creating a tagged type hierarchy."),(0,a.kt)("h3",c({},{id:"cohesion"}),"Cohesion"),(0,a.kt)("h4",c({},{id:"guideline-6"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Make each package serve a single purpose."),(0,a.kt)("li",{parentName:"ul"},"Use packages to group related data, types, and subprograms."),(0,a.kt)("li",{parentName:"ul"},"Avoid collections of unrelated objects and subprograms (NASA 1987;\nNissen and Wallis 1984)."),(0,a.kt)("li",{parentName:"ul"},"Consider restructuring a system to move two highly related units\ninto the same package (or package hierarchy) or to move relatively\nindependent units into separate packages.")),(0,a.kt)("h4",c({},{id:"example-5"}),"example"),(0,a.kt)("p",null,'As a bad example, a package named Project_Definitions is obviously a\n"catch all" for a particular project and is likely to be a jumbled mess.\nIt probably has this form to permit project members to incorporate a\nsingle with clause into their software.'),(0,a.kt)("p",null,"Better examples are packages called Display_Format_Definitions,\ncontaining all the types and constants needed by some specific display\nin a specific format, and Cartridge_Tape_Handler, containing all the\ntypes, constants, and subprograms that provide an interface to a\nspecial-purpose device."),(0,a.kt)("h4",c({},{id:"rationale-6"}),"rationale"),(0,a.kt)("p",null,"The degree to which the entities in a package are related has a direct\nimpact on the ease of understanding packages and programs made up of\npackages. There are different criteria for grouping, and some criteria\nare less effective than others. Grouping the class of data or activity\n(e.g., initialization modules) or grouping data or activities based on\ntheir timing characteristics is less effective than grouping based on\nfunction or need to communicate through data (Charette 1986)."),(0,a.kt)("p",null,'The "correct" structuring of a system can make a tremendous difference\nin the maintainability of a system. Although it may seem painful at the\ntime, it is important to restructure if the initial structuring is not\nquite right.'),(0,a.kt)("p",null,"See also Guideline 5.4.2 on heterogeneous data."),(0,a.kt)("h4",c({},{id:"notes-2"}),"notes"),(0,a.kt)("p",null,"Traditional subroutine libraries often group functionally unrelated\nsubroutines. Even such libraries should be broken into a collection of\npackages, each containing a logically cohesive set of subprograms."),(0,a.kt)("h3",c({},{id:"data-coupling"}),"Data Coupling"),(0,a.kt)("h4",c({},{id:"guideline-7"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Avoid declaring variables in package specifications.")),(0,a.kt)("h4",c({},{id:"example-6"}),"example"),(0,a.kt)("p",null,"This is part of a compiler. Both the package handling error messages and\nthe package containing the code generator need to know the current line\nnumber. Rather than storing this in a shared variable of type Natural,\nthe information is stored in a package that hides the details of how\nsuch information is represented and makes it available with access\nroutines:"),(0,a.kt)("pre",null,(0,a.kt)("code",c({parentName:"pre"},{className:"language-ada"}),"-------------------------------------------------------------------------\npackage Compilation_Status is\n   type Line_Number is range 1 .. 2_500_000;\n   function Source_Line_Number return Line_Number;\nend Compilation_Status;\n-------------------------------------------------------------------------\nwith Compilation_Status;\npackage Error_Message_Processing is\n   -- Handle compile-time diagnostic.\nend Error_Message_Processing;\n-------------------------------------------------------------------------\nwith Compilation_Status;\n\npackage Code_Generation is\n   -- Operations for code generation.\nend Code_Generation;\n-------------------------------------------------------------------------\n")),(0,a.kt)("h4",c({},{id:"rationale-7"}),"rationale"),(0,a.kt)("p",null,"Strongly coupled program units can be difficult to debug and very\ndifficult to maintain. By protecting shared data with access functions,\nthe coupling is lessened. This prevents dependence on the data\nstructure, and access to the data can be controlled."),(0,a.kt)("h4",c({},{id:"notes-3"}),"notes"),(0,a.kt)("p",null,"The most prevalent objection to this guideline usually involves\nperformance penalties. When a variable is moved to the package body,\nsubprograms to access the variable must be provided and the overhead\ninvolved during each call to those subprograms is introduced. See\nGuideline 10.7.1 for a discussion about subprogram overhead."),(0,a.kt)("h3",c({},{id:"tasks"}),"Tasks"),(0,a.kt)("h4",c({},{id:"guideline-8"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Use tasks to model abstract, asynchronous entities within the\nproblem domain."),(0,a.kt)("li",{parentName:"ul"},"Use tasks to define concurrent algorithms for multiprocessor\narchitectures."),(0,a.kt)("li",{parentName:"ul"},"Use tasks to perform concurrent, cyclic, or prioritized activities\n(NASA 1987).")),(0,a.kt)("h4",c({},{id:"rationale-8"}),"rationale"),(0,a.kt)("p",null,"The rationale for this guideline is given under Guideline 6.1.2. Chapter\n6 discusses tasking in more detail."),(0,a.kt)("h3",c({},{id:"protected-types"}),"Protected Types"),(0,a.kt)("h4",c({},{id:"guideline-9"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Use protected types to control or synchronize access to data or\ndevices."),(0,a.kt)("li",{parentName:"ul"},"Use protected types to implement synchronization tasks, such as a\npassive resource monitor.")),(0,a.kt)("h4",c({},{id:"example-7"}),"example"),(0,a.kt)("p",null,"See example in Guideline 6.1.1."),(0,a.kt)("h4",c({},{id:"rationale-9"}),"rationale"),(0,a.kt)("p",null,"The rationale for this guideline is given under Guideline 6.1.1. Chapter\n6 discusses concurrency and protected types in more detail."))}y.isMDXComponent=!0}}]);