"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2340],{67119:(e,n,a)=>{a.r(n),a.d(n,{assets:()=>l,contentTitle:()=>m,default:()=>T,frontMatter:()=>p,metadata:()=>g,toc:()=>c});var t=a(58168),r=(a(96540),a(15680)),i=a(20793),o=a(91435),s=a(21432),y=a(79162),d=a(34421);const p={sidebar_position:145},m="B.5 Interfacing with Fortran",g={unversionedId:"arm/AA-B/AA-B.5",id:"arm/AA-B/AA-B.5",title:"B.5 Interfacing with Fortran",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-B/AA-B.5.mdx",sourceDirName:"arm/AA-B",slug:"/arm/AA-B/AA-B.5",permalink:"/docs/arm/AA-B/AA-B.5",draft:!1,tags:[],version:"current",sidebarPosition:145,frontMatter:{sidebar_position:145},sidebar:"referenceManualSidebar",previous:{title:"B.4 Interfacing with COBOL",permalink:"/docs/arm/AA-B/AA-B.4"},next:{title:"Annex C Systems Programming",permalink:"/docs/arm/AA-C/"}},l={},c=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Implementation Advice",id:"implementation-advice",level:4},{value:"Examples",id:"examples",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],A={toc:c},u="wrapper";function T(e){let{components:n,...a}=e;return(0,r.yg)(u,(0,t.A)({},A,a,{components:n,mdxType:"MDXLayout"}),(0,r.yg)("h1",{id:"b5-interfacing-with-fortran"},"B.5 Interfacing with Fortran"),(0,r.yg)("admonition",{type:"warning"},(0,r.yg)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,r.yg)("a",{parentName:"p",href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"},"tracking issue"))),(0,r.yg)(y.A,{mdxType:"MarginText"},"1/3"),(0,r.yg)(d.A,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),(0,r.yg)("p",null,"The facilities relevant to interfacing with the Fortran language are the package Interfaces.Fortran and support for specifying the Convention aspect with ",(0,r.yg)("em",null,"convention","_"),(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier"))," Fortran.",(0,r.yg)("br",null)),(0,r.yg)(y.A,{mdxType:"MarginText"},"2"),(0,r.yg)("p",null,"The package Interfaces.Fortran defines Ada types whose representations are identical to the default representations of the Fortran intrinsic types Integer, Real, Double Precision, Complex, Logical, and Character in a supported Fortran implementation. These Ada types can therefore be used to pass objects between Ada and Fortran programs. ",(0,r.yg)("br",null)),(0,r.yg)("h4",{id:"static-semantics"},"Static Semantics"),(0,r.yg)(y.A,{mdxType:"MarginText"},"3"),(0,r.yg)("p",null,"The library package Interfaces.Fortran has the following declaration: ",(0,r.yg)("br",null)),(0,r.yg)(y.A,{mdxType:"MarginText"},"4/5"),(0,r.yg)(d.A,{items:["AI12-0414-1"],mdxType:"MarginInfo"}),(0,r.yg)(s.A,{language:"ada",mdxType:"CodeBlock"},"with Ada.Numerics.Generic","_","Complex","_","Types;  -- see ",(0,r.yg)("a",{href:"../AA-G/AA-G.1#Subclause_G.1.1"},"G.1.1"),"\n","pragma Elaborate","_","All(Ada.Numerics.Generic","_","Complex","_","Types);","\n","package Interfaces.Fortran","\n","   with Pure is","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"5"),"type Fortran","_","Integer is range implementation-defined;","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"6"),"type Real             is digits implementation-defined;","\n","   type Double","_","Precision is digits implementation-defined;","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"7"),"type Logical is new Boolean;","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"8"),"package Single","_","Precision","_","Complex","_","Types is","\n","      new Ada.Numerics.Generic","_","Complex","_","Types (Real);","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"9"),"type Complex is new Single","_","Precision","_","Complex","_","Types.Complex;","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"10"),"subtype Imaginary is Single","_","Precision","_","Complex","_","Types.Imaginary;","\n","   i : Imaginary renames Single","_","Precision","_","Complex","_","Types.i;","\n","   j : Imaginary renames Single","_","Precision","_","Complex","_","Types.j;","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"10.1/5"),(0,r.yg)(d.A,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),"package Double","_","Precision","_","Complex","_","Types is","\n","      new Ada.Numerics.Generic","_","Complex","_","Types (Double","_","Precision);","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"10.2/5"),(0,r.yg)(d.A,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),"type Double","_","Complex is new Double","_","Precision","_","Complex","_","Types.Complex;","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"10.3/5"),(0,r.yg)(d.A,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),"subtype Double","_","Imaginary is Double","_","Precision","_","Complex","_","Types.Imaginary;","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"11"),"type Character","_","Set is implementation-defined character type;","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"12/3"),(0,r.yg)(d.A,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),"type Fortran","_","Character is array (Positive range ","<",">",") of Character","_","Set","\n","      with Pack;","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"13"),"function To","_","Fortran (Item : in Character) return Character","_","Set;","\n","   function To","_","Ada (Item : in Character","_","Set) return Character;","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"14"),"function To","_","Fortran (Item : in String) return Fortran","_","Character;","\n","   function To","_","Ada     (Item : in Fortran","_","Character) return String;","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"15"),"procedure To","_","Fortran (Item       : in String;","\n","                         Target     : out Fortran","_","Character;","\n","                         Last       : out Natural);","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"16"),"procedure To","_","Ada (Item     : in Fortran","_","Character;","\n","                     Target   : out String;","\n","                     Last     : out Natural);","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"17"),"end Interfaces.Fortran;","\n"),(0,r.yg)(o.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(y.A,{mdxType:"MarginText"},"17.a.1/1"),(0,r.yg)(i.A,{type:"aarm",aarm:"implementation-defined",mdxType:"Admonition"},(0,r.yg)("strong",null),"The types Fortran","_","Integer, Real, Double","_","Precision, and Character","_","Set in Interfaces.Fortran.",(0,r.yg)("br",null))),(0,r.yg)(o.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(y.A,{mdxType:"MarginText"},"17.a/5"),(0,r.yg)(d.A,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),(0,r.yg)(i.A,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,r.yg)("strong",null),"The means by which the Complex and Double","_","Complex types are provided in Interfaces.Fortran creates a dependence of Interfaces.Fortran on Numerics.Generic","_","Complex","_","Types (see ",(0,r.yg)("a",{href:"../AA-G/AA-G.1#Subclause_G.1.1"},"G.1.1"),"). This dependence is intentional and unavoidable, if the Fortran-compatible Complex and Double","_","Complex types are to be useful in Ada code without duplicating facilities defined elsewhere. ",(0,r.yg)("br",null))),(0,r.yg)(y.A,{mdxType:"MarginText"},"18/5"),(0,r.yg)(d.A,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),(0,r.yg)("p",null,"The types Fortran","_","Integer, Real, Double","_","Precision, Logical, Complex, Double","_","Complex, Character","_","Set, and Fortran","_","Character are Fortran-compatible.",(0,r.yg)("br",null)),(0,r.yg)(y.A,{mdxType:"MarginText"},"19"),(0,r.yg)("p",null,"The To","_","Fortran and To","_","Ada functions map between the Ada type Character and the Fortran type Character","_","Set, and also between the Ada type String and the Fortran type Fortran","_","Character. The To","_","Fortran and To","_","Ada procedures have analogous effects to the string conversion subprograms found in Interfaces.COBOL. ",(0,r.yg)("br",null)),(0,r.yg)("h4",{id:"implementation-requirements"},"Implementation Requirements"),(0,r.yg)(y.A,{mdxType:"MarginText"},"20/3"),(0,r.yg)(d.A,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),(0,r.yg)("p",null,"An implementation shall support specifying aspect Convention with a Fortran ",(0,r.yg)("em",null,"convention"),"_",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier"))," for a Fortran-eligible type (see ",(0,r.yg)("a",{href:"../AA-B/AA-B.1"},"B.1"),"). ",(0,r.yg)("br",null)),(0,r.yg)("h4",{id:"implementation-permissions"},"Implementation Permissions"),(0,r.yg)(y.A,{mdxType:"MarginText"},"21/5"),(0,r.yg)(d.A,{items:["AI12-0058-1","AI12-0263-1","AI12-0450-1"],mdxType:"MarginInfo"}),(0,r.yg)("p",null,"An implementation may add additional declarations to the Fortran interface packages. For example, declarations are permitted for the character types corresponding to Fortran character kinds 'ascii' and 'iso","_","10646', which in turn correspond to ISO/IEC 646:1991 and to UCS-4 as specified in ISO/IEC 10646:2020. ",(0,r.yg)("br",null)),(0,r.yg)(o.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(y.A,{mdxType:"MarginText"},"21.a.1/5"),(0,r.yg)(d.A,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),(0,r.yg)(i.A,{type:"aarm",aarm:"reason",mdxType:"Admonition"},(0,r.yg)("strong",null),"Fortran compilers are required to recognize 'ascii' and 'iso","_","10646' as arguments to the SELECTED","_","CHAR","_","KIND intrinsic function, but are not required to support those kinds. ",(0,r.yg)("br",null))),(0,r.yg)(o.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(y.A,{mdxType:"MarginText"},"21.a/5"),(0,r.yg)(d.A,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),(0,r.yg)(i.A,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,r.yg)("strong",null),"Implementations may add auxiliary declarations as needed to assist in the declarations of additional Fortran-compatible types. For example,  if a wide character type is defined to match a Fortran 90 wide character type (accessible in Fortran 90 with the Kind attribute), then an auxiliary character set may be declared to serve as its component type. ",(0,r.yg)("br",null))),(0,r.yg)("h4",{id:"implementation-advice"},"Implementation Advice"),(0,r.yg)(y.A,{mdxType:"MarginText"},"22"),(0,r.yg)("p",null,"An Ada implementation should support the following interface correspondences between Ada and Fortran: ",(0,r.yg)("br",null)),(0,r.yg)(y.A,{mdxType:"MarginText"},"23"),(0,r.yg)("ul",null,(0,r.yg)("li",null,"An Ada procedure corresponds to a Fortran subroutine.",(0,r.yg)("br",null)),(0,r.yg)(y.A,{mdxType:"MarginText"},"24"),(0,r.yg)("li",null,"An Ada function corresponds to a Fortran function.",(0,r.yg)("br",null)),(0,r.yg)(y.A,{mdxType:"MarginText"},"25"),(0,r.yg)("li",null,"An Ada parameter of an elementary, array, or record type T is passed as a TF argument to a Fortran procedure, where TF is the Fortran type corresponding to the Ada type T, and where the INTENT attribute of the corresponding dummy argument matches the Ada formal parameter mode; the Fortran implementation's parameter passing conventions are used. For elementary types, a local copy is used if necessary to ensure by-copy semantics.",(0,r.yg)("br",null)),(0,r.yg)(y.A,{mdxType:"MarginText"},"26"),(0,r.yg)("li",null,"An Ada parameter of an access-to-subprogram type is passed as a reference to a Fortran procedure whose interface corresponds to the designated subprogram's specification. ",(0,r.yg)("br",null))),(0,r.yg)(o.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(y.A,{mdxType:"MarginText"},"26.a/2"),(0,r.yg)(i.A,{type:"aarm",aarm:"implementation-advice",mdxType:"Admonition"},(0,r.yg)("strong",null),"If Fortran interfacing is supported, the interface correspondences between Ada and Fortran should be supported.",(0,r.yg)("br",null))),(0,r.yg)(o.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(y.A,{mdxType:"MarginText"},"27"),(0,r.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE 1   An object of a Fortran-compatible record type, declared in a library package or subprogram, can correspond to a Fortran common block; the type also corresponds to a Fortran \u201cderived type\u201d.",(0,r.yg)("br",null))),(0,r.yg)(o.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(y.A,{mdxType:"MarginText"},"28/5"),(0,r.yg)(d.A,{items:["AI12-0224-1"],mdxType:"MarginInfo"}),(0,r.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE 2   For Fortran facilities not addressed by this subclause, consider using the Fortran to C interoperability features defined in ISO/IEC 1594-1:2018 along with the C interfacing features defined in ",(0,r.yg)("a",{href:"../AA-B/AA-B.3"},"B.3"),".",(0,r.yg)("br",null))),(0,r.yg)("h4",{id:"examples"},"Examples"),(0,r.yg)(y.A,{mdxType:"MarginText"},"29"),(0,r.yg)("p",null,(0,r.yg)("em",null,"Example of Interfaces.Fortran:")," ",(0,r.yg)("br",null)),(0,r.yg)(y.A,{mdxType:"MarginText"},"30"),(0,r.yg)(s.A,{language:"ada",mdxType:"CodeBlock"},"with Interfaces.Fortran;","\n","use Interfaces.Fortran;","\n","procedure Ada","_","Application is","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"31/5"),(0,r.yg)(d.A,{items:["AI05-0229-1","AI12-0178-1"],mdxType:"MarginInfo"}),"type Fortran","_","Matrix is ","\n","      array (Fortran","_","Integer range ","<",">",",","\n","             Fortran","_","Integer range ","<",">",") of Double","_","Precision","\n","      with Convention =",">"," Fortran;                  -- stored in Fortran's","\n","                                                   -- column-major order","\n","   procedure Invert (Rank : in Fortran","_","Integer; X : in out Fortran","_","Matrix)","\n","      with Import =",">"," True, Convention =",">"," Fortran; -- a Fortran subroutine","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"32"),"Rank      : constant Fortran","_","Integer := 100;","\n","   My","_","Matrix : Fortran","_","Matrix (1 .. Rank, 1 .. Rank);","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"33/5"),(0,r.yg)(d.A,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),"Precision: constant := 6;","\n","   type Standard","_","Deviation is digits Precision","\n","      with Convention =",">"," Fortran;","\n","   Deviation : Standard","_","Deviation;","\n","      -- Declarations to match the following Fortran declarations:","\n","      --   integer, parameter :: precision = selected","_","real","_","kind(p=6)","\n","      --   real(precision) :: deviation","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"34"),"begin","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"35"),"...","\n","   My","_","Matrix := ...;","\n","   ...","\n","   Invert (Rank, My","_","Matrix);","\n","   ...","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"36/5"),(0,r.yg)(d.A,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),"Deviation := ...;","\n","   ...","\n",(0,r.yg)(y.A,{mdxType:"MarginText"},"37"),"end Ada","_","Application;","\n"),(0,r.yg)(o.A,{mdxType:"AnnotatedOnly"},(0,r.yg)("h4",{id:"wording-changes-from-ada-2012"},"Wording Changes from Ada 2012")),(0,r.yg)(o.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(y.A,{mdxType:"MarginText"},"37.a/5"),(0,r.yg)(d.A,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),(0,r.yg)(i.A,{type:"aarm",aarm:"correction",mdxType:"Admonition"},(0,r.yg)("strong",null)," The package Double","_","Precision","_","Complex","_","Types and associated types are added to package Interfaces.Fortran. In unusual circumstances, this could cause an incompatibility; we don't document it as an incompatibility as implementations are allowed to add declarations to this package, so that risk of an incompatibility is present for any move from one version of an implementation to another (not to mention to another implementation). As such, the language-defined additions make no change in the risk of incompatibility. ",(0,r.yg)("br",null))))}T.isMDXComponent=!0}}]);