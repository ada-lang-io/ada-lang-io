"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[4100],{9256:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>f,contentTitle:()=>T,default:()=>_,frontMatter:()=>k,metadata:()=>x,toc:()=>h});var a=n(1716),r=n(3050),i=n(8604),o=n(7318),s=n(4768),p=Object.defineProperty,l=Object.defineProperties,m=Object.getOwnPropertyDescriptors,d=Object.getOwnPropertySymbols,c=Object.prototype.hasOwnProperty,u=Object.prototype.propertyIsEnumerable,y=(e,t,n)=>t in e?p(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,g=(e,t)=>{for(var n in t||(t={}))c.call(t,n)&&y(e,n,t[n]);if(d)for(var n of d(t))u.call(t,n)&&y(e,n,t[n]);return e};const k={sidebar_position:145},T="B.5 Interfacing with Fortran",x={unversionedId:"arm/AA-B/AA-B.5",id:"arm/AA-B/AA-B.5",title:"B.5 Interfacing with Fortran",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-B/AA-B.5.mdx",sourceDirName:"arm/AA-B",slug:"/arm/AA-B/AA-B.5",permalink:"/docs/arm/AA-B/AA-B.5",draft:!1,tags:[],version:"current",sidebarPosition:145,frontMatter:{sidebar_position:145},sidebar:"referenceManualSidebar",previous:{title:"B.4 Interfacing with COBOL",permalink:"/docs/arm/AA-B/AA-B.4"},next:{title:"Annex C Systems Programming",permalink:"/docs/arm/AA-C/"}},f={},h=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Implementation Advice",id:"implementation-advice",level:4},{value:"Examples",id:"examples",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],A={toc:h};function _(e){var t,n=e,{components:p}=n,y=((e,t)=>{var n={};for(var a in e)c.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&d)for(var a of d(e))t.indexOf(a)<0&&u.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=g(g({},A),y),l(t,m({components:p,mdxType:"MDXLayout"}))),(0,a.kt)("h1",g({},{id:"b5-interfacing-with-fortran"}),"B.5 Interfacing with Fortran"),(0,a.kt)("admonition",g({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",g({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(o.Z,{mdxType:"MarginText"},"1/3"),(0,a.kt)(s.Z,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"The facilities relevant to interfacing with the Fortran language are the package Interfaces.Fortran and support for specifying the Convention aspect with ",(0,a.kt)("em",null,"convention_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier"))," Fortran.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"2"),(0,a.kt)("p",null,"The package Interfaces.Fortran defines Ada types whose representations are identical to the default representations of the Fortran intrinsic types Integer, Real, Double Precision, Complex, Logical, and Character in a supported Fortran implementation. These Ada types can therefore be used to pass objects between Ada and Fortran programs. ",(0,a.kt)("br",null)),(0,a.kt)("h4",g({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"3"),(0,a.kt)("p",null,"The library package Interfaces.Fortran has the following declaration: ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"4/5"),(0,a.kt)(s.Z,{items:["AI12-0414-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{language:"ada",mdxType:"CodeBlock"},"with Ada.Numerics.Generic_Complex_Types;  -- see ",(0,a.kt)("a",{href:"../AA-G/AA-G.1#Subclause_G.1.1"},"G.1.1"),"\n","pragma Elaborate_All(Ada.Numerics.Generic_Complex_Types);","\n","package Interfaces.Fortran ","\n","   with  Pure is ","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"5"),"type Fortran_Integer is range implementation-defined;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"6"),"type Real             is digits implementation-defined;","\n","   type Double_Precision is digits implementation-defined;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"7"),"type Logical is new Boolean;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"8"),"package Single_Precision_Complex_Types is","\n","      new Ada.Numerics.Generic_Complex_Types (Real);","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"9"),"type Complex is new Single_Precision_Complex_Types.Complex;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"10"),"subtype Imaginary is Single_Precision_Complex_Types.Imaginary;","\n","   i : Imaginary renames Single_Precision_Complex_Types.i;","\n","   j : Imaginary renames Single_Precision_Complex_Types.j;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"10.1/5"),(0,a.kt)(s.Z,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),"package Double_Precision_Complex_Types is","\n","      new Ada.Numerics.Generic_Complex_Types (Double_Precision);","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"10.2/5"),(0,a.kt)(s.Z,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),"type Double_Complex is new Double_Precision_Complex_Types.Complex;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"10.3/5"),(0,a.kt)(s.Z,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),"subtype Double_Imaginary is Double_Precision_Complex_Types.Imaginary;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"11"),"type Character_Set is implementation-defined character type;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"12/3"),(0,a.kt)(s.Z,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),"type Fortran_Character is array (Positive range ","<",">",") of Character_Set","\n","      with Pack;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"13"),"function To_Fortran (Item : in Character) return Character_Set;","\n","   function To_Ada (Item : in Character_Set) return Character;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"14"),"function To_Fortran (Item : in String) return Fortran_Character;","\n","   function To_Ada     (Item : in Fortran_Character) return String;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"15"),"procedure To_Fortran (Item       : in String;","\n","                         Target     : out Fortran_Character;","\n","                         Last       : out Natural);","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"16"),"procedure To_Ada (Item     : in Fortran_Character;","\n","                     Target   : out String;","\n","                     Last     : out Natural);","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"17"),"end Interfaces.Fortran;","\n"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"17.a.1/1"),(0,a.kt)(r.Z,{type:"aarm",aarm:"implementation-defined",title:"Implementation defined: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The types Fortran_Integer, Real, Double_Precision, and Character_Set in Interfaces.Fortran.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"17.a/5"),(0,a.kt)(s.Z,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The means by which the Complex and Double_Complex types are  provided in Interfaces.Fortran creates a dependence of Interfaces.Fortran on Numerics.Generic_Complex_Types (see ",(0,a.kt)("a",{href:"../AA-G/AA-G.1#Subclause_G.1.1"},"G.1.1"),"). This dependence is intentional and unavoidable, if the Fortran-compatible Complex and Double_Complex types are  to be useful in Ada code without duplicating facilities defined elsewhere. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"18/5"),(0,a.kt)(s.Z,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"The types Fortran_Integer, Real, Double_Precision, Logical, Complex, Double_Complex, Character_Set, and Fortran_Character are Fortran-compatible.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"19"),(0,a.kt)("p",null,"The To_Fortran and To_Ada functions map between the Ada type Character and the Fortran type Character_Set, and also between the Ada type String and the Fortran type Fortran_Character. The To_Fortran and To_Ada procedures have analogous effects to the string conversion subprograms found in Interfaces.COBOL. ",(0,a.kt)("br",null)),(0,a.kt)("h4",g({},{id:"implementation-requirements"}),"Implementation Requirements"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"20/3"),(0,a.kt)(s.Z,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"An implementation shall support specifying aspect Convention with a Fortran ",(0,a.kt)("em",null,"convention"),"_",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier"))," for a Fortran-eligible type (see ",(0,a.kt)("a",{href:"../AA-B/AA-B.1"},"B.1"),"). ",(0,a.kt)("br",null)),(0,a.kt)("h4",g({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"21/5"),(0,a.kt)(s.Z,{items:["AI12-0058-1","AI12-0263-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"An implementation may add additional declarations to the Fortran interface packages. For example, declarations are permitted for the character types corresponding to Fortran character kinds 'ascii' and 'iso_10646', which in turn correspond to ISO/IEC 646:1991 and to UCS-4 as specified in ISO/IEC 10646:2017 . ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"21.a.1/5"),(0,a.kt)(s.Z,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"Fortran compilers are required to recognize 'ascii' and 'iso_10646' as arguments to the SELECTED_CHAR_KIND intrinsic function, but are not required to support those kinds. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"21.a/5"),(0,a.kt)(s.Z,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"Implementations may add auxiliary declarations as needed to assist in the declarations of additional Fortran-compatible types. For example,   if a wide character type is defined to match a Fortran 90 wide character type (accessible in Fortran 90 with the Kind attribute ), then an auxiliary character set may be declared to serve as its component type. ",(0,a.kt)("br",null)),(0,a.kt)("h4",g({},{id:"implementation-advice"}),"Implementation Advice"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"22"),(0,a.kt)("p",null,"An Ada implementation should support the following interface correspondences between Ada and Fortran: ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"23"),(0,a.kt)("ul",null,(0,a.kt)("li",null,"An Ada procedure corresponds to a Fortran subroutine.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"24"),(0,a.kt)("li",null,"An Ada function corresponds to a Fortran function.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"25"),(0,a.kt)("li",null,"An Ada parameter of an elementary, array, or record type T is passed as a TF argument to a Fortran procedure, where TF is the Fortran type corresponding to the Ada type T, and where the INTENT attribute of the corresponding dummy argument matches the Ada formal parameter mode; the Fortran implementation's parameter passing conventions are used. For elementary types, a local copy is used if necessary to ensure by-copy semantics.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"26"),(0,a.kt)("li",null,"An Ada parameter of an access-to-subprogram type is passed as a reference to a Fortran procedure whose interface corresponds to the designated subprogram's specification. ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"MarginText"},"26.a/2"),(0,a.kt)(r.Z,{type:"aarm",aarm:"implementation-advice",title:"Implementation Advice",mdxType:"Admonition"},(0,a.kt)("strong",null),"If Fortran interfacing is supported, the interface correspondences between Ada and Fortran should be supported.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"27"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 1   An object of a Fortran-compatible record type, declared in a library package or subprogram, can correspond to a Fortran common block; the type also corresponds to a Fortran \u201cderived type\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"28/5"),(0,a.kt)(s.Z,{items:["AI12-0224-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 2   For Fortran facilities not addressed by this subclause, consider using the Fortran to C interoperability features defined in ISO/IEC 1594-1:2018 along with the C interfacing features defined in ",(0,a.kt)("a",{href:"../AA-B/AA-B.3"},"B.3"),".",(0,a.kt)("br",null)),(0,a.kt)("h4",g({},{id:"examples"}),"Examples"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"29"),(0,a.kt)("p",null,(0,a.kt)("em",null,"Example of Interfaces.Fortran:")," ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"30"),(0,a.kt)(i.Z,{language:"ada",mdxType:"CodeBlock"},"with Interfaces.Fortran;","\n","use Interfaces.Fortran;","\n","procedure Ada_Application is","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"31/5"),(0,a.kt)(s.Z,{items:["AI05-0229-1","AI12-0178-1"],mdxType:"MarginInfo"}),"type Fortran_Matrix is ","\n","      array (Fortran_Integer  range ","<",">",",","\n","             Fortran_Integer  range ","<",">",") of Double_Precision","\n","      with Convention =",">"," Fortran;                  -- stored in Fortran's","\n","                                                   -- column-major order","\n","   procedure Invert (Rank : in Fortran_Integer; X : in out Fortran_Matrix)","\n","      with Import =",">"," True, Convention =",">"," Fortran; -- a Fortran subroutine","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"32"),"Rank      : constant Fortran_Integer := 100;","\n","   My_Matrix : Fortran_Matrix (1 .. Rank, 1 .. Rank);","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"32.1/5"),(0,a.kt)(s.Z,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),"Precision: constant := 6;","\n","   type Standard_Deviation is digits Precision","\n","      with Convention =",">"," Fortran;","\n","   Deviation : Standard_Deviation;","\n","      -- Declarations to match the following Fortran declarations:","\n","      --   integer, parameter :: precision = selected_real_kind(p=6)","\n","      --   real(precision) :: deviation","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"33"),"begin","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"34"),"...","\n","   My_Matrix := ...;","\n","   ...","\n","   Invert (Rank, My_Matrix);","\n","   ...","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"34.1/5"),(0,a.kt)(s.Z,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),"Deviation := ...;","\n","   ...","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"35"),"end Ada_Application;","\n"),(0,a.kt)("h4",g({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"35.a/5"),(0,a.kt)(s.Z,{items:["AI12-0058-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"correction",title:"Correction:",mdxType:"Admonition"},(0,a.kt)("strong",null)," The package Double_Precision_Complex_Types and associated types are added to package Interfaces.Fortran. In unusual circumstances, this could cause an incompatibility; we don't document it as an incompatibility as implementations are allowed to add declarations to this package, so that risk of an incompatibility is present for any move from one version of an implementation to another (not to mention to another implementation). As such, the language-defined additions make no change in the risk of incompatibility. ",(0,a.kt)("br",null)))}_.isMDXComponent=!0}}]);