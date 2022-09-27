"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[3119],{4096:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>x,contentTitle:()=>k,default:()=>f,frontMatter:()=>I,metadata:()=>A,toc:()=>g});var a=n(1716),i=n(3050),d=n(7318),r=n(4768),o=Object.defineProperty,l=Object.defineProperties,s=Object.getOwnPropertyDescriptors,u=Object.getOwnPropertySymbols,c=Object.prototype.hasOwnProperty,_=Object.prototype.propertyIsEnumerable,p=(e,t,n)=>t in e?o(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,m=(e,t)=>{for(var n in t||(t={}))c.call(t,n)&&p(e,n,t[n]);if(u)for(var n of u(t))_.call(t,n)&&p(e,n,t[n]);return e};const I={sidebar_position:128},k="A.11 Wide Text Input-Output and Wide Wide Text Input-Output",A={unversionedId:"arm/AA-A/AA-A.11",id:"arm/AA-A/AA-A.11",title:"A.11 Wide Text Input-Output and Wide Wide Text Input-Output",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-A/AA-A.11.mdx",sourceDirName:"arm/AA-A",slug:"/arm/AA-A/AA-A.11",permalink:"/docs/arm/AA-A/AA-A.11",draft:!1,tags:[],version:"current",sidebarPosition:128,frontMatter:{sidebar_position:128},sidebar:"referenceManualSidebar",previous:{title:"A.10 Text Input-Output",permalink:"/docs/arm/AA-A/AA-A.10"},next:{title:"A.12 Stream Input-Output",permalink:"/docs/arm/AA-A/AA-A.12"}},x={},g=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4}],T={toc:g};function f(e){var t,n=e,{components:o}=n,p=((e,t)=>{var n={};for(var a in e)c.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&u)for(var a of u(e))t.indexOf(a)<0&&_.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=m(m({},T),p),l(t,s({components:o,mdxType:"MDXLayout"}))),(0,a.kt)("h1",m({},{id:"a11-wide-text-input-output-and-wide-wide-text-input-output"}),"A.11 Wide Text Input-Output and Wide Wide Text Input-Output"),(0,a.kt)("admonition",m({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",m({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(d.Z,{mdxType:"MarginText"},"1/2"),(0,a.kt)(r.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," ",(0,a.kt)("br",null),"The packages Wide_Text_IO and Wide_Wide_Text_IO provide facilities for input and output in human-readable form. Each file is read or written sequentially, as a sequence of wide characters (or wide wide characters) grouped into lines, and as a sequence of lines grouped into pages. ",(0,a.kt)("br",null)),(0,a.kt)("h4",m({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)(d.Z,{mdxType:"MarginText"},"2/2"),(0,a.kt)(r.Z,{items:["AI95-00285-01","AI95-00301-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00301-01"),"}"," ",(0,a.kt)("br",null),"The specification of package Wide_Text_IO is the same as that for Text_IO, except that in each Get, Look_Ahead, Get_Immediate, Get_Line, Put, and Put_Line subprogram, any occurrence of Character is replaced by Wide_Character, and any occurrence of String is replaced by Wide_String. Nongeneric equivalents of Wide_Text_IO.Integer_IO and Wide_Text_IO.Float_IO are provided (as for Text_IO) for each predefined numeric type, with names such as Ada.Integer_Wide_Text_IO, Ada.Long_Integer_Wide_Text_IO, Ada.Float_Wide_Text_IO, Ada.Long_Float_Wide_Text_IO.",(0,a.kt)("br",null)),(0,a.kt)(d.Z,{mdxType:"MarginText"},"3/2"),(0,a.kt)(r.Z,{items:["AI95-00285-01","AI95-00301-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00301-01"),"}"," ",(0,a.kt)("br",null),"The specification of package Wide_Wide_Text_IO is the same as that for Text_IO, except that in each Get, Look_Ahead, Get_Immediate, Get_Line, Put, and Put_Line subprogram, any occurrence of Character is replaced by Wide_Wide_Character, and any occurrence of String is replaced by Wide_Wide_String. Nongeneric equivalents of Wide_Wide_Text_IO.Integer_IO and Wide_Wide_Text_IO.Float_IO are provided (as for Text_IO) for each predefined numeric type, with names such as Ada.Integer_Wide_Wide_Text_IO, Ada.Long_Integer_Wide_Wide_Text_IO, Ada.Float_Wide_Wide_Text_IO, Ada.Long_Float_Wide_Wide_Text_IO.",(0,a.kt)("br",null)),(0,a.kt)(d.Z,{mdxType:"MarginText"},"4/3"),(0,a.kt)(r.Z,{items:["AI95-00285-01","AI95-00428-01","AI05-0004-1","AI05-0092-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00428-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0004-1"),"}"," ","{",(0,a.kt)("em",null,"AI05-0092-1"),"}"," ",(0,a.kt)("br",null),"The specification of package Wide_Text_IO.Wide_Bounded_IO is the same as that for Text_IO.Bounded_IO, except that any occurrence of Bounded_String is replaced by Bounded_Wide_String, and any occurrence of package Bounded is replaced by Wide_Bounded. The specification of package Wide_Wide_Text_IO.Wide_Wide_Bounded_IO is the same as that for Text_IO.Bounded_IO, except that any occurrence of Bounded_String is replaced by Bounded_Wide_Wide_String, and any occurrence of package Bounded is replaced by Wide_Wide_Bounded.",(0,a.kt)("br",null)),(0,a.kt)(d.Z,{mdxType:"MarginText"},"4.a/3"),(0,a.kt)(r.Z,{items:["AI05-0005-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,a.kt)("strong",null,"To be honest: "),"{",(0,a.kt)("em",null,"AI05-0005-1"),"}"," \u201c",(0,a.kt)("br",null),"package Bounded\u201d refers to both the package Ada.Strings.Bounded and the formal package parameter named Bounded. ",(0,a.kt)("br",null)),(0,a.kt)(d.Z,{mdxType:"MarginText"},"5/3"),(0,a.kt)(r.Z,{items:["AI95-00285-01","AI95-00301-01","AI05-0092-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00301-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0092-1"),"}"," ",(0,a.kt)("br",null),"The specification of package Wide_Text_IO.Wide_Unbounded_IO is the same as that for Text_IO.Unbounded_IO, except that any occurrence of Unbounded_String is replaced by Unbounded_Wide_String, and any occurrence of package Unbounded is replaced by Wide_Unbounded. The specification of package Wide_Wide_Text_IO.Wide_Wide_Unbounded_IO is the same as that for Text_IO.Unbounded_IO, except that any occurrence of Unbounded_String is replaced by Unbounded_Wide_Wide_String, and any occurrence of package Unbounded is replaced by Wide_Wide_Unbounded. ",(0,a.kt)("br",null)),(0,a.kt)("h4",m({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,a.kt)(d.Z,{mdxType:"MarginText"},"5.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Support for Wide_Character and Wide_String I/O is new in Ada 95. ",(0,a.kt)("br",null)),(0,a.kt)("h4",m({},{id:"extensions-to-ada-95"}),"Extensions to Ada 95"),(0,a.kt)(d.Z,{mdxType:"MarginText"},"5.b/2"),(0,a.kt)(r.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," ",(0,a.kt)("br",null),"Package Wide_Wide_Text_IO is new. Be glad it wasn't called Double_Wide_Text_IO (for use in trailer parks) or Really_Wide_Text_IO.",(0,a.kt)("br",null)),(0,a.kt)(d.Z,{mdxType:"MarginText"},"5.c/2"),(0,a.kt)(r.Z,{items:["AI95-00301-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI95-00301-01"),"}"," ",(0,a.kt)("br",null),"Packages Wide_Text_IO.Wide_Unbounded_IO and Wide_Wide_Text_IO.Wide_Wide_Unbounded_IO are also new.",(0,a.kt)("br",null)),(0,a.kt)(d.Z,{mdxType:"MarginText"},"5.d/2"),(0,a.kt)(r.Z,{items:["AI95-00428-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI95-00428-01"),"}"," ",(0,a.kt)("br",null),"Packages Wide_Text_IO.Wide_Bounded_IO and Wide_Wide_Text_IO.Wide_Wide_Bounded_IO are new as well. ",(0,a.kt)("br",null)),(0,a.kt)("h4",m({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,a.kt)(d.Z,{mdxType:"MarginText"},"5.e/3"),(0,a.kt)(r.Z,{items:["AI05-0092-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI05-0092-1"),"}"," ",(0,a.kt)("strong",null,(0,a.kt)("br",null),"Correction"),": Corrected the names of various entities in the above description. Since the previously named entities don't exist and the intent is obvious, this is just considered a presentation change. ",(0,a.kt)("br",null)))}f.isMDXComponent=!0}}]);