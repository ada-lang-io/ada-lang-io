"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[7043],{2922:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>W,contentTitle:()=>p,default:()=>h,frontMatter:()=>l,metadata:()=>I,toc:()=>m});var a=n(1716),i=Object.defineProperty,d=Object.defineProperties,o=Object.getOwnPropertyDescriptors,r=Object.getOwnPropertySymbols,s=Object.prototype.hasOwnProperty,u=Object.prototype.propertyIsEnumerable,c=(e,t,n)=>t in e?i(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,_=(e,t)=>{for(var n in t||(t={}))s.call(t,n)&&c(e,n,t[n]);if(r)for(var n of r(t))u.call(t,n)&&c(e,n,t[n]);return e};const l={sidebar_position:128},p="A.11  Wide Text Input-Output and Wide Wide Text Input-Output",I={unversionedId:"arm/AA-A.11",id:"arm/AA-A.11",title:"A.11  Wide Text Input-Output and Wide Wide Text Input-Output",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-A.11.mdx",sourceDirName:"arm",slug:"/arm/AA-A.11",permalink:"/docs/arm/AA-A.11",draft:!1,tags:[],version:"current",sidebarPosition:128,frontMatter:{sidebar_position:128},sidebar:"tutorialSidebar",previous:{title:"A.10  Text Input-Output",permalink:"/docs/arm/AA-A.10"},next:{title:"A.12  Stream Input-Output",permalink:"/docs/arm/AA-A.12"}},W={},m=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4}],f={toc:m};function h(e){var t,n=e,{components:i}=n,c=((e,t)=>{var n={};for(var a in e)s.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&r)for(var a of r(e))t.indexOf(a)<0&&u.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=_(_({},f),c),d(t,o({components:i,mdxType:"MDXLayout"}))),(0,a.kt)("h1",_({},{id:"a11--wide-text-input-output-and-wide-wide-text-input-output"}),"A.11  Wide Text Input-Output and Wide Wide Text Input-Output"),(0,a.kt)("admonition",_({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,a.kt)("a",_({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," The packages Wide_Text_IO and Wide_Wide_Text_IO provide facilities for input and output in human-readable form. Each file is read or written sequentially, as a sequence of wide characters (or wide wide characters) grouped into lines, and as a sequence of lines grouped into pages. "),(0,a.kt)("h4",_({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00301-01"),"}"," The specification of package Wide_Text_IO is the same as that for Text_IO, except that in each Get, Look_Ahead, Get_Immediate, Get_Line, Put, and Put_Line subprogram, any occurrence of Character is replaced by Wide_Character, and any occurrence of String is replaced by Wide_String. Nongeneric equivalents of Wide_Text_IO.Integer_IO and Wide_Text_IO.Float_IO are provided (as for Text_IO) for each predefined numeric type, with names such as Ada.Integer_Wide_Text_IO, Ada.Long_Integer_Wide_Text_IO, Ada.Float_Wide_Text_IO, Ada.Long_Float_Wide_Text_IO."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00301-01"),"}"," The specification of package Wide_Wide_Text_IO is the same as that for Text_IO, except that in each Get, Look_Ahead, Get_Immediate, Get_Line, Put, and Put_Line subprogram, any occurrence of Character is replaced by Wide_Wide_Character, and any occurrence of String is replaced by Wide_Wide_String. Nongeneric equivalents of Wide_Wide_Text_IO.Integer_IO and Wide_Wide_Text_IO.Float_IO are provided (as for Text_IO) for each predefined numeric type, with names such as Ada.Integer_Wide_Wide_Text_IO, Ada.Long_Integer_Wide_Wide_Text_IO, Ada.Float_Wide_Wide_Text_IO, Ada.Long_Float_Wide_Wide_Text_IO."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00428-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0004-1"),"}"," ","{",(0,a.kt)("em",null,"AI05-0092-1"),"}"," The specification of package Wide_Text_IO.Wide_Bounded_IO is the same as that for Text_IO.Bounded_IO, except that any occurrence of Bounded_String is replaced by Bounded_Wide_String, and any occurrence of package Bounded is replaced by Wide_Bounded. The specification of package Wide_Wide_Text_IO.Wide_Wide_Bounded_IO is the same as that for Text_IO.Bounded_IO, except that any occurrence of Bounded_String is replaced by Bounded_Wide_Wide_String, and any occurrence of package Bounded is replaced by Wide_Wide_Bounded."),(0,a.kt)("p",null,(0,a.kt)("strong",null,"To be honest: "),"{",(0,a.kt)("em",null,"AI05-0005-1"),"}",' "package Bounded" refers to both the package Ada.Strings.Bounded and the formal package parameter named Bounded. '),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00301-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0092-1"),"}"," The specification of package Wide_Text_IO.Wide_Unbounded_IO is the same as that for Text_IO.Unbounded_IO, except that any occurrence of Unbounded_String is replaced by Unbounded_Wide_String, and any occurrence of package Unbounded is replaced by Wide_Unbounded. The specification of package Wide_Wide_Text_IO.Wide_Wide_Unbounded_IO is the same as that for Text_IO.Unbounded_IO, except that any occurrence of Unbounded_String is replaced by Unbounded_Wide_Wide_String, and any occurrence of package Unbounded is replaced by Wide_Wide_Unbounded. "),(0,a.kt)("h4",_({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,a.kt)("p",null,"Support for Wide_Character and Wide_String I/O is new in Ada 95. "),(0,a.kt)("h4",_({},{id:"extensions-to-ada-95"}),"Extensions to Ada 95"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00285-01"),"}"," Package Wide_Wide_Text_IO is new. Be glad it wasn't called Double_Wide_Text_IO (for use in trailer parks) or Really_Wide_Text_IO."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00301-01"),"}"," Packages Wide_Text_IO.Wide_Unbounded_IO and Wide_Wide_Text_IO.Wide_Wide_Unbounded_IO are also new."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00428-01"),"}"," Packages Wide_Text_IO.Wide_Bounded_IO and Wide_Wide_Text_IO.Wide_Wide_Bounded_IO are new as well. "),(0,a.kt)("h4",_({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0092-1"),"}"," ",(0,a.kt)("strong",null,"Correction"),": Corrected the names of various entities in the above description. Since the previously named entities don't exist and the intent is obvious, this is just considered a presentation change. "))}h.isMDXComponent=!0}}]);