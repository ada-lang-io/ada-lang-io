"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[7194],{6805:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>g,contentTitle:()=>f,default:()=>T,frontMatter:()=>h,metadata:()=>y,toc:()=>x});var a=n(1716),i=n(3050),o=n(8604),r=n(7318),l=n(4768),s=Object.defineProperty,d=Object.defineProperties,m=Object.getOwnPropertyDescriptors,c=Object.getOwnPropertySymbols,A=Object.prototype.hasOwnProperty,u=Object.prototype.propertyIsEnumerable,k=(e,t,n)=>t in e?s(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,p=(e,t)=>{for(var n in t||(t={}))A.call(t,n)&&k(e,n,t[n]);if(c)for(var n of c(t))u.call(t,n)&&k(e,n,t[n]);return e};const h={sidebar_position:112},f="13.8 Machine Code Insertions",y={unversionedId:"arm/AA-13/AA-13.8",id:"arm/AA-13/AA-13.8",title:"13.8 Machine Code Insertions",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-13/AA-13.8.mdx",sourceDirName:"arm/AA-13",slug:"/arm/AA-13/AA-13.8",permalink:"/docs/arm/AA-13/AA-13.8",draft:!1,tags:[],version:"current",sidebarPosition:112,frontMatter:{sidebar_position:112},sidebar:"referenceManualSidebar",previous:{title:"13.7 The Package System",permalink:"/docs/arm/AA-13/AA-13.7"},next:{title:"13.9 Unchecked Type Conversions",permalink:"/docs/arm/AA-13/AA-13.9"}},g={},x=[{value:"Syntax",id:"syntax",level:4},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4}],b={toc:x};function T(e){var t,n=e,{components:s}=n,k=((e,t)=>{var n={};for(var a in e)A.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&c)for(var a of c(e))t.indexOf(a)<0&&u.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=p(p({},b),k),d(t,m({components:s,mdxType:"MDXLayout"}))),(0,a.kt)("h1",p({},{id:"138-machine-code-insertions"}),"13.8 Machine Code Insertions"),(0,a.kt)("admonition",p({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",p({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(r.Z,{mdxType:"MarginText"},"1"),(0,a.kt)("p",null,"[ A machine code insertion can be achieved by a call to a subprogram whose ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.1#S0166"},"sequence_of_statements"))," contains ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.8#S0357"},"code_statement")),"s.] ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"syntax"}),"Syntax"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"2"),(0,a.kt)(o.Z,{mdxType:"CodeBlock"},(0,a.kt)("code",null,"code_statement"),(0,a.kt)("a",{id:"S0357"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression")),";",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"3"),(0,a.kt)("p",null,"A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.8#S0357"},"code_statement"))," is only allowed in the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.2#S0304"},"handled_sequence_of_statements"))," of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram_body")),". If a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram_body"))," contains any ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.8#S0357"},"code_statement")),"s, then within this ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram_body"))," the only allowed form of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.1#S0167"},"statement"))," is a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.8#S0357"},"code_statement"))," (labeled or not), the only allowed ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.11#S0087"},"declarative_item")),"s are ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-8/AA-8.4#S0235"},"use_clause")),"s, and no ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.2#S0305"},"exception_handler"))," is allowed (",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.7#S0018"},"comment")),"s and ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.8#S0019"},"pragma")),"s are allowed as usual). ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"name-resolution-rules"}),"Name Resolution Rules"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"4"),(0,a.kt)("p",null,"The ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression"))," is expected to be of any type. ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"legality-rules"}),"Legality Rules"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"5"),(0,a.kt)("p",null,"The ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression"))," shall be of a type declared in package System.Machine_Code. ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"5.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"This includes types declared in children of System.Machine_Code. ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"6"),(0,a.kt)("p",null,"A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.8#S0357"},"code_statement"))," shall appear only within the scope of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-10/AA-10.1#S0294"},"with_clause"))," that mentions package System.Machine_Code. ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"6.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"Note that this is not a note; without this rule, it would be possible to write machine code in compilation units which depend on System.Machine_Code only indirectly. ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"7"),(0,a.kt)("p",null,"The contents of the library package System.Machine_Code (if provided) are implementation defined. The meaning of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.8#S0357"},"code_statement")),"s is implementation defined. [Typically, each ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression"))," represents a machine instruction or assembly directive.] ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"7.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"For example, an instruction might be a record with an Op_Code component and other components for the operands. ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"7.b"),(0,a.kt)(i.Z,{type:"aarm",aarm:"implementation-defined",title:"Implementation defined: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The contents of the visible part of package System.Machine_Code, and the meaning of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.8#S0357"},"code_statement")),"s.",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"8"),(0,a.kt)("p",null,"An implementation may place restrictions on ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.8#S0357"},"code_statement")),"s. An implementation is not required to provide package System.Machine_Code. ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"9/5"),(0,a.kt)(l.Z,{items:["AI12-0440-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 1   ","{",(0,a.kt)("em",null,"AI12-0440-1"),"}"," ",(0,a.kt)("br",null),"An implementation can  provide implementation-defined pragmas specifying register conventions and calling conventions.",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"10/2"),(0,a.kt)(l.Z,{items:["AI95-00318-02"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 2   ","{",(0,a.kt)("em",null,"AI95-00318-02"),"}"," ",(0,a.kt)("br",null),"Machine code functions are exempt from the rule that a return statement is required. In fact, return statements are forbidden, since only ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.8#S0357"},"code_statement")),"s are allowed. ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"10.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The idea is that the author of a machine code subprogram knows the calling conventions, and refers to parameters and results accordingly. The implementation should document where to put the result of a machine code function, for example, \u201cScalar results are returned in register 0\u201d. ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"11"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 3   Intrinsic subprograms (see ",(0,a.kt)("a",{href:"../AA-6/AA-6.3#Subclause_6.3.1"},"6.3.1"),", \u201c",(0,a.kt)("a",{href:"../AA-6/AA-6.3#Subclause_6.3.1"},"Conformance Rules"),"\u201d) can also be used to achieve machine code insertions. Interface to assembly language can be achieved using the features in ",(0,a.kt)("a",{href:"../AA-B/"},"Annex B"),", \u201c",(0,a.kt)("a",{href:"../AA-B/"},"Interface to Other Languages"),"\u201d. ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"examples"}),"Examples"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"12"),(0,a.kt)("p",null,(0,a.kt)("em",null,"Example of a code statement:")," ",(0,a.kt)("br",null)),(0,a.kt)(r.Z,{mdxType:"MarginText"},"13/3"),(0,a.kt)(l.Z,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),(0,a.kt)(o.Z,{language:"ada",mdxType:"CodeBlock"},"--  ","{","AI05-0229-1","}","\n"," M : Mask;","\n","procedure Set_Mask","\n","  with Inline;","\n",(0,a.kt)(r.Z,{mdxType:"MarginText"},"14"),"procedure Set_Mask is","\n","  use System.Machine_Code; -- assume \u201cwith System.Machine_Code;\u201d appears somewhere above","\n","begin","\n","  SI_Format'(Code =",">"," SSM, B =",">"," M'Base_Reg, D =",">"," M'Disp);","\n","  --  Base_Reg and Disp are implementation-defined attributes","\n","end Set_Mask;","\n"),(0,a.kt)("h4",p({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"14.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Machine code functions are allowed in Ada 95; in Ada 83, only procedures were allowed. ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,a.kt)(r.Z,{mdxType:"MarginText"},"14.b"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The syntax for ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.8#S0357"},"code_statement"))," is changed to say \u201c",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression")),"\u201d instead of \u201c",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.2#S0028"},"subtype_mark")),"'",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.3#S0107"},"record_aggregate")),"\u201d. Requiring the type of each instruction to be a record type is overspecification. ",(0,a.kt)("br",null)))}T.isMDXComponent=!0}}]);