"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[3810],{69080:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>f,contentTitle:()=>k,default:()=>T,frontMatter:()=>g,metadata:()=>A,toc:()=>b});var n=a(91716),r=a(21256),i=a(24895),o=a(48424),s=a(82262),l=Object.defineProperty,d=Object.defineProperties,m=Object.getOwnPropertyDescriptors,p=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,y=(e,t,a)=>t in e?l(e,t,{enumerable:!0,configurable:!0,writable:!0,value:a}):e[t]=a,h=(e,t)=>{for(var a in t||(t={}))u.call(t,a)&&y(e,a,t[a]);if(p)for(var a of p(t))c.call(t,a)&&y(e,a,t[a]);return e};const g={sidebar_position:85},k="10 Program Structure and Compilation Issues",A={unversionedId:"arm/AA-10/AA-10",id:"arm/AA-10/AA-10",title:"10 Program Structure and Compilation Issues",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-10/AA-10.mdx",sourceDirName:"arm/AA-10",slug:"/arm/AA-10/",permalink:"/docs/arm/AA-10/",draft:!1,tags:[],version:"current",sidebarPosition:85,frontMatter:{sidebar_position:85},sidebar:"referenceManualSidebar",previous:{title:"9.11 Example of Tasking and Synchronization",permalink:"/docs/arm/AA-9/AA-9.11"},next:{title:"10.1 Separate Compilation",permalink:"/docs/arm/AA-10/AA-10.1"}},f={},b=[{value:"Language Design Principles",id:"language-design-principles",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4}],x={toc:b};function T(e){var t,a=e,{components:l}=a,y=((e,t)=>{var a={};for(var n in e)u.call(e,n)&&t.indexOf(n)<0&&(a[n]=e[n]);if(null!=e&&p)for(var n of p(e))t.indexOf(n)<0&&c.call(e,n)&&(a[n]=e[n]);return a})(a,["components"]);return(0,n.kt)("wrapper",(t=h(h({},x),y),d(t,m({components:l,mdxType:"MDXLayout"}))),(0,n.kt)("h1",h({},{id:"10-program-structure-and-compilation-issues"}),"10 Program Structure and Compilation Issues"),(0,n.kt)("admonition",h({},{type:"warning"}),(0,n.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,n.kt)("a",h({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,n.kt)(o.Z,{mdxType:"MarginText"},"1/3"),(0,n.kt)(s.Z,{items:["AI05-0299-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"[The overall structure of programs and the facilities for separate compilation are described in this clause. A ",(0,n.kt)("em",null,"program")," is a set of ",(0,n.kt)("em",null,"partitions"),", each of which may execute in a separate address space, possibly on a separate computer. ",(0,n.kt)("br",null)),(0,n.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,n.kt)(o.Z,{mdxType:"MarginText"},"1.a/5"),(0,n.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,n.kt)("strong",null,"Term entry: "),(0,n.kt)("strong",null,"program")," \u2014 set of partitions, each of which can execute in a separate address space, possibly on a separate computer",(0,n.kt)("br",null))),(0,n.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,n.kt)(o.Z,{mdxType:"MarginText"},"1.b/5"),(0,n.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,n.kt)("strong",null,"Term entry: "),(0,n.kt)("strong",null,"partition")," \u2014 part of a program, which consists of a set of interdependent library units",(0,n.kt)("br",null),(0,n.kt)("br",null))),(0,n.kt)(o.Z,{mdxType:"MarginText"},"2"),(0,n.kt)("p",null,"As explained below, a partition is constructed from ",(0,n.kt)("em",null,"library units"),". Syntactically, the declaration of a library unit is a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-10/AA-10.1#S0287"},"library_item")),", as is the body of a library unit. An implementation may support a concept of a ",(0,n.kt)("em",null,"program library")," (or simply, a \u201clibrary\u201d), which contains ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-10/AA-10.1#S0287"},"library_item")),"s and their subunits. Library units may be organized into a hierarchy of children, grandchildren, and so on.]",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"3/3"),(0,n.kt)(s.Z,{items:["AI05-0299-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"This clause has two subclauses: ",(0,n.kt)("a",{href:"../AA-10/AA-10.1"},"10.1"),", \u201c",(0,n.kt)("a",{href:"../AA-10/AA-10.1"},"Separate Compilation"),"\u201d discusses compile-time issues related to separate compilation. ",(0,n.kt)("a",{href:"../AA-10/AA-10.2"},"10.2"),", \u201c",(0,n.kt)("a",{href:"../AA-10/AA-10.2"},"Program Execution"),"\u201d discusses issues related to what is traditionally known as \u201clink time\u201d and \u201crun time\u201d \u2014 building and executing partitions.",(0,n.kt)("br",null)),(0,n.kt)("h4",h({},{id:"language-design-principles"}),"Language Design Principles"),(0,n.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,n.kt)(o.Z,{mdxType:"MarginText"},"3.a"),(0,n.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"We should avoid specifying details that are outside the domain of the language itself. The standard is intended (at least in part) to promote portability of Ada programs at the source level. It is not intended to standardize extra-language issues such as how one invokes the compiler (or other tools), how one's source is represented and organized, version management, the format of error messages, etc.",(0,n.kt)("br",null))),(0,n.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,n.kt)(o.Z,{mdxType:"MarginText"},"3.b"),(0,n.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"The rules of the language should be enforced even in the presence of separate compilation. Using separate compilation should not make a program less safe.",(0,n.kt)("br",null))),(0,n.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,n.kt)(o.Z,{mdxType:"MarginText"},"3.c"),(0,n.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"It should be possible to determine the legality of a compilation unit by looking only at the compilation unit itself and the compilation units upon which it depends semantically. As an example, it should be possible to analyze the legality of two compilation units in parallel if they do not depend semantically upon each other.",(0,n.kt)("br",null))),(0,n.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,n.kt)(o.Z,{mdxType:"MarginText"},"3.d"),(0,n.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"On the other hand, it may be necessary to look outside that set in order to generate code \u2014 this is generally true for generic instantiation and inlining, for example. Also on the other hand, it is generally necessary to look outside that set in order to check Post-Compilation Rules.",(0,n.kt)("br",null))),(0,n.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,n.kt)(o.Z,{mdxType:"MarginText"},"3.e"),(0,n.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"See also the \u201cgeneric contract model\u201d Language Design Principle of ",(0,n.kt)("a",{href:"../AA-12/AA-12.3"},"12.3"),", \u201c",(0,n.kt)("a",{href:"../AA-12/AA-12.3"},"Generic Instantiation"),"\u201d. ",(0,n.kt)("br",null))),(0,n.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,n.kt)("h4",h({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83")),(0,n.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,n.kt)(o.Z,{mdxType:"MarginText"},"3.f/3"),(0,n.kt)(s.Z,{items:["AI05-0299-1"],mdxType:"MarginInfo"}),(0,n.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"The clause organization mentioned above is different from that of RM83. ",(0,n.kt)("br",null))))}T.isMDXComponent=!0}}]);