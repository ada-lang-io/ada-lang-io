"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[5450],{2989:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>b,contentTitle:()=>y,default:()=>T,frontMatter:()=>k,metadata:()=>f,toc:()=>g});var n=r(1716),a=r(3050),o=r(3989),i=r(8604),s=r(7318),d=Object.defineProperty,l=Object.defineProperties,c=Object.getOwnPropertyDescriptors,m=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,p=Object.prototype.propertyIsEnumerable,A=(e,t,r)=>t in e?d(e,t,{enumerable:!0,configurable:!0,writable:!0,value:r}):e[t]=r,h=(e,t)=>{for(var r in t||(t={}))u.call(t,r)&&A(e,r,t[r]);if(m)for(var r of m(t))p.call(t,r)&&A(e,r,t[r]);return e};const k={sidebar_position:196},y="J.6 Numeric_Error",f={unversionedId:"arm/AA-J/AA-J.6",id:"arm/AA-J/AA-J.6",title:"J.6 Numeric_Error",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-J/AA-J.6.mdx",sourceDirName:"arm/AA-J",slug:"/arm/AA-J/AA-J.6",permalink:"/docs/arm/AA-J/AA-J.6",draft:!1,tags:[],version:"current",sidebarPosition:196,frontMatter:{sidebar_position:196},sidebar:"referenceManualSidebar",previous:{title:"J.5 ASCII",permalink:"/docs/arm/AA-J/AA-J.5"},next:{title:"J.7 At Clauses",permalink:"/docs/arm/AA-J/AA-J.7"}},b={},g=[{value:"Static Semantics",id:"static-semantics",level:4}],x={toc:g};function T(e){var t,r=e,{components:d}=r,A=((e,t)=>{var r={};for(var n in e)u.call(e,n)&&t.indexOf(n)<0&&(r[n]=e[n]);if(null!=e&&m)for(var n of m(e))t.indexOf(n)<0&&p.call(e,n)&&(r[n]=e[n]);return r})(r,["components"]);return(0,n.kt)("wrapper",(t=h(h({},x),A),l(t,c({components:d,mdxType:"MDXLayout"}))),(0,n.kt)("h1",h({},{id:"j6-numeric_error"}),"J.6 Numeric_Error"),(0,n.kt)("admonition",h({},{type:"warning"}),(0,n.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,n.kt)("a",h({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,n.kt)("h4",h({},{id:"static-semantics"}),"Static Semantics"),(0,n.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,n.kt)(s.Z,{mdxType:"MarginText"},"1")),(0,n.kt)("p",null,"The following declaration exists in the declaration of package Standard: ",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,n.kt)(s.Z,{mdxType:"MarginText"},"2")),(0,n.kt)(i.Z,{language:"ada",mdxType:"CodeBlock"},"Numeric_Error : exception renames Constraint_Error;","\n",(0,n.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,n.kt)(s.Z,{mdxType:"MarginText"},"2.a"))),(0,n.kt)(a.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"This is true even though it is not shown in ",(0,n.kt)("a",{href:"../AA-A/AA-A.1"},"A.1"),". ",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,n.kt)(s.Z,{mdxType:"MarginText"},"2.b")),(0,n.kt)(a.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"In Ada 83, it was unclear which situations should raise Numeric_Error, and which should raise Constraint_Error. The permissions of RM83-11.6 could often be used to allow the implementation to raise Constraint_Error in a situation where one would normally expect Numeric_Error. To avoid this confusion, all situations that raise Numeric_Error in Ada 83 are changed to raise Constraint_Error in Ada 95. Numeric_Error is changed to be a renaming of Constraint_Error to avoid most of the upward compatibilities associated with this change.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,n.kt)(s.Z,{mdxType:"MarginText"},"2.c")),(0,n.kt)(a.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"In new code, Constraint_Error should be used instead of Numeric_Error. ",(0,n.kt)("br",null)))}T.isMDXComponent=!0}}]);