"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[5096],{604:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>b,contentTitle:()=>f,default:()=>T,frontMatter:()=>k,metadata:()=>h,toc:()=>y});var a=n(1716),r=n(2787),i=n(2670),o=n(6990),l=Object.defineProperty,s=Object.defineProperties,d=Object.getOwnPropertyDescriptors,p=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,m=(e,t,n)=>t in e?l(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,A=(e,t)=>{for(var n in t||(t={}))u.call(t,n)&&m(e,n,t[n]);if(p)for(var n of p(t))c.call(t,n)&&m(e,n,t[n]);return e};const k={sidebar_position:201},f="J.11 The Class Attribute of Untagged Incomplete Types",h={unversionedId:"arm/AA-J/AA-J.11",id:"arm/AA-J/AA-J.11",title:"J.11 The Class Attribute of Untagged Incomplete Types",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-J/AA-J.11.mdx",sourceDirName:"arm/AA-J",slug:"/arm/AA-J/AA-J.11",permalink:"/docs/arm/AA-J/AA-J.11",draft:!1,tags:[],version:"current",sidebarPosition:201,frontMatter:{sidebar_position:201},sidebar:"referenceManualSidebar",previous:{title:"J.10 Specific Suppression of Checks",permalink:"/docs/arm/AA-J/AA-J.10"},next:{title:"J.12 Pragma Interface",permalink:"/docs/arm/AA-J/AA-J.12"}},b={},y=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],g={toc:y};function T(e){var t,n=e,{components:l}=n,m=((e,t)=>{var n={};for(var a in e)u.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&p)for(var a of p(e))t.indexOf(a)<0&&c.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=A(A({},g),m),s(t,d({components:l,mdxType:"MDXLayout"}))),(0,a.kt)("h1",A({},{id:"j11-the-class-attribute-of-untagged-incomplete-types"}),"J.11 The Class Attribute of Untagged Incomplete Types"),(0,a.kt)("admonition",A({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",A({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)("h4",A({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(o.Z,{mdxType:"MarginText"},"1/2")),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00326-01"),"}"," ",(0,a.kt)("br",null),"For the first subtype S of a type ",(0,a.kt)("em",null,"T")," declared by an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.10#S0085"},"incomplete_type_declaration"))," that is not tagged, the following attribute is defined: ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(o.Z,{mdxType:"MarginText"},"2/2")),(0,a.kt)("dt",null,(0,a.kt)("br",null),"{",(0,a.kt)("em",null,"AI95-00326-01"),"}"," ",(0,a.kt)("br",null),"S'Class "),(0,a.kt)("dl",null,(0,a.kt)("dd",null,"Denotes the first subtype of the incomplete class-wide type rooted at ",(0,a.kt)("em",null,"T"),". The completion of ",(0,a.kt)("em",null,"T")," shall declare a tagged type. Such an attribute reference shall occur in the same library unit as the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.10#S0085"},"incomplete_type_declaration")),". ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(o.Z,{mdxType:"MarginText"},"2.a/2"))),(0,a.kt)(r.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"{",(0,a.kt)("em",null,"AI95-00326-01"),"}"," ",(0,a.kt)("br",null),"This must occur in the same unit to prevent children from imposing requirements on their ancestor library units for deferred incomplete types. ",(0,a.kt)("br",null)),(0,a.kt)("h4",A({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,a.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(o.Z,{mdxType:"MarginText"},"2.b/3")),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI95-00326-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0299-1"),"}"," ",(0,a.kt)("br",null),"This subclause is new. This feature was moved here because the tagged incomplete type provides a better way to provide this capability (it doesn't put requirements on the completion based on uses that could be anywhere). Pity we didn't think of it in 1994. ",(0,a.kt)("br",null)))}T.isMDXComponent=!0}}]);