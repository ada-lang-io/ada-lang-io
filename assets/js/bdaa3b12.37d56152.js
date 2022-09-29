"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[5096],{604:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>b,contentTitle:()=>A,default:()=>T,frontMatter:()=>h,metadata:()=>g,toc:()=>y});var n=a(1716),r=a(3050),i=a(7318),o=a(4768),s=Object.defineProperty,l=Object.defineProperties,d=Object.getOwnPropertyDescriptors,p=Object.getOwnPropertySymbols,c=Object.prototype.hasOwnProperty,m=Object.prototype.propertyIsEnumerable,u=(e,t,a)=>t in e?s(e,t,{enumerable:!0,configurable:!0,writable:!0,value:a}):e[t]=a,f=(e,t)=>{for(var a in t||(t={}))c.call(t,a)&&u(e,a,t[a]);if(p)for(var a of p(t))m.call(t,a)&&u(e,a,t[a]);return e};const h={sidebar_position:204},A="J.11 The Class Attribute of Untagged Incomplete Types",g={unversionedId:"arm/AA-J/AA-J.11",id:"arm/AA-J/AA-J.11",title:"J.11 The Class Attribute of Untagged Incomplete Types",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-J/AA-J.11.mdx",sourceDirName:"arm/AA-J",slug:"/arm/AA-J/AA-J.11",permalink:"/docs/arm/AA-J/AA-J.11",draft:!1,tags:[],version:"current",sidebarPosition:204,frontMatter:{sidebar_position:204},sidebar:"referenceManualSidebar",previous:{title:"J.10 Specific Suppression of Checks",permalink:"/docs/arm/AA-J/AA-J.10"},next:{title:"J.12 Pragma Interface",permalink:"/docs/arm/AA-J/AA-J.12"}},b={},y=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],k={toc:y};function T(e){var t,a=e,{components:s}=a,u=((e,t)=>{var a={};for(var n in e)c.call(e,n)&&t.indexOf(n)<0&&(a[n]=e[n]);if(null!=e&&p)for(var n of p(e))t.indexOf(n)<0&&m.call(e,n)&&(a[n]=e[n]);return a})(a,["components"]);return(0,n.kt)("wrapper",(t=f(f({},k),u),l(t,d({components:s,mdxType:"MDXLayout"}))),(0,n.kt)("h1",f({},{id:"j11-the-class-attribute-of-untagged-incomplete-types"}),"J.11 The Class Attribute of Untagged Incomplete Types"),(0,n.kt)("admonition",f({},{type:"warning"}),(0,n.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,n.kt)("a",f({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,n.kt)("h4",f({},{id:"static-semantics"}),"Static Semantics"),(0,n.kt)(i.Z,{mdxType:"MarginText"},"1/2"),(0,n.kt)(o.Z,{items:["AI95-00326-01"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"For the first subtype S of a type ",(0,n.kt)("em",null,"T")," declared by an ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-3/AA-3.10#S0085"},"incomplete_type_declaration"))," that is not tagged, the following attribute is defined: ",(0,n.kt)("br",null)),(0,n.kt)(i.Z,{mdxType:"MarginText"},"2/2"),(0,n.kt)(o.Z,{items:["AI95-00326-01"],mdxType:"MarginInfo"}),(0,n.kt)("dt",null,(0,n.kt)("br",null),"S'Class "),(0,n.kt)("dl",null,(0,n.kt)("dd",null,"Denotes the first subtype of the incomplete class-wide type rooted at ",(0,n.kt)("em",null,"T"),". The completion of ",(0,n.kt)("em",null,"T")," shall declare a tagged type. Such an attribute reference shall occur in the same library unit as the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-3/AA-3.10#S0085"},"incomplete_type_declaration")),". ",(0,n.kt)("br",null))),(0,n.kt)(i.Z,{mdxType:"MarginText"},"2.a/2"),(0,n.kt)(o.Z,{items:["AI95-00326-01"],mdxType:"MarginInfo"}),(0,n.kt)(r.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"This must occur in the same unit to prevent children from imposing requirements on their ancestor library units for deferred incomplete types. ",(0,n.kt)("br",null)),(0,n.kt)("h4",f({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,n.kt)(i.Z,{mdxType:"MarginText"},"2.b/3"),(0,n.kt)(o.Z,{items:["AI95-00326-01","AI05-0299-1"],mdxType:"MarginInfo"}),(0,n.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"This subclause is new. This feature was moved here because the tagged incomplete type provides a better way to provide this capability (it doesn't put requirements on the completion based on uses that could be anywhere). Pity we didn't think of it in 1994. ",(0,n.kt)("br",null)))}T.isMDXComponent=!0}}]);