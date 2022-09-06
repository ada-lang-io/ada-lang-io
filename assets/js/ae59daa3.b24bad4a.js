"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[7383],{9415:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>f,contentTitle:()=>m,default:()=>b,frontMatter:()=>d,metadata:()=>h,toc:()=>g});var a=n(1716),r=Object.defineProperty,i=Object.defineProperties,o=Object.getOwnPropertyDescriptors,s=Object.getOwnPropertySymbols,l=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,u=(e,t,n)=>t in e?r(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,p=(e,t)=>{for(var n in t||(t={}))l.call(t,n)&&u(e,n,t[n]);if(s)for(var n of s(t))c.call(t,n)&&u(e,n,t[n]);return e};const d={sidebar_position:201},m="J.11  The Class Attribute of Untagged Incomplete Types",h={unversionedId:"arm/AA-J.11",id:"arm/AA-J.11",title:"J.11  The Class Attribute of Untagged Incomplete Types",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-J.11.mdx",sourceDirName:"arm",slug:"/arm/AA-J.11",permalink:"/docs/arm/AA-J.11",draft:!1,tags:[],version:"current",sidebarPosition:201,frontMatter:{sidebar_position:201},sidebar:"tutorialSidebar",previous:{title:"J.10  Specific Suppression of Checks",permalink:"/docs/arm/AA-J.10"},next:{title:"J.12  Pragma Interface",permalink:"/docs/arm/AA-J.12"}},f={},g=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],k={toc:g};function b(e){var t,n=e,{components:r}=n,u=((e,t)=>{var n={};for(var a in e)l.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&s)for(var a of s(e))t.indexOf(a)<0&&c.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=p(p({},k),u),i(t,o({components:r,mdxType:"MDXLayout"}))),(0,a.kt)("h1",p({},{id:"j11--the-class-attribute-of-untagged-incomplete-types"}),"J.11  The Class Attribute of Untagged Incomplete Types"),(0,a.kt)("admonition",p({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,a.kt)("a",p({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)("h4",p({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00326-01"),"}"," For the first subtype S of a type ",(0,a.kt)("em",null,"T")," declared by an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.10#S0085"},"incomplete_type_declaration"))," that is not tagged, the following attribute is defined: "),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00326-01"),"}"," S'Class Denotes the first subtype of the incomplete class-wide type rooted at ",(0,a.kt)("em",null,"T"),". The completion of ",(0,a.kt)("em",null,"T")," shall declare a tagged type. Such an attribute reference shall occur in the same library unit as the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.10#S0085"},"incomplete_type_declaration")),". "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Reason: "),"{",(0,a.kt)("em",null,"AI95-00326-01"),"}"," This must occur in the same unit to prevent children from imposing requirements on their ancestor library units for deferred incomplete types. "),(0,a.kt)("h4",p({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00326-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0299-1"),"}"," This subclause is new. This feature was moved here because the tagged incomplete type provides a better way to provide this capability (it doesn't put requirements on the completion based on uses that could be anywhere). Pity we didn't think of it in 1994. "))}b.isMDXComponent=!0}}]);