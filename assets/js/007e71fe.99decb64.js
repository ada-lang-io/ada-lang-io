"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[1237],{4789:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>k,contentTitle:()=>h,default:()=>A,frontMatter:()=>m,metadata:()=>f,toc:()=>b});var i=a(1716),n=a(7556),r=Object.defineProperty,o=Object.defineProperties,l=Object.getOwnPropertyDescriptors,s=Object.getOwnPropertySymbols,c=Object.prototype.hasOwnProperty,d=Object.prototype.propertyIsEnumerable,p=(e,t,a)=>t in e?r(e,t,{enumerable:!0,configurable:!0,writable:!0,value:a}):e[t]=a,u=(e,t)=>{for(var a in t||(t={}))c.call(t,a)&&p(e,a,t[a]);if(s)for(var a of s(t))d.call(t,a)&&p(e,a,t[a]);return e};const m={sidebar_position:175},h="F.1  Machine_Radix Attribute Definition Clause",f={unversionedId:"arm/AA-F.1",id:"arm/AA-F.1",title:"F.1  Machine_Radix Attribute Definition Clause",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-F.1.mdx",sourceDirName:"arm",slug:"/arm/AA-F.1",permalink:"/docs/arm/AA-F.1",draft:!1,tags:[],version:"current",sidebarPosition:175,frontMatter:{sidebar_position:175},sidebar:"tutorialSidebar",previous:{title:"Annex F Information Systems",permalink:"/docs/arm/AA-F"},next:{title:"F.2  The Package Decimal",permalink:"/docs/arm/AA-F.2"}},k={},b=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Advice",id:"implementation-advice",level:4},{value:"Examples",id:"examples",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],g={toc:b};function A(e){var t,a=e,{components:r}=a,p=((e,t)=>{var a={};for(var i in e)c.call(e,i)&&t.indexOf(i)<0&&(a[i]=e[i]);if(null!=e&&s)for(var i of s(e))t.indexOf(i)<0&&d.call(e,i)&&(a[i]=e[i]);return a})(a,["components"]);return(0,i.kt)("wrapper",(t=u(u({},g),p),o(t,l({components:r,mdxType:"MDXLayout"}))),(0,i.kt)("h1",u({},{id:"f1--machine_radix-attribute-definition-clause"}),"F.1  Machine_Radix Attribute Definition Clause"),(0,i.kt)("admonition",u({},{type:"warning"}),(0,i.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,i.kt)("a",u({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,i.kt)("h4",u({},{id:"static-semantics"}),"Static Semantics"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0272-1"),"}"," The representation attribute Machine_Radix may be specified for a decimal first subtype (see 3.5.9) via an ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-13.3#S0349"},"attribute_definition_clause")),"; the expression of such a clause shall be static, and its value shall be 2 or 10. A value of 2 implies a binary base range; a value of 10 implies a decimal base range. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Ramification: "),"In the absence of a Machine_Radix clause, the choice of 2 versus 10 for S'Machine_Radix is not specified. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Aspect Description for "),(0,i.kt)("strong",null,"Machine_Radix: "),"Radix (2 or 10) that is used to represent a decimal fixed point type."),(0,i.kt)("h4",u({},{id:"implementation-advice"}),"Implementation Advice"),(0,i.kt)("p",null,"Packed decimal should be used as the internal representation for objects of subtype S when S'Machine_Radix = 10. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Implementation Advice: "),"Packed decimal should be used as the internal representation for objects of subtype ",(0,i.kt)("em",null,"S")," when ",(0,i.kt)("em",null,"S"),"'Machine_Radix = 10."),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"{",(0,i.kt)("em",null,"AI05-0229-1"),"}"," The intent of a decimal Machine_Radix attribute definition clause is to allow the programmer to declare an Ada decimal data object whose representation matches a particular COBOL implementation's representation of packed decimal items. The Ada object may then be passed to an interfaced COBOL program that takes a packed decimal data item as a parameter, assuming that convention COBOL has been specified for the Ada object's type with an aspect Convention."),(0,i.kt)("p",null,"Additionally, the Ada compiler may choose to generate arithmetic instructions that exploit the packed decimal representation."),(0,i.kt)("h4",u({},{id:"examples"}),"Examples"),(0,i.kt)("p",null,(0,i.kt)("em",null,"Example of Machine_Radix attribute definition clause:")," "),(0,i.kt)(n.Z,{mdxType:"CodeBlock"},"type Money is delta 0.01 digits 15;","\n","for Money'Machine_Radix use 10;","\n"),(0,i.kt)("h4",u({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0272-1"),"}"," Clarified that Machine_Radix is a representation aspect. "))}A.isMDXComponent=!0}}]);