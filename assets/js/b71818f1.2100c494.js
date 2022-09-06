"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8543],{1443:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>f,contentTitle:()=>h,default:()=>b,frontMatter:()=>p,metadata:()=>m,toc:()=>k});var i=n(1716),r=Object.defineProperty,a=Object.defineProperties,o=Object.getOwnPropertyDescriptors,s=Object.getOwnPropertySymbols,d=Object.prototype.hasOwnProperty,l=Object.prototype.propertyIsEnumerable,c=(e,t,n)=>t in e?r(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,u=(e,t)=>{for(var n in t||(t={}))d.call(t,n)&&c(e,n,t[n]);if(s)for(var n of s(t))l.call(t,n)&&c(e,n,t[n]);return e};const p={sidebar_position:5},h="1.3  Terms and Definitions",m={unversionedId:"arm/AA-1.3",id:"arm/AA-1.3",title:"1.3  Terms and Definitions",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-1.3.mdx",sourceDirName:"arm",slug:"/arm/AA-1.3",permalink:"/docs/arm/AA-1.3",draft:!1,tags:[],version:"current",sidebarPosition:5,frontMatter:{sidebar_position:5},sidebar:"tutorialSidebar",previous:{title:"1.2  Normative References",permalink:"/docs/arm/AA-1.2"},next:{title:"2 Lexical Elements",permalink:"/docs/arm/AA-2"}},f={},k=[{value:"1.3.1  Types, Objects, and their Properties",id:"131--types-objects-and-their-properties",level:2},{value:"1.3.2  Subprograms and their Properties",id:"132--subprograms-and-their-properties",level:2},{value:"1.3.3  Other Syntactic Constructs",id:"133--other-syntactic-constructs",level:2},{value:"1.3.4  Runtime Actions",id:"134--runtime-actions",level:2},{value:"1.3.5  Exceptional Situations",id:"135--exceptional-situations",level:2}],A={toc:k};function b(e){var t,n=e,{components:r}=n,c=((e,t)=>{var n={};for(var i in e)d.call(e,i)&&t.indexOf(i)<0&&(n[i]=e[i]);if(null!=e&&s)for(var i of s(e))t.indexOf(i)<0&&l.call(e,i)&&(n[i]=e[i]);return n})(n,["components"]);return(0,i.kt)("wrapper",(t=u(u({},A),c),a(t,o({components:r,mdxType:"MDXLayout"}))),(0,i.kt)("h1",u({},{id:"13--terms-and-definitions"}),"1.3  Terms and Definitions"),(0,i.kt)("admonition",u({},{type:"warning"}),(0,i.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,i.kt)("a",u({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00415-01"),"}"," ","{",(0,i.kt)("em",null,"AI12-0443-1"),"}"," Terms are defined throughout this document, indicated by ",(0,i.kt)("em",null,"italic")," type. Terms explicitly defined in this document are not to be presumed to refer implicitly to similar terms defined elsewhere. Mathematical terms not defined in this document are to be interpreted according to the ",(0,i.kt)("em",null,"CRC Concise Encyclopedia of Mathematics, Second Edition"),". Other terms not defined in this document are to be interpreted according to the ",(0,i.kt)("em",null,"Webster's Third New International Dictionary of the English Language"),". Informal descriptions of some terms are also given below. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"The index contains an entry for every defined term."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00415-01"),"}"," The contents of the ",(0,i.kt)("em",null,"CRC Concise Encyclopedia of Mathematics, Second Edition")," can be accessed on ",(0,i.kt)("a",{href:"http://www.mathworld.com"},"http://www.mathworld.com"),". The ISBN number of the book is ISBN 1584883472."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0443-1"),"}"," The definitions found in this subclause are short, descriptive definitions. These are most useful for comprehension when an exact meaning is not needed. More formal, detailed definitions are found in the body of the Reference Manual; these are needed to determine the exact application of rules to constructs and entities."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0443-1"),"}",' To see the difference, consider "aliased view". The descriptive definition is "a view of an object that can be designated by an access value". The formal definition is a lengthy paragraph in 3.10 that defines 10 separate constructs as aliased views. In order to reason about a language rule that requires an aliased view in a particular context, one needs to look at the list of constructs in order to determine if the object in hand is actually an aliased view. The descriptive definition is no help in this case. '),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Glossary entry: "),"Each term defined in Annex N is marked like this. "),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0443-1"),"}"," Each term defined in this subclause is marked like this in the body of the AARM near its detailed formal definition. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),'Here are some AARM-only definitions: The Ada Rapporteur Group (ARG) interprets the Ada Reference Manual. An Ada Issue (AI) is a numbered ruling from the ARG. Ada Issues created for Ada 83 are denoted as "AI83", while Ada Issues created for Ada 95 are denoted as "AI95" in this document. Similarly, Ada Issues created for Ada 2005 are denoted as "AI05" The Ada Commentary Integration Document (ACID) is an edition of the Ada 83 RM in which clearly marked insertions and deletions indicate the effect of integrating the approved AIs. The Uniformity Rapporteur Group (URG) issued recommendations intended to increase uniformity across Ada implementations. The functions of the URG have been assumed by the ARG. A Uniformity Issue (UI) was a numbered recommendation from the URG. A Defect Report and Response is an official query to WG9 about an error in the standard. Defect Reports are processed by the ARG, and are referenced here by their ISO numbers: 8652/nnnn. Most changes to the Ada 95 standard include reference(s) to the Defect Report(s) that prompted the change. The ',(0,i.kt)("em",null,"Ada Conformity Assessment Test Suite (ACATS)")," is a set of tests intended to check the conformity of Ada implementations to this standard. This set of tests was previously known as the Ada Compiler Validation Capability (ACVC). "),(0,i.kt)("h2",u({},{id:"131--types-objects-and-their-properties"}),"1.3.1  Types, Objects, and their Properties"),(0,i.kt)("p",null,"Version=[5],Group=[T]"),(0,i.kt)("h2",u({},{id:"132--subprograms-and-their-properties"}),"1.3.2  Subprograms and their Properties"),(0,i.kt)("p",null,"Version=[5],Group=[S]"),(0,i.kt)("h2",u({},{id:"133--other-syntactic-constructs"}),"1.3.3  Other Syntactic Constructs"),(0,i.kt)("p",null,"Version=[5],Group=[C]"),(0,i.kt)("h2",u({},{id:"134--runtime-actions"}),"1.3.4  Runtime Actions"),(0,i.kt)("p",null,"Version=[5],Group=[R]"),(0,i.kt)("h2",u({},{id:"135--exceptional-situations"}),"1.3.5  Exceptional Situations"),(0,i.kt)("p",null,"Version=[5],Group=[E]"))}b.isMDXComponent=!0}}]);