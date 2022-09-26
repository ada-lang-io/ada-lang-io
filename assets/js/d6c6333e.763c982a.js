"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[7964],{4970:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>A,contentTitle:()=>p,default:()=>g,frontMatter:()=>h,metadata:()=>b,toc:()=>y});var a=n(1716),r=n(3050),i=n(7318),o=n(4768),l=Object.defineProperty,s=Object.defineProperties,d=Object.getOwnPropertyDescriptors,u=Object.getOwnPropertySymbols,c=Object.prototype.hasOwnProperty,m=Object.prototype.propertyIsEnumerable,f=(e,t,n)=>t in e?l(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,k=(e,t)=>{for(var n in t||(t={}))c.call(t,n)&&f(e,n,t[n]);if(u)for(var n of u(t))m.call(t,n)&&f(e,n,t[n]);return e};const h={sidebar_position:80},p="9.9 Task and Entry Attributes",b={unversionedId:"arm/AA-9/AA-9.9",id:"arm/AA-9/AA-9.9",title:"9.9 Task and Entry Attributes",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-9/AA-9.9.mdx",sourceDirName:"arm/AA-9",slug:"/arm/AA-9/AA-9.9",permalink:"/docs/arm/AA-9/AA-9.9",draft:!1,tags:[],version:"current",sidebarPosition:80,frontMatter:{sidebar_position:80},sidebar:"referenceManualSidebar",previous:{title:"9.8 Abort of a Task - Abort of a Sequence of Statements",permalink:"/docs/arm/AA-9/AA-9.8"},next:{title:"9.10 Shared Variables",permalink:"/docs/arm/AA-9/AA-9.10"}},A={},y=[{value:"Dynamic Semantics",id:"dynamic-semantics",level:4}],T={toc:y};function g(e){var t,n=e,{components:l}=n,f=((e,t)=>{var n={};for(var a in e)c.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&u)for(var a of u(e))t.indexOf(a)<0&&m.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=k(k({},T),f),s(t,d({components:l,mdxType:"MDXLayout"}))),(0,a.kt)("h1",k({},{id:"99-task-and-entry-attributes"}),"9.9 Task and Entry Attributes"),(0,a.kt)("admonition",k({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",k({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)("h4",k({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,a.kt)(i.Z,{mdxType:"MarginText"},"1"),(0,a.kt)("p",null,"For a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0093"},"prefix"))," T that is of a task type [(after any implicit dereference)], the following attributes are defined: ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"2"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"T'Callable"),(0,a.kt)("dl",null,(0,a.kt)("dd",null,"Yields the value True when the task denoted by T is ",(0,a.kt)("em",null,"callable"),", and False otherwise; a task is callable unless it is completed or abnormal. The value of this attribute is of the predefined type Boolean.",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"3"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"T'Terminated"),(0,a.kt)("dd",null,"Yields the value True if the task denoted by T is terminated, and False otherwise. The value of this attribute is of the predefined type Boolean. ",(0,a.kt)("br",null))),(0,a.kt)(i.Z,{mdxType:"MarginText"},"4"),(0,a.kt)("p",null,"For a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0093"},"prefix"))," E that denotes an entry of a task or protected unit, the following attribute is defined. This attribute is only allowed within the body of the task or protected unit, but excluding, in the case of an entry of a task unit, within any program unit that is, itself, inner to the body of the task unit. ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"5"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"E'Count"),(0,a.kt)("dl",null,(0,a.kt)("dd",null,"Yields the number of calls presently queued on the entry E of the current instance of the unit. The value of this attribute is of the type ",(0,a.kt)("em",null,"universal_integer"),".",(0,a.kt)("br",null))),(0,a.kt)(i.Z,{mdxType:"MarginText"},"6"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 1   For the Count attribute, the entry can be either a single entry or an entry of a family. The name of the entry or entry family can be either a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0092"},"direct_name"))," or an expanded name.",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"7/5"),(0,a.kt)(o.Z,{items:["AI12-0442-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 2   ","{",(0,a.kt)("em",null,"AI12-0442-1"),"}"," ",(0,a.kt)("br",null),"Within task units, by  interrogating the attribute E'Count an algorithm can  allow for the increase of the value of this attribute for incoming entry calls, and its decrease, for example with ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.7#S0276"},"timed_entry_call")),"s. A  ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.7#S0279"},"conditional_entry_call"))," can also  briefly increase this value, even if the conditional call is not accepted.",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"8/5"),(0,a.kt)(o.Z,{items:["AI12-0442-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 3   ","{",(0,a.kt)("em",null,"AI12-0442-1"),"}"," ",(0,a.kt)("br",null),"Within protected units, by  interrogating the attribute E'Count in the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.5#S0262"},"entry_barrier"))," for the entry E an algorithm can  allow for the evaluation of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.5#S0150"},"condition"))," of the barrier both before and after queuing a given caller. ",(0,a.kt)("br",null)))}g.isMDXComponent=!0}}]);