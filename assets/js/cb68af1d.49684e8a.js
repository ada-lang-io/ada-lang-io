"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[5845],{4421:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>b,contentTitle:()=>p,default:()=>y,frontMatter:()=>k,metadata:()=>h,toc:()=>g});var n=a(1716),r=a(3050),s=a(7318),o=a(4768),i=Object.defineProperty,A=Object.defineProperties,l=Object.getOwnPropertyDescriptors,d=Object.getOwnPropertySymbols,m=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,u=(e,t,a)=>t in e?i(e,t,{enumerable:!0,configurable:!0,writable:!0,value:a}):e[t]=a,f=(e,t)=>{for(var a in t||(t={}))m.call(t,a)&&u(e,a,t[a]);if(d)for(var a of d(t))c.call(t,a)&&u(e,a,t[a]);return e};const k={sidebar_position:41},p="5 Statements",h={unversionedId:"arm/AA-5/AA-5",id:"arm/AA-5/AA-5",title:"5 Statements",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-5/AA-5.mdx",sourceDirName:"arm/AA-5",slug:"/arm/AA-5/",permalink:"/docs/arm/AA-5/",draft:!1,tags:[],version:"current",sidebarPosition:41,frontMatter:{sidebar_position:41},sidebar:"referenceManualSidebar",previous:{title:"4.10 Image Attributes",permalink:"/docs/arm/AA-4/AA-4.10"},next:{title:"5.1 Simple and Compound Statements - Sequences of Statements",permalink:"/docs/arm/AA-5/AA-5.1"}},b={},g=[{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4}],S={toc:g};function y(e){var t,a=e,{components:i}=a,u=((e,t)=>{var a={};for(var n in e)m.call(e,n)&&t.indexOf(n)<0&&(a[n]=e[n]);if(null!=e&&d)for(var n of d(e))t.indexOf(n)<0&&c.call(e,n)&&(a[n]=e[n]);return a})(a,["components"]);return(0,n.kt)("wrapper",(t=f(f({},S),u),A(t,l({components:i,mdxType:"MDXLayout"}))),(0,n.kt)("h1",f({},{id:"5-statements"}),"5 Statements"),(0,n.kt)("admonition",f({},{type:"warning"}),(0,n.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,n.kt)("a",f({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,n.kt)(s.Z,{mdxType:"MarginText"},"1"),(0,n.kt)("p",null,"[A ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-5/AA-5.1#S0167"},"statement"))," defines an action to be performed upon its execution.]",(0,n.kt)("br",null)),(0,n.kt)(s.Z,{mdxType:"MarginText"},"2/3"),(0,n.kt)(o.Z,{items:["AI95-00318-02","AI05-0299-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"[This clause describes the general rules applicable to all ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-5/AA-5.1#S0167"},"statement")),"s. Some ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-5/AA-5.1#S0167"},"statement")),"s are discussed in later clauses: ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-6/AA-6.4#S0217"},"Procedure_call_statement")),"s and return statements are described in ",(0,n.kt)("a",{href:"../AA-6/"},"6"),", \u201c",(0,n.kt)("a",{href:"../AA-6/"},"Subprograms"),"\u201d. ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.5#S0264"},"Entry_call_statement")),"s, ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.5#S0265"},"requeue_statement")),"s, ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.6#S0266"},"delay_statement")),"s, ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.5#S0258"},"accept_statement")),"s, ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.7#S0269"},"select_statement")),"s, and ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.8#S0284"},"abort_statement")),"s are described in ",(0,n.kt)("a",{href:"../AA-9/"},"9"),", \u201c",(0,n.kt)("a",{href:"../AA-9/"},"Tasks and Synchronization"),"\u201d. ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-11/AA-11.3#S0308"},"Raise_statement")),"s are described in ",(0,n.kt)("a",{href:"../AA-11/"},"11"),", \u201c",(0,n.kt)("a",{href:"../AA-11/"},"Exceptions"),"\u201d, and ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-13/AA-13.8#S0357"},"code_statement")),"s in ",(0,n.kt)("a",{href:"../AA-13/"},"13"),". The remaining forms of ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-5/AA-5.1#S0167"},"statement")),"s are presented in this clause.] ",(0,n.kt)("br",null)),(0,n.kt)("h4",f({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,n.kt)(s.Z,{mdxType:"MarginText"},"2.a/2"),(0,n.kt)(o.Z,{items:["AI95-00318-02"],mdxType:"MarginInfo"}),(0,n.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The description of return statements has been moved to ",(0,n.kt)("a",{href:"../AA-6/AA-6.5"},"6.5"),", \u201c",(0,n.kt)("a",{href:"../AA-6/AA-6.5"},"Return Statements"),"\u201d, so that it is closer to the description of subprograms. ",(0,n.kt)("br",null)))}y.isMDXComponent=!0}}]);