"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2301],{1800:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>A,contentTitle:()=>g,default:()=>x,frontMatter:()=>h,metadata:()=>k,toc:()=>f});var a=n(1716),r=n(3050),i=n(3989),s=n(7318),o=Object.defineProperty,l=Object.defineProperties,d=Object.getOwnPropertyDescriptors,m=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,p=(e,t,n)=>t in e?o(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,y=(e,t)=>{for(var n in t||(t={}))u.call(t,n)&&p(e,n,t[n]);if(m)for(var n of m(t))c.call(t,n)&&p(e,n,t[n]);return e};const h={sidebar_position:182},g="Annex H High Integrity Systems",k={unversionedId:"arm/AA-H/AA-H",id:"arm/AA-H/AA-H",title:"Annex H High Integrity Systems",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-H/AA-H.mdx",sourceDirName:"arm/AA-H",slug:"/arm/AA-H/",permalink:"/docs/arm/AA-H/",draft:!1,tags:[],version:"current",sidebarPosition:182,frontMatter:{sidebar_position:182},sidebar:"referenceManualSidebar",previous:{title:"G.3 Vector and Matrix Manipulation",permalink:"/docs/arm/AA-G/AA-G.3"},next:{title:"H.1 Pragma Normalize_Scalars",permalink:"/docs/arm/AA-H/AA-H.1"}},A={},f=[{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],b={toc:f};function x(e){var t,n=e,{components:o}=n,p=((e,t)=>{var n={};for(var a in e)u.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&m)for(var a of m(e))t.indexOf(a)<0&&c.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=y(y({},b),p),l(t,d({components:o,mdxType:"MDXLayout"}))),(0,a.kt)("h1",y({},{id:"annex-h-high-integrity-systems"}),"Annex H High Integrity Systems"),(0,a.kt)("admonition",y({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",y({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(s.Z,{mdxType:"MarginText"},"1/2")),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00347-01"),"}"," ",(0,a.kt)("br",null),"This Annex addresses requirements for high integrity systems (including safety-critical systems and security-critical systems). It provides facilities and specifies documentation requirements that relate to several needs: ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(s.Z,{mdxType:"MarginText"},"2")),(0,a.kt)("ul",null,(0,a.kt)("li",null,"Understanding program execution;",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(s.Z,{mdxType:"MarginText"},"3")),(0,a.kt)("li",null,"Reviewing object code;",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(s.Z,{mdxType:"MarginText"},"4/5")),(0,a.kt)("li",null,"{",(0,a.kt)("em",null,"AI12-0439-1"),"}"," ",(0,a.kt)("br",null),"Restricting language constructs whose usage can complicate the demonstration of program correctness ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(s.Z,{mdxType:"MarginText"},"4.1"))),(0,a.kt)("p",null,"Execution understandability is supported by pragma Normalize_Scalars, and also by requirements for the implementation to document the effect of a program in the presence of a bounded error or where the language rules leave the effect unspecified. ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(s.Z,{mdxType:"MarginText"},"5")),(0,a.kt)("p",null,"The ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.8#S0019"},"pragma")),"s Reviewable and Restrictions relate to the other requirements addressed by this Annex. ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(s.Z,{mdxType:"MarginText"},"6/5")),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE   ","{",(0,a.kt)("em",null,"AI12-0440-1"),"}"," ",(0,a.kt)("br",null),"The Valid attribute (see ",(0,a.kt)("a",{href:"../AA-13/AA-13.9#Subclause_13.9.2"},"13.9.2"),") is also useful in addressing these needs, to avoid problems that can otherwise arise from scalars that have values outside their declared range constraints.",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(s.Z,{mdxType:"MarginText"},"6.a")),(0,a.kt)(r.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),'The Annex tries to provide high assurance rather than language features. However, it is not possible, in general, to test for high assurance. For any specific language feature, it is possible to demonstrate its presence by a functional test, as in the ACVC. One can also check for the presence of some documentation requirements, but it is not easy to determine objectively that the documentation is "adequate". ',(0,a.kt)("br",null)),(0,a.kt)("h4",y({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,a.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(s.Z,{mdxType:"MarginText"},"6.b")),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"This Annex is new to Ada 95. ",(0,a.kt)("br",null)),(0,a.kt)("h4",y({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,a.kt)(i.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(s.Z,{mdxType:"MarginText"},"6.c/2")),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI95-00347-01"),"}"," ",(0,a.kt)("br",null),"The title of this annex was changed to better reflect its purpose and scope. High integrity systems has become the standard way of identifying systems that have high reliability requirements; it subsumes terms such as safety and security. Moreover, the annex does not include any security specific features and as such the previous title is somewhat misleading. ",(0,a.kt)("br",null)))}x.isMDXComponent=!0}}]);