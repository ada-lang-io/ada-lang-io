"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2568],{4363:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>f,contentTitle:()=>h,default:()=>b,frontMatter:()=>k,metadata:()=>y,toc:()=>T});var n=a(1716),i=a(3050),r=a(8604),o=a(7318),l=a(4768),s=Object.defineProperty,d=Object.defineProperties,m=Object.getOwnPropertyDescriptors,p=Object.getOwnPropertySymbols,c=Object.prototype.hasOwnProperty,A=Object.prototype.propertyIsEnumerable,u=(e,t,a)=>t in e?s(e,t,{enumerable:!0,configurable:!0,writable:!0,value:a}):e[t]=a,g=(e,t)=>{for(var a in t||(t={}))c.call(t,a)&&u(e,a,t[a]);if(p)for(var a of p(t))A.call(t,a)&&u(e,a,t[a]);return e};const k={sidebar_position:122},h="A.2 The Package Ada",y={unversionedId:"arm/AA-A/AA-A.2",id:"arm/AA-A/AA-A.2",title:"A.2 The Package Ada",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-A/AA-A.2.mdx",sourceDirName:"arm/AA-A",slug:"/arm/AA-A/AA-A.2",permalink:"/docs/arm/AA-A/AA-A.2",draft:!1,tags:[],version:"current",sidebarPosition:122,frontMatter:{sidebar_position:122},sidebar:"referenceManualSidebar",previous:{title:"A.1 The Package Standard",permalink:"/docs/arm/AA-A/AA-A.1"},next:{title:"A.3 Character Handling",permalink:"/docs/arm/AA-A/AA-A.3"}},f={},T=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4}],x={toc:T};function b(e){var t,a=e,{components:s}=a,u=((e,t)=>{var a={};for(var n in e)c.call(e,n)&&t.indexOf(n)<0&&(a[n]=e[n]);if(null!=e&&p)for(var n of p(e))t.indexOf(n)<0&&A.call(e,n)&&(a[n]=e[n]);return a})(a,["components"]);return(0,n.kt)("wrapper",(t=g(g({},x),u),d(t,m({components:s,mdxType:"MDXLayout"}))),(0,n.kt)("h1",g({},{id:"a2-the-package-ada"}),"A.2 The Package Ada"),(0,n.kt)("admonition",g({},{type:"warning"}),(0,n.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,n.kt)("a",g({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,n.kt)("h4",g({},{id:"static-semantics"}),"Static Semantics"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"1"),(0,n.kt)("p",null,"The following language-defined library package exists: ",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"2/5"),(0,n.kt)(l.Z,{items:["AI12-0414-1"],mdxType:"MarginInfo"}),(0,n.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"package Ada ","\n","   with  Pure is ","\n","end Ada;","\n"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"3/5"),(0,n.kt)(l.Z,{items:["AI12-0414-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"Ada serves as the parent of most of the other language-defined library units; its declaration is empty . ",(0,n.kt)("br",null)),(0,n.kt)("h4",g({},{id:"legality-rules"}),"Legality Rules"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"4"),(0,n.kt)("p",null,"In the standard mode, it is illegal to compile a child of package Ada. ",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"4.a"),(0,n.kt)(i.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"The intention is that mentioning, say, Ada.Text_IO in a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-10/AA-10.1#S0294"},"with_clause"))," is guaranteed (at least in the standard mode) to refer to the standard version of Ada.Text_IO. The user can compile a root library unit Text_IO that has no relation to the standard version of Text_IO. ",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"4.b"),(0,n.kt)(i.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"Note that Ada can have non-language-defined grandchildren, assuming the implementation allows it. Also, packages System and Interfaces can have children, assuming the implementation allows it. ",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"4.c"),(0,n.kt)(i.Z,{type:"aarm",aarm:"implementation-note",title:"Implementation Note: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"An implementation will typically support a nonstandard mode in which compiling the language defined library units is allowed. Whether or not this mode is made available to users is up to the implementer.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"4.d"),(0,n.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"An implementation could theoretically have private children of Ada, since that would be semantically neutral. However, a programmer cannot compile such a library unit. ",(0,n.kt)("br",null)),(0,n.kt)("h4",g({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"4.e/3"),(0,n.kt)(l.Z,{items:["AI05-0299-1"],mdxType:"MarginInfo"}),(0,n.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"This subclause is new to Ada 95. ",(0,n.kt)("br",null)))}b.isMDXComponent=!0}}]);