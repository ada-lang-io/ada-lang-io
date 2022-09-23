"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[6481],{982:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>y,contentTitle:()=>f,default:()=>T,frontMatter:()=>h,metadata:()=>x,toc:()=>S});var a=n(1716),l=n(2787),o=n(2670),i=n(8604),r=n(6990),A=Object.defineProperty,s=Object.defineProperties,d=Object.getOwnPropertyDescriptors,k=Object.getOwnPropertySymbols,m=Object.prototype.hasOwnProperty,u=Object.prototype.propertyIsEnumerable,c=(e,t,n)=>t in e?A(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,p=(e,t)=>{for(var n in t||(t={}))m.call(t,n)&&c(e,n,t[n]);if(k)for(var n of k(t))u.call(t,n)&&c(e,n,t[n]);return e};const h={sidebar_position:46},f="5.7 Exit Statements",x={unversionedId:"arm/AA-5/AA-5.7",id:"arm/AA-5/AA-5.7",title:"5.7 Exit Statements",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-5/AA-5.7.mdx",sourceDirName:"arm/AA-5",slug:"/arm/AA-5/AA-5.7",permalink:"/docs/arm/AA-5/AA-5.7",draft:!1,tags:[],version:"current",sidebarPosition:46,frontMatter:{sidebar_position:46},sidebar:"referenceManualSidebar",previous:{title:"5.6 Block Statements",permalink:"/docs/arm/AA-5/AA-5.6"},next:{title:"5.8 Goto Statements",permalink:"/docs/arm/AA-5/AA-5.8"}},y={},S=[{value:"Syntax",id:"syntax",level:4},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Examples",id:"examples",level:4}],_={toc:S};function T(e){var t,n=e,{components:A}=n,c=((e,t)=>{var n={};for(var a in e)m.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&k)for(var a of k(e))t.indexOf(a)<0&&u.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=p(p({},_),c),s(t,d({components:A,mdxType:"MDXLayout"}))),(0,a.kt)("h1",p({},{id:"57-exit-statements"}),"5.7 Exit Statements"),(0,a.kt)("admonition",p({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",p({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(r.Z,{mdxType:"MarginText"},"1")),(0,a.kt)("p",null,"[An ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.7#S0193"},"exit_statement"))," is used to complete the execution of an enclosing ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.5#S0178"},"loop_statement")),"; the completion is conditional if the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.7#S0193"},"exit_statement"))," includes a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.5#S0150"},"condition")),".] ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"syntax"}),"Syntax"),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(r.Z,{mdxType:"MarginText"},"2")),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},(0,a.kt)("code",null,"exit_statement"),(0,a.kt)("a",{id:"S0193"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("br",null),"   ",(0,a.kt)("strong",null,"exit")," [",(0,a.kt)("em",null,"loop_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),"] [",(0,a.kt)("strong",null,"when")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.5#S0150"},"condition")),"];",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"name-resolution-rules"}),"Name Resolution Rules"),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(r.Z,{mdxType:"MarginText"},"3")),(0,a.kt)("p",null,"The ",(0,a.kt)("em",null,"loop_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),", if any, in an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.7#S0193"},"exit_statement"))," shall resolve to denote a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.5#S0178"},"loop_statement")),". ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"legality-rules"}),"Legality Rules"),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(r.Z,{mdxType:"MarginText"},"4")),(0,a.kt)("p",null,"Each ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.7#S0193"},"exit_statement"))," ",(0,a.kt)("em",null,"applies to")," a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.5#S0178"},"loop_statement")),"; this is the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.5#S0178"},"loop_statement"))," being exited. An ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.7#S0193"},"exit_statement"))," with a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," is only allowed within the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.5#S0178"},"loop_statement"))," denoted by the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),", and applies to that ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.5#S0178"},"loop_statement")),". An ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.7#S0193"},"exit_statement"))," without a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," is only allowed within a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.5#S0178"},"loop_statement")),", and applies to the innermost enclosing one. An ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.7#S0193"},"exit_statement"))," that applies to a given ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.5#S0178"},"loop_statement"))," shall not appear within a body or ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.5#S0258"},"accept_statement")),", if this construct is itself enclosed by the given ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.5#S0178"},"loop_statement")),". ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(r.Z,{mdxType:"MarginText"},"5")),(0,a.kt)("p",null,"For the execution of an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.7#S0193"},"exit_statement")),", the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.5#S0150"},"condition")),", if present, is first evaluated. If the value of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.5#S0150"},"condition"))," is True, or if there is no ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.5#S0150"},"condition")),", a transfer of control is done to complete the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.5#S0178"},"loop_statement")),". If the value of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.5#S0150"},"condition"))," is False, no transfer of control takes place. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(r.Z,{mdxType:"MarginText"},"6")),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 1   Several nested loops can be exited by an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.7#S0193"},"exit_statement"))," that names the outer loop. ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"examples"}),"Examples"),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(r.Z,{mdxType:"MarginText"},"7")),(0,a.kt)("p",null,(0,a.kt)("em",null,"Examples of loops with exit statements:")," ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(r.Z,{mdxType:"MarginText"},"8")),(0,a.kt)(i.Z,{language:"ada",mdxType:"CodeBlock"},"for N in 1 .. Max_Num_Items loop","\n","   Get_New_Item(New_Item);","\n","   Merge_Item(New_Item, Storage_File);","\n","   exit when New_Item = Terminal_Item;","\n","end loop;","\n",(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(r.Z,{mdxType:"MarginText"},"9")),"Main_Cycle:","\n","   loop","\n","      --  initial statements","\n","      exit Main_Cycle when Found;","\n","      --  final statements","\n","   end loop Main_Cycle;","\n"))}T.isMDXComponent=!0}}]);