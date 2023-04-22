"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[4354],{42352:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>x,contentTitle:()=>f,default:()=>Z,frontMatter:()=>h,metadata:()=>y,toc:()=>T});var a=n(91716),r=n(21256),l=n(24895),i=n(28090),o=n(48424),s=n(82262),d=Object.defineProperty,A=Object.defineProperties,m=Object.getOwnPropertyDescriptors,k=Object.getOwnPropertySymbols,c=Object.prototype.hasOwnProperty,u=Object.prototype.propertyIsEnumerable,p=(e,t,n)=>t in e?d(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,g=(e,t)=>{for(var n in t||(t={}))c.call(t,n)&&p(e,n,t[n]);if(k)for(var n of k(t))u.call(t,n)&&p(e,n,t[n]);return e};const h={sidebar_position:14},f="2.6 String Literals",y={unversionedId:"arm/AA-2/AA-2.6",id:"arm/AA-2/AA-2.6",title:"2.6 String Literals",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-2/AA-2.6.mdx",sourceDirName:"arm/AA-2",slug:"/arm/AA-2/AA-2.6",permalink:"/docs/arm/AA-2/AA-2.6",draft:!1,tags:[],version:"current",sidebarPosition:14,frontMatter:{sidebar_position:14},sidebar:"referenceManualSidebar",previous:{title:"2.5 Character Literals",permalink:"/docs/arm/AA-2/AA-2.5"},next:{title:"2.7 Comments",permalink:"/docs/arm/AA-2/AA-2.7"}},x={},T=[{value:"Syntax",id:"syntax",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],b={toc:T};function Z(e){var t,n=e,{components:d}=n,p=((e,t)=>{var n={};for(var a in e)c.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&k)for(var a of k(e))t.indexOf(a)<0&&u.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=g(g({},b),p),A(t,m({components:d,mdxType:"MDXLayout"}))),(0,a.kt)("h1",g({},{id:"26-string-literals"}),"2.6 String Literals"),(0,a.kt)("admonition",g({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",g({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(o.Z,{mdxType:"MarginText"},"1"),(0,a.kt)("p",null,"[A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal"))," is formed by a sequence of graphic characters (possibly none) enclosed between two quotation marks used as string brackets. They are used to represent ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.1#S0202"},"operator_symbol")),"s (see ",(0,a.kt)("a",{href:"../AA-6/AA-6.1"},"6.1"),"), values of a string type (see ",(0,a.kt)("a",{href:"../AA-4/AA-4.2"},"4.2"),"), and array subaggregates (see ",(0,a.kt)("a",{href:"../AA-4/AA-4.3#Subclause_4.3.3"},"4.3.3"),"). ]",(0,a.kt)("br",null)),(0,a.kt)("h4",g({},{id:"syntax"}),"Syntax"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"2"),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},(0,a.kt)("code",null,"string_literal"),(0,a.kt)("a",{id:"S0016"}),(0,a.kt)("code",null," ::= "),'"',"{",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.6#S0017"},"string_element")),"}",'"',(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"3"),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},(0,a.kt)("code",null,"string_element"),(0,a.kt)("a",{id:"S0017"}),(0,a.kt)("code",null," ::= "),'"" | ',(0,a.kt)("em",null,"non_quotation_mark_"),(0,a.kt)("code",null,"graphic_character"),(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"4"),(0,a.kt)("p",null,"A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.6#S0017"},"string_element")),' is either a pair of quotation marks (""), or a single ',(0,a.kt)("code",null,"graphic_character")," other than a quotation mark. ",(0,a.kt)("br",null)),(0,a.kt)("h4",g({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"5"),(0,a.kt)("p",null,"The ",(0,a.kt)("em",null,"sequence of characters")," of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal"))," is formed from the sequence of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.6#S0017"},"string_element")),"s between the bracketing quotation marks, in the given order, with a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.6#S0017"},"string_element")),' that is "" becoming a single quotation mark in the sequence of characters, and any other ',(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.6#S0017"},"string_element"))," being reproduced in the sequence.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"6"),(0,a.kt)("p",null,"A ",(0,a.kt)("em",null,"null string literal")," is a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal"))," with no ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.6#S0017"},"string_element")),"s between the quotation marks.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(o.Z,{mdxType:"MarginText"},"7"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE 1   An end of line cannot appear in a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal")),".",(0,a.kt)("br",null))),(0,a.kt)(l.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(o.Z,{mdxType:"MarginText"},"7.1/2"),(0,a.kt)(s.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE 2   No transformation is performed on the sequence of characters of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal")),". ",(0,a.kt)("br",null))),(0,a.kt)("h4",g({},{id:"examples"}),"Examples"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"8"),(0,a.kt)("p",null,(0,a.kt)("em",null,"Examples of string literals:")," ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"9/2"),(0,a.kt)(s.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{language:"ada",mdxType:"CodeBlock"},'"Message of the day:"',"\n","\n",'""                    --  a null string literal',"\n",'" "   "A"   """"      --  three string literals of length 1',"\n","\n",'"Characters such as $, %, and ',"}",' are allowed in string literals"',"\n",'"Archimedes said ""\u0395\u03cd\u03c1\u03b7\u03ba\u03b1"""',"\n",'"Volume of cylinder (\u03c0r\xb2h) = "',"\n"),(0,a.kt)(l.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)("h4",g({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83")),(0,a.kt)(l.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(o.Z,{mdxType:"MarginText"},"9.a"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"The wording has been changed to be strictly lexical. No mention is made of string or character values, since ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal")),"s are also used to represent ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.1#S0202"},"operator_symbol")),"s, which don't have a defined value.",(0,a.kt)("br",null))),(0,a.kt)(l.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(o.Z,{mdxType:"MarginText"},"9.b"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"The syntax is described differently. ",(0,a.kt)("br",null))),(0,a.kt)(l.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)("h4",g({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95")),(0,a.kt)(l.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(o.Z,{mdxType:"MarginText"},"9.c/2"),(0,a.kt)(s.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"We explicitly say that the characters of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal"))," should be used as is. In particular, no normalization or folding should be performed on a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal")),". ",(0,a.kt)("br",null))))}Z.isMDXComponent=!0}}]);