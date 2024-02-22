"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2504],{17960:(e,n,a)=>{a.r(n),a.d(n,{assets:()=>m,contentTitle:()=>A,default:()=>p,frontMatter:()=>y,metadata:()=>d,toc:()=>c});var r=a(58168),t=(a(96540),a(15680)),i=a(20793),l=a(91435),o=a(21432),g=a(79162),s=a(34421);const y={sidebar_position:14},A="2.6 String Literals",d={unversionedId:"arm/AA-2/AA-2.6",id:"arm/AA-2/AA-2.6",title:"2.6 String Literals",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-2/AA-2.6.mdx",sourceDirName:"arm/AA-2",slug:"/arm/AA-2/AA-2.6",permalink:"/docs/arm/AA-2/AA-2.6",draft:!1,tags:[],version:"current",sidebarPosition:14,frontMatter:{sidebar_position:14},sidebar:"referenceManualSidebar",previous:{title:"2.5 Character Literals",permalink:"/docs/arm/AA-2/AA-2.5"},next:{title:"2.7 Comments",permalink:"/docs/arm/AA-2/AA-2.7"}},m={},c=[{value:"Syntax",id:"syntax",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],u={toc:c},h="wrapper";function p(e){let{components:n,...a}=e;return(0,t.yg)(h,(0,r.A)({},u,a,{components:n,mdxType:"MDXLayout"}),(0,t.yg)("h1",{id:"26-string-literals"},"2.6 String Literals"),(0,t.yg)("admonition",{type:"warning"},(0,t.yg)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,t.yg)("a",{parentName:"p",href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"},"tracking issue"))),(0,t.yg)(g.A,{mdxType:"MarginText"},"1"),(0,t.yg)("p",null,"[A ",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal"))," is formed by a sequence of graphic characters (possibly none) enclosed between two quotation marks used as string brackets. They are used to represent ",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-6/AA-6.1#S0202"},"operator_symbol")),"s (see ",(0,t.yg)("a",{href:"../AA-6/AA-6.1"},"6.1"),"), values of a string type (see ",(0,t.yg)("a",{href:"../AA-4/AA-4.2"},"4.2"),"), and array subaggregates (see ",(0,t.yg)("a",{href:"../AA-4/AA-4.3#Subclause_4.3.3"},"4.3.3"),"). ]",(0,t.yg)("br",null)),(0,t.yg)("h4",{id:"syntax"},"Syntax"),(0,t.yg)(g.A,{mdxType:"MarginText"},"2"),(0,t.yg)(o.A,{mdxType:"CodeBlock"},(0,t.yg)("code",null,"string_literal"),(0,t.yg)("a",{id:"S0016"}),(0,t.yg)("code",null," ::= "),'"',"{",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-2/AA-2.6#S0017"},"string_element")),"}",'"',(0,t.yg)("br",null)),(0,t.yg)(g.A,{mdxType:"MarginText"},"3"),(0,t.yg)(o.A,{mdxType:"CodeBlock"},(0,t.yg)("code",null,"string_element"),(0,t.yg)("a",{id:"S0017"}),(0,t.yg)("code",null," ::= "),'"" | ',(0,t.yg)("em",null,"non_quotation_mark_"),(0,t.yg)("code",null,"graphic_character"),(0,t.yg)("br",null)),(0,t.yg)(g.A,{mdxType:"MarginText"},"4"),(0,t.yg)("p",{class:"Indented2"},"A ",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-2/AA-2.6#S0017"},"string_element")),' is either a pair of quotation marks (""), or a single ',(0,t.yg)("code",null,"graphic_character")," other than a quotation mark. ",(0,t.yg)("br",null)),(0,t.yg)("h4",{id:"static-semantics"},"Static Semantics"),(0,t.yg)(g.A,{mdxType:"MarginText"},"5"),(0,t.yg)("p",null,"The ",(0,t.yg)("em",null,"sequence of characters")," of a ",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal"))," is formed from the sequence of ",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-2/AA-2.6#S0017"},"string_element")),"s between the bracketing quotation marks, in the given order, with a ",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-2/AA-2.6#S0017"},"string_element")),' that is "" becoming a single quotation mark in the sequence of characters, and any other ',(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-2/AA-2.6#S0017"},"string_element"))," being reproduced in the sequence.",(0,t.yg)("br",null)),(0,t.yg)(g.A,{mdxType:"MarginText"},"6"),(0,t.yg)("p",null,"A ",(0,t.yg)("em",null,"null string literal")," is a ",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal"))," with no ",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-2/AA-2.6#S0017"},"string_element")),"s between the quotation marks.",(0,t.yg)("br",null)),(0,t.yg)(l.A,{mdxType:"AnnotatedOnly"},(0,t.yg)(g.A,{mdxType:"MarginText"},"7"),(0,t.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE 1   An end of line cannot appear in a ",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal")),".",(0,t.yg)("br",null))),(0,t.yg)(l.A,{mdxType:"AnnotatedOnly"},(0,t.yg)(g.A,{mdxType:"MarginText"},"7.1/2"),(0,t.yg)(s.A,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,t.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE 2   No transformation is performed on the sequence of characters of a ",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal")),". ",(0,t.yg)("br",null))),(0,t.yg)("h4",{id:"examples"},"Examples"),(0,t.yg)(g.A,{mdxType:"MarginText"},"8"),(0,t.yg)("p",null,(0,t.yg)("em",null,"Examples of string literals:")," ",(0,t.yg)("br",null)),(0,t.yg)(g.A,{mdxType:"MarginText"},"9/2"),(0,t.yg)(s.A,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),(0,t.yg)(o.A,{language:"ada",mdxType:"CodeBlock"},'"Message of the day:"',"\n","\n",'""                    --  a null string literal',"\n",'" "   "A"   """"      --  three string literals of length 1',"\n","\n",'"Characters such as $, %, and ',"}",' are allowed in string literals"',"\n",'"Archimedes said ""\u0395\u03cd\u03c1\u03b7\u03ba\u03b1"""',"\n",'"Volume of cylinder (\u03c0r\xb2h) = "',"\n"),(0,t.yg)(l.A,{mdxType:"AnnotatedOnly"},(0,t.yg)("h4",{id:"wording-changes-from-ada-83"},"Wording Changes from Ada 83")),(0,t.yg)(l.A,{mdxType:"AnnotatedOnly"},(0,t.yg)(g.A,{mdxType:"MarginText"},"9.a"),(0,t.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"The wording has been changed to be strictly lexical. No mention is made of string or character values, since ",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal")),"s are also used to represent ",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-6/AA-6.1#S0202"},"operator_symbol")),"s, which don't have a defined value.",(0,t.yg)("br",null))),(0,t.yg)(l.A,{mdxType:"AnnotatedOnly"},(0,t.yg)(g.A,{mdxType:"MarginText"},"9.b"),(0,t.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"The syntax is described differently. ",(0,t.yg)("br",null))),(0,t.yg)(l.A,{mdxType:"AnnotatedOnly"},(0,t.yg)("h4",{id:"wording-changes-from-ada-95"},"Wording Changes from Ada 95")),(0,t.yg)(l.A,{mdxType:"AnnotatedOnly"},(0,t.yg)(g.A,{mdxType:"MarginText"},"9.c/2"),(0,t.yg)(s.A,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,t.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"We explicitly say that the characters of a ",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal"))," should be used as is. In particular, no normalization or folding should be performed on a ",(0,t.yg)("code",null,(0,t.yg)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal")),". ",(0,t.yg)("br",null))))}p.isMDXComponent=!0}}]);