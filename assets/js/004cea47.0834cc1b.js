"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[6205],{94680:(e,a,n)=>{n.r(a),n.d(a,{assets:()=>m,contentTitle:()=>y,default:()=>A,frontMatter:()=>g,metadata:()=>u,toc:()=>d});var t=n(58168),r=(n(96540),n(15680)),o=n(20793),i=n(91435),s=(n(21432),n(79162)),l=n(34421);const g={sidebar_position:50},y="6 Subprograms",u={unversionedId:"arm/AA-6/AA-6",id:"arm/AA-6/AA-6",title:"6 Subprograms",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-6/AA-6.mdx",sourceDirName:"arm/AA-6",slug:"/arm/AA-6/",permalink:"/docs/arm/AA-6/",draft:!1,tags:[],version:"current",sidebarPosition:50,frontMatter:{sidebar_position:50},sidebar:"referenceManualSidebar",previous:{title:"5.8 Goto Statements",permalink:"/docs/arm/AA-5/AA-5.8"},next:{title:"6.1 Subprogram Declarations",permalink:"/docs/arm/AA-6/AA-6.1"}},m={},d=[],p={toc:d},c="wrapper";function A(e){let{components:a,...n}=e;return(0,r.yg)(c,(0,t.A)({},p,n,{components:a,mdxType:"MDXLayout"}),(0,r.yg)("h1",{id:"6-subprograms"},"6 Subprograms"),(0,r.yg)("admonition",{type:"warning"},(0,r.yg)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,r.yg)("a",{parentName:"p",href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"},"tracking issue"))),(0,r.yg)(s.A,{mdxType:"MarginText"},"1"),(0,r.yg)("p",null,"A subprogram is a program unit or intrinsic operation whose execution is invoked by a subprogram call. There are two forms of subprogram: procedures and functions. A procedure call is a ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-5/AA-5.1#S0167"},"statement")),"; a function call is an expression and returns a value. The definition of a subprogram can be given in two parts: a subprogram declaration defining its interface, and a ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram_body"))," defining its execution. [Operators and enumeration literals are functions.] ",(0,r.yg)("br",null)),(0,r.yg)(i.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(s.A,{mdxType:"MarginText"},"1.a"),(0,r.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,r.yg)("strong",null,"To be honest: "),"A function call is an expression, but more specifically it is a ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),". ",(0,r.yg)("br",null))),(0,r.yg)(i.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(s.A,{mdxType:"MarginText"},"1.b/5"),(0,r.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,r.yg)("strong",null,"Term entry: "),(0,r.yg)("strong",null,"subprogram")," \u2014 unit of a program that can be brought into execution in various contexts, with the invocation being a subprogram call that can parameterize the effect of the subprogram through the passing of operands",(0,r.yg)("br",null),"Note: There are two forms of subprograms: functions, which return values, and procedures, which do not.",(0,r.yg)("br",null))),(0,r.yg)(i.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(s.A,{mdxType:"MarginText"},"1.c/5"),(0,r.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,r.yg)("strong",null,"Term entry: "),(0,r.yg)("strong",null,"function")," \u2014 form of subprogram that returns a result and can be called as part of an expression",(0,r.yg)("br",null))),(0,r.yg)(i.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(s.A,{mdxType:"MarginText"},"1.d/5"),(0,r.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,r.yg)("strong",null,"Term entry: "),(0,r.yg)("strong",null,"procedure")," \u2014 form of subprogram that does not return a result and can only be invoked by a statement",(0,r.yg)("br",null))),(0,r.yg)(s.A,{mdxType:"MarginText"},"2/3"),(0,r.yg)(l.A,{items:["AI05-0299-1"],mdxType:"MarginInfo"}),(0,r.yg)("p",null,"A ",(0,r.yg)("em",null,"callable entity")," is a subprogram or entry (see Section 9). A callable entity is invoked by a ",(0,r.yg)("em",null,"call"),"; that is, a subprogram call or entry call. A ",(0,r.yg)("em",null,"callable construct")," is a construct that defines the action of a call upon a callable entity: a ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram_body")),", ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-9/AA-9.5#S0260"},"entry_body")),", or ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-9/AA-9.5#S0258"},"accept_statement")),". ",(0,r.yg)("br",null)),(0,r.yg)(i.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(s.A,{mdxType:"MarginText"},"2.a"),(0,r.yg)(o.A,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,r.yg)("strong",null),"Note that \u201ccallable entity\u201d includes predefined operators, enumeration literals, and abstract subprograms. \u201cCall\u201d includes calls of these things. They do not have callable constructs, since they don't have completions. ",(0,r.yg)("br",null))))}A.isMDXComponent=!0}}]);