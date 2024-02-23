"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2446],{75996:(e,a,n)=>{n.r(a),n.d(a,{assets:()=>A,contentTitle:()=>s,default:()=>u,frontMatter:()=>g,metadata:()=>y,toc:()=>m});var t=n(58168),r=(n(96540),n(15680)),l=n(20793),i=n(91435),o=(n(21432),n(79162)),d=n(34421);const g={sidebar_position:10},s="2.2 Lexical Elements, Separators, and Delimiters",y={unversionedId:"arm/AA-2/AA-2.2",id:"arm/AA-2/AA-2.2",title:"2.2 Lexical Elements, Separators, and Delimiters",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-2/AA-2.2.mdx",sourceDirName:"arm/AA-2",slug:"/arm/AA-2/AA-2.2",permalink:"/docs/arm/AA-2/AA-2.2",draft:!1,tags:[],version:"current",sidebarPosition:10,frontMatter:{sidebar_position:10},sidebar:"referenceManualSidebar",previous:{title:"2.1 Character Set",permalink:"/docs/arm/AA-2/AA-2.1"},next:{title:"2.3 Identifiers",permalink:"/docs/arm/AA-2/AA-2.3"}},A={},m=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],c={toc:m},p="wrapper";function u(e){let{components:a,...n}=e;return(0,r.yg)(p,(0,t.A)({},c,n,{components:a,mdxType:"MDXLayout"}),(0,r.yg)("h1",{id:"22-lexical-elements-separators-and-delimiters"},"2.2 Lexical Elements, Separators, and Delimiters"),(0,r.yg)("admonition",{type:"warning"},(0,r.yg)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,r.yg)("a",{parentName:"p",href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"},"tracking issue"))),(0,r.yg)("h4",{id:"static-semantics"},"Static Semantics"),(0,r.yg)(o.A,{mdxType:"MarginText"},"1"),(0,r.yg)("p",null,"The text of a program consists of the texts of one or more ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-10/AA-10.1#S0285"},"compilation")),"s. The text of each ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-10/AA-10.1#S0285"},"compilation"))," is a sequence of separate ",(0,r.yg)("em",null,"lexical elements"),". Each lexical element is formed from a sequence of characters, and is either a delimiter, an ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier")),", a reserved word, a ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.4#S0006"},"numeric_literal")),", a ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.5#S0015"},"character_literal")),", a ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal")),", or a comment. The meaning of a program depends only on the particular sequences of lexical elements that form its ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-10/AA-10.1#S0285"},"compilation")),"s, excluding ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.7#S0018"},"comment")),"s.",(0,r.yg)("br",null)),(0,r.yg)(o.A,{mdxType:"MarginText"},"2/3"),(0,r.yg)(d.A,{items:["AI95-00285-01","AI05-0262-1"],mdxType:"MarginInfo"}),(0,r.yg)("p",null,"The text of a ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-10/AA-10.1#S0285"},"compilation"))," is divided into ",(0,r.yg)("em",null,"lines"),". In general, the representation for an end of line is implementation defined. However, a sequence of one or more ",(0,r.yg)("code",null,"format","_","effector"),"s other than the character whose code point is 16","#","09","#"," (CHARACTER TABULATION) signifies at least one end of line. ",(0,r.yg)("br",null)),(0,r.yg)(i.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(o.A,{mdxType:"MarginText"},"2.a"),(0,r.yg)(l.A,{type:"aarm",aarm:"implementation-defined",mdxType:"Admonition"},(0,r.yg)("strong",null),"The representation for an end of line.",(0,r.yg)("br",null))),(0,r.yg)(o.A,{mdxType:"MarginText"},"3/2"),(0,r.yg)(d.A,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,r.yg)("p",null,"[In some cases an explicit ",(0,r.yg)("em",null,"separator")," is required to separate adjacent lexical elements.] A separator is any of a ",(0,r.yg)("code",null,"separator","_","space"),", a ",(0,r.yg)("code",null,"format","_","effector"),", or the end of a line, as follows: ",(0,r.yg)("br",null)),(0,r.yg)(o.A,{mdxType:"MarginText"},"4/2"),(0,r.yg)(d.A,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,r.yg)("ul",null,(0,r.yg)("li",null,"A ",(0,r.yg)("code",null,"separator","_","space")," is a separator except within a ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.7#S0018"},"comment")),", a ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal")),", or a ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.5#S0015"},"character_literal")),".",(0,r.yg)("br",null)),(0,r.yg)(o.A,{mdxType:"MarginText"},"5/3"),(0,r.yg)(d.A,{items:["AI95-00285-01","AI05-0262-1"],mdxType:"MarginInfo"}),(0,r.yg)("li",null,"The character whose code point is 16","#","09","#"," (CHARACTER TABULATION) is a separator except within a ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.7#S0018"},"comment")),".",(0,r.yg)("br",null)),(0,r.yg)(o.A,{mdxType:"MarginText"},"6"),(0,r.yg)("li",null,"The end of a line is always a separator. ",(0,r.yg)("br",null))),(0,r.yg)(o.A,{mdxType:"MarginText"},"7"),(0,r.yg)("p",null,"One or more separators are allowed between any two adjacent lexical elements, before the first of each ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-10/AA-10.1#S0285"},"compilation")),", or after the last. At least one separator is required between an ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier")),", a reserved word, or a ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.4#S0006"},"numeric_literal"))," and an adjacent ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier")),", reserved word, or ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.4#S0006"},"numeric_literal")),".",(0,r.yg)("br",null)),(0,r.yg)(o.A,{mdxType:"MarginText"},"7.1/3"),(0,r.yg)(d.A,{items:["AI05-0079-1"],mdxType:"MarginInfo"}),(0,r.yg)("p",null,"One or more ",(0,r.yg)("code",null,"other","_","format")," characters are allowed anywhere that a separator is[; any such characters have no effect on the meaning of an Ada program].",(0,r.yg)("br",null)),(0,r.yg)(o.A,{mdxType:"MarginText"},"8/2"),(0,r.yg)(d.A,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,r.yg)("p",null,"A ",(0,r.yg)("em",null,"delimiter")," is either one of the following characters: ",(0,r.yg)("br",null)),(0,r.yg)(o.A,{mdxType:"MarginText"},"9/5"),(0,r.yg)(d.A,{items:["AI12-0125-3","AI12-0212-1"],mdxType:"MarginInfo"}),(0,r.yg)("p",{class:"Indented2"},"&    '    (    )    ","*","    +    ,    \u2013    .    /    :    ;    ","<","    =    ",">","    @    [    ]    |",(0,r.yg)("br",null)),(0,r.yg)(o.A,{mdxType:"MarginText"},"10"),(0,r.yg)("p",null,"or one of the following ",(0,r.yg)("em",null,"compound delimiters")," each composed of two adjacent special characters ",(0,r.yg)("br",null)),(0,r.yg)(o.A,{mdxType:"MarginText"},"11"),(0,r.yg)("p",{class:"Indented2"},"=",">","    ..    ","*","*","    :=    /=    ",">","=    ","<","=    ","<","<","    ",">",">","    ","<",">",(0,r.yg)("br",null)),(0,r.yg)(o.A,{mdxType:"MarginText"},"12"),(0,r.yg)("p",null,"Each of the special characters listed for single character delimiters is a single delimiter except if this character is used as a character of a compound delimiter, or as a character of a ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.7#S0018"},"comment")),", ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal")),", ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.5#S0015"},"character_literal")),", or ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-2/AA-2.4#S0006"},"numeric_literal")),".",(0,r.yg)("br",null)),(0,r.yg)(o.A,{mdxType:"MarginText"},"13"),(0,r.yg)("p",null,"The following names are used when referring to compound delimiters:",(0,r.yg)("br",null)),(0,r.yg)("h4",{id:"implementation-requirements"},"Implementation Requirements"),(0,r.yg)(o.A,{mdxType:"MarginText"},"14"),(0,r.yg)("p",null,"delimiter name=",">","arrow..double dot","*","*","double star, exponentiate:=assignment (pronounced: \u201cbecomes\u201d)/=inequality (pronounced: \u201cnot equal\u201d)",">","=greater than or equal","<","=less than or equal","<","<","left label bracket",">",">","right label bracket","<",">","boxAn implementation shall support lines of at least 200 characters in length, not counting any characters used to signify the end of a line. An implementation shall support lexical elements of at least 200 characters in length. The maximum supported line length and lexical element length are implementation defined. ",(0,r.yg)("br",null)),(0,r.yg)(i.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(o.A,{mdxType:"MarginText"},"14.a"),(0,r.yg)(l.A,{type:"aarm",aarm:"implementation-defined",mdxType:"Admonition"},(0,r.yg)("strong",null),"Maximum supported line length and lexical element length.",(0,r.yg)("br",null))),(0,r.yg)(i.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(o.A,{mdxType:"MarginText"},"14.b"),(0,r.yg)(l.A,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,r.yg)("strong",null),"From URG recommendation. ",(0,r.yg)("br",null))),(0,r.yg)(i.A,{mdxType:"AnnotatedOnly"},(0,r.yg)("h4",{id:"wording-changes-from-ada-95"},"Wording Changes from Ada 95")),(0,r.yg)(i.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(o.A,{mdxType:"MarginText"},"14.c/3"),(0,r.yg)(d.A,{items:["AI95-00285-01","AI05-0299-1"],mdxType:"MarginInfo"}),(0,r.yg)(l.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"The wording was updated to use the new character categories defined in the preceding subclause. ",(0,r.yg)("br",null))),(0,r.yg)(i.A,{mdxType:"AnnotatedOnly"},(0,r.yg)("h4",{id:"extensions-to-ada-2005"},"Extensions to Ada 2005")),(0,r.yg)(i.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(o.A,{mdxType:"MarginText"},"14.d/3"),(0,r.yg)(d.A,{items:["AI05-0079-1"],mdxType:"MarginInfo"}),(0,r.yg)(l.A,{type:"aarm",aarm:"correction",mdxType:"Admonition"},(0,r.yg)("strong",null)," Clarified that ",(0,r.yg)("code",null,"other","_","format")," characters are allowed anywhere that separators are allowed. This was intended in Ada 2005, but didn't actually make it into the wording. ",(0,r.yg)("br",null))),(0,r.yg)(i.A,{mdxType:"AnnotatedOnly"},(0,r.yg)("h4",{id:"wording-changes-from-ada-2012"},"Wording Changes from Ada 2012")),(0,r.yg)(i.A,{mdxType:"AnnotatedOnly"},(0,r.yg)(o.A,{mdxType:"MarginText"},"14.e/5"),(0,r.yg)(d.A,{items:["AI12-0125-3","AI12-0212-1"],mdxType:"MarginInfo"}),(0,r.yg)(l.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Added square brackets and the ",(0,r.yg)("code",null,(0,r.yg)("a",{href:"../AA-5/AA-5.2#S0174"},"target_name"))," symbol (see ",(0,r.yg)("a",{href:"../AA-5/AA-5.2#Subclause_5.2.1"},"5.2.1"),") to the list of delimiters. ",(0,r.yg)("br",null))))}u.isMDXComponent=!0}}]);