"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2419],{62127:(e,s,r)=>{r.r(s),r.d(s,{assets:()=>j,contentTitle:()=>x,default:()=>p,frontMatter:()=>l,metadata:()=>n,toc:()=>A});const n=JSON.parse('{"id":"arm/AA-2/AA-2.1","title":"2.1 Character Set","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-2/AA-2.1.mdx","sourceDirName":"arm/AA-2","slug":"/arm/AA-2/AA-2.1","permalink":"/docs/arm/AA-2/AA-2.1","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":9,"frontMatter":{"sidebar_position":9},"sidebar":"referenceManualSidebar","previous":{"title":"2 Lexical Elements","permalink":"/docs/arm/AA-2/"},"next":{"title":"2.2 Lexical Elements, Separators, and Delimiters","permalink":"/docs/arm/AA-2/AA-2.2"}}');var a=r(74848),i=r(28453),t=r(13842),c=r(91435),d=r(21432),o=r(79162),h=r(34421);const l={sidebar_position:9},x="2.1 Character Set",j={},A=[{value:"Syntax",id:"syntax",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Inconsistencies With Ada 2005",id:"inconsistencies-with-ada-2005",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}];function m(e){const s={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,i.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(s.header,{children:(0,a.jsx)(s.h1,{id:"21-character-set",children:"2.1 Character Set"})}),"\n",(0,a.jsx)(s.admonition,{type:"danger",children:(0,a.jsxs)(s.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.jsx)(s.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,a.jsx)(o.A,{children:"1/5"}),"\n",(0,a.jsx)(h.A,{items:["AI95-00285-01","AI95-00395-01","AI05-0266-1","AI12-0263-1","AI12-0450-1","AI12-0454-1"]}),"\n",(0,a.jsxs)("p",{children:["The character repertoire for the text of an Ada program consists of the entire coding space described by the ISO/IEC 10646:2020 Universal Coded Character Set. This coding space is organized in ",(0,a.jsx)("em",{children:"planes"}),", each plane comprising 65536 characters. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"1.a/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)(t.A,{type:"aarm",aarm:"note",children:[(0,a.jsx)("em",{children:"This paragraph was deleted."}),(0,a.jsx)("br",{})]})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"1.b/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)(t.A,{type:"aarm",aarm:"note",children:[(0,a.jsx)("em",{children:"This paragraph was deleted."}),(0,a.jsx)("br",{})]})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"1.c/5"}),(0,a.jsx)(h.A,{items:["AI95-00285-01","AI05-0266-1","AI12-0263-1","AI12-0450-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"discussion",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{}),"It is our intent to follow the terminology of ISO/IEC 10646:2020 where appropriate, and to remain compatible with the character classifications defined in ",(0,a.jsx)("a",{href:"/docs/arm/AA-A/AA-A.3",children:"A.3"}),", \u201c",(0,a.jsx)("a",{href:"/docs/arm/AA-A/AA-A.3",children:"Character Handling"}),"\u201d. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(s.h4,{id:"syntax",children:"Syntax"}),"\n",(0,a.jsxs)("p",{class:"Indented2",children:[(0,a.jsx)("em",{children:"Paragraphs 2 and 3 were deleted."})," ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(o.A,{children:"3.1/5"}),"\n",(0,a.jsx)(h.A,{items:["AI95-00285-01","AI95-00395-01","AI05-0266-1","AI12-0263-1","AI12-0450-1"]}),"\n",(0,a.jsxs)("p",{class:"Indented2",children:["A ",(0,a.jsx)("code",{children:"character"})," is defined by this Reference Manual for each cell in the coding space described by ISO/IEC 10646:2020, regardless of whether or not ISO/IEC 10646:2020 allocates a character to that cell. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(s.h4,{id:"static-semantics",children:"Static Semantics"}),"\n",(0,a.jsx)(o.A,{children:"4/5"}),"\n",(0,a.jsx)(h.A,{items:["AI95-00285-01","AI95-00395-01","AI05-0079-1","AI05-0262-1","AI05-0266-1","AI12-0263-1","AI12-0444-1","AI12-0450-1"]}),"\n",(0,a.jsxs)("p",{children:["The coded representation for characters is implementation defined [(it can be a representation that is not defined within ISO/IEC 10646:2020)]. A character whose relative code point in its plane is 16","#","FFFE","#"," or 16","#","FFFF","#"," is not allowed anywhere in the text of a program. The only characters allowed outside of comments are those in categories ",(0,a.jsxs)("code",{children:["other","_","format"]}),", ",(0,a.jsxs)("code",{children:["format","_","effector"]}),", and ",(0,a.jsxs)("code",{children:["graphic","_","character"]}),". ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"4.a"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"implementation-defined",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{}),"The coded representation for the text of an Ada program.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"4.b/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"ramification",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{}),"Note that this rule doesn't really have much force, since the implementation can represent characters in the source in any way it sees fit. For example, an implementation could simply define that what seems to be an ",(0,a.jsxs)("code",{children:["other","_","private","_","use"]})," character is actually a representation of the space character. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(o.A,{children:"4.1/5"}),"\n",(0,a.jsx)(h.A,{items:["AI95-00285-01","AI05-0266-1","AI05-0299-1","AI12-0004-1","AI12-0263-1","AI12-0450-1"]}),"\n",(0,a.jsxs)("p",{children:["The semantics of an Ada program whose text is not in Normalization Form C (as defined by Clause 22 of ISO/IEC 10646:2020) is implementation defined. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"4.c/5"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"implementation-defined",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{}),"The semantics of an Ada program whose text is not in Normalization Form C.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"4.d/5"}),(0,a.jsx)(h.A,{items:["AI12-0004-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"ramification",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{}),"In particular, an implementation can reject such program source. It is easy during lexical analysis to reject source that contains any code point not present in Normalization Form C. Portable programs should always be encoded in Normalization Form C. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"4.e/5"}),(0,a.jsx)(h.A,{items:["AI12-0004-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"reason",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{}),"Normalization Form C ensures that all source is in a unique format; it eliminates ambiguities and security issues potentially caused by source using unusual sequences of characters. Note that WC3 (the Internet standards group) recommends that all Internet content be in Normalization Form C. We don't require this as there is a potentially significant cost to checking this (just rejecting unallowed code points is not enough), and some implementations may need to be interoperable with tools that produce unnormalized text. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(o.A,{children:"5/5"}),"\n",(0,a.jsx)(h.A,{items:["AI95-00285-01","AI05-0266-1","AI05-0299-1","AI12-0263-1","AI12-0450-1"]}),"\n",(0,a.jsxs)("p",{children:["The description of the language definition in this document uses the character properties General Category, Simple Uppercase Mapping, Uppercase Mapping, and Special Case Condition of the documents referenced by Clause 2 of ISO/IEC 10646:2020. The actual set of graphic symbols used by an implementation for the visual representation of the text of an Ada program is not specified. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"5.a/5"}),(0,a.jsx)(h.A,{items:["AI12-0263-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"discussion",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{}),"The \u201cdocuments referenced\u201d means Unicode, Chapter 4. See the Discussion after the the character categorization definition for a source for machine-readable definitions of these properties. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(o.A,{children:"6/3"}),"\n",(0,a.jsx)(h.A,{items:["AI95-00285-01","AI05-0266-1"]}),"\n",(0,a.jsxs)("p",{children:["Characters are categorized as follows: ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"6.a/5"}),(0,a.jsx)(h.A,{items:["AI05-0005-1","AI05-0262-1","AI05-0266-1","AI12-0263-1","AI12-0450-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"discussion",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{}),"Our character classification considers that the cells not allocated in ISO/IEC 10646:2020 are graphic characters, except for those whose relative code point in their plane is 16","#","FFFE","#"," or 16","#","FFFF","#",". This seems to provide the best compatibility with future versions of ISO/IEC 10646, as future characters can already be used in Ada character and string literals. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(o.A,{children:"7/2"}),"\n",(0,a.jsx)("dt",{children:(0,a.jsx)("br",{})}),"\n",(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),"\n",(0,a.jsxs)("dl",{children:[(0,a.jsxs)("dd",{children:[(0,a.jsx)("em",{children:"This paragraph was deleted."}),(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"8/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["letter","_","uppercase"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cLetter, Uppercase\u201d.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"9/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["letter","_","lowercase"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cLetter, Lowercase\u201d. ",(0,a.jsx)("br",{})]})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"9.a/1"}),(0,a.jsx)(h.A,{items:["AI95-00124-01"]}),(0,a.jsxs)(t.A,{type:"aarm",aarm:"note",children:[(0,a.jsx)("em",{children:"This paragraph was deleted."}),"{",(0,a.jsx)("em",{children:"8652/0001"}),"}",(0,a.jsx)("br",{})]})]}),"\n",(0,a.jsx)(o.A,{children:"9.1/2"}),"\n",(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),"\n",(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["letter","_","titlecase"]})]}),"\n",(0,a.jsxs)("dl",{children:[(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cLetter, Titlecase\u201d.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"9.2/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["letter","_","modifier"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cLetter, Modifier\u201d.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"9.3/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["letter","_","other"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cLetter, Other\u201d.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"9.4/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["mark","_","non","_","spacing"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cMark, Non-Spacing\u201d.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"9.5/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["mark","_","spacing","_","combining"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cMark, Spacing Combining\u201d.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"10/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["number","_","decimal"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cNumber, Decimal\u201d.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"10.1/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["number","_","letter"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cNumber, Letter\u201d.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"10.2/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["punctuation","_","connector"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cPunctuation, Connector\u201d.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"10.3/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["other","_","format"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cOther, Format\u201d.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"11/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["separator","_","space"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cSeparator, Space\u201d.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"12/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["separator","_","line"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cSeparator, Line\u201d. ",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"12.1/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["separator","_","paragraph"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cSeparator, Paragraph\u201d.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"13/3"}),(0,a.jsx)(h.A,{items:["AI95-00285-01","AI05-0262-1"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["format","_","effector"]})]}),(0,a.jsxs)("dd",{children:["The characters whose code points are 16","#","09","#"," (CHARACTER TABULATION), 16","#","0A","#"," (LINE FEED), 16","#","0B","#"," (LINE TABULATION), 16","#","0C","#"," (FORM FEED), 16","#","0D","#"," (CARRIAGE RETURN), 16","#","85","#"," (NEXT LINE), and the characters in categories ",(0,a.jsxs)("code",{children:["separator","_","line"]})," and ",(0,a.jsxs)("code",{children:["separator","_","paragraph"]}),". ",(0,a.jsx)("br",{})]})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"13.a/2"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"discussion",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{}),"ISO/IEC 10646:2003 does not define the names of control characters, but rather refers to the names defined by ISO/IEC 6429:1992. These are the names that we use here. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(o.A,{children:"13.1/2"}),"\n",(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),"\n",(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["other","_","control"]})]}),"\n",(0,a.jsxs)("dl",{children:[(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cOther, Control\u201d, and which is not defined to be a ",(0,a.jsxs)("code",{children:["format","_","effector"]}),".",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"13.2/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["other","_","private","_","use"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cOther, Private Use\u201d.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"13.3/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["other","_","surrogate"]})]}),(0,a.jsxs)("dd",{children:["Any character whose General Category is defined to be \u201cOther, Surrogate\u201d.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"14/3"}),(0,a.jsx)(h.A,{items:["AI95-00285-01","AI95-00395-01","AI05-0262-1"]}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),(0,a.jsxs)("code",{children:["graphic","_","character"]})]}),(0,a.jsxs)("dd",{children:["Any character that is not in the categories ",(0,a.jsxs)("code",{children:["other","_","control"]}),", ",(0,a.jsxs)("code",{children:["other","_","private","_","use"]}),", ",(0,a.jsxs)("code",{children:["other","_","surrogate"]}),", ",(0,a.jsxs)("code",{children:["format","_","effector"]}),", and whose relative code point in its plane is neither 16","#","FFFE","#"," nor 16","#","FFFF","#",". ",(0,a.jsx)("br",{})]})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"14.a/2"}),(0,a.jsxs)(t.A,{type:"aarm",aarm:"note",children:[(0,a.jsx)("em",{children:"This paragraph was deleted."}),(0,a.jsx)("br",{})]})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"14.b/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"discussion",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{}),"We considered basing the definition of lexical elements on Annex A of ISO/IEC TR 10176 (4th edition), which lists the characters which should be supported in identifiers for all programming languages, but we finally decided against this option. Note that it is not our intent to diverge from ISO/IEC TR 10176, except to the extent that ISO/IEC TR 10176 itself diverges from ISO/IEC 10646:2003 (which is the case at the time of this writing [January 2005]).",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"14.c/2"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(s.p,{children:["More precisely, we intend to align strictly with ISO/IEC 10646:2003. It must be noted that ISO/IEC TR 10176 is a Technical Report while ISO/IEC 10646:2003 is a Standard. If one has to make a choice, one should conform with the Standard rather than with the Technical Report. And, it turns out that one ",(0,a.jsx)("em",{children:"must"})," make a choice because there are important differences between the two:",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"14.d/2"}),(0,a.jsxs)("ul",{children:[(0,a.jsxs)("li",{children:["ISO/IEC TR 10176 is still based on ISO/IEC 10646:2000 while ISO/IEC 10646:2003 has already been published for a year. We cannot afford to delay the adoption of our amendment until ISO/IEC TR 10176 has been revised.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"14.e/2"}),(0,a.jsxs)("li",{children:["There are considerable differences between the two editions of ISO/IEC 10646, notably in supporting characters beyond the BMP (this might be significant for some languages, e.g. Korean).",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"14.f/2"}),(0,a.jsxs)("li",{children:["ISO/IEC TR 10176 does not define case conversion tables, which are essential for a case-insensitive language like Ada. To get case conversion tables, we would have to reference either ISO/IEC 10646:2003 or Unicode, or we would have to invent our own. ",(0,a.jsx)("br",{})]})]})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"14.g/2"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(s.p,{children:["For the purpose of defining the lexical elements of the language, we need character properties like categorization, as well as case conversion tables. These are mentioned in ISO/IEC 10646:2003 as useful for implementations, with a reference to Unicode. Machine-readable tables are available on the web at URLs: ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"14.h/2"}),(0,a.jsxs)(d.A,{language:"ada",children:[(0,a.jsx)("a",{href:"http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt",children:(0,a.jsx)(s.a,{href:"http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt",children:"http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt"})}),"\n",(0,a.jsx)("a",{href:"http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt",children:(0,a.jsx)(s.a,{href:"http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt",children:"http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt"})}),"\n"]})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"14.i/2"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(s.p,{children:["with an explanatory document found at URL: ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"14.j/2"}),(0,a.jsxs)(d.A,{language:"ada",children:[(0,a.jsx)("a",{href:"http://www.unicode.org/reports/tr44/tr44-20.html",children:(0,a.jsx)(s.a,{href:"http://www.unicode.org/reports/tr44/tr44-20.html",children:"http://www.unicode.org/reports/tr44/tr44-20.html"})}),"\n"]})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"14.k/5"}),(0,a.jsx)(h.A,{items:["AI12-0263-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(s.p,{children:["The actual text of the standard only makes specific references to the corresponding clauses of ISO/IEC 10646, not to Unicode.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(o.A,{children:"15/5"}),"\n",(0,a.jsx)(h.A,{items:["AI95-00285-01","AI05-0266-1","AI12-0263-1","AI12-0450-1"]}),"\n",(0,a.jsxs)("p",{children:["The following names are used when referring to certain characters (the first name is that given in ISO/IEC 10646:2020): ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"15.a/5"}),(0,a.jsx)(h.A,{items:["AI95-00285-01","AI05-0266-1","AI12-0125-3","AI12-0212-1","AI12-0263-1","AI12-0450-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"discussion",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{}),"This table serves to show the correspondence between ISO/IEC 10646:2020 names and the graphic symbols (glyphs) used in this document. These are the characters that play a special role in the syntax of Ada. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)("p",{children:["  graphic symbol",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)("p",{children:['         "',(0,a.jsx)("br",{}),"         ","#",(0,a.jsx)("br",{}),"         &",(0,a.jsx)("br",{}),"         '",(0,a.jsx)("br",{}),"         (",(0,a.jsx)("br",{}),"         )",(0,a.jsx)("br",{}),"         ","*",(0,a.jsx)("br",{}),"         +",(0,a.jsx)("br",{}),"         ,",(0,a.jsx)("br",{}),"         \u2013",(0,a.jsx)("br",{}),"         .",(0,a.jsx)("br",{}),"         @",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)("p",{children:["name",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)("p",{children:["quotation mark",(0,a.jsx)("br",{}),"number sign",(0,a.jsx)("br",{}),"ampersand",(0,a.jsx)("br",{}),"apostrophe, tick",(0,a.jsx)("br",{}),"left parenthesis",(0,a.jsx)("br",{}),"right parenthesis",(0,a.jsx)("br",{}),"asterisk, multiply",(0,a.jsx)("br",{}),"plus sign",(0,a.jsx)("br",{}),"comma",(0,a.jsx)("br",{}),"hyphen-minus, minus",(0,a.jsx)("br",{}),"full stop, dot, point",(0,a.jsx)("br",{}),"commercial at, at sign",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)("p",{children:["  graphic symbol",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)("p",{children:["         :",(0,a.jsx)("br",{}),"         ;",(0,a.jsx)("br",{}),"         ","<",(0,a.jsx)("br",{}),"         =",(0,a.jsx)("br",{}),"         ",">",(0,a.jsx)("br",{}),"         ","_",(0,a.jsx)("br",{}),"         |",(0,a.jsx)("br",{}),"         /",(0,a.jsx)("br",{}),"         !",(0,a.jsx)("br",{}),"         %",(0,a.jsx)("br",{}),"         [",(0,a.jsx)("br",{}),"         ] ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)("p",{children:["name",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)("p",{children:["colon",(0,a.jsx)("br",{}),"semicolon",(0,a.jsx)("br",{}),"less-than sign",(0,a.jsx)("br",{}),"equals sign",(0,a.jsx)("br",{}),"greater-than sign",(0,a.jsx)("br",{}),"low line, underline",(0,a.jsx)("br",{}),"vertical line",(0,a.jsx)("br",{}),"solidus, divide",(0,a.jsx)("br",{}),"exclamation point",(0,a.jsx)("br",{}),"percent sign",(0,a.jsx)("br",{}),"left square bracket",(0,a.jsx)("br",{}),"right square bracket ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(s.h4,{id:"implementation-requirements",children:"Implementation Requirements"}),"\n",(0,a.jsx)(o.A,{children:"16/3"}),"\n",(0,a.jsx)(h.A,{items:["AI05-0286-1"]}),"\n",(0,a.jsxs)("p",{children:["An Ada implementation shall accept Ada source code in UTF-8 encoding, with or without a BOM (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-A/AA-A.4#Subclause_A.4.11",children:"A.4.11"}),"), where every character is represented by its code point. The character pair CARRIAGE RETURN/LINE FEED (code points 16","#","0D","#"," 16","#","0A","#",") signifies a single end of line (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-2/AA-2.2",children:"2.2"}),"); every other occurrence of a ",(0,a.jsxs)("code",{children:["format","_","effector"]})," other than the character whose code point position is 16","#","09","#"," (CHARACTER TABULATION) also signifies a single end of line.",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"16.a/3"}),(0,a.jsx)(h.A,{items:["AI05-0079-1","AI05-0286-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"reason",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{}),'This is simply requiring that an Ada implementation be able to directly process the ACATS, which is provided in the described format. Note that files that only contain characters with code points in the first 128 (which is the majority of the ACATS) are represented in the same way in both UTF-8 and in "plain" string format. The ACATS includes a BOM in files that have any characters with code points greater than 127. Note that the BOM contains characters not legal in Ada source code, so an implementation can use that to automatically distinguish between files formatted as plain Latin-1 strings and UTF-8 with BOM.',(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"16.b/3"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(s.p,{children:["We allow line endings to be both represented as the pair CR LF (as in Windows and the ACATS), and as single ",(0,a.jsxs)("code",{children:["format","_","effector"]})," characters (usually LF, as in Linux), in order that files created by standard tools on most operating systems will meet the standard format. We specify how many line endings each represent so that compilers use the same line numbering for standard source files.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"16.c/3"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(s.p,{children:["This requirement increases portability by having a format that is accepted by all Ada compilers. Note that implementations can support other source representations, including structured representations like a parse tree.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(s.h4,{id:"implementation-permissions",children:"Implementation Permissions"}),"\n",(0,a.jsx)(o.A,{children:"17/3"}),"\n",(0,a.jsx)(h.A,{items:["AI95-00285-01","AI05-0266-1"]}),"\n",(0,a.jsxs)("p",{children:["The categories defined above, as well as case mapping and folding, may be based on an implementation-defined version of ISO/IEC 10646 (2003 edition or later). ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"17.b/3"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"ramification",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{}),"The exact categories, case mapping, and case folding chosen affects identifiers, the result of '[[Wide","_","]Wide","_","]Image, and packages Wide","_","Characters.Handling and Wide","_","Wide","_","Characters.Handling. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"17.c/3"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"discussion",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{}),"This permission allows implementations to upgrade to using a newer character set standard whenever that makes sense, rather than having to wait for the next Ada Standard. But the character set standard used cannot be older than ISO/IEC 10646:2003 (which is essentially similar to Unicode 4.0). ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"18/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(s.p,{children:["NOTE   The characters in categories ",(0,a.jsxs)("code",{children:["other","_","control"]}),", ",(0,a.jsxs)("code",{children:["other","_","private","_","use"]}),", and ",(0,a.jsxs)("code",{children:["other","_","surrogate"]})," are only allowed in comments.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"19.a/3"}),(0,a.jsx)(h.A,{items:["AI05-0286-1"]}),(0,a.jsxs)(t.A,{type:"aarm",aarm:"note",children:[(0,a.jsx)("em",{children:"This paragraph was deleted."}),(0,a.jsx)("br",{})]})]}),"\n",(0,a.jsx)(c.A,{children:(0,a.jsx)(s.h4,{id:"extensions-to-ada-83",children:"Extensions to Ada 83"})}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"19.b"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(s.p,{children:["Ada 95 allows 8-bit and 16-bit characters, as well as implementation-specified character sets. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(c.A,{children:(0,a.jsx)(s.h4,{id:"wording-changes-from-ada-83",children:"Wording Changes from Ada 83"})}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"19.c/3"}),(0,a.jsx)(h.A,{items:["AI95-00285-01","AI05-0299-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(s.p,{children:["The syntax rules in this subclause are modified to remove the emphasis on basic characters vs. others. (In this day and age, there is no need to point out that you can write programs without using (for example) lower case letters.) In particular, ",(0,a.jsx)("code",{children:"character"})," (representing all characters usable outside comments) is added, and ",(0,a.jsxs)("code",{children:["basic","_","graphic","_","character"]}),", ",(0,a.jsxs)("code",{children:["other","_","special","_","character"]}),", and ",(0,a.jsxs)("code",{children:["basic","_","character"]})," are removed. ",(0,a.jsxs)("code",{children:["Special","_","character"]})," is expanded to include Ada 83's ",(0,a.jsxs)("code",{children:["other","_","special","_","character"]}),", as well as new 8-bit characters not present in Ada 83. Ada 2005 removes ",(0,a.jsxs)("code",{children:["special","_","character"]})," altogether; we want to stick to ISO/IEC 10646:2003 character classifications. Note that the term \u201cbasic letter\u201d is used in ",(0,a.jsx)("a",{href:"/docs/arm/AA-A/AA-A.3",children:"A.3"}),", \u201c",(0,a.jsx)("a",{href:"/docs/arm/AA-A/AA-A.3",children:"Character Handling"}),"\u201d to refer to letters without diacritical marks.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"19.d/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(s.p,{children:["Character names now come from ISO/IEC 10646:2003.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"19.e/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01"]}),(0,a.jsxs)(t.A,{type:"aarm",aarm:"note",children:[(0,a.jsx)("em",{children:"This paragraph was deleted."}),(0,a.jsx)("br",{})]})]}),"\n",(0,a.jsx)(c.A,{children:(0,a.jsx)(s.h4,{id:"extensions-to-ada-95",children:"Extensions to Ada 95"})}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"19.f/2"}),(0,a.jsx)(h.A,{items:["AI95-00285-01","AI95-00395-01"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(s.p,{children:["Program text can use most characters defined by ISO-10646:2003. This subclause has been rewritten to use the categories defined in that Standard. This should ease programming in languages other than English. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(c.A,{children:(0,a.jsx)(s.h4,{id:"inconsistencies-with-ada-2005",children:"Inconsistencies With Ada 2005"})}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"19.g/3"}),(0,a.jsx)(h.A,{items:["AI05-0266-1","AI05-0299-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(s.p,{children:["An implementation is allowed (but not required) to use a newer character set standard to determine the categories, case mapping, and case folding. Doing so will change the results of attributes '[[Wide","_","]Wide","_","]Image and the packages [Wide","_","]Wide","_","Characters.Handling in the case of a few rarely used characters. (This also could make some identifiers illegal, for characters that are no longer classified as letters.) This is unlikely to be a problem in practice. Moreover, truly portable Ada 2012 programs should avoid using in these contexts any characters that would have different classifications in any character set standards issued since 10646:2003 (since the compiler can use any such standard as the basis for its classifications). ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(c.A,{children:(0,a.jsx)(s.h4,{id:"wording-changes-from-ada-2005",children:"Wording Changes from Ada 2005"})}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"19.h/3"}),(0,a.jsx)(h.A,{items:["AI05-0079-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"correction",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{})," Clarified that only characters in the categories defined here are allowed in the source of an Ada program. This was clear in Ada 95, but Amendment 1 dropped the wording instead of correcting it.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"19.i/3"}),(0,a.jsx)(h.A,{items:["AI05-0286-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(s.p,{children:['A standard source representation is defined that all compilers are expected to process. Since this is the same format as the ACATS, it seems unlikely that there are any implementations that don\'t meet this requirement. Moreover, other representations are still permitted, and the "impossible or impractical" loophole (see ',(0,a.jsx)("a",{href:"/docs/arm/AA-1/AA-1.1#Subclause_1.1.3",children:"1.1.3"}),") can be invoked for any implementations that cannot directly process the ACATS. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(c.A,{children:(0,a.jsx)(s.h4,{id:"wording-changes-from-ada-2012",children:"Wording Changes from Ada 2012"})}),"\n",(0,a.jsxs)(c.A,{children:[(0,a.jsx)(o.A,{children:"19.j/5"}),(0,a.jsx)(h.A,{items:["AI12-0004-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"correction",children:(0,a.jsxs)(s.p,{children:[(0,a.jsx)("strong",{})," The interpretation of Ada source that is in Normalization Form C but not in Normalization Form KC is no longer implementation-defined. This change could potentially change the meaning of a program for a compiler that normalized all program source to Normalization Form KC before processing it. We don't document this as an inconsistency as such handling was previously implementation defined (so any such code was already defined to be not portable), and we're not aware of any compiler that normalized source code (so we don't expect to see this problem in the real world). ",(0,a.jsx)("br",{})]})})]})]})}function p(e={}){const{wrapper:s}={...(0,i.R)(),...e.components};return s?(0,a.jsx)(s,{...e,children:(0,a.jsx)(m,{...e})}):m(e)}}}]);