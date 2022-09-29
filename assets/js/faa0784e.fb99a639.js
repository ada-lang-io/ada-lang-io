"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[9372],{5768:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>T,contentTitle:()=>y,default:()=>x,frontMatter:()=>g,metadata:()=>f,toc:()=>b});var a=n(1716),r=n(3050),i=n(8604),o=n(7318),l=n(4768),s=Object.defineProperty,d=Object.defineProperties,m=Object.getOwnPropertyDescriptors,c=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,p=Object.prototype.propertyIsEnumerable,h=(e,t,n)=>t in e?s(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,k=(e,t)=>{for(var n in t||(t={}))u.call(t,n)&&h(e,n,t[n]);if(c)for(var n of c(t))p.call(t,n)&&h(e,n,t[n]);return e};const g={sidebar_position:9},y="2.1 Character Set",f={unversionedId:"arm/AA-2/AA-2.1",id:"arm/AA-2/AA-2.1",title:"2.1 Character Set",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-2/AA-2.1.mdx",sourceDirName:"arm/AA-2",slug:"/arm/AA-2/AA-2.1",permalink:"/docs/arm/AA-2/AA-2.1",draft:!1,tags:[],version:"current",sidebarPosition:9,frontMatter:{sidebar_position:9},sidebar:"referenceManualSidebar",previous:{title:"2 Lexical Elements",permalink:"/docs/arm/AA-2/"},next:{title:"2.2 Lexical Elements, Separators, and Delimiters",permalink:"/docs/arm/AA-2/AA-2.2"}},T={},b=[{value:"Syntax",id:"syntax",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Inconsistencies With Ada 2005",id:"inconsistencies-with-ada-2005",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],A={toc:b};function x(e){var t,n=e,{components:s}=n,h=((e,t)=>{var n={};for(var a in e)u.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&c)for(var a of c(e))t.indexOf(a)<0&&p.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=k(k({},A),h),d(t,m({components:s,mdxType:"MDXLayout"}))),(0,a.kt)("h1",k({},{id:"21-character-set"}),"2.1 Character Set"),(0,a.kt)("admonition",k({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",k({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(o.Z,{mdxType:"MarginText"},"1/5"),(0,a.kt)(l.Z,{items:["AI95-00285-01","AI95-00395-01","AI05-0266-1","AI12-0263-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"The character repertoire for the text of an Ada program consists of the entire coding space described by the ISO/IEC 10646:2017  Universal  Coded Character Set. This coding space is organized in ",(0,a.kt)("em",null,"planes"),", each plane comprising 65536 characters. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"1.a/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,a.kt)("em",null,"This paragraph was deleted."),(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"1.b/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,a.kt)("em",null,"This paragraph was deleted."),(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"1.c/5"),(0,a.kt)(l.Z,{items:["AI95-00285-01","AI05-0266-1","AI12-0263-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"It is our intent to follow the terminology of ISO/IEC 10646:2017  where appropriate, and to remain compatible with the character classifications defined in ",(0,a.kt)("a",{href:"../AA-A/AA-A.3"},"A.3"),", \u201c",(0,a.kt)("a",{href:"../AA-A/AA-A.3"},"Character Handling"),"\u201d. ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"syntax"}),"Syntax"),(0,a.kt)("p",null,(0,a.kt)("em",null,"Paragraphs 2 and 3 were deleted.")," ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"3.1/5"),(0,a.kt)(l.Z,{items:["AI95-00285-01","AI95-00395-01","AI05-0266-1","AI12-0263-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"A ",(0,a.kt)("code",null,"character")," is defined by this Reference Manual for each cell in the coding space described by ISO/IEC 10646:2017 , regardless of whether or not ISO/IEC 10646:2017  allocates a character to that cell. ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"4/5"),(0,a.kt)(l.Z,{items:["AI95-00285-01","AI95-00395-01","AI05-0079-1","AI05-0262-1","AI05-0266-1","AI12-0263-1","AI12-0444-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"The coded representation for characters is implementation defined [(it can  be a representation that is not defined within ISO/IEC 10646:2017 )]. A character whose relative code point in its plane is 16#FFFE# or 16#FFFF# is not allowed anywhere in the text of a program. The only characters allowed outside of comments are those in categories ",(0,a.kt)("code",null,"other_format"),", ",(0,a.kt)("code",null,"format_effector"),", and ",(0,a.kt)("code",null,"graphic_character"),". ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"4.a"),(0,a.kt)(r.Z,{type:"aarm",aarm:"implementation-defined",title:"Implementation defined: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The coded representation for the text of an Ada program.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"4.b/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"Note that this rule doesn't really have much force, since the implementation can represent characters in the source in any way it sees fit. For example, an implementation could simply define that what seems to be an ",(0,a.kt)("code",null,"other_private_use")," character is actually a representation of the space character. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"4.1/5"),(0,a.kt)(l.Z,{items:["AI95-00285-01","AI05-0266-1","AI05-0299-1","AI12-0004-1","AI12-0263-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"The semantics of an Ada program whose text is not in Normalization Form C  (as defined by Clause 21 of ISO/IEC 10646:2017 ) is implementation defined. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"4.c/5"),(0,a.kt)(r.Z,{type:"aarm",aarm:"implementation-defined",title:"Implementation defined: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The semantics of an Ada program whose text is not in Normalization Form C .",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"4.d/5"),(0,a.kt)(l.Z,{items:["AI12-0004-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"In particular, an implementation can reject such program source. It is easy during lexical analysis to reject source that contains any code point not present in Normalization Form C. Portable programs should always be encoded in Normalization Form C. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"4.e/5"),(0,a.kt)(l.Z,{items:["AI12-0004-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"Normalization Form C ensures that all source is in a unique format; it eliminates ambiguities and security issues potentially caused by source using unusual sequences of characters. Note that WC3 (the Internet standards group) recommends that all Internet content be in Normalization Form C. We don't require this as there is a potentially significant cost to checking this (just rejecting unallowed code points is not enough), and some implementations may need to be interoperable with tools that produce unnormalized text. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"5/5"),(0,a.kt)(l.Z,{items:["AI95-00285-01","AI05-0266-1","AI05-0299-1","AI12-0263-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"The description of the language definition in this document uses the character properties General Category, Simple Uppercase Mapping, Uppercase Mapping, and Special Case Condition of the documents referenced by  Clause 2  of ISO/IEC 10646:2017 . The actual set of graphic symbols used by an implementation for the visual representation of the text of an Ada program is not specified. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"5.a/5"),(0,a.kt)(l.Z,{items:["AI12-0263-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The \u201cdocuments referenced\u201d means Unicode, Chapter 4. See the Discussion after the the character categorization definition for a source for machine-readable definitions of these properties. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"6/3"),(0,a.kt)(l.Z,{items:["AI95-00285-01","AI05-0266-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"Characters are categorized as follows: ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"6.a/5"),(0,a.kt)(l.Z,{items:["AI05-0005-1","AI05-0262-1","AI05-0266-1","AI12-0263-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"Our character classification considers that the cells not allocated in ISO/IEC 10646:2017  are graphic characters, except for those whose relative code point in their plane is 16#FFFE# or 16#FFFF#. This seems to provide the best compatibility with future versions of ISO/IEC 10646, as future characters can already be used in Ada character and string literals. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"7/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dl",null,(0,a.kt)("dd",null,(0,a.kt)("em",null,"This paragraph was deleted."),(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"8/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"letter_uppercase")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cLetter, Uppercase\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"9/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"letter_lowercase")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cLetter, Lowercase\u201d. ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"MarginText"},"9.a/1"),(0,a.kt)(l.Z,{items:["AI95-00124-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,a.kt)("em",null,"This paragraph was deleted."),"{",(0,a.kt)("em",null,"8652/0001"),"}"," ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"9.1/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"letter_titlecase")),(0,a.kt)("dl",null,(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cLetter, Titlecase\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"9.2/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"letter_modifier")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cLetter, Modifier\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"9.3/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"letter_other")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cLetter, Other\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"9.4/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"mark_non_spacing")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cMark, Non-Spacing\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"9.5/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"mark_spacing_combining")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cMark, Spacing Combining\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"10/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"number_decimal")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cNumber, Decimal\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"10.1/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"number_letter")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cNumber, Letter\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"10.2/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"punctuation_connector")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cPunctuation, Connector\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"10.3/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"other_format")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cOther, Format\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"11/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"separator_space")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cSeparator, Space\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"12/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"separator_line")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cSeparator, Line\u201d. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"12.1/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"separator_paragraph")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cSeparator, Paragraph\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"13/3"),(0,a.kt)(l.Z,{items:["AI95-00285-01","AI05-0262-1"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"format_effector")),(0,a.kt)("dd",null,"The characters whose code points are 16#09# (CHARACTER TABULATION), 16#0A# (LINE FEED), 16#0B# (LINE TABULATION), 16#0C# (FORM FEED), 16#0D# (CARRIAGE RETURN), 16#85# (NEXT LINE), and the characters in categories ",(0,a.kt)("code",null,"separator_line")," and ",(0,a.kt)("code",null,"separator_paragraph"),". ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"MarginText"},"13.a/2"),(0,a.kt)(r.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"ISO/IEC 10646:2003 does not define the names of control characters, but rather refers to the names defined by ISO/IEC 6429:1992. These are the names that we use here. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"13.1/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"other_control")),(0,a.kt)("dl",null,(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cOther, Control\u201d, and which is not defined to be a ",(0,a.kt)("code",null,"format_effector"),".",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"13.2/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"other_private_use")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cOther, Private Use\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"13.3/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"other_surrogate")),(0,a.kt)("dd",null,"Any character whose General Category is defined to be \u201cOther, Surrogate\u201d.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"14/3"),(0,a.kt)(l.Z,{items:["AI95-00285-01","AI95-00395-01","AI05-0262-1"],mdxType:"MarginInfo"}),(0,a.kt)("dt",null,(0,a.kt)("br",null),(0,a.kt)("code",null,"graphic_character")),(0,a.kt)("dd",null,"Any character that is not in the categories ",(0,a.kt)("code",null,"other_control"),", ",(0,a.kt)("code",null,"other_private_use"),", ",(0,a.kt)("code",null,"other_surrogate"),", ",(0,a.kt)("code",null,"format_effector"),", and whose relative code point in its plane is neither 16#FFFE# nor 16#FFFF#. ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"MarginText"},"14.a/2"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,a.kt)("em",null,"This paragraph was deleted."),(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"14.b/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"We considered basing the definition of lexical elements on Annex A of ISO/IEC TR 10176 (4th edition), which lists the characters which should be supported in identifiers for all programming languages, but we finally decided against this option. Note that it is not our intent to diverge from ISO/IEC TR 10176, except to the extent that ISO/IEC TR 10176 itself diverges from ISO/IEC 10646:2003 (which is the case at the time of this writing [January 2005]).",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"14.c/2"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"More precisely, we intend to align strictly with ISO/IEC 10646:2003. It must be noted that ISO/IEC TR 10176 is a Technical Report while ISO/IEC 10646:2003 is a Standard. If one has to make a choice, one should conform with the Standard rather than with the Technical Report. And, it turns out that one ",(0,a.kt)("em",null,"must")," make a choice because there are important differences between the two:",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"14.d/2"),(0,a.kt)("ul",null,(0,a.kt)("li",null,"ISO/IEC TR 10176 is still based on ISO/IEC 10646:2000 while ISO/IEC 10646:2003 has already been published for a year. We cannot afford to delay the adoption of our amendment until ISO/IEC TR 10176 has been revised.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"14.e/2"),(0,a.kt)("li",null,"There are considerable differences between the two editions of ISO/IEC 10646, notably in supporting characters beyond the BMP (this might be significant for some languages, e.g. Korean).",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"14.f/2"),(0,a.kt)("li",null,"ISO/IEC TR 10176 does not define case conversion tables, which are essential for a case-insensitive language like Ada. To get case conversion tables, we would have to reference either ISO/IEC 10646:2003 or Unicode, or we would have to invent our own. ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"MarginText"},"14.g/2"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"For the purpose of defining the lexical elements of the language, we need character properties like categorization, as well as case conversion tables. These are mentioned in ISO/IEC 10646:2003 as useful for implementations, with a reference to Unicode. Machine-readable tables are available on the web at URLs: ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"14.h/2"),(0,a.kt)(i.Z,{language:"ada",mdxType:"CodeBlock"},(0,a.kt)("a",{href:"http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt"},"http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt"),"\n",(0,a.kt)("a",{href:"http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt"},"http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt")," ","\n"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"14.i/2"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"with an explanatory document found at URL: ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"14.j/2"),(0,a.kt)(i.Z,{language:"ada",mdxType:"CodeBlock"},(0,a.kt)("a",{href:"http://www.unicode.org/reports/tr44/tr44-20.html"},"http://www.unicode.org/reports/tr44/tr44-20.html")," ","\n"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"14.k/5"),(0,a.kt)(l.Z,{items:["AI12-0263-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The actual text of the standard only makes specific references to the corresponding clauses of ISO/IEC 10646 , not to Unicode.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"15/5"),(0,a.kt)(l.Z,{items:["AI95-00285-01","AI05-0266-1","AI12-0263-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"The following names are used when referring to certain characters (the first name is that given in ISO/IEC 10646:2017 ): ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"15.a/5"),(0,a.kt)(l.Z,{items:["AI95-00285-01","AI05-0266-1","AI12-0125-3","AI12-0212-1","AI12-0263-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"This table serves to show the correspondence between ISO/IEC 10646:2017  names and the graphic symbols (glyphs) used in this document. These are the characters that play a special role in the syntax of Ada. ",(0,a.kt)("br",null)),(0,a.kt)("p",null,"  graphic symbol",(0,a.kt)("br",null)),(0,a.kt)("p",null,'         "',(0,a.kt)("br",null),"         #",(0,a.kt)("br",null),"         &",(0,a.kt)("br",null),"         '",(0,a.kt)("br",null),"         (",(0,a.kt)("br",null),"         )",(0,a.kt)("br",null),"         *",(0,a.kt)("br",null),"         +",(0,a.kt)("br",null),"         ,",(0,a.kt)("br",null),"         \u2013",(0,a.kt)("br",null),"         .",(0,a.kt)("br",null),"         @",(0,a.kt)("br",null)),(0,a.kt)("p",null,"name",(0,a.kt)("br",null)),(0,a.kt)("p",null,"quotation mark",(0,a.kt)("br",null),"number sign",(0,a.kt)("br",null),"ampersand",(0,a.kt)("br",null),"apostrophe, tick",(0,a.kt)("br",null),"left parenthesis",(0,a.kt)("br",null),"right parenthesis",(0,a.kt)("br",null),"asterisk, multiply",(0,a.kt)("br",null),"plus sign",(0,a.kt)("br",null),"comma",(0,a.kt)("br",null),"hyphen-minus, minus",(0,a.kt)("br",null),"full stop, dot, point",(0,a.kt)("br",null),"commercial at, at sign",(0,a.kt)("br",null)),(0,a.kt)("p",null,"  graphic symbol",(0,a.kt)("br",null)),(0,a.kt)("p",null,"         :",(0,a.kt)("br",null),"         ;",(0,a.kt)("br",null),"         ","<",(0,a.kt)("br",null),"         =",(0,a.kt)("br",null),"         ",">",(0,a.kt)("br",null),"         _",(0,a.kt)("br",null),"         |",(0,a.kt)("br",null),"         /",(0,a.kt)("br",null),"         !",(0,a.kt)("br",null),"         %",(0,a.kt)("br",null),"         [",(0,a.kt)("br",null),"         ] ",(0,a.kt)("br",null)),(0,a.kt)("p",null,"name",(0,a.kt)("br",null)),(0,a.kt)("p",null,"colon",(0,a.kt)("br",null),"semicolon",(0,a.kt)("br",null),"less-than sign",(0,a.kt)("br",null),"equals sign",(0,a.kt)("br",null),"greater-than sign",(0,a.kt)("br",null),"low line, underline",(0,a.kt)("br",null),"vertical line",(0,a.kt)("br",null),"solidus, divide",(0,a.kt)("br",null),"exclamation point",(0,a.kt)("br",null),"percent sign",(0,a.kt)("br",null),"left square bracket",(0,a.kt)("br",null),"right square bracket ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"implementation-requirements"}),"Implementation Requirements"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"16/3"),(0,a.kt)(l.Z,{items:["AI05-0286-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"An Ada implementation shall accept Ada source code in UTF-8 encoding, with or without a BOM (see ",(0,a.kt)("a",{href:"../AA-A/AA-A.4#Subclause_A.4.11"},"A.4.11"),"), where every character is represented by its code point. The character pair CARRIAGE RETURN/LINE FEED (code points 16#0D# 16#0A#) signifies a single end of line (see ",(0,a.kt)("a",{href:"../AA-2/AA-2.2"},"2.2"),"); every other occurrence of a ",(0,a.kt)("code",null,"format_effector")," other than the character whose code point position is 16#09# (CHARACTER TABULATION) also signifies a single end of line.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"16.a/3"),(0,a.kt)(l.Z,{items:["AI05-0079-1","AI05-0286-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),'This is simply requiring that an Ada implementation be able to directly process the ACATS, which is provided in the described format. Note that files that only contain characters with code points in the first 128 (which is the majority of the ACATS) are represented in the same way in both UTF-8 and in "plain" string format. The ACATS includes a BOM in files that have any characters with code points greater than 127. Note that the BOM contains characters not legal in Ada source code, so an implementation can use that to automatically distinguish between files formatted as plain Latin-1 strings and UTF-8 with BOM.',(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"16.b/3"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"We allow line endings to be both represented as the pair CR LF (as in Windows and the ACATS), and as single ",(0,a.kt)("code",null,"format_effector")," characters (usually LF, as in Linux), in order that files created by standard tools on most operating systems will meet the standard format. We specify how many line endings each represent so that compilers use the same line numbering for standard source files.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"16.c/3"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"This requirement increases portability by having a format that is accepted by all Ada compilers. Note that implementations can support other source representations, including structured representations like a parse tree.",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"17/3"),(0,a.kt)(l.Z,{items:["AI95-00285-01","AI05-0266-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"The categories defined above, as well as case mapping and folding, may be based on an implementation-defined version of ISO/IEC 10646 (2003 edition or later). ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"17.b/3"),(0,a.kt)(r.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The exact categories, case mapping, and case folding chosen affects identifiers, the result of '[[Wide_]Wide_]Image, and packages Wide_Characters.Handling and Wide_Wide_Characters.Handling. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"17.c/3"),(0,a.kt)(r.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"This permission allows implementations to upgrade to using a newer character set standard whenever that makes sense, rather than having to wait for the next Ada Standard. But the character set standard used cannot be older than ISO/IEC 10646:2003 (which is essentially similar to Unicode 4.0). ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"18/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE   The characters in categories ",(0,a.kt)("code",null,"other_control"),", ",(0,a.kt)("code",null,"other_private_use"),", and ",(0,a.kt)("code",null,"other_surrogate")," are only allowed in comments.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"19.a/3"),(0,a.kt)(l.Z,{items:["AI05-0286-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,a.kt)("em",null,"This paragraph was deleted."),(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"19.b"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Ada 95 allows 8-bit and 16-bit characters, as well as implementation-specified character sets. ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"19.c/3"),(0,a.kt)(l.Z,{items:["AI95-00285-01","AI05-0299-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The syntax rules in this subclause are modified to remove the emphasis on basic characters vs. others. (In this day and age, there is no need to point out that you can write programs without using (for example) lower case letters.) In particular, ",(0,a.kt)("code",null,"character")," (representing all characters usable outside comments) is added, and ",(0,a.kt)("code",null,"basic_graphic_character"),", ",(0,a.kt)("code",null,"other_special_character"),", and ",(0,a.kt)("code",null,"basic_character")," are removed. ",(0,a.kt)("code",null,"Special_character")," is expanded to include Ada 83's ",(0,a.kt)("code",null,"other_special_character"),", as well as new 8-bit characters not present in Ada 83. Ada 2005 removes ",(0,a.kt)("code",null,"special_character")," altogether; we want to stick to ISO/IEC 10646:2003 character classifications. Note that the term \u201cbasic letter\u201d is used in ",(0,a.kt)("a",{href:"../AA-A/AA-A.3"},"A.3"),", \u201c",(0,a.kt)("a",{href:"../AA-A/AA-A.3"},"Character Handling"),"\u201d to refer to letters without diacritical marks.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"19.d/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Character names now come from ISO/IEC 10646:2003.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"19.e/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,a.kt)("em",null,"This paragraph was deleted."),(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"extensions-to-ada-95"}),"Extensions to Ada 95"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"19.f/2"),(0,a.kt)(l.Z,{items:["AI95-00285-01","AI95-00395-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Program text can use most characters defined by ISO-10646:2003. This subclause has been rewritten to use the categories defined in that Standard. This should ease programming in languages other than English. ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"inconsistencies-with-ada-2005"}),"Inconsistencies With Ada 2005"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"19.g/3"),(0,a.kt)(l.Z,{items:["AI05-0266-1","AI05-0299-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"An implementation is allowed (but not required) to use a newer character set standard to determine the categories, case mapping, and case folding. Doing so will change the results of attributes '[[Wide_]Wide_]Image and the packages [Wide_]Wide_Characters.Handling in the case of a few rarely used characters. (This also could make some identifiers illegal, for characters that are no longer classified as letters.) This is unlikely to be a problem in practice. Moreover, truly portable Ada 2012 programs should avoid using in these contexts any characters that would have different classifications in any character set standards issued since 10646:2003 (since the compiler can use any such standard as the basis for its classifications). ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"19.h/3"),(0,a.kt)(l.Z,{items:["AI05-0079-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"correction",title:"Correction:",mdxType:"Admonition"},(0,a.kt)("strong",null)," Clarified that only characters in the categories defined here are allowed in the source of an Ada program. This was clear in Ada 95, but Amendment 1 dropped the wording instead of correcting it.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"19.i/3"),(0,a.kt)(l.Z,{items:["AI05-0286-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},'A standard source representation is defined that all compilers are expected to process. Since this is the same format as the ACATS, it seems unlikely that there are any implementations that don\'t meet this requirement. Moreover, other representations are still permitted, and the "impossible or impractical" loophole (see ',(0,a.kt)("a",{href:"../AA-1/AA-1.1#Subclause_1.1.3"},"1.1.3"),") can be invoked for any implementations that cannot directly process the ACATS. ",(0,a.kt)("br",null)),(0,a.kt)("h4",k({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"19.j/5"),(0,a.kt)(l.Z,{items:["AI12-0004-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"correction",title:"Correction:",mdxType:"Admonition"},(0,a.kt)("strong",null)," The interpretation of Ada source that is in Normalization Form C but not in Normalization Form KC is no longer implementation-defined. This change could potentially change the meaning of a program for a compiler that normalized all program source to Normalization Form KC before processing it. We don't document this as an inconsistency as such handling was previously implementation defined (so any such code was already defined to be not portable), and we're not aware of any compiler that normalized source code (so we don't expect to see this problem in the real world). ",(0,a.kt)("br",null)))}x.isMDXComponent=!0}}]);