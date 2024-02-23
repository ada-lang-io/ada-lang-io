"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2098],{81112:(e,n,a)=>{a.r(n),a.d(n,{assets:()=>p,contentTitle:()=>y,default:()=>h,frontMatter:()=>d,metadata:()=>g,toc:()=>m});var t=a(58168),o=(a(96540),a(15680)),i=a(20793),r=a(91435),s=(a(21432),a(79162)),l=a(34421);const d={sidebar_position:106},y="13.2 Packed Types",g={unversionedId:"arm/AA-13/AA-13.2",id:"arm/AA-13/AA-13.2",title:"13.2 Packed Types",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-13/AA-13.2.mdx",sourceDirName:"arm/AA-13",slug:"/arm/AA-13/AA-13.2",permalink:"/docs/arm/AA-13/AA-13.2",draft:!1,tags:[],version:"current",sidebarPosition:106,frontMatter:{sidebar_position:106},sidebar:"referenceManualSidebar",previous:{title:"13.1 Operational and Representation Aspects",permalink:"/docs/arm/AA-13/AA-13.1"},next:{title:"13.3 Operational and Representation Attributes",permalink:"/docs/arm/AA-13/AA-13.3"}},p={},m=[{value:"Language Design Principles",id:"language-design-principles",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Advice",id:"implementation-advice",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],c={toc:m},A="wrapper";function h(e){let{components:n,...a}=e;return(0,o.yg)(A,(0,t.A)({},c,a,{components:n,mdxType:"MDXLayout"}),(0,o.yg)("h1",{id:"132-packed-types"},"13.2 Packed Types"),(0,o.yg)("admonition",{type:"warning"},(0,o.yg)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,o.yg)("a",{parentName:"p",href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"},"tracking issue"))),(0,o.yg)(s.A,{mdxType:"MarginText"},"1/3"),(0,o.yg)(l.A,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),(0,o.yg)("p",null,"[The Pack aspect having the value True specifies that storage minimization should be the main criterion when selecting the representation of a composite type.] ",(0,o.yg)("br",null)),(0,o.yg)("p",null,(0,o.yg)("em",null,"Paragraphs 2 through 4 were moved to ",(0,o.yg)("a",{href:"../AA-J/"},"Annex J"),", \u201c",(0,o.yg)("a",{href:"../AA-J/"},"Obsolescent Features"),"\u201d.")," ",(0,o.yg)("br",null)),(0,o.yg)("h4",{id:"language-design-principles"},"Language Design Principles"),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"1.a/4"),(0,o.yg)(l.A,{items:["AI12-0001-1"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"If the default representation already uses minimal storage for a particular type, aspect Pack might not cause any representation change. It follows that aspect Pack should always be allowed, even when it has no effect on representation.",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"1.b/4"),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"As a consequence, the chosen representation for a packed type may change during program maintenance even if the type is unchanged (in particular, if other representation aspects change on a part of the type). This is different than the behavior of most other representation aspects, whose properties remain guaranteed no matter what changes are made to other aspects.",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"1.c/4"),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Therefore, aspect Pack should not be used to achieve a representation required by external criteria. For instance, setting Component","_","Size to 1 should be preferred over using aspect Pack to ensure an array of bits. If future maintenance would make the array components aliased, independent, or atomic, the program would become illegal if Component","_","Size is used (immediately identifying a problem) while the aspect Pack version would simply change representations (probably causing a hard-to-find bug). ",(0,o.yg)("br",null))),(0,o.yg)("h4",{id:"static-semantics"},"Static Semantics"),(0,o.yg)(s.A,{mdxType:"MarginText"},"5/3"),(0,o.yg)(l.A,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),(0,o.yg)("p",null,"For a full type declaration of a composite type, the following language-defined representation aspect may be specified:",(0,o.yg)("br",null)),(0,o.yg)(s.A,{mdxType:"MarginText"},"5.1/3"),(0,o.yg)("dt",null,(0,o.yg)("br",null),"Pack"),(0,o.yg)("dl",null,(0,o.yg)("dd",null,"The type of aspect Pack is Boolean. When aspect Pack is True for a type, the type (or the extension part) is said to be ",(0,o.yg)("em",null,"packed"),". For a type extension, the parent part is packed as for the parent type, and specifying Pack causes packing only of the extension part. ",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"5.a/3"),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,o.yg)("strong",null,"Aspect Description for "),(0,o.yg)("strong",null,"Pack: "),"Minimize storage when laying out records and arrays.",(0,o.yg)("br",null))),(0,o.yg)(s.A,{mdxType:"MarginText"},"5.2/3"),(0,o.yg)("dl",null,(0,o.yg)("dd",null,"If directly specified, the ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-13/AA-13.1#S0348"},"aspect_definition"))," shall be a static expression. If not specified (including by inheritance), the aspect is False. ",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"5.b/3"),(0,o.yg)(l.A,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,o.yg)("strong",null),"The only high level semantic effect of specifying the Pack aspect is potential loss of independent addressability (see ",(0,o.yg)("a",{href:"../AA-9/AA-9.10"},"9.10"),", \u201c",(0,o.yg)("a",{href:"../AA-9/AA-9.10"},"Shared Variables"),"\u201d).] ",(0,o.yg)("br",null))),(0,o.yg)("h4",{id:"implementation-advice"},"Implementation Advice"),(0,o.yg)(s.A,{mdxType:"MarginText"},"6"),(0,o.yg)("p",null,"If a type is packed, then the implementation should try to minimize storage allocated to objects of the type, possibly at the expense of speed of accessing components, subject to reasonable complexity in addressing calculations. ",(0,o.yg)("br",null)),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"6.a.1/2"),(0,o.yg)(i.A,{type:"aarm",aarm:"implementation-advice",mdxType:"Admonition"},(0,o.yg)("strong",null),"Storage allocated to objects of a packed type should be minimized.",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"6.a/3"),(0,o.yg)(l.A,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,o.yg)("strong",null),"Specifying the Pack aspect is for gaining space efficiency, possibly at the expense of time. If more explicit control over representation is desired, then a ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-13/AA-13.5#S0352"},"record_representation_clause")),", a Component","_","Size clause, or a Size clause should be used instead of, or in addition to, the Pack aspect. ",(0,o.yg)("br",null))),(0,o.yg)(s.A,{mdxType:"MarginText"},"6.1/4"),(0,o.yg)(l.A,{items:["AI95-00291-02","AI12-0001-1"],mdxType:"MarginInfo"}),(0,o.yg)("p",null,(0,o.yg)("em",null,"This paragraph was deleted."),(0,o.yg)("br",null)),(0,o.yg)(s.A,{mdxType:"MarginText"},"7/3"),(0,o.yg)(l.A,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),(0,o.yg)("p",null,"The recommended level of support for the Pack aspect is: ",(0,o.yg)("br",null)),(0,o.yg)(s.A,{mdxType:"MarginText"},"7.1/4"),(0,o.yg)(l.A,{items:["AI12-0001-1"],mdxType:"MarginInfo"}),(0,o.yg)("ul",null,(0,o.yg)("li",null,"Any component of a packed type that is of a by-reference type, that is specified as independently addressable, or that contains an aliased part, shall be aligned according to the alignment of its subtype.",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"7.a/4"),(0,o.yg)(i.A,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,o.yg)("strong",null),'This also applies to atomic components. "Atomic" implies "specified as independently addressable", so we don\'t need to mention atomic here.',(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"7.b/4"),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Other components do not have to respect the alignment of the subtype when packed; in many cases, the Recommended Level of Support will require the alignment to be ignored. ",(0,o.yg)("br",null))),(0,o.yg)(s.A,{mdxType:"MarginText"},"8/5"),(0,o.yg)(l.A,{items:["AI12-0001-1","AI12-0444-1"],mdxType:"MarginInfo"}),(0,o.yg)("ul",null,(0,o.yg)("li",null,"For a packed record type, the components should be packed as tightly as possible subject to the above alignment requirements, the Sizes of the component subtypes, and any ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-13/AA-13.5#S0352"},"record_representation_clause"))," that applies to the type; the implementation is allowed to reorder components or cross aligned word boundaries to improve the packing. A component whose Size is greater than the word size may be allocated an integral number of words.",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"8.a"),(0,o.yg)(i.A,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,o.yg)("strong",null),"The implementation can always allocate an integral number of words for a component that will not fit in a word. The rule also allows small component sizes to be rounded up if such rounding does not waste space. For example, if Storage","_","Unit = 8, then a component of size 8 is probably more efficient than a component of size 7 plus a 1-bit gap (assuming the gap is needed anyway). ",(0,o.yg)("br",null))),(0,o.yg)(s.A,{mdxType:"MarginText"},"9/4"),(0,o.yg)(l.A,{items:["AI05-0009-1","AI12-0001-1"],mdxType:"MarginInfo"}),(0,o.yg)("ul",null,(0,o.yg)("li",null,"For a packed array type, if the Size of the component subtype is less than or equal to the word size, Component","_","Size should be less than or equal to the Size of the component subtype, rounded up to the nearest factor of the word size, unless this would violate the above alignment requirements.",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"9.a/4"),(0,o.yg)(l.A,{items:["AI12-0001-1"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,o.yg)("em",null,"This paragraph was deleted."),(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"9.b/3"),(0,o.yg)(i.A,{type:"aarm",aarm:"implementation-advice",mdxType:"Admonition"},(0,o.yg)("strong",null),"The recommended level of support for the Pack aspect should be followed.",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)("h4",{id:"wording-changes-from-ada-95"},"Wording Changes from Ada 95")),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"9.c/3"),(0,o.yg)(l.A,{items:["AI95-00291-02","AI05-0229-1"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Added clarification that the Pack aspect can ignore alignment requirements on types that don't have by-reference or aliased parts. This was always intended, but there was no wording to that effect. ",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)("h4",{id:"extensions-to-ada-2005"},"Extensions to Ada 2005")),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"9.d/3"),(0,o.yg)(l.A,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Aspect Pack is new; ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-2/AA-2.8#S0019"},"pragma"))," Pack is now obsolescent. ",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)("h4",{id:"wording-changes-from-ada-2005"},"Wording Changes from Ada 2005")),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"9.e/3"),(0,o.yg)(l.A,{items:["AI05-0009-1"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"correction",mdxType:"Admonition"},(0,o.yg)("strong",null)," Fixed so that the presence or absence of a confirming Component","_","Size representation clause does not change the meaning of the Pack aspect. ",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)("h4",{id:"wording-changes-from-ada-2012"},"Wording Changes from Ada 2012")),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(s.A,{mdxType:"MarginText"},"9.f/4"),(0,o.yg)(l.A,{items:["AI12-0001-1"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,o.yg)("strong",null,"Corrigendum:")," Fixed so that the Recommended Level of Support does not require packing of components for which such packing would violate other representation items or aspects. This is not incompatible, as either such Pack aspects were treated as illegal or the Recommended Level of Support was ignored as impractical, neither of which would change the behavior of any working programs. (Other behavior cannot be justifed from the Reference Manual.) ",(0,o.yg)("br",null))))}h.isMDXComponent=!0}}]);