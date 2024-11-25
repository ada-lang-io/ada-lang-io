"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2098],{42752:(e,n,s)=>{s.r(n),s.d(n,{assets:()=>p,contentTitle:()=>h,default:()=>j,frontMatter:()=>l,metadata:()=>a,toc:()=>m});const a=JSON.parse('{"id":"arm/AA-13/AA-13.2","title":"13.2 Packed Types","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-13/AA-13.2.mdx","sourceDirName":"arm/AA-13","slug":"/arm/AA-13/AA-13.2","permalink":"/docs/arm/AA-13/AA-13.2","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":106,"frontMatter":{"sidebar_position":106},"sidebar":"referenceManualSidebar","previous":{"title":"13.1 Operational and Representation Aspects","permalink":"/docs/arm/AA-13/AA-13.1"},"next":{"title":"13.3 Operational and Representation Attributes","permalink":"/docs/arm/AA-13/AA-13.3"}}');var i=s(74848),r=s(28453),t=s(13842),o=s(91435),c=(s(21432),s(79162)),d=s(34421);const l={sidebar_position:106},h="13.2 Packed Types",p={},m=[{value:"Language Design Principles",id:"language-design-principles",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Advice",id:"implementation-advice",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}];function x(e){const n={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,r.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(n.header,{children:(0,i.jsx)(n.h1,{id:"132-packed-types",children:"13.2 Packed Types"})}),"\n",(0,i.jsx)(n.admonition,{type:"danger",children:(0,i.jsxs)(n.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,i.jsx)(n.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,i.jsx)(c.A,{children:"1/3"}),"\n",(0,i.jsx)(d.A,{items:["AI05-0229-1"]}),"\n",(0,i.jsxs)("p",{children:["[The Pack aspect having the value True specifies that storage minimization should be the main criterion when selecting the representation of a composite type.] ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsxs)("p",{children:[(0,i.jsxs)("em",{children:["Paragraphs 2 through 4 were moved to ",(0,i.jsx)("a",{href:"/docs/arm/AA-J/",children:"Annex J"}),", \u201c",(0,i.jsx)("a",{href:"/docs/arm/AA-J/",children:"Obsolescent Features"}),"\u201d."]})," ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(n.h4,{id:"language-design-principles",children:"Language Design Principles"}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"1.a/4"}),(0,i.jsx)(d.A,{items:["AI12-0001-1"]}),(0,i.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["If the default representation already uses minimal storage for a particular type, aspect Pack might not cause any representation change. It follows that aspect Pack should always be allowed, even when it has no effect on representation.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"1.b/4"}),(0,i.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["As a consequence, the chosen representation for a packed type may change during program maintenance even if the type is unchanged (in particular, if other representation aspects change on a part of the type). This is different than the behavior of most other representation aspects, whose properties remain guaranteed no matter what changes are made to other aspects.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"1.c/4"}),(0,i.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["Therefore, aspect Pack should not be used to achieve a representation required by external criteria. For instance, setting Component","_","Size to 1 should be preferred over using aspect Pack to ensure an array of bits. If future maintenance would make the array components aliased, independent, or atomic, the program would become illegal if Component","_","Size is used (immediately identifying a problem) while the aspect Pack version would simply change representations (probably causing a hard-to-find bug). ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(n.h4,{id:"static-semantics",children:"Static Semantics"}),"\n",(0,i.jsx)(c.A,{children:"5/3"}),"\n",(0,i.jsx)(d.A,{items:["AI05-0229-1"]}),"\n",(0,i.jsxs)("p",{children:["For a full type declaration of a composite type, the following language-defined representation aspect may be specified:",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(c.A,{children:"5.1/3"}),"\n",(0,i.jsxs)("dt",{children:[(0,i.jsx)("br",{}),"Pack"]}),"\n",(0,i.jsx)("dl",{children:(0,i.jsxs)("dd",{children:["The type of aspect Pack is Boolean. When aspect Pack is True for a type, the type (or the extension part) is said to be ",(0,i.jsx)("em",{children:"packed"}),". For a type extension, the parent part is packed as for the parent type, and specifying Pack causes packing only of the extension part. ",(0,i.jsx)("br",{})]})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"5.a/3"}),(0,i.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{children:"Aspect Description for "}),(0,i.jsx)("strong",{children:"Pack: "}),"Minimize storage when laying out records and arrays.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(c.A,{children:"5.2/3"}),"\n",(0,i.jsx)("dl",{children:(0,i.jsxs)("dd",{children:["If directly specified, the ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0348",children:"aspect_definition"})})," shall be a static expression. If not specified (including by inheritance), the aspect is False. ",(0,i.jsx)("br",{})]})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"5.b/3"}),(0,i.jsx)(d.A,{items:["AI05-0229-1"]}),(0,i.jsx)(t.A,{type:"aarm",aarm:"ramification",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"The only high level semantic effect of specifying the Pack aspect is potential loss of independent addressability (see ",(0,i.jsx)("a",{href:"/docs/arm/AA-9/AA-9.10",children:"9.10"}),", \u201c",(0,i.jsx)("a",{href:"/docs/arm/AA-9/AA-9.10",children:"Shared Variables"}),"\u201d).] ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(n.h4,{id:"implementation-advice",children:"Implementation Advice"}),"\n",(0,i.jsx)(c.A,{children:"6"}),"\n",(0,i.jsxs)("p",{children:["If a type is packed, then the implementation should try to minimize storage allocated to objects of the type, possibly at the expense of speed of accessing components, subject to reasonable complexity in addressing calculations. ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"6.a.1/2"}),(0,i.jsx)(t.A,{type:"aarm",aarm:"implementation-advice",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"Storage allocated to objects of a packed type should be minimized.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"6.a/3"}),(0,i.jsx)(d.A,{items:["AI05-0229-1"]}),(0,i.jsx)(t.A,{type:"aarm",aarm:"ramification",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"Specifying the Pack aspect is for gaining space efficiency, possibly at the expense of time. If more explicit control over representation is desired, then a ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-13/AA-13.5#S0352",children:"record_representation_clause"})}),", a Component","_","Size clause, or a Size clause should be used instead of, or in addition to, the Pack aspect. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(c.A,{children:"6.1/4"}),"\n",(0,i.jsx)(d.A,{items:["AI95-00291-02","AI12-0001-1"]}),"\n",(0,i.jsxs)("p",{children:[(0,i.jsx)("em",{children:"This paragraph was deleted."}),(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(c.A,{children:"7/3"}),"\n",(0,i.jsx)(d.A,{items:["AI05-0229-1"]}),"\n",(0,i.jsxs)("p",{children:["The recommended level of support for the Pack aspect is: ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsx)(c.A,{children:"7.1/4"}),"\n",(0,i.jsx)(d.A,{items:["AI12-0001-1"]}),"\n",(0,i.jsx)("ul",{children:(0,i.jsxs)("li",{children:["Any component of a packed type that is of a by-reference type, that is specified as independently addressable, or that contains an aliased part, shall be aligned according to the alignment of its subtype.",(0,i.jsx)("br",{})]})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"7.a/4"}),(0,i.jsx)(t.A,{type:"aarm",aarm:"ramification",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),'This also applies to atomic components. "Atomic" implies "specified as independently addressable", so we don\'t need to mention atomic here.',(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"7.b/4"}),(0,i.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["Other components do not have to respect the alignment of the subtype when packed; in many cases, the Recommended Level of Support will require the alignment to be ignored. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(c.A,{children:"8/5"}),"\n",(0,i.jsx)(d.A,{items:["AI12-0001-1","AI12-0444-1"]}),"\n",(0,i.jsx)("ul",{children:(0,i.jsxs)("li",{children:["For a packed record type, the components should be packed as tightly as possible subject to the above alignment requirements, the Sizes of the component subtypes, and any ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-13/AA-13.5#S0352",children:"record_representation_clause"})})," that applies to the type; the implementation is allowed to reorder components or cross aligned word boundaries to improve the packing. A component whose Size is greater than the word size may be allocated an integral number of words.",(0,i.jsx)("br",{})]})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"8.a"}),(0,i.jsx)(t.A,{type:"aarm",aarm:"ramification",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"The implementation can always allocate an integral number of words for a component that will not fit in a word. The rule also allows small component sizes to be rounded up if such rounding does not waste space. For example, if Storage","_","Unit = 8, then a component of size 8 is probably more efficient than a component of size 7 plus a 1-bit gap (assuming the gap is needed anyway). ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(c.A,{children:"9/4"}),"\n",(0,i.jsx)(d.A,{items:["AI05-0009-1","AI12-0001-1"]}),"\n",(0,i.jsx)("ul",{children:(0,i.jsxs)("li",{children:["For a packed array type, if the Size of the component subtype is less than or equal to the word size, Component","_","Size should be less than or equal to the Size of the component subtype, rounded up to the nearest factor of the word size, unless this would violate the above alignment requirements.",(0,i.jsx)("br",{})]})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"9.a/4"}),(0,i.jsx)(d.A,{items:["AI12-0001-1"]}),(0,i.jsxs)(t.A,{type:"aarm",aarm:"note",children:[(0,i.jsx)("em",{children:"This paragraph was deleted."}),(0,i.jsx)("br",{})]})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"9.b/3"}),(0,i.jsx)(t.A,{type:"aarm",aarm:"implementation-advice",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"The recommended level of support for the Pack aspect should be followed.",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(o.A,{children:(0,i.jsx)(n.h4,{id:"wording-changes-from-ada-95",children:"Wording Changes from Ada 95"})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"9.c/3"}),(0,i.jsx)(d.A,{items:["AI95-00291-02","AI05-0229-1"]}),(0,i.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["Added clarification that the Pack aspect can ignore alignment requirements on types that don't have by-reference or aliased parts. This was always intended, but there was no wording to that effect. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(o.A,{children:(0,i.jsx)(n.h4,{id:"extensions-to-ada-2005",children:"Extensions to Ada 2005"})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"9.d/3"}),(0,i.jsx)(d.A,{items:["AI05-0229-1"]}),(0,i.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["Aspect Pack is new; ",(0,i.jsx)("code",{children:(0,i.jsx)("a",{href:"/docs/arm/AA-2/AA-2.8#S0019",children:"pragma"})})," Pack is now obsolescent. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(o.A,{children:(0,i.jsx)(n.h4,{id:"wording-changes-from-ada-2005",children:"Wording Changes from Ada 2005"})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"9.e/3"}),(0,i.jsx)(d.A,{items:["AI05-0009-1"]}),(0,i.jsx)(t.A,{type:"aarm",aarm:"correction",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{})," Fixed so that the presence or absence of a confirming Component","_","Size representation clause does not change the meaning of the Pack aspect. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(o.A,{children:(0,i.jsx)(n.h4,{id:"wording-changes-from-ada-2012",children:"Wording Changes from Ada 2012"})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(c.A,{children:"9.f/4"}),(0,i.jsx)(d.A,{items:["AI12-0001-1"]}),(0,i.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{children:"Corrigendum:"})," Fixed so that the Recommended Level of Support does not require packing of components for which such packing would violate other representation items or aspects. This is not incompatible, as either such Pack aspects were treated as illegal or the Recommended Level of Support was ignored as impractical, neither of which would change the behavior of any working programs. (Other behavior cannot be justifed from the Reference Manual.) ",(0,i.jsx)("br",{})]})})]})]})}function j(e={}){const{wrapper:n}={...(0,r.R)(),...e.components};return n?(0,i.jsx)(n,{...e,children:(0,i.jsx)(x,{...e})}):x(e)}}}]);