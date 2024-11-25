"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8525],{88500:(e,r,s)=>{s.r(r),s.d(r,{assets:()=>x,contentTitle:()=>A,default:()=>p,frontMatter:()=>h,metadata:()=>a,toc:()=>j});const a=JSON.parse('{"id":"arm/AA-8/AA-8.1","title":"8.1 Declarative Region","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-8/AA-8.1.mdx","sourceDirName":"arm/AA-8","slug":"/arm/AA-8/AA-8.1","permalink":"/docs/arm/AA-8/AA-8.1","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":67,"frontMatter":{"sidebar_position":67},"sidebar":"referenceManualSidebar","previous":{"title":"8 Visibility Rules","permalink":"/docs/arm/AA-8/"},"next":{"title":"8.2 Scope of Declarations","permalink":"/docs/arm/AA-8/AA-8.2"}}');var n=s(74848),i=s(28453),t=s(13842),d=s(91435),c=s(21432),o=s(79162),l=s(34421);const h={sidebar_position:67},A="8.1 Declarative Region",x={},j=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Extensions to Ada 2012",id:"extensions-to-ada-2012",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}];function m(e){const r={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,i.R)(),...e.components};return(0,n.jsxs)(n.Fragment,{children:[(0,n.jsx)(r.header,{children:(0,n.jsx)(r.h1,{id:"81-declarative-region",children:"8.1 Declarative Region"})}),"\n",(0,n.jsx)(r.admonition,{type:"danger",children:(0,n.jsxs)(r.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,n.jsx)(r.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,n.jsx)(r.h4,{id:"static-semantics",children:"Static Semantics"}),"\n",(0,n.jsx)(o.A,{children:"1"}),"\n",(0,n.jsxs)("p",{children:["For each of the following constructs, there is a portion of the program text called its ",(0,n.jsx)("em",{children:"declarative region"}),", [within which nested declarations can occur]: ",(0,n.jsx)("br",{})]}),"\n",(0,n.jsx)(o.A,{children:"2"}),"\n",(0,n.jsxs)("ul",{children:[(0,n.jsxs)("li",{children:["any declaration, other than that of an enumeration type, that is not a completion [of a previous declaration];",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"2.1/4"}),(0,n.jsx)(l.A,{items:["AI12-0094-1"]}),(0,n.jsxs)("li",{children:["an ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.10#S0084",children:"access_definition"})}),";",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"2.2/5"}),(0,n.jsx)(l.A,{items:["AI12-0061-1"]}),(0,n.jsxs)("li",{children:["an ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.3#S0119",children:"iterated_component_association"})}),";",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"2.3/5"}),(0,n.jsx)(l.A,{items:["AI12-0308-1"]}),(0,n.jsxs)("li",{children:["an ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.3#S0131",children:"iterated_element_association"})}),";",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"2.4/5"}),(0,n.jsx)(l.A,{items:["AI05-0255-1","AI12-0308-1"]}),(0,n.jsxs)("li",{children:["a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0153",children:"quantified_expression"})}),";",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"2.5/5"}),(0,n.jsx)(l.A,{items:["AI12-0236-1"]}),(0,n.jsxs)("li",{children:["a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0156",children:"declare_expression"})}),";",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"3"}),(0,n.jsxs)("li",{children:["a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-5/AA-5.6#S0191",children:"block_statement"})}),";",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"4"}),(0,n.jsxs)("li",{children:["a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-5/AA-5.5#S0178",children:"loop_statement"})}),";",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"4.1/5"}),(0,n.jsx)(l.A,{items:["AI05-0255-1","AI12-0308-1"]}),(0,n.jsxs)("li",{children:[(0,n.jsx)("em",{children:"This paragraph was deleted."}),(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"4.2/3"}),(0,n.jsx)(l.A,{items:["AI95-00318-02"]}),(0,n.jsxs)("li",{children:["an ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-6/AA-6.5#S0225",children:"extended_return_statement"})}),";",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"5"}),(0,n.jsxs)("li",{children:["an ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0258",children:"accept_statement"})}),";",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"6"}),(0,n.jsxs)("li",{children:["an ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-11/AA-11.2#S0305",children:"exception_handler"})}),". ",(0,n.jsx)("br",{})]})]}),"\n",(0,n.jsx)(o.A,{children:"7"}),"\n",(0,n.jsxs)("p",{children:["The declarative region includes the text of the construct together with additional text determined [(recursively)], as follows: ",(0,n.jsx)("br",{})]}),"\n",(0,n.jsx)(o.A,{children:"8"}),"\n",(0,n.jsxs)("ul",{children:[(0,n.jsxs)("li",{children:["If a declaration is included, so is its completion, if any.",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"9"}),(0,n.jsxs)("li",{children:["If the declaration of a library unit [(including Standard \u2014 see ",(0,n.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#Subclause_10.1.1",children:"10.1.1"}),")] is included, so are the declarations of any child units [(and their completions, by the previous rule)]. The child declarations occur after the declaration.",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"10"}),(0,n.jsxs)("li",{children:["If a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#S0297",children:"body_stub"})})," is included, so is the corresponding ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#S0302",children:"subunit"})}),".",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"11"}),(0,n.jsxs)("li",{children:["If a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0023",children:"type_declaration"})})," is included, then so is a corresponding ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-13/AA-13.5#S0352",children:"record_representation_clause"})}),", if any. ",(0,n.jsx)("br",{})]})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"11.a"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"reason",children:(0,n.jsxs)(r.p,{children:[(0,n.jsx)("strong",{}),"This is so that the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.8#S0070",children:"component_declaration"})}),"s can be directly visible in the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-13/AA-13.5#S0352",children:"record_representation_clause"})}),". ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(o.A,{children:"12"}),"\n",(0,n.jsxs)("p",{children:["The declarative region of a declaration is also called the ",(0,n.jsx)("em",{children:"declarative region"})," of any view or entity declared by the declaration. ",(0,n.jsx)("br",{})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"12.a"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"reason",children:(0,n.jsxs)(r.p,{children:[(0,n.jsx)("strong",{}),"The constructs that have declarative regions are the constructs that can have declarations nested inside them. Nested declarations are declared in that declarative region. The one exception is for enumeration literals; although they are nested inside an enumeration type declaration, they behave as if they were declared at the same level as the type. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"12.b"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:[(0,n.jsx)("strong",{children:"To be honest: "}),"A declarative region does not include ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#S0291",children:"parent_unit_name"})}),"s. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"12.c"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"ramification",children:(0,n.jsxs)(r.p,{children:[(0,n.jsx)("strong",{}),"A declarative region does not include ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#S0292",children:"context_clause"})}),"s. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(o.A,{children:"13"}),"\n",(0,n.jsxs)("p",{children:["A declaration occurs ",(0,n.jsx)("em",{children:"immediately within"})," a declarative region if this region is the innermost declarative region that encloses the declaration (the ",(0,n.jsx)("em",{children:"immediately enclosing"})," declarative region), not counting the declarative region (if any) associated with the declaration itself. ",(0,n.jsx)("br",{})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"13.a"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"discussion",children:(0,n.jsxs)(r.p,{children:[(0,n.jsx)("strong",{}),"Don't confuse the declarative region of a declaration with the declarative region in which it immediately occurs. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(o.A,{children:"14"}),"\n",(0,n.jsxs)("p",{children:["[ A declaration is ",(0,n.jsx)("em",{children:"local"})," to a declarative region if the declaration occurs immediately within the declarative region.] [An entity is ",(0,n.jsx)("em",{children:"local"})," to a declarative region if the entity is declared by a declaration that is local to the declarative region.] ",(0,n.jsx)("br",{})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"14.a"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"ramification",children:(0,n.jsxs)(r.p,{children:[(0,n.jsx)("strong",{}),'"Occurs immediately within" and "local to" are synonyms (when referring to declarations).',(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"14.b"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:['Thus, \u201clocal to\u201d applies to both declarations and entities, whereas \u201coccurs immediately within\u201d only applies to declarations. We use this term only informally; for cases where precision is required, we use the term "occurs immediately within", since it is less likely to cause confusion. ',(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(o.A,{children:"15"}),"\n",(0,n.jsxs)("p",{children:["A declaration is ",(0,n.jsx)("em",{children:"global"})," to a declarative region if the declaration occurs immediately within another declarative region that encloses the declarative region. An entity is ",(0,n.jsx)("em",{children:"global"})," to a declarative region if the entity is declared by a declaration that is global to the declarative region. ",(0,n.jsx)("br",{})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"16"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:['NOTE 1   The children of a parent library unit are inside the parent\'s declarative region, even though they do not occur inside the parent\'s declaration or body. This implies that one can use (for example) "P.Q" to refer to a child of P whose defining name is Q, and that after "',(0,n.jsx)("strong",{children:"use"}),' P;" Q can refer (directly) to that child.',(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"17"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:["NOTE 2   As explained above and in ",(0,n.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#Subclause_10.1.1",children:"10.1.1"}),", \u201c",(0,n.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#Subclause_10.1.1",children:"Compilation Units - Library Units"}),"\u201d, all library units are descendants of Standard, and so are contained in the declarative region of Standard. They are ",(0,n.jsx)("em",{children:"not"})," inside the declaration or body of Standard, but they ",(0,n.jsx)("em",{children:"are"})," inside its declarative region.",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18/5"}),(0,n.jsx)(l.A,{items:["AI12-0442-1"]}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:["NOTE 3   For a declarative region that comes in multiple parts, the text of the declarative region does not include any of the text that appears between the parts. Thus, when a portion of a declarative region is said to extend from one place to another in the declarative region, the portion does not contain any of the text that appears between the parts of the declarative region. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18.a"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"discussion",children:(0,n.jsxs)(r.p,{children:[(0,n.jsx)("strong",{}),"It is necessary for the things that have a declarative region to include anything that contains declarations (except for enumeration type declarations). This includes any declaration that has a profile (that is, ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0195",children:"subprogram_declaration"})}),", ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-6/AA-6.3#S0216",children:"subprogram_body"})}),", ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0257",children:"entry_declaration"})}),", ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-8/AA-8.5#S0242",children:"subprogram_renaming_declaration"})}),", ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-12/AA-12.6#S0335",children:"formal_subprogram_declaration"})}),", access-to-subprogram ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0023",children:"type_declaration"})}),"), anything that has a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.7#S0059",children:"discriminant_part"})})," (that is, various kinds of ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0023",children:"type_declaration"})}),"), anything that has a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.8#S0068",children:"component_list"})})," (that is, record ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0023",children:"type_declaration"})})," and record extension ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0023",children:"type_declaration"})}),"), and finally the declarations of task and protected units and packages. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(d.A,{children:(0,n.jsx)(r.h4,{id:"wording-changes-from-ada-83",children:"Wording Changes from Ada 83"})}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18.b"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:["It was necessary to extend Ada 83's definition of declarative region to take the following Ada 95 features into account: ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18.c"}),(0,n.jsxs)("ul",{children:[(0,n.jsxs)("li",{children:["Child library units.",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"18.d"}),(0,n.jsxs)("li",{children:["Derived types/type extensions \u2014 we need a declarative region for inherited components and also for new components.",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"18.e"}),(0,n.jsxs)("li",{children:["All the kinds of types that allow discriminants.",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"18.f"}),(0,n.jsxs)("li",{children:["Protected units.",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"18.g"}),(0,n.jsxs)("li",{children:["Entries that have bodies instead of accept statements.",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"18.h"}),(0,n.jsxs)("li",{children:["The ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-11/AA-11.2#S0306",children:"choice_parameter_specification"})})," of an ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-11/AA-11.2#S0305",children:"exception_handler"})}),".",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"18.i"}),(0,n.jsxs)("li",{children:["The formal parameters of access-to-subprogram types.",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"18.j"}),(0,n.jsxs)("li",{children:["Renamings-as-body. ",(0,n.jsx)("br",{})]})]})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18.k"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:["Discriminated and access-to-subprogram type declarations need a declarative region. Enumeration type declarations cannot have one, because you don't have to say \"Color.Red\" to refer to the literal Red of Color. For other type declarations, it doesn't really matter whether or not there is an associated declarative region, so for simplicity, we give one to all types except enumeration types.",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18.l"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:["We now say that an ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0258",children:"accept_statement"})})," has its own declarative region, rather than being part of the declarative region of the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0257",children:"entry_declaration"})}),', so that declarative regions are properly nested regions of text, so that it makes sense to talk about "inner declarative regions", and "...extends to the end of a declarative region". Inside an ',(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0258",children:"accept_statement"})}),", the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," of one of the parameters denotes the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0207",children:"parameter_specification"})})," of the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0258",children:"accept_statement"})}),", not that of the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0257",children:"entry_declaration"})}),". If the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0258",children:"accept_statement"})})," is nested within a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-5/AA-5.6#S0191",children:"block_statement"})}),", these ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0207",children:"parameter_specification"})}),"s can hide declarations of the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-5/AA-5.6#S0191",children:"block_statement"})}),". The semantics of such cases was unclear in RM83. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18.m"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:[(0,n.jsx)("strong",{children:"To be honest: "}),"Unfortunately, we have the same problem for the entry name itself \u2014 it should denote the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0258",children:"accept_statement"})}),", but ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0258",children:"accept_statement"})}),"s are not declarations. They should be, and they should hide the entry from all visibility within themselves. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18.n"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:["Note that we can't generalize this to ",(0,n.jsxs)("code",{children:["entry","_","bodies"]}),", or other bodies, because the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})})," of a body is not supposed to contain (explicit) homographs of things in the declaration. It works for ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0258",children:"accept_statement"})}),"s only because an ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0258",children:"accept_statement"})})," does not have a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})}),".",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18.o"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:["To avoid confusion, we use the term \u201clocal to\u201d only informally in Ada 95. Even RM83 used the term incorrectly (see, for example, RM83-12.3(13)).",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18.p"}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:["In Ada 83, (root) library units were inside Standard; it was not clear whether the declaration or body of Standard was meant. In Ada 95, they are children of Standard, and so occur immediately within Standard's declarative region, but not within either the declaration or the body. (See RM83-8.6(2) and RM83-10.1.1(5).) ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(d.A,{children:(0,n.jsx)(r.h4,{id:"wording-changes-from-ada-95",children:"Wording Changes from Ada 95"})}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18.q/2"}),(0,n.jsx)(l.A,{items:["AI95-00318-02"]}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:[(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-6/AA-6.5#S0225",children:"Extended_return_statement"})})," (see ",(0,n.jsx)("a",{href:"/docs/arm/AA-6/AA-6.5",children:"6.5"}),") is added to the list of constructs that have a declarative region. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(d.A,{children:(0,n.jsx)(r.h4,{id:"extensions-to-ada-2012",children:"Extensions to Ada 2012"})}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18.r/4"}),(0,n.jsx)(l.A,{items:["AI12-0094-1"]}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:[(0,n.jsx)("strong",{children:"Corrigendum:"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.10#S0084",children:"access_definition"})})," is added to the list of constructs that have a declarative region. This allows parameter names declared in anonymous access type subprogram types to be the same as other names declared outside. For instance: ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18.s/4"}),(0,n.jsx)(c.A,{language:"ada",children:(0,n.jsxs)(r.p,{children:["type Foo is record","\n","   A : Natural;","\n","   B : access procedure (A : Boolean);","\n","end record;","\n"]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18.t/5"}),(0,n.jsx)(l.A,{items:["AI12-0005-1"]}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:["This is now legal, as one would expect; it was illegal in previous versions of Ada as the parameter A and the component A were homographs in the same declarative region (see ",(0,n.jsx)("a",{href:"/docs/arm/AA-8/AA-8.3",children:"8.3"}),"). Note that some implementations already allow this common sense interpretation, so this extension might in fact already be used in existing code. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(d.A,{children:(0,n.jsx)(r.h4,{id:"wording-changes-from-ada-2012",children:"Wording Changes from Ada 2012"})}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"18.u/5"}),(0,n.jsx)(l.A,{items:["AI12-0061-1","AI12-0236-1","AI12-0308-1"]}),(0,n.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(r.p,{children:["Added ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.3#S0119",children:"iterated_component_association"})}),", ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.3#S0131",children:"iterated_element_association"})}),", and ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0156",children:"declare_expression"})})," to the rapidly expanding list of constructs that have a declarative region. ",(0,n.jsx)("br",{})]})})]})]})}function p(e={}){const{wrapper:r}={...(0,i.R)(),...e.components};return r?(0,n.jsx)(r,{...e,children:(0,n.jsx)(m,{...e})}):m(e)}}}]);