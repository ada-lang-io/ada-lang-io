"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8025],{73335:(e,r,a)=>{a.r(r),a.d(r,{assets:()=>x,contentTitle:()=>h,default:()=>p,frontMatter:()=>l,metadata:()=>A,toc:()=>j});var s=a(74848),n=a(28453),i=a(13842),t=a(91435),d=a(21432),o=a(79162),c=a(34421);const l={sidebar_position:29},h="3.11 Declarative Parts",A={id:"arm/AA-3/AA-3.11",title:"3.11 Declarative Parts",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-3/AA-3.11.mdx",sourceDirName:"arm/AA-3",slug:"/arm/AA-3/AA-3.11",permalink:"/docs/arm/AA-3/AA-3.11",draft:!1,unlisted:!1,tags:[],version:"current",sidebarPosition:29,frontMatter:{sidebar_position:29},sidebar:"referenceManualSidebar",previous:{title:"3.10 Access Types",permalink:"/docs/arm/AA-3/AA-3.10"},next:{title:"4 Names and Expressions",permalink:"/docs/arm/AA-4/"}},x={},j=[{value:"Syntax",id:"syntax",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"3.11.1  Completions of Declarations",id:"3111--completions-of-declarations",level:2},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83-1",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95-1",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4}];function m(e){const r={a:"a",admonition:"admonition",h1:"h1",h2:"h2",h4:"h4",p:"p",...(0,n.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(r.h1,{id:"311-declarative-parts",children:"3.11 Declarative Parts"}),"\n",(0,s.jsx)(r.admonition,{type:"danger",children:(0,s.jsxs)(r.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,s.jsx)(r.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,s.jsx)(o.A,{children:"1"}),"\n",(0,s.jsxs)("p",{children:["[A ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})})," contains ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0087",children:"declarative_item"})}),"s (possibly none).] ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(r.h4,{id:"syntax",children:"Syntax"}),"\n",(0,s.jsx)(o.A,{children:"2"}),"\n",(0,s.jsxs)(d.A,{children:[(0,s.jsxs)("code",{children:["declarative","_","part"]}),(0,s.jsx)("a",{id:"S0086"}),(0,s.jsx)("code",{children:" ::= "}),"{",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0087",children:"declarative_item"})}),"}",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(o.A,{children:"3"}),"\n",(0,s.jsx)(d.A,{children:(0,s.jsxs)(r.p,{children:[(0,s.jsxs)("code",{children:["declarative","_","item"]}),(0,s.jsx)("a",{id:"S0087"}),(0,s.jsx)("code",{children:" ::= "}),(0,s.jsx)("br",{}),"    ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0088",children:"basic_declarative_item"})})," | ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0089",children:"body"})}),(0,s.jsx)("br",{})]})}),"\n",(0,s.jsx)(o.A,{children:"4/1"}),"\n",(0,s.jsx)(c.A,{items:["AI95-00137-01"]}),"\n",(0,s.jsx)(d.A,{children:(0,s.jsxs)(r.p,{children:["{",(0,s.jsx)("em",{children:"8652/0009"}),"}"," ",(0,s.jsxs)("code",{children:["basic","_","declarative","_","item"]}),(0,s.jsx)("a",{id:"S0088"}),(0,s.jsx)("code",{children:" ::= "}),(0,s.jsx)("br",{}),"    ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.1#S0021",children:"basic_declaration"})})," | ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0343",children:"aspect_clause"})})," | ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-8/AA-8.4#S0235",children:"use_clause"})}),(0,s.jsx)("br",{})]})}),"\n",(0,s.jsx)(o.A,{children:"5"}),"\n",(0,s.jsx)(d.A,{children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("code",{children:"body"}),(0,s.jsx)("a",{id:"S0089"}),(0,s.jsx)("code",{children:" ::= "}),(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0090",children:"proper_body"})})," | ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#S0297",children:"body_stub"})}),(0,s.jsx)("br",{})]})}),"\n",(0,s.jsx)(o.A,{children:"6"}),"\n",(0,s.jsx)(d.A,{children:(0,s.jsxs)(r.p,{children:[(0,s.jsxs)("code",{children:["proper","_","body"]}),(0,s.jsx)("a",{id:"S0090"}),(0,s.jsx)("code",{children:" ::= "}),(0,s.jsx)("br",{}),"    ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-6/AA-6.3#S0216",children:"subprogram_body"})})," | ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-7/AA-7.2#S0231",children:"package_body"})})," | ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.1#S0248",children:"task_body"})})," | ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.4#S0254",children:"protected_body"})}),(0,s.jsx)("br",{})]})}),"\n",(0,s.jsx)(r.h4,{id:"static-semantics",children:"Static Semantics"}),"\n",(0,s.jsx)(o.A,{children:"6.1/2"}),"\n",(0,s.jsx)(c.A,{items:["AI95-00420-01"]}),"\n",(0,s.jsxs)("p",{children:["The list of ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0087",children:"declarative_item"})}),"s of a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})})," is called the ",(0,s.jsx)("em",{children:"declaration list"})," of the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})}),". ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(r.h4,{id:"dynamic-semantics",children:"Dynamic Semantics"}),"\n",(0,s.jsx)(o.A,{children:"7"}),"\n",(0,s.jsxs)("p",{children:["The elaboration of a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})})," consists of the elaboration of the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0087",children:"declarative_item"})}),"s, if any, in the order in which they are given in the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})}),".",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(o.A,{children:"8"}),"\n",(0,s.jsxs)("p",{children:["An elaborable construct is in the ",(0,s.jsx)("em",{children:"elaborated"})," state after the normal completion of its elaboration. Prior to that, it is ",(0,s.jsx)("em",{children:"not yet elaborated"}),". ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"8.a"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"ramification",children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("strong",{}),"The elaborated state is only important for bodies; certain uses of a body raise an exception if the body is not yet elaborated.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"8.b"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:['Note that "prior" implies before the start of elaboration, as well as during elaboration.',(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"8.c"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:['The use of the term "normal completion" implies that if the elaboration propagates an exception or is aborted, the declaration is not elaborated. RM83 missed the aborted case. ',(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(o.A,{children:"9"}),"\n",(0,s.jsxs)("p",{children:["For a construct that attempts to use a body, a check (Elaboration","_","Check) is performed, as follows: ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(o.A,{children:"10/1"}),"\n",(0,s.jsx)(c.A,{items:["AI95-00064-01"]}),"\n",(0,s.jsx)("ul",{children:(0,s.jsxs)("li",{children:["{",(0,s.jsx)("em",{children:"8652/0014"}),"}"," For a call to a (non-protected) subprogram that has an explicit body, a check is made that the body is already elaborated. This check and the evaluations of any actual parameters of the call are done in an arbitrary order. ",(0,s.jsx)("br",{})]})}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"10.a"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"discussion",children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("strong",{}),"AI83-00180 specifies that there is no elaboration check for a subprogram defined by a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-2/AA-2.8#S0019",children:"pragma"})})," Interface (or equivalently, ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-2/AA-2.8#S0019",children:"pragma"})})," Import). AI83-00430 specifies that there is no elaboration check for an enumeration literal. AI83-00406 specifies that the evaluation of parameters and the elaboration check occur in an arbitrary order. AI83-00406 applies to generic instantiation as well (see below).",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"10.a.1/3"}),(0,s.jsx)(c.A,{items:["AI95-00064-01","AI05-0177-1"]}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["{",(0,s.jsx)("em",{children:"8652/0014"}),"}"," A subprogram can be completed by a renaming-as-body, a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-6/AA-6.7#S0227",children:"null_procedure_declaration"})}),", or an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:"expression_function_declaration"})}),", and we need to make an elaboration check on such a body, so we use \u201cbody\u201d rather than ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-6/AA-6.3#S0216",children:"subprogram_body"})})," above. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(o.A,{children:"11/3"}),"\n",(0,s.jsx)(c.A,{items:["AI05-0229-1"]}),"\n",(0,s.jsx)("ul",{children:(0,s.jsxs)("li",{children:["For a call to a protected operation of a protected type (that has a body \u2014 no check is performed if  the protected type is imported \u2014 see ",(0,s.jsx)("a",{href:"/docs/arm/AA-B/AA-B.1",children:"B.1"}),"), a check is made that the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.4#S0254",children:"protected_body"})})," is already elaborated. This check and the evaluations of any actual parameters of the call are done in an arbitrary order. ",(0,s.jsx)("br",{})]})}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"11.a"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"discussion",children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("strong",{}),"A protected type has only one elaboration \u201cbit\u201d, rather than one for each operation, because one call may result in evaluating the barriers of other entries, and because there are no elaborable declarations between the bodies of the operations. In fact, the elaboration of a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.4#S0254",children:"protected_body"})})," does not elaborate the enclosed bodies, since they are not considered independently elaborable.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"11.b"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["Note that there is no elaboration check when calling a task entry. Task entry calls are permitted even before the associated ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.1#S0248",children:"task_body"})})," has been seen. Such calls are simply queued until the task is activated and reaches a corresponding ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0258",children:"accept_statement"})}),". We considered a similar rule for protected entries \u2014 simply queuing all calls until the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.4#S0254",children:"protected_body"})})," was seen, but felt it was not worth the possible implementation overhead, particularly given that there might be multiple instances of the protected type. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(o.A,{children:"12"}),"\n",(0,s.jsx)("ul",{children:(0,s.jsxs)("li",{children:["For the activation of a task, a check is made by the activator that the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.1#S0248",children:"task_body"})})," is already elaborated. If two or more tasks are being activated together (see ",(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.2",children:"9.2"}),"), as the result of the elaboration of a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})})," or the initialization for the object created by an allocator, this check is done for all of them before activating any of them. ",(0,s.jsx)("br",{})]})}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"12.a"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"reason",children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("strong",{}),"As specified by AI83-00149, the check is done by the activator, rather than by the task itself. If it were done by the task itself, it would be turned into a Tasking","_","Error in the activator, and the other tasks would still be activated. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(o.A,{children:"13"}),"\n",(0,s.jsx)("ul",{children:(0,s.jsxs)("li",{children:["For the instantiation of a generic unit that has a body, a check is made that this body is already elaborated. This check and the evaluation of any ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.3#S0318",children:"explicit_generic_actual_parameter"})}),"s of the instantiation are done in an arbitrary order. ",(0,s.jsx)("br",{})]})}),"\n",(0,s.jsx)(o.A,{children:"14"}),"\n",(0,s.jsxs)("p",{children:["The exception Program","_","Error is raised if any of these checks fails. ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(t.A,{children:(0,s.jsx)(r.h4,{id:"extensions-to-ada-83",children:"Extensions to Ada 83"})}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"14.a/2"}),(0,s.jsx)(c.A,{items:["AI95-00114-01"]}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["The syntax for ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})})," is modified to remove the ordering restrictions of Ada 83; that is, the distinction between ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0088",children:"basic_declarative_item"})}),"s and ",(0,s.jsxs)("code",{children:["later","_","declarative","_","item"]}),"s within ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})}),"s is removed. This means that things like ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-8/AA-8.4#S0235",children:"use_clause"})}),"s and ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.3#S0032",children:"object_declaration"})}),"s can be freely intermixed with things like bodies.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"14.b"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["The syntax rule for ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0090",children:"proper_body"})})," now allows a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.4#S0254",children:"protected_body"})}),", and the rules for elaboration checks now cover calls on protected operations. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(t.A,{children:(0,s.jsx)(r.h4,{id:"wording-changes-from-ada-83",children:"Wording Changes from Ada 83"})}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"14.c"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["The syntax rule for ",(0,s.jsxs)("code",{children:["later","_","declarative","_","item"]})," is removed; the syntax rule for ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0087",children:"declarative_item"})})," is new.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"14.d"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["RM83 defines \u201celaborated\u201d and \u201cnot yet elaborated\u201d for ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0087",children:"declarative_item"})}),"s here, and for other things in ",(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.1",children:"3.1"}),", \u201c",(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.1",children:"Declarations"}),"\u201d. That's no longer necessary, since these terms are fully defined in ",(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.1",children:"3.1"}),".",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"14.e"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["In RM83, all uses of ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})})," are optional (except for the one in ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-5/AA-5.6#S0191",children:"block_statement"})})," with a ",(0,s.jsx)("strong",{children:"declare"}),") which is sort of strange, since a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})})," can be empty, according to the syntax. That is, ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})}),"s are sort of \u201cdoubly optional\u201d. In Ada 95, these ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})}),"s are always required (but can still be empty). To simplify description, we go further and say (see ",(0,s.jsx)("a",{href:"/docs/arm/AA-5/AA-5.6",children:"5.6"}),", \u201c",(0,s.jsx)("a",{href:"/docs/arm/AA-5/AA-5.6",children:"Block Statements"}),"\u201d) that a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-5/AA-5.6#S0191",children:"block_statement"})})," without an explicit ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0086",children:"declarative_part"})})," is equivalent to one with an empty one. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(t.A,{children:(0,s.jsx)(r.h4,{id:"wording-changes-from-ada-95",children:"Wording Changes from Ada 95"})}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"14.f/2"}),(0,s.jsx)(c.A,{items:["AI95-00137-01"]}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["{",(0,s.jsx)("em",{children:"8652/0009"}),"}"," ",(0,s.jsx)("strong",{children:"Corrigendum:"})," Changed representation clauses to aspect clauses to reflect that they are used for more than just representation.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"14.g/2"}),(0,s.jsx)(c.A,{items:["AI95-00064-01"]}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["{",(0,s.jsx)("em",{children:"8652/0014"}),"}"," ",(0,s.jsx)("strong",{children:"Corrigendum:"})," Clarified that the elaboration check applies to all kinds of subprogram bodies.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"14.h/2"}),(0,s.jsx)(c.A,{items:["AI95-00420-01"]}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["Defined \u201cdeclaration list\u201d to avoid confusion for various rules. Other kinds of declaration list are defined elsewhere. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)("a",{id:"Subclause_3.11.1"}),"\n",(0,s.jsx)(r.h2,{id:"3111--completions-of-declarations",children:"3.11.1  Completions of Declarations"}),"\n",(0,s.jsx)(o.A,{children:"1/3_3.11.1"}),"\n",(0,s.jsx)(c.A,{items:["AI95-00064-01","AI05-0177-1"]}),"\n",(0,s.jsxs)("p",{children:["{",(0,s.jsx)("em",{children:"8652/0014"}),"}"," Declarations sometimes come in two parts. A declaration that requires a second part is said to ",(0,s.jsx)("em",{children:"require completion"}),". The second part is called the ",(0,s.jsx)("em",{children:"completion"})," of the declaration (and of the entity declared), and is either another declaration, a body, or a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-2/AA-2.8#S0019",children:"pragma"})}),". A ",(0,s.jsx)("em",{children:"body"})," is a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0089",children:"body"})}),", an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0260",children:"entry_body"})}),", a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-6/AA-6.7#S0227",children:"null_procedure_declaration"})})," or an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-6/AA-6.8#S0228",children:"expression_function_declaration"})})," that completes another declaration, or a renaming-as-body (see ",(0,s.jsx)("a",{href:"/docs/arm/AA-8/AA-8.5#Subclause_8.5.4",children:"8.5.4"}),"). ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"1.a_3.11.1"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"discussion",children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("strong",{}),"Throughout the RM95, there are rules about completions that define the following: ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"1.b_3.11.1"}),(0,s.jsxs)("ul",{children:[(0,s.jsxs)("li",{children:["Which declarations require a corresponding completion.",(0,s.jsx)("br",{})]}),(0,s.jsx)(o.A,{children:"1.c_3.11.1"}),(0,s.jsxs)("li",{children:["Which constructs can only serve as the completion of a declaration.",(0,s.jsx)("br",{})]}),(0,s.jsx)(o.A,{children:"1.d_3.11.1"}),(0,s.jsxs)("li",{children:["Where the completion of a declaration is allowed to be.",(0,s.jsx)("br",{})]}),(0,s.jsx)(o.A,{children:"1.e_3.11.1"}),(0,s.jsxs)("li",{children:["What kinds of completions are allowed to correspond to each kind of declaration that allows one. ",(0,s.jsx)("br",{})]})]})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"1.f_3.11.1"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["Don't confuse this compile-time concept with the run-time concept of completion defined in ",(0,s.jsx)("a",{href:"/docs/arm/AA-7/AA-7.6#Subclause_7.6.1",children:"7.6.1"}),".",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"1.g_3.11.1"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["Note that the declaration of a private type (if limited) can be completed with the declaration of a task type, which is then completed with a body. Thus, a declaration can actually come in ",(0,s.jsx)("em",{children:"three"})," parts.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"1.h/3_3.11.1"}),(0,s.jsx)(c.A,{items:["AI95-00217-06","AI05-0162-1"]}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["An incomplete type (whether declared in the limited view of a package or not) may be completed by a private type declaration, so we can in fact have ",(0,s.jsx)("em",{children:"four"})," parts.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"1.i/3_3.11.1"}),(0,s.jsx)(c.A,{items:["AI05-0229-1"]}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["In Ada 2012, there are no language-defined pragmas that act as completions. Pragma Import (which is obsolescent) has the effect of setting aspect Import to True; such an aspect makes giving a completion illegal. The wording that allows pragmas as completions was left as it is harmless and appears in many places in this Reference Manual. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(r.h4,{id:"name-resolution-rules",children:"Name Resolution Rules"}),"\n",(0,s.jsx)(o.A,{children:"2_3.11.1"}),"\n",(0,s.jsxs)("p",{children:["A construct that can be a completion is interpreted as the completion of a prior declaration only if: ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(o.A,{children:"3_3.11.1"}),"\n",(0,s.jsxs)("ul",{children:[(0,s.jsxs)("li",{children:["The declaration and the completion occur immediately within the same declarative region;",(0,s.jsx)("br",{})]}),(0,s.jsx)(o.A,{children:"4_3.11.1"}),(0,s.jsxs)("li",{children:["The defining name or ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0201",children:"defining_program_unit_name"})})," in the completion is the same as in the declaration, or in the case of a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-2/AA-2.8#S0019",children:"pragma"})}),", the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-2/AA-2.8#S0019",children:"pragma"})})," applies to the declaration;",(0,s.jsx)("br",{})]}),(0,s.jsx)(o.A,{children:"5_3.11.1"}),(0,s.jsxs)("li",{children:["If the declaration is overloadable, then the completion either has a type-conformant profile, or is a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-2/AA-2.8#S0019",children:"pragma"})}),". ",(0,s.jsx)("br",{})]})]}),"\n",(0,s.jsx)(r.h4,{id:"legality-rules",children:"Legality Rules"}),"\n",(0,s.jsx)(o.A,{children:"6/3_3.11.1"}),"\n",(0,s.jsx)(c.A,{items:["AI05-0229-1"]}),"\n",(0,s.jsxs)("p",{children:["An implicit declaration shall not have a completion. For any explicit declaration that is specified to ",(0,s.jsx)("em",{children:"require completion"}),", there shall be a corresponding explicit completion, unless the declared entity is imported (see ",(0,s.jsx)("a",{href:"/docs/arm/AA-B/AA-B.1",children:"B.1"}),"). ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"6.a.1/2_3.11.1"}),(0,s.jsx)(c.A,{items:["AI95-00217-06"]}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("strong",{children:"To be honest: "}),"The implicit declarations occurring in a limited view do have a completion (the explicit declaration occurring in the full view) but that's a special case, since the implicit declarations are actually built from the explicit ones. So they do not ",(0,s.jsx)("em",{children:"require"})," a completion, they have one by ",(0,s.jsx)("em",{children:"fiat"}),". ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"6.a_3.11.1"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"discussion",children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("strong",{}),"The implicit declarations of predefined operators are not allowed to have a completion. Enumeration literals, although they are subprograms, are not allowed to have a corresponding ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-6/AA-6.3#S0216",children:"subprogram_body"})}),". That's because the completion rules are described in terms of constructs (",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0195",children:"subprogram_declaration"})}),"s) and not entities (subprograms). When a completion is required, it has to be explicit; the implicit null ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-7/AA-7.2#S0231",children:"package_body"})})," that Clause ",(0,s.jsx)("a",{href:"/docs/arm/AA-7/",children:"7"})," talks about cannot serve as the completion of a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0229",children:"package_declaration"})})," if a completion is required. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(o.A,{children:"7_3.11.1"}),"\n",(0,s.jsxs)("p",{children:["At most one completion is allowed for a given declaration. Additional requirements on completions appear where each kind of completion is defined. ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"7.a_3.11.1"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"ramification",children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("strong",{}),"A subunit is not a completion; the stub is.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"7.b_3.11.1"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["If the completion of a declaration is also a declaration, then ",(0,s.jsx)("em",{children:"that"})," declaration might have a completion, too. For example, a limited private type can be completed with a task type, which can then be completed with a task body. This is not a violation of the \u201cat most one completion\u201d rule. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(o.A,{children:"8_3.11.1"}),"\n",(0,s.jsxs)("p",{children:["A type is ",(0,s.jsx)("em",{children:"completely defined"})," at a place that is after its full type definition (if it has one) and after all of its subcomponent types are completely defined. A type shall be completely defined before it is frozen (see ",(0,s.jsx)("a",{href:"/docs/arm/AA-13/AA-13.14",children:"13.14"})," and ",(0,s.jsx)("a",{href:"/docs/arm/AA-7/AA-7.3",children:"7.3"}),"). ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"8.a_3.11.1"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"reason",children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("strong",{}),"Index types are always completely defined \u2014 no need to mention them. There is no way for a completely defined type to depend on the value of a (still) deferred constant. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"9/3_3.11.1"}),(0,s.jsx)(c.A,{items:["AI05-0229-1"]}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["NOTE 1   Completions are in principle allowed for any kind of explicit declaration. However, for some kinds of declaration, the only allowed completion is an implementation-defined pragma, and implementations are not required to have any such pragmas. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"9.a/3_3.11.1"}),(0,s.jsx)(c.A,{items:["AI05-0229-1"]}),(0,s.jsxs)(i.A,{type:"aarm",aarm:"note",children:[(0,s.jsx)("em",{children:"This paragraph was deleted."}),(0,s.jsx)("br",{})]})]}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"10/5_3.11.1"}),(0,s.jsx)(c.A,{items:["AI12-0449-1"]}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["NOTE 2   There are rules that prevent premature uses of declarations that have a corresponding completion. The Elaboration","_","Checks of ",(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11",children:"3.11"})," prevent such uses at run time for subprograms, protected operations, tasks, and generic units. The freezing rules (see ",(0,s.jsx)("a",{href:"/docs/arm/AA-13/AA-13.14",children:"13.14"}),") prevent, at compile time, premature uses of other entities such as private types and deferred constants. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(t.A,{children:(0,s.jsx)(r.h4,{id:"wording-changes-from-ada-83-1",children:"Wording Changes from Ada 83"})}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"10.a_3.11.1"}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["This subclause is new. It is intended to cover all kinds of completions of declarations, be they a body for a spec, a full type for an incomplete or private type, a full constant declaration for a deferred constant declaration, or a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-2/AA-2.8#S0019",children:"pragma"})})," Import for any kind of entity. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(t.A,{children:(0,s.jsx)(r.h4,{id:"wording-changes-from-ada-95-1",children:"Wording Changes from Ada 95"})}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"10.b/2_3.11.1"}),(0,s.jsx)(c.A,{items:["AI95-00064-01"]}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["{",(0,s.jsx)("em",{children:"8652/0014"}),"}"," ",(0,s.jsx)("strong",{children:"Corrigendum:"})," Added a definition of ",(0,s.jsx)("em",{children:"body"}),", which is different than ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0089",children:"body"})})," or ",(0,s.jsx)("strong",{children:"body"}),". ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(t.A,{children:(0,s.jsx)(r.h4,{id:"wording-changes-from-ada-2005",children:"Wording Changes from Ada 2005"})}),"\n",(0,s.jsxs)(t.A,{children:[(0,s.jsx)(o.A,{children:"10.c/3_3.11.1"}),(0,s.jsx)(c.A,{items:["AI95-0177-1"]}),(0,s.jsx)(i.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["Added null procedures and expression functions that are completions to the definition of ",(0,s.jsx)("em",{children:"body"}),". ",(0,s.jsx)("br",{})]})})]})]})}function p(e={}){const{wrapper:r}={...(0,n.R)(),...e.components};return r?(0,s.jsx)(r,{...e,children:(0,s.jsx)(m,{...e})}):m(e)}}}]);