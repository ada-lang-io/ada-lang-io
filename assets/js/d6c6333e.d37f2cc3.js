"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[4592],{54848:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>A,contentTitle:()=>c,default:()=>f,frontMatter:()=>h,metadata:()=>r,toc:()=>u});const r=JSON.parse('{"id":"arm/AA-9/AA-9.9","title":"9.9 Task and Entry Attributes","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-9/AA-9.9.mdx","sourceDirName":"arm/AA-9","slug":"/arm/AA-9/AA-9.9","permalink":"/docs/arm/AA-9/AA-9.9","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":82,"frontMatter":{"sidebar_position":82},"sidebar":"referenceManualSidebar","previous":{"title":"9.8 Abort of a Task - Abort of a Sequence of Statements","permalink":"/docs/arm/AA-9/AA-9.8"},"next":{"title":"9.10 Shared Variables","permalink":"/docs/arm/AA-9/AA-9.10"}}');var a=n(74848),i=n(28453),s=n(13842),d=n(91435),o=(n(21432),n(79162)),l=n(34421);const h={sidebar_position:82},c="9.9 Task and Entry Attributes",A={},u=[{value:"Dynamic Semantics",id:"dynamic-semantics",level:4}];function x(e){const t={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,i.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(t.header,{children:(0,a.jsx)(t.h1,{id:"99-task-and-entry-attributes",children:"9.9 Task and Entry Attributes"})}),"\n",(0,a.jsx)(t.admonition,{type:"danger",children:(0,a.jsxs)(t.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.jsx)(t.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,a.jsx)(t.h4,{id:"dynamic-semantics",children:"Dynamic Semantics"}),"\n",(0,a.jsx)(o.A,{children:"1"}),"\n",(0,a.jsxs)("p",{children:["For a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0093",children:"prefix"})})," T that is of a task type [(after any implicit dereference)], the following attributes are defined: ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(o.A,{children:"2"}),"\n",(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),"T'Callable"]}),"\n",(0,a.jsxs)("dl",{children:[(0,a.jsxs)("dd",{children:["Yields the value True when the task denoted by T is ",(0,a.jsx)("em",{children:"callable"}),", and False otherwise; a task is callable unless it is completed or abnormal. The value of this attribute is of the predefined type Boolean.",(0,a.jsx)("br",{})]}),(0,a.jsx)(o.A,{children:"3"}),(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),"T'Terminated"]}),(0,a.jsxs)("dd",{children:["Yields the value True if the task denoted by T is terminated, and False otherwise. The value of this attribute is of the predefined type Boolean. ",(0,a.jsx)("br",{})]})]}),"\n",(0,a.jsx)(o.A,{children:"4"}),"\n",(0,a.jsxs)("p",{children:["For a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0093",children:"prefix"})})," E that denotes an entry of a task or protected unit, the following attribute is defined. This attribute is only allowed within the body of the task or protected unit, but excluding, in the case of an entry of a task unit, within any program unit that is, itself, inner to the body of the task unit. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(o.A,{children:"5"}),"\n",(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),"E'Count"]}),"\n",(0,a.jsx)("dl",{children:(0,a.jsxs)("dd",{children:["Yields the number of calls presently queued on the entry E of the current instance of the unit. The value of this attribute is of the type ",(0,a.jsxs)("em",{children:["universal","_","integer"]}),".",(0,a.jsx)("br",{})]})}),"\n",(0,a.jsxs)(d.A,{children:[(0,a.jsx)(o.A,{children:"6"}),(0,a.jsx)(s.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(t.p,{children:["NOTE 1   For the Count attribute, the entry can be either a single entry or an entry of a family. The name of the entry or entry family can be either a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0092",children:"direct_name"})})," or an expanded name.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(d.A,{children:[(0,a.jsx)(o.A,{children:"7/5"}),(0,a.jsx)(l.A,{items:["AI12-0442-1"]}),(0,a.jsx)(s.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(t.p,{children:["NOTE 2   Within task units, by interrogating the attribute E'Count an algorithm can allow for the increase of the value of this attribute for incoming entry calls, and its decrease, for example with ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-9/AA-9.7#S0276",children:"timed_entry_call"})}),"s. A ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-9/AA-9.7#S0279",children:"conditional_entry_call"})})," can also briefly increase this value, even if the conditional call is not accepted.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(d.A,{children:[(0,a.jsx)(o.A,{children:"8/5"}),(0,a.jsx)(l.A,{items:["AI12-0442-1"]}),(0,a.jsx)(s.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(t.p,{children:["NOTE 3   Within protected units, by interrogating the attribute E'Count in the ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0262",children:"entry_barrier"})})," for the entry E an algorithm can allow for the evaluation of the ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0150",children:"condition"})})," of the barrier both before and after queuing a given caller. ",(0,a.jsx)("br",{})]})})]})]})}function f(e={}){const{wrapper:t}={...(0,i.R)(),...e.components};return t?(0,a.jsx)(t,{...e,children:(0,a.jsx)(x,{...e})}):x(e)}}}]);