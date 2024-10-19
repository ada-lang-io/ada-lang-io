"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[7804],{36433:(e,r,s)=>{s.r(r),s.d(r,{assets:()=>m,contentTitle:()=>l,default:()=>A,frontMatter:()=>h,metadata:()=>p,toc:()=>j});var a=s(74848),n=s(28453),t=s(13842),i=s(91435),o=s(21432),c=s(79162),d=s(34421);const h={sidebar_position:52},l="6.2 Formal Parameter Modes",p={id:"arm/AA-6/AA-6.2",title:"6.2 Formal Parameter Modes",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-6/AA-6.2.mdx",sourceDirName:"arm/AA-6",slug:"/arm/AA-6/AA-6.2",permalink:"/docs/arm/AA-6/AA-6.2",draft:!1,unlisted:!1,tags:[],version:"current",sidebarPosition:52,frontMatter:{sidebar_position:52},sidebar:"referenceManualSidebar",previous:{title:"6.1 Subprogram Declarations",permalink:"/docs/arm/AA-6/AA-6.1"},next:{title:"6.3 Subprogram Bodies",permalink:"/docs/arm/AA-6/AA-6.3"}},m={},j=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Bounded (Run-Time) Errors",id:"bounded-run-time-errors",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}];function x(e){const r={a:"a",admonition:"admonition",h1:"h1",h4:"h4",p:"p",...(0,n.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(r.h1,{id:"62-formal-parameter-modes",children:"6.2 Formal Parameter Modes"}),"\n",(0,a.jsx)(r.admonition,{type:"danger",children:(0,a.jsxs)(r.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.jsx)(r.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,a.jsx)(c.A,{children:"1"}),"\n",(0,a.jsxs)("p",{children:["[A ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0207",children:"parameter_specification"})})," declares a formal parameter of mode ",(0,a.jsx)("strong",{children:"in"}),", ",(0,a.jsx)("strong",{children:"in out"}),", or ",(0,a.jsx)("strong",{children:"out"}),".] ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(r.h4,{id:"static-semantics",children:"Static Semantics"}),"\n",(0,a.jsx)(c.A,{children:"2"}),"\n",(0,a.jsxs)("p",{children:["A parameter is passed either ",(0,a.jsx)("em",{children:"by copy"})," or ",(0,a.jsx)("em",{children:"by reference"}),". [When a parameter is passed by copy, the formal parameter denotes a separate object from the actual parameter, and any information transfer between the two occurs only before and after executing the subprogram. When a parameter is passed by reference, the formal parameter denotes (a view of) the object denoted by the actual parameter; reads and updates of the formal parameter directly reference the actual parameter object.]",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(c.A,{children:"3/3"}),"\n",(0,a.jsx)(d.A,{items:["AI05-0142-4","AI05-0262-1"]}),"\n",(0,a.jsxs)("p",{children:["A type is a ",(0,a.jsx)("em",{children:"by-copy type"})," if it is an elementary type, or if it is a descendant of a private type whose full type is a by-copy type. A parameter of a by-copy type is passed by copy, unless the formal parameter is explicitly aliased.",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(c.A,{children:"4"}),"\n",(0,a.jsxs)("p",{children:["A type is a ",(0,a.jsx)("em",{children:"by-reference type"})," if it is a descendant of one of the following: ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(c.A,{children:"5"}),"\n",(0,a.jsxs)("ul",{children:[(0,a.jsxs)("li",{children:["a tagged type;",(0,a.jsx)("br",{})]}),(0,a.jsx)(c.A,{children:"6"}),(0,a.jsxs)("li",{children:["a task or protected type;",(0,a.jsx)("br",{})]}),(0,a.jsx)(c.A,{children:"7/3"}),(0,a.jsx)(d.A,{items:["AI05-0096-1"]}),(0,a.jsxs)("li",{children:["an explicitly limited record type; ",(0,a.jsx)("br",{})]})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"7.a/3"}),(0,a.jsx)(d.A,{items:["AI05-0096-1"]}),(0,a.jsxs)(t.A,{type:"aarm",aarm:"note",children:[(0,a.jsx)("em",{children:"This paragraph was deleted."}),(0,a.jsx)("br",{})]})]}),"\n",(0,a.jsx)(c.A,{children:"8"}),"\n",(0,a.jsxs)("ul",{children:[(0,a.jsxs)("li",{children:["a composite type with a subcomponent of a by-reference type;",(0,a.jsx)("br",{})]}),(0,a.jsx)(c.A,{children:"9"}),(0,a.jsxs)("li",{children:["a private type whose full type is a by-reference type. ",(0,a.jsx)("br",{})]})]}),"\n",(0,a.jsx)(c.A,{children:"10/5"}),"\n",(0,a.jsx)(d.A,{items:["AI05-0142-4","AI05-0188-1","AI12-0027-1","AI12-0236-1","AI12-0317-1"]}),"\n",(0,a.jsxs)("p",{children:["A parameter of a by-reference type is passed by reference, as is an explicitly aliased parameter of any type. Each value of a by-reference type has an associated object. For a value conversion, the associated object is the anonymous result object if such an object is created (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.6",children:"4.6"}),"); otherwise it is the associated object of the operand. In other cases, the object associated with the evaluated operative constituent of the ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," or ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4",children:"4.4"}),") determines its associated object.",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"10.a"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"ramification",children:(0,a.jsxs)(r.p,{children:[(0,a.jsx)("strong",{}),"By-reference parameter passing makes sense only if there is an object to reference; hence, we define such an object for each case.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"10.b"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["Since tagged types are by-reference types, this implies that every value of a tagged type has an associated object. This simplifies things, because we can define the tag to be a property of the object, and not of the value of the object, which makes it clearer that object tags never change.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"10.b.1/5"}),(0,a.jsx)(d.A,{items:["AI12-0317-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["A construct like parenthesized expression or ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.7#S0163",children:"qualified_expression"})})," is ignored for the purposes of determining the associated object; for a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0148",children:"conditional_expression"})}),", it is relevant only in that it determines which ",(0,a.jsxs)("em",{children:["dependent","_"]}),(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," defines the associated object.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"10.c"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["We considered simplifying things even more by making every value (and therefore every expression) have an associated object. After all, there is little semantic difference between a constant object and a value. However, this would cause problems for untagged types. In particular, we would have to do a constraint check on every read of a type conversion (or a renaming thereof) in certain cases.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"10.d/2"}),(0,a.jsx)(d.A,{items:["AI95-00318-02"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["We do not want this definition to depend on the view of the type; privateness is essentially ignored for this definition. Otherwise, things would be confusing (does the rule apply at the call site, at the site of the declaration of the subprogram, at the site of the return statement?), and requiring different calls to use different mechanisms would be an implementation burden.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"10.e"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:[(0,a.jsx)("a",{href:"/docs/arm/AA-C/AA-C.6",children:"C.6"}),", \u201c",(0,a.jsx)("a",{href:"/docs/arm/AA-C/AA-C.6",children:"Shared Variable Control"}),"\u201d says that a composite type with an atomic or volatile subcomponent is a by-reference type, among other things.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"10.f"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["Every value of a limited by-reference type is the value of one and only one limited object. The ",(0,a.jsx)("em",{children:"associated object"})," of a value of a limited by-reference type is the object whose value it represents. Two values of a limited by-reference type are the ",(0,a.jsx)("em",{children:"same"})," if and only if they represent the value of the same object.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"10.g"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["We say \u201cby-reference\u201d above because these statements are not always true for limited private types whose underlying type is nonlimited (unfortunately). ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(c.A,{children:"11/3"}),"\n",(0,a.jsx)(d.A,{items:["AI05-0240-1"]}),"\n",(0,a.jsxs)("p",{children:["For other parameters, it is unspecified whether the parameter is passed by copy or by reference. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"11.a/3"}),(0,a.jsx)(d.A,{items:["AI05-0005-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"discussion",children:(0,a.jsxs)(r.p,{children:[(0,a.jsx)("strong",{}),"There is no need to incorporate the discussion of AI83-00178, which requires pass-by-copy for certain kinds of actual parameters, while allowing pass-by-reference for others. This is because we explicitly indicate that a function creates an anonymous constant object for its result (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.5",children:"6.5"}),"). We also provide a special dispensation for instances of Unchecked","_","Conversion to return by reference (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-13/AA-13.9",children:"13.9"}),"). ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(r.h4,{id:"bounded-run-time-errors",children:"Bounded (Run-Time) Errors"}),"\n",(0,a.jsx)(c.A,{children:"12/3"}),"\n",(0,a.jsx)(d.A,{items:["AI05-0240-1"]}),"\n",(0,a.jsxs)("p",{children:["If one ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," denotes a part of a formal parameter, and a second ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," denotes a part of a distinct formal parameter or an object that is not part of a formal parameter, then the two ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})}),"s are considered ",(0,a.jsx)("em",{children:"distinct access paths"}),". If an object is of a type for which the parameter passing mechanism is not specified and is not an explicitly aliased parameter, then it is a bounded error to assign to the object via one access path, and then read the value of the object via a distinct access path, unless the first access path denotes a part of a formal parameter that no longer exists at the point of the second access [(due to leaving the corresponding callable construct).] The possible consequences are that Program","_","Error is raised, or the newly assigned value is read, or some old value of the object is read. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"12.a"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"discussion",children:(0,a.jsxs)(r.p,{children:[(0,a.jsx)("strong",{}),"For example, if we call \u201cP(X =",">"," Global","_","Variable, Y =",">"," Global","_","Variable)\u201d, then within P, the names \u201cX\u201d, \u201cY\u201d, and \u201cGlobal","_","Variable\u201d are all distinct access paths. If Global","_","Variable's type is neither pass-by-copy nor pass-by-reference, then it is a bounded error to assign to Global","_","Variable and then read X or Y, since the language does not specify whether the old or the new value would be read. On the other hand, if Global","_","Variable's type is pass-by-copy, then the old value would always be read, and there is no error. Similarly, if Global","_","Variable's type is defined by the language to be pass-by-reference, then the new value would always be read, and again there is no error. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"12.b"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"reason",children:(0,a.jsxs)(r.p,{children:[(0,a.jsx)("strong",{}),"We are saying ",(0,a.jsx)("em",{children:"assign"})," here, not ",(0,a.jsx)("em",{children:"update"}),", because updating any subcomponent is considered to update the enclosing object.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"12.c"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["The \u201cstill exists\u201d part is so that a read after the subprogram returns is OK.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"12.d"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["If the parameter is of a by-copy type, then there is no issue here \u2014 the formal is not a view of the actual. If the parameter is of a by-reference type, then the programmer may depend on updates through one access path being visible through some other access path, just as if the parameter were of an access type. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"12.e"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"implementation-note",children:(0,a.jsxs)(r.p,{children:[(0,a.jsx)("strong",{}),"The implementation can keep a copy in a register of a parameter whose parameter-passing mechanism is not specified. If a different access path is used to update the object (creating a bounded error situation), then the implementation can still use the value of the register, even though the in-memory version of the object has been changed. However, to keep the error properly bounded, if the implementation chooses to read the in-memory version, it has to be consistent -- it cannot then assume that something it has proven about the register is true of the memory location. For example, suppose the formal parameter is L, the value of L(6) is now in a register, and L(6) is used in an ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0096",children:"indexed_component"})})," as in \u201cA(L(6)) := 99;\u201d, where A has bounds 1..3. If the implementation can prove that the value for L(6) in the register is in the range 1..3, then it need not perform the constraint check if it uses the register value. However, if the memory value of L(6) has been changed to 4, and the implementation uses that memory value, then it had better not alter memory outside of A.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"12.f"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["Note that the rule allows the implementation to pass a parameter by reference and then keep just part of it in a register, or, equivalently, to pass part of the parameter by reference and another part by copy. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"12.g"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"reason",children:(0,a.jsxs)(r.p,{children:[(0,a.jsx)("strong",{}),"We do not want to go so far as to say that the mere presence of aliasing is wrong. We wish to be able to write the following sorts of things in standard Ada: ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"12.h"}),(0,a.jsx)(o.A,{language:"ada",children:(0,a.jsxs)(r.p,{children:["procedure Move ( Source  : in  String;","\n","                 Target  : out String;","\n","                 Drop    : in  Truncation := Error;","\n","                 Justify : in  Alignment  := Left;","\n","                 Pad     : in  Character  := Space);","\n","-- Copies elements from Source to Target (safely if they overlap)","\n"]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"12.i"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["This is from the standard string handling package. It would be embarrassing if this couldn't be written in Ada!",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"12.j"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["The \u201cthen\u201d before \u201cread\u201d in the rule implies that the implementation can move a read to an earlier place in the code, but not to a later place after a potentially aliased assignment. Thus, if the subprogram reads one of its parameters into a local variable, and then updates another potentially aliased one, the local copy is safe \u2014 it is known to have the old value. For example, the above-mentioned Move subprogram can be implemented by copying Source into a local variable before assigning into Target.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"12.k"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["For an ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-5/AA-5.2#S0173",children:"assignment_statement"})})," assigning one array parameter to another, the implementation has to check which direction to copy at run time, in general, in case the actual parameters are overlapping slices. For example: ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"12.l"}),(0,a.jsx)(o.A,{language:"ada",children:(0,a.jsxs)(r.p,{children:["procedure Copy(X : in out String; Y: String) is","\n","begin","\n","    X := Y;","\n","end Copy;","\n"]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"12.m"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["It would be wrong for the compiler to assume that X and Y do not overlap (unless, of course, it can prove otherwise). ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"13/4"}),(0,a.jsx)(d.A,{items:["AI12-0056-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["NOTE 1   The mode of a formal parameter describes the direction of information transfer to or from the ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.3#S0216",children:"subprogram_body"})})," (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1",children:"6.1"}),").",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"14/4"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["NOTE 2   A formal parameter of mode ",(0,a.jsx)("strong",{children:"in"})," is a constant view (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-3/AA-3.3",children:"3.3"}),"); it cannot be updated within the ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.3#S0216",children:"subprogram_body"})}),".",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"15/5"}),(0,a.jsx)(d.A,{items:["AI12-0056-1","AI12-0440-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["NOTE 3   A formal parameter of mode ",(0,a.jsx)("strong",{children:"out"})," can be uninitialized at the start of the ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.3#S0216",children:"subprogram_body"})})," (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.4#Subclause_6.4.1",children:"6.4.1"}),"). ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(i.A,{children:(0,a.jsx)(r.h4,{id:"extensions-to-ada-83",children:"Extensions to Ada 83"})}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"15.a"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["The value of an ",(0,a.jsx)("strong",{children:"out"})," parameter may be read. An ",(0,a.jsx)("strong",{children:"out"})," parameter is treated like a declared variable without an explicit initial expression. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(i.A,{children:(0,a.jsx)(r.h4,{id:"wording-changes-from-ada-83",children:"Wording Changes from Ada 83"})}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"15.b"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["Discussion of copy-in for parts of out parameters is now covered in ",(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.4#Subclause_6.4.1",children:"6.4.1"}),", \u201c",(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.4#Subclause_6.4.1",children:"Parameter Associations"}),"\u201d.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"15.c"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["The concept of a by-reference type is new to Ada 95.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"15.d"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["We now cover in a general way in ",(0,a.jsx)("a",{href:"/docs/arm/AA-3/AA-3.7#Subclause_3.7.2",children:"3.7.2"})," the rule regarding erroneous execution when a discriminant is changed and one of the parameters depends on the discriminant. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(i.A,{children:(0,a.jsx)(r.h4,{id:"wording-changes-from-ada-2005",children:"Wording Changes from Ada 2005"})}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"15.e/3"}),(0,a.jsx)(d.A,{items:["AI05-0096-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"correction",children:(0,a.jsxs)(r.p,{children:[(0,a.jsx)("strong",{})," Corrected so that limited derived types are by-reference only if their parent is.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"15.f/3"}),(0,a.jsx)(d.A,{items:["AI05-0142-4"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["Defined that explicitly aliased parameters (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1",children:"6.1"}),") are always passed by reference. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(i.A,{children:(0,a.jsx)(r.h4,{id:"wording-changes-from-ada-2012",children:"Wording Changes from Ada 2012"})}),"\n",(0,a.jsxs)(i.A,{children:[(0,a.jsx)(c.A,{children:"15.g/4"}),(0,a.jsx)(d.A,{items:["AI05-0027-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:[(0,a.jsx)("strong",{children:"Corrigendum:"})," Corrected so that value conversions that are copies are the \u201cassociated object\u201d for parameter passing of by-reference types. This can only happen if the conversion is between unrelated non-limited types, and it is necessary just so the correct object is defined. ",(0,a.jsx)("br",{})]})})]})]})}function A(e={}){const{wrapper:r}={...(0,n.R)(),...e.components};return r?(0,a.jsx)(r,{...e,children:(0,a.jsx)(x,{...e})}):x(e)}}}]);