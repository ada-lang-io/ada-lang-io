"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[4404],{4827:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>k,contentTitle:()=>f,default:()=>g,frontMatter:()=>u,metadata:()=>y,toc:()=>b});var n=a(1716),o=a(7556),r=a(3183),i=Object.defineProperty,s=Object.defineProperties,l=Object.getOwnPropertyDescriptors,p=Object.getOwnPropertySymbols,d=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,m=(e,t,a)=>t in e?i(e,t,{enumerable:!0,configurable:!0,writable:!0,value:a}):e[t]=a,h=(e,t)=>{for(var a in t||(t={}))d.call(t,a)&&m(e,a,t[a]);if(p)for(var a of p(t))c.call(t,a)&&m(e,a,t[a]);return e};const u={sidebar_position:50},f="6.2 Formal Parameter Modes",y={unversionedId:"arm/AA-6/AA-6.2",id:"arm/AA-6/AA-6.2",title:"6.2 Formal Parameter Modes",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-6/AA-6.2.mdx",sourceDirName:"arm/AA-6",slug:"/arm/AA-6/AA-6.2",permalink:"/docs/arm/AA-6/AA-6.2",draft:!1,tags:[],version:"current",sidebarPosition:50,frontMatter:{sidebar_position:50},sidebar:"referenceManualSidebar",previous:{title:"6.1 Subprogram Declarations",permalink:"/docs/arm/AA-6/AA-6.1"},next:{title:"6.3 Subprogram Bodies",permalink:"/docs/arm/AA-6/AA-6.3"}},k={},b=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Bounded (Run-Time) Errors",id:"bounded-run-time-errors",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],A={toc:b};function g(e){var t,a=e,{components:i}=a,m=((e,t)=>{var a={};for(var n in e)d.call(e,n)&&t.indexOf(n)<0&&(a[n]=e[n]);if(null!=e&&p)for(var n of p(e))t.indexOf(n)<0&&c.call(e,n)&&(a[n]=e[n]);return a})(a,["components"]);return(0,n.kt)("wrapper",(t=h(h({},A),m),s(t,l({components:i,mdxType:"MDXLayout"}))),(0,n.kt)("h1",h({},{id:"62-formal-parameter-modes"}),"6.2 Formal Parameter Modes"),(0,n.kt)("admonition",h({},{type:"warning"}),(0,n.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,n.kt)("a",h({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,n.kt)("p",null,"[A ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-6/AA-6.1#S0207"},"parameter_specification"))," declares a formal parameter of mode ",(0,n.kt)("strong",null,"in"),", ",(0,n.kt)("strong",null,"in out"),", or ",(0,n.kt)("strong",null,"out"),".] "),(0,n.kt)("h4",h({},{id:"static-semantics"}),"Static Semantics"),(0,n.kt)("p",null,"A parameter is passed either ",(0,n.kt)("em",null,"by copy")," or ",(0,n.kt)("em",null,"by reference"),". [When a parameter is passed by copy, the formal parameter denotes a separate object from the actual parameter, and any information transfer between the two occurs only before and after executing the subprogram. When a parameter is passed by reference, the formal parameter denotes (a view of) the object denoted by the actual parameter; reads and updates of the formal parameter directly reference the actual parameter object.]"),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0142-4"),"}"," ","{",(0,n.kt)("em",null,"AI05-0262-1"),"}"," A type is a ",(0,n.kt)("em",null,"by-copy type")," if it is an elementary type, or if it is a descendant of a private type whose full type is a by-copy type. A parameter of a by-copy type is passed by copy, unless the formal parameter is explicitly aliased."),(0,n.kt)("p",null,"A type is a ",(0,n.kt)("em",null,"by-reference type")," if it is a descendant of one of the following: "),(0,n.kt)("p",null,"a tagged type;"),(0,n.kt)("p",null,"a task or protected type;"),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0096-1"),"}"," an explicitly limited record type; "),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("em",null,"This paragraph was deleted."),"{",(0,n.kt)("em",null,"AI05-0096-1"),"}"," ")),(0,n.kt)("p",null,"a composite type with a subcomponent of a by-reference type;"),(0,n.kt)("p",null,"a private type whose full type is a by-reference type. "),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0142-4"),"}"," ","{",(0,n.kt)("em",null,"AI05-0188-1"),"}"," ","{",(0,n.kt)("em",null,"AI12-0027-1"),"}"," ","{",(0,n.kt)("em",null,"AI12-0236-1"),"}"," ","{",(0,n.kt)("em",null,"AI12-0317-1"),"}"," A parameter of a by-reference type is passed by reference, as is an explicitly aliased parameter of any type. Each value of a by-reference type has an associated object. For a value conversion, the associated object is the anonymous result object if such an object is created (see 4.6); otherwise it is the associated object of the operand.  In other cases, the object associated with the evaluated operative constituent of the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," or ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," (see 4.4) determines its associated object."),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Ramification: "),"By-reference parameter passing makes sense only if there is an object to reference; hence, we define such an object for each case.")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"Since tagged types are by-reference types, this implies that every value of a tagged type has an associated object. This simplifies things, because we can define the tag to be a property of the object, and not of the value of the object, which makes it clearer that object tags never change.")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI12-0317-1"),"}"," A construct like parenthesized expression or ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression"))," is ignored for the purposes of determining the associated object; for a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.5#S0148"},"conditional_expression")),", it is relevant only in that it determines which ",(0,n.kt)("em",null,"dependent_"),(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," defines the associated object.")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"We considered simplifying things even more by making every value (and therefore every expression) have an associated object. After all, there is little semantic difference between a constant object and a value. However, this would cause problems for untagged types. In particular, we would have to do a constraint check on every read of a type conversion (or a renaming thereof) in certain cases.")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI95-00318-02"),"}"," We do not want this definition to depend on the view of the type; privateness is essentially ignored for this definition. Otherwise, things would be confusing (does the rule apply at the call site, at the site of the declaration of the subprogram, at the site of the return statement?), and requiring different calls to use different mechanisms would be an implementation burden.")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,'C.6, "Shared Variable Control" says that a composite type with an atomic or volatile subcomponent is a by-reference type, among other things.')),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"Every value of a limited by-reference type is the value of one and only one limited object. The ",(0,n.kt)("em",null,"associated object")," of a value of a limited by-reference type is the object whose value it represents. Two values of a limited by-reference type are the ",(0,n.kt)("em",null,"same")," if and only if they represent the value of the same object.")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,'We say "by-reference" above because these statements are not always true for limited private types whose underlying type is nonlimited (unfortunately). ')),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0240-1"),"}"," For other parameters, it is unspecified whether the parameter is passed by copy or by reference. "),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),"{",(0,n.kt)("em",null,"AI05-0005-1"),"}"," There is no need to incorporate the discussion of AI83-00178, which requires pass-by-copy for certain kinds of actual parameters, while allowing pass-by-reference for others. This is because we explicitly indicate that a function creates an anonymous constant object for its result (see 6.5). We also provide a special dispensation for instances of Unchecked_Conversion to return by reference (see 13.9). ")),(0,n.kt)("h4",h({},{id:"bounded-run-time-errors"}),"Bounded (Run-Time) Errors"),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0240-1"),"}"," If one ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," denotes a part of a formal parameter, and a second ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," denotes a part of a distinct formal parameter or an object that is not part of a formal parameter, then the two ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),"s are considered ",(0,n.kt)("em",null,"distinct access paths"),". If an object is of a type for which the parameter passing mechanism is not specified and is not an explicitly aliased parameter, then it is a bounded error to assign to the object via one access path, and then read the value of the object via a distinct access path, unless the first access path denotes a part of a formal parameter that no longer exists at the point of the second access [(due to leaving the corresponding callable construct).] The possible consequences are that Program_Error is raised, or the newly assigned value is read, or some old value of the object is read. "),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),'For example, if we call "P(X =',">"," Global_Variable, Y =",">",' Global_Variable)", then within P, the names "X", "Y", and "Global_Variable" are all distinct access paths. If Global_Variable\'s type is neither pass-by-copy nor pass-by-reference, then it is a bounded error to assign to Global_Variable and then read X or Y, since the language does not specify whether the old or the new value would be read. On the other hand, if Global_Variable\'s type is pass-by-copy, then the old value would always be read, and there is no error. Similarly, if Global_Variable\'s type is defined by the language to be pass-by-reference, then the new value would always be read, and again there is no error. ')),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Reason: "),"We are saying ",(0,n.kt)("em",null,"assign")," here, not ",(0,n.kt)("em",null,"update"),", because updating any subcomponent is considered to update the enclosing object.")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,'The "still exists" part is so that a read after the subprogram returns is OK.')),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"If the parameter is of a by-copy type, then there is no issue here - the formal is not a view of the actual. If the parameter is of a by-reference type, then the programmer may depend on updates through one access path being visible through some other access path, just as if the parameter were of an access type. ")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Implementation Note: "),"The implementation can keep a copy in a register of a parameter whose parameter-passing mechanism is not specified. If a different access path is used to update the object (creating a bounded error situation), then the implementation can still use the value of the register, even though the in-memory version of the object has been changed. However, to keep the error properly bounded, if the implementation chooses to read the in-memory version, it has to be consistent -- it cannot then assume that something it has proven about the register is true of the memory location. For example, suppose the formal parameter is L, the value of L(6) is now in a register, and L(6) is used in an ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.1#S0096"},"indexed_component")),' as in "A(L(6)) := 99;", where A has bounds 1..3. If the implementation can prove that the value for L(6) in the register is in the range 1..3, then it need not perform the constraint check if it uses the register value. However, if the memory value of L(6) has been changed to 4, and the implementation uses that memory value, then it had better not alter memory outside of A.')),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"Note that the rule allows the implementation to pass a parameter by reference and then keep just part of it in a register, or, equivalently, to pass part of the parameter by reference and another part by copy. ")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Reason: "),"We do not want to go so far as to say that the mere presence of aliasing is wrong. We wish to be able to write the following sorts of things in standard Ada: ")),(0,n.kt)(o.Z,{mdxType:"CodeBlock"},"procedure Move ( Source  : in  String;","\n","                 Target  : out String;","\n","                 Drop    : in  Truncation := Error;","\n","                 Justify : in  Alignment  := Left;","\n","                 Pad     : in  Character  := Space);","\n","-- Copies elements from Source to Target (safely if they overlap)","\n"),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"This is from the standard string handling package. It would be embarrassing if this couldn't be written in Ada!")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,'The "then" before "read" in the rule implies that the implementation can move a read to an earlier place in the code, but not to a later place after a potentially aliased assignment. Thus, if the subprogram reads one of its parameters into a local variable, and then updates another potentially aliased one, the local copy is safe - it is known to have the old value. For example, the above-mentioned Move subprogram can be implemented by copying Source into a local variable before assigning into Target.')),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"For an ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-5/AA-5.2#S0173"},"assignment_statement"))," assigning one array parameter to another, the implementation has to check which direction to copy at run time, in general, in case the actual parameters are overlapping slices. For example: ")),(0,n.kt)(o.Z,{mdxType:"CodeBlock"},"procedure Copy(X : in out String; Y: String) is","\n","begin","\n","    X := Y;","\n","end Copy;","\n"),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"It would be wrong for the compiler to assume that X and Y do not overlap (unless, of course, it can prove otherwise). ")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"NOTE 1   ","{",(0,n.kt)("em",null,"AI12-0056-1"),"}"," The mode of a formal parameter describes the direction of information transfer to or from the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram_body"))," (see 6.1).")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"NOTE 2   A formal parameter of mode ",(0,n.kt)("strong",null,"in")," is a constant view (see 3.3); it cannot be updated within the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram_body")),".")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"NOTE 3   ","{",(0,n.kt)("em",null,"AI12-0056-1"),"}"," ","{",(0,n.kt)("em",null,"AI12-0440-1"),"}"," A formal parameter of mode ",(0,n.kt)("strong",null,"out")," can be uninitialized at the start of the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram_body"))," (see 6.4.1). ")),(0,n.kt)("h4",h({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"The value of an ",(0,n.kt)("strong",null,"out")," parameter may be read. An ",(0,n.kt)("strong",null,"out")," parameter is treated like a declared variable without an explicit initial expression. ")),(0,n.kt)("h4",h({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,'Discussion of copy-in for parts of out parameters is now covered in 6.4.1, "Parameter Associations".')),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"The concept of a by-reference type is new to Ada 95.")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"We now cover in a general way in 3.7.2 the rule regarding erroneous execution when a discriminant is changed and one of the parameters depends on the discriminant. ")),(0,n.kt)("h4",h({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0096-1"),"}"," ",(0,n.kt)("strong",null,"Correction:")," Corrected so that limited derived types are by-reference only if their parent is.")),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0142-4"),"}"," Defined that explicitly aliased parameters (see 6.1) are always passed by reference. ")),(0,n.kt)("h4",h({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,n.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0027-1"),"}"," ",(0,n.kt)("strong",null,"Corrigendum:"),' Corrected so that value conversions that are copies are the "associated object" for parameter passing of by-reference types. This can only happen if the conversion is between unrelated non-limited types, and it is necessary just so the correct object is defined. ')))}g.isMDXComponent=!0}}]);