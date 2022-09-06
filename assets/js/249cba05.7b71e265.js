"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8011],{3298:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>f,contentTitle:()=>h,default:()=>A,frontMatter:()=>m,metadata:()=>k,toc:()=>g});var i=n(1716),l=n(7556),a=Object.defineProperty,o=Object.defineProperties,s=Object.getOwnPropertyDescriptors,r=Object.getOwnPropertySymbols,d=Object.prototype.hasOwnProperty,u=Object.prototype.propertyIsEnumerable,c=(e,t,n)=>t in e?a(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,p=(e,t)=>{for(var n in t||(t={}))d.call(t,n)&&c(e,n,t[n]);if(r)for(var n of r(t))u.call(t,n)&&c(e,n,t[n]);return e};const m={sidebar_position:62},h="7.5  Limited Types",k={unversionedId:"arm/AA-7.5",id:"arm/AA-7.5",title:"7.5  Limited Types",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-7.5.mdx",sourceDirName:"arm",slug:"/arm/AA-7.5",permalink:"/docs/arm/AA-7.5",draft:!1,tags:[],version:"current",sidebarPosition:62,frontMatter:{sidebar_position:62},sidebar:"tutorialSidebar",previous:{title:"7.4  Deferred Constants",permalink:"/docs/arm/AA-7.4"},next:{title:"7.6  Assignment and Finalization",permalink:"/docs/arm/AA-7.6"}},f={},g=[{value:"Legality Rules",id:"legality-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4},{value:"Extensions to Ada 2012",id:"extensions-to-ada-2012",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],y={toc:g};function A(e){var t,n=e,{components:a}=n,c=((e,t)=>{var n={};for(var i in e)d.call(e,i)&&t.indexOf(i)<0&&(n[i]=e[i]);if(null!=e&&r)for(var i of r(e))t.indexOf(i)<0&&u.call(e,i)&&(n[i]=e[i]);return n})(n,["components"]);return(0,i.kt)("wrapper",(t=p(p({},y),c),o(t,s({components:a,mdxType:"MDXLayout"}))),(0,i.kt)("h1",p({},{id:"75--limited-types"}),"7.5  Limited Types"),(0,i.kt)("admonition",p({},{type:"warning"}),(0,i.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,i.kt)("a",p({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00287-01"),"}"," [A limited type is (a view of) a type for which copying (such as for an ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-5.2#S0173"},"assignment_statement")),") is not allowed. A nonlimited type is a (view of a) type for which copying is allowed.] "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"The concept of the ",(0,i.kt)("em",null,"value")," of a limited type is difficult to define, since the abstract value of a limited type often extends beyond its physical representation. In some sense, values of a limited type cannot be divorced from their object. The value ",(0,i.kt)("em",null,"is")," the object."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00318-02"),"}",' In Ada 83, in the two places where limited types were defined by the language, namely tasks and files, an implicit level of indirection was implied by the semantics to avoid the separation of the value from an associated object. In Ada 95, most limited types are passed by reference, and even return-ed by reference. In Ada 2005, most limited types are built-in-place upon return, rather than returned by reference. Thus the object "identity" is part of the logical value of most limited types. '),(0,i.kt)("p",null,(0,i.kt)("strong",null,"To be honest: "),"{",(0,i.kt)("em",null,"AI95-00287-01"),"}"," ","{",(0,i.kt)("em",null,"AI95-00419-01"),"}"," For a limited partial view whose full view is nonlimited, copying is possible on parameter passing and function return. To prevent any copying whatsoever, one should make both the partial ",(0,i.kt)("em",null,"and")," full views limited. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Glossary entry: "),"A limited type is a type for which copying (such as in an ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-5.2#S0173"},"assignment_statement")),") is not allowed. A nonlimited type is a type for which copying is allowed."),(0,i.kt)("p",null,"Version=[5],Kind=(AddedNormal),Group=[T],Term=[limited type], Def=[a type for which copying (such as in an ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-5.2#S0173"},"assignment_statement")),") is not allowed], Note1=[A nonlimited type is a type for which copying is allowed.] "),(0,i.kt)("h4",p({},{id:"legality-rules"}),"Legality Rules"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00419-01"),"}"," If a tagged record type has any limited components, then the reserved word ",(0,i.kt)("strong",null,"limited")," shall appear in its ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-3.8#S0066"},"record_type_definition")),". [If the reserved word ",(0,i.kt)("strong",null,"limited")," appears in the definition of a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-3.4#S0035"},"derived_type_definition")),", its parent type and any progenitor interfaces shall be limited.] "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Proof: "),"{",(0,i.kt)("em",null,"AI95-00419-01"),"}"," ","{",(0,i.kt)("em",null,"AI12-0005-1"),"}"," The rule about the parent type being required to be limited can be found in 3.4. Rules about progenitor interfaces can be found in 3.9.4; specifically, a nonlimited interface can appear only on a nonlimited type. We repeat these rules here to gather these scattered rules in one obvious place. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Reason: "),"This prevents tagged limited types from becoming nonlimited. Otherwise, the following could happen: "),(0,i.kt)(l.Z,{mdxType:"CodeBlock"},"package P is","\n","    type T is limited private;","\n","    type R is tagged","\n","        record -- Illegal!","\n",'               -- This should say "limited record".',"\n","            X : T;","\n","        end record;","\n","private","\n","    type T is new Integer; -- R becomes nonlimited here.","\n","end P;","\n"),(0,i.kt)(l.Z,{mdxType:"CodeBlock"},"package Q is","\n","    type R2 is new R with","\n","        record","\n","            Y : Some_Task_Type;","\n","        end record;","\n","end Q;","\n"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00230-01"),"}"," If the above were legal, then assignment would be defined for R'Class in the body of P, which is bad news, given the task. "),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00287-01"),"}"," ","{",(0,i.kt)("em",null,"AI95-00318-02"),"}"," ","{",(0,i.kt)("em",null,"AI05-0147-1"),"}"," ","{",(0,i.kt)("em",null,"AI12-0172-1"),"}"," ","{",(0,i.kt)("em",null,"AI12-0236-1"),"}"," ","{",(0,i.kt)("em",null,"AI12-0317-1"),"}"," In the following contexts, an ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.4#S0132"},"expression"))," of a limited type is permitted only if each of its operative constituents is newly constructed (see 4.4): "),(0,i.kt)("p",null,"the initialization ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.4#S0132"},"expression"))," of an ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-3.3#S0032"},"object_declaration"))," (see 3.3.1)"),(0,i.kt)("p",null,"the ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-3.7#S0063"},"default_expression"))," of a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-3.8#S0070"},"component_declaration"))," (see 3.8)"),(0,i.kt)("p",null,"the ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.4#S0132"},"expression"))," of a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.3#S0109"},"record_component_association"))," (see 4.3.1)"),(0,i.kt)("p",null,"the ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.4#S0132"},"expression"))," for an ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.3#S0112"},"ancestor_part"))," of an ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.3#S0111"},"extension_aggregate"))," (see 4.3.2)"),(0,i.kt)("p",null,"an ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.4#S0132"},"expression"))," of a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.3#S0114"},"positional_array_aggregate"))," or the ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.4#S0132"},"expression"))," of an ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.3#S0118"},"array_component_association"))," (see 4.3.3)"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0127-1"),"}"," the ",(0,i.kt)("em",null,"base_"),(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.4#S0132"},"expression"))," of a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.3#S0121"},"record_delta_aggregate"))," (see 4.3.4) "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Ramification: "),"{",(0,i.kt)("em",null,"AI12-0127-1"),"}"," We don't need to mention the ",(0,i.kt)("em",null,"base_"),(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.4#S0132"},"expression"))," of an ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.3#S0122"},"array_delta_aggregate"))," here, as its type cannot be limited (see 4.3.4), and thus neither can its ",(0,i.kt)("em",null,"base_"),(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.4#S0132"},"expression")),". Similarly, we do not need any rules for components of a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.3#S0120"},"delta_aggregate"))," nor the elements of a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.3#S0123"},"container_aggregate")),", as neither are allowed to be limited (see 4.3.4 and 4.3.5) "),(0,i.kt)("p",null,"the ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.7#S0163"},"qualified_expression"))," of an initialized allocator (see 4.8)"),(0,i.kt)("p",null,"the ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.4#S0132"},"expression"))," of a return statement (see 6.5)"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0177-1"),"}"," ","{",(0,i.kt)("em",null,"AI12-0157-1"),"}"," the return expression of an expression function (see 6.8)"),(0,i.kt)("p",null,"the ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-3.7#S0063"},"default_expression"))," or actual parameter for a formal object of mode ",(0,i.kt)("strong",null,"in")," (see 12.4)"),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"All of these contexts normally require copying; by restricting the uses as above, we can require the new object to be built-in-place. "),(0,i.kt)("h4",p({},{id:"static-semantics"}),"Static Semantics"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00419-01"),"}"," ","{",(0,i.kt)("em",null,"AI05-0178-1"),"}"," A view of a type is ",(0,i.kt)("em",null,"limited")," if it is one of the following: "),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00411-01"),"}"," ","{",(0,i.kt)("em",null,"AI95-00419-01"),"}"," a type with the reserved word ",(0,i.kt)("strong",null,"limited"),", ",(0,i.kt)("strong",null,"synchronized"),", ",(0,i.kt)("strong",null,"task"),", or ",(0,i.kt)("strong",null,"protected")," in its definition; "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Ramification: "),'Note that there is always a "definition", conceptually, even if there is no syntactic category called "..._definition".'),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00419-01"),"}"," This includes interfaces of the above kinds, derived types with the reserved word ",(0,i.kt)("strong",null,"limited"),", as well as task and protected types. "),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00419-01"),"}"," ","{",(0,i.kt)("em",null,"AI05-0087-1"),"}"," a class-wide type whose specific type is limited;"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00419-01"),"}"," a composite type with a limited component;"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0178-1"),"}"," an incomplete view;"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00419-01"),"}"," a derived type whose parent is limited and is not an interface."),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Ramification: "),"{",(0,i.kt)("em",null,"AI95-00419-01"),"}"," Limitedness is not inherited from interfaces; it must be explicitly specified when the parent is an interface. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"To be honest: "),"{",(0,i.kt)("em",null,"AI95-00419-01"),"}"," A derived type can become nonlimited if ",(0,i.kt)("strong",null,"limited")," does not appear and the derivation takes place in the visible part of a child package, and the parent type is nonlimited as viewed from the private part or body of the child package. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Reason: "),"{",(0,i.kt)("em",null,"AI95-00419-01"),"}"," We considered a rule where limitedness was always inherited from the parent for derived types, but in the case of a type whose parent is an interface, this meant that the first interface is treated differently than other interfaces. It also would have forced users to declare dummy nonlimited interfaces just to get the limitedness right. We also considered a syntax like ",(0,i.kt)("strong",null,"not limited")," to specify nonlimitedness when the parent was limited, but that was unsavory. The rule given is more uniform and simpler to understand."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00419-01"),"}"," The rules for interfaces are asymmetrical, but the language is not: if the parent interface is limited, the presence of the word ",(0,i.kt)("strong",null,"limited")," determines the limitedness, and nonlimited progenitors are illegal by the rules in 3.9.4 if ",(0,i.kt)("strong",null,"limited")," is present. If the parent interface is nonlimited, the word ",(0,i.kt)("strong",null,"limited")," is illegal by the rules in 3.4. The net effect is that the order of the interfaces doesn't matter. "),(0,i.kt)("p",null,"Otherwise, the type is nonlimited."),(0,i.kt)("p",null,"[There are no predefined equality operators for a limited type.]"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0052-1"),"}"," A type is ",(0,i.kt)("em",null,"immutably limited")," if it is one of the following:"),(0,i.kt)("p",null,"An explicitly limited record type;"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0217-1"),"}"," A record extension with the reserved word ",(0,i.kt)("strong",null,"limited"),";"),(0,i.kt)("p",null,"A nonformal limited private type that is tagged or has at least one access discriminant with a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-3.7#S0063"},"default_expression")),";"),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Reason: "),"The full type in both of these cases must necessarily be immutably limited. We need to include private types as much as possible so that we aren't unintentionally discouraging the use of private types. "),(0,i.kt)("p",null,"A task type, a protected type, or a synchronized interface;"),(0,i.kt)("p",null,"A type derived from an immutably limited type."),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"An immutably limited type is a type that cannot become nonlimited subsequently in a private part or in a child unit. If a view of the type makes it immutably limited, then no copying (assignment) operations are ever available for objects of the type. This allows other properties; for instance, it is safe for such objects to have access discriminants that have defaults or designate other limited objects. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Ramification: "),"A nonsynchronized limited interface type is not immutably limited; a type derived from it can be nonlimited. "),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0052-1"),"}"," A descendant of a generic formal limited private type is presumed to be immutably limited except within the body of a generic unit or a body declared within the declarative region of a generic unit, if the formal type is declared within the formal part of the generic unit."),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Ramification: "),"In an instance, a type is descended from the actual type corresponding to the formal, and all rules are rechecked in the specification. Bodies are excepted so that we assume the worst there; the complex wording is required to handle children of generics and unrelated bodies properly. "),(0,i.kt)("p",null,"NOTE 1   ","{",(0,i.kt)("em",null,"AI95-00287-01"),"}"," ","{",(0,i.kt)("em",null,"AI95-00318-02"),"}"," ","{",(0,i.kt)("em",null,"AI05-0067-1"),"}"," ","{",(0,i.kt)("em",null,"AI12-0442-1"),"}"," While it is allowed to write initializations of limited objects, such initializations never copy a limited object. The source of such an assignment operation will be an ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.3#S0106"},"aggregate"))," or ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-6.4#S0218"},"function_call")),", and such ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.3#S0106"},"aggregate")),"s and ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-6.4#S0218"},"function_call")),"s will be built directly in the target object (see 7.6). "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"To be honest: "),"This isn't quite true if the type can become nonlimited (see below); ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-6.4#S0218"},"function_call")),'s only are required to be build-in-place for "really" limited types. '),(0,i.kt)("p",null,(0,i.kt)("em",null,"Paragraphs 10 through 15 were deleted.")," "),(0,i.kt)("p",null,"NOTE 2   As illustrated in 7.3.1, an untagged limited type can become nonlimited under certain circumstances. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Ramification: "),"Limited private types do not become nonlimited; instead, their full view can be nonlimited, which has a similar effect."),(0,i.kt)("p",null,'It is important to remember that a single nonprivate type can be both limited and nonlimited in different parts of its scope. In other words, "limited" is a property that depends on where you are in the scope of the type. We don\'t call this a "view property" because there is no particular declaration to declare the nonlimited view.'),(0,i.kt)("p",null,"Tagged types never become nonlimited. "),(0,i.kt)("h4",p({},{id:"examples"}),"Examples"),(0,i.kt)("p",null,(0,i.kt)("em",null,"Example of a package with a limited type:")," "),(0,i.kt)(l.Z,{mdxType:"CodeBlock"},"package IO_Package is","\n","   type File_Name is limited private;","\n"),(0,i.kt)(l.Z,{mdxType:"CodeBlock"},"procedure Open (F : in out File_Name);","\n","   procedure Close(F : in out File_Name);","\n","   procedure Read (F : in File_Name; Item : out Integer);","\n","   procedure Write(F : in File_Name; Item : in  Integer);","\n","private","\n","   type File_Name is","\n","      limited record","\n","         Internal_Name : Integer := 0;","\n","      end record;","\n","end IO_Package;","\n"),(0,i.kt)(l.Z,{mdxType:"CodeBlock"},"package body IO_Package is","\n","   Limit : constant := 200;","\n","   type File_Descriptor is record  ...  end record;","\n","   Directory : array (1 .. Limit) of File_Descriptor;","\n","   ...","\n","   procedure Open (F : in out File_Name) is  ...  end;","\n","   procedure Close(F : in out File_Name) is  ...  end;","\n","   procedure Read (F : in File_Name; Item : out Integer) is ... end;","\n","   procedure Write(F : in File_Name; Item : in  Integer) is ... end;","\n","begin","\n","   ...","\n","end IO_Package;","\n"),(0,i.kt)("p",null,"NOTE 3   ","{",(0,i.kt)("em",null,"AI12-0440-1"),"}"," ",(0,i.kt)("em",null,"Notes on the example:")," In the example above, an outside subprogram making use of IO_Package can obtain a file name by calling Open and later use it in calls to Read and Write. Thus, outside the package, a file name obtained from Open acts as a kind of password; its internal properties (such as containing a numeric value) are not known and no other operations (such as addition or comparison of internal names) can be performed on a file name. Most importantly, clients of the package cannot make copies of objects of type File_Name."),(0,i.kt)("p",null,"This example is characteristic of any case where complete control over the operations of a type is desired. Such packages serve a dual purpose. They prevent a user from making use of the internal structure of the type. They also implement the notion of an encapsulated data type where the only operations on the type are those given in the package specification."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00318-02"),"}"," The fact that the full view of File_Name is explicitly declared ",(0,i.kt)("strong",null,"limited")," means that parameter passing will always be by reference and function results will always be built directly in the result object (see 6.2 and 6.5)."),(0,i.kt)("h4",p({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,i.kt)("p",null,"The restrictions in RM83-7.4.4(4), which disallowed ",(0,i.kt)("strong",null,"out")," parameters of limited types in certain cases, are removed. "),(0,i.kt)("h4",p({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0299-1"),"}",' Since limitedness and privateness are orthogonal in Ada 95 (and to some extent in Ada 83), this is now its own subclause rather than being a subclause of 7.3, "Private Types and Private Extensions". '),(0,i.kt)("h4",p({},{id:"extensions-to-ada-95"}),"Extensions to Ada 95"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00287-01"),"}"," ","{",(0,i.kt)("em",null,"AI95-00318-02"),"}"," Limited types now have an assignment operation, but its use is restricted such that all uses are build-in-place. This is accomplished by restricting uses to ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.3#S0106"},"aggregate")),"s and ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-6.4#S0218"},"function_call")),"s. ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.3#S0106"},"Aggregate")),'s were not allowed to have a limited type in Ada 95, which causes a compatibility issue discussed in 4.3, "Aggregates". Compatibility issues with return statements for limited ',(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-6.4#S0218"},"function_call")),'s are discussed in 6.5, "Return Statements". '),(0,i.kt)("h4",p({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00411-01"),"}"," ","{",(0,i.kt)("em",null,"AI95-00419-01"),"}"," Rewrote the definition of limited to ensure that interfaces are covered, but that limitedness is not inherited from interfaces. Derived types that explicitly include ",(0,i.kt)("strong",null,"limited")," are now also covered. "),(0,i.kt)("h4",p({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0052-1"),"}"," ","{",(0,i.kt)("em",null,"AI05-0217-1"),"}"," ",(0,i.kt)("strong",null,"Correction:")," Added a definition for immutably limited types, so that the fairly complex definition does not need to be repeated in rules elsewhere in the Reference Manual."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0067-1"),"}"," ","{",(0,i.kt)("em",null,"AI05-0299-1"),"}"," ",(0,i.kt)("strong",null,"Correction:")," The built-in-place rules are consolidated in 7.6, and thus they are removed from this subclause."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0087-1"),"}"," ",(0,i.kt)("strong",null,"Correction:")," Fixed an oversight: class-wide types were never defined to be limited, even if their associated specific type is. It is thought that this oversight was never implemented incorrectly by any compiler, thus we have not classified it as an incompatibility."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0147-1"),"}"," Allowed ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.5#S0148"},"conditional_expression")),"s in limited constructor contexts - we want to treat these as closely to parentheses as possible."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0177-1"),"}"," Added wording so that expression functions can return limited entities."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0178-1"),"}"," ",(0,i.kt)("strong",null,"Correction:")," Added incomplete views to the list of reasons for a view of a type to be limited. This is not a change as the definition already was in 3.10.1. But it is much better to have all of the reasons for limitedness together. "),(0,i.kt)("h4",p({},{id:"extensions-to-ada-2012"}),"Extensions to Ada 2012"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0172-1"),"}"," ",(0,i.kt)("strong",null,"Correction:")," A ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-11.3#S0309"},"raise_expression"))," can be used in an expression used in a limited context. This is harmless (no object will be created if an exception is raised instead), useful, and thus appears to have been an omission when ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-11.3#S0309"},"raise_expression")),"s were added to the language. "),(0,i.kt)("h4",p({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0127-1"),"}"," Added the ",(0,i.kt)("em",null,"base_"),(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.4#S0132"},"expression"))," of a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.3#S0120"},"delta_aggregate"))," as a limited context. "))}A.isMDXComponent=!0}}]);