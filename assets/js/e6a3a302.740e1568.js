"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[636],{98209:(e,a,n)=>{n.r(a),n.d(a,{assets:()=>c,contentTitle:()=>A,default:()=>f,frontMatter:()=>d,metadata:()=>g,toc:()=>u});var t=n(58168),i=(n(96540),n(15680)),o=n(20793),r=n(91435),l=n(21432),y=n(79162),s=n(34421);const d={sidebar_position:19},A="3.1 Declarations",g={unversionedId:"arm/AA-3/AA-3.1",id:"arm/AA-3/AA-3.1",title:"3.1 Declarations",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-3/AA-3.1.mdx",sourceDirName:"arm/AA-3",slug:"/arm/AA-3/AA-3.1",permalink:"/docs/arm/AA-3/AA-3.1",draft:!1,tags:[],version:"current",sidebarPosition:19,frontMatter:{sidebar_position:19},sidebar:"referenceManualSidebar",previous:{title:"3 Declarations and Types",permalink:"/docs/arm/AA-3/"},next:{title:"3.2 Types and Subtypes",permalink:"/docs/arm/AA-3/AA-3.2"}},c={},u=[{value:"Syntax",id:"syntax",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4}],m={toc:u},h="wrapper";function f(e){let{components:a,...n}=e;return(0,i.yg)(h,(0,t.A)({},m,n,{components:a,mdxType:"MDXLayout"}),(0,i.yg)("h1",{id:"31-declarations"},"3.1 Declarations"),(0,i.yg)("admonition",{type:"warning"},(0,i.yg)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,i.yg)("a",{parentName:"p",href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"},"tracking issue"))),(0,i.yg)(y.A,{mdxType:"MarginText"},"1/5"),(0,i.yg)(s.A,{items:["AI12-0373-1"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"The language defines several kinds of named ",(0,i.yg)("em",null,"entities")," that are declared by declarations. The entity's ",(0,i.yg)("em",null,"name")," is defined by the declaration, usually by a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.1#S0022"},"defining","_","identifier")),", but sometimes by a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.5#S0040"},"defining","_","character","_","literal"))," or ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.1#S0203"},"defining","_","operator","_","symbol")),". There are also entities that are not directly declared; some of these are elements of other entities, or are allocated dynamically. Such entities can be denoted using ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0096"},"indexed_component")),", ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0098"},"selected_component")),", or dereference ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),"s (see ",(0,i.yg)("a",{href:"../AA-4/AA-4.1"},"4.1"),").",(0,i.yg)("br",null)),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"1.a/5"),(0,i.yg)(s.A,{items:["AI12-0373-1"],mdxType:"MarginInfo"}),(0,i.yg)(o.A,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,i.yg)("strong",null),"Some entities are always anonymous. For instance, a type is never named (the name represents the first subtype). We don't mention those here as this paragraph is about ",(0,i.yg)("em",null,"named")," entities. ",(0,i.yg)("br",null))),(0,i.yg)(y.A,{mdxType:"MarginText"},"2"),(0,i.yg)("p",null,"There are several forms of declaration. A ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.1#S0021"},"basic_declaration"))," is a form of declaration defined as follows. ",(0,i.yg)("br",null)),(0,i.yg)("h4",{id:"syntax"},"Syntax"),(0,i.yg)(y.A,{mdxType:"MarginText"},"3/3"),(0,i.yg)(s.A,{items:["AI95-00348-01","AI05-0177-1"],mdxType:"MarginInfo"}),(0,i.yg)(l.A,{mdxType:"CodeBlock"},(0,i.yg)("code",null,"basic","_","declaration"),(0,i.yg)("a",{id:"S0021"}),(0,i.yg)("code",null," ::= "),(0,i.yg)("br",null),"     ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.2#S0023"},"type_declaration"))," | ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.2#S0026"},"subtype_declaration")),(0,i.yg)("br",null),"   | ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.3#S0032"},"object_declaration"))," | ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.3#S0034"},"number_declaration")),(0,i.yg)("br",null),"   | ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.1#S0195"},"subprogram_declaration"))," | ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.9#S0076"},"abstract_subprogram_declaration")),(0,i.yg)("br",null),"   | ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.7#S0227"},"null_procedure_declaration"))," | ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.8#S0228"},"expression_function_declaration")),(0,i.yg)("br",null),"   | ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-7/AA-7.1#S0229"},"package_declaration"))," | ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-8/AA-8.5#S0238"},"renaming_declaration")),(0,i.yg)("br",null),"   | ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-11/AA-11.1#S0303"},"exception_declaration"))," | ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-12/AA-12.1#S0310"},"generic_declaration")),(0,i.yg)("br",null),"   | ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-12/AA-12.3#S0315"},"generic_instantiation")),(0,i.yg)("br",null)),(0,i.yg)(y.A,{mdxType:"MarginText"},"4"),(0,i.yg)(l.A,{mdxType:"CodeBlock"},(0,i.yg)("code",null,"defining","_","identifier"),(0,i.yg)("a",{id:"S0022"}),(0,i.yg)("code",null," ::= "),(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier")),(0,i.yg)("br",null)),(0,i.yg)("h4",{id:"static-semantics"},"Static Semantics"),(0,i.yg)(y.A,{mdxType:"MarginText"},"5"),(0,i.yg)("p",null,"A ",(0,i.yg)("em",null,"declaration")," is a language construct that associates a name with (a view of) an entity. A declaration may appear explicitly in the program text (an ",(0,i.yg)("em",null,"explicit")," declaration), or may be supposed to occur at a given place in the text as a consequence of the semantics of another construct (an ",(0,i.yg)("em",null,"implicit")," declaration). ",(0,i.yg)("br",null)),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"5.a"),(0,i.yg)(o.A,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,i.yg)("strong",null),"An implicit declaration generally declares a predefined or inherited operation associated with the definition of a type. This term is used primarily when allowing explicit declarations to override implicit declarations, as part of a type declaration. ",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"5.b/5"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,i.yg)("strong",null,"Term entry: "),(0,i.yg)("strong",null,"declaration")," \u2014 language construct that associates a name with (a view of) an entity",(0,i.yg)("br",null),"Note: A declaration can appear explicitly in the program text (an explicit declaration), or can be supposed to occur at a given place in the text as a consequence of the semantics of another construct (an implicit declaration).",(0,i.yg)("br",null))),(0,i.yg)(y.A,{mdxType:"MarginText"},"6/5"),(0,i.yg)(s.A,{items:["AI95-00318-02","AI05-0255-1","AI05-0277-1","AI12-0061-1","AI12-0308-1"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"Each of the following is defined to be a declaration: any ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.1#S0021"},"basic","_","declaration")),"; an ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.5#S0039"},"enumeration","_","literal","_","specification")),"; a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.7#S0062"},"discriminant","_","specification")),"; a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.8#S0070"},"component","_","declaration")),"; a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.1#S0022"},"defining_identifier"))," of an ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.3#S0119"},"iterated","_","component","_","association")),"; a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-5/AA-5.5#S0181"},"loop","_","parameter","_","specification")),"; a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.1#S0022"},"defining_identifier"))," of a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-5/AA-5.5#S0180"},"chunk","_","specification")),"; an ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-5/AA-5.5#S0183"},"iterator","_","specification")),"; a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.1#S0022"},"defining_identifier"))," of an ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-5/AA-5.5#S0186"},"iterator","_","parameter","_","specification")),"; a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.1#S0207"},"parameter","_","specification")),"; a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram","_","body")),"; an ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.5#S0224"},"extended","_","return","_","object","_","declaration")),"; an ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-9/AA-9.5#S0257"},"entry","_","declaration")),"; an ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-9/AA-9.5#S0263"},"entry","_","index","_","specification")),"; a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-11/AA-11.2#S0306"},"choice","_","parameter","_","specification")),"; a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-12/AA-12.1#S0314"},"generic","_","formal","_","parameter","_","declaration")),". ",(0,i.yg)("br",null)),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"6.a"),(0,i.yg)(o.A,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,i.yg)("strong",null),"This list (when ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.1#S0021"},"basic_declaration")),' is expanded out) contains all syntactic categories that end in "',"_",'declaration" or "',"_",'specification", except for program unit ',"_","specifications. Moreover, it contains ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram_body")),". A ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram_body"))," is a declaration, whether or not it completes a previous declaration. This is a bit strange, ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram_body"))," is not part of the syntax of ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.1#S0021"},"basic_declaration"))," or ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-10/AA-10.1#S0288"},"library_unit_declaration")),". A renaming-as-body is considered a declaration. An ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-9/AA-9.5#S0258"},"accept_statement"))," is not considered a declaration. Completions are sometimes declarations, and sometimes not. ",(0,i.yg)("br",null))),(0,i.yg)(y.A,{mdxType:"MarginText"},"7"),(0,i.yg)("p",null,"All declarations contain a ",(0,i.yg)("em",null,"definition")," for a ",(0,i.yg)("em",null,"view")," of an entity. A view consists of an identification of the entity (the entity ",(0,i.yg)("em",null,"of")," the view), plus view-specific characteristics that affect the use of the entity through that view (such as mode of access to an object, formal parameter names and defaults for a subprogram, or visibility to components of a type). In most cases, a declaration also contains the definition for the entity itself (a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-8/AA-8.5#S0238"},"renaming_declaration"))," is an example of a declaration that does not define a new entity, but instead defines a view of an existing entity (see ",(0,i.yg)("a",{href:"../AA-8/AA-8.5"},"8.5"),")).",(0,i.yg)("br",null)),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"7.a.1/5"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,i.yg)("strong",null,"Term entry: "),(0,i.yg)("strong",null,"view of an entity")," \u2014 representation of an entity that reveals some or all of the properties of the entity",(0,i.yg)("br",null),"Note: A single entity can have multiple views.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"7.a"),(0,i.yg)(o.A,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,i.yg)("strong",null),"Most declarations define a view (of some entity) whose view-specific characteristics are unchanging for the life of the view. However, subtypes are somewhat unusual in that they inherit characteristics from whatever view of their type is currently visible. Hence, a subtype is not a ",(0,i.yg)("em",null,"view")," of a type; it is more of an indirect reference. By contrast, a private type provides a single, unchanging (partial) view of its full type. ",(0,i.yg)("br",null))),(0,i.yg)(y.A,{mdxType:"MarginText"},"7.1/3"),(0,i.yg)(s.A,{items:["AI05-0080-1"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"When it is clear from context, the term ",(0,i.yg)("em",null,"object")," is used in place of ",(0,i.yg)("em",null,"view of an object"),". Similarly, the terms ",(0,i.yg)("em",null,"type")," and ",(0,i.yg)("em",null,"subtype")," are used in place of ",(0,i.yg)("em",null,"view of a type")," and ",(0,i.yg)("em",null,"view of a subtype"),", respectively.",(0,i.yg)("br",null)),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"7.b/3"),(0,i.yg)(o.A,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,i.yg)("strong",null),"Rules interpreted at compile time generally refer to views of entities, rather than the entities themselves. This is necessary to preserve privacy; characteristics that are not visible should not be used in compile-time rules. Thus, Static Semantics and Legality Rules generally implicitly have \u201cview of\u201d. Legality Rules that need to look into the private part are the exception to this interpretation.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"7.c/3"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"On the other hand, run-time rules can work either way, so \u201cview of\u201d should not be assumed in Dynamic Semantics rules.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"7.d/5"),(0,i.yg)(s.A,{items:["AI12-0191-1"],mdxType:"MarginInfo"}),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"For example, a reference to the components of an object in a rule that is interpreted at compile time would not apply to components that are not visible. On the other hand, a reference to the components of an object in a dynamic semantics rule would apply to all components of the object, visible or not, including (for tagged objects) components which are not components of the nominal type of the object (see ",(0,i.yg)("a",{href:"../AA-3/AA-3.9#Subclause_3.9.1"},"3.9.1"),"). Other terms, such as \u201csubcomponent\u201d and \u201cpart\u201d, are interpreted analogously. ",(0,i.yg)("br",null))),(0,i.yg)(y.A,{mdxType:"MarginText"},"8"),(0,i.yg)("p",null,"For each declaration, the language rules define a certain region of text called the ",(0,i.yg)("em",null,"scope")," of the declaration (see ",(0,i.yg)("a",{href:"../AA-8/AA-8.2"},"8.2"),"). Most declarations associate an ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier"))," with a declared entity. Within its scope, and only there, there are places where it is possible to use the ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier"))," to refer to the declaration, the view it defines, and the associated entity; these places are defined by the visibility rules (see ",(0,i.yg)("a",{href:"../AA-8/AA-8.3"},"8.3"),"). At such places the ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier"))," is said to be a ",(0,i.yg)("em",null,"name")," of the entity (the ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0092"},"direct_name"))," or ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0099"},"selector_name")),"); the name is said to ",(0,i.yg)("em",null,"denote")," the declaration, the view, and the associated entity (see ",(0,i.yg)("a",{href:"../AA-8/AA-8.6"},"8.6"),"). The declaration is said to ",(0,i.yg)("em",null,"declare")," the name, the view, and in most cases, the entity itself.",(0,i.yg)("br",null)),(0,i.yg)(y.A,{mdxType:"MarginText"},"9"),(0,i.yg)("p",null,"As an alternative to an ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier")),", an enumeration literal can be declared with a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.5#S0015"},"character_literal"))," as its name (see ",(0,i.yg)("a",{href:"../AA-3/AA-3.5#Subclause_3.5.1"},"3.5.1"),"), and a function can be declared with an ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.1#S0202"},"operator_symbol"))," as its name (see ",(0,i.yg)("a",{href:"../AA-6/AA-6.1"},"6.1"),").",(0,i.yg)("br",null)),(0,i.yg)(y.A,{mdxType:"MarginText"},"10"),(0,i.yg)("p",null,"The syntax rules use the terms ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.1#S0022"},"defining_identifier")),", ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.5#S0040"},"defining","_","character","_","literal")),", and ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.1#S0203"},"defining","_","operator","_","symbol"))," for the defining occurrence of a name; these are collectively called ",(0,i.yg)("em",null,"defining names"),". The terms ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0092"},"direct_name"))," and ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0099"},"selector_name"))," are used for usage occurrences of ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier")),"s, ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.5#S0015"},"character_literal")),"s, and ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.1#S0202"},"operator_symbol")),"s. These are collectively called ",(0,i.yg)("em",null,"usage names"),". ",(0,i.yg)("br",null)),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"10.a"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,i.yg)("strong",null,"To be honest: "),"The terms ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier")),", ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.5#S0015"},"character_literal")),", and ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.1#S0202"},"operator_symbol"))," are used directly in contexts where the normal visibility rules do not apply (such as the ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier"))," that appears after the ",(0,i.yg)("strong",null,"end")," of a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-9/AA-9.1#S0248"},"task_body")),"). Analogous conventions apply to the use of ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.1#S0199"},"designator")),", which is the collective term for ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier"))," and ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.1#S0202"},"operator_symbol")),". ",(0,i.yg)("br",null))),(0,i.yg)("h4",{id:"dynamic-semantics"},"Dynamic Semantics"),(0,i.yg)(y.A,{mdxType:"MarginText"},"11"),(0,i.yg)("p",null,"The process by which a construct achieves its run-time effect is called ",(0,i.yg)("em",null,"execution"),". This process is also called ",(0,i.yg)("em",null,"elaboration")," for declarations and ",(0,i.yg)("em",null,"evaluation")," for expressions. One of the terms execution, elaboration, or evaluation is defined by this Reference Manual for each construct that has a run-time effect. ",(0,i.yg)("br",null)),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"11.a.1/5"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,i.yg)("strong",null,"Term entry: "),(0,i.yg)("strong",null,"execution")," \u2014 process by which a construct achieves its run-time effect",(0,i.yg)("br",null),"Note: Execution of a declaration is also called elaboration. Execution of an expression is also called evaluation.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"11.a"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,i.yg)("strong",null,"To be honest: "),"The term elaboration is also used for the execution of certain constructs that are not declarations, and the term evaluation is used for the execution of certain constructs that are not expressions. For example, ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.2#S0027"},"subtype_indication")),"s are elaborated, and ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.5#S0037"},"range")),"s are evaluated.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"11.b"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"For bodies, execution and elaboration are both explicitly defined. When we refer specifically to the execution of a body, we mean the explicit definition of execution for that kind of body, not its elaboration. ",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"11.c"),(0,i.yg)(o.A,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,i.yg)("strong",null),'Technically, "the execution of a declaration" and "the elaboration of a declaration" are synonymous. We use the term "elaboration" of a construct when we know the construct is elaborable. When we are talking about more arbitrary constructs, we use the term "execution". For example, we use the term "erroneous execution", to refer to any erroneous execution, including erroneous elaboration or evaluation.',(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"11.d"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"When we explicitly define evaluation or elaboration for a construct, we are implicitly defining execution of that construct.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"11.e"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},'We also use the term "execution" for things like ',(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-5/AA-5.1#S0167"},"statement")),'s, which are executable, but neither elaborable nor evaluable. We considered using the term "execution" only for nonelaborable, nonevaluable constructs, and defining the term "action" to mean what we have defined "execution" to mean. We rejected this idea because we thought three terms that mean the same thing was enough \u2014 four would be overkill. Thus, the term "action" is used only informally in the standard (except where it is defined as part of a larger term, such as "protected action"). ',(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"11.e.1/5"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,i.yg)("strong",null,"Term entry: "),(0,i.yg)("strong",null,"elaboration")," \u2014 process by which a declaration achieves its run-time effect",(0,i.yg)("br",null),"Note: Elaboration is one of the forms of execution.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"11.e.2/5"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,i.yg)("strong",null,"Term entry: "),(0,i.yg)("strong",null,"evaluation")," \u2014 process by which an expression achieves its run-time effect",(0,i.yg)("br",null),"Note: Evaluation is one of the forms of execution.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"11.f"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,i.yg)("strong",null,"To be honest: "),"A construct is ",(0,i.yg)("em",null,"elaborable")," if elaboration is defined for it. A construct is ",(0,i.yg)("em",null,"evaluable")," if evaluation is defined for it. A construct is ",(0,i.yg)("em",null,"executable")," if execution is defined for it. ",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"11.g"),(0,i.yg)(o.A,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,i.yg)("strong",null),"Don't confuse \u201celaborable\u201d with \u201cpreelaborable\u201d (defined in ",(0,i.yg)("a",{href:"../AA-10/AA-10.2#Subclause_10.2.1"},"10.2.1"),").",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"11.h/2"),(0,i.yg)(s.A,{items:["AI95-00114-01"],mdxType:"MarginInfo"}),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Evaluation of an evaluable construct produces a result that is either a value, a denotation, or a range. The following are evaluable: expression; ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0093"},"prefix")),"; ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.5#S0037"},"range")),"; ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-9/AA-9.5#S0263"},"entry_index_specification")),"; and possibly ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.6#S0058"},"discrete_range")),". The last one is curious \u2014 RM83 uses the term \u201cevaluation of a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.6#S0058"},"discrete_range")),"\u201d, but never defines it. One might presume that the evaluation of a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.6#S0058"},"discrete_range"))," consists of the evaluation of the ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.5#S0037"},"range"))," or the ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.2#S0027"},"subtype_indication")),", depending on what it is. But ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.2#S0027"},"subtype_indication")),"s are not evaluated; they are elaborated.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"11.i"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Intuitively, an ",(0,i.yg)("em",null,"executable")," construct is one that has a defined run-time effect (which may be null). Since execution includes elaboration and evaluation as special cases, all elaborable and all evaluable constructs are also executable. Hence, most constructs in Ada are executable. An important exception is that the constructs inside a generic unit are not executable directly, but rather are used as a template for (generally) executable constructs in instances of the generic. ",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE   At compile time, the declaration of an entity ",(0,i.yg)("em",null,"declares")," the entity. At run time, the elaboration of the declaration ",(0,i.yg)("em",null,"creates")," the entity. ",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.a"),(0,i.yg)(o.A,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,i.yg)("strong",null),"Syntactic categories for declarations are named either ",(0,i.yg)("em",null,"entity","_"),(0,i.yg)("code",null,"declaration")," (if they include a trailing semicolon) or ",(0,i.yg)("em",null,"entity","_"),(0,i.yg)("code",null,"specification")," (if not).",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.b"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"The various kinds of named entities that can be declared are as follows: an object (including components and parameters), a named number, a type (the name always refers to its first subtype), a subtype, a subprogram (including enumeration literals and operators), a single entry, an entry family, a package, a protected or task unit (which corresponds to either a type or a single object), an exception, a generic unit, a label, and the name of a statement.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.c"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Identifiers are also associated with names of pragmas, arguments to pragmas, and with attributes, but these are not user-definable. ",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)("h4",{id:"wording-changes-from-ada-83"},"Wording Changes from Ada 83")),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.d"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"The syntax rule for ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.1#S0022"},"defining_identifier"))," is new. It is used for the defining occurrence of an ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier")),". Usage occurrences use the ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0092"},"direct_name"))," or ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0099"},"selector_name"))," syntactic categories. Each occurrence of an ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier"))," (or ",(0,i.yg)("code",null,"simple","_","name"),"), ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.5#S0015"},"character_literal")),", or ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.1#S0202"},"operator_symbol"))," in the Ada 83 syntax rules is handled as follows in Ada 95: ",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.e"),(0,i.yg)("ul",null,(0,i.yg)("li",null,"It becomes a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.1#S0022"},"defining_identifier")),", ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.5#S0040"},"defining_character_literal")),", or ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.1#S0203"},"defining_operator_symbol"))," (or some syntactic category composed of these), to indicate a defining occurrence;",(0,i.yg)("br",null)),(0,i.yg)(y.A,{mdxType:"MarginText"},"12.f"),(0,i.yg)("li",null,"It becomes a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0092"},"direct_name")),", in usage occurrences where the usage is required (in Clause ",(0,i.yg)("a",{href:"../AA-8/"},"8")," to be directly visible;",(0,i.yg)("br",null)),(0,i.yg)(y.A,{mdxType:"MarginText"},"12.g"),(0,i.yg)("li",null,"It becomes a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0099"},"selector_name")),", in usage occurrences where the usage is required (in Clause ",(0,i.yg)("a",{href:"../AA-8/"},"8"),") to be visible but not necessarily directly visible;",(0,i.yg)("br",null)),(0,i.yg)(y.A,{mdxType:"MarginText"},"12.h"),(0,i.yg)("li",null,"It remains an ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier")),", ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.5#S0015"},"character_literal")),", or ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.1#S0202"},"operator_symbol")),", in cases where the visibility rules do not apply (such as the ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.1#S0199"},"designator"))," that appears after the ",(0,i.yg)("strong",null,"end")," of a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram_body")),"). ",(0,i.yg)("br",null)))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.i"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"For declarations that come in \u201ctwo parts\u201d (program unit declaration plus body, private or incomplete type plus full type, deferred constant plus full constant), we consider both to be defining occurrences. Thus, for example, the syntax for ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-7/AA-7.2#S0231"},"package_body"))," uses ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.1#S0022"},"defining_identifier"))," after the reserved word ",(0,i.yg)("strong",null,"body"),", as opposed to ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0092"},"direct_name")),".",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.j"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"The defining occurrence of a statement name is in its implicit declaration, not where it appears in the program text. Considering the statement name itself to be the defining occurrence would complicate the visibility rules.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.k"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"The phrase \u201cvisible by selection\u201d is not used in Ada 95. It is subsumed by simply \u201cvisible\u201d and the Name Resolution Rules for ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0099"},"selector_name")),"s.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.l/3"),(0,i.yg)(s.A,{items:["AI05-0299-1"],mdxType:"MarginInfo"}),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"(Note that in Ada 95, a declaration is visible at all places where one could have used a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0099"},"selector_name")),", not just at places where a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-4/AA-4.1#S0099"},"selector_name"))," was actually used. Thus, the places where a declaration is directly visible are a subset of the places where it is visible. See Clause ",(0,i.yg)("a",{href:"../AA-8/"},"8")," for details.)",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.m"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"We use the term \u201cdeclaration\u201d to cover ",(0,i.yg)("code",null,"_","specification"),"s that declare (views of) objects, such as ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.1#S0207"},"parameter_specification")),"s. In Ada 83, these are referred to as a \u201cform of declaration\u201d, but it is not entirely clear that they are considered simply \u201cdeclarations\u201d.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.n/3"),(0,i.yg)(s.A,{items:["AI05-0299-1"],mdxType:"MarginInfo"}),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},'RM83 contains an incomplete definition of "elaborated" in this subclause: it defines "elaborated" for declarations, ',(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.11#S0086"},"declarative_part")),"s, ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.11#S0087"},"declarative_item")),"s and ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-10/AA-10.1#S0286"},"compilation_unit")),'s, but "elaboration" is defined elsewhere for various other constructs. To make matters worse, Ada 95 has a different set of elaborable constructs. Instead of correcting the list, it is more maintainable to refer to the term "elaborable," which is defined in a distributed manner.',(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.o"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"RM83 uses the term \u201chas no other effect\u201d to describe an elaboration that doesn't do anything except change the state from not-yet-elaborated to elaborated. This was a confusing wording, because the answer to \u201cother than what?\u201d was to be found many pages away. In Ada 95, we change this wording to \u201chas no effect\u201d (for things that truly do nothing at run time), and \u201chas no effect other than to establish that so-and-so can happen without failing the Elaboration","_","Check\u201d (for things where it matters).",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.p"),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},'We make it clearer that the term "execution" covers elaboration and evaluation as special cases. This was implied in RM83. For example, "erroneous execution" can include any execution, and RM83-9.4(3) has, "The task designated by any other task object depends on the master whose execution creates the task object;" the elaboration of the master\'s ',(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-3/AA-3.11#S0086"},"declarative_part"))," is doing the task creation. ",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)("h4",{id:"wording-changes-from-ada-95"},"Wording Changes from Ada 95")),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.q/2"),(0,i.yg)(s.A,{items:["AI95-00318-02"],mdxType:"MarginInfo"}),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Added ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-6/AA-6.5#S0225"},"extended_return_statement"))," to the list of declarations.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.r/2"),(0,i.yg)(s.A,{items:["AI95-00348-01"],mdxType:"MarginInfo"}),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Added null procedures (see ",(0,i.yg)("a",{href:"../AA-6/AA-6.7"},"6.7"),") to the syntax. ",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)("h4",{id:"wording-changes-from-ada-2005"},"Wording Changes from Ada 2005")),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(y.A,{mdxType:"MarginText"},"12.s/3"),(0,i.yg)(s.A,{items:["AI05-0177-1"],mdxType:"MarginInfo"}),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Added expression functions (see ",(0,i.yg)("a",{href:"../AA-6/AA-6.8"},"6.8"),") to the syntax. ",(0,i.yg)("br",null))))}f.isMDXComponent=!0}}]);