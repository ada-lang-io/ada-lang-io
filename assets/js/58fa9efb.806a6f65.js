"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[6312],{4298:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>m,contentTitle:()=>f,default:()=>b,frontMatter:()=>h,metadata:()=>k,toc:()=>p});var n=a(1716),i=Object.defineProperty,o=Object.defineProperties,l=Object.getOwnPropertyDescriptors,r=Object.getOwnPropertySymbols,s=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,d=(e,t,a)=>t in e?i(e,t,{enumerable:!0,configurable:!0,writable:!0,value:a}):e[t]=a,u=(e,t)=>{for(var a in t||(t={}))s.call(t,a)&&d(e,a,t[a]);if(r)for(var a of r(t))c.call(t,a)&&d(e,a,t[a]);return e};const h={sidebar_position:17},f="3.1  Declarations",k={unversionedId:"arm/AA-3.1",id:"arm/AA-3.1",title:"3.1  Declarations",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-3.1.mdx",sourceDirName:"arm",slug:"/arm/AA-3.1",permalink:"/docs/arm/AA-3.1",draft:!1,tags:[],version:"current",sidebarPosition:17,frontMatter:{sidebar_position:17},sidebar:"tutorialSidebar",previous:{title:"3 Declarations and Types",permalink:"/docs/arm/AA-3"},next:{title:"3.2  Types and Subtypes",permalink:"/docs/arm/AA-3.2"}},m={},p=[{value:"Syntax",id:"syntax",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4}],A={toc:p};function b(e){var t,a=e,{components:i}=a,d=((e,t)=>{var a={};for(var n in e)s.call(e,n)&&t.indexOf(n)<0&&(a[n]=e[n]);if(null!=e&&r)for(var n of r(e))t.indexOf(n)<0&&c.call(e,n)&&(a[n]=e[n]);return a})(a,["components"]);return(0,n.kt)("wrapper",(t=u(u({},A),d),o(t,l({components:i,mdxType:"MDXLayout"}))),(0,n.kt)("h1",u({},{id:"31--declarations"}),"3.1  Declarations"),(0,n.kt)("admonition",u({},{type:"warning"}),(0,n.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,n.kt)("a",u({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI12-0373-1"),"}"," The language defines several kinds of named ",(0,n.kt)("em",null,"entities")," that are declared by declarations. The entity's ",(0,n.kt)("em",null,"name")," is defined by the declaration, usually by a ",(0,n.kt)("code",null,"[defining_identifier](./AA-3.1#S0022)"),", but sometimes by a ",(0,n.kt)("code",null,"[defining_character_literal](./AA-3.5#S0040)")," or ",(0,n.kt)("code",null,"[defining_operator_symbol](./AA-6.1#S0203)"),". There are also entities that are not directly declared; some of these are elements of other entities, or are allocated dynamically. Such entities can be denoted using ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0096"},"indexed_component")),", ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0098"},"selected_component")),", or dereference ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0091"},"name")),"s (see 4.1)."),(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),"{",(0,n.kt)("em",null,"AI12-0373-1"),"}"," Some entities are always anonymous. For instance, a type is never named (the name represents the first subtype). We don't mention those here as this paragraph is about ",(0,n.kt)("em",null,"named")," entities. "),(0,n.kt)("p",null,"There are several forms of declaration. A ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.1#S0021"},"basic_declaration"))," is a form of declaration defined as follows. "),(0,n.kt)("h4",u({},{id:"syntax"}),"Syntax"),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI95-00348-01"),"}"," ","{",(0,n.kt)("em",null,"AI05-0177-1"),"}"," ",(0,n.kt)("code",null,"basic_declaration"),(0,n.kt)("a",{id:"S0021"}),(0,n.kt)("code",null," ::= "),"     ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.2#S0023"},"type_declaration")),"\t| ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.2#S0026"},"subtype_declaration")),"   | ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.3#S0032"},"object_declaration")),"\t| ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.3#S0034"},"number_declaration")),"   | ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.1#S0195"},"subprogram_declaration")),"\t| ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.9#S0076"},"abstract_subprogram_declaration")),"   | ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.7#S0227"},"null_procedure_declaration")),"\t| ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.8#S0228"},"expression_function_declaration")),"   | ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-7.1#S0229"},"package_declaration")),"\t| ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-8.5#S0238"},"renaming_declaration")),"   | ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-11.1#S0303"},"exception_declaration")),"\t| ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-12.1#S0310"},"generic_declaration")),"   | ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-12.3#S0315"},"generic_instantiation"))),(0,n.kt)("p",null,(0,n.kt)("code",null,"defining_identifier"),(0,n.kt)("a",{id:"S0022"}),(0,n.kt)("code",null," ::= "),(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.3#S0002"},"identifier"))),(0,n.kt)("h4",u({},{id:"static-semantics"}),"Static Semantics"),(0,n.kt)("p",null,"A ",(0,n.kt)("em",null,"declaration")," is a language construct that associates a name with (a view of) an entity. A declaration may appear explicitly in the program text (an ",(0,n.kt)("em",null,"explicit")," declaration), or may be supposed to occur at a given place in the text as a consequence of the semantics of another construct (an ",(0,n.kt)("em",null,"implicit")," declaration). "),(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),"An implicit declaration generally declares a predefined or inherited operation associated with the definition of a type. This term is used primarily when allowing explicit declarations to override implicit declarations, as part of a type declaration. "),(0,n.kt)("p",null,"Version=[5],Kind=(AddedNormal),Group=[C],Term=[declaration], Def=[a language construct that associates a name with (a view of) an entity], Note1=[A declaration can appear explicitly in the program text (an explicit declaration), or can be supposed to occur at a given place in the text as a consequence of the semantics of another construct (an implicit declaration).]"),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI95-00318-02"),"}"," ","{",(0,n.kt)("em",null,"AI05-0255-1"),"}"," ","{",(0,n.kt)("em",null,"AI05-0277-1"),"}"," ","{",(0,n.kt)("em",null,"AI12-0061-1"),"}"," ","{",(0,n.kt)("em",null,"AI12-0308-1"),"}"," Each of the following is defined to be a declaration: any ",(0,n.kt)("code",null,"[basic_declaration](./AA-3.1#S0021)"),"; an ",(0,n.kt)("code",null,"[enumeration_literal_specification](./AA-3.5#S0039)"),"; a ",(0,n.kt)("code",null,"[discriminant_specification](./AA-3.7#S0062)"),"; a ",(0,n.kt)("code",null,"[component_declaration](./AA-3.8#S0070)"),"; a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.1#S0022"},"defining_identifier"))," of an ",(0,n.kt)("code",null,"[iterated_component_association](./AA-4.3#S0119)"),"; a ",(0,n.kt)("code",null,"[loop_parameter_specification](./AA-5.5#S0181)"),"; a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.1#S0022"},"defining_identifier"))," of a ",(0,n.kt)("code",null,"[chunk_specification](./AA-5.5#S0180)"),"; an ",(0,n.kt)("code",null,"[iterator_specification](./AA-5.5#S0183)"),"; a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.1#S0022"},"defining_identifier"))," of an ",(0,n.kt)("code",null,"[iterator_parameter_specification](./AA-5.5#S0186)"),"; a ",(0,n.kt)("code",null,"[parameter_specification](./AA-6.1#S0207)"),"; a ",(0,n.kt)("code",null,"[subprogram_body](./AA-6.3#S0216)"),"; an ",(0,n.kt)("code",null,"[extended_return_object_declaration](./AA-6.5#S0224)"),"; an ",(0,n.kt)("code",null,"[entry_declaration](./AA-9.5#S0257)"),"; an ",(0,n.kt)("code",null,"[entry_index_specification](./AA-9.5#S0263)"),"; a ",(0,n.kt)("code",null,"[choice_parameter_specification](./AA-11.2#S0306)"),"; a ",(0,n.kt)("code",null,"[generic_formal_parameter_declaration](./AA-12.1#S0314)"),". "),(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),"This list (when ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.1#S0021"},"basic_declaration")),' is expanded out) contains all syntactic categories that end in "_declaration" or "_specification", except for program unit _specifications. Moreover, it contains ',(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.3#S0216"},"subprogram_body")),". A ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.3#S0216"},"subprogram_body"))," is a declaration, whether or not it completes a previous declaration. This is a bit strange, ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.3#S0216"},"subprogram_body"))," is not part of the syntax of ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.1#S0021"},"basic_declaration"))," or ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-10.1#S0288"},"library_unit_declaration")),". A renaming-as-body is considered a declaration. An ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-9.5#S0258"},"accept_statement"))," is not considered a declaration. Completions are sometimes declarations, and sometimes not. "),(0,n.kt)("p",null,"All declarations contain a ",(0,n.kt)("em",null,"definition")," for a ",(0,n.kt)("em",null,"view")," of an entity. A view consists of an identification of the entity (the entity ",(0,n.kt)("em",null,"of")," the view), plus view-specific characteristics that affect the use of the entity through that view (such as mode of access to an object, formal parameter names and defaults for a subprogram, or visibility to components of a type). In most cases, a declaration also contains the definition for the entity itself (a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-8.5#S0238"},"renaming_declaration"))," is an example of a declaration that does not define a new entity, but instead defines a view of an existing entity (see 8.5))."),(0,n.kt)("p",null,(0,n.kt)("strong",null,"Glossary entry: "),"A view of an entity reveals some or all of the properties of the entity. A single entity may have multiple views."),(0,n.kt)("p",null,"Version=[5],Kind=(Added),Group=[T],Term=[view of an entity], Def=[a representation of an entity that reveals some or all of the properties of the entity], Note1=[A single entity can have multiple views.]"),(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),"Most declarations define a view (of some entity) whose view-specific characteristics are unchanging for the life of the view. However, subtypes are somewhat unusual in that they inherit characteristics from whatever view of their type is currently visible. Hence, a subtype is not a ",(0,n.kt)("em",null,"view")," of a type; it is more of an indirect reference. By contrast, a private type provides a single, unchanging (partial) view of its full type. "),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0080-1"),"}"," When it is clear from context, the term ",(0,n.kt)("em",null,"object")," is used in place of ",(0,n.kt)("em",null,"view of an object"),". Similarly, the terms ",(0,n.kt)("em",null,"type")," and ",(0,n.kt)("em",null,"subtype")," are used in place of ",(0,n.kt)("em",null,"view of a type")," and ",(0,n.kt)("em",null,"view of a subtype"),", respectively."),(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),'Rules interpreted at compile time generally refer to views of entities, rather than the entities themselves. This is necessary to preserve privacy; characteristics that are not visible should not be used in compile-time rules. Thus, Static Semantics and Legality Rules generally implicitly have "view of". Legality Rules that need to look into the private part are the exception to this interpretation.'),(0,n.kt)("p",null,'On the other hand, run-time rules can work either way, so "view of" should not be assumed in Dynamic Semantics rules.'),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI12-0191-1"),"}",' For example, a reference to the components of an object in a rule that is interpreted at compile time would not apply to components that are not visible. On the other hand, a reference to the components of an object in a dynamic semantics rule would apply to all components of the object, visible or not, including (for tagged objects) components which are not components of the nominal type of the object (see 3.9.1). Other terms, such as "subcomponent" and "part", are interpreted analogously. '),(0,n.kt)("p",null,"For each declaration, the language rules define a certain region of text called the ",(0,n.kt)("em",null,"scope")," of the declaration (see 8.2). Most declarations associate an ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.3#S0002"},"identifier"))," with a declared entity. Within its scope, and only there, there are places where it is possible to use the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.3#S0002"},"identifier"))," to refer to the declaration, the view it defines, and the associated entity; these places are defined by the visibility rules (see 8.3). At such places the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.3#S0002"},"identifier"))," is said to be a ",(0,n.kt)("em",null,"name")," of the entity (the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0092"},"direct_name"))," or ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0099"},"selector_name")),"); the name is said to ",(0,n.kt)("em",null,"denote")," the declaration, the view, and the associated entity (see 8.6). The declaration is said to ",(0,n.kt)("em",null,"declare")," the name, the view, and in most cases, the entity itself."),(0,n.kt)("p",null,"As an alternative to an ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.3#S0002"},"identifier")),", an enumeration literal can be declared with a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.5#S0015"},"character_literal"))," as its name (see 3.5.1), and a function can be declared with an ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.1#S0202"},"operator_symbol"))," as its name (see 6.1)."),(0,n.kt)("p",null,"The syntax rules use the terms ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.1#S0022"},"defining_identifier")),", ",(0,n.kt)("code",null,"[defining_character_literal](./AA-3.5#S0040)"),", and ",(0,n.kt)("code",null,"[defining_operator_symbol](./AA-6.1#S0203)")," for the defining occurrence of a name; these are collectively called ",(0,n.kt)("em",null,"defining names"),". The terms ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0092"},"direct_name"))," and ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0099"},"selector_name"))," are used for usage occurrences of ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.3#S0002"},"identifier")),"s, ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.5#S0015"},"character_literal")),"s, and ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.1#S0202"},"operator_symbol")),"s. These are collectively called ",(0,n.kt)("em",null,"usage names"),". "),(0,n.kt)("p",null,(0,n.kt)("strong",null,"To be honest: "),"The terms ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.3#S0002"},"identifier")),", ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.5#S0015"},"character_literal")),", and ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.1#S0202"},"operator_symbol"))," are used directly in contexts where the normal visibility rules do not apply (such as the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.3#S0002"},"identifier"))," that appears after the ",(0,n.kt)("strong",null,"end")," of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-9.1#S0248"},"task_body")),"). Analogous conventions apply to the use of ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.1#S0199"},"designator")),", which is the collective term for ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.3#S0002"},"identifier"))," and ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.1#S0202"},"operator_symbol")),". "),(0,n.kt)("h4",u({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,n.kt)("p",null,"The process by which a construct achieves its run-time effect is called ",(0,n.kt)("em",null,"execution"),". This process is also called ",(0,n.kt)("em",null,"elaboration")," for declarations and ",(0,n.kt)("em",null,"evaluation")," for expressions. One of the terms execution, elaboration, or evaluation is defined by this Reference Manual for each construct that has a run-time effect. "),(0,n.kt)("p",null,(0,n.kt)("strong",null,"Glossary entry: "),"The process by which a construct achieves its run-time effect is called ",(0,n.kt)("em",null,"execution"),". Execution of a declaration is also called ",(0,n.kt)("em",null,"elaboration"),". Execution of an expression is also called ",(0,n.kt)("em",null,"evaluation"),"."),(0,n.kt)("p",null,"Version=[5],Kind=(Added),Group=[R],Term=[execution], Def=[the process by which a construct achieves its run-time effect], Note1=[Execution of a declaration is also called elaboration. Execution of an expression is also called evaluation.] "),(0,n.kt)("p",null,(0,n.kt)("strong",null,"To be honest: "),"The term elaboration is also used for the execution of certain constructs that are not declarations, and the term evaluation is used for the execution of certain constructs that are not expressions. For example, ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.2#S0027"},"subtype_indication")),"s are elaborated, and ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.5#S0037"},"range")),"s are evaluated."),(0,n.kt)("p",null,"For bodies, execution and elaboration are both explicitly defined. When we refer specifically to the execution of a body, we mean the explicit definition of execution for that kind of body, not its elaboration. "),(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),'Technically, "the execution of a declaration" and "the elaboration of a declaration" are synonymous. We use the term "elaboration" of a construct when we know the construct is elaborable. When we are talking about more arbitrary constructs, we use the term "execution". For example, we use the term "erroneous execution", to refer to any erroneous execution, including erroneous elaboration or evaluation.'),(0,n.kt)("p",null,"When we explicitly define evaluation or elaboration for a construct, we are implicitly defining execution of that construct."),(0,n.kt)("p",null,'We also use the term "execution" for things like ',(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-5.1#S0167"},"statement")),'s, which are executable, but neither elaborable nor evaluable. We considered using the term "execution" only for nonelaborable, nonevaluable constructs, and defining the term "action" to mean what we have defined "execution" to mean. We rejected this idea because we thought three terms that mean the same thing was enough - four would be overkill. Thus, the term "action" is used only informally in the standard (except where it is defined as part of a larger term, such as "protected action"). '),(0,n.kt)("p",null,(0,n.kt)("strong",null,"Glossary entry: "),"The process by which a declaration achieves its run-time effect is called elaboration. Elaboration is one of the forms of execution."),(0,n.kt)("p",null,(0,n.kt)("strong",null,"Glossary entry: "),"The process by which an expression achieves its run-time effect is called evaluation. Evaluation is one of the forms of execution."),(0,n.kt)("p",null,"Version=[5],Kind=(Added),Group=[R],Term=[elaboration], Def=[the process by which a declaration achieves its run-time effect], Note1=[Elaboration is one of the forms of execution.] Version=[5],Kind=(Added),Group=[R],Term=[evaluation], Def=[the process by which an expression achieves its run-time effect], Note1=[Evaluation is one of the forms of execution.] "),(0,n.kt)("p",null,(0,n.kt)("strong",null,"To be honest: "),"A construct is ",(0,n.kt)("em",null,"elaborable")," if elaboration is defined for it. A construct is ",(0,n.kt)("em",null,"evaluable")," if evaluation is defined for it. A construct is ",(0,n.kt)("em",null,"executable")," if execution is defined for it. "),(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),'Don\'t confuse "elaborable" with "preelaborable" (defined in 10.2.1).'),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI95-00114-01"),"}"," Evaluation of an evaluable construct produces a result that is either a value, a denotation, or a range. The following are evaluable: expression; ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0091"},"name"))," ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0093"},"prefix")),"; ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.5#S0037"},"range")),"; ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-9.5#S0263"},"entry_index_specification")),"; and possibly ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.6#S0058"},"discrete_range")),'. The last one is curious - RM83 uses the term "evaluation of a ',(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.6#S0058"},"discrete_range")),'", but never defines it. One might presume that the evaluation of a ',(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.6#S0058"},"discrete_range"))," consists of the evaluation of the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.5#S0037"},"range"))," or the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.2#S0027"},"subtype_indication")),", depending on what it is. But ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.2#S0027"},"subtype_indication")),"s are not evaluated; they are elaborated."),(0,n.kt)("p",null,"Intuitively, an ",(0,n.kt)("em",null,"executable")," construct is one that has a defined run-time effect (which may be null). Since execution includes elaboration and evaluation as special cases, all elaborable and all evaluable constructs are also executable. Hence, most constructs in Ada are executable. An important exception is that the constructs inside a generic unit are not executable directly, but rather are used as a template for (generally) executable constructs in instances of the generic. "),(0,n.kt)("p",null,"NOTE   At compile time, the declaration of an entity ",(0,n.kt)("em",null,"declares")," the entity. At run time, the elaboration of the declaration ",(0,n.kt)("em",null,"creates")," the entity. "),(0,n.kt)("p",null,(0,n.kt)("strong",null,"Ramification: "),"Syntactic categories for declarations are named either ",(0,n.kt)("em",null,"entity_"),(0,n.kt)("code",null,"declaration")," (if they include a trailing semicolon) or ",(0,n.kt)("em",null,"entity_"),(0,n.kt)("code",null,"specification")," (if not)."),(0,n.kt)("p",null,"The various kinds of named entities that can be declared are as follows: an object (including components and parameters), a named number, a type (the name always refers to its first subtype), a subtype, a subprogram (including enumeration literals and operators), a single entry, an entry family, a package, a protected or task unit (which corresponds to either a type or a single object), an exception, a generic unit, a label, and the name of a statement."),(0,n.kt)("p",null,"Identifiers are also associated with names of pragmas, arguments to pragmas, and with attributes, but these are not user-definable. "),(0,n.kt)("h4",u({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,n.kt)("p",null,"The syntax rule for ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.1#S0022"},"defining_identifier"))," is new. It is used for the defining occurrence of an ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.3#S0002"},"identifier")),". Usage occurrences use the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0092"},"direct_name"))," or ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0099"},"selector_name"))," syntactic categories. Each occurrence of an ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.3#S0002"},"identifier"))," (or ",(0,n.kt)("code",null,"simple_name"),"), ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.5#S0015"},"character_literal")),", or ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.1#S0202"},"operator_symbol"))," in the Ada 83 syntax rules is handled as follows in Ada 95: "),(0,n.kt)("p",null,"It becomes a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.1#S0022"},"defining_identifier")),", ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.5#S0040"},"defining_character_literal")),", or ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.1#S0203"},"defining_operator_symbol"))," (or some syntactic category composed of these), to indicate a defining occurrence;"),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0299-1"),"}"," It becomes a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0092"},"direct_name")),", in usage occurrences where the usage is required (in Clause 8) to be directly visible;"),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0299-1"),"}"," It becomes a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0099"},"selector_name")),", in usage occurrences where the usage is required (in Clause 8) to be visible but not necessarily directly visible;"),(0,n.kt)("p",null,"It remains an ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.3#S0002"},"identifier")),", ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-2.5#S0015"},"character_literal")),", or ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.1#S0202"},"operator_symbol")),", in cases where the visibility rules do not apply (such as the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.1#S0199"},"designator"))," that appears after the ",(0,n.kt)("strong",null,"end")," of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.3#S0216"},"subprogram_body")),"). "),(0,n.kt)("p",null,'For declarations that come in "two parts" (program unit declaration plus body, private or incomplete type plus full type, deferred constant plus full constant), we consider both to be defining occurrences. Thus, for example, the syntax for ',(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-7.2#S0231"},"package_body"))," uses ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.1#S0022"},"defining_identifier"))," after the reserved word ",(0,n.kt)("strong",null,"body"),", as opposed to ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0092"},"direct_name")),"."),(0,n.kt)("p",null,"The defining occurrence of a statement name is in its implicit declaration, not where it appears in the program text. Considering the statement name itself to be the defining occurrence would complicate the visibility rules."),(0,n.kt)("p",null,'The phrase "visible by selection" is not used in Ada 95. It is subsumed by simply "visible" and the Name Resolution Rules for ',(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0099"},"selector_name")),"s."),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0299-1"),"}"," (Note that in Ada 95, a declaration is visible at all places where one could have used a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0099"},"selector_name")),", not just at places where a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-4.1#S0099"},"selector_name"))," was actually used. Thus, the places where a declaration is directly visible are a subset of the places where it is visible. See Clause 8 for details.)"),(0,n.kt)("p",null,'We use the term "declaration" to cover ',(0,n.kt)("code",null,"_specification"),"s that declare (views of) objects, such as ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.1#S0207"},"parameter_specification")),'s. In Ada 83, these are referred to as a "form of declaration", but it is not entirely clear that they are considered simply "declarations".'),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0299-1"),"}",' RM83 contains an incomplete definition of "elaborated" in this subclause: it defines "elaborated" for declarations, ',(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.11#S0086"},"declarative_part")),"s, ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.11#S0087"},"declarative_item")),"s and ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-10.1#S0286"},"compilation_unit")),'s, but "elaboration" is defined elsewhere for various other constructs. To make matters worse, Ada 95 has a different set of elaborable constructs. Instead of correcting the list, it is more maintainable to refer to the term "elaborable," which is defined in a distributed manner.'),(0,n.kt)("p",null,'RM83 uses the term "has no other effect" to describe an elaboration that doesn\'t do anything except change the state from not-yet-elaborated to elaborated. This was a confusing wording, because the answer to "other than what?" was to be found many pages away. In Ada 95, we change this wording to "has no effect" (for things that truly do nothing at run time), and "has no effect other than to establish that so-and-so can happen without failing the Elaboration_Check" (for things where it matters).'),(0,n.kt)("p",null,'We make it clearer that the term "execution" covers elaboration and evaluation as special cases. This was implied in RM83. For example, "erroneous execution" can include any execution, and RM83-9.4(3) has, "The task designated by any other task object depends on the master whose execution creates the task object;" the elaboration of the master\'s ',(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-3.11#S0086"},"declarative_part"))," is doing the task creation. "),(0,n.kt)("h4",u({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI95-00318-02"),"}"," Added ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"./AA-6.5#S0225"},"extended_return_statement"))," to the list of declarations."),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI95-00348-01"),"}"," Added null procedures (see 6.7) to the syntax. "),(0,n.kt)("h4",u({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0177-1"),"}"," Added expression functions (see 6.8) to the syntax. "))}b.isMDXComponent=!0}}]);