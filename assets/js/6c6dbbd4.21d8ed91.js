"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8243],{9219:(e,t,i)=>{i.r(t),i.d(t,{assets:()=>k,contentTitle:()=>f,default:()=>y,frontMatter:()=>u,metadata:()=>m,toc:()=>b});var a=i(1716),n=i(7556),o=Object.defineProperty,l=Object.defineProperties,r=Object.getOwnPropertyDescriptors,s=Object.getOwnPropertySymbols,d=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,h=(e,t,i)=>t in e?o(e,t,{enumerable:!0,configurable:!0,writable:!0,value:i}):e[t]=i,p=(e,t)=>{for(var i in t||(t={}))d.call(t,i)&&h(e,i,t[i]);if(s)for(var i of s(t))c.call(t,i)&&h(e,i,t[i]);return e};const u={sidebar_position:66},f="8.2  Scope of Declarations",m={unversionedId:"arm/AA-8.2",id:"arm/AA-8.2",title:"8.2  Scope of Declarations",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-8.2.mdx",sourceDirName:"arm",slug:"/arm/AA-8.2",permalink:"/docs/arm/AA-8.2",draft:!1,tags:[],version:"current",sidebarPosition:66,frontMatter:{sidebar_position:66},sidebar:"tutorialSidebar",previous:{title:"8.1  Declarative Region",permalink:"/docs/arm/AA-8.1"},next:{title:"8.3  Visibility",permalink:"/docs/arm/AA-8.3"}},k={},b=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],g={toc:b};function y(e){var t,i=e,{components:o}=i,h=((e,t)=>{var i={};for(var a in e)d.call(e,a)&&t.indexOf(a)<0&&(i[a]=e[a]);if(null!=e&&s)for(var a of s(e))t.indexOf(a)<0&&c.call(e,a)&&(i[a]=e[a]);return i})(i,["components"]);return(0,a.kt)("wrapper",(t=p(p({},g),h),l(t,r({components:o,mdxType:"MDXLayout"}))),(0,a.kt)("h1",p({},{id:"82--scope-of-declarations"}),"8.2  Scope of Declarations"),(0,a.kt)("admonition",p({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,a.kt)("a",p({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)("p",null,"[For each declaration, the language rules define a certain portion of the program text called the ",(0,a.kt)("em",null,"scope")," of the declaration. The scope of a declaration is also called the scope of any view or entity declared by the declaration. Within the scope of an entity, and only there, there are places where it is legal to refer to the declared entity. These places are defined by the rules of visibility and overloading.] "),(0,a.kt)("h4",p({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)("p",null,"The ",(0,a.kt)("em",null,"immediate scope")," of a declaration is a portion of the declarative region immediately enclosing the declaration. The immediate scope starts at the beginning of the declaration, except in the case of an overloadable declaration, in which case the immediate scope starts just after the place where the profile of the callable entity is determined (which is at the end of the ",(0,a.kt)("code",null,"_specification")," for the callable entity, or at the end of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-12.3#S0315"},"generic_instantiation"))," if an instance). The immediate scope extends to the end of the declarative region, with the following exceptions: "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Reason: "),"The reason for making overloadable declarations with profiles special is to simplify compilation: until the compiler has determined the profile, it doesn't know which other declarations are homographs of this one, so it doesn't know which ones this one should hide. Without this rule, two passes over the ",(0,a.kt)("code",null,"_specification")," or ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-12.3#S0315"},"generic_instantiation"))," would be required to resolve names that denote things with the same name as this one. "),(0,a.kt)("p",null,"The immediate scope of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-10.1#S0287"},"library_item"))," includes only its semantic dependents. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Reason: "),"{",(0,a.kt)("em",null,"AI05-0299-1"),"}"," Clause 10 defines only a partial ordering of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-10.1#S0287"},"library_item")),"s. Therefore, it is a good idea to restrict the immediate scope (and the scope, defined below) to semantic dependents."),(0,a.kt)("p",null,"Consider also examples like this: "),(0,a.kt)(n.Z,{mdxType:"CodeBlock"},"package P is end P;","\n"),(0,a.kt)(n.Z,{mdxType:"CodeBlock"},"package P.Q is","\n","    I : Integer := 0;","\n","end P.Q;","\n"),(0,a.kt)(n.Z,{mdxType:"CodeBlock"},"with P;","\n","package R is","\n","    package X renames P;","\n","    J : Integer := X.Q.I; -- Illegal!","\n","end R;","\n"),(0,a.kt)("p",null,"The scope of P.Q does not contain R. Hence, neither P.Q nor X.Q are visible within R. However, the name R.X.Q would be visible in some other library unit where both R and P.Q are visible (assuming R were made legal by removing the offending declaration). "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Ramification: "),"{",(0,a.kt)("em",null,"AI95-00217-06"),"}",' This rule applies to limited views as well as "normal" library items. In that case, the semantic dependents are the units that have a ',(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-10.1#S0295"},"limited_with_clause"))," for the limited view. "),(0,a.kt)("p",null,"The immediate scope of a declaration in the private part of a library unit does not include the visible part of any public descendant of that library unit. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Ramification: "),"In other words, a declaration in the private part can be visible within the visible part, private part and body of a private child unit. On the other hand, such a declaration can be visible within only the private part and body of a public child unit. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Reason: "),"The purpose of this rule is to prevent children from giving private information to clients. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Ramification: "),"{",(0,a.kt)("em",null,"AI95-00231-01"),"}"," For a public child subprogram, this means that the parent's private part is not visible in the profile of the declaration and of the body. This is true even for ",(0,a.kt)("code",null,"subprogram_bodies")," that are not completions. For a public child generic unit, it means that the parent's private part is not visible in the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-12.1#S0313"},"generic_formal_part")),", as well as in the first list of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.11#S0088"},"basic_declarative_item")),"s (for a generic package), or the (syntactic) profile (for a generic subprogram). "),(0,a.kt)("p",null,"[The ",(0,a.kt)("em",null,"visible part")," of (a view of) an entity is a portion of the text of its declaration containing declarations that are visible from outside.] The ",(0,a.kt)("em",null,"private part")," of (a view of) an entity that has a visible part contains all declarations within the declaration of (the view of) the entity, except those in the visible part; [these are not visible from outside. Visible and private parts are defined only for these kinds of entities: callable entities, other program units, and composite types.] "),(0,a.kt)("p",null,"The visible part of a view of a callable entity is its profile."),(0,a.kt)("p",null,"The visible part of a composite type other than a task or protected type consists of the declarations of all components declared [(explicitly or implicitly)] within the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.2#S0023"},"type_declaration")),"."),(0,a.kt)("p",null,"The visible part of a generic unit includes the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-12.1#S0313"},"generic_formal_part")),". For a generic package, it also includes the first list of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-3.11#S0088"},"basic_declarative_item")),"s of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-7.1#S0230"},"package_specification")),". For a generic subprogram, it also includes the profile. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Reason: "),"Although there is no way to reference anything but the formals from outside a generic unit, they are still in the visible part in the sense that the corresponding declarations in an instance can be referenced (at least in some cases). In other words, these declarations have an effect on the outside world. The visible part of a generic unit needs to be defined this way in order to properly support the rule that makes a parent's private part invisible within a public child's visible part. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Ramification: "),"The visible part of an instance of a generic unit is as defined for packages and subprograms; it is not defined in terms of the visible part of a generic unit. "),(0,a.kt)("p",null,"[The visible part of a package, task unit, or protected unit consists of declarations in the program unit's declaration other than those following the reserved word ",(0,a.kt)("strong",null,"private"),", if any; see 7.1 and 12.7 for packages, 9.1 for task units, and 9.4 for protected units.] "),(0,a.kt)("p",null,"The scope of a declaration always contains the immediate scope of the declaration. In addition, for a given declaration that occurs immediately within the visible part of an outer declaration, or is a public child of an outer declaration, the scope of the given declaration extends to the end of the scope of the outer declaration, except that the scope of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-10.1#S0287"},"library_item"))," includes only its semantic dependents. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Ramification: "),"Note the recursion. If a declaration appears in the visible part of a library unit, its scope extends to the end of the scope of the library unit, but since that only includes dependents of the declaration of the library unit, the scope of the inner declaration also only includes those dependents. If X renames library package P, which has a child Q, a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-10.1#S0294"},"with_clause"))," mentioning P.Q is necessary to be able to refer to X.Q, even if P.Q is visible at the place where X is declared. "),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00408-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0183-1"),"}"," The scope of an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-13.3#S0349"},"attribute_definition_clause"))," is identical to the scope of a declaration that would occur at the point of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-13.3#S0349"},"attribute_definition_clause")),". The scope of an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-13.1#S0346"},"aspect_specification"))," is identical to the scope of the associated declaration."),(0,a.kt)("p",null,"The immediate scope of a declaration is also the immediate scope of the entity or view declared by the declaration. Similarly, the scope of a declaration is also the scope of the entity or view declared by the declaration. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Ramification: "),"The rule for immediate scope implies the following: "),(0,a.kt)("p",null,"If the declaration is that of a library unit, then the immediate scope includes the declarative region of the declaration itself, but not other places, unless they are within the scope of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-10.1#S0294"},"with_clause"))," that mentions the library unit."),(0,a.kt)("p",null,"It is necessary to attach the semantics of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-10.1#S0294"},"with_clause")),"s to [immediate] scopes (as opposed to visibility), in order for various rules to work properly. A library unit should hide a homographic implicit declaration that appears in its parent, but only within the scope of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-10.1#S0294"},"with_clause")),' that mentions the library unit. Otherwise, we would violate the "legality determinable via semantic dependences" rule of 10, "Program Structure and Compilation Issues". The declaration of a library unit should be allowed to be a homograph of an explicit declaration in its parent\'s body, so long as that body does not mention the library unit in a ',(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-10.1#S0294"},"with_clause")),"."),(0,a.kt)("p",null,"This means that one cannot denote the declaration of the library unit, but one might still be able to denote the library unit via another view."),(0,a.kt)("p",null,"A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-10.1#S0294"},"with_clause"))," does not make the declaration of a library unit visible; the lack of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-10.1#S0294"},"with_clause"))," prevents it from being visible. Even if a library unit is mentioned in a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-10.1#S0294"},"with_clause")),", its declaration can still be hidden."),(0,a.kt)("p",null,"The completion of the declaration of a library unit (assuming that's also a declaration) is not visible, neither directly nor by selection, outside that completion."),(0,a.kt)("p",null,"The immediate scope of a declaration immediately within the body of a library unit does not include any child of that library unit."),(0,a.kt)("p",null,"This is needed to prevent children from looking inside their parent's body. The children are in the declarative region of the parent, and they might be after the parent's body. Therefore, the scope of a declaration that occurs immediately within the body might include some children. "),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0003-1"),"}"," The immediate scope of a pragma that is not used as a configuration pragma is defined to be the region extending from immediately after the pragma to the end of the declarative region immediately enclosing the pragma. "),(0,a.kt)("p",null,"NOTE   ","{",(0,a.kt)("em",null,"AI05-0299-1"),"}"," There are notations for denoting visible declarations that are not directly visible. For example, ",(0,a.kt)("code",null,"[parameter_specification](./AA-6.1#S0207)"),"s are in the visible part of a ",(0,a.kt)("code",null,"[subprogram_declaration](./AA-6.1#S0195)")," so that they can be used in named-notation calls appearing outside the called subprogram. For another example, declarations of the visible part of a package can be denoted by expanded names appearing outside the package, and can be made directly visible by a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-8.4#S0235"},"use_clause")),". "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Ramification: "),"{",(0,a.kt)("em",null,"AI95-00114-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0299-1"),"}"," There are some obscure cases involving generics in which there is no such notation. See Clause 12. "),(0,a.kt)("h4",p({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,a.kt)("p",null,"The fact that the immediate scope of an overloadable declaration does not include its profile is new to Ada 95. It replaces RM83-8.3(16), which said that within a subprogram specification and within the formal part of an entry declaration or accept statement, all declarations with the same designator as the subprogram or entry were hidden from all visibility. The RM83-8.3(16) rule seemed to be overkill, and created both implementation difficulties and unnecessary semantic complexity. "),(0,a.kt)("h4",p({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,a.kt)("p",null,"We no longer need to talk about the scope of notations, ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-2.3#S0002"},"identifier")),"s, ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-2.5#S0015"},"character_literal")),"s, and ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-6.1#S0202"},"operator_symbol")),"s."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0299-1"),"}",' The notion of "visible part" has been extended in Ada 95. The syntax of task and protected units now allows private parts, thus requiring us to be able to talk about the visible part as well. It was necessary to extend the concept to subprograms and to generic units, in order for the visibility rules related to child library units to work properly. It was necessary to define the concept separately for generic formal packages, since their visible part is slightly different from that of a normal package. Extending the concept to composite types made the definition of scope slightly simpler. We define visible part for some things elsewhere, since it makes a big difference to the user for those things. For composite types and subprograms, however, the concept is used only in arcane visibility rules, so we localize it to this subclause.'),(0,a.kt)("p",null,"In Ada 83, the semantics of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-10.1#S0294"},"with_clause")),"s was described in terms of visibility. It is now described in terms of [immediate] scope."),(0,a.kt)("p",null,"We have clarified that the following is illegal (where Q and R are library units): "),(0,a.kt)(n.Z,{mdxType:"CodeBlock"},"package Q is","\n","    I : Integer := 0;","\n","end Q;","\n"),(0,a.kt)(n.Z,{mdxType:"CodeBlock"},"package R is","\n","    package X renames Standard;","\n","    X.Q.I := 17; -- Illegal!","\n","end R;","\n"),(0,a.kt)("p",null,"even though Q is declared in the declarative region of Standard, because R does not mention Q in a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-10.1#S0294"},"with_clause")),". "),(0,a.kt)("h4",p({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00408-01"),"}"," The scope of an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-13.3#S0349"},"attribute_definition_clause"))," is defined so that it can be used to define the visibility of such a clause, so ",(0,a.kt)("em",null,"that")," can be used by the stream attribute availability rules (see 13.13.2). "),(0,a.kt)("h4",p({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0183-1"),"}"," The scope of an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-13.1#S0346"},"aspect_specification"))," is defined for similar reasons that it was defined for ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-13.3#S0349"},"attribute_definition_clause")),"s. "),(0,a.kt)("h4",p({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0003-1"),"}"," The immediate scope of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-2.8#S0019"},"pragma"))," is defined as it is used in other rules in the Reference Manual. "))}y.isMDXComponent=!0}}]);