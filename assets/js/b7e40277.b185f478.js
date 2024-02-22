"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[7161],{60270:(e,i,t)=>{t.r(i),t.d(i,{assets:()=>l,contentTitle:()=>o,default:()=>g,frontMatter:()=>r,metadata:()=>s,toc:()=>p});var n=t(58168),a=(t(96540),t(15680));const r={title:"9.6 Summary"},o=void 0,s={unversionedId:"style-guide/s9/06",id:"style-guide/s9/06",title:"9.6 Summary",description:"tagged type hierarchies",source:"@site/docs/style-guide/s9/06.mdx",sourceDirName:"style-guide/s9",slug:"/style-guide/s9/06",permalink:"/docs/style-guide/s9/06",draft:!1,tags:[],version:"current",frontMatter:{title:"9.6 Summary"},sidebar:"styleGuideSidebar",previous:{title:"9.5 Multiple Inheritance",permalink:"/docs/style-guide/s9/05"},next:{title:"10. Improving Performance",permalink:"/docs/style-guide/s10/"}},l={},p=[{value:"tagged type hierarchies",id:"tagged-type-hierarchies",level:3},{value:"tagged type operations",id:"tagged-type-operations",level:3},{value:"managing visibility",id:"managing-visibility",level:3},{value:"multiple inheritance",id:"multiple-inheritance",level:3}],d={toc:p},c="wrapper";function g(e){let{components:i,...t}=e;return(0,a.yg)(c,(0,n.A)({},d,t,{components:i,mdxType:"MDXLayout"}),(0,a.yg)("h3",{id:"tagged-type-hierarchies"},"tagged type hierarchies"),(0,a.yg)("ul",null,(0,a.yg)("li",{parentName:"ul"},"Consider using type extension when designing an is-a\n(generalization/specialization) hierarchy."),(0,a.yg)("li",{parentName:"ul"},"Use tagged types to preserve a common interface across differing\nimplementations (Taft 1995a)."),(0,a.yg)("li",{parentName:"ul"},"When defining a tagged type in a package, consider including a\ndefinition of a general access type to the corresponding class-wide\ntype."),(0,a.yg)("li",{parentName:"ul"},"In general, define only one tagged type per package."),(0,a.yg)("li",{parentName:"ul"},"The implementation of the dispatching operations of each type in a\nderivation class rooted in a tagged type T should conform to the\nexpected semantics of the corresponding dispatching operations of\nthe class-wide type T'Class."),(0,a.yg)("li",{parentName:"ul"},'Consider using a controlled type whenever a type allocates resources\nthat must be deallocated or otherwise "cleaned up" on destruction or\noverwriting.'),(0,a.yg)("li",{parentName:"ul"},'Use a derivation from a controlled type in preference to providing\nan explicit "cleanup" operation that must be called by clients of\nthe type.'),(0,a.yg)("li",{parentName:"ul"},"When overriding the adjustment and finalization procedures derived\nfrom controlled types, define the finalization procedure to undo the\neffects of the adjustment procedure."),(0,a.yg)("li",{parentName:"ul"},"Derived type initialization procedures should call the\ninitialization procedure of their parent as part of their\ntype-specific initialization."),(0,a.yg)("li",{parentName:"ul"},"Derived type finalization procedures should call the finalization\nprocedure of their parent as part of their type-specific\nfinalization."),(0,a.yg)("li",{parentName:"ul"},"Consider deriving a data structure's components rather than the\nenclosing data structure from a controlled type."),(0,a.yg)("li",{parentName:"ul"},"Consider using abstract types and operations in creating\nclassification schemes, for example, a taxonomy, in which only the\nleaf objects will be meaningful in the application."),(0,a.yg)("li",{parentName:"ul"},"Consider declaring root types and internal nodes in a type tree as\nabstract."),(0,a.yg)("li",{parentName:"ul"},"Consider using abstract types for generic formal derived types."),(0,a.yg)("li",{parentName:"ul"},"Consider using abstract types to develop different implementations\nof a single abstraction.")),(0,a.yg)("h3",{id:"tagged-type-operations"},"tagged type operations"),(0,a.yg)("ul",null,(0,a.yg)("li",{parentName:"ul"},'Consider declaring a primitive abstract operation based on the\nabsence of a meaningful "default" behavior.'),(0,a.yg)("li",{parentName:"ul"},'Consider declaring a primitive nonabstract operation based on the\npresence of a meaningful "default" behavior.'),(0,a.yg)("li",{parentName:"ul"},"When overriding an operation, the overriding subprogram should not\nraise exceptions that are not known to the users of the overridden\nsubprogram."),(0,a.yg)("li",{parentName:"ul"},'If redispatching is used in the implementation of the operations of\na type, with the specific intent that some of the redispatched-to\noperations be overridden by specializations for the derived types,\nthen document this intent clearly in the specification as part of\nthe "interface" of a parent type with its derived types.'),(0,a.yg)("li",{parentName:"ul"},"When redispatching is used (for any reason) in the implementation of\na primitive operation of a tagged type, then document (in some\nproject-consistent way) this use in the body of the operation\nsubprogram so that it can be easily found during maintenance."),(0,a.yg)("li",{parentName:"ul"},"Consider using a class-wide operation (i.e., an operation with\nparameter","[","s","]"," of a class-wide type) when an operation can be\nwritten, compiled, and tested without knowing all the possible\ndescendants of a given tagged type (Barnes 1996)."),(0,a.yg)("li",{parentName:"ul"},"Consider using a class-wide operation when you do not want an\noperation to be inherited and/or overridden."),(0,a.yg)("li",{parentName:"ul"},"Avoid declaring a constructor as a primitive abstract operation."),(0,a.yg)("li",{parentName:"ul"},"Use a primitive abstract operation to declare an initialization\nfunction or constructor only when objects of the inheriting derived\ntypes will not require additional parameters for initialization."),(0,a.yg)("li",{parentName:"ul"},"Consider using access discriminants to provide parameters to default\ninitialization."),(0,a.yg)("li",{parentName:"ul"},"Use constructors for explicit initialization."),(0,a.yg)("li",{parentName:"ul"},"Consider splitting the initialization and construction of an object."),(0,a.yg)("li",{parentName:"ul"},"Consider declaring a constructor operation in a child package."),(0,a.yg)("li",{parentName:"ul"},"Consider declaring a constructor operation to return an access value\nto the constructed object (Dewar 1995)."),(0,a.yg)("li",{parentName:"ul"},'When you redefine the "=" operator on a tagged type, make sure that\nit has the expected behavior in extensions of this type and override\nit if necessary.'),(0,a.yg)("li",{parentName:"ul"},"Consider using class-wide programming to provide run-time, dynamic\npolymorphism when constructing larger, reusable, extensible\nframeworks."),(0,a.yg)("li",{parentName:"ul"},"When possible, use class-wide programming rather than variant\nrecords."),(0,a.yg)("li",{parentName:"ul"},"Use class-wide programming to provide a consistent interface across\nthe set of types in the tagged type hierarchy (i.e., class)."),(0,a.yg)("li",{parentName:"ul"},"Consider using generics to define a new type in terms of an existing\ntype, either as an extension or as a container, collection, or\ncomposite data structure."),(0,a.yg)("li",{parentName:"ul"},"Avoid using type extensions for parameterized abstractions when\ngenerics provide a more appropriate mechanism.")),(0,a.yg)("h3",{id:"managing-visibility"},"managing visibility"),(0,a.yg)("ul",null,(0,a.yg)("li",{parentName:"ul"},"Consider giving derived tagged types the same visibility to the\nparent type as other clients of the parent."),(0,a.yg)("li",{parentName:"ul"},"Define a derived tagged type in a child of the package that defines\nthe base type if the implementation of the derived type requires\ngreater visibility into the implementation of the base type than\nother clients of the base type require.")),(0,a.yg)("h3",{id:"multiple-inheritance"},"multiple inheritance"),(0,a.yg)("ul",null,(0,a.yg)("li",{parentName:"ul"},"Consider using type composition for implementation, as opposed to\ninterface, inheritance."),(0,a.yg)("li",{parentName:"ul"},'Consider using a generic to "mix in" functionality to a derivative\nof some core abstraction.'),(0,a.yg)("li",{parentName:"ul"},'Consider using access discriminants to support "full" multiple\ninheritance where an object must be referenceable as an entity of\ntwo or more distinct unrelated abstractions.')),(0,a.yg)("admonition",{type:"note"},(0,a.yg)("p",{parentName:"admonition"},'This page of the "Ada Quality and Style Guide" has been adapted from the\noriginal work at ',(0,a.yg)("a",{parentName:"p",href:"https://en.wikibooks.org/wiki/Ada_Style_Guide"},"https://en.wikibooks.org/wiki/Ada_Style_Guide"),", which is\nlicensed under the\n",(0,a.yg)("a",{parentName:"p",href:"https://creativecommons.org/licenses/by-sa/3.0/"},"Creative Commons Attribution-ShareAlike License"),";\nadditional terms may apply. Page not endorsed by Wikibooks or the Ada\nStyle Guide Wikibook authors. This page is licensed under the same license\nas the original work.")))}g.isMDXComponent=!0}}]);