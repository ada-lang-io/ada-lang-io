"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[1766],{13970:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>l,contentTitle:()=>r,default:()=>h,frontMatter:()=>a,metadata:()=>o,toc:()=>c});var s=i(74848),t=i(28453);const a={title:"3.5 Summary"},r=void 0,o={id:"style-guide/s3/05",title:"3.5 Summary",description:"spelling",source:"@site/docs/style-guide/s3/05.mdx",sourceDirName:"style-guide/s3",slug:"/style-guide/s3/05",permalink:"/docs/style-guide/s3/05",draft:!1,unlisted:!1,tags:[],version:"current",frontMatter:{title:"3.5 Summary"},sidebar:"styleGuideSidebar",previous:{title:"3.4 Using Types",permalink:"/docs/style-guide/s3/04"},next:{title:"4. Program Structure",permalink:"/docs/style-guide/s4/"}},l={},c=[{value:"spelling",id:"spelling",level:3},{value:"naming conventions",id:"naming-conventions",level:3},{value:"comments",id:"comments",level:3},{value:"using types",id:"using-types",level:3}];function d(e){const n={a:"a",admonition:"admonition",h3:"h3",li:"li",p:"p",ul:"ul",...(0,t.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(n.h3,{id:"spelling",children:"spelling"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Use underscores to separate words in a compound name."}),"\n",(0,s.jsx)(n.li,{children:"Represent numbers in a consistent fashion."}),"\n",(0,s.jsx)(n.li,{children:"Represent literals in a radix appropriate to the problem."}),"\n",(0,s.jsx)(n.li,{children:"Use underscores to separate digits the same way commas or periods\n(or spaces for nondecimal bases) would be used in normal text."}),"\n",(0,s.jsx)(n.li,{children:"When using scientific notation, make the E consistently either\nuppercase or lowercase."}),"\n",(0,s.jsx)(n.li,{children:"In an alternate base, represent the alphabetic characters in either\nall uppercase or all lowercase."}),"\n",(0,s.jsx)(n.li,{children:"Make reserved words and other elements of the program visually\ndistinct from each other."}),"\n",(0,s.jsx)(n.li,{children:"Do not use an abbreviation of a long word as an identifier where a\nshorter synonym exists."}),"\n",(0,s.jsx)(n.li,{children:"Use a consistent abbreviation strategy."}),"\n",(0,s.jsx)(n.li,{children:"Do not use ambiguous abbreviations."}),"\n",(0,s.jsx)(n.li,{children:"To justify its use, an abbreviation must save many characters over\nthe full word."}),"\n",(0,s.jsx)(n.li,{children:"Use abbreviations that are well-accepted in the application domain."}),"\n",(0,s.jsx)(n.li,{children:"Maintain a list of accepted abbreviations, and use only\nabbreviations on that list."}),"\n"]}),"\n",(0,s.jsx)(n.h3,{id:"naming-conventions",children:"naming conventions"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Choose names that are as self-documenting as possible."}),"\n",(0,s.jsx)(n.li,{children:"Use a short synonym instead of an abbreviation."}),"\n",(0,s.jsx)(n.li,{children:"Use names given by the application, but do not use obscure jargon."}),"\n",(0,s.jsx)(n.li,{children:"Avoid using the same name to declare different kinds of identifiers."}),"\n",(0,s.jsx)(n.li,{children:"Use singular, general nouns as subtype identifiers."}),"\n",(0,s.jsx)(n.li,{children:"Choose identifiers that describe one of the subtype's values."}),"\n",(0,s.jsx)(n.li,{children:"Consider using suffixes for subtype identifiers that define visible\naccess types, visible subranges, or visible array types."}),"\n",(0,s.jsx)(n.li,{children:"For private types, do not use identifier constructions (e.g.,\nsuffixes) that are unique to subtype identifiers."}),"\n",(0,s.jsx)(n.li,{children:"Do not use the subtype names from predefined packages."}),"\n",(0,s.jsx)(n.li,{children:"Use predicate clauses or adjectives for Boolean objects."}),"\n",(0,s.jsx)(n.li,{children:"Use singular, specific nouns as object identifiers."}),"\n",(0,s.jsx)(n.li,{children:"Choose identifiers that describe the object's value during\nexecution."}),"\n",(0,s.jsx)(n.li,{children:"Use singular, general nouns as identifiers for record components."}),"\n",(0,s.jsx)(n.li,{children:"Use a consistent naming convention for tagged types and associated\npackages."}),"\n",(0,s.jsx)(n.li,{children:"Use action verbs for procedures and entries."}),"\n",(0,s.jsx)(n.li,{children:"Use predicate clauses for Boolean functions."}),"\n",(0,s.jsx)(n.li,{children:"Use nouns for non-Boolean functions."}),"\n",(0,s.jsx)(n.li,{children:"Give packages names that imply a higher level of organization than\nsubprograms. Generally, these are noun phrases that describe the\nabstraction provided."}),"\n",(0,s.jsx)(n.li,{children:"Give tasks names that imply an active entity."}),"\n",(0,s.jsx)(n.li,{children:"Use nouns descriptive of the data being protected for protected\nunits."}),"\n",(0,s.jsx)(n.li,{children:"Consider naming generic subprograms as if they were nongeneric\nsubprograms."}),"\n",(0,s.jsx)(n.li,{children:"Consider naming generic packages as if they were nongeneric\npackages."}),"\n",(0,s.jsx)(n.li,{children:"Make the generic names more general than the instantiated names."}),"\n",(0,s.jsx)(n.li,{children:"Use symbolic values instead of literals, wherever possible."}),"\n",(0,s.jsx)(n.li,{children:"Use the predefined constants Ada.Numerics.Pi and Ada.Numerics.e for\nthe mathematical constants Pi and e."}),"\n",(0,s.jsx)(n.li,{children:"Use constants instead of variables for constant values."}),"\n",(0,s.jsx)(n.li,{children:"Use a constant when the value is specific to a type or when the\nvalue must be static."}),"\n",(0,s.jsx)(n.li,{children:"Use named numbers instead of constants, whenever possible."}),"\n",(0,s.jsx)(n.li,{children:"Use named numbers to replace numeric literals whose type or context\nis truly universal."}),"\n",(0,s.jsx)(n.li,{children:"Use constants for objects whose values cannot change after\nelaboration. (United Technologies 1987)."}),"\n",(0,s.jsx)(n.li,{children:"Show relationships between symbolic values by defining them with\nstatic expressions."}),"\n",(0,s.jsx)(n.li,{children:"Use linearly independent sets of literals."}),"\n",(0,s.jsx)(n.li,{children:"Use attributes like 'First and 'Last instead of literals, wherever\npossible."}),"\n",(0,s.jsx)(n.li,{children:"Use a name that indicates the kind of problem the exception\nrepresents."}),"\n",(0,s.jsx)(n.li,{children:"Include a prefix like New, Make, or Create in naming constructors\n(in this sense, operations to create and/or initialize an object)."}),"\n",(0,s.jsx)(n.li,{children:"Use names indicative of their content for child packages containing\nconstructors."}),"\n"]}),"\n",(0,s.jsx)(n.h3,{id:"comments",children:"comments"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Make the code as clear as possible to reduce the need for comments."}),"\n",(0,s.jsx)(n.li,{children:"Never repeat information in a comment that is readily available in\nthe code."}),"\n",(0,s.jsx)(n.li,{children:"Where a comment is required, make it concise and complete."}),"\n",(0,s.jsx)(n.li,{children:"Use proper grammar and spelling in comments."}),"\n",(0,s.jsx)(n.li,{children:"Make comments visually distinct from the code."}),"\n",(0,s.jsx)(n.li,{children:"Structure comments in headers so that information can be\nautomatically extracted by a tool."}),"\n",(0,s.jsx)(n.li,{children:"Put a file header on each source file."}),"\n",(0,s.jsx)(n.li,{children:"Place ownership, responsibility, and history information for the\nfile in the file header."}),"\n",(0,s.jsx)(n.li,{children:"Put a header on the specification of each program unit."}),"\n",(0,s.jsx)(n.li,{children:"Place information required by the user of the program unit in the\nspecification header."}),"\n",(0,s.jsx)(n.li,{children:"Do not repeat information (except unit name) in the specification\nheader that is present in the specification."}),"\n",(0,s.jsx)(n.li,{children:"Explain what the unit does, not how or why it does it."}),"\n",(0,s.jsx)(n.li,{children:"Describe the complete interface to the program unit, including any\nexceptions it can raise and any global effects it can have."}),"\n",(0,s.jsx)(n.li,{children:"Do not include information about how the unit fits into the\nenclosing software system."}),"\n",(0,s.jsx)(n.li,{children:"Describe the performance (time and space) characteristics of the\nunit."}),"\n",(0,s.jsx)(n.li,{children:"Place information required by the maintainer of the program unit in\nthe body header."}),"\n",(0,s.jsx)(n.li,{children:"Explain how and why the unit performs its function, not what the\nunit does."}),"\n",(0,s.jsx)(n.li,{children:"Do not repeat information (except unit name) in the header that is\nreadily apparent from reading the code."}),"\n",(0,s.jsx)(n.li,{children:"Do not repeat information (except unit name) in the body header that\nis available in the specification header."}),"\n",(0,s.jsx)(n.li,{children:"Comment on all data types, objects, and exceptions unless their\nnames are self-explanatory."}),"\n",(0,s.jsx)(n.li,{children:"Include information on the semantic structure of complex,\npointer-based data structures."}),"\n",(0,s.jsx)(n.li,{children:"Include information about relationships that are maintained between\ndata objects."}),"\n",(0,s.jsx)(n.li,{children:"Omit comments that merely repeat the information in the name."}),"\n",(0,s.jsx)(n.li,{children:"Include information on redispatching for tagged types in cases where\nyou intend the specializations (i.e., derived types) to override\nthese redispatching operations."}),"\n",(0,s.jsx)(n.li,{children:"Minimize comments embedded among statements."}),"\n",(0,s.jsx)(n.li,{children:"Use comments only to explain parts of the code that are not obvious."}),"\n",(0,s.jsx)(n.li,{children:"Comment intentional omissions from the code."}),"\n",(0,s.jsx)(n.li,{children:"Do not use comments to paraphrase the code."}),"\n",(0,s.jsx)(n.li,{children:"Do not use comments to explain remote pieces of code, such as\nsubprograms called by the current unit."}),"\n",(0,s.jsx)(n.li,{children:"Where comments are necessary, make them visually distinct from the\ncode."}),"\n",(0,s.jsx)(n.li,{children:"Use pagination markers to mark program unit boundaries."}),"\n",(0,s.jsx)(n.li,{children:"Repeat the unit name in a comment to mark the begin of a package\nbody, subprogram body, task body, or block if the begin is preceded\nby declarations."}),"\n",(0,s.jsx)(n.li,{children:"For long or heavily nested if and case statements, mark the end of\nthe statement with a comment summarizing the condition governing the\nstatement."}),"\n",(0,s.jsx)(n.li,{children:"For long or heavily nested if statements, mark the else part with a\ncomment summarizing the conditions governing this portion of the\nstatement."}),"\n"]}),"\n",(0,s.jsx)(n.h3,{id:"using-types",children:"using types"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Limit the range of scalar types as much as possible."}),"\n",(0,s.jsx)(n.li,{children:"Seek information about possible values from the application."}),"\n",(0,s.jsx)(n.li,{children:"Do not reuse any of the subtype names in package Standard."}),"\n",(0,s.jsx)(n.li,{children:"Use subtype declarations to improve program readability (Booch\n1987)."}),"\n",(0,s.jsx)(n.li,{children:"Use derived types and subtypes in concert."}),"\n",(0,s.jsx)(n.li,{children:"Use enumeration types instead of numeric codes."}),"\n",(0,s.jsx)(n.li,{children:"Only if absolutely necessary, use representation clauses to match\nrequirements of external devices."}),"\n"]}),"\n",(0,s.jsx)(n.admonition,{type:"note",children:(0,s.jsxs)(n.p,{children:['This page of the "Ada Quality and Style Guide" has been adapted from the\noriginal work at ',(0,s.jsx)(n.a,{href:"https://en.wikibooks.org/wiki/Ada_Style_Guide",children:"https://en.wikibooks.org/wiki/Ada_Style_Guide"}),", which is\nlicensed under the\n",(0,s.jsx)(n.a,{href:"https://creativecommons.org/licenses/by-sa/3.0/",children:"Creative Commons Attribution-ShareAlike License"}),";\nadditional terms may apply. Page not endorsed by Wikibooks or the Ada\nStyle Guide Wikibook authors. This page is licensed under the same license\nas the original work."]})})]})}function h(e={}){const{wrapper:n}={...(0,t.R)(),...e.components};return n?(0,s.jsx)(n,{...e,children:(0,s.jsx)(d,{...e})}):d(e)}}}]);