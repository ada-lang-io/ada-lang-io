"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[4047],{5521:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>g,contentTitle:()=>m,default:()=>v,frontMatter:()=>c,metadata:()=>h,toc:()=>f});var a=t(91716),i=Object.defineProperty,r=Object.defineProperties,o=Object.getOwnPropertyDescriptors,s=Object.getOwnPropertySymbols,l=Object.prototype.hasOwnProperty,d=Object.prototype.propertyIsEnumerable,u=(e,n,t)=>n in e?i(e,n,{enumerable:!0,configurable:!0,writable:!0,value:t}):e[n]=t,p=(e,n)=>{for(var t in n||(n={}))l.call(n,t)&&u(e,t,n[t]);if(s)for(var t of s(n))d.call(n,t)&&u(e,t,n[t]);return e};const c={title:"3.4 Using Types"},m=void 0,h={unversionedId:"style-guide/s3/04",id:"style-guide/s3/04",title:"3.4 Using Types",description:"Strong typing promotes reliability in software. The type definition of",source:"@site/docs/style-guide/s3/04.mdx",sourceDirName:"style-guide/s3",slug:"/style-guide/s3/04",permalink:"/docs/style-guide/s3/04",draft:!1,tags:[],version:"current",frontMatter:{title:"3.4 Using Types"},sidebar:"styleGuideSidebar",previous:{title:"3.3 Comments",permalink:"/docs/style-guide/s3/03"},next:{title:"3.5 Summary",permalink:"/docs/style-guide/s3/05"}},g={},f=[{value:"Declaring Types",id:"declaring-types",level:3},{value:"guideline",id:"guideline",level:4},{value:"example",id:"example",level:4},{value:"rationale",id:"rationale",level:4},{value:"notes",id:"notes",level:4},{value:"exceptions",id:"exceptions",level:4},{value:"Enumeration Types",id:"enumeration-types",level:3},{value:"guideline",id:"guideline-1",level:4},{value:"example",id:"example-1",level:4},{value:"rationale",id:"rationale-1",level:4}],y={toc:f};function v(e){var n,t=e,{components:i}=t,u=((e,n)=>{var t={};for(var a in e)l.call(e,a)&&n.indexOf(a)<0&&(t[a]=e[a]);if(null!=e&&s)for(var a of s(e))n.indexOf(a)<0&&d.call(e,a)&&(t[a]=e[a]);return t})(t,["components"]);return(0,a.kt)("wrapper",(n=p(p({},y),u),r(n,o({components:i,mdxType:"MDXLayout"}))),(0,a.kt)("p",null,"Strong typing promotes reliability in software. The type definition of\nan object defines all legal values and operations and allows the\ncompiler to check for and identify potential errors during compilation.\nIn addition, the rules of type allow the compiler to generate code to\ncheck for violations of type constraints at execution time. Using these\nAda compiler's features facilitates earlier and more complete error\ndetection than that which is available with less strongly typed\nlanguages."),(0,a.kt)("h3",p({},{id:"declaring-types"}),"Declaring Types"),(0,a.kt)("h4",p({},{id:"guideline"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Limit the range of scalar types as much as possible."),(0,a.kt)("li",{parentName:"ul"},"Seek information about possible values from the application."),(0,a.kt)("li",{parentName:"ul"},"Do not reuse any of the subtype names in package Standard."),(0,a.kt)("li",{parentName:"ul"},"Use subtype declarations to improve program readability (Booch\n1987)."),(0,a.kt)("li",{parentName:"ul"},"Use derived types and subtypes in concert (see Guideline 5.3.1).")),(0,a.kt)("h4",p({},{id:"example"}),"example"),(0,a.kt)("pre",null,(0,a.kt)("code",p({parentName:"pre"},{className:"language-ada"}),"subtype Card_Image is String (1 .. 80);\nInput_Line : Card_Image := (others => ' ');\n-- restricted integer type:\ntype    Day_Of_Leap_Year     is                  range 1 .. 366;\nsubtype Day_Of_Non_Leap_Year is Day_Of_Leap_Year range 1 .. 365;\n")),(0,a.kt)("p",null,'By the following declaration, the programmer means, "I haven\'t the\nfoggiest idea how many," but the actual base range will show up buried\nin the code or as a system parameter:'),(0,a.kt)("pre",null,(0,a.kt)("code",p({parentName:"pre"},{className:"language-ada"}),"Employee_Count : Integer;\n")),(0,a.kt)("h4",p({},{id:"rationale"}),"rationale"),(0,a.kt)("p",null,"Eliminating meaningless values from the legal range improves the\ncompiler's ability to detect errors when an object is set to an invalid\nvalue. This also improves program readability. In addition, it forces\nyou to carefully think about each use of objects declared to be of the\nsubtype."),(0,a.kt)("p",null,"Different implementations provide different sets of values for most of\nthe predefined types. A reader cannot determine the intended range from\nthe predefined names. This situation is aggravated when the predefined\nnames are overloaded."),(0,a.kt)("p",null,"The names of an object and its subtype can clarify their intended use\nand document low-level design decisions. The example above documents a\ndesign decision to restrict the software to devices whose physical\nparameters are derived from the characteristics of punch cards. This\ninformation is easy to find for any later changes, thus enhancing\nprogram maintainability."),(0,a.kt)("p",null,"You can rename a type by declaring a subtype without a constraint (Ada\nReference Manual 1995, \xa78.5). You cannot overload a subtype name;\noverloading only applies to callable entities. Enumeration literals are\ntreated as parameterless functions and so are included in this rule."),(0,a.kt)("p",null,"Types can have highly constrained sets of values without eliminating\nuseful values. Usage as described in Guideline 5.3.1 eliminates many\nflag variables and type conversions within executable statements. This\nrenders the program more readable while allowing the compiler to enforce\nstrong typing constraints."),(0,a.kt)("h4",p({},{id:"notes"}),"notes"),(0,a.kt)("p",null,"Subtype declarations do not define new types, only constraints for\nexisting types."),(0,a.kt)("p",null,"Any deviation from this guideline detracts from the advantages of the\nstrong typing facilities of the Ada language."),(0,a.kt)("h4",p({},{id:"exceptions"}),"exceptions"),(0,a.kt)("p",null,"There are cases where you do not have a particular dependence on any\nrange of numeric values. Such situations occur, for example, with array\nindices (e.g., a list whose size is not fixed by any particular\nsemantics). See Guideline 7.2.1 for a discussion of appropriate uses of\npredefined types."),(0,a.kt)("h3",p({},{id:"enumeration-types"}),"Enumeration Types"),(0,a.kt)("h4",p({},{id:"guideline-1"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Use enumeration types instead of numeric codes."),(0,a.kt)("li",{parentName:"ul"},"Only if absolutely necessary, use representation clauses to match\nrequirements of external devices.")),(0,a.kt)("h4",p({},{id:"example-1"}),"example"),(0,a.kt)("p",null,"Use:"),(0,a.kt)("pre",null,(0,a.kt)("code",p({parentName:"pre"},{className:"language-ada"}),"type Color is (Blue, Red, Green, Yellow);\n")),(0,a.kt)("p",null,"rather than:"),(0,a.kt)("pre",null,(0,a.kt)("code",p({parentName:"pre"},{className:"language-ada"}),"Blue   : constant := 1;\nRed    : constant := 2;\nGreen  : constant := 3;\nYellow : constant := 4;\n")),(0,a.kt)("p",null,"and add the following if necessary:"),(0,a.kt)("pre",null,(0,a.kt)("code",p({parentName:"pre"},{className:"language-ada"}),"for Color use (Blue   => 1,\n               Red    => 2,\n               Green  => 3,\n               Yellow => 4);\n")),(0,a.kt)("h4",p({},{id:"rationale-1"}),"rationale"),(0,a.kt)("p",null,"Enumerations are more robust than numeric codes; they leave less\npotential for errors resulting from incorrect interpretation and from\nadditions to and deletions from the set of values during maintenance.\nNumeric codes are holdovers from languages that have no user-defined\ntypes."),(0,a.kt)("p",null,"In addition, Ada provides a number of attributes ('Pos, 'Val, 'Succ,\n'Pred, 'Image, and 'Value) for enumeration types that, when used, are\nmore reliable than user-written operations on encodings."),(0,a.kt)("p",null,'A numeric code may at first seem appropriate to match external values.\nInstead, these situations call for a representation clause on the\nenumeration type. The representation clause documents the "encoding." If\nthe program is properly structured to isolate and encapsulate hardware\ndependencies (see Guideline 7.1.5), the numeric code ends up in an\ninterface package where it can be easily found and replaced if the\nrequirements change.'),(0,a.kt)("p",null,"In general, avoid using representation clauses for enumeration types.\nWhen there is no obvious ordering of the enumeration literals, an\nenumeration representation can create portability problems if the\nenumeration type must be reordered to accommodate a change in\nrepresentation order on the new platform."))}v.isMDXComponent=!0}}]);