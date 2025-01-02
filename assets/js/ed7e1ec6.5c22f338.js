"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2081],{7770:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>l,contentTitle:()=>o,default:()=>h,frontMatter:()=>t,metadata:()=>a,toc:()=>d});const a=JSON.parse('{"id":"style-guide/s3/01","title":"3.1 Spelling","description":"Spelling conventions in source code include rules for capitalization and","source":"@site/docs/style-guide/s3/01.mdx","sourceDirName":"style-guide/s3","slug":"/style-guide/s3/01","permalink":"/docs/style-guide/s3/01","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"title":"3.1 Spelling"},"sidebar":"styleGuideSidebar","previous":{"title":"3. Readability","permalink":"/docs/style-guide/s3/"},"next":{"title":"3.2 Naming Conventions","permalink":"/docs/style-guide/s3/02"}}');var s=i(4848),r=i(8453);const t={title:"3.1 Spelling"},o=void 0,l={},d=[{value:"Use of Underscores",id:"use-of-underscores",level:3},{value:"guideline",id:"guideline",level:4},{value:"example",id:"example",level:4},{value:"rationale",id:"rationale",level:4},{value:"Numbers",id:"numbers",level:3},{value:"guideline",id:"guideline-1",level:4},{value:"instantiation",id:"instantiation",level:4},{value:"example",id:"example-1",level:4},{value:"rationale",id:"rationale-1",level:4},{value:"notes",id:"notes",level:4},{value:"Capitalization",id:"capitalization",level:3},{value:"guideline",id:"guideline-2",level:4},{value:"instantiation",id:"instantiation-1",level:4},{value:"example",id:"example-2",level:4},{value:"rationale",id:"rationale-2",level:4},{value:"automation notes",id:"automation-notes",level:4},{value:"Abbreviations",id:"abbreviations",level:3},{value:"guideline",id:"guideline-3",level:4},{value:"example",id:"example-3",level:4},{value:"rationale",id:"rationale-3",level:4}];function c(e){const n={code:"code",h3:"h3",h4:"h4",li:"li",p:"p",pre:"pre",ul:"ul",...(0,r.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(n.p,{children:"Spelling conventions in source code include rules for capitalization and\nuse of underscores, numbers, and abbreviations. If you follow these\nconventions consistently, the resulting code is clearer and more\nreadable."}),"\n",(0,s.jsx)(n.h3,{id:"use-of-underscores",children:"Use of Underscores"}),"\n",(0,s.jsx)(n.h4,{id:"guideline",children:"guideline"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Use underscores to separate words in a compound name."}),"\n"]}),"\n",(0,s.jsx)(n.h4,{id:"example",children:"example"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-none",children:"Miles_Per_Hour\nEntry_Value\n"})}),"\n",(0,s.jsx)(n.h4,{id:"rationale",children:"rationale"}),"\n",(0,s.jsx)(n.p,{children:"When an identifier consists of more than one word, it is much easier to\nread if the words are separated by underscores. Indeed, there is\nprecedent in English in which compound words are separated by a hyphen\nor a space. In addition to promoting readability of the code, if\nunderscores are used in names, a code formatter has more control over\naltering capitalization. See Guideline 3.1.3."}),"\n",(0,s.jsx)(n.h3,{id:"numbers",children:"Numbers"}),"\n",(0,s.jsx)(n.h4,{id:"guideline-1",children:"guideline"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Represent numbers in a consistent fashion."}),"\n",(0,s.jsx)(n.li,{children:"Represent literals in a radix appropriate to the problem."}),"\n",(0,s.jsx)(n.li,{children:"Use underscores to separate digits the same way commas or periods\n(or spaces for nondecimal bases) would be used in normal text."}),"\n",(0,s.jsx)(n.li,{children:"When using scientific notation, make the E consistently either\nuppercase or lowercase."}),"\n",(0,s.jsx)(n.li,{children:"In an alternate base, represent the alphabetic characters in either\nall uppercase or all lowercase."}),"\n"]}),"\n",(0,s.jsx)(n.h4,{id:"instantiation",children:"instantiation"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Decimal and octal numbers are grouped by threes beginning on the\nleft side of the radix point and by fives beginning on the right\nside of the radix point."}),"\n",(0,s.jsx)(n.li,{children:"The E is always capitalized in scientific notation."}),"\n",(0,s.jsx)(n.li,{children:"Use uppercase for the alphabetic characters representing digits in\nbases above 10."}),"\n",(0,s.jsx)(n.li,{children:"Hexadecimal numbers are grouped by fours beginning on either side of\nthe radix point."}),"\n"]}),"\n",(0,s.jsx)(n.h4,{id:"example-1",children:"example"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ada",children:"type Maximum_Samples     is range          1 ..  1_000_000;\ntype Legal_Hex_Address   is range   16#0000# ..   16#FFFF#;\ntype Legal_Octal_Address is range 8#000_000# .. 8#777_777#;\n\nAvogadro_Number : constant := 6.02216_9E+23;\n"})}),"\n",(0,s.jsx)(n.p,{children:"To represent the number 1/3 as a constant, use:"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ada",children:"One_Third : constant := 1.0 / 3.0;\n"})}),"\n",(0,s.jsx)(n.p,{children:"Avoid this use:"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ada",children:"One_Third_As_Decimal_Approximation : constant := 0.33333_33333_3333;\n"})}),"\n",(0,s.jsx)(n.p,{children:"or:"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ada",children:"One_Third_Base_3 : constant := 3#0.1#;\n"})}),"\n",(0,s.jsx)(n.h4,{id:"rationale-1",children:"rationale"}),"\n",(0,s.jsx)(n.p,{children:"Consistent use of uppercase or lowercase aids scanning for numbers.\nUnderscores serve to group portions of numbers into familiar patterns.\nConsistency with common use in everyday contexts is a large part of\nreadability."}),"\n",(0,s.jsx)(n.h4,{id:"notes",children:"notes"}),"\n",(0,s.jsx)(n.p,{children:"If a rational fraction is represented in a base in which it has a\nterminating rather than a repeating representation, as 3#0.1# does in\nthe example above, it may have increased accuracy upon conversion to the\nmachine base. (This is wrong for named numbers as in this example - they\nmust be calculated exactly.)"}),"\n",(0,s.jsx)(n.h3,{id:"capitalization",children:"Capitalization"}),"\n",(0,s.jsx)(n.h4,{id:"guideline-2",children:"guideline"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Make reserved words and other elements of the program visually\ndistinct from each other."}),"\n"]}),"\n",(0,s.jsx)(n.h4,{id:"instantiation-1",children:"instantiation"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Use lowercase for all reserved words (when used as reserved words)."}),"\n",(0,s.jsx)(n.li,{children:"Use mixed case for all other identifiers, a capital letter beginning\nevery word separated by underscores."}),"\n",(0,s.jsx)(n.li,{children:"Use uppercase for abbreviations and acronyms (see automation notes)."}),"\n"]}),"\n",(0,s.jsx)(n.h4,{id:"example-2",children:"example"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ada",children:"...\n\ntype Second_Of_Day      is range 0 .. 86_400;\ntype Noon_Relative_Time is (Before_Noon, After_Noon, High_Noon);\n\nsubtype Morning   is Second_Of_Day range 0 .. 86_400 / 2 - 1;\nsubtype Afternoon is Second_Of_Day range Morning'Last + 2 .. 86_400;\n\n...\n\nCurrent_Time := Second_Of_Day(Calendar.Seconds(Calendar.Clock));\nif Current_Time in Morning then\n   Time_Of_Day := Before_Noon;\nelsif Current_Time in Afternoon then\n   Time_Of_Day := After_Noon;\nelse\n   Time_Of_Day := High_Noon;\nend if;\n\ncase Time_Of_Day is\n   when Before_Noon =>   Get_Ready_For_Lunch;\n   when High_Noon   =>   Eat_Lunch;\n   when After_Noon  =>   Get_To_Work;\nend case;\n\n...\n"})}),"\n",(0,s.jsx)(n.h4,{id:"rationale-2",children:"rationale"}),"\n",(0,s.jsx)(n.p,{children:"Visually distinguishing reserved words allows you to focus on program\nstructure alone, if desired, and also aids scanning for particular\nidentifiers."}),"\n",(0,s.jsx)(n.p,{children:"The instantiation chosen here is meant to be more readable for the\nexperienced Ada programmer, who does not need reserved words to leap off\nthe page. Beginners to any language often find that reserved words\nshould be emphasized to help them find the control structures more\neasily. Because of this, instructors in the classroom and books\nintroducing the Ada language may want to consider an alternative\ninstantiation. The Ada Reference Manual (1995) chose bold lowercase for\nall reserved words."}),"\n",(0,s.jsx)(n.h4,{id:"automation-notes",children:"automation notes"}),"\n",(0,s.jsxs)(n.p,{children:["Ada names are not case sensitive. Therefore, the names ",(0,s.jsx)(n.code,{children:"max_limit"}),",\n",(0,s.jsx)(n.code,{children:"MAX_LIMIT"}),", and ",(0,s.jsx)(n.code,{children:"Max_Limit"})," denote the same object or entity. A good\ncode formatter should be able to automatically convert from one style to\nanother as long as the words are delimited by underscores."]}),"\n",(0,s.jsx)(n.p,{children:"As recommended in Guideline 3.1.4, abbreviations should be project-wide.\nAn automated tool should allow a project to specify those abbreviations\nand format them accordingly."}),"\n",(0,s.jsx)(n.h3,{id:"abbreviations",children:"Abbreviations"}),"\n",(0,s.jsx)(n.h4,{id:"guideline-3",children:"guideline"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:"Do not use an abbreviation of a long word as an identifier where a\nshorter synonym exists."}),"\n",(0,s.jsx)(n.li,{children:"Use a consistent abbreviation strategy."}),"\n",(0,s.jsx)(n.li,{children:"Do not use ambiguous abbreviations."}),"\n",(0,s.jsx)(n.li,{children:"To justify its use, an abbreviation must save many characters over\nthe full word."}),"\n",(0,s.jsx)(n.li,{children:"Use abbreviations that are well-accepted in the application domain."}),"\n",(0,s.jsx)(n.li,{children:"Maintain a list of accepted abbreviations, and use only\nabbreviations on that list."}),"\n"]}),"\n",(0,s.jsx)(n.h4,{id:"example-3",children:"example"}),"\n",(0,s.jsx)(n.p,{children:"Use:"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ada",children:"Time_Of_Receipt\n"})}),"\n",(0,s.jsx)(n.p,{children:"rather than:"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ada",children:"Recd_Time or R_Time\n"})}),"\n",(0,s.jsxs)(n.p,{children:["But in an application that commonly deals with message formats that meet\nmilitary standards, ",(0,s.jsx)(n.code,{children:"DOD_STD_MSG_FMT"})," is an acceptable abbreviation for:"]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-ada",children:"Department_Of_Defense_Standard_Message_Format.\n"})}),"\n",(0,s.jsx)(n.h4,{id:"rationale-3",children:"rationale"}),"\n",(0,s.jsx)(n.p,{children:"Many abbreviations are ambiguous or unintelligible unless taken in\ncontext. As an example, Temp could indicate either temporary or\ntemperature. For this reason, you should choose abbreviations carefully\nwhen you use them. The rationale in Guideline 8.1.2 provides a more\nthorough discussion of how context should influence the use of\nabbreviations."}),"\n",(0,s.jsx)(n.p,{children:"Because very long variable names can obscure the structure of the\nprogram, especially in deeply nested (indented) control structures, it\nis a good idea to try to keep identifiers short and meaningful. Use\nshort unabbreviated names whenever possible. If there is no short word\nthat will serve as an identifier, then a well-known unambiguous\nabbreviation is the next best choice, especially if it comes from a list\nof standard abbreviations used throughout the project."}),"\n",(0,s.jsx)(n.p,{children:"You can establish an abbreviated format for a fully qualified name using\nthe renames clause. This capability is useful when a very long, fully\nqualified name would otherwise occur many times in a localized section\nof code (see Guideline 5.7.2)."}),"\n",(0,s.jsx)(n.p,{children:"A list of accepted abbreviations for a project provides a standard\ncontext for using each abbreviation."})]})}function h(e={}){const{wrapper:n}={...(0,r.R)(),...e.components};return n?(0,s.jsx)(n,{...e,children:(0,s.jsx)(c,{...e})}):c(e)}}}]);