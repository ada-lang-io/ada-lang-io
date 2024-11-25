"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[6905],{11897:(e,a,n)=>{n.r(a),n.d(a,{assets:()=>l,contentTitle:()=>o,default:()=>h,frontMatter:()=>s,metadata:()=>t,toc:()=>d});const t=JSON.parse('{"id":"style-guide/s5/02","title":"5.2 Parameter Lists","description":"A subprogram or entry parameter list is the interface to the abstraction","source":"@site/docs/style-guide/s5/02.mdx","sourceDirName":"style-guide/s5","slug":"/style-guide/s5/02","permalink":"/docs/style-guide/s5/02","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{"title":"5.2 Parameter Lists"},"sidebar":"styleGuideSidebar","previous":{"title":"5.1 Optional Parts of the Syntax","permalink":"/docs/style-guide/s5/01"},"next":{"title":"5.3 Types","permalink":"/docs/style-guide/s5/03"}}');var i=n(74848),r=n(28453);const s={title:"5.2 Parameter Lists"},o=void 0,l={},d=[{value:"Formal Parameters",id:"formal-parameters",level:3},{value:"guideline",id:"guideline",level:4},{value:"example",id:"example",level:4},{value:"rationale",id:"rationale",level:4},{value:"Named Association",id:"named-association",level:3},{value:"guideline",id:"guideline-1",level:4},{value:"instantiation",id:"instantiation",level:4},{value:"example",id:"example-1",level:4},{value:"rationale",id:"rationale-1",level:4},{value:"notes",id:"notes",level:4},{value:"caution",id:"caution",level:4},{value:"Default Parameters",id:"default-parameters",level:3},{value:"guideline",id:"guideline-2",level:4},{value:"example",id:"example-2",level:4},{value:"rationale",id:"rationale-2",level:4},{value:"exceptions",id:"exceptions",level:4},{value:"Mode Indication",id:"mode-indication",level:3},{value:"guideline",id:"guideline-3",level:4},{value:"example",id:"example-3",level:4},{value:"rationale",id:"rationale-3",level:4},{value:"exceptions",id:"exceptions-1",level:4}];function c(e){const a={code:"code",em:"em",h3:"h3",h4:"h4",li:"li",p:"p",ul:"ul",...(0,r.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(a.p,{children:"A subprogram or entry parameter list is the interface to the abstraction\nimplemented by the subprogram or entry. It is important that it is clear\nand that it is expressed in a consistent style. Careful decisions about\nformal parameter naming and ordering can make the purpose of the\nsubprogram easier to understand, which can make it easier to use."}),"\n",(0,i.jsx)(a.h3,{id:"formal-parameters",children:"Formal Parameters"}),"\n",(0,i.jsx)(a.h4,{id:"guideline",children:"guideline"}),"\n",(0,i.jsxs)(a.ul,{children:["\n",(0,i.jsx)(a.li,{children:"Name formal parameters descriptively to reduce the need for\ncomments."}),"\n"]}),"\n",(0,i.jsx)(a.h4,{id:"example",children:"example"}),"\n",(0,i.jsx)(a.p,{children:"List_Manager.Insert (Element     => New_Employee,\nInto_List   => Probationary_Employees,\nAt_Position => 1);"}),"\n",(0,i.jsx)(a.h4,{id:"rationale",children:"rationale"}),"\n",(0,i.jsx)(a.p,{children:"Following the variable naming guidelines ( 3.2.1 and 3.2.3 ) for formal\nparameters can make calls to subprograms read more like regular prose,\nas shown in the example above, where no comments are necessary.\nDescriptive names of this sort can also make the code in the body of the\nsubprogram more clear."}),"\n",(0,i.jsx)(a.h3,{id:"named-association",children:"Named Association"}),"\n",(0,i.jsx)(a.h4,{id:"guideline-1",children:"guideline"}),"\n",(0,i.jsxs)(a.ul,{children:["\n",(0,i.jsx)(a.li,{children:"Use named parameter association in calls of infrequently used\nsubprograms or entries with many formal parameters."}),"\n",(0,i.jsx)(a.li,{children:"Use named association when instantiating generics."}),"\n",(0,i.jsx)(a.li,{children:"Use named association for clarification when the actual parameter is\nany literal or expression."}),"\n",(0,i.jsx)(a.li,{children:"Use named association when supplying a nondefault value to an\noptional parameter."}),"\n"]}),"\n",(0,i.jsx)(a.h4,{id:"instantiation",children:"instantiation"}),"\n",(0,i.jsxs)(a.ul,{children:["\n",(0,i.jsx)(a.li,{children:"Use named parameter association in calls of subprograms or entries\ncalled from less than five places in a single source file or with\nmore than two formal parameters."}),"\n"]}),"\n",(0,i.jsx)(a.h4,{id:"example-1",children:"example"}),"\n",(0,i.jsx)(a.p,{children:"Encode_Telemetry_Packet (Source         => Power_Electronics,\nContent        => Temperature,\nValue          => Read_Temperature_Sensor(Power_Electronics),\nTime           => Current_Time,\nSequence       => Next_Packet_ID,\nVehicle        => This_Spacecraft,\nPrimary_Module => True);"}),"\n",(0,i.jsx)(a.h4,{id:"rationale-1",children:"rationale"}),"\n",(0,i.jsx)(a.p,{children:"Calls of infrequently used subprograms or entries with many formal\nparameters can be difficult to understand without referring to the\nsubprogram or entry code. Named parameter association can make these\ncalls more readable."}),"\n",(0,i.jsx)(a.p,{children:"When the formal parameters have been named appropriately, it is easier\nto determine exactly what purpose the subprogram serves without looking\nat its code. This reduces the need for named constants that exist solely\nto make calls more readable. It also allows variables used as actual\nparameters to be given names indicating what they are without regard to\nwhy they are being passed in a call. An actual parameter, which is an\nexpression rather than a variable, cannot be named otherwise."}),"\n",(0,i.jsx)(a.p,{children:"Named association allows subprograms to have new parameters inserted\nwith minimal ramifications to existing calls."}),"\n",(0,i.jsx)(a.h4,{id:"notes",children:"notes"}),"\n",(0,i.jsx)(a.p,{children:"The judgment of when named parameter association improves readability is\nsubjective. Certainly, simple or familiar subprograms, such as a swap\nroutine or a sine function, do not require the extra clarification of\nnamed association in the procedure call."}),"\n",(0,i.jsx)(a.h4,{id:"caution",children:"caution"}),"\n",(0,i.jsx)(a.p,{children:"A consequence of named parameter association is that the formal\nparameter names may not be changed without modifying the text of each\ncall."}),"\n",(0,i.jsx)(a.h3,{id:"default-parameters",children:"Default Parameters"}),"\n",(0,i.jsx)(a.h4,{id:"guideline-2",children:"guideline"}),"\n",(0,i.jsxs)(a.ul,{children:["\n",(0,i.jsx)(a.li,{children:"Provide default parameters to allow for occasional, special use of\nwidely used subprograms or entries."}),"\n",(0,i.jsx)(a.li,{children:"Place default parameters at the end of the formal parameter list."}),"\n",(0,i.jsx)(a.li,{children:"Consider providing default values to new parameters added to an\nexisting subprogram."}),"\n"]}),"\n",(0,i.jsx)(a.h4,{id:"example-2",children:"example"}),"\n",(0,i.jsx)(a.p,{children:"Ada Reference Manual (1995) contains many examples of this practice."}),"\n",(0,i.jsx)(a.h4,{id:"rationale-2",children:"rationale"}),"\n",(0,i.jsx)(a.p,{children:"Often, the majority of uses of a subprogram or entry need the same value\nfor a given parameter. Providing that value, as the default for the\nparameter, makes the parameter optional on the majority of calls. It\nalso allows the remaining calls to customize the subprogram or entry by\nproviding different values for that parameter."}),"\n",(0,i.jsx)(a.p,{children:"Placing default parameters at the end of the formal parameter list\nallows the caller to use positional association on the call; otherwise,\ndefaults are available only when named association is used."}),"\n",(0,i.jsx)(a.p,{children:"Often during maintenance activities, you increase the functionality of a\nsubprogram or entry. This requires more parameters than the original\nform for some calls. New parameters may be required to control this new\nfunctionality. Give the new parameters default values that specify the\nold functionality. Calls needing the old functionality need not be\nchanged; they take the defaults. This is true if the new parameters are\nadded to the end of the parameter list, or if named association is used\non all calls. New calls needing the new functionality can specify that\nby providing other values for the new parameters."}),"\n",(0,i.jsxs)(a.p,{children:["This enhances maintainability in that the places that use the modified\nroutines do not themselves have to be modified, while the previous\nfunctionality levels of the routines are allowed to be ",(0,i.jsx)(a.em,{children:'"reused."'})]}),"\n",(0,i.jsx)(a.h4,{id:"exceptions",children:"exceptions"}),"\n",(0,i.jsx)(a.p,{children:"Do not go overboard. If the changes in functionality are truly radical,\nyou should be preparing a separate routine rather than modifying an\nexisting one. One indicator of this situation would be that it is\ndifficult to determine value combinations for the defaults that uniquely\nand naturally require the more restrictive of the two functions. In such\ncases, it is better to go ahead with creation of a separate routine."}),"\n",(0,i.jsx)(a.h3,{id:"mode-indication",children:"Mode Indication"}),"\n",(0,i.jsx)(a.h4,{id:"guideline-3",children:"guideline"}),"\n",(0,i.jsxs)(a.ul,{children:["\n",(0,i.jsx)(a.li,{children:"Show the mode indication of all procedure and entry parameters\n(Nissen and Wallis 1984 )."}),"\n",(0,i.jsx)(a.li,{children:"Use the most restrictive parameter mode applicable to your\napplication."}),"\n"]}),"\n",(0,i.jsx)(a.h4,{id:"example-3",children:"example"}),"\n",(0,i.jsx)(a.p,{children:"procedure Open_File (File_Name   : in     String;\nOpen_Status :    out Status_Codes);\nentry Acquire (Key      : in     Capability;\nResource :    out Tape_Drive);"}),"\n",(0,i.jsx)(a.h4,{id:"rationale-3",children:"rationale"}),"\n",(0,i.jsxs)(a.p,{children:["By showing the mode of parameters, you aid the reader. If you do not\nspecify a parameter mode, the default mode is ",(0,i.jsx)(a.code,{children:"in"}),". Explicitly showing\nthe mode indication of all parameters is a more assertive action than\nsimply taking the default mode. Anyone reviewing the code later will be\nmore confident that you intended the parameter mode to be ",(0,i.jsx)(a.code,{children:"in"}),"."]}),"\n",(0,i.jsxs)(a.p,{children:["Use the mode that reflects the actual use of the parameter. You should\navoid the tendency to make all parameters ",(0,i.jsx)(a.code,{children:"in out"})," mode because ",(0,i.jsx)(a.code,{children:"out"}),"\nmode parameters may be examined as well as updated."]}),"\n",(0,i.jsx)(a.h4,{id:"exceptions-1",children:"exceptions"}),"\n",(0,i.jsxs)(a.p,{children:["It may be necessary to consider several alternative implementations for\na given abstraction. For example, a bounded stack can be implemented as\na pointer to an array. Even though an update to the object being pointed\nto does not require changing the pointer value itself, you may want to\nconsider making the mode ",(0,i.jsx)(a.code,{children:"in out"})," to allow changes to the implementation\nand to document more accurately what the operation is doing. If you\nlater change the implementation to a simple array, the mode will have to\nbe ",(0,i.jsx)(a.code,{children:"in out"}),", potentially causing changes to all places that the routine\nis called."]})]})}function h(e={}){const{wrapper:a}={...(0,r.R)(),...e.components};return a?(0,i.jsx)(a,{...e,children:(0,i.jsx)(c,{...e})}):c(e)}}}]);