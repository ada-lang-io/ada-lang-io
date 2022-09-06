"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[3123],{8350:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>f,contentTitle:()=>h,default:()=>b,frontMatter:()=>p,metadata:()=>m,toc:()=>k});var i=n(1716),a=Object.defineProperty,o=Object.defineProperties,s=Object.getOwnPropertyDescriptors,l=Object.getOwnPropertySymbols,r=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,u=(e,t,n)=>t in e?a(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,d=(e,t)=>{for(var n in t||(t={}))r.call(t,n)&&u(e,n,t[n]);if(l)for(var n of l(t))c.call(t,n)&&u(e,n,t[n]);return e};const p={sidebar_position:186},h="H.4  High Integrity Restrictions",m={unversionedId:"arm/AA-H.4",id:"arm/AA-H.4",title:"H.4  High Integrity Restrictions",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-H.4.mdx",sourceDirName:"arm",slug:"/arm/AA-H.4",permalink:"/docs/arm/AA-H.4",draft:!1,tags:[],version:"current",sidebarPosition:186,frontMatter:{sidebar_position:186},sidebar:"tutorialSidebar",previous:{title:"H.3  Reviewable Object Code",permalink:"/docs/arm/AA-H.3"},next:{title:"H.5  Pragma Detect_Blocking",permalink:"/docs/arm/AA-H.5"}},f={},k=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Documentation Requirements",id:"documentation-requirements",level:4},{value:"Erroneous Execution",id:"erroneous-execution",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4},{value:"Incompatibilities With Ada 2012",id:"incompatibilities-with-ada-2012",level:4},{value:"Extensions to Ada 2012",id:"extensions-to-ada-2012",level:4},{value:"H.4.1  Aspect No_Controlled_Parts",id:"h41--aspect-no_controlled_parts",level:2},{value:"Static Semantics",id:"static-semantics-1",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Extensions to Ada 2012",id:"extensions-to-ada-2012-1",level:4}],g={toc:k};function b(e){var t,n=e,{components:a}=n,u=((e,t)=>{var n={};for(var i in e)r.call(e,i)&&t.indexOf(i)<0&&(n[i]=e[i]);if(null!=e&&l)for(var i of l(e))t.indexOf(i)<0&&c.call(e,i)&&(n[i]=e[i]);return n})(n,["components"]);return(0,i.kt)("wrapper",(t=d(d({},g),u),o(t,s({components:a,mdxType:"MDXLayout"}))),(0,i.kt)("h1",d({},{id:"h4--high-integrity-restrictions"}),"H.4  High Integrity Restrictions"),(0,i.kt)("admonition",d({},{type:"warning"}),(0,i.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,i.kt)("a",d({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0299-1"),"}"," This subclause defines restrictions that can be used with pragma Restrictions (see 13.12); these facilitate the demonstration of program correctness by allowing tailored versions of the run-time system. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"{",(0,i.kt)("em",null,"AI05-0005-1"),"}"," Note that the restrictions are absolute. If a partition has 100 library units and just one needs Unchecked_Conversion, then the pragma cannot be used to ensure the other 99 units do not use Unchecked_Conversion. Note also that these are restrictions on all Ada code within a partition, and therefore it might not be evident from the specification of a package whether a restriction can be imposed."),(0,i.kt)("h4",d({},{id:"static-semantics"}),"Static Semantics"),(0,i.kt)("p",null,(0,i.kt)("em",null,"This paragraph was deleted."),"{",(0,i.kt)("em",null,"AI95-00347-01"),"}"," ","{",(0,i.kt)("em",null,"AI95-00394-01"),"}"," "),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00394-01"),"}"," The following ",(0,i.kt)("em",null,"restriction_"),(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-2.3#S0002"},"identifier")),"s are language defined:"),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Tasking-related restriction:")),(0,i.kt)("p",null,"No_Protected_Types There are no declarations of protected types or protected objects. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Memory-management related restrictions:")),(0,i.kt)("p",null,"No_Allocators There are no occurrences of an ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.8#S0164"},"allocator")),"."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"8652/0042"),"}"," ","{",(0,i.kt)("em",null,"AI95-00130"),"}"," No_Local_Allocators ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.8#S0164"},"Allocator")),"s are prohibited in subprograms, generic subprograms, tasks, and entry bodies. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Ramification: "),"Thus ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.8#S0164"},"allocator")),"s are permitted only in expressions whose evaluation can only be performed before the main subprogram is invoked. "),(0,i.kt)("p",null,(0,i.kt)("em",null,"This paragraph was deleted."),"{",(0,i.kt)("em",null,"8652/0042"),"}"," ","{",(0,i.kt)("em",null,"AI95-00130"),"}"," "),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0152-1"),"}"," ","{",(0,i.kt)("em",null,"AI05-0262-1"),"}"," No_Anonymous_Allocators There are no ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.8#S0164"},"allocator")),"s of anonymous access types."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0190-1"),"}"," No_Coextensions There are no coextensions. See 3.10.2."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0190-1"),"}"," No_Access_Parameter_Allocators ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.8#S0164"},"Allocator")),"s are not permitted as the actual parameter to an access parameter. See 6.1."),(0,i.kt)("p",null,(0,i.kt)("em",null,"This paragraph was deleted."),"{",(0,i.kt)("em",null,"AI95-00394-01"),"}"," "),(0,i.kt)("p",null,"Immediate_Reclamation Except for storage occupied by objects created by ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.8#S0164"},"allocator")),"s and not deallocated via unchecked deallocation, any storage reserved at run time for an object is immediately reclaimed when the object no longer exists. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"Immediate reclamation would apply to storage created by the compiler, such as for a return value from a function whose size is not known at the call site. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Exception-related restriction:")),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0344-1"),"}"," No_Exceptions ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-11.3#S0308"},"Raise_statement")),"s and ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-11.2#S0305"},"exception_handler")),"s are not allowed. No language-defined runtime checks are generated; however, a runtime check performed automatically by the hardware is permitted. The callable entity associated with a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-5.5#S0185"},"procedural_iterator"))," (see 5.5.3) is considered to not allow exit, independent of the value of its Allows_Exit aspect."),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"This restriction mirrors a method of working that is quite common in the safety area. The programmer is required to show that exceptions cannot be raised. Then a simplified run-time system is used without exception handling. However, some hardware checks may still be enforced. If the software check would have failed, or if the hardware check actually fails, then the execution of the program is unpredictable. There are obvious dangers in this approach, but it is similar to programming at the assembler level."),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Other restrictions:")),(0,i.kt)("p",null,"No_Floating_Point Uses of predefined floating point types and operations, and declarations of new floating point types, are not allowed. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"{",(0,i.kt)("em",null,"AI95-00114-01"),"}",' The intention is to avoid the use of floating point hardware at run time, but this is expressed in language terms. It is conceivable that floating point is used implicitly in some contexts, say fixed point type conversions of high accuracy. However, the Implementation Requirements below make it clear that the restriction would apply to the "run-time system" and hence not be allowed. This restriction could be used to inform a compiler that a variant of the architecture is being used which does not have floating point instructions.'),(0,i.kt)("p",null,"No_Fixed_Point Uses of predefined fixed point types and operations, and declarations of new fixed point types, are not allowed. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"This restriction would have the side effect of prohibiting the ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-9.6#S0268"},"delay_relative_statement")),". As with the No_Floating_Point restriction, this might be used to avoid any question of rounding errors. Unless an Ada run-time is written in Ada, it seems hard to rule out implicit use of fixed point, since at the machine level, fixed point is virtually the same as integer arithmetic."),(0,i.kt)("p",null,(0,i.kt)("em",null,"This paragraph was deleted."),"{",(0,i.kt)("em",null,"AI95-00394-01"),"}"," "),(0,i.kt)("p",null,"No_Access_Subprograms The declaration of access-to-subprogram types is not allowed. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"Most critical applications would require some restrictions or additional validation checks on uses of access-to-subprogram types. If the application does not require the functionality, then this restriction provides a means of ensuring the design requirement has been satisfied. The same applies to several of the following restrictions, and to restriction No_Dependence =",">"," Ada.Unchecked_Conversion. "),(0,i.kt)("p",null,"No_Unchecked_Access The Unchecked_Access attribute is not allowed."),(0,i.kt)("p",null,"No_Dispatch Occurrences of T'Class are not allowed, for any (tagged) subtype T."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00285-01"),"}"," ","{",(0,i.kt)("em",null,"AI12-0318-1"),"}"," No_IO Semantic dependence on any of the library units Sequential_IO, Direct_IO, Text_IO, Wide_Text_IO, Wide_Wide_Text_IO, Stream_IO, or Directories is not allowed. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"Excluding the input-output facilities of an implementation may be needed in those environments which cannot support the supplied functionality. A program in such an environment is likely to require some low level facilities or a call on a non-Ada feature."),(0,i.kt)("p",null,"No_Delay ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-9.6#S0266"},"Delay_Statement")),"s and semantic dependence on package Calendar are not allowed. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Ramification: "),"This implies that ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-9.7#S0274"},"delay_alternative")),"s in a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-9.7#S0269"},"select_statement"))," are prohibited."),(0,i.kt)("p",null,"The purpose of this restriction is to avoid the need for timing facilities within the run-time system."),(0,i.kt)("p",null,"No_Recursion As part of the execution of a subprogram, the same subprogram is not invoked."),(0,i.kt)("p",null,"No_Reentrancy During the execution of a subprogram by a task, no other task invokes the same subprogram."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0079-3"),"}"," No_Unspecified_GlobalsNo library-level entity shall have a Global aspect of Unspecified, either explicitly or by default. No library-level entity shall have a Global'Class aspect of Unspecified, explicitly or by default, if it is used as part of a dispatching call."),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Ramification: "),"Global'Class need not be specified on an operation if there are no dispatching calls to the operation, or if all of the dispatching calls are covered by ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-H.7#S0366"},"dispatching_operation_specifier")),"s for operations with such calls (see H.7). "),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0079-3"),"}"," ","{",(0,i.kt)("em",null,"AI12-0380-1"),"}"," No_Hidden_Indirect_GlobalsWhen within a context where an applicable global aspect is neither Unspecified nor ",(0,i.kt)("strong",null,"in out all"),", any execution within such a context does neither of the following:"),(0,i.kt)("p",null,"Update (or return a writable reference to) a variable that is reachable via a sequence of zero or more dereferences of access-to-object values from a parameter of a visibly access-to-constant type, from a part of a non-access-type formal parameter of mode ",(0,i.kt)("strong",null,"in")," (after any ",(0,i.kt)("strong",null,"overriding"),"  see H.7), or from a global that has mode ",(0,i.kt)("strong",null,"in")," or is not within the applicable global variable set, unless the initial dereference is of a part of a formal parameter or global that is visibly of an access-to-variable type;"),(0,i.kt)("p",null,"Read (or return a readable reference to) a variable that is reachable via a sequence of zero or more dereferences of access-to-object values from a global that is not within the applicable global variable set, unless the initial dereference is of a part of a formal parameter or global that is visibly of an access-to-object type. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Ramification: "),"The above two rules specify that any hidden indirect references are covered by the global or formal parameter modes that apply, and are ",(0,i.kt)("em",null,"not")," subject to alternative paths of access (such as aliasing) that could result in conflicts. On the other hand, any visible access-to-object parts are allowed to designate objects that are accessible via other means, and side-effects on such objects are permitted if the value is visibly of an access-to-variable type. Such effects do not need to be covered by the applicable global aspect(s), but are rather for the caller to worry about. "),(0,i.kt)("p",null,"For the purposes of the above rules:"),(0,i.kt)("p",null,"a part of an object is ",(0,i.kt)("em",null,"visibly of an access type")," if the type of the object is declared immediately within the visible part of a package specification, and at the point of declaration of the type the part is visible and of an access type;"),(0,i.kt)("p",null,"a function ",(0,i.kt)("em",null,"returns a writable reference to V")," if it returns a result with a part that is visibly of an access-to-variable type designating ",(0,i.kt)("em",null,"V"),"; similarly, a function ",(0,i.kt)("em",null,"returns a readable reference to V")," if it returns a result with a part that is visibly of an access-to-constant type designating ",(0,i.kt)("em",null,"V"),";"),(0,i.kt)("p",null,"if an applicable global variable set includes a package name, and the collection of some pool-specific access type (see 7.6.1) is implicitly declared in a part of the declarative region of the package included within the global variable set, then all objects allocated from that collection are considered included within the global variable set. "),(0,i.kt)("p",null,"The consequences of violating the No_Hidden_Indirect_Globals restriction is implementation-defined. Any aspects or other means for identifying such violations prior to or during execution are implementation-defined. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Implementation defined: "),"The consequences of violating No_Hidden_Indirect_Globals."),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),'We do not make violations automatically erroneous, because if the implementation chooses to never fully trust it, there is nothing erroneous that can happen. If an implementation chooses to trust the restriction, and performs some optimization as a result of the restriction, the implementation would define such a violation as erroneous. Such an implementation might also endeavor to detect most violations, perhaps by providing additional aspects, thereby reducing the situations which result in erroneous execution. Implementations might detect some but not all violations of the restrictions. Implementations that completely ignore the restriction should treat the restriction as an unsupported capability of Annex H, "High Integrity Systems". '),(0,i.kt)("h4",d({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0020-1"),"}"," ","{",(0,i.kt)("em",null,"AI12-0340-1"),"}"," The following ",(0,i.kt)("em",null,"restriction_parameter_"),(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-2.3#S0002"},"identifier"))," is language defined:"),(0,i.kt)("p",null,"Max_Image_LengthSpecifies the maximum length for the result of an Image, Wide_Image, or Wide_Wide_Image attribute. Violation of this restriction results in the raising of Program_Error at the point of the invocation of an image attribute. "),(0,i.kt)("h4",d({},{id:"implementation-requirements"}),"Implementation Requirements"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00394-01"),"}"," An implementation of this Annex shall support: "),(0,i.kt)("p",null,"the restrictions defined in this subclause; and"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0189-1"),"}"," the following restrictions defined in D.7: No_Task_Hierarchy, No_Abort_Statement, No_Implicit_Heap_Allocation, No_Standard_Allocators_After_Elaboration; and"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00347-01"),"}"," the ",(0,i.kt)("strong",null,"pragma")," Profile(Ravenscar); and "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"{",(0,i.kt)("em",null,"AI95-00347-01"),"}"," The reference to pragma Profile(Ravenscar) is intended to show that properly restricted tasking is appropriate for use in high integrity systems. The Ada 95 Annex seemed to suggest that tasking was inappropriate for such systems. "),(0,i.kt)("p",null,"the following uses of ",(0,i.kt)("em",null,"restriction_parameter_"),(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-2.3#S0002"},"identifier")),"s defined in D.7[, which are checked prior to program execution]: "),(0,i.kt)("p",null,"Max_Task_Entries =",">"," 0,"),(0,i.kt)("p",null,"Max_Asynchronous_Select_Nesting =",">"," 0, and"),(0,i.kt)("p",null,"Max_Tasks =",">"," 0. "),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0020-1"),"}"," ","{",(0,i.kt)("em",null,"AI12-0340-1"),"}"," If a Max_Image_Length restriction applies to any compilation unit in the partition, then for any subtype S, S'Image, S'Wide_Image, and S'Wide_Wide_Image shall be implemented within that partition without any dynamic allocation."),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Implementation Note: "),"{",(0,i.kt)("em",null,"AI12-0340-1"),"}"," ","{",(0,i.kt)("em",null,"AI12-0384-2"),"}"," This can be accomplished by using an object of the Text_Buffers.Bounded.Buffer_Type with the maximum characters as specified in the Max_Image_Length restriction, with a raise of Program_Error afterward if Text_Truncated (Buf) is True after the call on Put_Image (Buf, Arg). "),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0263-1"),"}"," ","{",(0,i.kt)("em",null,"AI05-0272-1"),"}"," ","{",(0,i.kt)("em",null,"AI12-0308-1"),"}"," If an implementation supports ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-2.8#S0019"},"pragma"))," Restrictions for a particular argument, then except for the restrictions No_Access_Subprograms, No_Unchecked_Access, No_Specification_of_Aspect, No_Use_of_Attribute, No_Use_of_Pragma, No_Dependence =",">"," Ada.Unchecked_Conversion, and No_Dependence =",">"," Ada.Unchecked_Deallocation, the associated restriction applies to the run-time system. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Reason: "),"Permission is granted for the run-time system to use the specified otherwise-restricted features, since the use of these features may simplify the run-time system by allowing more of it to be written in Ada. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"The restrictions that are applied to the partition are also applied to the run-time system. For example, if No_Floating_Point is specified, then an implementation that uses floating point for implementing the delay statement (say) would require that No_Floating_Point is only used in conjunction with No_Delay. It is clearly important that restrictions are effective so that Max_Tasks=0 does imply that tasking is not used, even implicitly (for input-output, say)."),(0,i.kt)("p",null,"An implementation of tasking could be produced based upon a run-time system written in Ada in which the rendezvous was controlled by protected types. In this case, No_Protected_Types could only be used in conjunction with Max_Task_Entries=0. Other implementation dependencies could be envisaged."),(0,i.kt)("p",null,"If the run-time system is not written in Ada, then the wording needs to be applied in an appropriate fashion."),(0,i.kt)("h4",d({},{id:"documentation-requirements"}),"Documentation Requirements"),(0,i.kt)("p",null,"If a pragma Restrictions(No_Exceptions) is specified, the implementation shall document the effects of all constructs where language-defined checks are still performed automatically (for example, an overflow check performed by the processor). "),(0,i.kt)("p",null,(0,i.kt)("em",null,"This paragraph was deleted.")),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Documentation Requirement: "),"If a pragma Restrictions(No_Exceptions) is specified, the effects of all constructs where language-defined checks are still performed."),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"{",(0,i.kt)("em",null,"AI95-00114-01"),"}"," The documentation requirements here are quite difficult to satisfy. One method is to review the object code generated and determine the checks that are still present, either explicitly, or implicitly within the architecture. As another example from that of overflow, consider the question of dereferencing a null pointer. This could be undertaken by a memory access trap when checks are performed. When checks are suppressed via the argument No_Exceptions, it would not be necessary to have the memory access trap mechanism enabled."),(0,i.kt)("h4",d({},{id:"erroneous-execution"}),"Erroneous Execution"),(0,i.kt)("p",null,"Program execution is erroneous if pragma Restrictions(No_Exceptions) has been specified and the conditions arise under which a generated language-defined runtime check would fail. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"The situation here is very similar to the application of pragma Suppress. Since users are removing some of the protection the language provides, they had better be careful!"),(0,i.kt)("p",null,"Program execution is erroneous if pragma Restrictions(No_Recursion) has been specified and a subprogram is invoked as part of its own execution, or if pragma Restrictions(No_Reentrancy) has been specified and during the execution of a subprogram by a task, another task invokes the same subprogram. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"{",(0,i.kt)("em",null,"AI05-0005-1"),"}"," In practice, many implementations might not exploit the absence of recursion or need for reentrancy, in which case the program execution would be unaffected by the use of recursion or reentrancy, even though the program is still formally erroneous."),(0,i.kt)("p",null,(0,i.kt)("em",null,"This paragraph was deleted.")),(0,i.kt)("p",null,"NOTE   ","{",(0,i.kt)("em",null,"AI95-00394-01"),"}"," ","{",(0,i.kt)("em",null,"AI12-0440-1"),"}"," Uses of ",(0,i.kt)("em",null,"restriction_parameter_"),(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-2.3#S0002"},"identifier"))," No_Dependence defined in 13.12.1: No_Dependence =",">"," Ada.Unchecked_Deallocation and No_Dependence =",">"," Ada.Unchecked_Conversion can be appropriate for high-integrity systems. Other uses of No_Dependence can also be appropriate for high-integrity systems. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),'The specific mention of these two uses is meant to replace the identifiers now banished to J.13, "Dependence Restriction Identifiers".'),(0,i.kt)("p",null,"Restriction No_Dependence =",">"," Ada.Unchecked_Deallocation would be useful in those contexts in which heap storage is needed on program start-up, but need not be increased subsequently. The danger of a dangling pointer can therefore be avoided. "),(0,i.kt)("h4",d({},{id:"extensions-to-ada-95"}),"Extensions to Ada 95"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"8652/0042"),"}"," ","{",(0,i.kt)("em",null,"AI95-00130-01"),"}"," No_Local_Allocators no longer prohibits generic instantiations. "),(0,i.kt)("h4",d({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00285-01"),"}"," Wide_Wide_Text_IO (which is new) is added to the No_IO restriction."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00347-01"),"}"," ","{",(0,i.kt)("em",null,"AI05-0299-1"),"}"," The title of this subclause was changed to match the change to the Annex title. Pragma Profile(Ravenscar) is part of this annex."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00394-01"),"}"," Restriction No_Dependence is used instead of special ",(0,i.kt)("em",null,"restriction_"),(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-2.3#S0002"},"identifier")),"s. The old names are banished to Obsolescent Features (see J.13)."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI95-00394-01"),"}",' The bizarre wording "apply in this Annex" (which no one quite can explain the meaning of) is banished. '),(0,i.kt)("h4",d({},{id:"extensions-to-ada-2005"}),"Extensions to Ada 2005"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0152-1"),"}"," ","{",(0,i.kt)("em",null,"AI05-0190-1"),"}"," Restrictions No_Anonymous_Allocators, No_Coextensions, and No_Access_Parameter_Allocators are new. "),(0,i.kt)("h4",d({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0189-1"),"}"," New restriction No_Standard_Allocators_After_Elaboration is added to the list of restrictions that are required by this annex."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0263-1"),"}"," ",(0,i.kt)("strong",null,"Correction:")," Ada 2005 restriction No_Dependence is added where needed (this was missed in Ada 2005)."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0272-1"),"}"," Restrictions against individual aspects, pragmas, and attributes do not apply to the run-time system, in order that an implementation can use whatever aspects, pragmas, and attributes are needed to do the job. For instance, attempting to write a run-time system for Linux that does not use the Import aspect would be very difficult and probably is not what the user is trying to prevent anyway. "),(0,i.kt)("h4",d({},{id:"incompatibilities-with-ada-2012"}),"Incompatibilities With Ada 2012"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0318-1"),"}"," ",(0,i.kt)("strong",null,"Correction:")," Restriction No_IO now excludes use of Ada.Directories. If a program using No_IO used Ada.Directories, it would be legal in Ada 2012 and illegal in Ada 2022. However, given the role of Ada.Directories as a support package for the other packages that are excluded by No_IO, it seems unlikely that any use of the restriction would use this package (and it's possible that implementations wouldn't support its use with No_IO anyway). "),(0,i.kt)("h4",d({},{id:"extensions-to-ada-2012"}),"Extensions to Ada 2012"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0020-1"),"}"," Restriction Max_Image_Length is new."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0079-3"),"}"," Restrictions No_Unspecified_Globals and No_Hidden_Indirect_Globals are new. "),(0,i.kt)("h2",d({},{id:"h41--aspect-no_controlled_parts"}),"H.4.1  Aspect No_Controlled_Parts"),(0,i.kt)("h4",d({},{id:"static-semantics-1"}),"Static Semantics"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0256-1"),"}"," ","{",(0,i.kt)("em",null,"AI12-0403-1"),"}"," For a type, the following type-related, operational aspect may be specified:"),(0,i.kt)("p",null,"No_Controlled_PartsThe type of this aspect is Boolean. If True, the type and any descendants shall not have any controlled parts. If specified, the value of the expression shall be static. If not specified, the value of this aspect is False."),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Aspect Description for "),(0,i.kt)("strong",null,"No_Controlled_Parts: "),"A specification that a type and its descendants do not have controlled parts."),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0256-1"),"}"," The No_Controlled_Parts aspect is nonoverridable (see 13.1.1)."),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"{",(0,i.kt)("em",null,"AI12-0407-1"),"}"," Since this is a Boolean-valued aspect, the blanket restrictions defined by 13.1.1 apply to the specification of Boolean-valued aspects on descendants of types with such aspects. But we still need rules about inheritance from progenitors and about hiding the aspect; it would be too painful to repeat those rules here (and have future maintenance fixes not get applied to this aspect). "),(0,i.kt)("h4",d({},{id:"legality-rules"}),"Legality Rules"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0256-1"),"}"," ","{",(0,i.kt)("em",null,"AI12-0407-1"),"}"," If No_Controlled_Parts is True for a type, no component of the type shall have a controlled part nor shall the type itself be controlled. For the purposes of this rule, a type has a controlled part if its full type has a controlled part; this is applied recursively. In addition to the places where Legality Rules normally apply (see 12.3), this rule also applies in the private part of an instance of a generic unit."),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Discussion: "),"{",(0,i.kt)("em",null,"AI12-0407-1"),"}"," This check breaks privacy by looking at the full definition of all of the types involved. This is more like a representation aspect than an operational aspect, but representation aspects are not allowed on partial views and we need this aspect to be visible to clients. "),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0256-1"),"}"," ","{",(0,i.kt)("em",null,"AI12-0407-1"),"}"," When enforcing the above rule within a generic body ",(0,i.kt)("em",null,"G")," or within the body of a generic unit declared within the declarative region of generic unit ",(0,i.kt)("em",null,"G"),", a generic formal private type of ",(0,i.kt)("em",null,"G")," and a generic formal derived type of ",(0,i.kt)("em",null,"G")," whose ancestor is a tagged type whose No_Controlled_Parts aspect is False are considered to have a controlled part."),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Reason: "),"This is a typical generic assume-the-worst rule. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"To be honest: "),"{",(0,i.kt)("em",null,"AI12-0407-1"),"}"," If the ancestor of the generic derived type is class-wide, the aspect in question belongs to the specific type associated with the class-wide type. "),(0,i.kt)("h4",d({},{id:"extensions-to-ada-2012-1"}),"Extensions to Ada 2012"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI12-0256-1"),"}"," ","{",(0,i.kt)("em",null,"AI12-0407-1"),"}"," Aspect No_Controlled_Parts is new. "))}b.isMDXComponent=!0}}]);