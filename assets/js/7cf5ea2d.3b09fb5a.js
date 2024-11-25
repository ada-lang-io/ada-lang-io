"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[7398],{65848:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>l,contentTitle:()=>r,default:()=>h,frontMatter:()=>o,metadata:()=>i,toc:()=>d});const i=JSON.parse('{"id":"style-guide/s7/s7","title":"7. Portability","description":"Discussions concerning portability usually concentrate on the","source":"@site/docs/style-guide/s7/s7.mdx","sourceDirName":"style-guide/s7","slug":"/style-guide/s7/","permalink":"/docs/style-guide/s7/","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":7,"frontMatter":{"title":"7. Portability","sidebar_position":7},"sidebar":"styleGuideSidebar","previous":{"title":"6.4 Summary","permalink":"/docs/style-guide/s6/04"},"next":{"title":"7.1 Fundamentals","permalink":"/docs/style-guide/s7/01"}}');var a=t(74848),s=t(28453);const o={title:"7. Portability",sidebar_position:7},r=void 0,l={},d=[];function c(e){const n={br:"br",li:"li",p:"p",ul:"ul",...(0,s.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(n.p,{children:"Discussions concerning portability usually concentrate on the\ndifferences in computer systems, but the development and run-time\nenvironment may also change:"}),"\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsxs)(n.li,{children:["portability (software)",(0,a.jsx)(n.br,{}),"\n","The ease with which software can be transferred from one computer\nsystem or environment to another (IEEE Dictionary 1984)."]}),"\n"]}),"\n",(0,a.jsx)(n.p,{children:"Most portability problems are not pure language issues. Portability\ninvolves hardware (byte order, device I/O) and software (utility\nlibraries, operating systems, run-time libraries). This chapter will not\naddress these challenging design issues."}),"\n",(0,a.jsx)(n.p,{children:"This chapter does identify the more common portability problems that are\nspecific to Ada when moving from one platform or compiler to another. It\nalso suggests ways that nonportable code can be isolated. By using the\nimplementation hiding features of Ada, the cost of porting can be\nsignificantly reduced."}),"\n",(0,a.jsx)(n.p,{children:"In fact, many language portability issues are solved by the strict\ndefinition of the Ada language itself. In most programming languages,\ndifferent dialects are prevalent as vendors extend or dilute a language\nfor various reasons: conformance to a programming environment or\nfeatures for a particular application domain. The Ada Compiler\nValidation Capability (ACVC) was developed by the U.S. Department of\nDefense at the Ada Validation Facility, ASD/SIDL, Wright-Patterson Air\nForce Base, to ensure that implementors strictly adhered to the Ada\nstandard."}),"\n",(0,a.jsx)(n.p,{children:"As part of the strict definition of Ada, certain constructs are defined\nto be erroneous, and the effect of executing an erroneous construct is\nunpredictable. Therefore, erroneous constructs are obviously not\nportable. Erroneous constructs and bounded errors are discussed in\nGuideline 5.9.10 and are not repeated in this chapter."}),"\n",(0,a.jsx)(n.p,{children:"Most programmers new to the language expect Ada to eliminate all\nportability problems; it definitely does not. Certain areas of Ada are\nnot yet covered by validation. The definition of Ada leaves certain\ndetails to the implementor. The compiler implementor's choices, with\nrespect to these details, affect portability."}),"\n",(0,a.jsx)(n.p,{children:"The revisions to the Ada language approved in the 1995 standard generate\na new area of portability concerns. Some programs are intended to have a\nlong life and may start in Ada 83 (Ada Reference Manual 1983) but\ntransition to Ada 95 (Ada Reference Manual 1995). Although this style\nguide focuses on the current Ada standard and does not address\ntransition issues, there are portability issues relating to using\ncertain features of the language. These issues revolve around the\nlanguage features designated as obsolescent in Annex J of the Ada\nReference Manual (1995)."}),"\n",(0,a.jsx)(n.p,{children:"The constructs of the language have been developed to satisfy a series\nof needs. These constructs can legitimately be used even though they may\nimpact portability. There are some general principles to enhancing\nportability that are exemplified by many of the guidelines in this\nchapter. They are:"}),"\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsx)(n.li,{children:"Recognize those Ada constructs that may adversely affect portability\non the relevant implementations or platforms."}),"\n",(0,a.jsx)(n.li,{children:"Rely on those Ada constructs that depend on characteristics shared\nby all relevant implementations. Avoid the use of those constructs\nwhose implementation characteristics vary on the relevant platforms."}),"\n",(0,a.jsx)(n.li,{children:"Localize and encapsulate nonportable features of a program if their\nuse is essential."}),"\n",(0,a.jsx)(n.li,{children:"Highlight the use of constructs that may cause portability problems."}),"\n"]}),"\n",(0,a.jsx)(n.p,{children:"These guidelines cannot be applied thoughtlessly. Many of them involve a\ndetailed understanding of the Ada model and its implementation. In many\ncases, you will have to make carefully considered tradeoffs between\nefficiency and portability. Reading this chapter should improve your\ninsight into the tradeoffs involved. The material in this chapter was\nlargely acquired from three sources: the Ada Run-Time Environments\nWorking Group (ARTEWG) Catalogue of Ada Runtime Implementation\nDependencies (ARTEWG 1986); the Nissen and Wallis book on Portability\nand Style in Ada (Nissen and Wallis 1984); and a paper written for the\nU.S. Air Force by SofTech on Ada Portability Guidelines (Pappas 1985).\nThe last of these sources (Pappas 1985) encompasses the other two and\nprovides an in-depth explanation of the issues, numerous examples, and\ntechniques for minimizing portability problems. Conti (1987) is a\nvaluable reference for understanding the latitude allowed for\nimplementors of Ada and the criteria often used to make decisions."}),"\n",(0,a.jsx)(n.p,{children:"This chapter's purpose is to provide a summary of portability issues in\nthe guideline format of this book. The chapter does not include all\nissues identified in the references but only the most significant. For\nan in-depth presentation, see Pappas (1985). A few additional guidelines\nare presented here and others are elaborated upon where applicable. For\nfurther reading on Ada I/O portability issues, see Matthews (1987),\nGriest (1989), and CECOM (1989)."}),"\n",(0,a.jsx)(n.p,{children:"Some of the guidelines in this chapter cross reference and place\nstricter constraints on other guidelines in this book. These constraints\napply when portability is being emphasized."}),"\n",(0,a.jsx)(n.p,{children:'Guidelines in this chapter are frequently worded "consider . . ."\nbecause hard and fast rules cannot apply in all situations. The specific\nchoice you make in a given situation involves design tradeoffs. The\nrationale for these guidelines is intended to give you insight into some\nof these tradeoffs.'})]})}function h(e={}){const{wrapper:n}={...(0,s.R)(),...e.components};return n?(0,a.jsx)(n,{...e,children:(0,a.jsx)(c,{...e})}):c(e)}}}]);