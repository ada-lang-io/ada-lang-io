"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[4195],{118:(e,t,a)=>{a.r(t),a.d(t,{HomepageHeader:()=>oe,alireVersion:()=>ae,default:()=>ce,getInstallTarget:()=>re,gitHubProjectPage:()=>ne,gitHubReleasePage:()=>le,installTargets:()=>te});var n=a(6687),l=a(860),r=a(1651),i=a(6198),o=a(1591),c=a(2105),s=a(2322),m=a(2500),d=a(7567),u=a(7411),p=a(811),g=a(9133),b=a(1990),E=a(1873),f=a(9075),h=a(608),y=a(584),v=a(8310),w=a(2981),k=a(9450),x=a(3820),_=a(7754);const S="timelineItemTitle_t46M",N="timeline_W6yk",I=new Map([["macos",{download:n.createElement(n.Fragment,null,n.createElement(u.x,{color:"dimmed"},"On macOS, remove the quarantine attribute to avoid getting a message suggesting to move the file to the bin because the developer cannot be verified:"),n.createElement(h.p,{language:"shell"},"xattr -d com.apple.quarantine bin/alr"))}]]);function P({children:e}){return n.createElement(u.x,{size:"sm",className:S},e)}function A(e){const t=(0,n.useCallback)((t=>{const a=e=>"button"===e.type&&null!==e.onclick;(a(t.target)||a(t.target.parentNode)||a(t.target.parentNode.parentNode))&&e()}),[e]);return(0,_.O)("click",t)}function T(){const e=(0,o.Z)(),t=(0,f.i)(),a=e&&te.has(t)?t:null,i=null!==a?te.get(a):null,c=null!==i?` for ${i.label}`:"",s=null!==i?re(ae,i.urlSuffix):le,[m,d]=(0,n.useState)(-1),p=(0,n.useCallback)((()=>{d(0)}),[d]),g=(0,n.useCallback)((()=>{d(1)}),[d]),b=(0,n.useCallback)((()=>{d(2)}),[d]),E=(0,n.useCallback)((()=>{d(3)}),[d]),v=A(g),w=A(g),_=A(b),S=A(E),T=n.createElement("span",null," ","or view other options on the"," ",n.createElement(r.Z,{onClick:p,to:le},"release page"));return n.createElement(k.T,{active:m,bulletSize:32,lineWidth:3,className:N},n.createElement(k.T.Item,{bullet:n.createElement(y.tEr,{size:16}),title:n.createElement(P,null,"Download Alire")},n.createElement(x.K,{spacing:"sm"},n.createElement(u.x,{color:"dimmed"},"Download"," ",n.createElement(r.Z,{onClick:p,to:s},"Alire ",ae.slice(0),c),null!==i&&T,"."),I.has(a)&&I.get(a).download)),n.createElement(k.T.Item,{bullet:n.createElement(l.fF,{size:12}),title:n.createElement(P,null,"Install toolchain")},n.createElement(x.K,{spacing:"sm"},n.createElement(h.p,{ref:v,language:"shell"},"alr toolchain --select"),n.createElement(u.x,{color:"dimmed"},"Select gnat_native and gprbuild."))),n.createElement(k.T.Item,{bullet:n.createElement(y.xoN,{size:16}),title:n.createElement(P,null,"Start coding")},n.createElement(x.K,{spacing:"sm"},n.createElement(u.x,{color:"dimmed"},"Create a crate:"),n.createElement(h.p,{ref:w,language:"shell"},"alr init --bin mycrate && cd mycrate"),n.createElement(u.x,{color:"dimmed"},"Build the crate:"),n.createElement(h.p,{ref:_,language:"shell"},"alr build"))),n.createElement(k.T.Item,{bullet:n.createElement(y.vB0,{size:16}),title:n.createElement(P,null,"Run your application")},n.createElement(x.K,{spacing:"sm"},n.createElement(h.p,{ref:S,language:"shell"},"alr run"))))}var z=a(4923);const D="features_mewZ",C="sectionWrapper_at2H",O="spark_PAbP",R="description_CMvZ",Z="itemWrapper_ESv2",W="itemDescription_fq1O",G="title_osQQ",K="subTitle_DsT5",M="itemIcon_hbuH",L="itemTitle_Aqew",U=JSON.parse('[{"title":"Ada","subTitle":"Readable, correct, performant","description":"Express intent with explicitness, describe properties with predicates and pre/post conditions, and import C/C++ functions or intrinsics.","columns":3,"items":[{"title":"Readable","description":["Express intent with explicitness and keywords over symbols and special structures.","Express concepts like meaning in integers. Use built-in design by contract with pre/post-conditions and invariants. Model problems with typechecks and range constraints."],"icon":"feat-readable"},{"title":"Correct","description":["Build with technology used in 40 years of reliability in planes, trains, and satellites.","Use the SPARK subset to formally verify part or all of your program, and integrate existing SPARK crates available in the Alire package manager."],"icon":"feat-correct"},{"title":"Performant","description":["Build native applications and take advantage of other libraries through binding to C and C++.","Use inline assembly or compiler intrinsics when you need it. Control resources with scope-based resource control (RAII) and your own memory allocators."],"icon":"feat-performant"}]},{"title":"Set-up environment","subTitle":"Package manager + toolchain","description":"Download the Alire package manager and install the compiler.","columns":1},{"title":"SPARK","subTitle":"From memory safety to functional correctness","description":"Gradually adopt the SPARK subset to reach various levels of assurance. Higher levels take more effort, but give more benefits and stronger guarantees.","items":[{"title":"Valid SPARK","description":"Restrict Ada packages to the SPARK subset. Avoids side-effects in functions and parameter aliasing.","icon":"spark-stone"},{"title":"Initialization and correct data flow","description":"No uninitialized variables are read or undesired access to globals occurs.","icon":"spark-bronze"},{"title":"Absence of run-time errors","description":"No buffer and arithmetic overflow, division by zero, or values out of range, among others, can occur.","icon":"spark-silver"},{"title":"Key integrity properties","description":"Verify integrity of data and valid state transitions.","icon":"spark-gold"}]}]');var F=Object.defineProperty,B=Object.getOwnPropertySymbols,H=Object.prototype.hasOwnProperty,$=Object.prototype.propertyIsEnumerable,j=(e,t,a)=>t in e?F(e,t,{enumerable:!0,configurable:!0,writable:!0,value:a}):e[t]=a,q=(e,t)=>{for(var a in t||(t={}))H.call(t,a)&&j(e,a,t[a]);if(B)for(var a of B(t))$.call(t,a)&&j(e,a,t[a]);return e};U[1].children=n.createElement(T,null),U[2].className=O;const V={"feat-readable":n.createElement(y.b2B,null),"feat-correct":n.createElement(y.vr,null),"feat-performant":n.createElement(y.JKp,null),"spark-stone":n.createElement(y.rom,null),"spark-bronze":n.createElement(y.XRi,null),"spark-silver":n.createElement(y.Il9,null),"spark-gold":n.createElement(y.ikh,null),"spark-platinum":n.createElement(y.$FZ,null)};function X({title:e,description:t,icon:a}){return n.createElement("div",{className:Z},n.createElement(v.k,{variant:"light",className:M,size:60,radius:"md"},V[a]),n.createElement("div",null,n.createElement(u.x,{weight:700,size:"lg",className:L},e),n.createElement(u.x,{className:W,color:"dimmed"},Array.isArray(t)?t.map(((e,t)=>n.createElement("p",{key:t},e))):t)))}function J({title:e,subTitle:t,description:a,items:l,className:r,children:i,columns:o=2}){return n.createElement("section",{className:(0,z.Z)(C,{[r]:!!r})},n.createElement(d.W,{size:700},n.createElement(u.x,{className:G},e),n.createElement(m.D,{className:K,order:2},t),n.createElement(d.W,{size:660,p:0},n.createElement(u.x,{color:"dimmed",className:R},a)),n.createElement(w.M,{cols:Number(!!l)+Number(!!i),spacing:"md"},!!l&&n.createElement(w.M,{cols:o,spacing:48,breakpoints:[{maxWidth:475,cols:1,spacing:16},{maxWidth:768,cols:2,spacing:24},{maxWidth:996,cols:Math.min(3,o),spacing:32}]},l.map(((e,t)=>n.createElement(X,q({key:t},e))))),i)))}function Q(){return n.createElement("div",{className:D},U.map(((e,t)=>n.createElement(J,q({key:t},e)))))}const Y={heroWrapper:"heroWrapper_Lp5O",heroTabs:"heroTabs_oUBJ",heroInner:"heroInner_egXW",title:"title_spfF",description:"description_UIrY",textDownloadLinks:"textDownloadLinks_vHOi",controls:"controls_oqqj",downloadIcon:"downloadIcon_mksw",controlSecondary:"controlSecondary_pt7O",columns:"columns_oYBZ",codeTabPanel:"codeTabPanel_c50c"},ee=[{key:"ada",code:"with Ada.Text_IO;\n\nprocedure Main is\n   type GUID is new String (1 .. 32)\n     with Dynamic_Predicate =>\n       (for all C of GUID => C in '0' .. '9' | 'a' .. 'f');\n\n   ID_1 : constant GUID := \"030000004c050000cc09000011810000\";\nbegin\n   Ada.Text_IO.Put_Line (\"Reading from device \" & String (ID_1) & \"...\");\nend Main;\n"},{key:"spark",code:"with Interfaces;\n\npackage Xoshiro128 with Pure, SPARK_Mode => On is\n   use type Interfaces.Unsigned_64;\n\n   type Generator is limited private;\n\n   procedure Next (S : in out Generator; Value : out Interfaces.Unsigned_32)\n     with Global  => null,\n          Depends => (S => S, Value => S);\n\n   procedure Reset (S : out Generator; Seed : Interfaces.Unsigned_64)\n     with Global  => null,\n          Depends => (S => Seed),\n          Pre     => Seed /= 0;\n\nprivate\n   type Generator is array (0 .. 3) of Interfaces.Unsigned_32;\nend Xoshiro128;\n"},{key:"embedded",code:"with RP.GPIO;\nwith Pico;\n\nprocedure Main is\nbegin\n   Pico.LED.Configure (RP.GPIO.Output);\n   loop\n      Pico.LED.Toggle;\n      delay 0.1;\n   end loop;\nend Main;\n"}],te=new Map([["windows",{label:"Windows",urlSuffix:"installer-x86_64-windows.exe"}],["macos",{label:"macOS",urlSuffix:"bin-x86_64-macos.zip"}],["linux",{label:"Linux",urlSuffix:"bin-x86_64-linux.zip"}],["appimage",{label:"AppImage",urlSuffix:"x86_64.AppImage"}]]),ae="1.2.1",ne="https://github.com/alire-project/alire",le=`${ne}/releases`;function re(e,t){return`${ne}/releases/download/v${e}/alr-${e}-${t}`}function ie({showLineNumbers:e,children:t}){return n.createElement(h.p,{withLineNumbers:e,language:"ada"},t)}function oe({title:e,description:t}){const a=(0,o.Z)(),i=(0,f.i)(),c=a&&te.has(i)?i:null,E=null!==c?te.get(c):null,h=null!==E?` for ${E.label}`:"",y=null!==E?re(ae,E.urlSuffix):le,v=Array.from(te.values()).map((({label:e,urlSuffix:t},a)=>n.createElement(r.Z,{key:a,to:re(ae,t)},e))),w=(k=", ",v.reduce(((e,t)=>e.length>0?e.concat([k,t]):[t]),[]));var k;const x=n.createElement(r.Z,{to:le},"others");return n.createElement("header",{className:Y.heroWrapper},n.createElement(s.a,{color:"#000",opacity:.65,zIndex:1}),n.createElement("div",{className:Y.heroInner},n.createElement("div",{className:Y.columns},n.createElement("div",null,n.createElement(m.D,{className:Y.title},e),n.createElement(d.W,{size:640},n.createElement(u.x,{size:"lg",className:Y.description},t),n.createElement(u.x,{size:"lg",className:Y.description},"Get started with Alire, the Ada package manager.")),n.createElement(p.Z,{position:"center",className:Y.controls},n.createElement(g.z,{className:Y.controlPrimary,size:"md",component:r.Z,to:y,variant:"gradient",gradient:{from:"blue",to:"cyan"},leftIcon:n.createElement(l.aBF,{className:Y.downloadIcon})},"Download Alire ",ae.slice(0),h),n.createElement(g.z,{className:Y.controlSecondary,size:"md",component:r.Z,to:"/docs/getting-started/why-ada"},"Get Started")),n.createElement(u.x,{size:"xs",className:Y.textDownloadLinks},"Download for"," ",w.map(((e,t)=>n.createElement("span",{key:t},e))),", or ",x)),n.createElement("div",null,n.createElement("div",{className:Y.heroTabs},n.createElement(b.m,{color:"blue",variant:"pills",defaultValue:"ada"},n.createElement(b.m.List,null,n.createElement(b.m.Tab,{value:"ada"},"Ada"),n.createElement(b.m.Tab,{value:"spark"},"SPARK"),n.createElement(b.m.Tab,{value:"embedded"},"Embedded")),n.createElement("div",{className:Y.codeTabPanel},ee.map((({key:e,code:t})=>n.createElement(b.m.Panel,{key:e,value:e,pt:"xs"},n.createElement(ie,{showLineNumbers:!0},t)))))))))))}function ce(){const{siteConfig:e}=(0,i.Z)();return n.createElement(E.Me,{theme:{colorScheme:"dark",fontFamily:"var(--ada-lang-font-family)"}},n.createElement(c.Z,{title:e.title,description:e.customFields.description},n.createElement(oe,{title:e.title,description:e.customFields.description}),n.createElement("main",null,n.createElement(Q,null))))}}}]);