"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[3204],{42009:(e,n,t)=>{t.d(n,{Z:()=>p});var a=t(76687),r=t(64923);const i="tabItem_aosM";var o=Object.defineProperty,l=Object.getOwnPropertySymbols,s=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,u=(e,n,t)=>n in e?o(e,n,{enumerable:!0,configurable:!0,writable:!0,value:t}):e[n]=t;function p({children:e,hidden:n,className:t}){return a.createElement("div",((e,n)=>{for(var t in n||(n={}))s.call(n,t)&&u(e,t,n[t]);if(l)for(var t of l(n))c.call(n,t)&&u(e,t,n[t]);return e})({role:"tabpanel",className:(0,r.Z)(i,t)},{hidden:n}),e)}},82770:(e,n,t)=>{t.d(n,{Z:()=>y});var a=t(76687),r=t(64923),i=t(66073),o=t(43958),l=t(36483),s=t(42323);const c="tabList_CDAa",u="tabItem_xrYE";var p=Object.defineProperty,d=Object.defineProperties,m=Object.getOwnPropertyDescriptors,b=Object.getOwnPropertySymbols,v=Object.prototype.hasOwnProperty,f=Object.prototype.propertyIsEnumerable,g=(e,n,t)=>n in e?p(e,n,{enumerable:!0,configurable:!0,writable:!0,value:t}):e[n]=t,h=(e,n)=>{for(var t in n||(n={}))v.call(n,t)&&g(e,t,n[t]);if(b)for(var t of b(n))f.call(n,t)&&g(e,t,n[t]);return e};function k(e){var n,t;const{lazy:i,block:p,defaultValue:b,values:v,groupId:f,className:g}=e,k=a.Children.map(e.children,(e=>{if((0,a.isValidElement)(e)&&"value"in e.props)return e;throw new Error(`Docusaurus error: Bad <Tabs> child <${"string"==typeof e.type?e.type:e.type.name}>: all children of the <Tabs> component should be <TabItem>, and every <TabItem> should have a unique "value" prop.`)})),y=null!=v?v:k.map((({props:{value:e,label:n,attributes:t}})=>({value:e,label:n,attributes:t}))),C=(0,o.l)(y,((e,n)=>e.value===n.value));if(C.length>0)throw new Error(`Docusaurus error: Duplicate values "${C.map((e=>e.value)).join(", ")}" found in <Tabs>. Every value needs to be unique.`);const w=null===b?b:null!=(t=null!=b?b:null==(n=k.find((e=>e.props.default)))?void 0:n.props.value)?t:k[0].props.value;if(null!==w&&!y.some((e=>e.value===w)))throw new Error(`Docusaurus error: The <Tabs> has a defaultValue "${w}" but none of its children has the corresponding value. Available values are: ${y.map((e=>e.value)).join(", ")}. If you intend to show no default tab, use defaultValue={null} instead.`);const{tabGroupChoices:I,setTabGroupChoices:O}=(0,l.U)(),[T,j]=(0,a.useState)(w),A=[],{blockElementScrollPositionUntilNextRender:_}=(0,s.o5)();if(null!=f){const e=I[f];null!=e&&e!==T&&y.some((n=>n.value===e))&&j(e)}const E=e=>{const n=e.currentTarget,t=A.indexOf(n),a=y[t].value;a!==T&&(_(n),j(a),null!=f&&O(f,String(a)))},N=e=>{var n,t;let a=null;switch(e.key){case"Enter":E(e);break;case"ArrowRight":{const t=A.indexOf(e.currentTarget)+1;a=null!=(n=A[t])?n:A[0];break}case"ArrowLeft":{const n=A.indexOf(e.currentTarget)-1;a=null!=(t=A[n])?t:A[A.length-1];break}}null==a||a.focus()};return a.createElement("div",{className:(0,r.Z)("tabs-container",c)},a.createElement("ul",{role:"tablist","aria-orientation":"horizontal",className:(0,r.Z)("tabs",{"tabs--block":p},g)},y.map((({value:e,label:n,attributes:t})=>{return a.createElement("li",(i=h({role:"tab",tabIndex:T===e?0:-1,"aria-selected":T===e,key:e,ref:e=>A.push(e),onKeyDown:N,onClick:E},t),o={className:(0,r.Z)("tabs__item",u,null==t?void 0:t.className,{"tabs__item--active":T===e})},d(i,m(o))),null!=n?n:e);var i,o}))),i?(0,a.cloneElement)(k.filter((e=>e.props.value===T))[0],{className:"margin-top--md"}):a.createElement("div",{className:"margin-top--md"},k.map(((e,n)=>(0,a.cloneElement)(e,{key:n,hidden:e.props.value!==T})))))}function y(e){const n=(0,i.Z)();return a.createElement(k,h({key:String(n)},e))}},52358:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>g,contentTitle:()=>v,default:()=>y,frontMatter:()=>b,metadata:()=>f,toc:()=>h});var a=t(91716),r=t(42009),i=t(82770),o=Object.defineProperty,l=Object.defineProperties,s=Object.getOwnPropertyDescriptors,c=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,p=Object.prototype.propertyIsEnumerable,d=(e,n,t)=>n in e?o(e,n,{enumerable:!0,configurable:!0,writable:!0,value:t}):e[n]=t,m=(e,n)=>{for(var t in n||(n={}))u.call(n,t)&&d(e,t,n[t]);if(c)for(var t of c(n))p.call(n,t)&&d(e,t,n[t]);return e};const b={sidebar_position:1},v="Advanced Techniques",f={unversionedId:"learn/tips/advanced-techniques",id:"learn/tips/advanced-techniques",title:"Advanced Techniques",description:"RAII",source:"@site/docs/learn/tips/advanced-techniques.mdx",sourceDirName:"learn/tips",slug:"/learn/tips/advanced-techniques",permalink:"/docs/learn/tips/advanced-techniques",draft:!1,tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1},sidebar:"tutorialSidebar",previous:{title:"Tips and Tricks",permalink:"/docs/category/tips-and-tricks"},next:{title:"Being More Terse",permalink:"/docs/learn/tips/being-more-terse"}},g={},h=[{value:"RAII",id:"raii",level:2},{value:"Timing out on a Blocking Operation",id:"timing-out-on-a-blocking-operation",level:2},{value:"Waiting for all tasks to complete",id:"waiting-for-all-tasks-to-complete",level:2}],k={toc:h};function y(e){var n,t=e,{components:o}=t,d=((e,n)=>{var t={};for(var a in e)u.call(e,a)&&n.indexOf(a)<0&&(t[a]=e[a]);if(null!=e&&c)for(var a of c(e))n.indexOf(a)<0&&p.call(e,a)&&(t[a]=e[a]);return t})(t,["components"]);return(0,a.kt)("wrapper",(n=m(m({},k),d),l(n,s({components:o,mdxType:"MDXLayout"}))),(0,a.kt)("h1",m({},{id:"advanced-techniques"}),"Advanced Techniques"),(0,a.kt)("h2",m({},{id:"raii"}),"RAII"),(0,a.kt)("p",null,"Ada supports scope-based resources, also called ",(0,a.kt)("a",m({parentName:"p"},{href:"https://en.wikipedia.org/wiki/Resource_acquisition_is_initialization"}),"RAII"),"\nby extending the ",(0,a.kt)("inlineCode",{parentName:"p"},"Controlled")," or ",(0,a.kt)("inlineCode",{parentName:"p"},"LimitedControlled")," types."),(0,a.kt)(i.Z,{mdxType:"Tabs"},(0,a.kt)(r.Z,{value:"ada",label:"Ada",mdxType:"TabItem"},(0,a.kt)("pre",null,(0,a.kt)("code",m({parentName:"pre"},{className:"language-ada"}),"with Ada.Finalization;  use Ada.Finalization;\npackage Sample is\n    -- \"Controlled\" types exhibit RAII behavior:\n    type Capricorn is new Controlled with\n    record\n        Dummy : Integer;\n    end record;\n\n    overriding procedure Initialize(C : in out Capricorn);\n        -- Initialization after creation.\n\n    overriding procedure Adjust(C : in out Capricorn);\n        -- Adjustment after assignment.\n\n    overriding procedure Finalize(C : in out Capricorn);\n        -- Different than Java's Finalize, in that it's deterministic and more\n        -- analogous to a C++ destructor.\n\n    -- If you don't want one of these do to anything, you can avoid writing a\n    -- definition in the package body and define the function as \"do nothing\"\n    -- by writing:\n    --\n    -- overriding procedure Finalize(C : in out Capricorn) is null;\nend Sample;\n\npackage body Sample is\n    procedure Initialize(C : in out Capricorn) is\n    begin\n        -- Do something on initialize.\n    end Initialize;\n\n    procedure Adjust(C : in out Capricorn) is\n    begin\n        -- Adjustment after assignment.\n        --\n        -- If you want Adjust to do the same as Initialize and use the same object\n        -- code without generating a separate function, you can just do\n        -- procedure Adjust(C: in out Capricorn) renames Initialize;\n    end Adjust;\n\n    overriding procedure Finalize(C : in out Capricorn);\n        -- Different than Java's Finalize, in that it's deterministic and more\n        -- analogous to a C++ destructor.\nend Sample;\n"))),(0,a.kt)(r.Z,{value:"cpp",label:"C++",mdxType:"TabItem"},(0,a.kt)("pre",null,(0,a.kt)("code",m({parentName:"pre"},{className:"language-cpp"}),"class Capricorn {\npublic:\n// Similar for all constructors.\nCapricorn () {}\n\n// Copy constructor.\nCapricorn(const Capricorn&) {}\n\n// Move constructor.\nCapricorn(Capricorn&&) {}\n\n// Copy assignment.\nCapricorn& operator=(const Capricorn&) { return *this; }\n\n// Move assignment.\nCapricorn& operator=(Capricorn&&) { return *this; }\n\n// Destructor.\n~Capricorn () {}\n};\n")))),(0,a.kt)("h2",m({},{id:"timing-out-on-a-blocking-operation"}),"Timing out on a Blocking Operation"),(0,a.kt)("pre",null,(0,a.kt)("code",m({parentName:"pre"},{className:"language-ada"}),"task body My_Task is\n    Elem : A_Queue_Element;\nbegin\n    loop -- processing loop\n        select\n            A_Queue.Blocking_Queue (Elem);\n        or\n            -- Stop processing after a 1 second timeout. Removing this delay causes\n            -- immediate exit if a block occurs.\n            delay 1.0;\n            exit;\n        end select;\n\n        -- ... process Elem ...\n    end loop\nend My_Task;\n")),(0,a.kt)("h2",m({},{id:"waiting-for-all-tasks-to-complete"}),"Waiting for all tasks to complete"),(0,a.kt)("p",null,"A list of statements doesn't exit until all tasks are complete, so by using\n",(0,a.kt)("inlineCode",{parentName:"p"},"declare ... begin ... end")," you can wait until all your tasks are done."),(0,a.kt)("pre",null,(0,a.kt)("code",m({parentName:"pre"},{className:"language-ada"}),"declare\n    A_Task : My_Task;  -- task which needs to finish before more processing\nbegin\n    null; -- Just wait until the task is done.\nend;\n\n-- Continue other operations here.\n")))}y.isMDXComponent=!0}}]);