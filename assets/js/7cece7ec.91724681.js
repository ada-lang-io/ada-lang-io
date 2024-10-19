"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[137],{37927:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>o,contentTitle:()=>s,default:()=>h,frontMatter:()=>l,metadata:()=>r,toc:()=>c});var a=t(74848),i=t(28453);const l={sidebar_position:1},s="Installation",r={id:"learn/getting-started/installation",title:"Installation",description:"An Ada development environment consists basically of a toolchain: an Ada compiler and a build manager. In the case of GNAT, the basic build manager gnatmake comes with the compiler, while a more capable one is the separate GNAT Project Manager gprbuild.",source:"@site/docs/learn/getting-started/installation.md",sourceDirName:"learn/getting-started",slug:"/learn/getting-started/installation",permalink:"/docs/learn/getting-started/installation",draft:!1,unlisted:!1,tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1},sidebar:"tutorialSidebar",previous:{title:"Getting started",permalink:"/docs/category/getting-started"},next:{title:"Editors",permalink:"/docs/learn/getting-started/editors"}},o={},c=[{value:"Alire",id:"alire",level:2},{value:"Toolchain",id:"toolchain",level:2}];function d(e){const n={a:"a",code:"code",h1:"h1",h2:"h2",li:"li",p:"p",pre:"pre",ul:"ul",...(0,i.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(n.h1,{id:"installation",children:"Installation"}),"\n",(0,a.jsxs)(n.p,{children:["An Ada development environment consists basically of a toolchain: an Ada compiler and a build manager. In the case of GNAT, the basic build manager ",(0,a.jsx)(n.a,{href:"https://docs.adacore.com/live/wave/gnat_ugn/html/gnat_ugn/gnat_ugn/building_executable_programs_with_gnat.html#building-with-gnatmake",children:(0,a.jsx)(n.code,{children:"gnatmake"})})," comes with the compiler, while a more capable one is the separate GNAT Project Manager ",(0,a.jsx)(n.a,{href:"https://docs.adacore.com/live/wave/gprbuild/html/gprbuild_ug/gprbuild_ug.html",children:(0,a.jsx)(n.code,{children:"gprbuild"})}),"."]}),"\n",(0,a.jsxs)(n.p,{children:["By far the easiest way to get hold of an Ada toolchain is to use the Ada package manager ",(0,a.jsx)(n.a,{href:"https://alire.ada.dev/docs/#introduction",children:"Alire"}),"."]}),"\n",(0,a.jsx)(n.h2,{id:"alire",children:"Alire"}),"\n",(0,a.jsxs)(n.p,{children:["The Alire website's ",(0,a.jsx)(n.a,{href:"https://github.com/alire-project/alire/releases",children:"Releases page"})," provides builds:"]}),"\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsxs)(n.li,{children:["the current stable build, ",(0,a.jsx)(n.a,{href:"https://github.com/alire-project/alire/releases/tag/v2.0.2",children:"v2.0.2"}),","]}),"\n",(0,a.jsxs)(n.li,{children:["a ",(0,a.jsx)(n.a,{href:"https://github.com/alire-project/alire/releases/tag/nightly",children:"nightly build"}),"."]}),"\n"]}),"\n",(0,a.jsxs)(n.p,{children:["Any of these can be installed as described ",(0,a.jsx)(n.a,{href:"https://alire.ada.dev/docs/#installation",children:"here"}),"; follow up with these ",(0,a.jsx)(n.a,{href:"https://alire.ada.dev/docs/#first-steps",children:"first steps"})," (this will have the added effect of installing a toolchain for you!)"]}),"\n",(0,a.jsx)(n.h2,{id:"toolchain",children:"Toolchain"}),"\n",(0,a.jsx)(n.p,{children:"Once you have Alire installed, you have a choice of toolchains. You'll normally want to use the latest native toolchain (the one that generates code to run on the machine you're developing on)."}),"\n",(0,a.jsxs)(n.p,{children:[(0,a.jsx)(n.code,{children:"alr toolchain --select"})," will present you with a list of choices, something like this:"]}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-none",children:"  1. gnat_native=13.2.1\n  2. None\n  3. gnat_arm_elf=13.2.1\n  4. gnat_avr_elf=13.2.1\n  5. gnat_riscv64_elf=13.2.1\n  6. gnat_arm_elf=13.1.0\n  7. gnat_avr_elf=13.1.0\n  8. gnat_native=13.1.0\n  9. gnat_riscv64_elf=13.1.0\n  0. gnat_arm_elf=12.2.1\n  a. (See more choices...)\nEnter your choice index (first is default):\n>\n"})}),"\n",(0,a.jsxs)(n.p,{children:["Press ",(0,a.jsx)(n.code,{children:"<return>"})," to choose the latest compiler. The selection tool then looks for a matching ",(0,a.jsx)(n.code,{children:"gprbuild"}),":"]}),"\n",(0,a.jsx)(n.pre,{children:(0,a.jsx)(n.code,{className:"language-none",children:"Please select the gprbuild version for use with this configuration\n  1. gprbuild=22.0.1\n  2. None\n  3. gprbuild=21.0.2\n  4. gprbuild=21.0.1\nEnter your choice index (first is default):\n>\n"})}),"\n",(0,a.jsxs)(n.p,{children:["Again, press ",(0,a.jsx)(n.code,{children:"<return>"}),". If necessary, Alire will download the selected tools and install them in the Alire environment."]})]})}function h(e={}){const{wrapper:n}={...(0,i.R)(),...e.components};return n?(0,a.jsx)(n,{...e,children:(0,a.jsx)(d,{...e})}):d(e)}}}]);