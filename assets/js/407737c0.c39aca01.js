"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8438],{30917:(e,r,s)=>{s.r(r),s.d(r,{assets:()=>o,contentTitle:()=>n,default:()=>h,frontMatter:()=>t,metadata:()=>l,toc:()=>c});var a=s(74848),i=s(28453);const t={sidebar_position:24},n="Alire vs Apple silicon",l={id:"learn/getting-started/macos-issues/alire-vs-aarch64",title:"Alire vs Apple silicon",description:"Alire releases available from the Alire website are available for either Intel or Apple silicon, but at the time of writing the download links on the landing page refer only to the Intel version.",source:"@site/docs/learn/getting-started/macos-issues/alire-vs-aarch64.md",sourceDirName:"learn/getting-started/macos-issues",slug:"/learn/getting-started/macos-issues/alire-vs-aarch64",permalink:"/docs/learn/getting-started/macos-issues/alire-vs-aarch64",draft:!1,unlisted:!1,tags:[],version:"current",sidebarPosition:24,frontMatter:{sidebar_position:24},sidebar:"tutorialSidebar",previous:{title:"Alire",permalink:"/docs/learn/getting-started/macos-issues/alire"},next:{title:"Crates for macOS",permalink:"/docs/learn/getting-started/macos-issues/crates"}},o={},c=[];function d(e){const r={a:"a",code:"code",h1:"h1",p:"p",...(0,i.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(r.h1,{id:"alire-vs-apple-silicon",children:"Alire vs Apple silicon"}),"\n",(0,a.jsxs)(r.p,{children:["Alire releases available from the ",(0,a.jsx)(r.a,{href:"https://github.com/alire-project/alire/releases",children:"Alire website"})," are available for either Intel or Apple silicon, but at the time of writing the download links on the landing page refer only to the Intel version."]}),"\n",(0,a.jsx)(r.p,{children:"For pure Ada work, this has no effect, whether or not you're working on Apple silicon."}),"\n",(0,a.jsxs)(r.p,{children:["One area where there's a considerable impact is when your work involves \"",(0,a.jsx)(r.a,{href:"https://alire.ada.dev/docs/#external-releases",children:"external releases"}),'". These are external libraries which Alire manages as required using your system\'s "package manager". An example is the crate ',(0,a.jsx)(r.code,{children:"sdlada"}),", which depends on ",(0,a.jsx)(r.code,{children:"libsdl2"})," amongst others. On a Debian system, Alire will load the package ",(0,a.jsx)(r.code,{children:"libsdl2-dev"}),"; on macOS with Homebrew, ",(0,a.jsx)(r.code,{children:"sdl2"}),"."]}),"\n",(0,a.jsxs)(r.p,{children:["For macOS, the package managers supported are ",(0,a.jsx)(r.a,{href:"https://brew.sh",children:"Homebrew"})," and ",(0,a.jsx)(r.a,{href:"https://www.macports.org",children:"MacPorts"})," - if you have both installed (not really recommended) Alire will choose Homebrew. Homebrew is the one that this page concentrates on."]}),"\n",(0,a.jsxs)(r.p,{children:["If yours is an ",(0,a.jsx)(r.code,{children:"x86_64"})," Mac, Homebrew will load ",(0,a.jsx)(r.code,{children:"x86_64"})," binaries under ",(0,a.jsx)(r.code,{children:"/usr/local/"}),". If it's an ",(0,a.jsx)(r.code,{children:"aarch64"})," Mac, Homebrew will load ",(0,a.jsx)(r.code,{children:"aarch64"})," binaries under ",(0,a.jsx)(r.code,{children:"/opt/homebrew/"})," (this is by default; trying to mix architectures is likely to be at best confusing)."]}),"\n",(0,a.jsxs)(r.p,{children:["So, if you're on an ",(0,a.jsx)(r.code,{children:"aarch64"})," Mac with an ",(0,a.jsx)(r.code,{children:"x86_64"})," GNAT compiler, you won't be able to use external libraries, because the linker will refuse to link your ",(0,a.jsx)(r.code,{children:"x86_64"})," binaries with Homebrew's ",(0,a.jsx)(r.code,{children:"aarch64"})," ones."]})]})}function h(e={}){const{wrapper:r}={...(0,i.R)(),...e.components};return r?(0,a.jsx)(r,{...e,children:(0,a.jsx)(d,{...e})}):d(e)}}}]);