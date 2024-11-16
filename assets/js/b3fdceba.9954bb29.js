"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[227],{17920:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>d,contentTitle:()=>o,default:()=>c,frontMatter:()=>t,metadata:()=>s,toc:()=>a});var l=i(74848),r=i(28453);const t={sidebar_position:2},o="Hello, World!",s={id:"learn/tutorial/hello-world",title:"Hello, World!",description:"Welcome to Ada!",source:"@site/docs/learn/tutorial/01-hello-world.mdx",sourceDirName:"learn/tutorial",slug:"/learn/tutorial/hello-world",permalink:"/docs/learn/tutorial/hello-world",draft:!1,unlisted:!1,tags:[],version:"current",sidebarPosition:2,frontMatter:{sidebar_position:2},sidebar:"tutorialSidebar",previous:{title:"Tutorial",permalink:"/docs/category/tutorial"},next:{title:"Command Line Arguments",permalink:"/docs/learn/tutorial/command-line-arguments"}},d={},a=[{value:"Requirements",id:"requirements",level:2},{value:"Objectives",id:"objectives",level:2},{value:"Starting a new project",id:"starting-a-new-project",level:2},{value:"Building a project",id:"building-a-project",level:2},{value:"It&#39;s just an empty program...",id:"its-just-an-empty-program",level:2},{value:"Making it do something",id:"making-it-do-something",level:2},{value:"Need help?",id:"need-help",level:2}];function h(e){const n={a:"a",admonition:"admonition",code:"code",h1:"h1",h2:"h2",li:"li",ol:"ol",p:"p",pre:"pre",strong:"strong",...(0,r.R)(),...e.components};return(0,l.jsxs)(l.Fragment,{children:[(0,l.jsx)(n.h1,{id:"hello-world",children:"Hello, World!"}),"\n",(0,l.jsx)(n.p,{children:"Welcome to Ada!"}),"\n",(0,l.jsxs)(n.p,{children:["Traditionally, the first program written in a new language is ",(0,l.jsx)(n.a,{href:"https://en.wikipedia.org/wiki/%22Hello,_World!%22_program",children:'"Hello, World."'}),"\nThis is a simple program just to print a greeting, and help ensure your environment is\nset up correctly."]}),"\n",(0,l.jsx)(n.h2,{id:"requirements",children:"Requirements"}),"\n",(0,l.jsxs)(n.ol,{children:["\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:[(0,l.jsx)(n.strong,{children:(0,l.jsx)(n.a,{href:"https://alire.ada.dev/",children:"Alire"})})," - This program provide simple commands to build and\nrun your code, and will install the programs behind this process. The\ndetailed controls are still in the background if you need them, but it helps\nstreamline the process for developers. You will often find just the main\nAlire interface will work well enough for many projects."]}),"\n",(0,l.jsxs)(n.p,{children:["If you haven't set it up, you can follow the\n",(0,l.jsx)(n.a,{href:"https://alire.ada.dev/docs/#installation",children:"instructions on the Alire site"}),"."]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["A text editor such as ",(0,l.jsx)(n.a,{href:"https://code.visualstudio.com/",children:"Visual Studio Code"})," or ",(0,l.jsx)(n.a,{href:"https://notepad-plus-plus.org/",children:"Notepad++"}),"."]}),"\n"]}),"\n"]}),"\n",(0,l.jsx)(n.h2,{id:"objectives",children:"Objectives"}),"\n",(0,l.jsxs)(n.ol,{children:["\n",(0,l.jsx)(n.li,{children:"Make your first running Ada program!"}),"\n",(0,l.jsx)(n.li,{children:"Learn how to use standard Ada libraries."}),"\n"]}),"\n",(0,l.jsx)(n.h2,{id:"starting-a-new-project",children:"Starting a new project"}),"\n",(0,l.jsxs)(n.p,{children:[(0,l.jsx)(n.code,{children:"Alire"})," runs from a command-line terminal. This keeps it slim and allows it\nto be easily usable, such as when making automation to build and test large\nprojects."]}),"\n",(0,l.jsx)(n.admonition,{type:"note",children:(0,l.jsxs)(n.p,{children:["Commands to type in the terminal will follow a ",(0,l.jsx)(n.code,{children:"$"}),", don't type the ",(0,l.jsx)(n.code,{children:"$"}),", that's\njust there to indicate this is a command. Commands in tutorials might also have\nanother block that will show you their output."]})}),"\n",(0,l.jsxs)(n.p,{children:[(0,l.jsx)(n.code,{children:"Alire"})," will generate you a new project. You want to start, or initialize (init),\na new project which runs an executable, which is also called a binary (bin)."]}),"\n",(0,l.jsx)(n.pre,{children:(0,l.jsx)(n.code,{className:"language-bash",children:"$ alr init --bin my_hello_world\n"})}),"\n",(0,l.jsx)(n.p,{children:"You should respond with something like this:"}),"\n",(0,l.jsx)(n.pre,{children:(0,l.jsx)(n.code,{className:"language-text",children:"Success: my_hello_world initialized successfully.\n"})}),"\n",(0,l.jsxs)(n.p,{children:["This made a new directory for your project, ",(0,l.jsx)(n.code,{children:"my_hello_world/"})," with some contents:"]}),"\n",(0,l.jsx)(n.pre,{children:(0,l.jsx)(n.code,{className:"language-text",children:"my_hello_world/\n\u251c\u2500\u2500 alire.toml\n\u251c\u2500\u2500 my_hello_world.gpr\n\u251c\u2500\u2500 share\n\u2502   \u2514\u2500\u2500 my_hello_world\n\u2514\u2500\u2500 src\n\u2514\u2500\u2500 my_hello_world.adb\n"})}),"\n",(0,l.jsx)(n.p,{children:"There's a few files and a few directories that it made."}),"\n",(0,l.jsxs)("table",{children:[(0,l.jsxs)("tr",{children:[(0,l.jsx)("td",{children:(0,l.jsx)("code",{children:"alire.toml"})}),(0,l.jsx)("td",{children:"Describes what is in your project."})]}),(0,l.jsxs)("tr",{children:[(0,l.jsx)("td",{children:(0,l.jsx)("code",{children:"my_hello_world.gpr"})}),(0,l.jsx)("td",{children:(0,l.jsxs)(n.p,{children:[(0,l.jsx)("a",{href:"https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug.html",children:"A GNAT project file."}),"\nThis is a more detailed file describing how to build your code into a running program or a library\nof code for others to use."]})})]}),(0,l.jsxs)("tr",{children:[(0,l.jsx)("td",{children:(0,l.jsx)("code",{children:"my_hello_world.adb"})}),(0,l.jsx)("td",{children:(0,l.jsx)(n.p,{children:'The file with the code executed when your program starts. "adb" is the suffix for the "bodies"\nof Ada files. "adb" files tell the details of what to do, the other type of file, an "ads" file\nby convention, describes how to use what is in an "adb" file.'})})]}),(0,l.jsxs)("tr",{children:[(0,l.jsx)("td",{children:(0,l.jsx)("code",{children:"src/"})}),(0,l.jsx)("td",{children:"A directory to place additional code your program needs."})]}),(0,l.jsxs)("tr",{children:[(0,l.jsx)("td",{children:(0,l.jsx)("code",{children:"share/"})}),(0,l.jsx)("td",{children:"Used for additional things by Alire."})]})]}),"\n",(0,l.jsx)(n.h2,{id:"building-a-project",children:"Building a project"}),"\n",(0,l.jsx)(n.p,{children:"Let's try to build the project:"}),"\n",(0,l.jsx)(n.pre,{children:(0,l.jsx)(n.code,{className:"language-bash",children:"$ cd my_hello_world\n$ alr build\n"})}),"\n",(0,l.jsx)(n.p,{children:"You should see something like:"}),"\n",(0,l.jsx)(n.pre,{children:(0,l.jsx)(n.code,{className:"language-text",children:"Note: Synchronizing workspace...\nNothing to update.\n\nNote: Building my_hello_world/my_hello_world.gpr...\nSetup\n   [mkdir]        object directory for project My_Hello_World\n   [mkdir]        exec directory for project My_Hello_World\nCompile\n   [Ada]          my_hello_world.adb\nBind\n   [gprbind]      my_hello_world.bexch\n   [Ada]          my_hello_world.ali\nLink\n   [link]         my_hello_world.adb\nBuild finished successfully in 1.51 seconds.\n"})}),"\n",(0,l.jsx)(n.p,{children:"Let's try to run the program to see what happens:"}),"\n",(0,l.jsx)(n.pre,{children:(0,l.jsx)(n.code,{className:"language-bash",children:"$ alr run\n"})}),"\n",(0,l.jsx)(n.p,{children:"It prints some things, but it's not very interesting. It doesn't look like it\ndid anything."}),"\n",(0,l.jsx)(n.pre,{children:(0,l.jsx)(n.code,{className:"language-text",children:'Note: Building my_hello_world/my_hello_world.gpr...\ngprbuild: "my_hello_world.exe" up to date\nBuild finished successfully in 0.70 seconds.\n'})}),"\n",(0,l.jsx)(n.h2,{id:"its-just-an-empty-program",children:"It's just an empty program..."}),"\n",(0,l.jsxs)(n.p,{children:["Open up ",(0,l.jsx)(n.code,{children:"my_hello_world.adb"})," and have a look."]}),"\n",(0,l.jsx)(n.pre,{children:(0,l.jsx)(n.code,{className:"language-ada",children:"procedure My_Hello_World is\nbegin\n   null;\nend My_Hello_World;\n"})}),"\n",(0,l.jsxs)(n.p,{children:['In most programming languages, the point where the program begins (the entry point)\nis called "main" but in Ada it can have other names. This is something\nconfigured in the ',(0,l.jsx)(n.code,{children:"my_hello_world.gpr"})," file made when Alire created the project."]}),"\n",(0,l.jsx)(n.p,{children:"This is just an empty program. Broken down, it here's what the various words\nmean:"}),"\n",(0,l.jsxs)(n.p,{children:[(0,l.jsx)(n.code,{children:"procedure"})," - What follows is a block of code that can be run by giving its name."]}),"\n",(0,l.jsxs)(n.p,{children:[(0,l.jsx)(n.code,{children:"My_Hello_World"}),' - This "identifies" the procedure uniquely in this part of the\nprogram. Since it provides identification, it\'s called an ',(0,l.jsx)(n.strong,{children:'"identifier."'})]}),"\n",(0,l.jsxs)(n.p,{children:[(0,l.jsx)(n.code,{children:"is"})," ... ",(0,l.jsx)(n.code,{children:"begin"})," - Between these two words is the section of code is where\nadditional variables and constants would go. There' no variables or constants\nused here, so it's just empty."]}),"\n",(0,l.jsxs)(n.p,{children:[(0,l.jsx)(n.code,{children:"begin"})," ... ",(0,l.jsx)(n.code,{children:"end"}),' - Instructions called "statements" go between these words to\ntell the program to do things. They are executed one at a time, and each one\nis separated by semicolon ',(0,l.jsx)(n.code,{children:";"}),"."]}),"\n",(0,l.jsxs)(n.p,{children:[(0,l.jsx)(n.code,{children:"null;"})," - The section between ",(0,l.jsx)(n.code,{children:"begin"})," and ",(0,l.jsx)(n.code,{children:"end"})," cannot be empty, so it ",(0,l.jsx)(n.strong,{children:"MUST"}),'\nhave a statement. The "null statement" does nothing.']}),"\n",(0,l.jsxs)(n.p,{children:[(0,l.jsx)(n.code,{children:"end My_Hello_World;"})," - Indicates the end of the program. In Ada, ",(0,l.jsx)(n.code,{children:"end"}),"s can be\nannotated the name of the thing which started the block being terminated. In\nthis case, it's the procedure ",(0,l.jsx)(n.code,{children:"My_Hello_World"}),", so that's the name used here."]}),"\n",(0,l.jsx)(n.h2,{id:"making-it-do-something",children:"Making it do something"}),"\n",(0,l.jsx)(n.p,{children:"There's an empty program, with absolutely nothing in it. How do you make it\ndo something?"}),"\n",(0,l.jsxs)(n.p,{children:['The start of every Ada file is what is called the "context clause." This is\nwhere dependencies on other pieces of code get put. In Ada, there are three\npackages provided as part of the language, though with ',(0,l.jsx)(n.code,{children:"Alire"})," you can easily bring\nin additional code from other libraries, but these are the big three:"]}),"\n",(0,l.jsxs)("table",{children:[(0,l.jsxs)("tr",{children:[(0,l.jsx)("td",{children:(0,l.jsx)("code",{children:"Ada"})}),(0,l.jsx)("td",{children:"Data structures, basic facilities to read/write files, etc."})]})," ",(0,l.jsxs)("tr",{children:[(0,l.jsx)("td",{children:(0,l.jsx)("code",{children:"Standard"})}),(0,l.jsx)("td",{children:(0,l.jsx)(n.p,{children:"The built-in library of extremely basic things to build upon. These are often things the\nlanguage must handle specially that can't be built out of other things."})})]}),(0,l.jsxs)("tr",{children:[(0,l.jsx)("td",{children:(0,l.jsx)("code",{children:"Interfaces"})}),(0,l.jsx)("td",{children:"Components to talk to other languages, like C."})]})]}),"\n",(0,l.jsxs)(n.p,{children:['These "packages" of code can be nested. You want to print things, which is in\nthe ',(0,l.jsx)(n.code,{children:"Text_IO"})," package, which resides within the ",(0,l.jsx)(n.code,{children:"Ada"})," package. A period (full stop)\nbetween names indicates that the thing on the right is contained within the thing\non the left."]}),"\n",(0,l.jsxs)(n.p,{children:["You want to use procedure to print text to the console, so you bring in ",(0,l.jsx)(n.code,{children:"Ada.Text_IO"}),"\nas a dependency:"]}),"\n",(0,l.jsx)(n.pre,{children:(0,l.jsx)(n.code,{className:"language-ada",children:"with Ada.Text_IO;\n\nprocedure My_Hello_World is\nbegin\n   null;\nend My_Hello_World;\n"})}),"\n",(0,l.jsxs)(n.p,{children:["There's something to do now, so replace that ",(0,l.jsx)(n.code,{children:"null"})," statement with a greeting\nto the world. ",(0,l.jsx)(n.code,{children:"Put_Line"})," is a procedure within the ",(0,l.jsx)(n.code,{children:"Ada.Text_IO"})," package, so\nplace a period after that name to indicate that's where the procedure is located.\n",(0,l.jsx)(n.code,{children:"Put_Line"})," accepts an input, so use parentheses ",(0,l.jsx)(n.code,{children:"("})," and ",(0,l.jsx)(n.code,{children:")"})," around your greeting\nof ",(0,l.jsx)(n.code,{children:'"Hello, World!"'}),"."]}),"\n",(0,l.jsx)(n.pre,{children:(0,l.jsx)(n.code,{className:"language-ada",children:'with Ada.Text_IO;\n\nprocedure My_Hello_World is\nbegin\n   Ada.Text_IO.Put_Line ("Hello, World!");\nend My_Hello_World;\n'})}),"\n",(0,l.jsxs)(n.p,{children:["Let's run the program again. You don't need to do ",(0,l.jsx)(n.code,{children:"alr build"})," every time, if\nyou've change your program code, you can use ",(0,l.jsx)(n.code,{children:"alr run"})," and it will build your\ncode if it changed automatically before running your program."]}),"\n",(0,l.jsx)(n.pre,{children:(0,l.jsx)(n.code,{className:"language-bash",children:"$ alr run\n"})}),"\n",(0,l.jsx)(n.pre,{children:(0,l.jsx)(n.code,{className:"language-text",children:"Note: Building my_hello_world/my_hello_world.gpr...\nCompile\n   [Ada]          my_hello_world.adb\nBind\n   [gprbind]      my_hello_world.bexch\n   [Ada]          my_hello_world.ali\nLink\n   [link]         my_hello_world.adb\nBuild finished successfully in 1.03 seconds.\nHello, World!\n"})}),"\n",(0,l.jsx)(n.p,{children:"And there's your greeting to the world!"}),"\n",(0,l.jsx)(n.h2,{id:"need-help",children:"Need help?"}),"\n",(0,l.jsxs)(n.p,{children:["Go to the ",(0,l.jsx)(n.a,{href:"https://gitter.im/ada-lang/Lobby",children:"Ada language gitter"})," if you have\nquestions or need help."]})]})}function c(e={}){const{wrapper:n}={...(0,r.R)(),...e.components};return n?(0,l.jsx)(n,{...e,children:(0,l.jsx)(h,{...e})}):h(e)}}}]);