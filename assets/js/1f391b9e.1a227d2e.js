"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[3085],{8320:(e,n,t)=>{t.r(n),t.d(n,{default:()=>u});var r=t(6687),a=t(4923),l=t(6382),i=t(7732),o=t(2873),c=t(6453),s=t(797);const m="mdxPageWrapper_yWu8";function u(e){const{content:n}=e,{metadata:{title:t,description:u,frontMatter:d}}=n,{wrapperClassName:f,hide_table_of_contents:v}=d;return r.createElement(l.FG,{className:(0,a.Z)(null!=f?f:i.k.wrapper.mdxPages,i.k.page.mdxPage)},r.createElement(l.d,{title:t,description:u}),r.createElement(o.Z,null,r.createElement("main",{className:"container container--fluid margin-vert--lg"},r.createElement("div",{className:(0,a.Z)("row",m)},r.createElement("div",{className:(0,a.Z)("col",!v&&"col--8")},r.createElement("article",null,r.createElement(c.Z,null,r.createElement(n,null)))),!v&&n.toc.length>0&&r.createElement("div",{className:"col col--2"},r.createElement(s.Z,{toc:n.toc,minHeadingLevel:d.toc_min_heading_level,maxHeadingLevel:d.toc_max_heading_level}))))))}},797:(e,n,t)=>{t.d(n,{Z:()=>v});var r=t(6687),a=t(4923),l=t(6182);const i="tableOfContents_xYee";var o=Object.defineProperty,c=Object.defineProperties,s=Object.getOwnPropertyDescriptors,m=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,d=Object.prototype.propertyIsEnumerable,f=(e,n,t)=>n in e?o(e,n,{enumerable:!0,configurable:!0,writable:!0,value:t}):e[n]=t;function v(e){var n,t=e,{className:o}=t,v=((e,n)=>{var t={};for(var r in e)u.call(e,r)&&n.indexOf(r)<0&&(t[r]=e[r]);if(null!=e&&m)for(var r of m(e))n.indexOf(r)<0&&d.call(e,r)&&(t[r]=e[r]);return t})(t,["className"]);return r.createElement("div",{className:(0,a.Z)(i,"thin-scrollbar",o)},r.createElement(l.Z,(n=((e,n)=>{for(var t in n||(n={}))u.call(n,t)&&f(e,t,n[t]);if(m)for(var t of m(n))d.call(n,t)&&f(e,t,n[t]);return e})({},v),c(n,s({linkClassName:"table-of-contents__link toc-highlight",linkActiveClassName:"table-of-contents__link--active"})))))}},6182:(e,n,t)=>{t.d(n,{Z:()=>_});var r=t(6687),a=t(4830),l=Object.defineProperty,i=Object.defineProperties,o=Object.getOwnPropertyDescriptors,c=Object.getOwnPropertySymbols,s=Object.prototype.hasOwnProperty,m=Object.prototype.propertyIsEnumerable,u=(e,n,t)=>n in e?l(e,n,{enumerable:!0,configurable:!0,writable:!0,value:t}):e[n]=t,d=(e,n)=>{for(var t in n||(n={}))s.call(n,t)&&u(e,t,n[t]);if(c)for(var t of c(n))m.call(n,t)&&u(e,t,n[t]);return e},f=(e,n)=>i(e,o(n));function v(e){const n=e.map((e=>f(d({},e),{parentIndex:-1,children:[]}))),t=Array(7).fill(-1);n.forEach(((e,n)=>{const r=t.slice(2,e.level);e.parentIndex=Math.max(...r),t[e.level]=n}));const r=[];return n.forEach((e=>{const t=e,{parentIndex:a}=t,l=((e,n)=>{var t={};for(var r in e)s.call(e,r)&&n.indexOf(r)<0&&(t[r]=e[r]);if(null!=e&&c)for(var r of c(e))n.indexOf(r)<0&&m.call(e,r)&&(t[r]=e[r]);return t})(t,["parentIndex"]);a>=0?n[a].children.push(l):r.push(l)})),r}function p({toc:e,minHeadingLevel:n,maxHeadingLevel:t}){return e.flatMap((e=>{const r=p({toc:e.children,minHeadingLevel:n,maxHeadingLevel:t});return function(e){return e.level>=n&&e.level<=t}(e)?[f(d({},e),{children:r})]:r}))}function g(e){const n=e.getBoundingClientRect();return n.top===n.bottom?g(e.parentNode):n}function b(e,{anchorTopOffset:n}){var t,r;const a=e.find((e=>g(e).top>=n));if(a){return function(e){return e.top>0&&e.bottom<window.innerHeight/2}(g(a))?a:null!=(t=e[e.indexOf(a)-1])?t:null}return null!=(r=e[e.length-1])?r:null}function h(){const e=(0,r.useRef)(0),{navbar:{hideOnScroll:n}}=(0,a.L)();return(0,r.useEffect)((()=>{e.current=n?0:document.querySelector(".navbar").clientHeight}),[n]),e}function O(e){const n=(0,r.useRef)(void 0),t=h();(0,r.useEffect)((()=>{if(!e)return()=>{};const{linkClassName:r,linkActiveClassName:a,minHeadingLevel:l,maxHeadingLevel:i}=e;function o(){const e=function(e){return Array.from(document.getElementsByClassName(e))}(r),o=function({minHeadingLevel:e,maxHeadingLevel:n}){const t=[];for(let r=e;r<=n;r+=1)t.push(`h${r}.anchor`);return Array.from(document.querySelectorAll(t.join()))}({minHeadingLevel:l,maxHeadingLevel:i}),c=b(o,{anchorTopOffset:t.current}),s=e.find((e=>c&&c.id===function(e){return decodeURIComponent(e.href.substring(e.href.indexOf("#")+1))}(e)));e.forEach((e=>{!function(e,t){t?(n.current&&n.current!==e&&n.current.classList.remove(a),e.classList.add(a),n.current=e):e.classList.remove(a)}(e,e===s)}))}return document.addEventListener("scroll",o),document.addEventListener("resize",o),o(),()=>{document.removeEventListener("scroll",o),document.removeEventListener("resize",o)}}),[e,t])}const L=r.memo((function e({toc:n,className:t,linkClassName:a,isChild:l}){return n.length?r.createElement("ul",{className:l?void 0:t},n.map((n=>r.createElement("li",{key:n.id},r.createElement("a",{href:`#${n.id}`,className:null!=a?a:void 0,dangerouslySetInnerHTML:{__html:n.value}}),r.createElement(e,{isChild:!0,toc:n.children,className:t,linkClassName:a}))))):null}));var y=Object.defineProperty,x=Object.getOwnPropertySymbols,E=Object.prototype.hasOwnProperty,N=Object.prototype.propertyIsEnumerable,H=(e,n,t)=>n in e?y(e,n,{enumerable:!0,configurable:!0,writable:!0,value:t}):e[n]=t;function _(e){var n=e,{toc:t,className:l="table-of-contents table-of-contents__left-border",linkClassName:i="table-of-contents__link",linkActiveClassName:o,minHeadingLevel:c,maxHeadingLevel:s}=n,m=((e,n)=>{var t={};for(var r in e)E.call(e,r)&&n.indexOf(r)<0&&(t[r]=e[r]);if(null!=e&&x)for(var r of x(e))n.indexOf(r)<0&&N.call(e,r)&&(t[r]=e[r]);return t})(n,["toc","className","linkClassName","linkActiveClassName","minHeadingLevel","maxHeadingLevel"]);const u=(0,a.L)(),d=null!=c?c:u.tableOfContents.minHeadingLevel,f=null!=s?s:u.tableOfContents.maxHeadingLevel,g=function({toc:e,minHeadingLevel:n,maxHeadingLevel:t}){return(0,r.useMemo)((()=>p({toc:v(e),minHeadingLevel:n,maxHeadingLevel:t})),[e,n,t])}({toc:t,minHeadingLevel:d,maxHeadingLevel:f});return O((0,r.useMemo)((()=>{if(i&&o)return{linkClassName:i,linkActiveClassName:o,minHeadingLevel:d,maxHeadingLevel:f}}),[i,o,d,f])),r.createElement(L,((e,n)=>{for(var t in n||(n={}))E.call(n,t)&&H(e,t,n[t]);if(x)for(var t of x(n))N.call(n,t)&&H(e,t,n[t]);return e})({toc:g,className:l,linkClassName:i},m))}}}]);